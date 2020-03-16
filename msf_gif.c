//TODO: combine rbits/gbits/bbits into a struct to reduce verbosity?

#include "msf_gif.h"

#include <string.h> //memcpy
#include <stdio.h> //FILE ops (fopen, etc.)
#include <stdlib.h> //malloc, etc.

static int bit_log(int i) {
    return 32 - __builtin_clz(i);
}

////////////////////////////////////////////////////////////////////////////////
/// FileBuffer                                                               ///
////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint8_t * block;
    uint8_t * head;
    uint8_t * end;
} FileBuffer;

static void check(FileBuffer * buf, size_t bytes) {
    if (buf->head + bytes < buf->end)
        return;

    size_t byte = buf->head - buf->block;
    size_t size = buf->end - buf->block;

    //done in a loop so adding payloads larger than the current buffer size will work
    while (byte + bytes >= size) {
        size = size * 2 + 1;
    }

    buf->block = (uint8_t *) realloc(buf->block, size);
    buf->head = buf->block + byte;
    buf->end = buf->block + size;
}

static void write_data(FileBuffer * buf, void * data, size_t bytes) {
    memcpy(buf->head, data, bytes);
    buf->head += bytes;
}

static void write_u8(FileBuffer * buf, uint8_t data) {
    *buf->head++ = data;
}

static void write_u16(FileBuffer * buf, uint16_t data) {
    *buf->head++ = data;
    *buf->head++ = data >> 8;
}

static FileBuffer create_file_buffer(size_t bytes) {
    uint8_t * block = (uint8_t *) malloc(bytes);
    FileBuffer ret = { block, block, block + bytes };
    return ret;
}

////////////////////////////////////////////////////////////////////////////////
/// Frame Cooking                                                            ///
////////////////////////////////////////////////////////////////////////////////

static void cook_frame(int width, int height, bool * used, uint8_t * raw, uint32_t * cooked,
    int rbits, int gbits, int bbits)
{
    int paletteSize = 1 << (rbits + gbits + bbits);
    memset(used, 0, paletteSize * sizeof(bool));

    for (int i = 0; i < width * height; ++i) {
        uint8_t * p = &raw[i * 4];
        cooked[i] = p[2] >> (8 - bbits) << (rbits + gbits) | p[1] >> (8 - gbits) << rbits | p[0] >> (8 - rbits);
        used[cooked[i]] = true; //mark colors
    }
}

typedef struct {
    int rbits, gbits, bbits;
} MetaPalette;

typedef struct {
    bool * usedMem;
    MetaPalette * palettes;
} MetaPaletteInfo;

// typedef struct {
//     uint32_t * pixels;
//     bool * used;
//     int rbits, gbits, bbits;
// } CookedFrame;

static MetaPaletteInfo choose_meta_palette(uint8_t ** rawFrames, int rawFrameCount, uint32_t ** cookedFrames,
    int width, int height)
{
    bool * usedMem = (bool *) malloc(rawFrameCount * (1 << 15) * sizeof(bool));

    //bit depth for each channel
    int rbits[9] = { 5, 5, 4, 4, 4, 3, 3, 3, 2 };
    int gbits[9] = { 5, 5, 5, 4, 4, 4, 3, 3, 3 };
    int bbits[9] = { 5, 4, 4, 4, 3, 3, 3, 2, 2 };

    MetaPalette * palettes = (MetaPalette *) malloc(rawFrameCount * sizeof(MetaPalette));
    for (int i = 0; i < rawFrameCount; ++i) {
        int pal = 0, count = 0;
        bool * used = &usedMem[i * (1 << 15)];

        do {
            cook_frame(width, height, used, rawFrames[i], cookedFrames[i], rbits[pal], gbits[pal], bbits[pal]);

            //count used colors
            count = 0;
            for (int j = 0; j < 1 << (15 - pal); ++j) {
                count += used[j];
            }
        } while (count >= 256 && ++pal);

        palettes[i] = (MetaPalette) { rbits[pal], gbits[pal], bbits[pal] };
    }

    MetaPaletteInfo ret = { usedMem, palettes };
    return ret;
}

////////////////////////////////////////////////////////////////////////////////
/// Frame Compression                                                        ///
////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint32_t bits;
    uint16_t bytes[129];
} BlockBuffer;

static void put_code(FileBuffer * buf, BlockBuffer * block, int bits, uint32_t code) {
    //insert new code into block buffer
    int idx = block->bits / 16;
    int bit = block->bits % 16;
    block->bytes[idx + 0] |= code <<       bit ;
    block->bytes[idx + 1] |= code >> (16 - bit);
    block->bits += bits;

    //flush the block buffer if it's full
    if (block->bits >= 255 * 8) {
        check(buf, 256);
        write_u8(buf, 255);
        write_data(buf, block->bytes, 255);

        block->bits -= 255 * 8;
        block->bytes[0] = block->bytes[127] >> 8 | block->bytes[128] << 8;
        memset(block->bytes + 1, 0, 256);
    }
}



typedef struct {
    int16_t * data;
    size_t len;
    size_t stride;
} StridedList;

static void reset(StridedList * lzw, int tableSize, int stride) {
    memset(lzw->data, 0xFF, 4096 * stride * sizeof(int16_t));
    lzw->len = tableSize + 2;
    lzw->stride = stride;
}



static void compress_frames(int width, int height, int centiSeconds, MetaPaletteInfo meta,
    uint32_t ** cookedFrames, FileBuffer * compressedFrames, int frameCount)
{
    StridedList lzw = { (int16_t *) malloc(4096 * 256 * sizeof(int16_t)) };
    for (int frameIdx = 0; frameIdx < frameCount; ++frameIdx) {
        uint32_t * cframe = cookedFrames[frameIdx];
        bool * used = &meta.usedMem[frameIdx * (1 << 15)];
        FileBuffer buf = create_file_buffer(1024);

        //allocate tlb
        MetaPalette pal = meta.palettes[frameIdx];
        int totalBits = pal.rbits + pal.gbits + pal.bbits;
        int tlbSize = 1 << totalBits;
        uint8_t tlb[tlbSize];

        //generate palette
        typedef struct { uint8_t r, g, b; } Color3;
        Color3 table[256] = {};
        int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
        for (int i = 0; i < tlbSize; ++i) {
            if (used[i]) {
                tlb[i] = tableIdx;
                int rmask = (1 << pal.rbits) - 1;
                int gmask = (1 << pal.gbits) - 1;
                //isolate components
                int r = i & rmask;
                int g = i >> pal.rbits & gmask;
                int b = i >> (pal.rbits + pal.gbits);
                //shift into highest bits
                r <<= 8 - pal.rbits;
                g <<= 8 - pal.gbits;
                b <<= 8 - pal.bbits;
                Color3 color = {
                    (uint8_t)(r | r >> pal.rbits),
                    (uint8_t)(g | g >> pal.gbits),
                    (uint8_t)(b | b >> pal.bbits),
                };
                table[tableIdx] = color;
                ++tableIdx;
            }
        }

        int tableBits = bit_log(tableIdx - 1);
        int tableSize = 1 << tableBits;

        check(&buf, 8 + 10);
        //graphics control extension
        write_u8(&buf, 0x21); //extension introducer
        write_u8(&buf, 0xF9); //extension identifier
        write_u8(&buf, 4); //block size (always 4)
        write_u8(&buf, 0b00000100); //000 001 0 0    reserved, disposal method: keep, input flag, transparency flag
        write_u16(&buf, centiSeconds); //x/100 seconds per frame
        write_u8(&buf, 0); //transparent color index
        write_u8(&buf, 0); //block terminator

        //image descriptor
        write_u8(&buf, 0x2C); //image separator
        write_u16(&buf, 0); //image left
        write_u16(&buf, 0); //image top
        write_u16(&buf, width);
        write_u16(&buf, height);
        //local color table flag, interlace flag, sort flag, reserved, local color table size
        write_u8(&buf, 0b10000000 | (tableBits - 1)); //1 0 0 00 000

        //local color table
        check(&buf, tableSize * sizeof(Color3));
        write_data(&buf, table, tableSize * sizeof(Color3));

        //image data
        BlockBuffer block = {};
        check(&buf, 1);
        write_u8(&buf, tableBits);
        reset(&lzw, tableSize, tableIdx);
        //XXX: do we actually need to write this?
        put_code(&buf, &block, bit_log(lzw.len - 1), tableSize); //clear code

        uint8_t idxBuffer[4096];
        int idxLen = 0;
        int lastCode = tlb[cframe[0]];
        for (int i = 1; i < width * height; ++i) {
            idxBuffer[idxLen++] = tlb[cframe[i]];
            int code = (&lzw.data[lastCode * lzw.stride])[idxBuffer[idxLen - 1]];
            if (code < 0) {
                //write to code stream
                int codeBits = bit_log(lzw.len - 1);
                put_code(&buf, &block, codeBits, lastCode);

                //NOTE: [I THINK] we need to leave room for 2 more codes (leftover and end code)
                //      because we don't ever reset the table after writing the leftover bits
                //XXX: is my thinking correct on this one?
                if (lzw.len > 4094) {
                    //reset buffer code table
                    put_code(&buf, &block, codeBits, tableSize);
                    reset(&lzw, tableSize, tableIdx);
                } else {
                    (&lzw.data[lastCode * lzw.stride])[idxBuffer[idxLen - 1]] = lzw.len;
                    ++lzw.len;
                }

                //reset index buffer
                idxBuffer[0] = idxBuffer[idxLen - 1];
                idxLen = 1;

                lastCode = idxBuffer[0];
            } else {
                lastCode = code;
            }
        }

        //write code for leftover index buffer contents, then the end code
        put_code(&buf, &block, bit_log(lzw.len - 1), lastCode);
        put_code(&buf, &block, bit_log(lzw.len), tableSize + 1); //end code

        //flush remaining data
        if (block.bits) {
            int bytes = (block.bits + 7) / 8; //round up
            check(&buf, bytes + 1);
            write_u8(&buf, bytes);
            write_data(&buf, block.bytes, bytes);
        }

        check(&buf, 1);
        write_u8(&buf, 0); //terminating block

        compressedFrames[frameIdx - 1] = buf;
    }
    free(lzw.data);
}

////////////////////////////////////////////////////////////////////////////////
/// Driver                                                                   ///
////////////////////////////////////////////////////////////////////////////////

size_t msf_save_gif(uint8_t ** rawFrames, int rawFrameCount, int width, int height, int centiSeconds, const char * path)
{
    //allocate space for cooked frames
    //TODO: regenerate these pointers implicitly on the fly, to save on stack space
    uint32_t * cookedFrames[rawFrameCount];
    uint32_t * cookedMemBlock = (uint32_t *) malloc(rawFrameCount * width * height * sizeof(uint32_t));
    for (int i = 0; i < rawFrameCount; ++i) {
        cookedFrames[i] = cookedMemBlock + width * height * i;
    }

    MetaPaletteInfo meta = choose_meta_palette(rawFrames, rawFrameCount, cookedFrames, width, height);
    FileBuffer compressedFrames[rawFrameCount];
    compress_frames(width, height, centiSeconds, meta, cookedFrames, compressedFrames, rawFrameCount);

    //header
    FileBuffer buf = create_file_buffer(2048);
    for (const char * c = "GIF89a"; *c != '\0'; ++c)
        write_u8(&buf, *c);

    //logical screen descriptor
    write_u16(&buf, width);
    write_u16(&buf, height);
    //global color table flag, color resolution (???), sort flag, global color table size
    write_u8(&buf, 0b00010000); //0 001 0 000
    write_u8(&buf, 0); //background color index
    write_u8(&buf, 0); //pixel aspect ratio

    //application extension
    write_u8(&buf, 0x21); //extension introducer
    write_u8(&buf, 0xFF); //extension identifier
    write_u8(&buf, 11); //fixed length data size
    for (const char * c = "NETSCAPE2.0"; *c != '\0'; ++c)
        write_u8(&buf, *c);
    write_u8(&buf, 3); //data block size
    write_u8(&buf, 1); //???
    write_u16(&buf, 0); //loop forever
    write_u8(&buf, 0); //block terminator

    for (int i = 0; i < rawFrameCount; ++i) {
        FileBuffer b = compressedFrames[i];
        check(&buf, b.head - b.block);
        write_data(&buf, b.block, b.head - b.block);
        free(b.block);
    }

    check(&buf, 1);
    write_u8(&buf, 0x3B); //trailing marker

    //write data to file
    FILE * fp = fopen(path, "wb");
    fwrite(buf.block, buf.head - buf.block, 1, fp);
    fclose(fp);

    //cleanup
    free(buf.block);
    free(cookedMemBlock);
    free(meta.usedMem);
    free(meta.palettes);

    return buf.head - buf.block;
}
