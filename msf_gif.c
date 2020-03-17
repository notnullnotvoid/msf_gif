#include "msf_gif.h"

#include <string.h> //memcpy
#include <stdio.h> //FILE ops (fopen, etc.)
#include <stdlib.h> //malloc, etc.

static inline int bit_log(int i) {
    return 32 - __builtin_clz(i);
}

static inline int min(int a, int b) {
    return a < b? a : b;
}

static inline int max(int a, int b) {
    return b < a? a : b;
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

static inline void write_data(FileBuffer * buf, void * data, size_t bytes) {
    memcpy(buf->head, data, bytes);
    buf->head += bytes;
}

static inline void write_u8(FileBuffer * buf, uint8_t data) {
    *buf->head++ = data;
}

static FileBuffer create_file_buffer(size_t bytes) {
    uint8_t * block = (uint8_t *) malloc(bytes);
    FileBuffer ret = { block, block, block + bytes };
    return ret;
}

////////////////////////////////////////////////////////////////////////////////
/// Frame Cooking                                                            ///
////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint32_t * pixels;
    bool * used;
    int rbits, gbits, bbits;
} CookedFrame;

static CookedFrame cook_frame(int width, int height, int pitchInBytes, uint8_t * raw) {
    //bit depth for each channel
    const int rbitdepths[] = { 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1 };
    const int gbitdepths[] = { 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1 };
    const int bbitdepths[] = { 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1 };

    // const int rbitdepths[] = { 5, 5, 4, 4, 3, 3, 2, 2, 1 };
    // const int gbitdepths[] = { 5, 5, 4, 4, 3, 3, 2, 2, 1 };
    // const int bbitdepths[] = { 5, 4, 4, 3, 3, 2, 2, 1, 1 };

    // const int rbitdepths[] = { 5, 4, 3, 2, 1 };
    // const int gbitdepths[] = { 5, 4, 3, 2, 1 };
    // const int bbitdepths[] = { 5, 4, 3, 2, 1 };

    const int ditherKernel[16] = {
         0 << 4,  8 << 4,  2 << 4, 10 << 4,
        12 << 4,  4 << 4, 14 << 4,  6 << 4,
         3 << 4, 11 << 4,  1 << 4,  9 << 4,
        15 << 4,  7 << 4, 13 << 4,  5 << 4,
    };

    int pal = 0, count = 0;
    bool * used = (bool *) malloc((1 << 15) * sizeof(bool));
    uint32_t * cooked = (uint32_t *) malloc(width * height * sizeof(uint32_t));

    do {
        int rbits = rbitdepths[pal], gbits = gbitdepths[pal], bbits = bbitdepths[pal];
        int paletteSize = 1 << (rbits + gbits + bbits);
        memset(used, 0, paletteSize * sizeof(bool));

        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                uint8_t * p = &raw[y * pitchInBytes + x * 4];
                int dx = x & 3, dy = y & 3;
                int k = ditherKernel[dy * 4 + dx];
                cooked[y * width + x] =
                    max(0, min(255, p[2] - (127 >> bbits) + (k >> bbits))) >> (8 - bbits) << (rbits + gbits) |
                    max(0, min(255, p[1] - (127 >> gbits) + (k >> gbits))) >> (8 - gbits) <<  rbits          |
                    max(0, min(255, p[0] - (127 >> rbits) + (k >> rbits))) >> (8 - rbits);
                used[cooked[y * width + x]] = true; //mark colors
            }
        }

        //count used colors
        count = 0;
        for (int j = 0; j < paletteSize; ++j) {
            count += used[j];
        }
    } while (count >= 256 && ++pal);

    return (CookedFrame) { cooked, used, rbitdepths[pal], gbitdepths[pal], bbitdepths[pal] };
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

static FileBuffer compress_frame(int width, int height, int centiSeconds, CookedFrame frame) {
    FileBuffer buf = create_file_buffer(1024);
    StridedList lzw = { (int16_t *) malloc(4096 * 256 * sizeof(int16_t)) };

    //allocate tlb
    int totalBits = frame.rbits + frame.gbits + frame.bbits;
    int tlbSize = 1 << totalBits;
    uint8_t * tlb = (uint8_t *) malloc(tlbSize * sizeof(uint8_t));

    //generate palette
    typedef struct { uint8_t r, g, b; } Color3;
    Color3 table[256] = {};
    int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
    for (int i = 0; i < tlbSize; ++i) {
        if (frame.used[i]) {
            tlb[i] = tableIdx;
            int rmask = (1 << frame.rbits) - 1;
            int gmask = (1 << frame.gbits) - 1;
            //isolate components
            int r = i & rmask;
            int g = i >> frame.rbits & gmask;
            int b = i >> (frame.rbits + frame.gbits);
            //shift into highest bits
            r <<= 8 - frame.rbits;
            g <<= 8 - frame.gbits;
            b <<= 8 - frame.bbits;
            table[tableIdx++] = (Color3) {
                // (uint8_t) (r | r >> frame.rbits | r >> (frame.rbits * 2)),
                // (uint8_t) (g | g >> frame.gbits | g >> (frame.gbits * 2)),
                // (uint8_t) (b | b >> frame.bbits | b >> (frame.bbits * 2)),
                // (uint8_t) (r | r >> frame.rbits),
                // (uint8_t) (g | g >> frame.gbits),
                // (uint8_t) (b | b >> frame.bbits),
                (uint8_t) r | 1 << (7 - frame.rbits),
                (uint8_t) g | 1 << (7 - frame.gbits),
                (uint8_t) b | 1 << (7 - frame.bbits),
            };
        }
    }

    int tableBits = bit_log(tableIdx - 1);
    int tableSize = 1 << tableBits;

    struct __attribute__((__packed__)) {
        //graphics control extension
        uint8_t extIntroducer, extIdentifier, blockSize, extFlags;
        uint16_t centiSeconds;
        uint8_t transparentColorIdx, blockTerminator;
        //image descriptor
        uint8_t imageSeparator;
        uint16_t left, top, width, height;
        uint8_t imgFlags;
    } header = {
        0x21, 0xF9, 4, 0x04, 0, 0, 0,
        0x2C, 0, 0, 0, 0, 0x80,
    };
    header.centiSeconds = centiSeconds;
    header.width = width;
    header.height = height;
    header.imgFlags |= tableBits - 1;
    write_data(&buf, &header, sizeof(header));

    //local color table
    write_data(&buf, table, tableSize * sizeof(Color3));

    //image data
    BlockBuffer block = {};
    write_u8(&buf, tableBits);
    reset(&lzw, tableSize, tableIdx);

    uint8_t idxBuffer[4096];
    int idxLen = 0;
    int lastCode = tlb[frame.pixels[0]];
    for (int i = 1; i < width * height; ++i) {
        idxBuffer[idxLen++] = tlb[frame.pixels[i]];
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

    free(tlb);
    free(lzw.data);
    return buf;
}

////////////////////////////////////////////////////////////////////////////////
/// Drivers                                                                  ///
////////////////////////////////////////////////////////////////////////////////

size_t msf_save_gif(uint8_t ** rawFrames, int rawFrameCount,
    int width, int height, int pitchInBytes, int centiSeconds, const char * path)
{
    CookedFrame * cookedFrames = (CookedFrame *) malloc(rawFrameCount * sizeof(CookedFrame));
    for (int i = 0; i < rawFrameCount; ++i) {
        cookedFrames[i] = cook_frame(width, height, pitchInBytes, rawFrames[i]);
    }

    FileBuffer * compressedFrames = (FileBuffer *) malloc(rawFrameCount * sizeof(FileBuffer));
    for (int i = 0; i < rawFrameCount; ++i) {
        compressedFrames[i] = compress_frame(width, height, centiSeconds, cookedFrames[i]);
    }

    struct __attribute__((__packed__)) {
        char header[6];
        //logical screen descriptor
        uint16_t width, height;
        uint8_t flags, bgColorIdx, pixelAspectRatio;
        //application extension
        uint8_t extIntroducer, extIdentifier, extDataSize;
        char extData[11];
        uint8_t dataBlockSize, idk;
        uint16_t loopFlag;
        uint8_t blockTerminator;
    } header = {
        { 'G', 'I', 'F', '8', '9', 'a' },
        0, 0, 0x10, 0, 0,
        0x21, 0xFF, 11, { 'N', 'E', 'T', 'S', 'C', 'A', 'P', 'E', '2', '.', '0' }, 3, 1, 0, 0
    };
    header.width = width;
    header.height = height;
    FileBuffer buf = create_file_buffer(2048);
    write_data(&buf, &header, sizeof(header));

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
    for (int i = 0; i < rawFrameCount; ++i) {
        free(cookedFrames[i].pixels);
        free(cookedFrames[i].used);
    }
    free(cookedFrames);

    return buf.head - buf.block;
}
