//NOTE: AVX2 optimizations and LZCNT don't appear to make GIF exporter any faster overall,
//      so we have them disabled for now
//TODO: figure out why they aren't faster

#include "giff.hpp"
#include "common.hpp"
#include "FileBuffer.hpp"

#ifdef _MSC_VER
    #include <intrin.h>

    int bit_count(int i) {
        unsigned long idx;
        _BitScanReverse(&idx, i) + 1; //the knockoff MSVC version of this macros sucks hard, man
        return idx; //why couldn't they just support the standard one?
    }
#else
    #include <x86intrin.h>

    //TODO: replace this with a builtin on clang and gcc,
    //      and a generic version with de bruijn multiplication
    int bit_count(int i) {
        return _bit_scan_reverse(i) + 1;
    }
#endif

//forward declaration
double get_time();

//TODO: find a way to write the data blocks without storing up a buffer of 255 bytes
struct BlockBuffer {
    u16 bits;
    u8 bytes[257]; //up to 12 bits can be written at once, so we need 2 extra "overflow" bytes
};

//XXX: this is very slow because of how it uses the file buffer!
void put_code(FileBuffer * buf, BlockBuffer * block, int bits, u16 code) {
    //insert new code into block buffer
    int idx = block->bits / 8;
    int bit = block->bits % 8;
    block->bytes[idx + 0] |= code <<       bit      ;
    block->bytes[idx + 1] |= code >>  (8 - bit)     ;
    block->bytes[idx + 2] |= code >> ((8 - bit) + 8);
    block->bits += bits;

    //flush the block buffer if it's full
    if (block->bits >= 255 * 8) {
        buf->check(256);
        buf->write_unsafe<u8>(255);
        buf->write_block_unsafe(block->bytes, 255);

        block->bits -= 255 * 8;
        block->bytes[0] = block->bytes[255];
        block->bytes[1] = block->bytes[256];
        memset(block->bytes + 2, 0, 255);
    }
}

struct StridedList {
    i16 * data;
    size_t len;
    size_t stride;

    i16 * operator[](size_t index) {
        return &data[index * stride];
    }
};

void reset(StridedList * lzw, int tableSize, int stride) {
    memset(lzw->data, 0xFF, 4096 * stride * sizeof(i16));
    lzw->len = tableSize + 2;
    lzw->stride = stride;
}

struct MetaPaletteInfo {
    bool * * used;
    int rbits, gbits, bbits;
};

void cook_frame(RawFrame raw, u32 * cooked, int width, int height,
                int rbits, int gbits, int bbits) {

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            Pixel p = raw.pixels[y * raw.pitch + x];
            cooked[y * width + x] = p.b >> (8 - bbits) << (rbits + gbits) |
                                    p.g >> (8 - gbits) <<  rbits |
                                    p.r >> (8 - rbits);
        }
    }
}

void cook_frame_dithered(RawFrame raw, u32 * cooked, int width, int height,
                         int rbits, int gbits, int bbits) {
    int ditherKernel[8 * 8] = {
         0, 48, 12, 60,  3, 51, 15, 63,
        32, 16, 44, 28, 35, 19, 47, 31,
         8, 56,  4, 52, 11, 59,  7, 55,
        40, 24, 36, 20, 43, 27, 39, 23,
         2, 50, 14, 62,  1, 49, 13, 61,
        34, 18, 46, 30, 33, 17, 45, 29,
        10, 58,  6, 54,  9, 57,  5, 53,
        42, 26, 38, 22, 41, 25, 37, 21,
    };

    // int derivedKernel[8 * 8];
    // for (int i = 0; i < 8 * 8; ++i) {
    //     int k = ditherKernel[i];
    //     derivedKernel[i] = k >> rshift & k >> gshift << 8 & k >> bshift << 16;
    // }

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            Pixel p = raw.pixels[y * raw.pitch + x];
            int dx = x % 8, dy = y % 8;
            int k = ditherKernel[dy * 8 + dx];
            cooked[y * width + x] =
                min(255, p.b + (k >> (rbits - 2))) >> (8 - bbits) << (rbits + gbits) |
                min(255, p.g + (k >> (gbits - 2))) >> (8 - gbits) <<  rbits |
                min(255, p.r + (k >> (bbits - 2))) >> (8 - rbits);
        }
    }
}

MetaPaletteInfo choose_meta_palette(
        List<RawFrame> rawFrames, List<u32 *> cookedFrames, int width, int height, bool dither)
{
    bool * * used = (bool * *) malloc(rawFrames.len * sizeof(bool *));
    //set to null so we can spuriously free() with no effect
    memset(used, 0, rawFrames.len * sizeof(bool *));

    //bit depth for each channel
    int rbits[9] = { 5, 5, 4, 4, 4, 3, 3, 3, 2 };
    int gbits[9] = { 5, 5, 5, 4, 4, 4, 3, 3, 3 };
    int bbits[9] = { 5, 4, 4, 4, 3, 3, 3, 2, 2 };

    //NOTE: it's possible (if unlikely) for a reduction in bit depth to result in an increase in
    //      colors used for a given frame when dithering is applied, so we treat the list of frames
    //      like a ring buffer, circling around until all frames are cooked with the same bit depth
    int minPalette = 0;
    int minCorrect = 0;
    int idx = 0;
    while (idx < minCorrect + (int) cookedFrames.len - 1) {
        int i = idx % (cookedFrames.len - 1) + 1;
        u32 * frame = cookedFrames[i];
        // bool * used1 = used + (i - 1) * 32768;
        int paletteSize = 1 << (15 - minPalette);
        free(used[i - 1]);
        used[i - 1] = (bool *) malloc(paletteSize * sizeof(bool));

        if (dither)
            cook_frame_dithered(rawFrames[i - 1], frame, width, height,
                rbits[minPalette], gbits[minPalette], bbits[minPalette]);
        else
            cook_frame(rawFrames[i - 1], frame, width, height,
                rbits[minPalette], gbits[minPalette], bbits[minPalette]);

        //mark down which colors are used out of the full 15-bit palette
        memset(used[i - 1], 0, paletteSize * sizeof(bool));
        for (int j = 0; j < width * height; ++j)
            used[i - 1][frame[j]] = true;

        //count how many fall into the meta-palette
        int count = 0;
        for (int j = 0; j < paletteSize; ++j)
            if (used[i - 1][j])
                ++count; //XXX: should we break early here (if count > 255) for performance?
        // printf("frame %3d: %4d colors used out of %5d      ", i, count, paletteSize);

        if (count < 256) {
            ++idx;
            // printf("\n");
            // fflush(stdout);
        } else {
            ++minPalette;
            minCorrect = idx;
        }
    }

    return { used, rbits[minPalette], gbits[minPalette], bbits[minPalette] };
}

DebugTimers save_gif(int width, int height, List<RawFrame> rawFrames,
                     int centiSeconds, const char * path, bool dither) {
    DebugTimers timers = {};

    float preAmble, preTotal;
    preAmble = preTotal = get_time();
    //allocate space for cooked frames
    List<u32 *> cookedFrames = create_list<u32 *>(rawFrames.len + 1);
    u32 * cookedMemBlock = (u32 *) malloc((rawFrames.len + 1) * width * height * sizeof(u32));
    memset(cookedMemBlock, 0, width * height * sizeof(u32)); //set dummy frame to background color
    for (int i = 0; i < (int) rawFrames.len + 1; ++i) {
        cookedFrames.add(cookedMemBlock + width * height * i);
    }



    float preChoice = get_time();
    MetaPaletteInfo meta = choose_meta_palette(rawFrames, cookedFrames, width, height, dither);
    timers.choice = get_time() - preChoice;
    printf("bits: %d, %d, %d\n", meta.rbits, meta.gbits, meta.bbits);
    timers.amble = get_time() - preAmble;

    float preCompress = get_time();
    //header
    FileBuffer buf = create_file_buffer(2048);
    for (char c : range("GIF89a")) {
        buf.write_unsafe(c);
    }

    //logical screen descriptor
    buf.write_unsafe<u16>(width);
    buf.write_unsafe<u16>(height);
    //global color table flag, color resolution (???), sort flag, global color table size
    // buf.write_unsafe<u8>(0b0'001'0'000);
    buf.write_unsafe<u8>(0b00010000);
    buf.write_unsafe<u8>(0); //background color index
    buf.write_unsafe<u8>(0); //pixel aspect ratio

    //application extension
    buf.write_unsafe<u8>(0x21); //extension introducer
    buf.write_unsafe<u8>(0xFF); //extension identifier
    buf.write_unsafe<u8>(11); //fixed length data size
    for (char c : range("NETSCAPE2.0")) {
        buf.write_unsafe(c);
    }
    buf.write_unsafe<u8>(3); //data block size
    buf.write_unsafe<u8>(1); //???
    buf.write_unsafe<u16>(0); //loop forever
    buf.write_unsafe<u8>(0); //block terminator

    // StridedList lzw = { (i16 *) malloc(4096 * (meta.maxUsed + 1) * sizeof(i16)) };
    StridedList lzw = { (i16 *) malloc(4096 * 256 * sizeof(i16)) };
    List<u8> idxBuffer = create_list<u8>(200);
    uint largestIdxBuffer = 0; //DEBUG

    for (int j : range(1, cookedFrames.len)) {
        u32 * pframe = cookedFrames[j - 1];
        u32 * cframe = cookedFrames[j];

        float prePalette = get_time();
        //allocate tlb
        int totalBits = meta.rbits + meta.gbits + meta.bbits;
        int tlbSize = 1 << totalBits;
        u8 * tlb = (u8 *) malloc(tlbSize * sizeof(u8));

        //generate palette
        struct Color3 { u8 r, g, b; };
        Color3 table[256] = {};
        int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
        for (int i = 0; i < tlbSize; ++i) {
            if (meta.used[j - 1][i]) {
                tlb[i] = tableIdx;
                int rmask = (1 << meta.rbits) - 1;
                int gmask = (1 << meta.gbits) - 1;
                //isolate components
                int r = i & rmask;
                int g = i >> meta.rbits & gmask;
                int b = i >> (meta.rbits + meta.gbits);
                //shift into highest bits
                r <<= 8 - meta.rbits;
                g <<= 8 - meta.gbits;
                b <<= 8 - meta.bbits;
                table[tableIdx] = {
                    (u8)(r | r >> meta.rbits),
                    (u8)(g | g >> meta.gbits),
                    (u8)(b | b >> meta.bbits),
                };
                ++tableIdx;
            }
        }
        // printf("frame %d uses %d colors\n", j, tableIdx);
        timers.palette += get_time() - prePalette;

        int tableBits = bit_count(tableIdx - 1);
        int tableSize = 1 << tableBits;
        // printf("idx: %d bits: %d size: %d\n\n", tableIdx, tableBits, tableSize);

        buf.check(8 + 10);
        //graphics control extension
        buf.write_unsafe<u8>(0x21); //extension introducer
        buf.write_unsafe<u8>(0xF9); //extension identifier
        buf.write_unsafe<u8>(4); //block size (always 4)
        //reserved, disposal method:keep, input flag, transparency flag
        //NOTE: MSVC incorrectly generates warning C4806 here due to a compiler bug.
        // buf.write_unsafe<u8>(0b000'001'0'0 | (j != 1));
        buf.write_unsafe<u8>(0b00000100 | (j != 1));
        buf.write_unsafe<u16>(centiSeconds); //x/100 seconds per frame
        buf.write_unsafe<u8>(0); //transparent color index
        buf.write_unsafe<u8>(0); //block terminator

        //image descriptor
        buf.write_unsafe<u8>(0x2C); //image separator
        buf.write_unsafe<u16>(0); //image left
        buf.write_unsafe<u16>(0); //image top
        buf.write_unsafe<u16>(width);
        buf.write_unsafe<u16>(height);
        //local color table flag, interlace flag, sort flag, reserved, local color table size
        // buf.write_unsafe<u8>(0b1'0'0'00'000 | (tableBits - 1));
        buf.write_unsafe<u8>(0b10000000 | (tableBits - 1));

        //local color table
        buf.write_block(table, tableSize);

        //image data
        BlockBuffer block = {};
        buf.write<u8>(tableBits); //lzw minimum code size
        reset(&lzw, tableSize, tableIdx);
        //XXX: do we actually need to write this?
        put_code(&buf, &block, bit_count(lzw.len - 1), tableSize); //clear code

        float preInner = get_time();
        int lastCode = cframe[0] == pframe[0]? 0 : tlb[cframe[0]];
        for (int i : range(1, width * height)) {
            idxBuffer.add(cframe[i] == pframe[i]? 0 : tlb[cframe[i]]);
            int code = lzw[lastCode][idxBuffer[idxBuffer.len - 1]];
            if (code < 0) {
                //write to code stream
                int codeBits = bit_count(lzw.len - 1);
                put_code(&buf, &block, codeBits, lastCode);
                // printf("%d-%d-%d  ", lastCode, codeBits, (int) lzw.len);

                //NOTE: [I THINK] we need to leave room for 2 more codes (leftover and end code)
                //      because we don't ever reset the table after writing the leftover bits
                //XXX: is my thinking correct on this one?
                if (lzw.len > 4094) {
                    //reset buffer code table
                    put_code(&buf, &block, codeBits, tableSize);
                    reset(&lzw, tableSize, tableIdx);
                } else {
                    lzw[lastCode][idxBuffer[idxBuffer.len - 1]] = lzw.len;
                    ++lzw.len;
                }

                //reset index buffer
                idxBuffer[0] = idxBuffer[idxBuffer.len - 1];
                idxBuffer.len = 1;

                lastCode = idxBuffer[0];
            } else {
                lastCode = code;
            }

            //DEBUG
            if (idxBuffer.len > largestIdxBuffer)
                largestIdxBuffer = idxBuffer.len;
        }
        timers.inner += get_time() - preInner;

        //write code for leftover index buffer contents, then the end code
        put_code(&buf, &block, bit_count(lzw.len - 1), lastCode);
        put_code(&buf, &block, bit_count(lzw.len), tableSize + 1); //end code

        //flush remaining data
        if (block.bits) {
            int bytes = (block.bits + 7) / 8; //round up
            buf.check(bytes + 1);
            buf.write_unsafe<u8>(bytes);
            buf.write_block_unsafe(block.bytes, bytes);
        }

        buf.write<u8>(0); //terminating block
        idxBuffer.len = 0; //reset encoding state
    }
    timers.compress = get_time() - preCompress;

    printf("largest idx buffer: %d\n", largestIdxBuffer); //DEBUG

    buf.write<u8>(0x3B); //trailing marker

    float preWrite = get_time();
    //write data to file
    FILE * fp = fopen(path, "wb");
    assert(fp);
    fwrite(buf.block, buf.size(), 1, fp);
    fclose(fp);
    timers.write = get_time() - preWrite;

    //cleanup
    buf.finalize();
    free(lzw.data);
    idxBuffer.finalize();
    free(cookedMemBlock);
    cookedFrames.finalize();
    free(meta.used);

    timers.total = get_time() - preTotal;
    return timers;
}
