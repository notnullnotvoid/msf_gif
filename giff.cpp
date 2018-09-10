//NOTE: AVX2 optimizations and LZCNT don't appear to make GIF exporter any faster overall,
//      so we have them disabled for now
//TODO: figure out why they aren't faster
//TODO: setup a GIF exporting test bench to more easily compare performance

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
    int cvtMask;
    int maxUsed;
    bool * used;
};

MetaPaletteInfo choose_meta_palette(List<u16 *> cookedFrames, int width, int height) {
    bool * used = (bool *) malloc(4096 * (cookedFrames.len - 1) * sizeof(bool));
    // int cvtMasks[6] = { 0xFFF, 0xEFF, 0xEFE, 0xEEE, 0xCEE, 0xCEC }; //favor green > red > blue
    int cvtMasks[6] = { 0xFFF, 0xEFF, 0xEEF, 0xEEE, 0xCEE, 0xCCE }; //favor red > green > blue
    int maxUsed[6] = {};
    int minPalette = 0;
    for (int i : range(1, cookedFrames.len)) {
        u16 * frame = cookedFrames[i];
        bool * used1 = used + (i - 1) * 4096;

        //mark down which colors are used out of the full 12-bit palette
        memset(used1, 0, 4096 * sizeof(bool));
        for (int i : range(width * height))
            used1[frame[i]] = true;

        //count how many fall into each meta-palette
        for (int m : range(minPalette, 6)) {
            bool used2[4096] = {};
            for (int i : range(4096))
                used2[i & cvtMasks[m]] |= used1[i];

            int count = 0;
            for (int i : range(4096))
                if (used2[i])
                    ++count;

            // printf("used %3d colors out of %4d\t\t", count, 1 << (12 - m));
            if (count > 255)
                ++minPalette;
            // maxUsed[m] = imax(maxUsed[m], count);
            maxUsed[m] = max(maxUsed[m], count);
        }
        // printf("\n");
    }

    return { cvtMasks[minPalette], maxUsed[minPalette], used };
}

DebugTimers save_gif(int width, int height, List<RawFrame> rawFrames, int centiSeconds, const char * path) {
    DebugTimers timers = {};

    float preCook, preAmble, preTotal;
    preCook = preAmble = preTotal = get_time();
    //cook frames (downsample to 12-bit color)
    List<u16 *> cookedFrames = create_list<u16 *>(rawFrames.len);
    cookedFrames.add((u16 *) malloc(width * height * sizeof(u16))); //dummy frame for diff base
    memset(cookedFrames[0], 0, width * height * sizeof(u16)); //set dummy frame to background color
    for (RawFrame frame : rawFrames) {
        u16 * data = (u16 *) malloc(width * height * sizeof(u16));
        for (int y : range(height)) {
            for (int x : range(width)) {
                Pixel p = frame.pixels[y * frame.pitch + x];
                data[y * width + x] = (p.b & 0xF0) << 4 | (p.g & 0xF0) | (p.r & 0xF0) >> 4;
            }

            // Pixel * row = &frame.pixels[y * frame.pitch];
            // for (int x = 0; x < width; x += 2 * 8) {
            //     __m256i batch1 = _mm256_load_si256((__m256i *) &row[x]); //NOTE: assumes memory is aligned!
            //     __m256i batch2 = _mm256_load_si256((__m256i *) &row[x + 8]);

            //     __m256i b1 = _mm256_slli_epi32(_mm256_and_si256(batch1, _mm256_set1_epi32(0x0000F0)), 4);
            //     __m256i g1 = _mm256_srli_epi32(_mm256_and_si256(batch1, _mm256_set1_epi32(0x00F000)), 8);
            //     __m256i r1 = _mm256_srli_epi32(_mm256_and_si256(batch1, _mm256_set1_epi32(0xF00000)), 20);

            //     __m256i b2 = _mm256_slli_epi32(_mm256_and_si256(batch2, _mm256_set1_epi32(0x0000F0)), 4);
            //     __m256i g2 = _mm256_srli_epi32(_mm256_and_si256(batch2, _mm256_set1_epi32(0x00F000)), 8);
            //     __m256i r2 = _mm256_srli_epi32(_mm256_and_si256(batch2, _mm256_set1_epi32(0xF00000)), 20);

            //     __m256i quant1 = _mm256_or_si256(_mm256_or_si256(b1, g1), r1);
            //     __m256i quant2 = _mm256_or_si256(_mm256_or_si256(b2, g2), r2);

            //     //TODO: what is the difference between _mm256_permute2x128_si256 and _mm256_permute2f128_si256
            //     __m256i pack1 = _mm256_shuffle_epi8(quant1, _mm256_setr_epi8(0, 1, 4, 5,  8,  9, 12, 13,
            //                                                                  2, 3, 6, 7, 10, 11, 14, 15,
            //                                                                  2, 3, 6, 7, 10, 11, 14, 15,
            //                                                                  0, 1, 4, 5,  8,  9, 12, 13));
            //     __m256i pack2 = _mm256_shuffle_epi8(quant2, _mm256_setr_epi8(0, 1, 4, 5,  8,  9, 12, 13,
            //                                                                  2, 3, 6, 7, 10, 11, 14, 15,
            //                                                                  2, 3, 6, 7, 10, 11, 14, 15,
            //                                                                  0, 1, 4, 5,  8,  9, 12, 13));
            //     __m256i perm1 = _mm256_permute2f128_si256(pack1, pack2, 0b00100000);
            //     __m256i perm2 = _mm256_permute2f128_si256(pack1, pack2, 0b00110001);

            //     __m256i result = _mm256_or_si256(perm1, perm2);
            //     //TODO: make sure this memory is aligned and use an aligned store instead
            //     _mm256_storeu_si256((__m256i *) &data[y * width + x], result);
            // }
        }
        cookedFrames.add(data);
    }
    timers.cook = get_time() - preCook;



    //season the frames (apply mask)
    float preChoice = get_time();
    MetaPaletteInfo meta = choose_meta_palette(cookedFrames, width, height);
    printf("conversion mask: %X\n", meta.cvtMask);
    printf("stride value: %d\n", meta.maxUsed);
    timers.choice = get_time() - preChoice;
    float preMask = get_time();
    if (meta.cvtMask != 0xFFF) {
        for (u16 * frame : cookedFrames) {
            for (int i : range(width * height)) {
                frame[i] &= meta.cvtMask;
            }

            // for (int i = 0; i < width * height; i += 16) {
            //     __m256i in = _mm256_loadu_si256((__m256i *) &frame[i]);
            //     __m256i out = _mm256_and_si256(in, _mm256_set1_epi16(meta.cvtMask));
            //     _mm256_storeu_si256((__m256i *) &frame[i], out);
            // }
        }
    }
    timers.mask = get_time() - preMask;
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

    StridedList lzw = { (i16 *) malloc(4096 * (meta.maxUsed + 1) * sizeof(i16)) };
    List<u8> idxBuffer = create_list<u8>(200);
    uint largestIdxBuffer = 0; //DEBUG

    for (int j : range(1, cookedFrames.len)) {
        u16 * pframe = cookedFrames[j - 1];
        u16 * cframe = cookedFrames[j];
        // printf("\n\n\n\nnew frame\n\n\n\n");

        float prePalette = get_time();
        //generate palette
        u8 tlb[4096] = {};
        struct Color3 { u8 r, g, b; };
        Color3 table[256] = {};
        int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
        bool * used = meta.used + (j - 1) * 4096;
        for (int i : range(4096)) {
            int newIdx = i & meta.cvtMask;
            if (!tlb[newIdx] && used[i]) {
                tlb[newIdx] = tableIdx;
                table[tableIdx] = {
                    (u8)((i & 0x00F) << 4 | (i & 0x00F)     ),
                    (u8)((i & 0x0F0)      | (i & 0x0F0) >> 4),
                    (u8)((i & 0xF00) >> 4 | (i & 0xF00) >> 8),
                };
                ++tableIdx;
            }
        }
        // printf("frame %d uses %d colors\n", j, tableIdx);
        timers.palette += get_time() - prePalette;

        int tableBits = bit_count(tableIdx - 1);
        int tableSize = 1 << tableBits;
        // printf("idx: %d bits: %d size: %d\n\n\n\n", tableIdx, tableBits, tableSize);

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
        buf.write_unsafe<u8>(0b10000000 | (tableBits - 1));
        // buf.write_unsafe<u8>(0b1'0'0'00'000 | (tableBits - 1));

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
    for (u16 * frame : cookedFrames)
        free(frame);
    cookedFrames.finalize();
    free(meta.used);

    timers.total = get_time() - preTotal;
    return timers;
}

void save_raw_frames(int width, int height, List<RawFrame> rawFrames, int centiSeconds) {
    printf("width: %d   height: %d   frames: %d   centiSeconds: %d\n",
            width, height, (int) rawFrames.len, centiSeconds);

    FileBuffer buf = {};
    buf.write(width);
    buf.write(height);
    buf.write<int>(rawFrames.len);
    buf.write(centiSeconds);
    for (RawFrame frame : rawFrames) {
        for (int row = 0; row < height; ++row) {
            buf.write_block(frame.pixels + row * frame.pitch, width);
        }
    }

    //write data to file
    FILE * fp = fopen("out.gif", "wb");
    assert(fp);
    fwrite(buf.block, buf.size(), 1, fp);
    fclose(fp);

    buf.finalize();
}
