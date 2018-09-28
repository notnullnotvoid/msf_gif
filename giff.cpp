//NOTE: AVX2 optimizations and LZCNT don't appear to make GIF exporter any faster overall,
//      so we have them disabled for now
//TODO: figure out why they aren't faster

//threading TODOS:
//- multithread frame cooking
//- reimplement synchronization using condition variables
//- dynamically determine physical/logical core count
//- make threading code work on win/MSVC as well
//- put threading code behind preprocessor option

#include "giff.hpp"
#include "common.hpp"
#include "FileBuffer.hpp"

#include <pthread.h>
#include <emmintrin.h>

#ifdef _MSC_VER
    static int bit_log(int i) {
        unsigned long idx;
        _BitScanReverse(&idx, i) + 1;
        return idx;
    }
#else
    //TODO: only use this version on clang and gcc,
    static int bit_log(int i) {
        return 32 - __builtin_clz(i);
    }
    //TODO: add a generic version of bit_log() using de bruijn multiplication
#endif

//forward declaration
double get_time();

struct BlockBuffer {
    u16 bits;
    u8 bytes[257]; //up to 12 bits can be written at once, so we need 2 extra "overflow" bytes
};

static void put_code(FileBuffer * buf, BlockBuffer * block, int bits, u16 code) {
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

static void reset(StridedList * lzw, int tableSize, int stride) {
    memset(lzw->data, 0xFF, 4096 * stride * sizeof(i16));
    lzw->len = tableSize + 2;
    lzw->stride = stride;
}

struct MetaPaletteInfo {
    bool * * used;
    int rbits, gbits, bbits;
};

static bool simd_friendly(PixelFormat f) {
    return f.stride == 4 && f.ridx >= 0 && f.ridx < 4
                         && f.gidx >= 0 && f.gidx < 4
                         && f.bidx >= 0 && f.bidx < 4;
}

static void cook_frame_part(RawFrame raw, u32 * cooked, int width, int miny, int maxy,
                     int rbits, int gbits, int bbits, PixelFormat format)
{
    if (simd_friendly(format)) {
        int rshift = format.ridx * 8 + 8 - rbits;
        int gshift = format.gidx * 8 + 8 - gbits;
        int bshift = format.bidx * 8 + 8 - bbits;
        __m128i rmask = _mm_set1_epi32((1 << rbits) - 1);
        __m128i gmask = _mm_set1_epi32((1 << gbits) - 1);
        __m128i bmask = _mm_set1_epi32((1 << bbits) - 1);

        // int rshift = format.ridx * 8 + 8 - rbits;
        // int gshift = format.gidx * 8 + 8 - rbits - gbits;
        // int bshift = format.bidx * 8 + 8 - rbits - gbits - bbits;
        // // __m128i rmask = _mm_set1_epi32(((1 << rbits) - 1) << (format.ridx * 8 + 8 - rbits));
        // // __m128i gmask = _mm_set1_epi32(((1 << gbits) - 1) << (format.gidx * 8 + 8 - gbits));
        // // __m128i bmask = _mm_set1_epi32(((1 << bbits) - 1) << (format.bidx * 8 + 8 - bbits));
        // __m128i rmask = _mm_set1_epi32(((1 << rbits) - 1));
        // __m128i gmask = _mm_set1_epi32(((1 << gbits) - 1) << rbits);
        // __m128i bmask = _mm_set1_epi32(((1 << bbits) - 1) << rbits << gbits);

        for (int y = miny; y < maxy; ++y) {
            int x = 0;
            //NOTE: If we could guarantee the channels are in the right byte order (R,G,B)
            //      then we could get away with masking first and only doing 3 right-shifts.
            //      This is relevant because we might be getting hamstrung by the fact that we can
            //      only issue one vector shift instruction per cycle.
            for (; x < width - 3; x += 4) {
                u8 * p = &raw.pixels[y * raw.pitch + x * format.stride];
                u32 * c = &cooked[y * width + x];
                __m128i in = _mm_loadu_si128((__m128i *) p);

                __m128i r = _mm_and_si128(_mm_srli_epi32(in, rshift), rmask);
                __m128i g = _mm_and_si128(_mm_srli_epi32(in, gshift), gmask);
                __m128i b = _mm_and_si128(_mm_srli_epi32(in, bshift), bmask);
                g = _mm_slli_epi32(g, rbits);
                b = _mm_slli_epi32(b, rbits + gbits);
                __m128i out = _mm_or_si128(r, _mm_or_si128(g, b));

                // // __m128i r = _mm_srli_epi32(_mm_and_si128(in, rmask), rshift);
                // // __m128i g = _mm_srli_epi32(_mm_and_si128(in, gmask), gshift);
                // // __m128i b = _mm_srli_epi32(_mm_and_si128(in, bmask), bshift);
                // __m128i r = _mm_and_si128(_mm_srli_epi32(in, rshift), rmask);
                // __m128i g = _mm_and_si128(_mm_srli_epi32(in, gshift), gmask);
                // __m128i b = _mm_and_si128(_mm_srli_epi32(in, bshift), bmask);
                // __m128i out = _mm_or_si128(r, _mm_or_si128(g, b));

                _mm_storeu_si128((__m128i *) c, out);
            }

            for (; x < width; ++x) {
                u8 * p = &raw.pixels[y * raw.pitch + x * format.stride];
                cooked[y * width + x] = p[format.bidx] >> (8 - bbits) << (rbits + gbits) |
                                        p[format.gidx] >> (8 - gbits) <<  rbits          |
                                        p[format.ridx] >> (8 - rbits);
            }
        }
    } else {
        for (int y = miny; y < maxy; ++y) {
            for (int x = 0; x < width; ++x) {
                u8 * p = &raw.pixels[y * raw.pitch + x * format.stride];
                cooked[y * width + x] = p[format.bidx] >> (8 - bbits) << (rbits + gbits) |
                                        p[format.gidx] >> (8 - gbits) <<  rbits          |
                                        p[format.ridx] >> (8 - rbits);
            }
        }
    }
}

static void cook_frame_part_dithered(RawFrame raw, u32 * cooked, int width, int miny, int maxy,
                         int rbits, int gbits, int bbits, PixelFormat format)
{
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

    if (simd_friendly(format) && !(format.ridx == format.gidx || format.ridx == format.bidx)) {
        //TODO: is the cost of deriving the dither kernel significant to performance?
        int derivedKernel[8 * 8];
        for (int i = 0; i < 8 * 8; ++i) {
            int k = ditherKernel[i];
            derivedKernel[i] = k >> (rbits - 2) << format.ridx * 8 |
                               k >> (gbits - 2) << format.gidx * 8 |
                               k >> (bbits - 2) << format.bidx * 8;
        }

        int rshift = format.ridx * 8 + 8 - rbits;
        int gshift = format.gidx * 8 + 8 - gbits;
        int bshift = format.bidx * 8 + 8 - bbits;
        __m128i rmask = _mm_set1_epi32((1 << rbits) - 1);
        __m128i gmask = _mm_set1_epi32((1 << gbits) - 1);
        __m128i bmask = _mm_set1_epi32((1 << bbits) - 1);
        for (int y = miny; y < maxy; ++y) {
            int x = 0;
            for (; x < width - 3; x += 4) {
                u8 * p = &raw.pixels[y * raw.pitch + x * format.stride];
                u32 * c = &cooked[y * width + x];
                int dx = x & 7, dy = y & 7;
                int * kp = &derivedKernel[dy * 8 + dx];

                __m128i in = _mm_loadu_si128((__m128i *) p);
                __m128i k = _mm_loadu_si128((__m128i *) kp);
                in = _mm_adds_epu8(in, k);
                __m128i r = _mm_and_si128(_mm_srli_epi32(in, rshift), rmask);
                __m128i g = _mm_and_si128(_mm_srli_epi32(in, gshift), gmask);
                __m128i b = _mm_and_si128(_mm_srli_epi32(in, bshift), bmask);
                g = _mm_slli_epi32(g, rbits);
                b = _mm_slli_epi32(b, rbits + gbits);
                __m128i out = _mm_or_si128(r, _mm_or_si128(g, b));
                _mm_storeu_si128((__m128i *) c, out);
            }

            for (; x < width; ++x) {
                u8 * p = &raw.pixels[y * raw.pitch + x * format.stride];
                int dx = x & 7, dy = y & 7;
                int k = ditherKernel[dy * 8 + dx];
                cooked[y * width + x] =
                    min(255, p[format.bidx] + (k >> (bbits - 2))) >> (8 - bbits) << (rbits + gbits) |
                    min(255, p[format.gidx] + (k >> (gbits - 2))) >> (8 - gbits) <<  rbits          |
                    min(255, p[format.ridx] + (k >> (rbits - 2))) >> (8 - rbits);
            }
        }
    } else {
        for (int y = miny; y < maxy; ++y) {
            for (int x = 0; x < width; ++x) {
                u8 * p = &raw.pixels[y * raw.pitch + x * format.stride];
                int dx = x & 7, dy = y & 7;
                int k = ditherKernel[dy * 8 + dx];
                cooked[y * width + x] =
                    min(255, p[format.bidx] + (k >> (bbits - 2))) >> (8 - bbits) << (rbits + gbits) |
                    min(255, p[format.gidx] + (k >> (gbits - 2))) >> (8 - gbits) <<  rbits          |
                    min(255, p[format.ridx] + (k >> (rbits - 2))) >> (8 - rbits);
            }
        }
    }
}

static const int pixelsPerBatch = 8192;

struct CookData {
    int width, height;
    RawFrame raw;
    u32 * cooked;
    int rbits, gbits, bbits;
    int sharedLineCounter;
    bool dither;
    PixelFormat format;
    int activeThreads;
};

static void cook_frame(CookData * data) {
    int linesPerBatch = (pixelsPerBatch + data->width - 1) / data->width; //round up to nearest line number
    while (true) {
        int miny = __sync_fetch_and_add(&data->sharedLineCounter, linesPerBatch);
        if (miny >= data->height) {
            int dummy = __sync_fetch_and_add(&data->activeThreads, -1);
            // printf("retiring    miny %d   active threads %d\n", miny, dummy);
            // fflush(stdout);
            return;
        }
        // printf("beginning batch at y = %d\n", miny);
        // fflush(stdout);
        int maxy = min(miny + linesPerBatch, data->height);
        if (data->dither)
            cook_frame_part_dithered(data->raw, data->cooked, data->width, miny, maxy,
                                     data->rbits, data->gbits, data->bbits, data->format);
        else
            cook_frame_part(data->raw, data->cooked, data->width, miny, maxy,
                            data->rbits, data->gbits, data->bbits, data->format);
    }
}

struct CompressionData {
    int width, height, centiSeconds;
    int frameIdx;
    MetaPaletteInfo meta;
    List<u32 *> cookedFrames;
    List<FileBuffer> compressedFrames;
    // pthread_mutex_t mutex;
};

struct ThreadData {
    // pthread_mutex_t mutex;
    // pthread_cond_t condition;
    pthread_t parent;
    // bool compress;
    // bool dither;
    int retired;
    int activeThreads;
    CookData cookData;
    CompressionData compressionData;
};

static MetaPaletteInfo choose_meta_palette(List<RawFrame> rawFrames, List<u32 *> cookedFrames,
        int width, int height, bool dither, PixelFormat format,
        pthread_t * threads, int threadCount, ThreadData * threadData,
        DebugTimers & timers)
{
    bool * * used = (bool * *) malloc(rawFrames.len * sizeof(bool *));
    //set to null so we can spuriously free() with no effect
    memset(used, 0, rawFrames.len * sizeof(bool *));

    //bit depth for each channel
    //TODO: generate these programmatically so we can get rid of the arrays
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
        int paletteSize = 1 << (15 - minPalette);
        free(used[i - 1]);
        used[i - 1] = (bool *) malloc(paletteSize * sizeof(bool));

        float preCook = get_time();
        int linesPerBatch = (pixelsPerBatch + width - 1) / width; //round up
        int totalBatches = (height + linesPerBatch - 1) / linesPerBatch; //round up
        int maxThreads = min(threadCount + 1, totalBatches) - 1;
        threadData->cookData = { width, height, rawFrames[i - 1], frame,
                                 rbits[minPalette], gbits[minPalette], bbits[minPalette],
                                 0, dither, format, maxThreads + 1 };
        // printf("   frame %d\n", idx);
        // fflush(stdout);
        if (idx == 0) {
            printf("lines per batch: %d   total batches: %d   threads used: %d\n", linesPerBatch, totalBatches, maxThreads);
            fflush(stdout);
        }

        //wake up as many worker threads as necessary
        for (int t = 0; t < maxThreads; ++t) {
            pthread_kill(threads[t], SIGUSR1);
        }
        cook_frame(&threadData->cookData);

        //wait until all threads have finished before we continue
        //TODO: maybe start summing as soon as the first batch finishes and wait at each batch boundary?
        while (threadData->cookData.activeThreads != 0) {
            __sync_synchronize();
        }
        timers.cook += get_time() - preCook;

        //mark down which colors are used out of the full 15-bit palette
        memset(used[i - 1], 0, paletteSize * sizeof(bool));
        for (int j = 0; j < width * height; ++j)
            used[i - 1][frame[j]] = true;

        float preCount = get_time();
        //count how many fall into the meta-palette
        int count = 0;
        for (int j = 0; j < paletteSize; ++j)
            if (used[i - 1][j])
                ++count;
        timers.count += get_time() - preCount;

        if (count < 256) {
            ++idx;
        } else {
            ++minPalette;
            minCorrect = idx;
        }
    }

    return { used, rbits[minPalette], gbits[minPalette], bbits[minPalette] };
}

static void * compress_frames(CompressionData * data) {
    MetaPaletteInfo meta = data->meta;
    int width = data->width, height = data->height, centiSeconds = data->centiSeconds;
    // StridedList lzw = { (i16 *) malloc(4096 * (meta.maxUsed + 1) * sizeof(i16)) };
    StridedList lzw = { (i16 *) malloc(4096 * 256 * sizeof(i16)) };
    u8 idxBuffer[4096];
    int idxLen = 0;

    while (true) {
        // pthread_mutex_lock(&data->mutex);
        // if (data->frameIdx >= (int) data->cookedFrames.len) {
        //     pthread_mutex_unlock(&data->mutex);
        //     free(lzw.data);
        //     return nullptr;
        // }
        // int frameIdx = data->frameIdx;
        // ++data->frameIdx;
        // pthread_mutex_unlock(&data->mutex);

        int frameIdx = __sync_fetch_and_add(&data->frameIdx, 1);
        if (frameIdx >= (int) data->cookedFrames.len) {
            free(lzw.data);
            return nullptr;
        }

        u32 * pframe = data->cookedFrames[frameIdx - 1];
        u32 * cframe = data->cookedFrames[frameIdx];
        FileBuffer buf = create_file_buffer(1024);

        //allocate tlb
        int totalBits = meta.rbits + meta.gbits + meta.bbits;
        int tlbSize = 1 << totalBits;
        u8 * tlb = (u8 *) malloc(tlbSize * sizeof(u8));

        //generate palette
        struct Color3 { u8 r, g, b; };
        Color3 table[256] = {};
        int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
        for (int i = 0; i < tlbSize; ++i) {
            if (meta.used[frameIdx - 1][i]) {
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

        free(meta.used[frameIdx - 1]);
        // printf("frame %d uses %d colors\n", j, tableIdx);

        int tableBits = bit_log(tableIdx - 1);
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
        buf.write_unsafe<u8>(0b00000100 | (frameIdx != 1));
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
        put_code(&buf, &block, bit_log(lzw.len - 1), tableSize); //clear code

        int lastCode = cframe[0] == pframe[0]? 0 : tlb[cframe[0]];
        for (int i : range(1, width * height)) {
            idxBuffer[idxLen++] = cframe[i] == pframe[i]? 0 : tlb[cframe[i]];
            int code = lzw[lastCode][idxBuffer[idxLen - 1]];
            if (code < 0) {
                //write to code stream
                int codeBits = bit_log(lzw.len - 1);
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
                    lzw[lastCode][idxBuffer[idxLen - 1]] = lzw.len;
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

        free(tlb);

        //write code for leftover index buffer contents, then the end code
        put_code(&buf, &block, bit_log(lzw.len - 1), lastCode);
        put_code(&buf, &block, bit_log(lzw.len), tableSize + 1); //end code

        //flush remaining data
        if (block.bits) {
            int bytes = (block.bits + 7) / 8; //round up
            buf.check(bytes + 1);
            buf.write_unsafe<u8>(bytes);
            buf.write_block_unsafe(block.bytes, bytes);
        }

        buf.write<u8>(0); //terminating block
        idxLen = 0; //reset encoding state

        data->compressedFrames[frameIdx - 1] = buf;
    }
}

#include <signal.h>

static void * wait_for_work(void * arg) {
    ThreadData * data = (ThreadData *) arg;
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGUSR1);
    sigaddset(&set, SIGUSR2);

    while (true) {
        int sig;
        sigwait(&set, &sig);
        __sync_synchronize(); //XXX: do we need this here?
        if (sig == SIGUSR1) {
            cook_frame(&data->cookData);
        } else if (sig == SIGUSR2) {
            compress_frames(&data->compressionData);
            return nullptr;
        }
    }
}

DebugTimers save_gif(int width, int height, List<RawFrame> rawFrames, int centiSeconds,
                     const char * path, bool dither, PixelFormat format, int threadCount)
{
    DebugTimers timers = {};

    float preAmble, preTotal;
    preAmble = preTotal = get_time();
    //create worker threads for later use
    ThreadData threadData = { pthread_self() };
    threadCount = min(63, threadCount - 1);
    pthread_t threads[63];
    pthread_attr_t attr;
    //ensure that threads will be joinable (this is not guaranteed to be a default setting)
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    for (int i = 0; i < threadCount; ++i) {
        pthread_create(&threads[i], &attr, wait_for_work, &threadData);
    }
    pthread_attr_destroy(&attr);

    //allocate space for cooked frames
    List<u32 *> cookedFrames = create_list<u32 *>(rawFrames.len + 1);
    u32 * cookedMemBlock = (u32 *) malloc((rawFrames.len + 1) * width * height * sizeof(u32));
    memset(cookedMemBlock, 0, width * height * sizeof(u32)); //set dummy frame to background color
    for (int i = 0; i < (int) rawFrames.len + 1; ++i) {
        cookedFrames.add(cookedMemBlock + width * height * i);
    }



    float preChoice = get_time();
    MetaPaletteInfo meta = choose_meta_palette(
        rawFrames, cookedFrames, width, height, dither, format, threads, threadCount, &threadData, timers);
    timers.choice = get_time() - preChoice;
    printf("bits: %d, %d, %d\n", meta.rbits, meta.gbits, meta.bbits);
    timers.amble = get_time() - preAmble;



    float preCompress = get_time();
    threadData.compressionData = { width, height, centiSeconds, 1, meta, cookedFrames,
                                   create_list<FileBuffer>(cookedFrames.len - 1) };
    threadData.compressionData.compressedFrames.len = cookedFrames.len - 1;
    // pthread_mutex_init(&data.mutex, nullptr);

    //wake all worker threads to do compression work
    for (int i = 0; i < threadCount; ++i) {
        pthread_kill(threads[i], SIGUSR2);
    }

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

    compress_frames(&threadData.compressionData);

    //wait for threads to finish and round up results
    for (int i = 0; i < threadCount; ++i) {
        pthread_join(threads[i], nullptr);
    }

    for (FileBuffer b : threadData.compressionData.compressedFrames) {
        buf.write_block(b.block, b.size());
        b.finalize();
    }

    buf.write<u8>(0x3B); //trailing marker
    timers.compress = get_time() - preCompress;

    float preWrite = get_time();
    //write data to file
    FILE * fp = fopen(path, "wb");
    assert(fp);
    fwrite(buf.block, buf.size(), 1, fp);
    fclose(fp);
    timers.write = get_time() - preWrite;

    //cleanup
    buf.finalize();
    free(cookedMemBlock);
    cookedFrames.finalize();
    free(meta.used);
    threadData.compressionData.compressedFrames.finalize();

    timers.total = get_time() - preTotal;
    return timers;
}
