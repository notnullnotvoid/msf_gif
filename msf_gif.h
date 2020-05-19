#ifndef MSF_GIF_HPP
#define MSF_GIF_HPP

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

typedef struct {
    uint32_t * pixels;
    bool * used;
    int rbits, gbits, bbits;
} CookedFrame;

typedef struct {
    FILE * fp;
    CookedFrame previousFrame;
    int width, height;
} MsfGifState;

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

//incremental API

/**
 * @param path      Relative path to the output file, as per fopen().
 * @param width     Image width in pixels - must be the same for the whole gif.
 * @param height    Image height in pixels - must be the same for the whole gif.
 * @return          The size of the file written so far, or 0 on error.
 */
size_t msf_gif_begin(MsfGifState * state, const char * path, int width, int height);
/**
 * @param pixels        Pointer to raw framebuffer data. Data must be contiguous in memory and in RGBA8 format.
 * @param centiSeconds  How long this frame should be displayed for.
 * @param maxBitDepth   Limits how many bits per pixel can be used when quantizing the gif.
 *                      The actual bit depth chosen for a given frame will be equal or less than the supplied maximum,
 *                      depending on the variety of colors used in the frame.
 *                      `maxBitDepth` will be clamped between 3 and 15. The recommended default is 15.
 *                      Lowering this value can result in faster exports and smaller gifs, but the quality will suffer.
 *                      Please experiment with this value to find what works best for your particular application.
 * @param upsideDown    Whether the image should be flipped vertically on output - useful e.g. with opengl framebuffers.
 * @return              The size of the file written so far, or 0 on error.
 */
size_t msf_gif_frame(MsfGifState * state, uint8_t * pixels, int centiSeconds, int maxBitDepth, bool upsideDown);
/**
 * @return          The size of the written file in bytes, or 0 on error.
 */
size_t msf_gif_end(MsfGifState * handle);



//all-at-once API

/**
 * @brief               An alternative to the incremental API. Its only advantage is that it is multithreaded.
 *                      All parameters shared with the incremental API are treated the same in both.
 *
 * @param maxThreads    This function will encode frames in parallel using the minimum of `maxThreads`, `frameCount`,
 *                      and the number of logical cores (a.k.a. hyperthreads) in the system.
 * @return              The size of the written file in bytes, or 0 on error.
 */
size_t msf_gif_save(const char * path, uint8_t ** frames, int frameCount, int width, int height,
    int maxBitDepth, int centiSecondsPerFrame, bool upsideDown, int maxThreads);
#ifdef __cplusplus
}
#endif //__cplusplus

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// IMPLEMENTATION                                                                                                   ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef MSF_GIF_IMPL

#ifndef MSF_GIF_ENABLE_TRACING
#define TimeFunc
#define TimeLoop(name)
#define init_profiling_thread()
#endif //MSF_GIF_ENABLE_TRACING

#include <string.h> //memcpy
#include <stdio.h> //FILE ops (fopen, etc.)
#include <stdlib.h> //malloc, etc.
#ifdef __GNUC__
static inline int bit_log(int i) { return 32 - __builtin_clz(i); }
#else //MSVC
static int bit_log(int i) { unsigned long idx; _BitScanReverse(&idx, i) + 1; return idx; }
#endif
static inline int min(int a, int b) { return a < b? a : b; }
static inline int max(int a, int b) { return b < a? a : b; }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// FileBuffer                                                                                                       ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint8_t * block;
    uint8_t * head;
    uint8_t * end;
} FileBuffer;

static void check(FileBuffer * buf, size_t bytes) {
    if (buf->head + bytes < buf->end) return;

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Frame Cooking                                                                                                    ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined (__SSE2__) || defined (_M_X64) || _M_IX86_FP == 2
#include <emmintrin.h>
#endif

static CookedFrame cook_frame(int width, int height, int pitchInBytes, int maxBitDepth, uint8_t * raw) { TimeFunc
    //bit depth for each channel
    const static int rbitdepths[13] = { 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1 };
    const static int gbitdepths[13] = { 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1 };
    const static int bbitdepths[13] = { 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1 };
    int pal = 15 - max(3, min(15, maxBitDepth));

    const static int ditherKernel[16] = {
         0 << 12,  8 << 12,  2 << 12, 10 << 12,
        12 << 12,  4 << 12, 14 << 12,  6 << 12,
         3 << 12, 11 << 12,  1 << 12,  9 << 12,
        15 << 12,  7 << 12, 13 << 12,  5 << 12,
    };

    bool * used = (bool *) malloc((1 << 15) * sizeof(bool));
    uint32_t * cooked = (uint32_t *) malloc(width * height * sizeof(uint32_t));
    int count = 0;
    TimeLoop("do") do {
        int rbits = rbitdepths[pal], gbits = gbitdepths[pal], bbits = bbitdepths[pal];
        int paletteSize = 1 << (rbits + gbits + bbits);
        memset(used, 0, paletteSize * sizeof(bool));

        int rdiff = (1 << (8 - rbits)) - 1;
        int gdiff = (1 << (8 - gbits)) - 1;
        int bdiff = (1 << (8 - bbits)) - 1;
        int rmul = (255.0f - rdiff) / 255.0f * 257;
        int gmul = (255.0f - gdiff) / 255.0f * 257;
        int bmul = (255.0f - bdiff) / 255.0f * 257;

        int gmask = ((1 << gbits) - 1) << rbits;
        int bmask = ((1 << bbits) - 1) << rbits << gbits;

        TimeLoop("cook") for (int y = 0; y < height; ++y) {
            int x = 0;

			#if defined (__SSE2__) || defined (_M_X64) || _M_IX86_FP == 2
                __m128i k = _mm_loadu_si128((__m128i *) &ditherKernel[(y & 3) * 4]);
                __m128i k2 = _mm_or_si128(_mm_srli_epi32(k, rbits), _mm_slli_epi32(_mm_srli_epi32(k, bbits), 16));
                // TimeLoop("SIMD")
                for (; x < width - 3; x += 4) {
                    uint8_t * pixels = &raw[y * pitchInBytes + x * 4];
                    __m128i p = _mm_loadu_si128((__m128i *) pixels);

                    __m128i rb = _mm_and_si128(p, _mm_set1_epi32(0x00FF00FF));
                    __m128i rb1 = _mm_mullo_epi16(rb, _mm_set_epi16(bmul, rmul, bmul, rmul, bmul, rmul, bmul, rmul));
                    __m128i rb2 = _mm_adds_epu16(rb1, k2);
                    __m128i r3 = _mm_srli_epi32(_mm_and_si128(rb2, _mm_set1_epi32(0x0000FFFF)), 16 - rbits);
                    __m128i b3 = _mm_and_si128(_mm_srli_epi32(rb2, 32 - rbits - gbits - bbits), _mm_set1_epi32(bmask));

                    __m128i g = _mm_and_si128(_mm_srli_epi32(p, 8), _mm_set1_epi32(0x000000FF));
                    __m128i g1 = _mm_mullo_epi16(g, _mm_set1_epi32(gmul));
                    __m128i g2 = _mm_adds_epu16(g1, _mm_srli_epi32(k, gbits));
                    __m128i g3 = _mm_and_si128(_mm_srli_epi32(g2, 16 - rbits - gbits), _mm_set1_epi32(gmask));

                    //TODO: does storing this as a __m128i then reading it back as a uint32_t violate strict aliasing?
                    uint32_t * c = &cooked[y * width + x];
                    __m128i out = _mm_or_si128(_mm_or_si128(r3, g3), b3);
                    _mm_storeu_si128((__m128i *) c, out);
                }
            #endif

            //scalar cleanup loop
            // TimeLoop("scalar")
            for (; x < width; ++x) {
                uint8_t * p = &raw[y * pitchInBytes + x * 4];
                int dx = x & 3, dy = y & 3;
                int k = ditherKernel[dy * 4 + dx];
                cooked[y * width + x] =
                    (min(65535, p[2] * bmul + (k >> bbits)) >> (16 - rbits - gbits - bbits) & bmask) |
                    (min(65535, p[1] * gmul + (k >> gbits)) >> (16 - rbits - gbits        ) & gmask) |
                     min(65535, p[0] * rmul + (k >> rbits)) >> (16 - rbits                );
            }

            //mark used colors
            // TimeLoop("mark used")
            for (int x = 0; x < width; ++x) {
                used[cooked[y * width + x]] = true;
            }
        }

        //count used colors
        count = 0;
        TimeLoop("count") for (int j = 0; j < paletteSize; ++j) {
            count += used[j];
        }
    } while (count >= 256 && ++pal);

    return (CookedFrame) { cooked, used, rbitdepths[pal], gbitdepths[pal], bbitdepths[pal] };
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Frame Compression                                                                                                ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint32_t bits;
    uint16_t bytes[129];
} BlockBuffer;

static inline void put_code(FileBuffer * buf, BlockBuffer * block, int bits, uint32_t code) {
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

static inline void reset(StridedList * lzw, int tableSize, int stride) { //TimeFunc
    memset(lzw->data, 0xFF, 4096 * stride * sizeof(int16_t));
    lzw->len = tableSize + 2;
    lzw->stride = stride;
}

static FileBuffer compress_frame(int width, int height, int centiSeconds, CookedFrame frame, CookedFrame previous)
{ TimeFunc
    FileBuffer buf = create_file_buffer(1024);
    StridedList lzw = { (int16_t *) malloc(4096 * 256 * sizeof(int16_t)) };

    //allocate tlb
    int totalBits = frame.rbits + frame.gbits + frame.bbits;
    int tlbSize = 1 << totalBits;
    uint8_t tlb[1 << 15]; //only 32k, so stack allocating is fine

    //generate palette
    typedef struct { uint8_t r, g, b; } Color3;
    Color3 table[256] = {};
    int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
    TimeLoop("table") for (int i = 0; i < tlbSize; ++i) {
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
                (uint8_t) (r | r >> frame.rbits | r >> (frame.rbits * 2) | r >> (frame.rbits * 3)),
                (uint8_t) (g | g >> frame.gbits | g >> (frame.gbits * 2) | g >> (frame.gbits * 3)),
                (uint8_t) (b | b >> frame.bbits | b >> (frame.bbits * 2) | b >> (frame.bbits * 3)),
            };
        }
    }

    int tableBits = bit_log(tableIdx - 1);
    int tableSize = 1 << tableBits;
    bool diff = frame.rbits == previous.rbits && frame.gbits == previous.gbits && frame.bbits == previous.bbits;

    //NOTE: because __attribute__((__packed__)) is annoyingly non-cross-platform, we do this unreadable weirdness
    char headerBytes[19] = "\x21\xF9\x04\x05\0\0\0\0" "\x2C\0\0\0\0\0\0\0\0\x80";
    memcpy(&headerBytes[4], &centiSeconds, 2);
    memcpy(&headerBytes[13], &width, 2);
    memcpy(&headerBytes[15], &height, 2);
    headerBytes[17] |= tableBits - 1;
    write_data(&buf, &headerBytes, 18);

    //local color table
    write_data(&buf, table, tableSize * sizeof(Color3));

    //image data
    BlockBuffer block = {};
    write_u8(&buf, tableBits);
    reset(&lzw, tableSize, tableIdx);

    uint8_t idxBuffer[4096];
    int idxLen = 0;
    int lastCode = diff && frame.pixels[0] == previous.pixels[0]? 0 : tlb[frame.pixels[0]];
    TimeLoop("compress") for (int i = 1; i < width * height; ++i) {
        idxBuffer[idxLen++] = diff && frame.pixels[i] == previous.pixels[i]? 0 : tlb[frame.pixels[i]];
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

    free(lzw.data);
    return buf;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Incremental API                                                                                                  ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

size_t msf_gif_begin(MsfGifState * state, const char * path, int width, int height) { TimeFunc
    if (!(state->fp = fopen(path, "wb"))) return 0;
    state->previousFrame = (CookedFrame) {};
    state->width = width;
    state->height = height;

    //NOTE: because __attribute__((__packed__)) is annoyingly non-cross-platform, we do this unreadable weirdness
    char headerBytes[33] = "GIF89a\0\0\0\0\x10\0\0" "\x21\xFF\x0BNETSCAPE2.0\x03\x01\0\0\0";
    memcpy(&headerBytes[6], &width, 2);
    memcpy(&headerBytes[8], &height, 2);
    fwrite(&headerBytes, 32, 1, state->fp);

    return max(0, ftell(state->fp));
}

size_t msf_gif_frame(MsfGifState * state, uint8_t * pixels, int centiSeconds, int maxBitDepth, bool upsideDown)
{ TimeFunc
    int pitchInBytes = upsideDown? -state->width * 4 : state->width * 4;
    uint8_t * raw = upsideDown? &pixels[state->width * 4 * (state->height - 1)] : pixels;
    CookedFrame frame = cook_frame(state->width, state->height, pitchInBytes, maxBitDepth, raw);
    FileBuffer buf = compress_frame(state->width, state->height, centiSeconds, frame, state->previousFrame);
    fwrite(buf.block, buf.head - buf.block, 1, state->fp);
    free(buf.block);
    free(frame.used);
    free(state->previousFrame.pixels);
    state->previousFrame = frame;
    return max(0, ftell(state->fp));
}

size_t msf_gif_end(MsfGifState * state) { TimeFunc
    uint8_t trailingMarker = 0x3B;
    fwrite(&trailingMarker, 1, 1, state->fp);
    size_t bytesWritten = ftell(state->fp);
    fclose(state->fp);
    free(state->previousFrame.pixels);
    return bytesWritten;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Non-Incremental API                                                                                              ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct { //TODO: rename to reflect its new use
    uint8_t ** frames;
    CookedFrame * cooked;
    FileBuffer * buffers;
    int frameCount, width, height, centiSeconds, maxBitDepth;
    bool upsideDown;
    int frameIdx;
} ThreadData;

//TODO: define atomic_post_inc() based on compiler detection
//      and disable multithreading if we're on an undetected compiler

static void * thread_cook_frames(void * arg) {
    init_profiling_thread();
    ThreadData * data = (ThreadData *) arg;
    int frameIdx = __sync_fetch_and_add(&data->frameIdx, 1);
    while (frameIdx < data->frameCount) {
        uint8_t * pixels = data->frames[frameIdx];
        int pitchInBytes = data->upsideDown? -data->width * 4 : data->width * 4;
        uint8_t * raw = data->upsideDown? &pixels[data->width * 4 * (data->height - 1)] : pixels;
        data->cooked[frameIdx] = cook_frame(data->width, data->height, pitchInBytes, data->maxBitDepth, raw);
        frameIdx = __sync_fetch_and_add(&data->frameIdx, 1);
    }
    return NULL;
}

static void * thread_compress_frames(void * arg) {
    init_profiling_thread();
    ThreadData * data = (ThreadData *) arg;
    int frameIdx = __sync_fetch_and_add(&data->frameIdx, 1);
    while (frameIdx < data->frameCount) {
        CookedFrame prev = frameIdx == 0? (CookedFrame) {} : data->cooked[frameIdx - 1];
        data->buffers[frameIdx] =
            compress_frame(data->width, data->height, data->centiSeconds, data->cooked[frameIdx], prev);
        frameIdx = __sync_fetch_and_add(&data->frameIdx, 1);
    }
    return NULL;
}

#define MAX_THREADS 64

#if defined (__unix__) || (defined (__APPLE__) && defined (__MACH__))
#include <unistd.h>
#include <pthread.h>

static void fork_join(void * (* func) (void *), void * data, int maxThreads) {
    int poolSize = min(MAX_THREADS, min(maxThreads, sysconf(_SC_NPROCESSORS_ONLN))) - 1;
    pthread_t threads[MAX_THREADS] = {};

    //we have to create a pthread_attr_t to ensure that the threads will be joinable,
    //because threads are not guaranteed to be joinable by default according to the standard
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    for (int i = 0; i < poolSize; ++i) {
        pthread_create(&threads[i], &attr, func, data);
    }
    pthread_attr_destroy(&attr);
    func(data);
    for (int i = 0; i < poolSize; ++i) {
        pthread_join(threads[i], NULL);
    }
}
#elif defined (_WIN32)
//TODO: windows version of the above function
static void fork_join(void * (* func) (void *), void * data, int maxThreads) { func(data); }
#else
static void fork_join(void * (* func) (void *), void * data, int maxThreads) { func(data); }
#endif

size_t msf_gif_save(const char * path, uint8_t ** frames, int frameCount, int width, int height,
    int maxBitDepth, int centiSeconds, bool upsideDown, int maxThreads)
{ TimeFunc
    MsfGifState state;
    msf_gif_begin(&state, path, width, height);

    CookedFrame * cookedFrames = (CookedFrame *) malloc(frameCount * sizeof(CookedFrame));
    FileBuffer * buffers = (FileBuffer *) malloc(frameCount * sizeof(FileBuffer));
    ThreadData cookData = { frames, cookedFrames, buffers, frameCount,
                            width, height, centiSeconds, maxBitDepth, upsideDown, 0 };

    //NOTE: from empirical tests, it seems like both cooking and compressing benefit slightly from hyperthreading
    fork_join(thread_cook_frames, &cookData, min(frameCount, maxThreads));
    cookData.frameIdx = 0;
    fork_join(thread_compress_frames, &cookData, min(frameCount, maxThreads));

    for (int i = 0; i < frameCount; ++i) {
        fwrite(buffers[i].block, buffers[i].head - buffers[i].block, 1, state.fp);
        free(cookedFrames[i].pixels);
        free(cookedFrames[i].used);
        free(buffers[i].block);
    }
    free(cookedFrames);
    free(buffers);

    uint8_t trailingMarker = 0x3B;
    fwrite(&trailingMarker, 1, 1, state.fp);
    size_t bytesWritten = ftell(state.fp);
    fclose(state.fp);
    return bytesWritten;
}

#endif //MSF_GIF_IMPL

#endif //MSF_GIF_HPP
