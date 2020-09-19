/*
HOW TO USE:

    In exactly one translation unit (.c or .cpp file), #define MSF_GIF_IMPL before including the header, like so:

    #define MSF_GIF_IMPL
    #include "msf_gif.h"

    Everywhere else, just include the header like normal.


USAGE EXAMPLE:

    int width = 480, height = 320, centisecondsPerFrame = 5, bitDepth = 15;
    MsfGifState gifState = {};
    msf_gif_begin(&gifState, "example.gif", width, height);
    msf_gif_frame(&gifState, ..., bitDepth, centisecondsPerFrame, width * 4); //frame 1
    msf_gif_frame(&gifState, ..., bitDepth, centisecondsPerFrame, width * 4); //frame 2
    msf_gif_frame(&gifState, ..., bitDepth, centisecondsPerFrame, width * 4); //frame 3, etc...
    msf_gif_end(&gifState);

Detailed function documentation can be found in the header section below.

REPLACING MALLOC AND FWRITE:

    This library uses malloc+realloc+free internally for memory allocation and fopen+fwrite+fclose for file output.
    To facilitate custom memory management and I/O, these calls go through macros, which can be redefined.
    These macros provide additional information (such as previous allocation size when calling realloc and free),
    as well as a context pointer, in order to be usable with a wider range of allocators.
    In order to replace them, simply #define the relevant macros in the same place where you #define MSF_GIF_IMPL.
    The allocator macros are MSF_GIF_MALLOC, MSF_GIF_REALLOC, and MSF_GIF_FREE.
    The file output macros are MSF_GIF_FOPEN, MSF_GIF_FWRITE, and MSF_GIF_FCLOSE.
    Search for their default definitions below to see the exactly what arguments they take.
    If your allocator needs a context pointer, you can set the `customAllocatorContext` field of the MsfGifState struct
    before calling msf_gif_begin(), and it will be passed to all subsequent allocator macro calls.
    The same goes for file I/O macros, using the `customOutputContext` field.

See end of file for license information.
*/

//version 1.1

#ifndef MSF_GIF_H
#define MSF_GIF_H

#include <stdint.h>
#include <stddef.h>

typedef struct {
    uint32_t * pixels;
    uint8_t * used;
    int rbits, gbits, bbits;
} MsfCookedFrame;

typedef struct {
    void * fp;
    MsfCookedFrame previousFrame;
    int width, height;
    int totalBytesWritten;
    void * customAllocatorContext;
    void * customOutputContext;
} MsfGifState;

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

/**
 * @param outputFilePath       Relative path to the output file, as a null-terminated string of UTF-8 bytes.
 * @param width                Image width in pixels - must be the same for the whole gif.
 * @param height               Image height in pixels - must be the same for the whole gif.
 * @return                     The size of the file written so far, or 0 on error.
 */
size_t msf_gif_begin(MsfGifState * handle, const char * outputFilePath, int width, int height);

/**
 * @param pixelData            Pointer to raw framebuffer data. Rows must be contiguous in memory, in RGBA8 format.
 *                             Note: This function does NOT free `pixelData`. You must free it yourself afterwards.
 * @param centiSecondsPerFrame How many hundredths of a second this frame should be displayed for.
 *                             Note: This being specified in centiseconds is a limitation of the GIF format.
 * @param maxBitDepth          Limits how many bits per pixel can be used when quantizing the gif.
 *                             The actual bit depth chosen for a given frame will be less than or equal to
 *                             the supplied maximum, depending on the variety of colors used in the frame.
 *                             `maxBitDepth` will be clamped between 3 and 15. The recommended default is 15.
 *                             Lowering this value can result in faster exports and smaller gifs,
 *                             but the quality may suffer.
 *                             Please experiment with this value to find what works best for your application.
 * @param pitchInBytes         The number of bytes from the beginning of one row of pixels to the beginning of the next.
 *                             If you want to flip the image, just pass in a negative pitch.
 * @return                     The size of the file written so far, or 0 on error.
 */
size_t msf_gif_frame(MsfGifState * handle,
                     uint8_t * pixelData, int centiSecondsPerFame, int maxBitDepth, int pitchInBytes);

/**
 * @return                     The size of the written file in bytes, or 0 on error.
 */
size_t msf_gif_end(MsfGifState * handle);

#ifdef __cplusplus
}
#endif //__cplusplus

#endif //MSF_GIF_H

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// IMPLEMENTATION                                                                                                   ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef MSF_GIF_IMPL
#ifndef MSF_GIF_ALREADY_IMPLEMENTED_IN_THIS_TRANSLATION_UNIT
#define MSF_GIF_ALREADY_IMPLEMENTED_IN_THIS_TRANSLATION_UNIT

//ensure the library user has either defined all of malloc/realloc/free, or none
#if defined(MSF_GIF_MALLOC) && defined(MSF_GIF_REALLOC) && defined(MSF_GIF_FREE)
#elif !defined(MSF_GIF_MALLOC) && !defined(MSF_GIF_REALLOC) && !defined(MSF_GIF_FREE)
#else
#error "You must either define all of MSF_GIF_MALLOC, MSF_GIF_REALLOC, and MSF_GIF_FREE, or define none of them"
#endif

//provide default allocator definitions that redirect to the standard global allocator
#if !defined(MSF_GIF_MALLOC)
#include <stdlib.h> //malloc, etc.
#define MSF_GIF_MALLOC(contextPointer, newSize) malloc(newSize)
#define MSF_GIF_REALLOC(contextPointer, oldMemory, oldSize, newSize) realloc(oldMemory, newSize)
#define MSF_GIF_FREE(contextPointer, oldMemory, oldSize) free(oldMemory)
#endif

//ensure the library user has either defined all of fopen/fwrite/fclose, or none
#if defined(MSF_GIF_FOPEN) && defined(MSF_GIF_FWRITE) && defined(MSF_GIF_FCLOSE)
#elif !defined(MSF_GIF_FOPEN) && !defined(MSF_GIF_FWRITE) && !defined(MSF_GIF_FCLOSE)
#else
#error "You must either define all of MSF_GIF_FOPEN, MSF_GIF_FWRITE, and MSF_GIF_FCLOSE, or define none of them"
#endif

//provide default file ops that redirect to the standard library ones
#if !defined(MSF_GIF_FOPEN)
#include <stdio.h> //FILE ops (fopen, etc.)
#define MSF_GIF_FOPEN(contextPointer, filePath) fopen(filePath, "wb")
#define MSF_GIF_FWRITE(contextPointer, filePointer, data, dataSize) fwrite(data, dataSize, 1, (FILE *) filePointer)
#define MSF_GIF_FCLOSE(contextPointer, filePointer) fclose((FILE *) filePointer)
#endif

//instrumentation for capturing profiling traces (useless for the library user, but useful for the library author)
#ifdef MSF_GIF_ENABLE_TRACING
#define MsfTimeFunc TimeFunc
#define MsfTimeLoop TimeLoop
#define msf_init_profiling_thread init_profiling_thread
#else
#define MsfTimeFunc
#define MsfTimeLoop(name)
#define msf_init_profiling_thread()
#endif //MSF_GIF_ENABLE_TRACING

#include <string.h> //memcpy

//TODO: use compiler-specific notation to force-inline functions currently marked inline
#if defined(__GNUC__) //gcc, clang
static inline int msf_bit_log(int i) { return 32 - __builtin_clz(i); }
#elif defined(_MSC_VER) //msvc
#include <intrin.h>
static inline int msf_bit_log(int i) { unsigned long idx; _BitScanReverse(&idx, i); return idx + 1; }
#else //fallback implementation for other compilers
//from https://stackoverflow.com/a/31718095/3064745 - thanks!
static inline int msf_bit_log(int i) {
    static const int MultiplyDeBruijnBitPosition[32] = {
        0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
        8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31,
    };
    i |= i >> 1;
    i |= i >> 2;
    i |= i >> 4;
    i |= i >> 8;
    i |= i >> 16;
    return MultiplyDeBruijnBitPosition[(uint32_t)(i * 0x07C4ACDDU) >> 27] + 1;
}
#endif
static inline int msf_imin(int a, int b) { return a < b? a : b; }
static inline int msf_imax(int a, int b) { return b < a? a : b; }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// MsfFileBuffer                                                                                                    ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint8_t * block;
    uint8_t * head;
    uint8_t * end;
} MsfFileBuffer;

static int msf_fb_check(void * allocContext, MsfFileBuffer * buf, size_t bytes) {
    if (buf->head + bytes < buf->end) return 1;

    size_t byte = buf->head - buf->block;
    size_t size = buf->end - buf->block;

    //done in a loop so adding payloads larger than the current buffer size will work
    while (byte + bytes >= size) {
        size = size * 2 + 1;
    }

    void * moved = MSF_GIF_REALLOC(allocContext, buf->block, buf->end - buf->block, size);
    if (!moved) { MSF_GIF_FREE(allocContext, buf->block, buf->end - buf->block); return 0; }
    buf->block = (uint8_t *) moved;
    buf->head = buf->block + byte;
    buf->end = buf->block + size;
    return 1;
}

static inline void msf_fb_write_data(MsfFileBuffer * buf, void * data, size_t bytes) {
    memcpy(buf->head, data, bytes);
    buf->head += bytes;
}

static inline void msf_fb_write_u8(MsfFileBuffer * buf, uint8_t data) {
    *buf->head++ = data;
}

static MsfFileBuffer msf_create_file_buffer(void * allocContext, size_t bytes) {
    uint8_t * block = (uint8_t *) MSF_GIF_MALLOC(allocContext, bytes);
    MsfFileBuffer ret = { block, block, block + bytes };
    return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Frame Cooking                                                                                                    ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined (__SSE2__) || defined (_M_X64) || _M_IX86_FP == 2
#include <emmintrin.h>
#endif

static const int msfUsedAllocSize = (1 << 15) * sizeof(uint8_t);

static MsfCookedFrame msf_cook_frame(void * allocContext,
    int width, int height, int pitchInBytes, int maxBitDepth, uint8_t * raw)
{ MsfTimeFunc
	MsfCookedFrame ret = {0};

    //bit depth for each channel
    const static int rbitdepths[13] = { 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1 };
    const static int gbitdepths[13] = { 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1 };
    const static int bbitdepths[13] = { 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1 };
    int pal = 15 - msf_imax(3, msf_imin(15, maxBitDepth));

    const static int ditherKernel[16] = {
         0 << 12,  8 << 12,  2 << 12, 10 << 12,
        12 << 12,  4 << 12, 14 << 12,  6 << 12,
         3 << 12, 11 << 12,  1 << 12,  9 << 12,
        15 << 12,  7 << 12, 13 << 12,  5 << 12,
    };

    uint8_t * used = (uint8_t *) MSF_GIF_MALLOC(allocContext, msfUsedAllocSize);
    if (!used) return ret;
    uint32_t * cooked = (uint32_t *) MSF_GIF_MALLOC(allocContext, width * height * sizeof(uint32_t));
    if (!cooked) { MSF_GIF_FREE(allocContext, used, msfUsedAllocSize); return ret; }
    int count = 0;
    MsfTimeLoop("do") do {
        int rbits = rbitdepths[pal], gbits = gbitdepths[pal], bbits = bbitdepths[pal];
        int paletteSize = 1 << (rbits + gbits + bbits);
        memset(used, 0, paletteSize * sizeof(uint8_t));

        //TODO: document what this math does and why it's correct
        int rdiff = (1 << (8 - rbits)) - 1;
        int gdiff = (1 << (8 - gbits)) - 1;
        int bdiff = (1 << (8 - bbits)) - 1;
        short rmul = (short) ((255.0f - rdiff) / 255.0f * 257);
        short gmul = (short) ((255.0f - gdiff) / 255.0f * 257);
        short bmul = (short) ((255.0f - bdiff) / 255.0f * 257);

        int gmask = ((1 << gbits) - 1) << rbits;
        int bmask = ((1 << bbits) - 1) << rbits << gbits;

        MsfTimeLoop("cook") for (int y = 0; y < height; ++y) {
            int x = 0;

            #if defined (__SSE2__) || defined (_M_X64) || _M_IX86_FP == 2
                __m128i k = _mm_loadu_si128((__m128i *) &ditherKernel[(y & 3) * 4]);
                __m128i k2 = _mm_or_si128(_mm_srli_epi32(k, rbits), _mm_slli_epi32(_mm_srli_epi32(k, bbits), 16));
                // MsfTimeLoop("SIMD")
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
            // MsfTimeLoop("scalar")
            for (; x < width; ++x) {
                uint8_t * p = &raw[y * pitchInBytes + x * 4];
                int dx = x & 3, dy = y & 3;
                int k = ditherKernel[dy * 4 + dx];
                cooked[y * width + x] =
                    (msf_imin(65535, p[2] * bmul + (k >> bbits)) >> (16 - rbits - gbits - bbits) & bmask) |
                    (msf_imin(65535, p[1] * gmul + (k >> gbits)) >> (16 - rbits - gbits        ) & gmask) |
                     msf_imin(65535, p[0] * rmul + (k >> rbits)) >> (16 - rbits                );
            }

            //mark used colors
            // MsfTimeLoop("mark used")
            for (int x = 0; x < width; ++x) {
                used[cooked[y * width + x]] = 1;
            }
        }

        //count used colors
        count = 0;
        MsfTimeLoop("count") for (int j = 0; j < paletteSize; ++j) {
            count += used[j];
        }
    } while (count >= 256 && ++pal);

	ret.pixels = cooked;
	ret.used = used;
	ret.rbits = rbitdepths[pal];
	ret.gbits = gbitdepths[pal];
	ret.bbits = bbitdepths[pal];
	return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Frame Compression                                                                                                ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    uint32_t bits;
    uint16_t bytes[129];
} MsfBlockBuffer;

static inline int msf_put_code(void * allocContext,
    MsfFileBuffer * buf, MsfBlockBuffer * block, int bits, uint32_t code)
{
    //insert new code into block buffer
    int idx = block->bits / 16;
    int bit = block->bits % 16;
    block->bytes[idx + 0] |= code <<       bit ;
    block->bytes[idx + 1] |= code >> (16 - bit);
    block->bits += bits;

    //flush the block buffer if it's full
    if (block->bits >= 255 * 8) {
        if (!msf_fb_check(allocContext, buf, 256)) return 0;
        msf_fb_write_u8(buf, 255);
        msf_fb_write_data(buf, block->bytes, 255);

        block->bits -= 255 * 8;
        block->bytes[0] = block->bytes[127] >> 8 | block->bytes[128] << 8;
        memset(block->bytes + 1, 0, 256);
    }

    return 1;
}

typedef struct {
    int16_t * data;
    int len;
    int stride;
} MsfStridedList;

static inline void msf_lzw_reset(MsfStridedList * lzw, int tableSize, int stride) { //MsfTimeFunc
    memset(lzw->data, 0xFF, 4096 * stride * sizeof(int16_t));
    lzw->len = tableSize + 2;
    lzw->stride = stride;
}

static MsfFileBuffer msf_compress_frame(void * allocContext, int width, int height, int centiSeconds,
                                        MsfCookedFrame frame, MsfCookedFrame previous)
{ MsfTimeFunc
	MsfFileBuffer ret = {0};

    MsfFileBuffer buf = msf_create_file_buffer(allocContext, 1024);
    if (!buf.block) return ret;
    int lzwAllocSize = 4096 * 256 * sizeof(int16_t);
    MsfStridedList lzw = { (int16_t *) MSF_GIF_MALLOC(allocContext, lzwAllocSize) };
    if (!lzw.data) { MSF_GIF_FREE(allocContext, buf.block, buf.end - buf.block); return ret; }

    //allocate tlb
    int totalBits = frame.rbits + frame.gbits + frame.bbits;
    int tlbSize = 1 << totalBits;
    uint8_t tlb[1 << 15]; //only 32k, so stack allocating is fine

    //generate palette
    typedef struct { uint8_t r, g, b; } Color3;
    Color3 table[256] = { {0} };
    int tableIdx = 1; //we start counting at 1 because 0 is the transparent color
    MsfTimeLoop("table") for (int i = 0; i < tlbSize; ++i) {
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
            table[tableIdx].r = r | r >> frame.rbits | r >> (frame.rbits * 2) | r >> (frame.rbits * 3);
            table[tableIdx].g = g | g >> frame.gbits | g >> (frame.gbits * 2) | g >> (frame.gbits * 3);
            table[tableIdx].b = b | b >> frame.bbits | b >> (frame.bbits * 2) | b >> (frame.bbits * 3);
            ++tableIdx;
        }
    }

    int tableBits = msf_bit_log(tableIdx - 1);
    int tableSize = 1 << tableBits;
    int hasDiffPal = frame.rbits == previous.rbits && frame.gbits == previous.gbits && frame.bbits == previous.bbits;

    //NOTE: because __attribute__((__packed__)) is annoyingly compiler-specific, we do this unreadable weirdness
    char headerBytes[19] = "\x21\xF9\x04\x05\0\0\0\0" "\x2C\0\0\0\0\0\0\0\0\x80";
    memcpy(&headerBytes[4], &centiSeconds, 2);
    memcpy(&headerBytes[13], &width, 2);
    memcpy(&headerBytes[15], &height, 2);
    headerBytes[17] |= tableBits - 1;
    msf_fb_write_data(&buf, headerBytes, 18);

    //local color table
    msf_fb_write_data(&buf, table, tableSize * sizeof(Color3));

    //image data
    MsfBlockBuffer block = {0};
    msf_fb_write_u8(&buf, tableBits);
    msf_lzw_reset(&lzw, tableSize, tableIdx);

    uint8_t idxBuffer[4096];
    int idxLen = 0;
    int lastCode = hasDiffPal && frame.pixels[0] == previous.pixels[0]? 0 : tlb[frame.pixels[0]];
    MsfTimeLoop("compress") for (int i = 1; i < width * height; ++i) {
        idxBuffer[idxLen++] = hasDiffPal && frame.pixels[i] == previous.pixels[i]? 0 : tlb[frame.pixels[i]];
        int code = (&lzw.data[lastCode * lzw.stride])[idxBuffer[idxLen - 1]];
        if (code < 0) {
            //write to code stream
            int codeBits = msf_bit_log(lzw.len - 1);
            if (!msf_put_code(allocContext, &buf, &block, codeBits, lastCode)) {
                MSF_GIF_FREE(allocContext, lzw.data, lzwAllocSize);
                return ret;
            }

            //NOTE: [I THINK] we need to leave room for 2 more codes (leftover and end code)
            //      because we don't ever reset the table after writing the leftover bits
            //Q: is my thinking correct on this one?
            //Q: why can't we just check when writing out those codes? too verbose? can't we factor to a funtion?
            if (lzw.len > 4094) {
                //reset buffer code table
                if (!msf_put_code(allocContext, &buf, &block, codeBits, tableSize)) {
                    MSF_GIF_FREE(allocContext, lzw.data, lzwAllocSize);
                    return ret;
                }
                msf_lzw_reset(&lzw, tableSize, tableIdx);
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
    MSF_GIF_FREE(allocContext, lzw.data, lzwAllocSize);
    if (!msf_put_code(allocContext, &buf, &block, msf_bit_log(lzw.len - 1), lastCode)) { return ret; }
    if (!msf_put_code(allocContext, &buf, &block, msf_bit_log(lzw.len), tableSize + 1)) { return ret; }

    //flush remaining data
    if (block.bits) {
        int bytes = (block.bits + 7) / 8; //round up
        if (!msf_fb_check(allocContext, &buf, bytes + 1)) return ret;
        msf_fb_write_u8(&buf, bytes);
        msf_fb_write_data(&buf, block.bytes, bytes);
    }

    if (!msf_fb_check(allocContext, &buf, 1)) return ret;
    msf_fb_write_u8(&buf, 0); //terminating block

    return buf;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Incremental API                                                                                                  ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

size_t msf_gif_begin(MsfGifState * handle, const char * outputFilePath, int width, int height) { MsfTimeFunc
    //TODO: convert this to UTF-16 to correctly handle unicode on windows!?!?
    //      (or just say "do it yourself" now that replacing fopen is a thing?)
    if (!(handle->fp = MSF_GIF_FOPEN(handle->customOutputContext, outputFilePath))) return 0;
    MsfCookedFrame empty = {0}; //god I hate MSVC...
    handle->previousFrame = empty;
    handle->width = width;
    handle->height = height;
    handle->totalBytesWritten = 0;

    //NOTE: because __attribute__((__packed__)) is annoyingly compiler-specific, we do this unreadable weirdness
    char headerBytes[33] = "GIF89a\0\0\0\0\x10\0\0" "\x21\xFF\x0BNETSCAPE2.0\x03\x01\0\0\0";
    memcpy(&headerBytes[6], &width, 2);
    memcpy(&headerBytes[8], &height, 2);
    if (!MSF_GIF_FWRITE(handle->customOutputContext, handle->fp, headerBytes, 32)) {
        MSF_GIF_FCLOSE(handle->customOutputContext, handle->fp);
        return 0;
    }
    handle->totalBytesWritten += 32;
    return handle->totalBytesWritten;
}

size_t msf_gif_frame(MsfGifState * handle,
                     uint8_t * pixelData, int centiSecondsPerFame, int maxBitDepth, int pitchInBytes)
{ MsfTimeFunc
    if (pitchInBytes == 0) pitchInBytes = handle->width * 4;
    if (pitchInBytes < 0) pixelData -= pitchInBytes * (handle->height - 1);
    MsfCookedFrame frame = msf_cook_frame(handle->customAllocatorContext,
        handle->width, handle->height, pitchInBytes, maxBitDepth, pixelData);
    if (!frame.pixels) { MSF_GIF_FCLOSE(handle->customOutputContext, handle->fp); return 0; }
    MsfFileBuffer buf = msf_compress_frame(handle->customAllocatorContext,
        handle->width, handle->height, centiSecondsPerFame, frame, handle->previousFrame);
    if (!buf.block) { MSF_GIF_FCLOSE(handle->customOutputContext, handle->fp); return 0; }
    if (!MSF_GIF_FWRITE(handle->customOutputContext, handle->fp, buf.block, buf.head - buf.block)) {
        MSF_GIF_FCLOSE(handle->customOutputContext, handle->fp);
        return 0;
    }
    handle->totalBytesWritten += buf.head - buf.block;
    MSF_GIF_FREE(handle->customAllocatorContext, buf.block, buf.end - buf.block);
    MSF_GIF_FREE(handle->customAllocatorContext, frame.used, msfUsedAllocSize);
    MSF_GIF_FREE(handle->customAllocatorContext,
        handle->previousFrame.pixels, handle->width * handle->height * sizeof(uint32_t));
    handle->previousFrame = frame;
    return handle->totalBytesWritten;
}

size_t msf_gif_end(MsfGifState * handle) { MsfTimeFunc
    int allocSize = handle->width * handle->height * sizeof(uint32_t);
    uint8_t trailingMarker = 0x3B;
    if (!MSF_GIF_FWRITE(handle->customOutputContext, handle->fp, &trailingMarker, 1)) {
        MSF_GIF_FCLOSE(handle->customOutputContext, handle->fp);
        MSF_GIF_FREE(handle->customAllocatorContext, handle->previousFrame.pixels, allocSize);
        return 0;
    }
    handle->totalBytesWritten += 1;
    if (MSF_GIF_FCLOSE(handle->customOutputContext, handle->fp)) return 0;
    MSF_GIF_FREE(handle->customAllocatorContext, handle->previousFrame.pixels, allocSize);
    return handle->totalBytesWritten;
}

#endif //MSF_GIF_ALREADY_IMPLEMENTED_IN_THIS_TRANSLATION_UNIT
#endif //MSF_GIF_IMPL

/*
------------------------------------------------------------------------------
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2020 Miles Fogle
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
*/
