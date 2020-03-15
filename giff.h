#ifndef GIFF_HPP
#define GIFF_HPP

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct {
    uint8_t * pixels;
    int pitch;
} RawFrame;

typedef struct {
    ptrdiff_t ridx, gidx, bidx, stride;
} PixelFormat;

#ifdef __cplusplus
extern "C"
#endif //__cplusplus
size_t save_gif(RawFrame * rawFrames, int rawFrameCount, int width, int height, int centiSeconds,
              const char * path, bool dither, PixelFormat format,
              int cookThreadCount, int compressThreadCount);

#define GIFF_FORMAT_RGBA { 0, 1, 2, 4 }
#define GIFF_FORMAT_BGRA { 2, 1, 0, 4 }
#define GIFF_FORMAT_ARGB { 1, 2, 3, 4 }
#define GIFF_FORMAT_ABGR { 3, 2, 1, 4 }
#define GIFF_FORMAT_RGB  { 0, 1, 2, 3 }
#define GIFF_FORMAT_BGR  { 2, 1, 0, 3 }
#define GIFF_FORMAT_YA   { 0, 0, 0, 2 }
#define GIFF_FORMAT_AY   { 1, 1, 1, 2 }
#define GIFF_FORMAT_Y    { 0, 0, 0, 1 }

#endif //GIFF_HPP
