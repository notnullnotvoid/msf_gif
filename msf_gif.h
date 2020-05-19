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

#endif //MSF_GIF_HPP
