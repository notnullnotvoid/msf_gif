#ifndef MSF_GIF_HPP
#define MSF_GIF_HPP

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus
//incremental API
/**
 * @param path      Relative path to the output file, as per fopen().
 * @param width     Image width in pixels - must be the same for the whole gif.
 * @param height    Image height in pixels - must be the same for the whole gif.
 * @return opaque   An opaque handle to be passed into subsequent incremental API calls.
 */
void * msf_gif_begin(const char * path, int width, int height);
/**
 * @param handle        The opaque handle returned by msf_begin_gif()
 * @param pixels        Pointer to raw framebuffer data. Data must be contiguous in memory and in RGBA8 format
 * @param centiSeconds  How long this frame should be displayed for, in hundredths of a second
 * @param maxBitDepth   Limits how many bits per pixel can be used when quantizing the gif.
 *                      The actual bit depth chosen for a given frame will be equal or less than the supplied maximum,
 *                      depending on the variety of colors used in the frame.
 *                      `maxBitDepth` will be clamped between 3 and 15. The recommended default is 15.
 *                      Lowering this value can result in faster exports and smaller gifs, but the quality will suffer.
 *                      Please experiment with this value to find what works best for your particular application.
 * @param upsideDown    Whether the image should be flipped vertically on output - useful e.g. with opengl framebuffers
 */
void msf_gif_frame(void * handle, uint8_t * pixels, int centiSeconds, int maxBitDepth, bool upsideDown);
/**
 * @param handle    The opaque handle returned by msf_begin_gif(). It will be invalid after this call.
 * @return          The size of the written file in bytes.
 */
size_t msf_gif_end(void * handle);



//non-incremental API
size_t msf_gif_save(const char * path, uint8_t ** rawFrames, int rawFrameCount, int width, int height,
    int maxBitDepth, int centiSecondsPerFrame, bool upsideDown, int maxThreads);
#ifdef __cplusplus
}
#endif //__cplusplus

#endif //MSF_GIF_HPP
