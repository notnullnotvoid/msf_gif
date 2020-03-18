#ifndef MSF_GIF_HPP
#define MSF_GIF_HPP

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus
size_t msf_save_gif(uint8_t ** rawFrames, int rawFrameCount,
	int width, int height, bool flipOutput, int centiSeconds, int quality, const char * path);

//incremental API
void * msf_begin_gif(const char * path, int width, int height, int centiSecondsPerFrame, int quality, bool upsideDown);
void msf_gif_frame(void * handle, uint8_t * pixels);
size_t msf_end_gif(void * handle);
#ifdef __cplusplus
}
#endif //__cplusplus

#endif //MSF_GIF_HPP
