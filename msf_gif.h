#ifndef MSF_GIF_HPP
#define MSF_GIF_HPP

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
#endif //__cplusplus
size_t msf_save_gif(uint8_t ** rawFrames, int rawFrameCount, int width, int height, int centiSeconds,
              const char * path);

#endif //MSF_GIF_HPP
