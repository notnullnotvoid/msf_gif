#ifndef GIF_HPP
#define GIF_HPP

#include "List.hpp"
#include "common.hpp"

struct RawFrame {
    void * pixels;
    int pitch;
};

struct DebugTimers {
	float cook, choice, amble, palette, inner, compress, write, total;
};

DebugTimers save_gif(int width, int height, List<RawFrame> rawFrames, int centiSeconds, const char * path, bool dither);

#endif
