#ifndef GIF_HPP
#define GIF_HPP

#include "List.hpp"
// #include "blit.hpp" //TODO: get rid of this dependency so it will be easy to drop into new projects
#include "common.hpp"

struct Pixel {
	u8 b, g, r, a;
};

struct RawFrame {
    Pixel * pixels;
    int pitch;
};

struct DebugTimers {
	float cook, choice, amble, palette, inner, compress, write, total;
};

DebugTimers save_gif(int width, int height, List<RawFrame> rawFrames, int centiSeconds, const char * path, bool dither);
void save_raw_frames(int width, int height, List<RawFrame> rawFrames, int centiSeconds);

#endif
