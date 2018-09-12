#include <stdio.h>
#include <SDL2/SDL.h>

#include "common.hpp"
#include "List.hpp"
#include "giff.hpp"

u64 applicationStartupTimeValue;

double get_time() {
    u64 currentTimeValue = SDL_GetPerformanceCounter();
    u64 diffTimeValue = currentTimeValue - applicationStartupTimeValue;
    double elapsedSeconds = (double)diffTimeValue / (double)SDL_GetPerformanceFrequency();
    return elapsedSeconds;
}

struct RawBlob {
	int width, height, frames, centiSeconds;
	int pixels[1];
};

int main() {
	applicationStartupTimeValue = SDL_GetPerformanceCounter();

	printf("let's export some fuckin' gifs!\n");

	//TODO: generate this list dynamically?
	auto names = { "bouncy", "diwide-large", "diwide", "floor", "increase", "keyhole", "odd", "sky", "tiles" };

	List<RawBlob *> blobs = {};

	for (const char * name : names) {
		char * path = cat("in/", cat(name, ".rawframes"));
		printf("loading %s...\n", path);
		fflush(stdout);
		blobs.add((RawBlob *) read_entire_file(path));
	}

	printf("loaded raw blobs\n");
	fflush(stdout);

	List<DebugTimers> timers = {};

	//TODO: write both dithered and non-dithered versions each to their own folder?
	//TODO: automatically regression-test against known good versions of the GIFs?
	for (int i : range(blobs.len)) {
		RawBlob * blob = blobs[i];
		List<RawFrame> frames = {};
		for (int j : range(blob->frames)) {
			frames.add({ &blob->pixels[blob->width * blob->height * j], blob->width });
		}
		char * path = cat("out/", cat(names.begin()[i], ".gif"));
		printf("\nwriting %s      width: %d   height: %d   frames: %d   centiSeconds: %d\n",
			path, blob->width, blob->height, blob->frames, blob->centiSeconds);
		timers.add(save_gif(blob->width, blob->height, frames, blob->centiSeconds, path, true));
		printf("\n");
		fflush(stdout);
	}

	DebugTimers totals = {};
	for (int i : range(timers.len)) {
		printf("%12s      cook: %6.4f   choice: %6.4f   amble: %6.4f   palette: %6.4f   inner: %6.4f   compress: %6.4f   write: %6.4f   total: %6.4f\n",
			names.begin()[i], timers[i].cook, timers[i].choice, timers[i].amble, timers[i].palette, timers[i].inner, timers[i].compress, timers[i].write, timers[i].total);
		totals.cook += timers[i].cook;
		totals.choice += timers[i].choice;
		totals.amble += timers[i].amble;
		totals.palette += timers[i].palette;
		totals.inner += timers[i].inner;
		totals.compress += timers[i].compress;
		totals.write += timers[i].write;
		totals.total += timers[i].total;
	}
	printf("\n%12s      cook: %6.4f   choice: %6.4f   amble: %6.4f   palette: %6.4f   inner: %6.4f   compress: %6.4f   write: %6.4f   total: %6.4f\n",
		"totals", totals.cook, totals.choice, totals.amble, totals.palette, totals.inner, totals.compress, totals.write, totals.total);
	printf("lost time:   %6.4f\n\n", totals.total - (totals.amble + totals.compress + totals.write));

	//TODO: track input and output sizes and encode speed for each GIF and for all of them together

	return 0;
}
