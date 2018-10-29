//TODO: write timing code that doesn't rely on instrumentation inside giff.c

#include "giff.h"
#include "List.hpp"
#include "common.hpp"

#include <stdio.h>
#include <SDL2/SDL.h>

u64 applicationStartupTimeValue;

extern "C" double get_time() {
    u64 currentTimeValue = SDL_GetPerformanceCounter();
    u64 diffTimeValue = currentTimeValue - applicationStartupTimeValue;
    double elapsedSeconds = (double)diffTimeValue / (double)SDL_GetPerformanceFrequency();
    return elapsedSeconds;
    // return 0;
}

struct RawBlob {
    int width, height, frames, centiSeconds;
    int pixels[1];
};

// #include <unistd.h>

int main() {
    applicationStartupTimeValue = SDL_GetPerformanceCounter();

    // printf("logical cores available: %ld\n", sysconf(_SC_NPROCESSORS_ONLN));

    printf("let's export some fuckin' gifs!\n");

    //TODO: generate this list dynamically?
    auto names = { "bouncy", "diwide-large", "diwide", "floor", "increase", "keyhole", "odd", "sky", "tiles" };
    // auto names = { "bouncy", "diwide-large", "diwide", "floor", "increase", "keyhole", "odd", "tiles" };
    // auto names = { "bouncy", "diwide", "floor", "keyhole" };

    List<RawBlob *> blobs = {};

    for (const char * name : names) {
        char * path = cat("in/", cat(name, ".rawframes"));
        printf("loading %s...\n", path);
        fflush(stdout);
        blobs.add((RawBlob *) read_entire_file(path));
    }

    printf("loaded raw blobs\n");
    fflush(stdout);

    // List<DebugTimers> timers = {};
    // size_t totalSize = 0;

    //TODO: write both dithered and non-dithered versions each to their own folder?
    //TODO: automatically regression-test against known good versions of the GIFs?
    for (int i : range(blobs.len)) {
        RawBlob * blob = blobs[i];
        List<RawFrame> frames = {};
        for (int j : range(blob->frames)) {
            frames.add({ (u8 *) &blob->pixels[blob->width * blob->height * j], blob->width * 4 });
            for (int k : range(blob->width * blob->height)) {
                u8 * p = (u8 *) &blob->pixels[blob->width * blob->height * j + k];
                swap(p[0], p[2]);
            }
        }
        char * path = cat("out/", cat(names.begin()[i], ".gif"));
        printf("\nwriting %s      width: %d   height: %d   frames: %d   centiSeconds: %d\n",
            path, blob->width, blob->height, blob->frames, blob->centiSeconds);
        // timers.add(save_gif(blob->width, blob->height, frames, blob->centiSeconds, path, true, GIFF_FORMAT_RGBA, 4, 8));
        // timers.add(save_gif(frames.data, frames.len, blob->width, blob->height, blob->centiSeconds, path, true, GIFF_FORMAT_RGBA, 4, 8));
        save_gif(frames.data, frames.len, blob->width, blob->height, blob->centiSeconds, path, true, GIFF_FORMAT_RGBA, 4, 8);
        fflush(stdout);
    }

    // DebugTimers totals = {};
    // for (int i : range(timers.len)) {
    //     int size = blobs[i]->frames * blobs[i]->width * blobs[i]->height * 4;
    //     totalSize += size;
    //     printf("%14s      cook: %5.3f   count: %5.3f   choice: %5.3f   amble: %5.3f   compress: %5.3f   write: %5.3f   total: %5.3f   lost: %5.3f   %4.2f GB/s in   %5.2f MB/s out\n",
    //         names.begin()[i], timers[i].cook, timers[i].count, timers[i].choice, timers[i].amble, timers[i].compress, timers[i].write, timers[i].total,
    //         timers[i].total - (timers[i].amble + timers[i].compress + timers[i].write), size / timers[i].total / 1024 / 1024 / 1024, timers[i].size / timers[i].total / 1024 / 1024);
    //     totals.cook += timers[i].cook;
    //     totals.count += timers[i].count;
    //     totals.choice += timers[i].choice;
    //     totals.amble += timers[i].amble;
    //     totals.palette += timers[i].palette;
    //     totals.inner += timers[i].inner;
    //     totals.compress += timers[i].compress;
    //     totals.write += timers[i].write;
    //     totals.total += timers[i].total;
    //     totals.size += timers[i].size;
    // }
    // printf("\n%14s      cook: %5.3f   count: %5.3f   choice: %5.3f   amble: %5.3f   compress: %5.3f   write: %5.3f   total: %5.3f   lost: %5.3f   %4.2f GB/s in   %5.2f MB/s out\n\n",
    //     "totals", totals.cook, totals.count, totals.choice, totals.amble, totals.compress, totals.write, totals.total,
    //     totals.total - (totals.amble + totals.compress + totals.write), totalSize / totals.total / 1024 / 1024 / 1024, totals.size / totals.total / 1024 / 1024);

    // printf("%ld in, %d out\n\n", totalSize, totals.size);

    return 0;
}
