#include "giff.hpp"
#include "List.hpp"
#include "common.hpp"

#include <stdio.h>
#include <SDL2/SDL.h>

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

// #include <unistd.h>

int main() {
    applicationStartupTimeValue = SDL_GetPerformanceCounter();

    // printf("logical cores available: %ld\n", sysconf(_SC_NPROCESSORS_ONLN));

    printf("let's export some fuckin' gifs!\n");

    //TODO: generate this list dynamically?
    auto names = { "bouncy", "diwide-large", "diwide", "floor", "increase", "keyhole", "odd", "sky", "tiles" };
    // auto names = { "bouncy", "diwide-large", "diwide", "floor", "increase", "keyhole", "odd", "tiles" };

    List<RawBlob *> blobs = {};

    for (const char * name : names) {
        char * path = cat("in/", cat(name, ".rawframes"));
        printf("loading %s...\n", path);
        fflush(stdout);
        blobs.add((RawBlob *) read_entire_file(path));
    }

    //TODO: test SIMD fast path with RGBA pixel data

    printf("loaded raw blobs\n");
    fflush(stdout);

    List<DebugTimers> timers = {};

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
        timers.add(save_gif(blob->width, blob->height, frames, blob->centiSeconds, path, true, GIFF_FORMAT_RGBA, 4));
        printf("\n");
        fflush(stdout);
    }

    DebugTimers totals = {};
    for (int i : range(timers.len)) {
        // printf("%12s      cook: %6.4f   count: %6.4f   choice: %6.4f   amble: %6.4f   palette: %6.4f   inner: %6.4f   compress: %6.4f   write: %6.4f   total: %6.4f   loss: %6.4f\n",
        //     names.begin()[i], timers[i].cook, timers[i].count, timers[i].choice, timers[i].amble, timers[i].palette, timers[i].inner, timers[i].compress, timers[i].write, timers[i].total,
        //     timers[i].total - (timers[i].amble + timers[i].compress + timers[i].write));
        printf("%12s      cook: %6.4f   count: %6.4f   choice: %6.4f   amble: %6.4f   compress: %6.4f   write: %6.4f   total: %6.4f   loss: %6.4f\n",
            names.begin()[i], timers[i].cook, timers[i].count, timers[i].choice, timers[i].amble, timers[i].compress, timers[i].write, timers[i].total,
            timers[i].total - (timers[i].amble + timers[i].compress + timers[i].write));
        totals.cook += timers[i].cook;
        totals.count += timers[i].count;
        totals.choice += timers[i].choice;
        totals.amble += timers[i].amble;
        totals.palette += timers[i].palette;
        totals.inner += timers[i].inner;
        totals.compress += timers[i].compress;
        totals.write += timers[i].write;
        totals.total += timers[i].total;
    }
    // printf("\n%12s      cook: %6.4f   count: %6.4f   choice: %6.4f   amble: %6.4f   palette: %6.4f   inner: %6.4f   compress: %6.4f   write: %6.4f   total: %6.4f   lost: %6.4f\n",
    //     "totals", totals.cook, totals.count, totals.choice, totals.amble, totals.palette, totals.inner, totals.compress, totals.write, totals.total,
    //     totals.total - (totals.amble + totals.compress + totals.write));
    printf("\n%12s      cook: %6.4f   count: %6.4f   choice: %6.4f   amble: %6.4f   compress: %6.4f   write: %6.4f   total: %6.4f   lost: %6.4f\n",
        "totals", totals.cook, totals.count, totals.choice, totals.amble, totals.compress, totals.write, totals.total, totals.total - (totals.amble + totals.compress + totals.write));

    //TODO: track input and output sizes and encode speed for each GIF and for all of them together

    return 0;
}
