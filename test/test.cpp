#include "../msf_gif.h"
#include "trace.hpp"
#include "List.hpp"
#include "common.hpp"

bool write_entire_file(const char * filepath, const void * data, size_t bytes) {
    assert(filepath && data);
    FILE * f = fopen(filepath, "wb");
    if (!f) return false;
    if (!fwrite(data, bytes, 1, f)) return false;
    if (fclose(f)) return false;
    return true;
}



struct RawBlob {
    int width, height, frames, centiSeconds;
    int pixels[1];
};

int main() {
    const char * names[] = {
        "bouncy", "diwide-large", "diwide", "floor", "increase", "keyhole", "odd", "tiles",
        "anchor", "always-in-front", "flip",
        // "sky",
        // "keyhole",
    };
    // const char * names[] = { "bouncy", "diwide", "increase" };

    List<RawBlob *> blobs = {};
    for (const char * name : names) {
        char * path = dsprintf(nullptr, "in/%s.rawframes", name);
        printf("loading %s...\n", path); fflush(stdout);
        blobs.add((RawBlob *) read_entire_file(path));
    }

    struct TimerInfo {
        double time;
        size_t in, out;
    };
    List<TimerInfo> timers = {};
    init_profiling_trace();

    //TODO: automatically regression-test against known good versions of the GIFs?
    for (int i = 0; i < blobs.len; ++i) {
        RawBlob * blob = blobs[i];
        bool flipped = blob->height < 0;
        if (flipped) blob->height *= -1;

        char * path = dsprintf(nullptr, "new/%s.gif", names[i]);
        TimeScope(path);
        printf("writing %24s      width: %d   height: %d   frames: %d   centiSeconds: %d\n",
            path, blob->width, blob->height, blob->frames, blob->centiSeconds); fflush(stdout);
        double pre = get_time();
        MsfGifState handle = {};
        handle.customAllocatorContext = path;
        assert(msf_gif_begin(&handle, blob->width, blob->height));
        for (int j = 0; j < blob->frames; ++j) {
            // handle.customAllocatorContext = dsprintf(nullptr, "%s frame %d", path, j);
            int pitch = flipped? -blob->width * 4 : blob->width * 4;
            assert(msf_gif_frame(&handle,
                (uint8_t *) &blob->pixels[blob->width * blob->height * j], blob->centiSeconds, 16, pitch));
        }
        MsfGifResult result = msf_gif_end(&handle);
        assert(result.data);
        timers.add({ get_time() - pre, (size_t)(blob->frames * blob->width * blob->height * 4), result.dataSize });
        FILE * fp = fopen(path, "wb");
        assert(fp);
        assert(fwrite(result.data, result.dataSize, 1, fp));
        fclose(fp);
        free(result.data);
    }

    printf("\n");
    TimerInfo totals = {};
    for (int i = 0; i < timers.len; ++i) {
        totals.time += timers[i].time;
        totals.in += timers[i].in;
        totals.out += timers[i].out;
        printf("%16s      time: %5.2f s   in: %4.2f GB/s   out: %5.2f MB/s\n",
            names[i], timers[i].time,
            timers[i].in / timers[i].time / 1024 / 1024 / 1024, timers[i].out / timers[i].time / 1024 / 1024);
    }
    printf("\n%16s      time: %5.2f s  in: %4.2f GB/s   out: %5.2f MB/s\n",
        "totals", totals.time, totals.in / totals.time / 1024 / 1024 / 1024, totals.out / totals.time / 1024 / 1024);

    trace_end_event("main");
    print_profiling_trace();
    return 0;
}
