#ifndef TRACE_HPP
#define TRACE_HPP

#include <stdint.h>

struct TraceEvent {
    const char * name;
    uint64_t timestamp;
};

extern TraceEvent * beginEventList;
extern TraceEvent * beginEventHead;
extern TraceEvent * beginEventEnd;

extern TraceEvent * endEventList;
extern TraceEvent * endEventHead;
extern TraceEvent * endEventEnd;

extern TraceEvent * instantEventList;
extern TraceEvent * instantEventHead;
extern TraceEvent * instantEventEnd;

void init_profiling_trace();
void print_profiling_trace();

static inline __attribute__((always_inline)) void trace_begin_event(const char * name) {
    *beginEventHead++ = { name, __rdtsc() };
    if (beginEventHead == beginEventEnd)
        { beginEventHead = beginEventList; endEventHead = endEventList; instantEventHead = instantEventList; }
}

static inline __attribute__((always_inline)) void trace_end_event(const char * name) {
    *endEventHead++ = { name, __rdtsc() };
    if (endEventHead == endEventEnd)
        { beginEventHead = beginEventList; endEventHead = endEventList; instantEventHead = instantEventList; }
}

static inline __attribute__((always_inline)) void trace_instant_event(const char * name) {
    *instantEventHead++ = { name, __rdtsc() };
    if (instantEventHead == instantEventEnd)
        { beginEventHead = beginEventList; endEventHead = endEventList; instantEventHead = instantEventList; }
}

struct ScopedTraceTimer {
    const char * name;
    __attribute__((always_inline)) ScopedTraceTimer(const char * name_) {
        name = name_;
        trace_begin_event(name);
    }
    __attribute__((always_inline)) ~ScopedTraceTimer() {
        trace_end_event(name);
    }
};

#define PASTE2(a, b) a ## b
#define PASTE(a, b) PASTE2(a, b)
#define TimeFunc ScopedTraceTimer PASTE(Unique_Name_, __COUNTER__) (__PRETTY_FUNCTION__);
#define TimeScope(NAME) ScopedTraceTimer PASTE(Unique_Name_, __COUNTER__) (NAME);
#define TimeLoop(NAME) if (ScopedTraceTimer PASTE(Unique_Name_, __COUNTER__) (NAME); true)
#define TimeLine(NAME) if (ScopedTraceTimer PASTE(Unique_Name_, __COUNTER__) (NAME); true)

uint64_t get_nanos();
static inline float get_time() { return get_nanos() * (1 / 1'000'000'000.0f); }

#endif //TRACE_HPP
