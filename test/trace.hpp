#ifndef TRACE_HPP
#define TRACE_HPP

#include <stdint.h>

struct TraceEvent {
    const char * name;
    uint64_t timestamp;
};

struct TraceEventArrays {
    TraceEvent * beginList;
    TraceEvent * beginHead;
    TraceEvent * beginEnd;

    TraceEvent * endList;
    TraceEvent * endHead;
    TraceEvent * endEnd;

    TraceEvent * instantList;
    TraceEvent * instantHead;
    TraceEvent * instantEnd;
};

extern thread_local TraceEventArrays * traceArrays;

void init_profiling_trace();
void init_profiling_thread();
void print_profiling_trace();

static inline __attribute__((always_inline)) void trace_begin_event(const char * name) {
    *traceArrays->beginHead++ = { name, __rdtsc() };
    if (traceArrays->beginHead == traceArrays->beginEnd) {
        traceArrays->beginHead = traceArrays->beginList;
        traceArrays->endHead = traceArrays->endList;
        traceArrays->instantHead = traceArrays->instantList;
    }
}

static inline __attribute__((always_inline)) void trace_end_event(const char * name) {
    *traceArrays->endHead++ = { name, __rdtsc() };
    if (traceArrays->endHead == traceArrays->endEnd) {
        traceArrays->beginHead = traceArrays->beginList;
        traceArrays->endHead = traceArrays->endList;
        traceArrays->instantHead = traceArrays->instantList;
    }
}

static inline __attribute__((always_inline)) void trace_instant_event(const char * name) {
    *traceArrays->instantHead++ = { name, __rdtsc() };
    if (traceArrays->instantHead == traceArrays->instantEnd) {
        traceArrays->beginHead = traceArrays->beginList;
        traceArrays->endHead = traceArrays->endList;
        traceArrays->instantHead = traceArrays->instantList;
    }
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
