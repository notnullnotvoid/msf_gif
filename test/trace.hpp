#ifndef TRACE_HPP
#define TRACE_HPP

#include <stdint.h>

#ifdef _MSC_VER
#include <intrin.h>
#define TRUE_INLINE __forceinline
#define __PRETTY_FUNCTION__ __FUNCSIG__
#else
#define TRUE_INLINE __attribute__((always_inline))
#endif

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

static inline TRUE_INLINE void trace_begin_event(const char * name) {
    *traceArrays->beginHead++ = { name, __rdtsc() };
    if (traceArrays->beginHead == traceArrays->beginEnd) {
        traceArrays->beginHead = traceArrays->beginList;
        traceArrays->endHead = traceArrays->endList;
        traceArrays->instantHead = traceArrays->instantList;
    }
}

static inline TRUE_INLINE void trace_end_event(const char * name) {
    *traceArrays->endHead++ = { name, __rdtsc() };
    if (traceArrays->endHead == traceArrays->endEnd) {
        traceArrays->beginHead = traceArrays->beginList;
        traceArrays->endHead = traceArrays->endList;
        traceArrays->instantHead = traceArrays->instantList;
    }
}

static inline TRUE_INLINE void trace_instant_event(const char * name) {
    *traceArrays->instantHead++ = { name, __rdtsc() };
    if (traceArrays->instantHead == traceArrays->instantEnd) {
        traceArrays->beginHead = traceArrays->beginList;
        traceArrays->endHead = traceArrays->endList;
        traceArrays->instantHead = traceArrays->instantList;
    }
}

struct ScopedTraceTimer {
    const char * name;
    TRUE_INLINE ScopedTraceTimer(const char * name_) {
        name = name_;
        trace_begin_event(name);
    }
    TRUE_INLINE ~ScopedTraceTimer() {
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
