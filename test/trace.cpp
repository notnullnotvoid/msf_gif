#include "trace.hpp"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

static uint64_t nanoStart;
static uint64_t tscStart;

#ifdef _WIN32
	#define WIN32_LEAN_AND_MEAN
	#include <windows.h>
	uint64_t get_nanos() {
		LARGE_INTEGER counter, frequency;
		assert(QueryPerformanceCounter(&counter));
		assert(QueryPerformanceFrequency(&frequency));
		double seconds = (counter.QuadPart - nanoStart) / (double) frequency.QuadPart;
		return seconds * 1'000'000'000.0;
	}
#else
	#include <time.h>
	uint64_t get_nanos() {
		timespec now;
		assert(!clock_gettime(CLOCK_MONOTONIC, &now));
		return now.tv_sec * 1'000'000'000LL + now.tv_nsec - nanoStart;
	}
#endif

thread_local TraceEventArrays * traceArrays;

struct TraceThreadInfo {
	TraceEventArrays * arrays;
	int thread;
};

TraceEventArrays ** traceThreads;
int traceThreadCount;

void init_profiling_thread() {
	if (traceArrays) return;
	const size_t NUM_TRACE_EVENTS = 1024 * 1024;
	traceArrays = (TraceEventArrays *) malloc(sizeof(TraceEventArrays));
	traceArrays->beginList = (TraceEvent *) malloc(NUM_TRACE_EVENTS * sizeof(TraceEvent));
	traceArrays->beginHead = traceArrays->beginList;
	traceArrays->beginEnd  = traceArrays->beginList + NUM_TRACE_EVENTS;
	traceArrays->endList = (TraceEvent *) malloc(NUM_TRACE_EVENTS * sizeof(TraceEvent));
	traceArrays->endHead = traceArrays->endList;
	traceArrays->endEnd  = traceArrays->endList + NUM_TRACE_EVENTS;
	traceArrays->instantList = (TraceEvent *) malloc(NUM_TRACE_EVENTS * sizeof(TraceEvent));
	traceArrays->instantHead = traceArrays->instantList;
	traceArrays->instantEnd  = traceArrays->instantList + NUM_TRACE_EVENTS;
	#ifdef _MSC_VER
		int threadNum = InterlockedIncrement((LONG *) &traceThreadCount) - 1;
	#else
		int threadNum = __sync_fetch_and_add(&traceThreadCount, 1);
	#endif
	traceThreads[threadNum] = traceArrays;
}

void init_profiling_trace() {
	#ifdef _WIN32
		LARGE_INTEGER counter;
		assert(QueryPerformanceCounter(&counter));
		nanoStart = counter.QuadPart;
	#else
		timespec now;
		assert(!clock_gettime(CLOCK_MONOTONIC, &now));
		nanoStart = now.tv_sec * 1'000'000'000LL + now.tv_nsec;
	#endif
	tscStart = __rdtsc();
	traceThreads = (TraceEventArrays **) malloc(1024 * sizeof(TraceEventArrays *));

	init_profiling_thread();

	//NOTE: to avoid discrepancies between times listed in json and times shown in chrome,
	//		we make sure the first event starts at 0 microseconds
	*traceArrays->instantHead++ = { "profiling start", tscStart };
}

void print_profiling_trace() {
    uint64_t diffTime = get_nanos();
    double elapsedSeconds = diffTime / 1'000'000'000.0;

	uint64_t tsc = __rdtsc();
    uint64_t diffTsc = tsc - tscStart;
    double tscPerSecond = diffTsc / elapsedSeconds;
    //NOTE: we repord nanoseconds instead of microseconds because of a bug in chrome://tracing
    //		that causes function timings to stack incorrectly if they are too short
    double tscPerMicrosecond = tscPerSecond / 1'000'000'000;

	FILE * out = fopen("trace.json", "wb");
	fprintf(out, "[\n");
	for (int i = 0; i < traceThreadCount; ++i) {
		TraceEventArrays * arrays = traceThreads[i];
		TraceEvent * begin = arrays->beginList;
		TraceEvent * end = arrays->endList;
		while (begin != arrays->beginHead || end != arrays->endHead) {
			if (end == arrays->endHead) {
				fprintf(out, "{\"name\":\"%s\",\"ph\":\"B\",\"pid\":0,\"tid\":%d,\"ts\":%f},\n",
					begin->name, i, (begin->timestamp - tscStart) / tscPerMicrosecond);
				++begin;
			} else if (begin == arrays->beginHead) {
				fprintf(out, "{\"name\":\"%s\",\"ph\":\"E\",\"pid\":0,\"tid\":%d,\"ts\":%f},\n",
					end->name, i, (end->timestamp - tscStart) / tscPerMicrosecond);
				++end;
			} else if (begin->timestamp < end->timestamp) {
				fprintf(out, "{\"name\":\"%s\",\"ph\":\"B\",\"pid\":0,\"tid\":%d,\"ts\":%f},\n",
					begin->name, i, (begin->timestamp - tscStart) / tscPerMicrosecond);
				++begin;
			} else {
				fprintf(out, "{\"name\":\"%s\",\"ph\":\"E\",\"pid\":0,\"tid\":%d,\"ts\":%f},\n",
					end->name, i, (end->timestamp - tscStart) / tscPerMicrosecond);
				++end;
			}
		}
		for (TraceEvent * e = arrays->instantList; e != arrays->instantHead; ++e) {
			fprintf(out, "{\"name\":\"%s\",\"ph\":\"i\",\"pid\":0,\"tid\":%d,\"s\":\"g\",\"ts\":%f},\n",
				e->name, i, (e->timestamp - tscStart) / tscPerMicrosecond);
		}
	}
	fclose(out);
}
