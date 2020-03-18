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
		double seconds = counter.QuadPart / (double) frequency.QuadPart;
		return seconds * 1'000'000'000.0 - nanoStart;
	}
#else
	#include <time.h>
	uint64_t get_nanos() {
		timespec now;
		assert(!clock_gettime(CLOCK_MONOTONIC, &now));
		return now.tv_sec * 1'000'000'000LL + now.tv_nsec - nanoStart;
	}
#endif

TraceEvent * beginEventList;
TraceEvent * beginEventHead;
TraceEvent * beginEventEnd;

TraceEvent * endEventList;
TraceEvent * endEventHead;
TraceEvent * endEventEnd;

TraceEvent * instantEventList;
TraceEvent * instantEventHead;
TraceEvent * instantEventEnd;

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

	const size_t NUM_TRACE_EVENTS = 1024 * 1024;

	beginEventList = (TraceEvent *) malloc(NUM_TRACE_EVENTS * sizeof(TraceEvent));
	beginEventHead = beginEventList;
	beginEventEnd  = beginEventList + NUM_TRACE_EVENTS;

	endEventList = (TraceEvent *) malloc(NUM_TRACE_EVENTS * sizeof(TraceEvent));
	endEventHead = endEventList;
	endEventEnd  = endEventList + NUM_TRACE_EVENTS;

	instantEventList = (TraceEvent *) malloc(NUM_TRACE_EVENTS * sizeof(TraceEvent));
	instantEventHead = instantEventList;
	instantEventEnd  = instantEventList + NUM_TRACE_EVENTS;

	//NOTE: to avoid discrepancies between times listed in json and times shown in chrome,
	//		we make sure the first event starts at 0 microseconds
	*beginEventHead++ = { "main", tscStart };
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
	TraceEvent * begin = beginEventList;
	TraceEvent * end = endEventList;
	while (begin != beginEventHead || end != endEventHead) {
		if (end == endEventHead) {
			fprintf(out, "{\"name\":\"%s\",\"ph\":\"B\",\"pid\":0,\"tid\":0,\"ts\":%f},\n",
				begin->name, (begin->timestamp - tscStart) / tscPerMicrosecond);
			++begin;
		} else if (begin == beginEventHead) {
			fprintf(out, "{\"name\":\"%s\",\"ph\":\"E\",\"pid\":0,\"tid\":0,\"ts\":%f},\n",
				end->name, (end->timestamp - tscStart) / tscPerMicrosecond);
			++end;
		} else if (begin->timestamp < end->timestamp) {
			fprintf(out, "{\"name\":\"%s\",\"ph\":\"B\",\"pid\":0,\"tid\":0,\"ts\":%f},\n",
				begin->name, (begin->timestamp - tscStart) / tscPerMicrosecond);
			++begin;
		} else {
			fprintf(out, "{\"name\":\"%s\",\"ph\":\"E\",\"pid\":0,\"tid\":0,\"ts\":%f},\n",
				end->name, (end->timestamp - tscStart) / tscPerMicrosecond);
			++end;
		}
	}
	for (TraceEvent * e = instantEventList; e != instantEventHead; ++e) {
		fprintf(out, "{\"name\":\"%s\",\"ph\":\"i\",\"pid\":0,\"tid\":0,\"s\":\"g\",\"ts\":%f},\n",
			e->name, (e->timestamp - tscStart) / tscPerMicrosecond);
	}
	fclose(out);
}
