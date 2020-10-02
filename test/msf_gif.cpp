
#include <stdio.h>
#include <stdlib.h>
#include "trace.hpp"

static void * dummy_realloc(void * context, void * old, int oldSize, int newSize) {
	float pre = get_time();
	void * ret = realloc(old, newSize);
	printf("REALLOC context: %s, old: %p, oldSize: %d, newSize: %d, new: %p, time: %5.3f ms\n",
		context, old, oldSize, newSize, ret, 1000 * (get_time() - pre));
	return ret;
}

#define MSF_GIF_MALLOC(contextPointer, newSize) malloc(newSize)
#define MSF_GIF_REALLOC(contextPointer, oldMemory, oldSize, newSize) dummy_realloc(contextPointer, oldMemory, oldSize, newSize)
#define MSF_GIF_FREE(contextPointer, oldMemory, oldSize) free(oldMemory)

#define MSF_GIF_IMPL
#if __cplusplus >= 201703L
#define MSF_GIF_ENABLE_TRACING
#include "trace.hpp"
#endif
#include "../msf_gif.h"
