
#include <stdio.h>
#include <stdlib.h>

static void * dummy_malloc(void * context, int newSize) {
	void * ret = malloc(newSize);
	// printf("MALLOC context: %s, newSize: %d, new: %p\n", context, newSize, ret);
	return ret;
}

static void * dummy_realloc(void * context, void * old, int oldSize, int newSize) {
	void * ret = realloc(old, newSize);
	// printf("REALLOC context: %s, old: %p, oldSize: %d, newSize: %d, new: %p\n", context, old, oldSize, newSize, ret);
	return ret;
}

static void dummy_free(void * context, void * old, int oldSize) {
	// printf("FREE context: %s, old: %p, oldSize: %d\n", context, old, oldSize);
	free(old);
}

#define MSF_GIF_IMPL
#define MSF_GIF_MALLOC(context, newSize) dummy_malloc(context, newSize)
#define MSF_GIF_REALLOC(context, oldMemory, oldSize, newSize) dummy_realloc(context, oldMemory, oldSize, newSize)
#define MSF_GIF_FREE(context, oldMemory, oldSize) dummy_free(context, oldMemory, oldSize)
#include "../msf_gif.h"
