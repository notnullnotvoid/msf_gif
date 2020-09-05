
#include <stdio.h>
#include <stdlib.h>

int totalReallocCalls = 0;
int totalReallocCopies = 0;
int totalReallocData = 0;
int totalReallocBytesCopied = 0;

static void * dummy_malloc(void * context, int newSize) {
	void * ret = malloc(newSize);
	// printf("MALLOC context: %s, newSize: %d, new: %p\n", context, newSize, ret);
	return ret;
}

static void * dummy_realloc(void * context, void * old, int oldSize, int newSize) {
	void * ret = realloc(old, newSize);
	// printf("REALLOC context: %s, old: %p, oldSize: %d, newSize: %d, new: %p\n", context, old, oldSize, newSize, ret);
	totalReallocCalls += 1;
	if (old != ret) totalReallocCopies += 1;
	totalReallocData += oldSize;
	if (old != ret) totalReallocBytesCopied += oldSize;
	return ret;
}

static void dummy_free(void * context, void * old, int oldSize) {
	// printf("FREE context: %s, old: %p, oldSize: %d\n", context, old, oldSize);
	free(old);
}

#define MSF_GIF_IMPL
#define MSF_GIF_MALLOC(contextPointer, newSize) dummy_malloc(contextPointer, newSize)
#define MSF_GIF_REALLOC(contextPointer, oldMemory, oldSize, newSize) dummy_realloc(contextPointer, oldMemory, oldSize, newSize)
#define MSF_GIF_FREE(contextPointer, oldMemory, oldSize) dummy_free(contextPointer, oldMemory, oldSize)
#include "../msf_gif.h"
