
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

static void * dummy_fopen(void * context, const char * filepath) {
	FILE * ret = fopen(filepath, "wb");
	FILE ** handle = (FILE **) malloc(sizeof(FILE *));
	*handle = ret;
	// printf("FOPEN context: %s, filepath: %s, handle: %p\n", context, filepath, handle);
	// fflush(stdout);
	return handle;
}

static int dummy_fwrite(void * context, void * handle, void * buffer, int bytes) {
	FILE * fp = *((FILE **) handle);
	int ret = fwrite(buffer, bytes, 1, fp);
	// printf("FWRITE context: %s, handle: %p, buffer: %p, bytes: %d, written: %d\n", context, handle, buffer, bytes, ret);
	// fflush(stdout);
	return ret;
}

static int dummy_fclose(void * context, void * handle) {
	FILE * fp = *((FILE **) handle);
	int ret = fclose(fp);
	// printf("FCLOSE context: %s, handle: %p\n", context, handle);
	// fflush(stdout);
	free(handle);
	return ret;
}

#define MSF_GIF_IMPL
#define MSF_GIF_MALLOC(context, newSize) dummy_malloc(context, newSize)
#define MSF_GIF_REALLOC(context, oldMemory, oldSize, newSize) dummy_realloc(context, oldMemory, oldSize, newSize)
#define MSF_GIF_FREE(context, oldMemory, oldSize) dummy_free(context, oldMemory, oldSize)
#define MSF_GIF_FOPEN(context, filePath) dummy_fopen(context, filePath)
#define MSF_GIF_FWRITE(context, filePointer, data, dataSize) dummy_fwrite(context, filePointer, data, dataSize)
#define MSF_GIF_FCLOSE(context, filePointer) dummy_fclose(context, filePointer)
#include "../msf_gif.h"
