
#include <stdio.h>
#include <stdlib.h>

static void * dummy_malloc(void * context, int newSize) {
	void * ret = malloc(newSize);
	// printf("MALLOC context: %s, newSize: %d, new: %p\n", context, newSize, ret);
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
#define MSF_GIF_FREE(context, oldMemory, oldSize) dummy_free(context, oldMemory, oldSize)
#define MSF_GIF_FOPEN(context, filePath) dummy_fopen(context, filePath)
#define MSF_GIF_FWRITE(context, filePointer, data, dataSize) dummy_fwrite(context, filePointer, data, dataSize)
#define MSF_GIF_FCLOSE(context, filePointer) dummy_fclose(context, filePointer)
#include "../msf_gif.h"
