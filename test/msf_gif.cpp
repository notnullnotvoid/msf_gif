#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct FileBuffer {
	uint8_t * mem;
	size_t len;
	size_t max;
};

int writeBytes(FileBuffer * buf, void * data, size_t size) {
	while (buf->len + size >= buf->max) {
		buf->max = buf->max * 2 + 1;
		buf->mem = (uint8_t *) realloc(buf->mem, buf->max);
	}

	memcpy(buf->mem + buf->len, data, size);
	buf->len += size;
	return 1; //needs to return non-zero to match the error reporting behavior of fwrite
}

#define MSF_GIF_FOPEN(contextPointer, filePath) contextPointer //no-op (but it needs to evaluate to something non-zero)
#define MSF_GIF_FWRITE(contextPointer, filePointer, data, dataSize) writeBytes((FileBuffer *) filePointer, data, dataSize)
#define MSF_GIF_FCLOSE(contextPointer, filePointer) 0 //no-op (but it needs to evaluate to 0)

#define MSF_GIF_IMPL
#if __cplusplus >= 201703L
#define MSF_GIF_ENABLE_TRACING
#include "trace.hpp"
#endif
#include "../msf_gif.h"
