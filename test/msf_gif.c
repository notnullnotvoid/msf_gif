
#include <stdio.h>
#include <stdlib.h>

#if 0
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
#else
#include <string.h>
#include <assert.h>

static uint8_t * prefaultedMallocBlock = NULL;
static uint8_t * prefaultedMallocHead = NULL;
static const size_t blockSize = 1 * 1024 * 1024 * 1024; //1 GB, happens to be plenty for each test case individually

void init_prefaulted_malloc() {
	prefaultedMallocBlock = (uint8_t *) malloc(blockSize);
	prefaultedMallocHead = prefaultedMallocBlock;
	memset(prefaultedMallocBlock, 1, blockSize);
}

void reset_prefaulted_malloc() {
	prefaultedMallocHead = prefaultedMallocBlock;
}

static void * dummy_malloc(void * context, int newSize) {
	size_t head = (size_t) prefaultedMallocHead;
	head = (head + 15) & 0xFFFFFFFFFFFFFFF0; //16-byte align forward for best speed
	prefaultedMallocHead = (uint8_t *) (head + newSize);
	// printf("MALLOC context: %s, newSize: %d, new: %p\n", context, newSize, (void *) head);
	assert(prefaultedMallocHead < prefaultedMallocBlock + blockSize);
	return (void *) head;
}

static void * dummy_realloc(void * context, void * old, int oldSize, int newSize) {
	// printf("REALLOC context: %s, old: %p, oldSize: %d, newSize: %d, new: %p\n", context, old, oldSize, newSize, old);
	return old; //we happen to never realloc to increase size with the current strategy, so we are allowed to do this
}

static void dummy_free(void * context, void * old, int oldSize) {
	// printf("FREE context: %s, old: %p, oldSize: %d\n", context, old, oldSize);
	//no-op
}
#endif

#define MSF_GIF_IMPL
#define MSF_GIF_MALLOC(context, newSize) dummy_malloc(context, newSize)
#define MSF_GIF_REALLOC(context, oldMemory, oldSize, newSize) dummy_realloc(context, oldMemory, oldSize, newSize)
#define MSF_GIF_FREE(context, oldMemory, oldSize) dummy_free(context, oldMemory, oldSize)
#include "../msf_gif.h"
