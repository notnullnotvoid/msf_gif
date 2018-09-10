#ifndef FILEBUFFER_HPP
#define FILEBUFFER_HPP

#include <stdlib.h>

//TODO: add method to re-align "head" pointer

//NOTE: this struct zero-initializes to a valid state!
//      FileBuffer buf = {}; //this is valid
struct FileBuffer {
    uint8_t * block;
    uint8_t * head;
    uint8_t * end;

    void init(size_t bytes = 1024) {
        assert(bytes > 0);
        block = (uint8_t *) malloc(bytes);
        head = block;
        end = block + bytes;
    }

    void check(size_t bytes) {
        if (head + bytes < end)
            return;

        size_t byte = head - block;
        size_t size = end - block;

        //done in a loop so adding payloads larger than the current buffer size will work
        while (byte + bytes >= size) {
            size = size * 2 + 1;
        }

        block = (uint8_t *) realloc(block, size);
        head = block + byte;
        end = block + size;
    }

    template<typename TYPE>
    size_t write(TYPE t) {
        check(sizeof(t));
        *((TYPE *)head) = t;
        size_t pos = head - block;
        head += sizeof(t);
        return pos;
    }

    template<typename TYPE>
    size_t write_unsafe(TYPE t) {
        *((TYPE *)head) = t;
        size_t pos = head - block;
        head += sizeof(t);
        return pos;
    }

    template<typename TYPE>
    size_t write_block(TYPE * t, size_t count) {
        check(count * sizeof(TYPE));
        size_t pos = head - block;
        memcpy(head, t, count * sizeof(TYPE));
        head += count * sizeof(TYPE);
        return pos;
    }

    template<typename TYPE>
    size_t write_block_unsafe(TYPE * t, size_t count) {
        size_t pos = head - block;
        memcpy(head, t, count * sizeof(TYPE));
        head += count * sizeof(TYPE);
        return pos;
    }

    template<typename TYPE>
    size_t update(size_t pos, TYPE t) {
        uint8_t * ptr = block + pos;
        *((TYPE *)ptr) = t;
        return pos;
    }

    size_t size() {
        return head - block;
    }

    void finalize() {
        free(block);
        *this = {};
    }
};

//convenience function for same-line declaration+initialization
static inline FileBuffer create_file_buffer(size_t bytes = 1024) {
    FileBuffer buf;
    buf.init(bytes);
    return buf;
}

#endif
