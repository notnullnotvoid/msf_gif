#ifndef COMMON_HPP
#define COMMON_HPP

//these exist so we can easily switch non-essential console messages on/off
#define print_error printf
#define print_log printf

////////////////////////////////////////////////////////////////////////////////
/// TYPES                                                                    ///
////////////////////////////////////////////////////////////////////////////////

#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef unsigned int uint;

template <typename T1, typename T2>
struct Pair {
    T1 first;
    T2 second;
};

////////////////////////////////////////////////////////////////////////////////
/// STRING UTILS                                                             ///
////////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <stdlib.h>

static inline char * dup(const char * src, int len) {
    char * ret = (char *) malloc(len + 1);
    strncpy(ret, src, len);
    ret[len] = '\0';
    return ret;
}

static inline char * dup(const char * src, const char * end) {
    return dup(src, end - src);
}

static inline char * dup(const char * src) {
    if (!src) return nullptr;
    return dup(src, strlen(src));
}

////////////////////////////////////////////////////////////////////////////////
/// MISC                                                                     ///
////////////////////////////////////////////////////////////////////////////////

#include "List.hpp"
#include <string.h>
#include <stdio.h>
#include <initializer_list>
//TODO: replace with custom assert (BEFORE DISABLING ASSERTS IN RELEASE BUILDS)
#include <assert.h>

static inline bool file_exists(const char * filepath) {
    FILE * f = fopen(filepath, "rb");
    if (f) {
        fclose(f);
        return true;
    }
    return false;
}

static inline List<char *> split_lines_in_place(char * mutableSrc) {
    List<char *> lines = {};
    char * line = strtok(mutableSrc, "\r\n");
    while (line) {
        lines.add(line);
        line = strtok(nullptr, "\r\n");
    }
    return lines;
}

//NOTE: the correct behavior of this function is unfortunately not guaranteed by the standard
static inline char * read_entire_file(const char * filepath) {
    FILE * f = fopen(filepath, "rb");
    assert(f);
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);  //same as rewind(f);

    char * string = (char *) malloc(fsize + 1);
    fread(string, fsize, 1, f);
    fclose(f);

    string[fsize] = 0;

    return string;
}

#include <stdarg.h>

//allocates a buffer large enough to fit resulting string, and `sprintf`s to it
#ifndef _MSC_VER
	__attribute__((format(printf, 2, 3)))
#endif
static inline char * dsprintf(char * buf, const char * fmt, ...) {
    size_t len = buf? strlen(buf) : 0;
    va_list args1, args2;
    va_start(args1, fmt);
    va_copy(args2, args1);
    buf = (char *) realloc(buf, len + vsnprintf(nullptr, 0, fmt, args1) + 1);
    vsprintf(buf + len, fmt, args2);
    va_end(args1);
    va_end(args2);
    return buf;
}

template <typename VAL, typename KEY>
static inline VAL match_pair(std::initializer_list<Pair<VAL, KEY>> pairs, KEY key, VAL defaultVal) {
    for (auto it = pairs.begin(); it != pairs.end(); ++it) {
        if (it->second == key) {
            return it->first;
        }
    }
    return defaultVal;
}

template <typename TYPE>
static inline bool one_of(std::initializer_list<TYPE> list, TYPE key) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        if (*it == key) {
            return true;
        }
    }
    return false;
}

template <typename TYPE>
static inline void swap(TYPE & a, TYPE & b) {
    TYPE temp = a;
    a = b;
    b = temp;
}

#include <math.h>

static inline float dbToVolume(float db) { return powf(1.0f, 0.05f * db); }
static inline float volumeToDb(float volume) { return 20.0f * log10f(volume); }

#define ARR_SIZE(x) (sizeof(x) / sizeof((x)[0]))

#endif
