#define MSF_GIF_IMPL
#if __cplusplus >= 201703L
#define MSF_GIF_ENABLE_TRACING
#include "trace.hpp"
#endif
#include "../msf_gif.h"

extern "C" {
    int totalReallocCalls;
    int totalReallocCopies;
    int totalReallocData;
    int totalReallocBytesCopied;
}
