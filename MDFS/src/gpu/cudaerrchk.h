#ifndef CUDAERRCHK_CUH
#define CUDAERRCHK_CUH

#include "cuda_exception.h"

#define CUDA(ans) { cudaAssert((ans), __FILE__, __LINE__); }

inline void cudaAssert(cudaError_t code, const char *file, int line) {
  if (code != cudaSuccess)
    throw(cudaException(code, file, line));
}

#endif
