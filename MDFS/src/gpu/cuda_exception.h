#ifndef CUDA_EXCEPTION_H
#define CUDA_EXCEPTION_H

#include <cuda_runtime_api.h>

struct cudaException {
	const cudaError_t code;
	const char * const file;
	const int line;
	cudaException(cudaError_t code, const char *file, int line) : code(code), file(file), line(line) {}
};

#endif
