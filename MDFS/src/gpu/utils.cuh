#ifndef UTILS_CUH
#define UTILS_CUH

__device__ __host__ __forceinline__
constexpr int pow(int a, int b) {
	return b < 1 ? 1 : a * pow(a, b - 1);
}

__device__ __host__ __forceinline__
constexpr int blockSize(int dim) {
	return dim == 2 ? 16 : 1 << (10 / dim);
}

#endif
