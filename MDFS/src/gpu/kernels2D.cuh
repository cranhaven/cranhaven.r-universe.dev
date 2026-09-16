#ifndef KERNELS2D_CUH
#define KERNELS2D_CUH

#include <stdint.h>
#include "kernel_param.h"
#include "utils.cuh"
#include "cudaerrchk.h"

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int WARPS,
	int BITS,
	int PX,
	int PY>
__global__
void kernel2D(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int WARPS,
	int BITS,
	int PX,
	int PY>
__global__
void kernel2D_short(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int BITS>
__global__
void kernel2D_simple(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int SHORT>
void kernel2DWrapper(KernelParam param, cudaStream_t stream) {
	const int warps = 2;
	dim3 grid(TILE_SIZE / 2, TILE_SIZE / 2);
	dim3 block(32, warps);

	void (*kernel)(KernelParam);

	if (SHORT) {
		if (DIV < 2) kernel = kernel2D_short<TILE_SIZE, DIV, AVG, warps, 1, 2, 2>;
		else if (DIV < 4) kernel = kernel2D_short<TILE_SIZE, DIV, AVG, warps, 2, 2, 2>;
		else if (DIV < 8) kernel = kernel2D_short<TILE_SIZE, DIV, AVG, warps, 3, 2, 2>;
		else kernel = kernel2D_short<TILE_SIZE, DIV, AVG, warps, 4, 2, 2>;
	} else {
		if (DIV < 2) kernel = kernel2D<TILE_SIZE, DIV, AVG, warps, 1, 2, 2>;
		else if (DIV < 4) kernel = kernel2D<TILE_SIZE, DIV, AVG, warps, 2, 2, 2>;
		else if (DIV < 8) kernel = kernel2D<TILE_SIZE, DIV, AVG, warps, 3, 2, 2>;
		else kernel = kernel2D<TILE_SIZE, DIV, AVG, warps, 4, 2, 2>;
	}

	kernel<<<grid, block, 0, stream>>>(param);
	CUDA(cudaPeekAtLastError());

/*	dim3 grid(TILE_SIZE, TILE_SIZE);
	void (*kernel)(KernelParam);

	if (DIV < 2) kernel = kernel2D_simple<TILE_SIZE, DIV, AVG, 1>;
	else if (DIV < 4) kernel = kernel2D_simple<TILE_SIZE, DIV, AVG, 2>;
	else if (DIV < 8) kernel = kernel2D_simple<TILE_SIZE, DIV, AVG, 3>;
	else kernel = kernel2D_simple<TILE_SIZE, DIV, AVG, 4>;

	kernel<<<grid, 32, 0, stream>>>(param);
	CUDA(cudaPeekAtLastError());*/
}

template<int COUNTERS>
__forceinline__ __device__
float totalInf_warp(uint32_t* counters, float pseudo0, float pseudo1);

template<int DIV>
__forceinline__ __device__
float inf2D_X_warp(uint32_t* counters, float pseudo0, float pseudo1);

template<int DIV>
__forceinline__ __device__
float inf2D_Y_warp(uint32_t* counters, float pseudo0, float pseudo1);

template<int COUNTERS>
__forceinline__ __device__
float totalInf_warp_short(uint32_t* counters, float pseudo0, float pseudo1);

template<int DIV>
__forceinline__ __device__
float inf2D_X_warp_short(uint32_t* counters, float pseudo0, float pseudo1);

template<int DIV>
__forceinline__ __device__
float inf2D_Y_warp_short(uint32_t* counters, float pseudo0, float pseudo1);


template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int WARPS,
	int BITS,
	int PX,
	int PY>
__global__
__launch_bounds__(32 * WARPS)
void kernel2D(KernelParam param) {
	__shared__ uint32_t counters[PX][PY][2][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX = PX * blockIdx.x;
	int bY = PY * blockIdx.y;
	int tId = blockDim.x * threadIdx.y + threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[2] = {
		param.data[0] + bX * varlen,
		param.data[1] + bY * varlen,
	};

	float acc[2] = {0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		__syncthreads();
		for (int i = tId; i < 2 * PX * PY * (DIV + 1) * (DIV + 1); i += 32 * WARPS) {
			shared[i] = 0;
		}
		__syncthreads();

		for (int dec = 0; dec < 2; dec++) {
			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32 * WARPS) {

				uint64_t prefetch[2][PX > PY ? PX : PY];

				#pragma unroll
				for (int j = 0; j < PX; j++) {
					prefetch[0][j] = data[0][j * varlen + i];
				}
				#pragma unroll
				for (int j = 0; j < PY; j++) {
					prefetch[1][j] = data[1][j * varlen + i];
				}

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					#pragma unroll
					for (int x = 0; x < PX; x++) {
						#pragma unroll
						for (int y = 0; y < PY; y++) {
							uint32_t* p = &counters[x][y][dec][
								prefetch[0][x] & mask][
								prefetch[1][y] & mask];

							atomicAdd(p, 1);
						}
					}

					#pragma unroll
					for (int j = 0; j < PX; j++) {
						prefetch[0][j] >>= BITS;
					}
					#pragma unroll
					for (int j = 0; j < PY; j++) {
						prefetch[1][j] >>= BITS;
					}
				}
			}
		}
		__syncthreads();

		for (int i = threadIdx.y; i < PX * PY; i += WARPS) {
			if (threadIdx.x == 0) {
				for (int j = 0; j < 2; j++) {
					counters[i / PY][i % PY][j][0][0] -=
						(64 / BITS) * param.packs[j] - param.objs[j];
				}
			}

			uint32_t* cnts = &counters[i / PY][i % PY][0][0][0];
			float total = totalInf_warp<(DIV + 1) * (DIV + 1)>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infX = inf2D_X_warp<DIV>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infY = inf2D_Y_warp<DIV>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float relX = total - infY;
			float relY = total - infX;

			if (threadIdx.x == i) {
				if (AVG) {
					acc[0] += relX;
					acc[1] += relY;
				} else {
					acc[0] = (relX > acc[0] ? relX : acc[0]);
					acc[1] = (relY > acc[1] ? relY : acc[1]);
				}
			}
		}

		for (int i = 0; i < 2; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 2
	for (int i = 0; i < 2; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	for (int i = threadIdx.y; i < PX * PY; i += WARPS) {
		int x = i / PY;
		int y = i % PY;

		if (threadIdx.x == i) {
			int X = param.offset[0] * TILE_SIZE + bX + x;
			int Y = param.offset[1] * TILE_SIZE + bY + y;

			if (X >= param.vars || Y >= param.vars) {
				continue;
			}

			atomicMax((unsigned*) (param.IG + X), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + Y), *((unsigned*) &acc[1]));
		}
	}
}

template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int WARPS,
	int BITS,
	int PX,
	int PY>
__global__
__launch_bounds__(32 * WARPS)
void kernel2D_short(KernelParam param) {
	__shared__ uint32_t counters[PX][PY][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX = PX * blockIdx.x;
	int bY = PY * blockIdx.y;
	int tId = blockDim.x * threadIdx.y + threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[2] = {
		param.data[0] + bX * varlen,
		param.data[1] + bY * varlen,
	};

	float acc[2] = {0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		__syncthreads();
		for (int i = tId; i < PX * PY * (DIV + 1) * (DIV + 1); i += 32 * WARPS) {
			shared[i] = 0;
		}
		__syncthreads();

		for (int dec = 0; dec < 2; dec++) {
			unsigned add = dec ? (1 << 16) : 1;

			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32 * WARPS) {

				uint64_t prefetch[2][PX > PY ? PX : PY];

				#pragma unroll
				for (int j = 0; j < PX; j++) {
					prefetch[0][j] = data[0][j * varlen + i];
				}
				#pragma unroll
				for (int j = 0; j < PY; j++) {
					prefetch[1][j] = data[1][j * varlen + i];
				}

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					#pragma unroll
					for (int x = 0; x < PX; x++) {
						#pragma unroll
						for (int y = 0; y < PY; y++) {
							uint32_t* p = &counters[x][y][
								prefetch[0][x] & mask][
								prefetch[1][y] & mask];

							atomicAdd(p, add);
						}
					}

					#pragma unroll
					for (int j = 0; j < PX; j++) {
						prefetch[0][j] >>= BITS;
					}
					#pragma unroll
					for (int j = 0; j < PY; j++) {
						prefetch[1][j] >>= BITS;
					}
				}
			}
		}
		__syncthreads();

		for (int i = threadIdx.y; i < PX * PY; i += WARPS) {
			if (threadIdx.x == 0) {
				for (int j = 0; j < 2; j++) {
					counters[i / PY][i % PY][0][0] -=
						((64 / BITS) * param.packs[j] - param.objs[j]) << (16 * j);
				}
			}

			uint32_t* cnts = &counters[i / PY][i % PY][0][0];
			float total = totalInf_warp_short<(DIV + 1) * (DIV + 1)>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infX = inf2D_X_warp_short<DIV>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infY = inf2D_Y_warp_short<DIV>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float relX = total - infY;
			float relY = total - infX;

			if (threadIdx.x == i) {
				if (AVG) {
					acc[0] += relX;
					acc[1] += relY;
				} else {
					acc[0] = (relX > acc[0] ? relX : acc[0]);
					acc[1] = (relY > acc[1] ? relY : acc[1]);
				}
			}
		}

		for (int i = 0; i < 2; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 2
	for (int i = 0; i < 2; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	for (int i = threadIdx.y; i < PX * PY; i += WARPS) {
		int x = i / PY;
		int y = i % PY;

		if (threadIdx.x == i) {
			int X = param.offset[0] * TILE_SIZE + bX + x;
			int Y = param.offset[1] * TILE_SIZE + bY + y;

			if (X >= param.vars || Y >= param.vars) {
				continue;
			}

			atomicMax((unsigned*) (param.IG + X), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + Y), *((unsigned*) &acc[1]));
		}
	}
}


template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int BITS>
__global__
__launch_bounds__(32)
void kernel2D_simple(KernelParam param) {
	__shared__ uint32_t counters[2][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX = blockIdx.x;
	int bY = blockIdx.y;
	int tId = threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[2] = {
		param.data[0] + bX * varlen,
		param.data[1] + bY * varlen
	};

	float acc[2] = {0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		for (int i = tId; i < 2 * (DIV + 1) * (DIV + 1); i += 32) {
			shared[i] = 0;
		}

		for (int dec = 0; dec < 2; dec++) {
			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32) {

				uint64_t X = data[0][i];
				uint64_t Y = data[1][i];

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					uint32_t* p = &counters[dec][X & mask][Y & mask];
					atomicAdd(p, 1);

					X >>= BITS;
					Y >>= BITS;
				}
			}
		}

		if (threadIdx.x == 0) {
			for (int j = 0; j < 2; j++) {
				counters[j][0][0] -=
					(64 / BITS) * param.packs[j] - param.objs[j];
			}
		}

		uint32_t* cnts = &counters[0][0][0];
		float total = totalInf_warp<(DIV + 1) * (DIV + 1)>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float infX = inf2D_X_warp<DIV>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float infY = inf2D_Y_warp<DIV>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float relX = total - infY;
		float relY = total - infX;

		if (AVG) {
			acc[0] += relX;
			acc[1] += relY;
		} else {
			acc[0] = (relX > acc[0] ? relX : acc[0]);
			acc[1] = (relY > acc[1] ? relY : acc[1]);
		}

		for (int i = 0; i < 2; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 2
	for (int i = 0; i < 2; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	if (threadIdx.x == 0) {
		int X = param.offset[0] * TILE_SIZE + bX;
		int Y = param.offset[1] * TILE_SIZE + bY;

		if (X < param.vars && Y < param.vars) {
			atomicMax((unsigned*) (param.IG + X), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + Y), *((unsigned*) &acc[1]));
		}
	}
}

template<int COUNTERS>
__forceinline__ __device__
float totalInf_warp(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;

#pragma unroll
	for (int i = threadIdx.x; i < COUNTERS; i += 32) {
		float n0i = (float) counters[i] + pseudo0;
		float n1i = (float) counters[COUNTERS + i] + pseudo1;
		float ni = n0i + n1i;

		ig += n0i * __log2f(n0i / ni);
		ig += n1i * __log2f(n1i / ni);
	}

#pragma unroll
	for (int i = 1; i < 32; i *= 2) {
		ig += __shfl_xor(ig, i);
	}

	return ig;

}

template<int DIV>
__forceinline__ __device__
float inf2D_X_warp(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;
	int off0 = threadIdx.x * (DIV + 1);
	int off1 = (DIV + 1) * (DIV + 1) + off0;

	if (threadIdx.x < DIV + 1) {
		float n0i = pseudo0 * (DIV + 1);
		float n1i = pseudo1 * (DIV + 1);
#pragma unroll
		for (int i = 0; i < DIV + 1; i++) {
			n0i += (float) counters[off0 + i];
			n1i += (float) counters[off1 + i];
		}

		float ni = n0i + n1i;
		ig += n0i * __log2f(n0i / ni);
		ig += n1i * __log2f(n1i / ni);
	}

#pragma unroll
	for (int i = 1; i < 16; i *= 2) {
		ig += __shfl_xor(ig, i);
	}

	return ig;
}

template<int DIV>
__forceinline__ __device__
float inf2D_Y_warp(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;
	int off0 = 0;
	int off1 = (DIV + 1) * (DIV + 1) + off0;

	if (threadIdx.x < DIV + 1) {
		float n0i = pseudo0 * (DIV + 1);
		float n1i = pseudo1 * (DIV + 1);
#pragma unroll
		for (int i = 0; i < DIV + 1; i++) {
			n0i += (float) counters[off0 + i * (DIV + 1) + threadIdx.x];
			n1i += (float) counters[off1 + i * (DIV + 1) + threadIdx.x];
		}

		float ni = n0i + n1i;
		ig += n0i * __log2f(n0i / ni);
		ig += n1i * __log2f(n1i / ni);
	}

#pragma unroll
	for (int i = 1; i < 16; i *= 2) {
		ig += __shfl_xor(ig, i);
	}

	return ig;
}

template<int COUNTERS>
__forceinline__ __device__
float totalInf_warp_short(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;

	uint32_t mask = (1 << 16) - 1;

#pragma unroll
	for (int i = threadIdx.x; i < COUNTERS; i += 32) {
		float n0i = (float) (counters[i] & mask) + pseudo0;
		float n1i = (float) (counters[i] >> 16 ) + pseudo1;
		float ni = n0i + n1i;

		ig += n0i * __log2f(n0i / ni);
		ig += n1i * __log2f(n1i / ni);
	}

#pragma unroll
	for (int i = 1; i < 32; i *= 2) {
		ig += __shfl_xor(ig, i);
	}

	return ig;

}

template<int DIV>
__forceinline__ __device__
float inf2D_X_warp_short(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;
	int off = threadIdx.x * (DIV + 1);

	uint32_t mask = (1 << 16) - 1;

	if (threadIdx.x < DIV + 1) {
		float n0i = pseudo0 * (DIV + 1);
		float n1i = pseudo1 * (DIV + 1);
#pragma unroll
		for (int i = 0; i < DIV + 1; i++) {
			n0i += (float) (counters[off + i] & mask);
			n1i += (float) (counters[off + i] >> 16 );
		}

		float ni = n0i + n1i;
		ig += n0i * __log2f(n0i / ni);
		ig += n1i * __log2f(n1i / ni);
	}

#pragma unroll
	for (int i = 1; i < 16; i *= 2) {
		ig += __shfl_xor(ig, i);
	}

	return ig;
}

template<int DIV>
__forceinline__ __device__
float inf2D_Y_warp_short(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;
	int off = 0;

	uint32_t mask = (1 << 16) - 1;

	if (threadIdx.x < DIV + 1) {
		float n0i = pseudo0 * (DIV + 1);
		float n1i = pseudo1 * (DIV + 1);
#pragma unroll
		for (int i = 0; i < DIV + 1; i++) {
			n0i += (float) (counters[off + i * (DIV + 1) + threadIdx.x] & mask);
			n1i += (float) (counters[off + i * (DIV + 1) + threadIdx.x] >> 16 );
		}

		float ni = n0i + n1i;
		ig += n0i * __log2f(n0i / ni);
		ig += n1i * __log2f(n1i / ni);
	}

#pragma unroll
	for (int i = 1; i < 16; i *= 2) {
		ig += __shfl_xor(ig, i);
	}

	return ig;
}


#endif












