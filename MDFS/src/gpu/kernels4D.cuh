#ifndef KERNELS4D_CUH
#define KERNELS4D_CUH

#include <stdint.h>
#include "kernel_param.h"
#include "utils.cuh"
#include "cudaerrchk.h"

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int WARPS,
	int BITS>
__global__
__launch_bounds__(32 * WARPS)
void kernel4D(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int WARPS,
	int BITS>
__global__
__launch_bounds__(32 * WARPS)
void kernel4D_short(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int BITS>
__global__
__launch_bounds__(32)
void kernel4D_simple(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int SHORT>
void kernel4DWrapper(KernelParam param, cudaStream_t stream) {
	const int warps = 4;
	dim3 grid(TILE_SIZE / 2, TILE_SIZE / 2, TILE_SIZE * TILE_SIZE / 2);
	dim3 block(32, warps);

	void (*kernel)(KernelParam);

	if (SHORT) {
		if (DIV < 2) kernel = kernel4D_short<TILE_SIZE, DIV, AVG, warps, 1>;
		else if (DIV < 4) kernel = kernel4D_short<TILE_SIZE, DIV, AVG, warps, 2>;
		else if (DIV < 8) kernel = kernel4D_short<TILE_SIZE, DIV, AVG, warps, 3>;
		else kernel = kernel4D_short<TILE_SIZE, DIV, AVG, warps, 4>;
	} else {
		if (DIV < 2) kernel = kernel4D<TILE_SIZE, DIV, AVG, warps, 1>;
		else if (DIV < 4) kernel = kernel4D<TILE_SIZE, DIV, AVG, warps, 2>;
		else if (DIV < 8) kernel = kernel4D<TILE_SIZE, DIV, AVG, warps, 3>;
		else kernel = kernel4D<TILE_SIZE, DIV, AVG, warps, 4>;
	}

	kernel<<<grid, block, 0, stream>>>(param);
	CUDA(cudaPeekAtLastError());

/*	dim3 grid(TILE_SIZE, TILE_SIZE, TILE_SIZE * TILE_SIZE);
	void (*kernel)(KernelParam);

	if (DIV < 2) kernel = kernel4D_simple<TILE_SIZE, DIV, AVG, 1>;
	else if (DIV < 4) kernel = kernel4D_simple<TILE_SIZE, DIV, AVG, 2>;
	else if (DIV < 8) kernel = kernel4D_simple<TILE_SIZE, DIV, AVG, 3>;
	else kernel = kernel4D_simple<TILE_SIZE, DIV, AVG, 4>;

	kernel<<<grid, 32, 0, stream>>>(param);
	CUDA(cudaPeekAtLastError());*/
}

template<int DIV, int IX>
__forceinline__ __device__
float inf4D_warp(uint32_t* counters, float pseudo0, float pseudo1);

template<int DIV, int IX>
__forceinline__ __device__
float inf4D_warp_short(uint32_t* counters, float pseudo0, float pseudo1);

template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int WARPS,
	int BITS>
__global__
__launch_bounds__(32 * WARPS)
void kernel4D(KernelParam param) {
	__shared__ uint32_t counters[2][2][2][2][DIV + 1][DIV + 1][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX0 = 2 * blockIdx.x;
	int bX1 = 2 * blockIdx.y;
	int bX2 = 2 * (blockIdx.z / TILE_SIZE);
	int bX3 = blockIdx.z % TILE_SIZE;
	int tId = blockDim.x * threadIdx.y + threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[4] = {
		param.data[0] + bX0 * varlen,
		param.data[1] + bX1 * varlen,
		param.data[2] + bX2 * varlen,
		param.data[3] + bX3 * varlen
	};

	float acc[4] = {0.0, 0.0, 0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		__syncthreads();
		for (int i = tId; i < 16 * (DIV + 1) * (DIV + 1) * (DIV + 1) * (DIV + 1); i += 32 * WARPS) {
			shared[i] = 0;
		}
		__syncthreads();

		for (int dec = 0; dec < 2; dec++) {
			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32 * WARPS) {

				uint64_t prefetch[4][2];

				#pragma unroll 2
				for (int j = 0; j < 2; j++) {
					prefetch[0][j] = data[0][j * varlen + i];
					prefetch[1][j] = data[1][j * varlen + i];
					prefetch[2][j] = data[2][j * varlen + i];
				}
				prefetch[3][0] = data[3][i];

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					#pragma unroll 2
					for (int x = 0; x < 2; x++) {
						#pragma unroll 2
						for (int y = 0; y < 2; y++) {
							#pragma unroll 2
							for (int z = 0; z < 2; z++) {
								uint32_t* p = &counters[x][y][z][dec][
									prefetch[0][x] & mask][
									prefetch[1][y] & mask][
									prefetch[2][z] & mask][
									prefetch[3][0] & mask];

								atomicAdd(p, 1);
							}
						}
					}

					#pragma unroll 2
					for (int j = 0; j < 2; j++) {
						prefetch[0][j] >>= BITS;
						prefetch[1][j] >>= BITS;
						prefetch[2][j] >>= BITS;
					}
					prefetch[3][0] >>= BITS;
				}
			}
		}
		__syncthreads();

		for (int i = threadIdx.y; i < 8; i += WARPS) {
			if (threadIdx.x == 0) {
				for (int j = 0; j < 2; j++) {
					counters[i / 4][(i / 2) % 2][i % 2][j][0][0][0][0] -=
						(64 / BITS) * param.packs[j] - param.objs[j];
				}
			}

			uint32_t* cnts = &counters[i / 4][(i / 2) % 2][i % 2][0][0][0][0][0];
			float total = totalInf_warp<(DIV + 1) * (DIV + 1) * (DIV + 1) * (DIV + 1)>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf012 = inf4D_warp<DIV, 0>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf013 = inf4D_warp<DIV, 1>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf023 = inf4D_warp<DIV, 2>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf123 = inf4D_warp<DIV, 3>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float relX0 = total - inf123;
			float relX1 = total - inf023;
			float relX2 = total - inf013;
			float relX3 = total - inf012;

			if (threadIdx.x == i) {
				if (AVG) {
					acc[0] += relX0;
					acc[1] += relX1;
					acc[2] += relX2;
					acc[3] += relX3;
				} else {
					acc[0] = (relX0 > acc[0] ? relX0 : acc[0]);
					acc[1] = (relX1 > acc[1] ? relX1 : acc[1]);
					acc[2] = (relX2 > acc[2] ? relX2 : acc[2]);
					acc[3] = (relX3 > acc[3] ? relX3 : acc[3]);
				}
			}
		}

		for (int i = 0; i < 4; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 4
	for (int i = 0; i < 4; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	for (int i = threadIdx.y; i < 8; i += WARPS) {
		int x = i / 4;
		int y = (i / 2) % 2;
		int z = i % 2;

		if (threadIdx.x == i) {
			int X0 = param.offset[0] * TILE_SIZE + bX0 + x;
			int X1 = param.offset[1] * TILE_SIZE + bX1 + y;
			int X2 = param.offset[2] * TILE_SIZE + bX2 + z;
			int X3 = param.offset[3] * TILE_SIZE + bX3;

			if (X0 >= param.vars || X1 >= param.vars
				|| X2 >= param.vars || X3 >= param.vars) {
				continue;
			}

			atomicMax((unsigned*) (param.IG + X0), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + X1), *((unsigned*) &acc[1]));
			atomicMax((unsigned*) (param.IG + X2), *((unsigned*) &acc[2]));
			atomicMax((unsigned*) (param.IG + X3), *((unsigned*) &acc[3]));
		}
	}
}

template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int WARPS,
	int BITS>
__global__
__launch_bounds__(32 * WARPS)
void kernel4D_short(KernelParam param) {
	__shared__ uint32_t counters[2][2][2][DIV + 1][DIV + 1][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX0 = 2 * blockIdx.x;
	int bX1 = 2 * blockIdx.y;
	int bX2 = 2 * (blockIdx.z / TILE_SIZE);
	int bX3 = blockIdx.z % TILE_SIZE;
	int tId = blockDim.x * threadIdx.y + threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[4] = {
		param.data[0] + bX0 * varlen,
		param.data[1] + bX1 * varlen,
		param.data[2] + bX2 * varlen,
		param.data[3] + bX3 * varlen
	};

	float acc[4] = {0.0, 0.0, 0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		__syncthreads();
		for (int i = tId; i < 8 * (DIV + 1) * (DIV + 1) * (DIV + 1) * (DIV + 1); i += 32 * WARPS) {
			shared[i] = 0;
		}
		__syncthreads();

		for (int dec = 0; dec < 2; dec++) {
			uint32_t add = dec ? (1 << 16) : 1;

			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32 * WARPS) {

				uint64_t prefetch[4][2];

				#pragma unroll 2
				for (int j = 0; j < 2; j++) {
					prefetch[0][j] = data[0][j * varlen + i];
					prefetch[1][j] = data[1][j * varlen + i];
					prefetch[2][j] = data[2][j * varlen + i];
				}
				prefetch[3][0] = data[3][i];

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					#pragma unroll 2
					for (int x = 0; x < 2; x++) {
						#pragma unroll 2
						for (int y = 0; y < 2; y++) {
							#pragma unroll 2
							for (int z = 0; z < 2; z++) {
								uint32_t* p = &counters[x][y][z][
									prefetch[0][x] & mask][
									prefetch[1][y] & mask][
									prefetch[2][z] & mask][
									prefetch[3][0] & mask];

								atomicAdd(p, add);
							}
						}
					}

					#pragma unroll 2
					for (int j = 0; j < 2; j++) {
						prefetch[0][j] >>= BITS;
						prefetch[1][j] >>= BITS;
						prefetch[2][j] >>= BITS;
					}
					prefetch[3][0] >>= BITS;
				}
			}
		}
		__syncthreads();

		for (int i = threadIdx.y; i < 8; i += WARPS) {
			if (threadIdx.x == 0) {
				for (int j = 0; j < 2; j++) {
					counters[i / 4][(i / 2) % 2][i % 2][0][0][0][0] -=
						((64 / BITS) * param.packs[j] - param.objs[j]) << (16 * j);
				}
			}

			uint32_t* cnts = &counters[i / 4][(i / 2) % 2][i % 2][0][0][0][0];
			float total = totalInf_warp_short<(DIV + 1) * (DIV + 1) * (DIV + 1) * (DIV + 1)>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf012 = inf4D_warp_short<DIV, 0>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf013 = inf4D_warp_short<DIV, 1>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf023 = inf4D_warp_short<DIV, 2>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float inf123 = inf4D_warp_short<DIV, 3>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float relX0 = total - inf123;
			float relX1 = total - inf023;
			float relX2 = total - inf013;
			float relX3 = total - inf012;

			if (threadIdx.x == i) {
				if (AVG) {
					acc[0] += relX0;
					acc[1] += relX1;
					acc[2] += relX2;
					acc[3] += relX3;
				} else {
					acc[0] = (relX0 > acc[0] ? relX0 : acc[0]);
					acc[1] = (relX1 > acc[1] ? relX1 : acc[1]);
					acc[2] = (relX2 > acc[2] ? relX2 : acc[2]);
					acc[3] = (relX3 > acc[3] ? relX3 : acc[3]);
				}
			}
		}

		for (int i = 0; i < 4; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 4
	for (int i = 0; i < 4; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	for (int i = threadIdx.y; i < 8; i += WARPS) {
		int x = i / 4;
		int y = (i / 2) % 2;
		int z = i % 2;

		if (threadIdx.x == i) {
			int X0 = param.offset[0] * TILE_SIZE + bX0 + x;
			int X1 = param.offset[1] * TILE_SIZE + bX1 + y;
			int X2 = param.offset[2] * TILE_SIZE + bX2 + z;
			int X3 = param.offset[3] * TILE_SIZE + bX3;

			if (X0 >= param.vars || X1 >= param.vars
				|| X2 >= param.vars || X3 >= param.vars) {
				continue;
			}

			atomicMax((unsigned*) (param.IG + X0), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + X1), *((unsigned*) &acc[1]));
			atomicMax((unsigned*) (param.IG + X2), *((unsigned*) &acc[2]));
			atomicMax((unsigned*) (param.IG + X3), *((unsigned*) &acc[3]));
		}
	}
}


template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int BITS>
__global__
__launch_bounds__(32)
void kernel4D_simple(KernelParam param) {
	__shared__ uint32_t counters[2][DIV + 1][DIV + 1][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX0 = blockIdx.x;
	int bX1 = blockIdx.y;
	int bX2 = blockIdx.z / TILE_SIZE;
	int bX3 = blockIdx.z % TILE_SIZE;
	int tId = threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[4] = {
		param.data[0] + bX0 * varlen,
		param.data[1] + bX1 * varlen,
		param.data[2] + bX2 * varlen,
		param.data[3] + bX3 * varlen
	};

	float acc[4] = {0.0, 0.0, 0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		for (int i = tId; i < 2 * (DIV + 1) * (DIV + 1) * (DIV + 1) * (DIV + 1); i += 32) {
			shared[i] = 0;
		}

		for (int dec = 0; dec < 2; dec++) {
			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32) {

				uint64_t X0 = data[0][i];
				uint64_t X1 = data[1][i];
				uint64_t X2 = data[2][i];
				uint64_t X3 = data[3][i];

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					uint32_t* p = &counters[dec][X0 & mask][X1 & mask][X2 & mask][X3 & mask];
					atomicAdd(p, 1);

					X0 >>= BITS;
					X1 >>= BITS;
					X2 >>= BITS;
					X3 >>= BITS;
				}
			}
		}

		if (threadIdx.x == 0) {
			for (int j = 0; j < 2; j++) {
				counters[j][0][0][0][0] -=
					(64 / BITS) * param.packs[j] - param.objs[j];
			}
		}

		uint32_t* cnts = &counters[0][0][0][0][0];
		float total = totalInf_warp<(DIV + 1) * (DIV + 1) * (DIV + 1) * (DIV + 1)>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float inf012 = inf4D_warp<DIV, 0>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float inf013 = inf4D_warp<DIV, 1>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float inf023 = inf4D_warp<DIV, 2>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float inf123 = inf4D_warp<DIV, 3>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float relX0 = total - inf123;
		float relX1 = total - inf023;
		float relX2 = total - inf013;
		float relX3 = total - inf012;

		if (AVG) {
			acc[0] += relX0;
			acc[1] += relX1;
			acc[2] += relX2;
			acc[3] += relX3;
		} else {
			acc[0] = (relX0 > acc[0] ? relX0 : acc[0]);
			acc[1] = (relX1 > acc[1] ? relX1 : acc[1]);
			acc[2] = (relX2 > acc[2] ? relX2 : acc[2]);
			acc[3] = (relX3 > acc[3] ? relX3 : acc[3]);
		}

		for (int i = 0; i < 4; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 4
	for (int i = 0; i < 4; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	if (threadIdx.x == 0) {
		int X0 = param.offset[0] * TILE_SIZE + bX0;
		int X1 = param.offset[1] * TILE_SIZE + bX1;
		int X2 = param.offset[2] * TILE_SIZE + bX2;
		int X3 = param.offset[3] * TILE_SIZE + bX3;

		if (X0 < param.vars && X1 < param.vars && X2 < param.vars && X3 < param.vars) {
			atomicMax((unsigned*) (param.IG + X0), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + X1), *((unsigned*) &acc[1]));
			atomicMax((unsigned*) (param.IG + X2), *((unsigned*) &acc[2]));
			atomicMax((unsigned*) (param.IG + X3), *((unsigned*) &acc[3]));
		}
	}
}

template<int DIV, int IX>
__forceinline__ __device__
float inf4D_warp(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;

	int D1 = (DIV + 1);
	int D2 = (DIV + 1) * D1;
	int D3 = (DIV + 1) * D2;
	int D4 = (DIV + 1) * D3;

	for (int i = threadIdx.x; i < D3; i += 32) {
		float n0i = pseudo0 * D1;
		float n1i = pseudo1 * D1;

		int off0, shift;
		if (IX == 0) { off0 = i * D1; shift = 1; }
		if (IX == 1) { off0 = (i / D1) * D2 + (i % D1); shift = D1; }
		if (IX == 2) { off0 = (i / D2) * D3 + (i % D2); shift = D2; }
		if (IX == 3) { off0 = i; shift = D3; }
		int off1 = D4 + off0;

#pragma unroll
		for (int j = 0; j < D1; j++) {
			n0i += (float) counters[off0 + j * shift];
			n1i += (float) counters[off1 + j * shift];
		}

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

template<int DIV, int IX>
__forceinline__ __device__
float inf4D_warp_short(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;

	int D1 = (DIV + 1);
	int D2 = (DIV + 1) * D1;
	int D3 = (DIV + 1) * D2;
	//int D4 = (DIV + 1) * D3;

	for (int i = threadIdx.x; i < D3; i += 32) {
		float n0i = pseudo0 * D1;
		float n1i = pseudo1 * D1;

		int off, shift;
		if (IX == 0) { off = i * D1; shift = 1; }
		if (IX == 1) { off = (i / D1) * D2 + (i % D1); shift = D1; }
		if (IX == 2) { off = (i / D2) * D3 + (i % D2); shift = D2; }
		if (IX == 3) { off = i; shift = D3; }
		uint64_t mask = (1 << 16) - 1;

#pragma unroll
		for (int j = 0; j < D1; j++) {
			n0i += (float) (counters[off + j * shift] & mask);
			n1i += (float) (counters[off + j * shift] >> 16 );
		}

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

#endif
