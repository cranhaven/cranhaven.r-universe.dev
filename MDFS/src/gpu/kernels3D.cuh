#ifndef KERNELS3D_CUH
#define KERNELS3D_CUH

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
	int PY,
	int PZ>
__global__
void kernel3D(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int WARPS,
	int BITS,
	int PX,
	int PY,
	int PZ>
__global__
void kernel3D_short(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int BITS>
__global__
void kernel3D_simple(KernelParam param);

template<int TILE_SIZE,
	int DIV,
	int AVG,
	int SHORT>
void kernel3DWrapper(KernelParam param, cudaStream_t stream) {
	const int warps = 4;
	dim3 grid(TILE_SIZE / 2, TILE_SIZE / 2, TILE_SIZE / 2);
	dim3 block(32, warps);

	void (*kernel)(KernelParam);

	if (SHORT) {
		if (DIV < 2) kernel = kernel3D_short<TILE_SIZE, DIV, AVG, warps, 1, 2, 2, 2>;
		else if (DIV < 4) kernel = kernel3D_short<TILE_SIZE, DIV, AVG, warps, 2, 2, 2, 2>;
		else if (DIV < 8) kernel = kernel3D_short<TILE_SIZE, DIV, AVG, warps, 3, 2, 2, 2>;
		else kernel = kernel3D_short<TILE_SIZE, DIV, AVG, warps, 4, 2, 2, 2>;
	} else {
		if (DIV < 2) kernel = kernel3D<TILE_SIZE, DIV, AVG, warps, 1, 2, 2, 2>;
		else if (DIV < 4) kernel = kernel3D<TILE_SIZE, DIV, AVG, warps, 2, 2, 2, 2>;
		else if (DIV < 8) kernel = kernel3D<TILE_SIZE, DIV, AVG, warps, 3, 2, 2, 2>;
		else kernel = kernel3D<TILE_SIZE, DIV, AVG, warps, 4, 2, 2, 2>;
	}

	kernel<<<grid, block, 0, stream>>>(param);
	CUDA(cudaPeekAtLastError());

/*	dim3 grid(TILE_SIZE, TILE_SIZE, TILE_SIZE);
	void (*kernel)(KernelParam);

	if (DIV < 2) kernel = kernel3D_simple<TILE_SIZE, DIV, AVG, 1>;
	else if (DIV < 4) kernel = kernel3D_simple<TILE_SIZE, DIV, AVG, 2>;
	else if (DIV < 8) kernel = kernel3D_simple<TILE_SIZE, DIV, AVG, 3>;
	else kernel = kernel3D_simple<TILE_SIZE, DIV, AVG, 4>;

	kernel<<<grid, 32, 0, stream>>>(param);
	CUDA(cudaPeekAtLastError());*/
}

template<int DIV, int IX>
__forceinline__ __device__
float inf3D_warp(uint32_t* counters, float pseudo0, float pseudo1);

template<int DIV, int IX>
__forceinline__ __device__
float inf3D_warp_short(uint32_t* counters, float pseudo0, float pseudo1);

template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int WARPS,
	int BITS,
	int PX,
	int PY,
	int PZ>
__global__
__launch_bounds__(32 * WARPS)
void kernel3D(KernelParam param) {
	__shared__ uint32_t counters[PX][PY][PZ][2][DIV + 1][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX = PX * blockIdx.x;
	int bY = PY * blockIdx.y;
	int bZ = PZ * blockIdx.z;
	int tId = blockDim.x * threadIdx.y + threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[3] = {
		param.data[0] + bX * varlen,
		param.data[1] + bY * varlen,
		param.data[2] + bZ * varlen
	};

	float acc[3] = {0.0, 0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		__syncthreads();
		for (int i = tId; i < 2 * PX * PY * PZ * (DIV + 1) * (DIV + 1) * (DIV + 1); i += 32 * WARPS) {
			shared[i] = 0;
		}
		__syncthreads();

		for (int dec = 0; dec < 2; dec++) {
			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32 * WARPS) {

				uint64_t prefetch[3][4];

				#pragma unroll
				for (int j = 0; j < PX; j++) {
					prefetch[0][j] = data[0][j * varlen + i];
				}
				#pragma unroll
				for (int j = 0; j < PY; j++) {
					prefetch[1][j] = data[1][j * varlen + i];
				}
				#pragma unroll
				for (int j = 0; j < PZ; j++) {
					prefetch[2][j] = data[2][j * varlen + i];
				}

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					#pragma unroll
					for (int x = 0; x < PX; x++) {
						#pragma unroll
						for (int y = 0; y < PY; y++) {
							#pragma unroll
							for (int z = 0; z < PZ; z++) {
								uint32_t* p = &counters[x][y][z][dec][
									prefetch[0][x] & mask][
									prefetch[1][y] & mask][
									prefetch[2][z] & mask];

								atomicAdd(p, 1);
							}
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
					#pragma unroll
					for (int j = 0; j < PZ; j++) {
						prefetch[2][j] >>= BITS;
					}
				}
			}
		}
		__syncthreads();

		for (int i = threadIdx.y; i < PX * PY * PZ; i += WARPS) {
			if (threadIdx.x == 0) {
				for (int j = 0; j < 2; j++) {
					counters[i / (PZ * PY)][(i / PZ) % PY][i % PZ][j][0][0][0] -=
						(64 / BITS) * param.packs[j] - param.objs[j];
				}
			}

			uint32_t* cnts = &counters[i / (PZ * PY)][(i / PZ) % PY][i % PZ][0][0][0][0];
			float total = totalInf_warp<(DIV + 1) * (DIV + 1) * (DIV + 1)>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infXY = inf3D_warp<DIV, 0>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infXZ = inf3D_warp<DIV, 1>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infYZ = inf3D_warp<DIV, 2>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float relX = total - infYZ;
			float relY = total - infXZ;
			float relZ = total - infXY;

			if (threadIdx.x == i) {
				if (AVG) {
					acc[0] += relX;
					acc[1] += relY;
					acc[2] += relZ;
				} else {
					acc[0] = (relX > acc[0] ? relX : acc[0]);
					acc[1] = (relY > acc[1] ? relY : acc[1]);
					acc[2] = (relZ > acc[2] ? relZ : acc[2]);
				}
			}
		}

		for (int i = 0; i < 3; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 3
	for (int i = 0; i < 3; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	for (int i = threadIdx.y; i < PX * PY * PZ; i += WARPS) {
		int x = i / (PY * PZ);
		int y = (i / PZ) % PY;
		int z = i % PZ;

		if (threadIdx.x == i) {
			int X = param.offset[0] * TILE_SIZE + bX + x;
			int Y = param.offset[1] * TILE_SIZE + bY + y;
			int Z = param.offset[2] * TILE_SIZE + bZ + z;

			if (X >= param.vars || Y >= param.vars || Z >= param.vars) {
				continue;
			}

			atomicMax((unsigned*) (param.IG + X), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + Y), *((unsigned*) &acc[1]));
			atomicMax((unsigned*) (param.IG + Z), *((unsigned*) &acc[2]));
		}
	}
}

template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int WARPS,
	int BITS,
	int PX,
	int PY,
	int PZ>
__global__
__launch_bounds__(32 * WARPS)
void kernel3D_short(KernelParam param) {
	__shared__ uint32_t counters[PX][PY][PZ][DIV + 1][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX = PX * blockIdx.x;
	int bY = PY * blockIdx.y;
	int bZ = PZ * blockIdx.z;
	int tId = blockDim.x * threadIdx.y + threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[3] = {
		param.data[0] + bX * varlen,
		param.data[1] + bY * varlen,
		param.data[2] + bZ * varlen
	};

	float acc[3] = {0.0, 0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		__syncthreads();
		for (int i = tId; i < PX * PY * PZ * (DIV + 1) * (DIV + 1) * (DIV + 1); i += 32 * WARPS) {
			shared[i] = 0;
		}
		__syncthreads();

		for (int dec = 0; dec < 2; dec++) {
			uint32_t add = dec ? (1 << 16) : 1;

			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32 * WARPS) {

				uint64_t prefetch[3][4];

				#pragma unroll
				for (int j = 0; j < PX; j++) {
					prefetch[0][j] = data[0][j * varlen + i];
				}
				#pragma unroll
				for (int j = 0; j < PY; j++) {
					prefetch[1][j] = data[1][j * varlen + i];
				}
				#pragma unroll
				for (int j = 0; j < PZ; j++) {
					prefetch[2][j] = data[2][j * varlen + i];
				}

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					#pragma unroll
					for (int x = 0; x < PX; x++) {
						#pragma unroll
						for (int y = 0; y < PY; y++) {
							#pragma unroll
							for (int z = 0; z < PZ; z++) {
								uint32_t* p = &counters[x][y][z][
									prefetch[0][x] & mask][
									prefetch[1][y] & mask][
									prefetch[2][z] & mask];

								atomicAdd(p, add);
							}
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
					#pragma unroll
					for (int j = 0; j < PZ; j++) {
						prefetch[2][j] >>= BITS;
					}
				}
			}
		}
		__syncthreads();

		for (int i = threadIdx.y; i < PX * PY * PZ; i += WARPS) {
			if (threadIdx.x == 0) {
				for (int j = 0; j < 2; j++) {
					counters[i / (PZ * PY)][(i / PZ) % PY][i % PZ][0][0][0] -=
						((64 / BITS) * param.packs[j] - param.objs[j]) << (16 * j);
				}
			}

			uint32_t* cnts = &counters[i / (PZ * PY)][(i / PZ) % PY][i % PZ][0][0][0];
			float total = totalInf_warp_short<(DIV + 1) * (DIV + 1) * (DIV + 1)>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infXY = inf3D_warp_short<DIV, 0>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infXZ = inf3D_warp_short<DIV, 1>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float infYZ = inf3D_warp_short<DIV, 2>(cnts,
				param.pseudo[0], param.pseudo[1]);

			float relX = total - infYZ;
			float relY = total - infXZ;
			float relZ = total - infXY;

			if (threadIdx.x == i) {
				if (AVG) {
					acc[0] += relX;
					acc[1] += relY;
					acc[2] += relZ;
				} else {
					acc[0] = (relX > acc[0] ? relX : acc[0]);
					acc[1] = (relY > acc[1] ? relY : acc[1]);
					acc[2] = (relZ > acc[2] ? relZ : acc[2]);
				}
			}
		}

		for (int i = 0; i < 3; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 3
	for (int i = 0; i < 3; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	for (int i = threadIdx.y; i < PX * PY * PZ; i += WARPS) {
		int x = i / (PY * PZ);
		int y = (i / PZ) % PY;
		int z = i % PZ;

		if (threadIdx.x == i) {
			int X = param.offset[0] * TILE_SIZE + bX + x;
			int Y = param.offset[1] * TILE_SIZE + bY + y;
			int Z = param.offset[2] * TILE_SIZE + bZ + z;

			if (X >= param.vars || Y >= param.vars || Z >= param.vars) {
				continue;
			}

			atomicMax((unsigned*) (param.IG + X), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + Y), *((unsigned*) &acc[1]));
			atomicMax((unsigned*) (param.IG + Z), *((unsigned*) &acc[2]));
		}
	}
}


template<int TILE_SIZE,
	int DIV,
	int AVG, // 0 - max, 1 - avg
	int BITS>
__global__
__launch_bounds__(32)
void kernel3D_simple(KernelParam param) {
	__shared__ uint32_t counters[2][DIV + 1][DIV + 1][DIV + 1];
	uint32_t* shared = (uint32_t*) counters;

	int bX = blockIdx.x;
	int bY = blockIdx.y;
	int bZ = blockIdx.z;
	int tId = threadIdx.x;

	int varlen = param.packs[0] + param.packs[1];
	uint64_t* data[3] = {
		param.data[0] + bX * varlen,
		param.data[1] + bY * varlen,
		param.data[2] + bZ * varlen
	};

	float acc[3] = {0.0, 0.0, 0.0};
	for (int r = 0; r < param.disc; r++) {
		for (int i = tId; i < 2 * (DIV + 1) * (DIV + 1) * (DIV + 1); i += 32) {
			shared[i] = 0;
		}

		for (int dec = 0; dec < 2; dec++) {
			for (int i = tId + (dec ? param.packs[0] : 0);
				i < param.packs[dec] + (dec ? param.packs[0] : 0);
				i += 32) {

				uint64_t X = data[0][i];
				uint64_t Y = data[1][i];
				uint64_t Z = data[2][i];

				uint64_t mask = (1 << BITS) - 1;
				#pragma unroll
				for (int k = 0; k < 64 / BITS; k++) {
					uint32_t* p = &counters[dec][X & mask][Y & mask][Z & mask];
					atomicAdd(p, 1);

					X >>= BITS;
					Y >>= BITS;
					Z >>= BITS;
				}
			}
		}

		if (threadIdx.x == 0) {
			for (int j = 0; j < 2; j++) {
				counters[j][0][0][0] -=
					(64 / BITS) * param.packs[j] - param.objs[j];
			}
		}

		uint32_t* cnts = &counters[0][0][0][0];
		float total = totalInf_warp<(DIV + 1) * (DIV + 1) * (DIV + 1)>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float infXY = inf3D_warp<DIV, 0>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float infXZ = inf3D_warp<DIV, 1>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float infYZ = inf3D_warp<DIV, 2>(cnts,
			param.pseudo[0], param.pseudo[1]);

		float relX = total - infYZ;
		float relY = total - infXZ;
		float relZ = total - infXY;

		if (AVG) {
			acc[0] += relX;
			acc[1] += relY;
			acc[2] += relZ;
		} else {
			acc[0] = (relX > acc[0] ? relX : acc[0]);
			acc[1] = (relY > acc[1] ? relY : acc[1]);
			acc[2] = (relZ > acc[2] ? relZ : acc[2]);
		}

		for (int i = 0; i < 3; i++) {
			data[i] += TILE_SIZE * varlen;
		}
	}

	#pragma unroll 3
	for (int i = 0; i < 3; i++) {
		if (AVG) acc[i] /= (float) param.disc;
		acc[i] = (acc[i] < 0.0 ? 0.0 : acc[i]);
	}

	if (threadIdx.x == 0) {
		int X = param.offset[0] * TILE_SIZE + bX;
		int Y = param.offset[1] * TILE_SIZE + bY;
		int Z = param.offset[2] * TILE_SIZE + bZ;

		if (X < param.vars && Y < param.vars && Z < param.vars) {
			atomicMax((unsigned*) (param.IG + X), *((unsigned*) &acc[0]));
			atomicMax((unsigned*) (param.IG + Y), *((unsigned*) &acc[1]));
			atomicMax((unsigned*) (param.IG + Z), *((unsigned*) &acc[2]));
		}
	}
}

template<int DIV, int IX>
__forceinline__ __device__
float inf3D_warp(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;

	for (int i = threadIdx.x; i < (DIV + 1) * (DIV + 1); i += 32) {
		float n0i = pseudo0 * (DIV + 1);
		float n1i = pseudo1 * (DIV + 1);

		int off0, shift;
		if (IX == 0) { off0 = i * (DIV + 1); shift = 1; }
		if (IX == 1) { off0 = (i / (DIV + 1)) * (DIV + 1) * (DIV + 1) + (i % (DIV + 1)); shift = (DIV + 1); }
		if (IX == 2) { off0 = i; shift = (DIV + 1) * (DIV + 1); }
		int off1 = (DIV + 1) * (DIV + 1) * (DIV + 1) + off0;

#pragma unroll
		for (int j = 0; j < DIV + 1; j++) {
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
float inf3D_warp_short(uint32_t* counters, float pseudo0, float pseudo1) {
	float ig = 0.0;

	for (int i = threadIdx.x; i < (DIV + 1) * (DIV + 1); i += 32) {
		float n0i = pseudo0 * (DIV + 1);
		float n1i = pseudo1 * (DIV + 1);

		int off, shift;
		if (IX == 0) { off = i * (DIV + 1); shift = 1; }
		if (IX == 1) { off = (i / (DIV + 1)) * (DIV + 1) * (DIV + 1) + (i % (DIV + 1)); shift = (DIV + 1); }
		if (IX == 2) { off = i; shift = (DIV + 1) * (DIV + 1); }
		uint64_t mask = (1 << 16) - 1;

#pragma unroll
		for (int j = 0; j < DIV + 1; j++) {
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





