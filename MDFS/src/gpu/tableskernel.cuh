#ifndef TABLESKERNEL_CUH
#define TABLESKERNEL_CUH

#include <stdint.h>
#include "kernel_param.h"
#include "utils.cuh"
#include "cudaerrchk.h"

template<typename CType,
	int TILE_SIZE,
	int DIM,
	int DIV,
	int BITS,
        int BLOCK,
        int PREFETCH>
__global__
void tablesKernel(KernelParam param);

template<int TILE_SIZE,
	int DIM,
	int DIV,
	int BITS,
	int SHORT>
void tablesKernelWrapper(KernelParam param, cudaStream_t stream) {
	int grid = pow(TILE_SIZE / blockSize(DIM), DIM);
	int block = pow(blockSize(DIM), DIM);

	if (SHORT) {
		tablesKernel<uint16_t, TILE_SIZE, DIM, DIV, BITS, blockSize(DIM), 4>
			<<<grid, block, 0, stream>>>(param);
	} else {
		tablesKernel<uint32_t, TILE_SIZE, DIM, DIV, BITS, blockSize(DIM), 4>
			<<<grid, block, 0, stream>>>(param);
	}
	CUDA(cudaPeekAtLastError());
}

// simplified split kernel (no logarithms, no optimisation for lower dims)
template<typename CType,
	int TILE_SIZE,
	int DIM,
	int DIV,
	int BITS,
        int BLOCK,
        int PREFETCH>
__global__
//__launch_bounds__(pow(BLOCK, DIM))
__launch_bounds__(DIM == 2 ? BLOCK*BLOCK : DIM == 3 ? BLOCK*BLOCK*BLOCK :
BLOCK*BLOCK*BLOCK*BLOCK) // Workaround
void tablesKernel(KernelParam param) {
	__shared__ uint64_t prefetch[DIM][BLOCK][PREFETCH * BITS + 1];

	// Variable length in 64bit packs
	int varlen = param.packs[0] + param.packs[1];
	// Data vectors offset by blocks
	uint64_t* data[DIM];

	uint64_t* output = param.counters[param.index];

	int var[DIM];
	int off[DIM]; // offset within a block

	int tId = threadIdx.x;
	int bId = blockIdx.x;

	int linId = 0;
	int shift = 1;

	#pragma unroll
	for (int i = 0; i < DIM; i++) {
		var[i] = bId % (TILE_SIZE / BLOCK) * BLOCK;
		off[i] = tId % BLOCK;

		linId += (var[i] + off[i]) * shift;
		shift *= TILE_SIZE;

		tId /= BLOCK;
		bId /= TILE_SIZE / BLOCK;
		data[i] = param.data[i >= DIM - param.index ? i + 1 : i] + var[i] * varlen;
	}

	for (int r = 0; r < param.disc; r++) { // discretizations
		CType counters[pow(DIV + 1, DIM)][2]; // contingency table
		#pragma unroll
		for (int i = 0; i < pow(DIV + 1, DIM); i++) {
			counters[i][0] = 0;
			counters[i][1] = 0;
		}

		for (int dec = 0; dec < 2; dec++) { // decision variable
			int lim = param.packs[dec] + (dec ? param.packs[0] : 0);
			for (int i = dec ? param.packs[0] : 0; i < lim; i += PREFETCH * BITS) {

				__syncthreads();
				if (threadIdx.x < BLOCK * PREFETCH * BITS) {
					int vari = threadIdx.x / (PREFETCH * BITS); // 0..BLOCK
					int pack = threadIdx.x % (PREFETCH * BITS); // coalesced read

					if (i + pack < lim) {
						#pragma unroll
						for (int j = 0; j < DIM; j++) {
							prefetch[j][vari][pack] = data[j][vari * varlen + i + pack];
						}
					}
				}
				__syncthreads();

				#pragma unroll
				for (int j = 0; j < PREFETCH; j++) { // read PREFETCH, porcess PREFETCH 64bit packs
					uint64_t bitVec[DIM][DIV + 1]; // bit vectors fo DIV + 1 classes

					// Decode:
					#pragma unroll
					for (int k = 0; k < DIM; k++) {
						#pragma unroll
						for (int l = 0; l <= DIV; l++) {
							bitVec[k][l] = l & 1 ?
								 prefetch[k][off[k]][BITS * j] :
								~prefetch[k][off[k]][BITS * j];

							#pragma unroll
							for (int m = 1; m < BITS; m++) {
								bitVec[k][l] &= l & (1 << m) ?
									 prefetch[k][off[k]][BITS * j + m] :
									~prefetch[k][off[k]][BITS * j + m];
							}
						}
					}

					if (i + BITS * j >= lim) break;
					// Count:
					#pragma unroll
					for (int t = 0; t < pow(DIV + 1, DIM); t++) {
						int tt = t;
						uint64_t vec = bitVec[0][tt % (DIV + 1)];
						tt /= (DIV + 1);
						#pragma unroll
						for (int d = 1; d < DIM; d++) {
							vec &= bitVec[d][tt % (DIV + 1)];
							tt /= (DIV + 1);
						}

						counters[t][dec] += __popcll(vec);
					}
				}
			}
		}

		counters[0][0] -= param.packs[0] / BITS * 64 - param.objs[0];
		counters[0][1] -= param.packs[1] / BITS * 64 - param.objs[1];

		// Save results:
		int idx = linId * pow(DIV + 1, DIM);
		#pragma unroll
		for (int i = 0; i < pow(DIV + 1, DIM); i++) {
			uint64_t cnt = (uint64_t)counters[i][0] << 32 | (uint64_t)counters[i][1];
			output[idx + i] = cnt;
		}

		// Offset pointers:
		output += pow(TILE_SIZE * (DIV + 1), DIM);
		#pragma unroll
		for (int i = 0; i < DIM; i++) {
			data[i] += param.tileSize * varlen;
		}
	}

}

#endif
