#ifndef SPLITKERNEL_CUH
#define SPLITKERNEL_CUH

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
        int PREFETCH,
        int AVG>
__global__
void splitKernel(KernelParam param);

template<int TILE_SIZE,
	int DIM,
	int DIV,
	int BITS,
	int AVG,
	int SHORT>
void splitKernelWrapper(KernelParam param, cudaStream_t stream) {
	int grid = pow(TILE_SIZE / blockSize(DIM), DIM);
	int block = pow(blockSize(DIM), DIM);

	// PREFETCH = 4 is a compute/memcopy tradeoff (as well as limited by shared memory)
	if (SHORT) {
		splitKernel<uint16_t, TILE_SIZE, DIM, DIV, BITS, blockSize(DIM), 4, AVG>
			<<<grid, block, 0, stream>>>(param);
	} else {
		splitKernel<uint32_t, TILE_SIZE, DIM, DIV, BITS, blockSize(DIM), 4, AVG>
			<<<grid, block, 0, stream>>>(param);
	}
	CUDA(cudaPeekAtLastError());
}

template<typename CType,
	int TILE_SIZE,
	int DIM,
	int DIV,
	int BITS,
        int BLOCK, // block edge length
        int PREFETCH,
        int AVG>
__global__
//__launch_bounds__(pow(BLOCK, DIM))
__launch_bounds__(DIM == 2 ? BLOCK*BLOCK : DIM == 3 ? BLOCK*BLOCK*BLOCK :
DIM == 4 ? BLOCK*BLOCK*BLOCK*BLOCK : BLOCK*BLOCK*BLOCK*BLOCK*BLOCK) // Workaround
void splitKernel(KernelParam param) {
	// size+1 to avoid shared memory bank conflicts
	__shared__ uint64_t prefetch[DIM][BLOCK][PREFETCH * BITS + 1];

	// Variable length in 64bit packs
	int varlen = param.packs[0] + param.packs[1];
	// Data vectors offset by blocks
	uint64_t* data[DIM];

	int var[DIM];
	int off[DIM]; // offset within a block

	int tId = threadIdx.x;
	int bId = blockIdx.x;

	int linId[DIM];
	#pragma unroll
	for (int i = 0; i < DIM; i++) {
		linId[i] = 0;
	}

	float ig[DIM];

	#pragma unroll
	for (int i = 0; i < DIM; i++) {
		var[i] = bId % (TILE_SIZE / BLOCK) * BLOCK;
		off[i] = tId % BLOCK;

		tId /= BLOCK;
		bId /= TILE_SIZE / BLOCK;
		data[i] = param.data[i] + var[i] * varlen;

		ig[i] = 0.0f;
	}

	#pragma unroll
	for (int i = DIM - 1; i >= 0; i--) {
		#pragma unroll
		for (int j = 0; j < DIM; j++) {
			if (i != DIM - 1 - j) {
				linId[j] = linId[j] * TILE_SIZE + var[i] + off[i];
			}
		}
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
					uint64_t bitVec[DIM][DIV]; // bit vectors fo DIV classes (Without 0)

					// Decode (decompress):
					#pragma unroll
					for (int k = 0; k < DIM; k++) {
						#pragma unroll
						for (int l = 1; l <= DIV; l++) {
							bitVec[k][l - 1] = l & 1 ?
								 prefetch[k][off[k]][BITS * j] :
								~prefetch[k][off[k]][BITS * j];

							#pragma unroll
							for (int m = 1; m < BITS; m++) {
								bitVec[k][l - 1] &= l & (1 << m) ?
									 prefetch[k][off[k]][BITS * j + m] :
									~prefetch[k][off[k]][BITS * j + m];
							}
						}
					}

					if (i + BITS * j >= lim) break;

					// Count:
					#pragma unroll
					for (int t = 0; t < pow(DIV, DIM); t++) {
						int tt = t;
						uint64_t vec = bitVec[0][tt % DIV];
						int where = tt % DIV + 1;
						int shift = DIV + 1;
						tt /= DIV;
						#pragma unroll
						for (int d = 1; d < DIM; d++) {
							vec &= bitVec[d][tt % DIV];
							where += shift * (tt % DIV + 1);
							shift *= DIV + 1;
							tt /= DIV;
						}

						counters[where][dec] += __popcll(vec);
					}
				}
			}
		}

		#pragma unroll
		for (int d = 0; d < DIM; d++) {
			#pragma unroll
			for (int cst = 0; cst < pow(DIV, DIM - 1); cst++) {
				int crd[DIM];
				int tmp = cst;
				#pragma unroll
				for (int i = 0; i < DIM - 1; i++) {
					crd[i >= d ? i + 1 : i] = tmp % DIV + 1;
					tmp /= DIV;
				}
				int ref = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					if (i != d) {
						ref = ref * (DIV + 1) + crd[i];
					}
				}
				uint32_t sum[2] = {0, 0};
				#pragma unroll
				for (int t = 1; t <= DIV; t++) {
					crd[d] = t;
					int index = 0;
					#pragma unroll
					for (int i = DIM - 1; i >= 0; i--) {
						index = index * (DIV + 1) + crd[i];
					}
					sum[0] += counters[index][0];
					sum[1] += counters[index][1];
				}
				int lower = DIM - 1 - d;
				uint64_t val = param.counters[lower][linId[lower] * pow(DIV + 1, DIM - 1) + ref];

				crd[d] = 0;
				int index = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					index = index * (DIV + 1) + crd[i];
				}
				counters[index][0] = (uint32_t)(val >> 32) - sum[0];
				counters[index][1] = (uint32_t)((val << 32) >> 32) - sum[1];
			}
		}

		if (DIM > 2) {
		#pragma unroll
		for (int d0 = 0; d0 < DIM; d0++) {
		#pragma unroll
		for (int d1 = d0 + 1; d1 < DIM; d1++) {
			#pragma unroll
			for (int cst = 0; cst < pow(DIV, DIM - 2); cst++) {
				int crd[DIM];
				#pragma unroll
				for (int i = 0; i < DIM; i++) {
					crd[i] = 0;
				}
				int tmp = cst;
				#pragma unroll
				for (int i = 0; i < DIM - 2; i++) {
					crd[i >= d0 ? (i + 1 >= d1 ? i + 2 : i + 1) : i] = tmp % DIV + 1;
					tmp /= DIV;
				}
				int ref = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					if (i != d0) {
						ref = ref * (DIV + 1) + crd[i];
					}
				}
				uint32_t sum[2] = {0, 0};
				#pragma unroll
				for (int t = 1; t <= DIV; t++) {
					crd[d0] = t;
					int index = 0;
					#pragma unroll
					for (int i = DIM - 1; i >= 0; i--) {
						index = index * (DIV + 1) + crd[i];
					}
					sum[0] += counters[index][0];
					sum[1] += counters[index][1];
				}
				int lower = DIM - 1 - d0;
				uint64_t val = param.counters[lower][linId[lower] * pow(DIV + 1, DIM - 1) + ref];
				crd[d0] = 0;
				int index = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					index = index * (DIV + 1) + crd[i];
				}
				counters[index][0] = (uint32_t)(val >> 32) - sum[0];
				counters[index][1] = (uint32_t)((val << 32) >> 32) - sum[1];
			}
		}}}

		if (DIM > 3) {
		#pragma unroll
		for (int d0 = 0; d0 < DIM; d0++) {
		#pragma unroll
		for (int d1 = d0 + 1; d1 < DIM; d1++) {
		#pragma unroll
		for (int d2 = d1 + 1; d2 < DIM; d2++) {
			#pragma unroll
			for (int cst = 0; cst < pow(DIV, DIM - 3); cst++) {
				int crd[DIM];
				#pragma unroll
				for (int i = 0; i < DIM; i++) {
					crd[i] = 0;
				}
				int tmp = cst;
				#pragma unroll
				for (int i = 0; i < DIM - 3; i++) {
					crd[i >= d0 ?
					(i + 1 >= d1 ?
					(i + 2 >= d2 ? i + 3 : i + 2) : i + 1) : i] = tmp % DIV + 1;
					tmp /= DIV;
				}
				int ref = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					if (i != d0) {
						ref = ref * (DIV + 1) + crd[i];
					}
				}
				uint32_t sum[2] = {0, 0};
				#pragma unroll
				for (int t = 1; t <= DIV; t++) {
					crd[d0] = t;
					int index = 0;
					#pragma unroll
					for (int i = DIM - 1; i >= 0; i--) {
						index = index * (DIV + 1) + crd[i];
					}
					sum[0] += counters[index][0];
					sum[1] += counters[index][1];
				}
				int lower = DIM - 1 - d0;
				uint64_t val = param.counters[lower][linId[lower] * pow(DIV + 1, DIM - 1) + ref];
				crd[d0] = 0;
				int index = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					index = index * (DIV + 1) + crd[i];
				}
				counters[index][0] = (uint32_t)(val >> 32) - sum[0];
				counters[index][1] = (uint32_t)((val << 32) >> 32) - sum[1];
			}
		}}}}

		if (DIM > 4) {
		#pragma unroll
		for (int d0 = 0; d0 < DIM; d0++) {
		#pragma unroll
		for (int d1 = d0 + 1; d1 < DIM; d1++) {
		#pragma unroll
		for (int d2 = d1 + 1; d2 < DIM; d2++) {
		#pragma unroll
		for (int d3 = d2 + 1; d3 < DIM; d3++) {
			#pragma unroll
			for (int cst = 0; cst < pow(DIV, DIM - 4); cst++) {
				int crd[DIM];
				#pragma unroll
				for (int i = 0; i < DIM; i++) {
					crd[i] = 0;
				}
				int tmp = cst;
				#pragma unroll
				for (int i = 0; i < DIM - 4; i++) {
					crd[i >= d0 ?
					(i + 1 >= d1 ?
					(i + 2 >= d2 ?
					(i + 3 >= d3 ? i + 4 : i + 3) : i + 2) : i + 1) : i] = tmp % DIV + 1;
					tmp /= DIV;
				}
				int ref = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					if (i != d0) {
						ref = ref * (DIV + 1) + crd[i];
					}
				}
				uint32_t sum[2] = {0, 0};
				#pragma unroll
				for (int t = 1; t <= DIV; t++) {
					crd[d0] = t;
					int index = 0;
					#pragma unroll
					for (int i = DIM - 1; i >= 0; i--) {
						index = index * (DIV + 1) + crd[i];
					}
					sum[0] += counters[index][0];
					sum[1] += counters[index][1];
				}
				int lower = DIM - 1 - d0;
				uint64_t val = param.counters[lower][linId[lower] * pow(DIV + 1, DIM - 1) + ref];
				crd[d0] = 0;
				int index = 0;
				#pragma unroll
				for (int i = DIM - 1; i >= 0; i--) {
					index = index * (DIV + 1) + crd[i];
				}
				counters[index][0] = (uint32_t)(val >> 32) - sum[0];
				counters[index][1] = (uint32_t)((val << 32) >> 32) - sum[1];
			}
		}}}}}

		uint32_t sum[2] = {0, 0};
		#pragma unroll
		for (int i = 1; i < pow(DIV + 1, DIM); i++) {
			sum[0] += counters[i][0];
			sum[1] += counters[i][1];
		}
		counters[0][0] = param.objs[0] - sum[0];
		counters[0][1] = param.objs[1] - sum[1];

		// IG
		float totalInf = 0.0f;
		for (int i = 0; i < pow(DIV + 1, DIM); i++) {
			float n0i = (float)counters[i][0] + param.pseudo[0];
			float n1i = (float)counters[i][1] + param.pseudo[1];
			float ni = n0i + n1i;

			totalInf += n0i * __log2f(n0i / ni);
			totalInf += n1i * __log2f(n1i / ni);
		}

		#pragma unroll
		for (int d = 0; d < DIM; d++) {
			float inf = 0.0f;

			#pragma unroll
			for (int cst = 0; cst < pow(DIV + 1, DIM - 1); cst++) {
				int crd[DIM];
				int tmp = cst;
				#pragma unroll
				for (int i = 0; i < DIM - 1; i++) {
					crd[i >= d ? i + 1 : i] = tmp % (DIV + 1);
					tmp /= DIV + 1;
				}

				float n0i = param.pseudo[0] * (DIV + 1);
				float n1i = param.pseudo[1] * (DIV + 1);

				#pragma unroll
				for (int t = 0; t < DIV + 1; t++) {
					crd[d] = t;
					int index = 0;
					#pragma unroll
					for (int i = DIM - 1; i >= 0; i--) {
						index = index * (DIV + 1) + crd[i];
					}

					n0i += (float)counters[index][0];
					n1i += (float)counters[index][1];
				}

				float ni = n0i + n1i;
				inf += n0i * __log2f(n0i / ni);
				inf += n1i * __log2f(n1i / ni);
			}

			if (AVG) {
				ig[d] += totalInf - inf;
			} else {
				ig[d] = totalInf - inf > ig[d] ? totalInf - inf : ig[d];
			}

		}

		// Offset pointers:
		#pragma unroll
		for (int i = 0; i < DIM; i++) {
			data[i] += TILE_SIZE * varlen;
			param.counters[i] += pow(TILE_SIZE * (DIV + 1), DIM - 1);
		}
	}

	#pragma unroll
	for (int i = 0; i < DIM; i++) {
		if (param.offset[i] * TILE_SIZE + var[i] + off[i] >= param.vars) {
			return;
		}
	}

	#pragma unroll
	for (int i = 0; i < DIM; i++) {
		if (AVG) ig[i] /= (float) param.disc;
		ig[i] = ig[i] < 0.0 ? 0.0 : ig[i];

		int x = param.offset[i] * TILE_SIZE + var[i] + off[i];
		atomicMax((unsigned*) (param.IG + x), *((unsigned*) &ig[i]));
	}
}

#endif
