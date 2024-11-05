#ifndef KERNEL_PARAM_H
#define KERNEL_PARAM_H

#include <ostream>
#include <vector>
#include <stdint.h>
#include "launchconfig.h"

// cudaStream_t
#include <cuda_runtime_api.h>

#define MAX_DIM 5

struct KernelParam {
	int tileSize;
	int dim;
	int div;
	ReduceMethod rm; // RM_AVG, RM_MAX
	BinaryFormat bf; // BF_SHIFT, BF_SPLIT
	bool tablesKernel;

	int objs[2];

	int index;
	int disc;
	int vars;
	uint64_t* data[MAX_DIM];
	uint64_t* counters[MAX_DIM];
	int offset[MAX_DIM];
	int packs[2];
	float pseudo[2];
	float* IG;

	KernelParam(LaunchConfig lc, bool tablesKernel, int index,
		int vars, uint64_t** data, uint64_t** counters,
		std::vector<int> offset,
		int packs0, int packs1, int objs0, int objs1,
		float* IG = 0);

	KernelParam(int tileSize, int dim, int div, ReduceMethod rm,
		BinaryFormat bf, bool tablesKernel, int objs0);
};

std::ostream& operator<< (std::ostream& out, KernelParam const& prop);
bool operator==(const KernelParam& lhs, const KernelParam& rhs);
extern std::vector<std::pair<KernelParam, void(*)(KernelParam, cudaStream_t)>> kernels;

#endif

