#ifndef KERNELLAUNCHER_CUH
#define KERNELLAUNCHER_CUH

#include <vector>
#include "launchconfig.h"

// cudaStream_t
#include <cuda_runtime_api.h>

void tablesKernelLauncher(int vars,
	FileInfo fi,
	LaunchConfig lc,
	int ix,
	std::vector<int> offset,
	uint64_t** data,
	uint64_t** counters,
	cudaStream_t stream);

void mainKernelLauncher(int vars,
	FileInfo fi,
	LaunchConfig lc,
	std::vector<int> offset,
	uint64_t** data,
	uint64_t** counters,
	float* IG,
	cudaStream_t stream);

#endif
