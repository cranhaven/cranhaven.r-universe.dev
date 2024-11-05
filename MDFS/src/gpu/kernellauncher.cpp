#include "kernellauncher.h"
#include "kernel_param.h"
#include "not_implemented_exception.h"

#include <sstream>

void notImplemented(KernelParam param) {
	std::ostringstream oss;
	oss << param;
	throw new NotImplementedException(oss.str());
}

void tablesKernelLauncher(int vars,
	FileInfo fi,
	LaunchConfig lc,
	int ix,
	std::vector<int> offset,
	uint64_t** data,
	uint64_t** counters,
	cudaStream_t stream) {

	KernelParam param(lc, true, ix, vars, data, counters, offset,
		fi.varlen0, fi.varlen1, fi.tobjc0, fi.tobjc1);

	for (auto i : kernels) {
		if (i.first == param) {
			i.second(param, stream);
			return;
		}
	}

	notImplemented(param);
}

void mainKernelLauncher(int vars,
	FileInfo fi,
	LaunchConfig lc,
	std::vector<int> offset,
	uint64_t** data,
	uint64_t** counters,
	float* IG,
	cudaStream_t stream) {

	KernelParam param(lc, false, 0, vars, data, counters, offset,
		fi.varlen0, fi.varlen1, fi.tobjc0, fi.tobjc1, IG);

	for (auto i : kernels) {
		if (i.first == param) {
			i.second(param, stream);
			return;
		}
	}

	notImplemented(param);
}
