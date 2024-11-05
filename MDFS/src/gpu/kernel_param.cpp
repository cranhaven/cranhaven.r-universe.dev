#include "kernel_param.h"

std::vector<std::pair<KernelParam, void(*)(KernelParam, cudaStream_t)>> kernels;

std::ostream& operator<< (std::ostream& out, KernelParam const& prop) {
	out << "tileSize:" << prop.tileSize << " ";
	out << "dim:" << prop.dim << " ";
	out << "div:" << prop.div << " ";
	out << "rm:" << prop.rm << " ";
	out << "bf:" << prop.bf << " ";
	out << "tablesKernel:" << prop.tablesKernel << " ";
	out << (prop.objs[0] < (1 << 16) ? "objc0 < 2^16 " : "objc0 >= 2^16 ");
	out << (prop.objs[1] < (1 << 16) ? "objc1 < 2^16 " : "objc1 >= 2^16 ");
	return out;
}

bool operator==(const KernelParam& lhs, const KernelParam& rhs) {
	bool lobjs = (lhs.objs[0] < (1 << 16)) && (lhs.objs[1] < (1 << 16));
	bool robjs = (rhs.objs[0] < (1 << 16)) && (rhs.objs[1] < (1 << 16));

	return (lobjs == robjs) &&
		(lhs.tileSize == rhs.tileSize) &&
		(lhs.dim == rhs.dim) &&
		(lhs.div == rhs.div) &&
		(lhs.rm == rhs.rm) &&
		(lhs.bf == rhs.bf) &&
		(lhs.tablesKernel == rhs.tablesKernel);
}

KernelParam::KernelParam(LaunchConfig lc, bool tablesKernel, int index,
	int vars, uint64_t** data, uint64_t** counters,
	std::vector<int> offset, int packs0, int packs1,
	int objs0, int objs1, float* IG)
	: tileSize(lc.tileSize), dim(lc.dim), div(lc.div),
	rm(lc.rm), bf(lc.bf), tablesKernel(tablesKernel),
	index(index), disc(lc.disc), vars(vars), IG(IG) {

	objs[0] = objs0;
	objs[1] = objs1;

	for (int i = 0; i < dim; i++) {
		this->data[i] = data[i];
		this->counters[i] = counters[i];
		this->offset[i] = offset[i];
	}

	packs[0] = packs0;
	packs[1] = packs1;

	const float objsmin = std::min(objs0, objs1);

	for (int i = 0; i < 2; i++) {
		pseudo[i] = ((float)objs[i] / objsmin) * lc.pseudo;
	}
}

KernelParam::KernelParam(int tileSize, int dim, int div, ReduceMethod rm,
	BinaryFormat bf, bool tablesKernel, int objs0)
	: tileSize(tileSize), dim(dim), div(div), rm(rm), bf(bf),
	tablesKernel(tablesKernel)  {

	for (int i = 0; i < 2; i++) {
		objs[i] = objs0;
	}
}
