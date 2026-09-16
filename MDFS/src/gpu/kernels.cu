#include "kernel_param.h"

#include "kernels2D.cuh"
#include "kernels3D.cuh"
#include "tableskernel.cuh"
#include "splitkernel.cuh"

bool init() {
#define KERNELS2D(DIV) \
	kernels.push_back(std::make_pair(KernelParam(512, 2, (DIV), RM_AVG, BF_SHIFT, false,     0), kernel2DWrapper<512, (DIV), 1, 1>));\
	kernels.push_back(std::make_pair(KernelParam(512, 2, (DIV), RM_MAX, BF_SHIFT, false,     0), kernel2DWrapper<512, (DIV), 0, 1>));\
	kernels.push_back(std::make_pair(KernelParam(512, 2, (DIV), RM_AVG, BF_SHIFT, false, 1<<16), kernel2DWrapper<512, (DIV), 1, 0>));\
	kernels.push_back(std::make_pair(KernelParam(512, 2, (DIV), RM_MAX, BF_SHIFT, false, 1<<16), kernel2DWrapper<512, (DIV), 0, 0>));
#define KERNELS3D(DIV) \
	kernels.push_back(std::make_pair(KernelParam(64, 3, (DIV), RM_AVG, BF_SHIFT, false,     0), kernel3DWrapper<64, (DIV), 1, 1>));\
	kernels.push_back(std::make_pair(KernelParam(64, 3, (DIV), RM_MAX, BF_SHIFT, false,     0), kernel3DWrapper<64, (DIV), 0, 1>));\
	kernels.push_back(std::make_pair(KernelParam(64, 3, (DIV), RM_AVG, BF_SHIFT, false, 1<<16), kernel3DWrapper<64, (DIV), 1, 0>));\
	kernels.push_back(std::make_pair(KernelParam(64, 3, (DIV), RM_MAX, BF_SHIFT, false, 1<<16), kernel3DWrapper<64, (DIV), 0, 0>));

#define SPLITKERNELS(TS, DIM, DIV, BITS) \
	kernels.push_back(std::make_pair(KernelParam((TS),(DIM),(DIV),RM_AVG,BF_SPLIT,false,    0),splitKernelWrapper<(TS),(DIM),(DIV),(BITS),1,1>));\
	kernels.push_back(std::make_pair(KernelParam((TS),(DIM),(DIV),RM_MAX,BF_SPLIT,false,    0),splitKernelWrapper<(TS),(DIM),(DIV),(BITS),0,1>));\
	kernels.push_back(std::make_pair(KernelParam((TS),(DIM),(DIV),RM_AVG,BF_SPLIT,false,1<<16),splitKernelWrapper<(TS),(DIM),(DIV),(BITS),1,0>));\
	kernels.push_back(std::make_pair(KernelParam((TS),(DIM),(DIV),RM_MAX,BF_SPLIT,false,1<<16),splitKernelWrapper<(TS),(DIM),(DIV),(BITS),0,0>));

	KERNELS2D(10)
	KERNELS2D(11)
	KERNELS2D(12)
	KERNELS2D(13)
	KERNELS2D(14)
	KERNELS2D(15)

	KERNELS3D(5)

	SPLITKERNELS(512, 2, 1, 1)
	SPLITKERNELS(512, 2, 2, 2)
	SPLITKERNELS(512, 2, 3, 2)
	SPLITKERNELS(512, 2, 4, 4)
	SPLITKERNELS(512, 2, 5, 4)
	SPLITKERNELS(512, 2, 6, 4)
	SPLITKERNELS(512, 2, 7, 4)
	SPLITKERNELS(512, 2, 8, 4)
	SPLITKERNELS(512, 2, 9, 4)

	SPLITKERNELS(64, 3, 1, 1)
	SPLITKERNELS(64, 3, 2, 2)
	SPLITKERNELS(64, 3, 3, 2)
	SPLITKERNELS(64, 3, 4, 4)

	SPLITKERNELS(32, 4, 1, 1)
	SPLITKERNELS(32, 4, 2, 2)
	SPLITKERNELS(32, 4, 3, 2)

	SPLITKERNELS(8, 5, 1, 1)
	SPLITKERNELS(8, 5, 2, 2)

	// FIXME?
	kernels.push_back(std::make_pair(KernelParam(64, 3, 1, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 1, 1, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 1, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 1, 1, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 1, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 1, 1, 0>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 1, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 1, 1, 0>));

	kernels.push_back(std::make_pair(KernelParam(64, 3, 2, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 2, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 2, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 2, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 2, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 2, 2, 0>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 2, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 2, 2, 0>));

	kernels.push_back(std::make_pair(KernelParam(64, 3, 3, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 3, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 3, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 3, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 3, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 3, 2, 0>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 3, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 3, 2, 0>));

	kernels.push_back(std::make_pair(KernelParam(64, 3, 4, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 4, 4, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 4, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<64, 2, 4, 4, 1>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 4, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 4, 4, 0>));
	kernels.push_back(std::make_pair(KernelParam(64, 3, 4, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<64, 2, 4, 4, 0>));

	kernels.push_back(std::make_pair(KernelParam(32, 4, 1, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<32, 3, 1, 1, 1>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 1, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<32, 3, 1, 1, 1>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 1, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<32, 3, 1, 1, 0>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 1, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<32, 3, 1, 1, 0>));

	kernels.push_back(std::make_pair(KernelParam(32, 4, 2, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<32, 3, 2, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 2, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<32, 3, 2, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 2, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<32, 3, 2, 2, 0>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 2, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<32, 3, 2, 2, 0>));

	kernels.push_back(std::make_pair(KernelParam(32, 4, 3, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<32, 3, 3, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 3, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<32, 3, 3, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 3, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<32, 3, 3, 2, 0>));
	kernels.push_back(std::make_pair(KernelParam(32, 4, 3, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<32, 3, 3, 2, 0>));

	kernels.push_back(std::make_pair(KernelParam(8, 5, 1, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<8, 4, 1, 1, 1>));
	kernels.push_back(std::make_pair(KernelParam(8, 5, 1, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<8, 4, 1, 1, 1>));
	kernels.push_back(std::make_pair(KernelParam(8, 5, 1, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<8, 4, 1, 1, 0>));
	kernels.push_back(std::make_pair(KernelParam(8, 5, 1, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<8, 4, 1, 1, 0>));

	kernels.push_back(std::make_pair(KernelParam(8, 5, 2, RM_AVG, BF_SPLIT, true,     0), tablesKernelWrapper<8, 4, 2, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(8, 5, 2, RM_MAX, BF_SPLIT, true,     0), tablesKernelWrapper<8, 4, 2, 2, 1>));
	kernels.push_back(std::make_pair(KernelParam(8, 5, 2, RM_AVG, BF_SPLIT, true, 1<<16), tablesKernelWrapper<8, 4, 2, 2, 0>));
	kernels.push_back(std::make_pair(KernelParam(8, 5, 2, RM_MAX, BF_SPLIT, true, 1<<16), tablesKernelWrapper<8, 4, 2, 2, 0>));

	return true;
}

static const bool doneInit = init();

