#ifndef CUCUBES_H
#define CUCUBES_H

// required to catch possible CUDA exception
#include "cuda_exception.h"

// required to catch possible NotImplementedException
#include "not_implemented_exception.h"

void run_cucubes(
	int n,
	int k,
	int dimension,
	int divisions,
	int discretizations,
	int seed,
	double range,
	double pseudocount,
	double* data,
	int* decision,
	double* IGmax);

#endif
