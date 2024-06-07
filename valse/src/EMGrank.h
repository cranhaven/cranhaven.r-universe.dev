#ifndef valse_EMGrank_H
#define valse_EMGrank_H

#include "utils.h"

void EMGrank_core(
	// IN parameters
	const Real* Pi,
	const Real* Rho,
	int mini,
	int maxi,
	const Real* X,
	const Real* Y,
	Real tau,
	const int* rank,
	// OUT parameters
	Real* phi,
	Real* LLF,
	// additional size parameters
	int n,
	int p,
	int m,
	int k);

#endif
