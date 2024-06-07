#ifndef valse_EMGLLF_H
#define valse_EMGLLF_H

#include "utils.h"

void EMGLLF_core(
	// IN parameters
	const Real* phiInit,
	const Real* rhoInit,
	const Real* piInit,
	const Real* gamInit,
	int mini,
	int maxi,
	Real gamma,
	Real lambda,
	const Real* X,
	const Real* Y,
	Real tau,
	// OUT parameters
	Real* phi,
	Real* rho,
	Real* pi,
	Real* LLF,
	Real* S,
	int* affec,
	// additional size parameters
	int n,
	int p,
	int m,
	int k);

#endif
