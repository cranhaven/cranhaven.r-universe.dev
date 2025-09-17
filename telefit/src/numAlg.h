#ifndef _NUMALG_H
#define _NUMALG_H

#include <RcppArmadillo.h>

namespace mcstat2 {
	
	using namespace Rcpp;
	using namespace arma;
	
	// evaluate kron(A,B) * C without storing kron(A,B)
	mat dgemkmm(const mat& A, const mat& B, const mat& C);
	
	// evaluate kron(I_N, A) * B without storing kron(I_N, A) and I_N is
	// the NxN identity matrix
	mat dgeikmm(int N, mat A, mat B);
}

#endif
