/*
 * IncompleteCholesky.cpp
 * 
 * Method for the incomplete Cholesky decomposition
 *
 *  Created on: Jul 9, 2019
 *      Author: Christoph Koesner
 */

#include "IncompleteCholesky.h"
#include <vector>
#include <RcppEigen.h>
#include "Kernel.h"

using namespace Eigen;

template<typename DerivedA>
choleskyReturnVal choleskyLowRank(const MatrixBase<DerivedA> &x, Kernel &ker,
		double eps) {

	int nmax = 20, nmax_old = 0;
	const double resFac = 1.5;

	int i = 0;
	int j = 0;
	int iter = 0;
	int jast = 0;
	int n = x.rows();

	double residual = n;
	double b = 0;
	double maxDiag = 0;

	VectorXd x_perm = x;
	MatrixXd G = MatrixXd::Zero(n, nmax);

	std::vector<double> diagG;
	std::vector<double> diagK;
	diagG.reserve(n);

	PermutationMatrix<Dynamic, Dynamic> singlePerm(n);
	PermutationMatrix<Dynamic, Dynamic> fullPerm(n);

	fullPerm.setIdentity();

	for(i = 0; i < n; i++){
		diagG.push_back(ker.calc(x(i), x(i)));
	}
	diagK = diagG;

	while (residual > eps) {
		if (iter == (nmax - 1)) {
			nmax_old = nmax;
			nmax = round(nmax * resFac);
			G.conservativeResize(NoChange, nmax);
			G.block(0, nmax_old, n, nmax - nmax_old) = MatrixXd::Zero(n,
					nmax - nmax_old);
		}
		/*
		 * permutation of already calculated elements
		 */
		if (jast != iter) {
			singlePerm.setIdentity();
			singlePerm.applyTranspositionOnTheLeft(jast, iter);
			fullPerm.applyTranspositionOnTheLeft(jast, iter);
			x_perm = singlePerm * x_perm;
			for (i = 0; i <= iter; i++) {
				b = G(jast, i);
				G(jast, i) = G(iter, i);
				G(iter, i) = b;
			}
		}

		// update diagonal element
		G(iter, iter) = sqrt(diagG[jast]);

		// calculate column
		for (i = iter + 1; i <= n - 1; i++) {
			G(i, iter) = ker.calc(x_perm(iter), x_perm(i));
		}

		if (iter > 0) {
			for (j = 0; j < iter; j++) {
				for (i = iter + 1; i < n; i++) {
					G(i, iter) = G(i, iter) - G(i, j) * G(iter, j);
				}
			}
		}

		for (i = iter + 1; i <= n - 1; i++) {
			G(i, iter) /= G(iter, iter);
		}

		residual = 0;
		jast = iter + 1;
		maxDiag = 0;
		/*
		 * finds the maximum diagonal element of the residual matrix.
		 * sets jast to the corresponding index. calculates the residuum
		 * (sum of the remaining diagonal) in the process.
		 */
		for (i = iter + 1; i < n; i++) {
			b = diagK[fullPerm.indices()(i)];
			for (j = 0; j <= iter; j++) {
				b -= G(i, j) * G(i, j);
			}
			diagG[i] = b;
			if (b > maxDiag) {
				jast = i;
				maxDiag = b;
			}
			residual += b;
		}
//		std::cout << "Iteration " << iter << " done." << std::endl
//				<< std::flush;
		iter++;
	}
	G.conservativeResize(NoChange, iter);

	choleskyReturnVal returnVal { G, fullPerm };
	return returnVal;
}

template choleskyReturnVal choleskyLowRank<Matrix<double, -1, 1, 0, -1, 1>>(
		const MatrixBase<Matrix<double, -1, 1, 0, -1, 1>>&, Kernel&, double);
