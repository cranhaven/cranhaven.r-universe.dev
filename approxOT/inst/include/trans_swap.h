#ifndef TRANS_SWAP_H
#define TRANS_SWAP_H

#include "approxOT_types.h"
#include "cost.h"

//' Transport plan based on swapping
//'
//' @param A An Eigen::MatrixXd of the data in sample A
//' @param B An Eigen::MatrixXd of the data in sample B
//' @param N The columns of A
//' @param M The columns of B
//' @param idx A two column Eigen::MatrixXi giving the paired
//' indexes between samples.
//' @param mass An Eigen::VectorXd giving the mass between pairs
//' of observations.
//' @param ground_p The power for the Lp norm (double >=1)
//' @param p The power for the Wasserstein distance
//' @param tol The tolerance to use for stopping
//' @param niter The number of iterations
//' @return void
//' @keywords internal
void trans_swap(const matrix & A, const matrix & B, 
                int N, int M,
                 matrixI & idx, vector &  mass, 
                 double ground_p,
                 double p, double tol, int niter);

#endif //TRANS_SWAP_H
