#ifndef WASSERSTEIN_H
#define WASSERSTEIN_H

#include "approxOT_types.h"
#include "sort.h"

//' Calculates exact or approximate optimal transport distances
//'
//' @param mass_a A reference to an Eigen::VectorXd 
//' with the empirical weights from sample 1
//' @param mass_b A reference to an Eigen::VectorXd 
//' with the empirical weights from sample 2
//' @param cost The cost matrix
//' @param p The power to raise the cost by
//' @param from The indexes of the first sample
//' @param to he indexes of the second sample
//' @return returns a double value denoting the distance
//' @keywords internal
double wasserstein(const refVec & mass_a, const refVec & mass_b,
                   const refMat & cost, const double p, 
                   const refVecI & from,  const refVecI & to);

//' Calculates exact or approximate optimal transport distances
//' averaged by observation
//'
//' @param X a matrix of observations from sample 1, already sorted
//' @param Y a matrix of observations from sample 2, already sorted
//' @return returns a double value denoting the distance
//' @details Just calculates the univariate by observation
//' squared wasserstein distance
//' @keywords internal
double wasserstein_2_iid_2(refMat X, refMat Y);

#endif //WASSERSTEIN_H
