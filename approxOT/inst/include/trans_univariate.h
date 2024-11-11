#ifndef TRANS_UNIVARIATE_H
#define TRANS_UNIVARIATE_H

#include "approxOT_types.h"
#include "sort.h"

//' Transport plan based for univariate measures
//'
//' @param A An Eigen::MatrixXd of the data in sample A
//' @param B An Eigen::MatrixXd of the data in sample B
//' @param N The columns of A
//' @param M The columns of B
//' @param idx A two column Eigen::MatrixXi giving the paired
//' indexes between samples.
//' @param mass An Eigen::VectorXd giving the mass between pairs
//' of observations.
//' @param a_sort Is the data in A already sorted? (bool)
//' @return void
//' @keywords internal
void trans_univariate(const vector & A, const vector & B, 
                      int N, int M,
                      matrixI & idx, vector & mass, 
                      bool & a_sort);
#endif //TRANS_UNIVARIATE_H
