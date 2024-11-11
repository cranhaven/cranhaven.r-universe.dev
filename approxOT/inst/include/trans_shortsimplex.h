#ifndef TRANS_SHORTSIMPLEX_H
#define TRANS_SHORTSIMPLEX_H

#include "approxOT_types.h"
extern "C" {
#include "shortsimplex.h"
}

//' Generates exact optimal transport plans 
//' using the shortlist algorithm
//'
//' @param mass_a An Eigen::VectorXi
//' giving the empirical mass in sample 1
//' @param mass_b An Eigen::VectorXi
//' giving the empirical mass in sample 2
//' @param cost_matrix A reference to an Eigen::MatrixXd giving 
//' the cost between samples A and B
//' @param assign_mat The assignment matrix as
//' @param basis_mat An Eigen::MatrixXi giving a variable used to
//' construct the basis functions
//' @return void
//' @keywords internal
void trans_shortsimplex(vectorI & mass_a, 
                        vectorI & mass_b, refMat cost_matrix, 
                        matrixI & assign_mat, matrixI &  basis_mat);
#endif //TRANS_SHORTSIMPLEX_H
