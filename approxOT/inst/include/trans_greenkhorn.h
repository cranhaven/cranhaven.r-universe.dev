#ifndef TRANS_GREENKHORN_H
#define TRANS_GREENKHORN_H

#include "approxOT_types.h"
#include "utils.h"

//' Generates approximate optimal transport plans 
//' using the greenkhorn algorithm
//'
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 1
//' @param mass_b A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 2
//' @param exp_cost A reference to an Eigen::MatrixXd giving 
//' the cost between samples A and B
//' @param A The assignment matrix
//' @param eta The inverse of lambda value used for 
//' the entropy penalty
//' @param epsilon The desired error bound
//' @param niterations The iterations to use for the methods
//' @return void
//' @keywords internal
void trans_greenkhorn(const refVecConst & mass_a, 
                      const refVecConst & mass_b, 
                    const matrix & exp_cost, 
                    matrix & A,
                    double eta, double epsilon, int niterations);
#endif //TRANS_GREENKHORN_H
