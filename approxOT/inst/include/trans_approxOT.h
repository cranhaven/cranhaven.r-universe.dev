#ifndef TRANS_APPROX_OT_H
#define TRANS_APPROX_OT_H

#include "approxOT_types.h"
#include "round_feasible.h"
#include "trans_sinkhorn.h"
#include "trans_greenkhorn.h"
// #include "trans_randkhorn.h"
// #include "trans_gandkhorn.h"
#include "utils.h"

//' Generates approximate optimal transport plans
//'
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample A
//' @param mass_b A reference to an Eigen::VectorXd
//' giving the empirical mass in sample B
//' @param cost_matrix A reference to an Eigen::MatrixXd giving 
//' the cost between samples A and b
//' @param assign_mat The assigment matrix as an Eigen::MatrixXd
//' @param epsilon The value to multiply the median of the cost matrix
//' by to get the lambda value used for the entropy penalty
//' @param niterations The iterations to use for the methods
//' @param unbiased Should the Sinkhorn distance be de-biased? (bool)
//' Note if its unbiased, the transport plan won't be rounded to
//' the feasible set
//' @param method One of "sinkhorn", "greenkhorn", "randkhorn", 
//' or "grandkhorn"
//' @param cost_matrix_A The cost matrix between observations in sample 1.
//' For use method is "sinkhorn" and unbiased is true.
//' @param cost_matrix_B The cost matrix between observations in sample 2.
//' For use method is "sinkhorn" and unbiased is true.
//' @return void
//' @keywords internal
void trans_approxOT(const refVecConst & mass_a, 
                    const refVecConst & mass_b, 
                    refMat cost_matrix, 
                    matrix & assign_mat,
                    double epsilon, 
                    int niterations,
                    bool unbiased,
                    const std::string & method,
                    refMat cost_matrix_A, 
                    refMat cost_matrix_B);
#endif //TRANS_SINKHORN_H
