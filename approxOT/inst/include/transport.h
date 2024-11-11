#ifndef TRANSPORT_H
#define TRANSPORT_H

#include "approxOT_types.h"
#include "cost.h"
#include <string>
#include "utils.h"
#include "trans_hilbert.h"
#include "networkflow.h"
#include "trans_shortsimplex.h"
#include "trans_rank.h"
#include "trans_univariate.h"
#include "trans_univariate_approx_pwr.h"
#include "systematic_sample.h"
#include "trans_approxOT.h"
#include "trans_swap.h"

//' Calculate transportation plans given a cost matrix
//'
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample A
//' @param mass_b A reference to an Eigen::VectorXi 
//' giving the empirical mass in sample B
//' @param cost_matrix A reference to an Eigen::MatrixXd giving 
//' the cost between samples A and B
//' @param idx A two column Eigen::MatrixXi giving the paired
//' indexes between samples.
//' @param mass An Eigen::VectorXd giving the mass between pairs
//' of observations.
//' @param method One of "sinkhorn", "greenkhorn", "randkhorn", 
//' grandkhorn", "hilbert", "shortsimplex", "exact",
//' "networkflow", or "univariate"
//' @param cost_matrix_A The cost matrix between observations in sample 1.
//' For use method is "sinkhorn" and unbiased is true.
//' @param cost_matrix_B The cost matrix between observations in sample 2.
//' For use method is "sinkhorn" and unbiased is true.
//' @param epsilon The value to multiple the median of the
//' cost_matrix by to give a value of lambda for the entropic
//' penalty
//' @param niter The iterations to use for the methods
//' @param unbiased Should the Sinkhorn distance be de-biased? (bool)
//' Note if its unbiased, the transport plan won't be rounded to
//' the feasible set
//' @param threads If method is "exact" or "networkflow", how many
//' threads to use in the optimization problem?
//' @return void
//' @keywords internal
void transport_C(const refVecConst & mass_a, 
                 const refVecConst & mass_b, 
                 refMat cost_matrix, 
                 matrixI & idx, vector & mass, 
                 const std::string & method,
                 refMat cost_matrix_A, 
                 refMat cost_matrix_B,
                 double epsilon = 0.0, 
                 int niter = 0,
                 bool unbiased = false, 
                 int threads = 1);

//' Calculate transportation plans where a cost matrix
//' needs to be calculated
//'
//' @param A An Eigen::MatrixXd of the data in sample A
//' @param B An Eigen::MatrixXd of the data in sample B
//' @param p A double greater than or equal to 1 giving the power
//' to raise the cost matrix by
//' @param ground_p A double greater than or equal to 1 
//' giving the power of the L_p norm
//' @param idx A two column Eigen::MatrixXi giving the paired
//' indexes between samples.
//' @param mass An Eigen::VectorXd giving the mass between pairs
//' of observations.
//' @param method One of "sinkhorn", "greenkhorn", "randkhorn", 
//' grandkhorn", "hilbert", "shortsimplex", "exact",
//' "networkflow", or "univariate"
//' @param a_sort Is the data in A already sorted? (bool)
//' @param epsilon The value to multiple the median of the
//' cost_matrix by to give a value of lambda for the entropic
//' penalty
//' @param niter The iterations to use for the methods
//' @param unbiased Should the Sinkhorn distance be de-biased? (bool)
//' Note if its unbiased, the transport plan won't be rounded to
//' the feasible set
//' @param threads If method is "exact" or "networkflow", how many
//' threads to use in the optimization problem?
//' @return void
//' @keywords internal
void transport(const matrix & A, 
               const matrix & B, 
               const double p, 
               const double ground_p,
               matrixI & idx, 
               vector & mass, 
               const std::string & method, bool & a_sort,
               double epsilon = 0.0, int niter = 0,
               bool unbiased = false, int threads = 1);
  
#endif //TRANSPORT_H
