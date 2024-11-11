#ifndef TRANS_SINKHORN_H
#define TRANS_SINKHORN_H

#include "approxOT_types.h"
#include "utils.h"

//' Generates approximate optimal transport plans 
//' using the sinkhorn algorithm
//'
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 1
//' @param mass_b A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 2
//' @param exp_cost A reference to an Eigen::MatrixXd giving 
//' the exponentiated cost between samples A and B
//' @param A The assignment matrix
//' @param eta The inverse of lambda value used for 
//' the entropy penalty
//' @param epsilon The desired error bound
//' @param niterations The iterations to use for the methods
//' @param f The potentials of the dual problem
//' @param g The potentials of the dual problem
//' @return void
//' @keywords internal
void trans_sinkhorn(const refVecConst & mass_a, 
                    const refVecConst & mass_b,
                    const matrix & exp_cost,
                    matrix & A,
                    double eta, double epsilon, int niterations,
                    vector & f, vector & g);

//' Self-sinkhorn distance
//'
//' @param f The potentials of the dual problem
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 1
//' @param eta The inverse of lambda value used for 
//' the entropy penalty
//' @param exp_cost A reference to an Eigen::MatrixXd giving 
//' the exponentiated cost between samples A and B
//' @param A The assignment matrix
//' @param epsilon The desired error bound
//' @param niterations The iterations to use for the methods
//' @return void
//' @keywords internal
void trans_sinkhorn_self(vector & f, const refVecConst & mass_a,
                         double eta,
                         const matrix & exp_cost,
                         double epsilon, int niterations);

//' Generates approximate optimal transport plans 
//' using the sinkhorn algorithm on the log scale
//'
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 1
//' @param mass_b A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 2
//' @param cost A reference to an Eigen::MatrixXd giving 
//' the cost between samples A and B
//' @param Assign The assignment matrix
//' @param eta The inverse of lambda value used for 
//' the entropy penalty
//' @param epsilon The desired error bound
//' @param niterations The iterations to use for the methods
//' @param f_pot The potentials of the dual problem
//' @param g_pot The potentials of the dual problem
//' @return void
//' @keywords internal
void trans_sinkhorn_log(const refVecConst & mass_a, const refVecConst & mass_b,
                        const matrix & cost,
                        matrix & Assign,
                        double eta, double epsilon, int niterations,
                        vector & f_pot, vector & g_pot);

//' Autocorr sinkhorn distance
//'
//' @param f The potentials of the dual problem
//' @param mass_a A reference to an Eigen::VectorXd
//' giving the empirical mass in sample 1
//' @param eta The inverse of lambda value used for 
//' the entropy penalty
//' @param exp_cost A reference to an Eigen::MatrixXd giving 
//' the exponentiated cost between samples A and B
//' @param A The assignment matrix
//' @param epsilon The desired error bound
//' @param niterations The iterations to use for the methods
//' @return void
//' @keywords internal
void trans_sinkhorn_autocorr(vector & f, const refVecConst & mass_a,
                             const matrix & exp_cost,
                             double eta, double epsilon, int niterations);
#endif //TRANS_SINKHORN_H
