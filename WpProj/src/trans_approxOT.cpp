#include "trans_approxOT.h"

void trans_approxOT(const refVecConst & mass_a, const refVecConst & mass_b, 
                    refMat cost_matrix, 
                    matrix & assign_mat,
                    double epsilon, int niterations,
                    const std::string & method) {
  
  double med_cost = median(cost_matrix);
  double eta = 1.0 / (epsilon * med_cost); //avoid underflow
  // double eta = 4 * log(double(mass_a.size())) / epsilon;
  const matrix exp_cost = (-eta * cost_matrix.array() ).exp();
  double epsilon_prime = epsilon / (8 * cost_matrix.maxCoeff());
  // double epsilon_prime = epsilon;
  // Rcpp::Rcout << epsilon;
  // Rcpp::Rcout << " " << cost_matrix.maxCoeff();
  // Rcpp::Rcout << " " << epsilon_prime;
  
  if (method == "sinkhorn") {
    trans_sinkhorn(mass_a, mass_b, exp_cost, assign_mat, eta, epsilon_prime/2.0, niterations);
  } else if (method == "greenkhorn" ) {
    // Rcpp::stop("transport method greenkhorn not found!");
    // trans_greenkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
    //                  const matrix & exp_cost, 
    //                  matrix & A,
    //                  double eta, double epsilon, int niterations)
    trans_greenkhorn(mass_a, mass_b, exp_cost, assign_mat, eta, epsilon_prime/2.0, niterations);
  // } else if (method == "randkhorn") {
  //   // Rcpp::stop("transport method randkhorn not found!");
  //   vector a_tilde = (1.0 - epsilon_prime/8.0) * mass_a.array();
  //   a_tilde += epsilon_prime/(8.0 * double(mass_a.size())) * vector::Ones(mass_a.size());
  //   vector b_tilde = (1.0 - epsilon_prime/8.0) * mass_b.array();
  //   b_tilde += epsilon_prime/(8.0 * double(mass_b.size())) * vector::Ones(mass_b.size());
  //   trans_randkhorn(a_tilde, b_tilde, exp_cost, assign_mat, eta, epsilon_prime/2.0, niterations);
  //   // Rcpp::Rcout << "epsilon': " << epsilon_prime/2.0 <<"\n";
  //   // Rcpp::Rcout << "niter: " << niterations <<"\n";
  // } else if (method == "gandkhorn") {
  //   // Rcpp::stop("transport method gandkhorn not found!");
  //   vector a_tilde = (1.0 - epsilon_prime/8.0) * mass_a.array();
  //   a_tilde += epsilon_prime/(8.0 * double(mass_a.size())) * vector::Ones(mass_a.size());
  //   vector b_tilde = (1.0 - epsilon_prime/8.0) * mass_b.array();
  //   b_tilde += epsilon_prime/(8.0 * double(mass_b.size())) * vector::Ones(mass_b.size());
  //   trans_gandkhorn(a_tilde, b_tilde, exp_cost, assign_mat, eta, epsilon_prime/2.0, niterations);
  //   // Rcpp::Rcout << "epsilon': " << epsilon_prime/2.0 <<"\n";
  //   // Rcpp::Rcout << "niter: " << niterations <<"\n";
  }
  // algorithm 2 in Altschuler, J., Weed, J., & Rigollet, P. (2017). 
  // Near-linear time approximation algorithms for optimal transport via Sinkhorn iteration. 
  // 31st Conference on Neural Information Processing Systems, (1), 1–11. Long Beach, CA.
  // Rcpp::Rcout << assign_mat(0,0) << "\n";
  // Rcpp::Rcout << assign_mat.sum() << "\n";
  round_feasible(assign_mat, mass_a, mass_b);
  // Rcpp::Rcout << assign_mat(0,0) << "\n";
  // Rcpp::Rcout << assign_mat.sum() << "\n";
}
  
  
  
  
