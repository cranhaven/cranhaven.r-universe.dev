#include "trans_approxOT.h"

void trans_approxOT(const refVecConst & mass_a, const refVecConst & mass_b, 
                    refMat cost_matrix, 
                    matrix & assign_mat,
                    double epsilon, int niterations,
                    bool unbiased,
                    const std::string & method,
                    refMat cost_matrix_A, refMat cost_matrix_B) {
  
  double med_cost = median(cost_matrix);
  double eta = 1.0 / (epsilon * med_cost); //avoid underflow
  // double eta = 4 * log(double(mass_a.size())) / epsilon;
  std::string meth = method;
  bool underflow_risk = (cost_matrix.maxCoeff() * eta > 700);
  if (underflow_risk && meth != "sinkhorn_log") {
    Rcpp::warning("Risk of underflow. Switching to log-domain sinkhorn");
    meth = "sinkhorn_log";
  }
  const matrix exp_cost = (-eta * cost_matrix.array() ).exp();
  double epsilon_prime = 1e-8; //epsilon / (8 * cost_matrix.maxCoeff());
  // double epsilon_prime = epsilon;
  // Rcpp::Rcout << epsilon;
  // Rcpp::Rcout << " " << cost_matrix.maxCoeff();
  // Rcpp::Rcout << " " << epsilon_prime;
  
  if (meth == "sinkhorn") {
    vector u = vector::Zero(mass_a.rows());
    vector v = vector::Zero(mass_b.rows());
    // if (unbiased) {
    //   const matrix exp_cost_a = (-eta * cost_matrix_A.array() ).exp();
    //   const matrix exp_cost_b = (-eta * cost_matrix_B.array() ).exp();
    //   vector p = vector::Zero(mass_a.rows());
    //   vector q = vector::Zero(mass_b.rows());
    //   trans_sinkhorn_self(p, mass_a,
    //                           exp_cost_a,
    //                           epsilon_prime/2.0, niterations);
    //   trans_sinkhorn_self(q, mass_b,
    //                           exp_cost_b,
    //                           eta, epsilon_prime/2.0, niterations);
    //   trans_sinkhorn(mass_a, mass_b, cost_matrix, assign_mat, eta, epsilon_prime/2.0, niterations,
    //                      p, q);
    // 
    // } else {
      trans_sinkhorn(mass_a, mass_b, exp_cost, assign_mat, eta, epsilon_prime, niterations,
                     u, v);
    // }
    

  } else if (meth == "sinkhorn_log") {
    vector f = vector::Zero(mass_a.rows());
    vector g = vector::Zero(mass_b.rows());
    // void trans_sinkhorn_log(const refVecConst & mass_a, const refVecConst & mass_b,
    //                         const matrix & cost,
    //                         matrix & Assign,
    //                         double eta, double epsilon, int niterations,
    //                         vector & f_pot, vector & g_pot)
    trans_sinkhorn_log(mass_a, mass_b, cost_matrix, assign_mat, eta, epsilon_prime, niterations,
                   f, g);
    
    
  } else if (meth == "greenkhorn" ) {
    // Rcpp::stop("transport method greenkhorn not found!");
    // trans_greenkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
    //                  const matrix & exp_cost, 
    //                  matrix & A,
    //                  double eta, double epsilon, int niterations)
    trans_greenkhorn(mass_a, mass_b, exp_cost, assign_mat, eta, epsilon_prime, niterations);
  // } else if (meth == "randkhorn") {
  //   // Rcpp::stop("transport method randkhorn not found!");
  //   vector a_tilde = (1.0 - epsilon_prime/8.0) * mass_a.array();
  //   a_tilde += epsilon_prime/(8.0 * double(mass_a.size())) * vector::Ones(mass_a.size());
  //   vector b_tilde = (1.0 - epsilon_prime/8.0) * mass_b.array();
  //   b_tilde += epsilon_prime/(8.0 * double(mass_b.size())) * vector::Ones(mass_b.size());
  //   trans_randkhorn(a_tilde, b_tilde, exp_cost, assign_mat, eta, epsilon_prime, niterations);
  //   // Rcpp::Rcout << "epsilon': " << epsilon_prime/2.0 <<"\n";
  //   // Rcpp::Rcout << "niter: " << niterations <<"\n";
  // } else if (meth == "gandkhorn") {
  //   // Rcpp::stop("transport method gandkhorn not found!");
  //   vector a_tilde = (1.0 - epsilon_prime/8.0) * mass_a.array();
  //   a_tilde += epsilon_prime/(8.0 * double(mass_a.size())) * vector::Ones(mass_a.size());
  //   vector b_tilde = (1.0 - epsilon_prime/8.0) * mass_b.array();
  //   b_tilde += epsilon_prime/(8.0 * double(mass_b.size())) * vector::Ones(mass_b.size());
  //   trans_gandkhorn(a_tilde, b_tilde, exp_cost, assign_mat, eta, epsilon_prime, niterations);
  //   // Rcpp::Rcout << "epsilon': " << epsilon_prime/2.0 <<"\n";
  //   // Rcpp::Rcout << "niter: " << niterations <<"\n";
  }
  // algorithm 2 in Altschuler, J., Weed, J., & Rigollet, P. (2017). 
  // Near-linear time approximation algorithms for optimal transport via Sinkhorn iteration. 
  // 31st Conference on Neural Information Processing Systems, (1), 1–11. Long Beach, CA.
  // Rcpp::Rcout << assign_mat(0,0) << "\n";
  // Rcpp::Rcout << assign_mat.sum() << "\n";
  if (!unbiased) {
    round_feasible(assign_mat, mass_a, mass_b);
  }
  // Rcpp::Rcout << assign_mat(0,0) << "\n";
  // Rcpp::Rcout << assign_mat.sum() << "\n";
}
  
  
  
  
