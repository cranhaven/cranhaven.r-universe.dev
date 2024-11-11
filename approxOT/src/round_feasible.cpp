#include "round_feasible.h"

void round_feasible(matrix & F, const refVecConst & mass_a, const refVecConst & mass_b) {
  vector a_f = F.rowwise().sum();
  vector b_f = F.colwise().sum();
  
  // Rcpp::Rcout << a_f << "\n";
  // Rcpp::Rcout << b_f << "\n";
  // 
  vector x = mass_a.cwiseQuotient(a_f).cwiseMin(1.0);
  vector y = mass_b.cwiseQuotient(b_f).cwiseMin(1.0);
  
  matrix F_prime = x.asDiagonal() * F * y.asDiagonal();
  
  vector err_a = mass_a;
  err_a -= F_prime.rowwise().sum();
  vector err_b = mass_b;
  err_b -= F_prime.colwise().sum();
  
  // Rcpp::Rcout << F_prime.colwise().sum() <<"\n";
  // Rcpp::Rcout << (mass_b - F_prime.colwise().sum().transpose()) <<"\n";
  // // Rcpp::Rcout << err_a.lpNorm<1>() <<"\n";
  // if((F_prime.array() < 0).any()) Rcpp::stop("F' < 0");
  // if((err_a.array() < 0).any()) Rcpp::stop("erra' < 0");
  // if((err_b.array() < 0).any()) Rcpp::stop("errb' < 0");
  // 
  F.noalias() = F_prime;
  F.noalias() += err_a * err_b.transpose() / err_a.lpNorm<1>();
  // if((F.array() < 0).any()) Rcpp::stop("F < 0");
}

//[[Rcpp::export]]
matrix round_2_feasible_(matrix & F, const vector & mass_a, const vector & mass_b) {
  matrix F_safe = F;
  round_feasible(F_safe, mass_a, mass_b);
  return(F_safe);
}
