#include "trans_sinkhorn.h"

// algorithm 3 in Altschuler, J., Weed, J., & Rigollet, P. (2017). 
// Near-linear time approximation algorithms for optimal transport via Sinkhorn iteration. 
// 31st Conference on Neural Information Processing Systems, (1), 1–11. Long Beach, CA.
// void trans_sinkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
//                     const matrix & exp_cost, 
//                     matrix & A,
//                     double eta, double epsilon, int niterations) {
//   int N = mass_a.size();
//   int M = mass_b.size();
//   
//   
//   matrix A_0 = exp_cost;
//   A = exp_cost.array()/exp_cost.lpNorm<1>();
//   vector log_a = mass_a.array().log();
//   vector log_b = mass_b.array().log();
//   vector x = vector::Zero(N);
//   vector y = vector::Zero(M);
//   vector r = vector::Zero(N);
//   vector c = vector::Zero(M);
//   
//   for( int i = 0; i < niterations; i ++){
//     r = A.rowwise().sum();
//     c = A.colwise().sum();
//     
//     if ((i % 2) == 0) {
//       x.noalias() += log_a;
//       x.array() -= r.array().log();
//       
//     } else {
//       y.noalias() += log_b;
//       y.array() -= c.array().log();
//       
//     }
//     A = x.array().exp().matrix().asDiagonal() * A_0 * y.array().exp().matrix().asDiagonal();
//     // A /= A.sum();
//     if(dist_approx_ot(mass_a, mass_b, r, c, 2.0) <= epsilon) {
//       break;
//     }
//   }
// }


void trans_sinkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
                    const matrix & exp_cost, 
                    matrix & A,
                    double eta, double epsilon, int niterations) {
  // int N = mass_a.size();
  // int M = mass_b.size();
  
  
  // matrix A_0 = exp_cost;
  A = exp_cost.array()/exp_cost.lpNorm<1>();
  vector log_a = mass_a.array().log();
  vector log_b = mass_b.array().log();
  // vector x = vector::Zero(N);
  // vector y = vector::Zero(M);
  vector r = A.rowwise().sum();
  vector c = A.colwise().sum();
  
  for( int i = 0; i < niterations; i ++){

    if ((i % 2) == 0) {
      vector x = log_a;
      x.array() -= r.array().log();
      
      A = x.array().exp().matrix().asDiagonal() * A;
      r = mass_a;
      c = A.colwise().sum();
      // Rcpp::Rcout << "Even: ";
      // // Rcpp::Rcout << "x: " << x.transpose() << " ";
      // Rcpp::Rcout << "Rsum: " << A.rowwise().sum().sum() << " ";
      // Rcpp::Rcout << "Csum: " << A.colwise().sum().sum() << " ";
    } else {
      vector y = log_b;
      y.array() -= c.array().log();
      
      A = A * y.array().exp().matrix().asDiagonal();
      r = A.rowwise().sum();
      c = mass_b;
      // Rcpp::Rcout << "Odd: ";
      // // Rcpp::Rcout << "y: " << y.transpose() << " ";
      // Rcpp::Rcout << "Rsum: " << A.rowwise().sum().sum() << " ";
      // Rcpp::Rcout << "Csum: " << A.colwise().sum().sum() << " ";
    }
    // A /= A.sum();
    // r = A.rowwise().sum();
    // c = A.colwise().sum();
    // Rcpp::Rcout << "R: " << r.transpose() << " ";
    // Rcpp::Rcout << "Csum: " << A.colwise().sum().sum() << " ";
    // Rcpp::Rcout << "Asum " << A.sum() << "\n";
    if(dist_approx_ot(mass_a, mass_b, r, c, 1.0) <= epsilon) {
      // Rcpp::Rcout << epsilon << "\n";
      // Rcpp::Rcout << dist_approx_ot(mass_a, mass_b, r, c, 2.0) << "\n";
      break;
    }
  }
  A /= A.sum();
}
