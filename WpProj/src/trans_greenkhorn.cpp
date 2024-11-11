#include "trans_greenkhorn.h"

// double rho(double a, double b) {
//   return(b  - a + a * (std::log(a) - std::log(b)));
// }
// 
// vector rho_vec(vector & a, vector & b) {
//   return(b.array()  - a.array() + a.array() * (a.array().log() - b.array().log()));
// }

// int argmax_rho (const vector & a, const vector & b) {
//   vector r = rho_vec(a,b);
//   Eigen::Index maxIndex;
//   
//   r.maxCoeff(&maxIndex);
//   
//   int which_max = maxIndex;
//   return(which_max);
// }

int argmax_rho (const vector & r) {
  // vector r = rho_vec(a,b);
  Eigen::Index maxIndex;
  
  r.maxCoeff(&maxIndex);
  
  int which_max = maxIndex;
  return(which_max);
}

// Algorithm 4 of Altschuler, J., Weed, J., & Rigollet, P. (2017). Near-linear time approximation 
// algorithms for optimal transport via Sinkhorn iteration. 31st Conference on 
// Neural Information Processing Systems, (1), 1–11. Long Beach, CA.
// void trans_greenkhorn(const refVecConst & mass_a, const refVecConst & mass_b,
//                     const matrix & exp_cost,
//                     matrix & A,
//                     double eta, double epsilon, int niterations) {
//   int N = mass_a.size();
//   int M = mass_b.size();
// 
//   matrix A_0 = exp_cost.array()/exp_cost.lpNorm<1>();
//   A = A_0;
//   // matrix A_0 = exp_cost;
//   // A = exp_cost.array()/exp_cost.lpNorm<1>();
//   vector log_a = mass_a.array().log();
//   vector log_b = mass_b.array().log();
//   vector x = vector::Zero(N);
//   vector y = vector::Zero(M);
//   vector r = A.rowwise().sum();
//   vector c = A.colwise().sum();
//   vector rho_vals_r = rho_vec(mass_a, r);
//   vector rho_vals_c = rho_vec(mass_b, c);
// 
//   for( int i = 0; i < niterations; i ++){
//     int I = argmax_rho(rho_vals_r);
//     int J = argmax_rho(rho_vals_c);
// 
//     if (rho_vals_r(I) > rho_vals_c(J)) {
//       x(I) += log_a(I);
//       x(I) -= std::log(r(I));
//     } else {
//       y(J) += log_b(J);
//       y(J) -= std::log(c(J));
//     }
//     A = (x.array().exp()).matrix().asDiagonal() * A_0 * (y.array().exp()).matrix().asDiagonal();
//     // Rcpp::Rcout <<"dist: "<< dist_approx_ot(mass_a, mass_b, r, c, 2) << "\n";
//     // Rcpp::Rcout <<"A: " << A(0,0) << ", ";
//     // Rcpp::Rcout <<"A0: " << A_0(0,0) << ", ";
//     // Rcpp::Rcout <<"x: " << x(0) << ", ";
//     // Rcpp::Rcout <<"y: " << y(0) << "\n";
//     r = A.rowwise().sum();
//     c = A.colwise().sum();
//     if(dist_approx_ot(mass_a, mass_b, r, c, 2) <= epsilon) {
//       break;
//     }
//     
//     if (rho_vals_r(I) > rho_vals_c(J)) {
//       rho_vals_r(I) = 0.0;
//       rho_vals_c = rho_vec(mass_b, c);
//     } else {
//       rho_vals_r = rho_vec(mass_a, r);
//       rho_vals_c(J) = 0.0;
//     }
//   }
// }

// code adapted from https://github.com/JasonAltschuler/OptimalTransportNIPS17/blob/master/algorithms/greenkhorn.m
void trans_greenkhorn(const refVecConst & mass_a, const refVecConst & mass_b,
                      const matrix & exp_cost,
                      matrix & A,
                      double eta, double epsilon, int niterations) {
  int N = mass_a.size();
  int M = mass_b.size();

  A = exp_cost.array()/exp_cost.lpNorm<1>(); //can probably just use sum
  // A = A_0;
  // matrix A_0 = exp_cost;
  // vector log_a = mass_a.array().log(); //not working on log-scale but maybe should?
  // vector log_b = mass_b.array().log();
  vector r = A.rowwise().sum();
  vector c = A.colwise().sum();
  vector rho_vals_r = rho_vec(mass_a, r);
  vector rho_vals_c = rho_vec(mass_b, c);

  for( int i = 0; i < niterations; i ++){
    int I = argmax_rho(rho_vals_r);
    int J = argmax_rho(rho_vals_c);

    if (rho_vals_r(I) > rho_vals_c(J)) {
      // double x = log_a(I) - std::log(r(I));
      double scale_x = mass_a(I)/r(I);
      vector A_row = A.row(I);
      // vector A_new_row = std::exp(x) * A_row;
      vector A_new_row = scale_x * A_row;

      A.row(I) = A_new_row;
      r(I) = mass_a(I);
      c += A_new_row - A_row;
      rho_vals_r(I) = 0.0;
      rho_vals_c = rho_vec(mass_b, c);

    } else {
      // double y = log_b(J) - std::log(c(J));
      double scale_y = mass_b(J)/c(J);
      vector A_col = A.col(J);
      // vector A_col_new = std::exp(y) * A_col;
      vector A_col_new = scale_y * A_col;

      A.col(J) = A_col_new;
      c(J) = mass_b(J);
      r += A_col_new - A_col;
      rho_vals_r = rho_vec(mass_a, r);
      rho_vals_c(J) = 0.0;
    }
    // A = (x.array().exp()).matrix().asDiagonal() * A_0 * (y.array().exp()).matrix().asDiagonal();
    // Rcpp::Rcout <<"\ndist: "<< dist_approx_ot(mass_a, mass_b, r, c, 2) << ", ";
    // Rcpp::Rcout <<"A: " << A(0,0) << ", ";
    // // Rcpp::Rcout <<"A0: " << A_0(0,0) << ", ";
    // Rcpp::Rcout <<"r: " << r(0) << ", ";
    // Rcpp::Rcout <<"c: " << c(0) << "\n\n";
    // r = A.rowwise().sum();
    // c = A.colwise().sum();
    if(dist_approx_ot(mass_a, mass_b, r, c, 1.0) <= epsilon) {
      // Rcpp::Rcout << " " << dist_approx_ot(mass_a, mass_b, r, c, 2.0) << "\n";
      // Rcpp::Rcout << dist_approx_ot(mass_a, mass_b, r, c, 1.0) << "\n";
      // Rcpp::Rcout << epsilon << "\n";
      break;
    }
  }
  A /= A.sum();
}
