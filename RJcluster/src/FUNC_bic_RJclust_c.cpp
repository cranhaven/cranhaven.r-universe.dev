#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// 
// // [[Rcpp::export]]
// Rcpp::List get_gamma_labels_c(const arma::mat z, const int C)
// {
//   int n = z.n_rows;
//   arma::vec labels = arma::zeros<arma::vec>(n);
//   Rcpp::List gamma(C);
//   
//   for (int i = 0; i < z.n_cols; i++)
//   {
//     arma::uvec temp = find(z.col(i) == 1);
//     
//     if (temp.n_elem > 0)
//     {
//       arma::vec fill = arma::vec(temp.n_elem);
//       fill.fill(i + 1.0);
//       labels(temp) = fill;
//       
//       for (int j = 0; j < temp.n_elem; j++)
//       {
//         temp[j] = temp[j] + 1;
//       }
//       gamma[i] = temp;
//     }
//   }
//   
//   return Rcpp::List::create(Rcpp::Named("labels") = labels, 
//                             Rcpp::Named("gamma") = gamma);
// }




