#include <Rcpp.h>
#include "pdist_rcpp.h" 
using namespace Rcpp;

// pdist function defined earlier

// [[Rcpp::export]]
List pnorm_rcpp(NumericVector pobs_vec,
                        List Vobs,
                        NumericVector Vobs_L,
                        NumericVector Vobs_R,
                        NumericMatrix cur_par_X,
                        std::string dist_X,
                        bool collapsed_g) {
  
  int n = Vobs.size();
  NumericVector m(n);
  for(int i = 0; i < n; ++i) {
    NumericVector temp = Vobs[i];
    m[i] = temp.size() - 1;
  }
  
  // Calculate 'par'
  int total_rows = sum(m);
  NumericMatrix par(total_rows, 2);
  int row_idx = 0;
  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < m[i]; ++j) {
      par(row_idx, 0) = cur_par_X(i, 0);
      par(row_idx, 1) = cur_par_X(i, 1);
      ++row_idx;
    }
  }
  
  // Calculate Fxl, Fxr, and pobs_
  NumericVector Fxl = pdist_rcpp(Vobs_L, par, dist_X);
  NumericVector Fxr = pdist_rcpp(Vobs_R, par, dist_X);
  NumericVector pobs_ = (Fxr - Fxl) * pobs_vec;
  
  // Split pobs_
  List pobs_split(n);
  int start_idx = 0;
  for(int i = 0; i < n; ++i) {
    NumericVector temp(m[i]);
    std::copy(pobs_.begin() + start_idx, pobs_.begin() + start_idx + (int)m[i], temp.begin());
    
    pobs_split[i] = temp;
    start_idx += m[i];
  }
  
  // Calculate pobs_norm
  List pobs_norm(n);
  NumericVector sums_;
  if (collapsed_g) {
    sums_ = NumericVector(n); // Initialize sums_ only if collapsed_g is TRUE
  }
  for(int i = 0; i < n; ++i) {
    NumericVector temp = pobs_split[i];
    double sum_temp = sum(temp);
    if (collapsed_g) {
      sums_[i] = sum_temp; // Store the sum in sums_ if collapsed_g is TRUE
    }
    pobs_norm[i] = temp / sum_temp;
  }
  
  if (collapsed_g) {
    // Return pobs_norm along with sums_ if collapsed_g is TRUE
    return List::create(_["pobs_norm"] = pobs_norm, _["sums_"] = sums_);
  } else {
    // Return pobs_norm only if collapsed_g is FALSE
    return List::create(_["pobs_norm"] = pobs_norm);
  }
}
