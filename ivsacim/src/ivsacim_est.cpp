# include <Rcpp.h>
using namespace Rcpp;
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' This is the main function to compute the estimator in C++
//' @param time censored event time
//' @param event event indicator
//' @param stime unique sorted noncensored event time
//' @param Zc centered IV
//' @param D_status treatment process at each noncensored event time
//' @param eps_1 influence function from modeling IV
//' @param Zc_dot bread matrix from modeling IV
//' @param weights optional weights
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP ivsacim_est(NumericVector time,
                 NumericVector event,
                 NumericVector stime, 
                 NumericVector Zc, 
                 NumericMatrix D_status,
                 NumericMatrix eps_1,
                 NumericMatrix Zc_dot,
                 NumericVector weights){
  
  // variables for estimation
  int n = time.size();
  int k = stime.size();
  NumericVector B_D(k), dB_D(k), b_numer_D(k), b_denom_D(k), risk_cumsum(k), indik_v(k);
  NumericMatrix dN(n, k), risk_t(n, k), B_intD(n, k + 1);
  double tmp_denom_D = 0, beta_D = 0, tot_risk = 0, tmp = 0, beta_D_se = 0, del = 0.01;
  NumericVector B_D_se(k), beta_D_IF(n);
  

  // variables for variance estimate
  int pdim = Zc_dot.ncol();
  // eps is the influence function of d\hat B_D(t_1), ..., d\hat B_D(t_M)
  NumericMatrix b_var_est(k, k), eps_tmp(n, k), eps(n, k), B_D_IF(k, n), b_D_dot(pdim, k);
  //H_dot is the derivative matrix of H against dB_D(t)
  NumericMatrix H_numer_dot(k, k), H_denom_dot(k, k), H_dot_Z(n, k), H_dot(k, k);

  for (int j = 0; j < k; j++) {

    for (int i = 0; i < n; i++) {
      // estimation
      dN(i, j) = (time[i] == stime[j]) ? 1:0;
      dN(i, j) *= event[i];
      risk_t(i, j) = (time[i] >= stime[j]) ? 1:0;
      risk_cumsum[j] += risk_t(i, j);
      b_numer_D[j] += weights[i] * Zc[i] * exp(B_intD(i, j)) * dN(i, j);
      b_denom_D[j] += weights[i] * Zc[i] * risk_t(i, j) * exp(B_intD(i, j)) * D_status(i, j);
    }


    tmp_denom_D = b_denom_D[j];
    tmp_denom_D = (tmp_denom_D < 0) ? -tmp_denom_D:tmp_denom_D;
    indik_v[j] = (tmp_denom_D < del) ? 0:1;
    if (indik_v[j]) {
      dB_D[j] = b_numer_D[j] / b_denom_D[j];
    }


    // estimation
    for (int i = 0; i < n; i++) {
      B_intD(i, j + 1) += D_status(i, j) * dB_D[j];
      B_intD(i, j + 1) += B_intD(i, j);
      eps_tmp(i, j) += weights[i] * Zc[i] * exp(B_intD(i, j)) * (dN(i, j) - risk_t(i, j) * D_status(i, j) * dB_D[j]);
    }


    // variance estimate
    if (indik_v[j]) {
      for (int i = 0; i < n; i++) {
        eps_tmp(i, j) /= b_denom_D[j];
      }
    }
    else{
      for (int i = 0; i < n; i++) {
        eps_tmp(i, j) = 0;
      }
    }


    // estimation part for beta
    beta_D += risk_cumsum[j] * dB_D[j];

    if (j == 0) {
      B_D[j] = 0 + dB_D[j];
      tot_risk += risk_cumsum[j] * (stime[j] - 0);
    }
    else{
      B_D[j] = B_D[j - 1] + dB_D[j];
      tot_risk += risk_cumsum[j] * (stime[j] - stime[j - 1]);
    }

  }

  // time invariant intensity
  beta_D /= tot_risk;

  // variance estimate
  for (int j = 0; j < k; j++) {
    for (int l = 0; l < j; l++) {
      for (int i = 0; i < n; i++) {
        H_numer_dot(l, j) += weights[i] * Zc[i] * exp(B_intD(i, j)) * dN(i, j) * D_status(i, l);
        H_denom_dot(l, j) += weights[i] * Zc[i] * risk_t(i, j) * exp(B_intD(i, j)) * D_status(i, j) * D_status(i, l);
      }
      if (indik_v[j]) {
        H_dot(l, j) = H_numer_dot(l, j) / b_denom_D[j] - H_denom_dot(l, j) * b_numer_D[j] / b_denom_D[j] / b_denom_D[j];
      }
    }
  }



  for (int j = 0; j < k; j++) {
    for (int j1 = 0; j1 < pdim; j1++){
      for (int l = 0; l < j; l++) {
        b_D_dot(j1, j) += H_dot(l, j) * b_D_dot(j1, l);
      }
      for (int i = 0; i < n; i++) {
        if (indik_v[j]) {
          H_dot_Z(i, j) += weights[i] * exp(B_intD(i, j)) * dN(i, j) / b_denom_D[j];
          H_dot_Z(i, j) -= weights[i] * risk_t(i, j) * exp(B_intD(i, j)) * D_status(i, j) * b_numer_D[j] / b_denom_D[j] / b_denom_D[j];
        }
        b_D_dot(j1, j) -= H_dot_Z(i, j) * Zc_dot(i, j1);
      }
    }
  }


  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      for (int l = 0; l < j; l++) {
        tmp -= H_dot(l, j) * eps(i, l);
      }
      eps(i, j) += eps_tmp(i, j) - tmp;
      tmp = 0;
    }
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      for (int j1 = 0; j1 < pdim; j1++) {
        eps(i, j) += eps_1(i, j1) * b_D_dot(j1, j);
      }
    }
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      beta_D_IF[i] += eps(i, j) * risk_cumsum[j];
    }
    beta_D_IF[i] /= tot_risk;
  }


  for (int j = 0; j < k; j++) {
    for (int l = 0; l < k; l++) {
      for (int i = 0; i < n; i++) {
        b_var_est(j, l) += eps(i, j) * eps(i, l);
      }
    }
  }



  for (int j = 0; j < k; j++) {
    for (int l = 0; l < j; l++) {
      B_D_se[j] += b_var_est(l, j);
    }
    for (int l = 0; l < j; l++) {
      B_D_se[j] += b_var_est(j, l);
    }
    B_D_se[j] += b_var_est(j, j);
    if (j > 0) {
      B_D_se[j] += B_D_se[j - 1];
    }
  }

  for (int j = 0; j < k; j++) {
    B_D_se[j] = sqrt(B_D_se[j]);
  }

  for (int i = 0; i < n; i++) {
    beta_D_se += beta_D_IF[i] * beta_D_IF[i];
  }


  beta_D_se = sqrt(beta_D_se);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      B_D_IF(j, i) = eps(i, j);
      if (j > 0) {
        B_D_IF(j, i) += B_D_IF(j - 1, i);
      }
    }
  }
  
  List by_prod = List::create(
    Named("Zc") = Zc,
    Named("dN") = dN,
    Named("risk_t") = risk_t,
    Named("indik_v") = indik_v,
    Named("b_numer_D") = b_numer_D,
    Named("b_denom_D") = b_denom_D,
    Named("B_intD") = B_intD,
    Named("eps_1") = eps_1,
    Named("eps") = eps,
    Named("B_D_IF") = B_D_IF,
    Named("H_dot") = H_dot,
    Named("b_D_dot") = b_D_dot,
    Named("b_var_est") = b_var_est,
    Named("beta_D_IF") = beta_D_IF);
  
  return List::create(
    Named("stime") = stime,
    Named("dB_D") = dB_D,
    Named("B_D") = B_D,
    Named("beta_D") = beta_D,
    Named("B_D_se") = B_D_se,
    Named("beta_D_se") = beta_D_se,
    Named("by_prod") = by_prod);
  
}


