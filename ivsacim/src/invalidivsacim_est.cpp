// [[Rcpp::depends(RcppArmadillo)]]
# include <RcppArmadillo.h>
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
//' @param Z the instrument
//' @param Z_c centered IV
//' @param D_status treatment process at each noncensored event time
//' @param D_status_c centered treatment process at each noncensored event time
//' @param Z_model_mat bread matrix from modeling IV
//' @param eps_1 influence function from modeling IV
//' @param D_model_mat bread matrix from modeling treatment
//' @param eps_2 influence function from modeling treatment
//' @param weights optional weights
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP invalidivsacim_est(NumericVector time,
                        NumericVector event,
                        NumericVector stime, 
                        NumericVector Z,
                        NumericVector Z_c, 
                        NumericMatrix D_status,
                        NumericMatrix D_status_c,
                        NumericMatrix Z_model_mat,
                        NumericMatrix eps_1,
                        NumericMatrix D_model_mat,
                        NumericMatrix eps_2,
                        NumericVector weights){
  
  // variables for estimation
  int N = time.size();
  int K = stime.size();
  NumericVector B_D(K), dB_D(K), b_numer_D(2 * K), b_denom_D(4 * K), B_Z(K), dB_Z(K), risk_cumsum(K);
  NumericMatrix dN(N, K),  risk_t(N, K), B_intD(N, K + 1), B_intZ(N, K + 1), tmp_mat(2, 2);
  double beta_D = 0.0, beta_Z = 0.0, tot_risk = 0.0;
  double del = 0.001;
  
  // variables for variance estimate
  int Z_model_dim = Z_model_mat.cols(), D_model_dim = D_model_mat.length() / N / K; 
  NumericVector B_D_se(K), B_Z_se(K), beta_D_IF(N), beta_Z_IF(N);
  NumericMatrix res(2 * K, N), bread(2 * K, 2 * K), IF(2 * K, N), B_D_IF(K, N), B_Z_IF(K, N), res_dot_Z(2 * K, Z_model_dim), res_dot_D(2 * K, D_model_dim);
  double beta_D_se = 0.0, beta_Z_se = 0.0;
  
  
  for (int j = 0; j < K; j++) {
    
    for (int i = 0; i < N; i++) {
      // estimation
      dN(i, j) = (time[i] == stime[j]) ? 1:0;
      dN(i, j) *= event[i];
      risk_t(i, j) = (time[i] >= stime[j]) ? 1:0;
      risk_cumsum[j] += risk_t(i, j);
      b_numer_D[2 * j + 0] += weights[i] * Z_c[i] * exp(B_intD(i, j) + B_intZ(i, j)) * dN(i, j);
      b_numer_D[2 * j + 1] += weights[i] * Z_c[i] * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * dN(i, j);
      b_denom_D[4 * j + 0] += weights[i] * Z_c[i] * risk_t(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * D_status(i, j);
      b_denom_D[4 * j + 1] += weights[i] * Z_c[i] * D_status_c(i, j) * risk_t(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * D_status(i, j);
      b_denom_D[4 * j + 2] += weights[i] * Z_c[i] * risk_t(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * Z[i];
      b_denom_D[4 * j + 3] += weights[i] * Z_c[i] * D_status_c(i, j) * risk_t(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * Z[i];
    }
    
    
    
    tmp_mat(0, 0) = b_denom_D[4 * j + 0];
    tmp_mat(1, 0) = b_denom_D[4 * j + 1];
    tmp_mat(0, 1) = b_denom_D[4 * j + 2];
    tmp_mat(1, 1) = b_denom_D[4 * j + 3];
    // solve linear system using RcppArmadillo
    arma::mat X(tmp_mat.begin(), 2, 2, false);
    arma::mat tmp_pinv = arma::pinv(X, del);
    dB_D[j] = b_numer_D[2 * j + 0] * tmp_pinv(0, 0) + b_numer_D[2 * j + 1] * tmp_pinv(0, 1);
    dB_Z[j] = b_numer_D[2 * j + 0] * tmp_pinv(1, 0) + b_numer_D[2 * j + 1] * tmp_pinv(1, 1);

    
    // estimation
    for (int i = 0; i < N; i++) {
      B_intD(i, j + 1) += D_status(i, j) * dB_D[j];
      B_intD(i, j + 1) += B_intD(i, j);
      B_intZ(i, j + 1) += Z[i] * dB_Z[j];
      B_intZ(i, j + 1) += B_intZ(i, j);
      res(2 * j + 0, i) += weights[i] * Z_c[i] * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) / N;
      res(2 * j + 1, i) += weights[i] * Z_c[i] * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) / N;
    }
    
    
    
    // estimation part for beta_D
    beta_D += risk_cumsum[j] * dB_D[j];
    beta_Z += risk_cumsum[j] * dB_Z[j];
    
    if (j == 0) {
      B_D[j] = 0 + dB_D[j];
      B_Z[j] = 0 + dB_Z[j];
      tot_risk += risk_cumsum[j] * (stime[j] - 0);
    }
    else{
      B_D[j] = B_D[j - 1] + dB_D[j];
      B_Z[j] = B_Z[j - 1] + dB_Z[j];
      tot_risk += risk_cumsum[j] * (stime[j] - stime[j - 1]);
    }
  }
  
  // time invariant intensity
  beta_D /= tot_risk;
  beta_Z /= tot_risk;
  
  // variance estimate
  for (int j = 0; j < K; j++) {
    for (int l = 0; l < j; l++) {
      for (int i = 0; i < N; i++) {
        bread(2 * j + 0, 2 * l + 0) += weights[i] * Z_c[i] * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) * D_status(i, l) / N;
        bread(2 * j + 0, 2 * l + 1) += weights[i] * Z_c[i] * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) * Z[i] / N;
        bread(2 * j + 1, 2 * l + 0) += weights[i] * Z_c[i] * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) * D_status(i, l) / N;
        bread(2 * j + 1, 2 * l + 1) += weights[i] * Z_c[i] * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) * Z[i] / N;
      }
    }
  }
  
  
  for (int j = 0; j < K; j++) {
    for (int i = 0; i < N; i++) {
      bread(2 * j + 0, 2 * j + 0) += weights[i] * Z_c[i] * exp(B_intD(i, j) + B_intZ(i, j)) * -risk_t(i, j) * D_status(i, j) / N;
      bread(2 * j + 0, 2 * j + 1) += weights[i] * Z_c[i] * exp(B_intD(i, j) + B_intZ(i, j)) * -risk_t(i, j) * Z[i] / N;
      bread(2 * j + 1, 2 * j + 0) += weights[i] * Z_c[i] * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * -risk_t(i, j) * D_status(i, j) / N;
      bread(2 * j + 1, 2 * j + 1) += weights[i] * Z_c[i] * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * -risk_t(i, j) * Z[i] / N;
    }
  }
  
  
  // compute derivative matrix of the estimating equation against first step estimators
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < K; j++) {
      for (int j1 = 0; j1 < Z_model_dim; j1++) {
        res_dot_Z(2 * j + 0, j1) += weights[i] * -Z_model_mat(i, j1) * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) / N;
        res_dot_Z(2 * j + 1, j1) += weights[i] * -Z_model_mat(i, j1) * D_status_c(i, j) * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) / N;
      }
      for (int j2 = 0; j2 < D_model_dim; j2++) {
        res_dot_D(2 * j + 0, j2) += weights[i] * 0;
        res_dot_D(2 * j + 1, j2) += weights[i] * Z_c[i] * -D_model_mat(i, j * D_model_dim + j2) * exp(B_intD(i, j) + B_intZ(i, j)) * (dN(i, j) - risk_t(i, j) * (D_status(i, j) * dB_D[j] + Z[i] * dB_Z[j])) / N;
      }
    }
  }
  
  // stacking residuals
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < 2 * K; j++) {
      for (int j1 = 0; j1 < Z_model_dim; j1++) {
        res(j, i) += res_dot_Z(j, j1) * eps_1(j1, i);
      }
    }
  }
  
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < K; j++) {
      for (int j2 = 0; j2 < D_model_dim; j2++) {
        res(2 * j + 0, i) += res_dot_D(2 * j + 0, j2) * eps_2(2 * j + j2, i);
        res(2 * j + 1, i) += res_dot_D(2 * j + 1, j2) * eps_2(2 * j + j2, i);
      }
    }
  }
  
  
  // solve linear system using RcppArmadillo
  arma::mat X(bread.begin(), 2 * K, 2 * K, false);
  arma::mat X_pinv = arma::pinv(-X, del);
  
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < 2 * K; j++) {
      for (int k = 0; k < 2 * K; k++) {
        IF(j, i) += X_pinv(j, k) * res(k, i);
      }
    }
  }
  
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < K; j++) {
      B_D_IF(j, i) = IF(2 * j + 0, i);
      B_Z_IF(j, i) = IF(2 * j + 1, i);
      if (j > 0) {
        B_D_IF(j, i) += B_D_IF(j - 1, i);
        B_Z_IF(j, i) += B_Z_IF(j - 1, i);
      }
      beta_D_IF[i] += IF(2 * j + 0, i) * risk_cumsum[j] / tot_risk;
      beta_Z_IF[i] += IF(2 * j + 1, i) * risk_cumsum[j] / tot_risk;
    }
  }
  
  for (int j = 0; j < K; j++) {
    for (int i = 0; i < N; i++) {
      B_D_se[j] += B_D_IF(j, i) * B_D_IF(j, i);
      B_Z_se[j] += B_Z_IF(j, i) * B_Z_IF(j, i);
    }
    B_D_se[j] = sqrt(B_D_se[j]);
    B_Z_se[j] = sqrt(B_Z_se[j]);
  }
  
  for (int i = 0; i < N; i++) {
    beta_D_se += beta_D_IF[i] * beta_D_IF[i];
    beta_Z_se += beta_Z_IF[i] * beta_Z_IF[i];
  }
  beta_D_se = sqrt(beta_D_se);
  beta_Z_se = sqrt(beta_Z_se);
  
  List by_prod = List::create(
    Named("dN") = dN,
    Named("risk_t") = risk_t,
    Named("b_numer_D") = b_numer_D,
    Named("b_denom_D") = b_denom_D,
    Named("B_intD") = B_intD,
    Named("Z_model_dim") = Z_model_dim,
    Named("D_model_dim") = D_model_dim,
    Named("bread") = bread,
    Named("B_D_IF") = B_D_IF,
    Named("B_Z_IF") = B_Z_IF,
    Named("beta_D_IF") = beta_D_IF,
    Named("beta_Z_IF") = beta_Z_IF);
  
  return List::create(
    Named("stime") = stime,
    Named("dB_D") = dB_D,
    Named("B_D") = B_D,
    Named("B_D_se") = B_D_se,
    Named("dB_Z") = dB_Z,
    Named("B_Z") = B_Z,
    Named("B_Z_se") = B_Z_se,
    Named("beta_D") = beta_D,
    Named("beta_Z") = beta_Z,
    Named("beta_D_se") = beta_D_se,
    Named("beta_Z_se") = beta_Z_se,
    Named("by_prod") = by_prod);
}
