// Thiago de Paula Oliveira
#include <RcppArmadillo.h>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
List cccUst_rcpp(NumericVector y_vec,
                 IntegerVector met_vec,
                 IntegerVector time_vec,
                 int nmet0,
                 int nmet1,
                 int ntime,
                 int ns,
                 NumericMatrix Dmat,
                 double delta,
                 double cl) {

  arma::mat D = Rcpp::as<arma::mat>(Dmat);  // Convert once

  // Reshape into ns × ntime matrices
  arma::mat X(ns, ntime), Y(ns, ntime);
  int idx0 = 0, idx1 = 0;
  for (int i = 0; i < y_vec.size(); ++i) {
    int mi = met_vec[i];
    int ti = time_vec[i];
    if (mi == nmet0)
      X(idx0, ti) = y_vec[i];
    else if (mi == nmet1)
      Y(idx1, ti) = y_vec[i];

    if (ti == ntime - 1) {
      if (mi == nmet0) ++idx0;
      else ++idx1;
    }
  }

  arma::vec phi1_rows(ns * (ns - 1));
  arma::vec phi2_rows(ns * (ns - 1));
  double U = 0.0, V = 0.0;

  // ===== OpenMP parallel loop =====
#ifdef _OPENMP
#pragma omp parallel for reduction(+:U,V)
#endif
  for (int i = 0; i < ns; ++i) {
    for (int j = 0; j < ns; ++j) {
      if (i == j) continue;

      int idx = i * (ns - 1) + (j < i ? j : j - 1); // unique index for (i,j)

      arma::rowvec Xi = X.row(i), Yi = Y.row(i);
      arma::rowvec Xj = X.row(j), Yj = Y.row(j);

      arma::rowvec d1 = arma::abs(Xi - Yi);
      arma::rowvec d2 = arma::abs(Xj - Yj);
      arma::rowvec d3 = arma::abs(Xi - Yj);
      arma::rowvec d4 = arma::abs(Xj - Yi);

      double phi1, phi2;

      if (delta != 0.0) {
        arma::rowvec v1 = arma::pow(d1, delta);
        arma::rowvec v2 = arma::pow(d2, delta);
        arma::rowvec v3 = arma::pow(d3, delta);
        arma::rowvec v4 = arma::pow(d4, delta);

        phi1 = 0.5 * (arma::as_scalar(v1 * D * v1.t()) +
          arma::as_scalar(v2 * D * v2.t()));
        phi2 = 0.5 * (arma::as_scalar(v3 * D * v3.t()) +
          arma::as_scalar(v4 * D * v4.t()));
      } else {
        arma::rowvec nz1 = arma::conv_to<arma::rowvec>::from(d1 != 0);
        arma::rowvec nz2 = arma::conv_to<arma::rowvec>::from(d2 != 0);
        arma::rowvec nz3 = arma::conv_to<arma::rowvec>::from(d3 != 0);
        arma::rowvec nz4 = arma::conv_to<arma::rowvec>::from(d4 != 0);

        phi1 = 0.5 * (arma::as_scalar(nz1 * D * nz1.t()) +
          arma::as_scalar(nz2 * D * nz2.t()));
        phi2 = 0.5 * (arma::as_scalar(nz3 * D * nz3.t()) +
          arma::as_scalar(nz4 * D * nz4.t()));
      }

      phi1_rows(idx) = phi1;
      phi2_rows(idx) = phi2;
      U += phi1;
      V += phi2;
    }
  }

  // Mean U and V
  double denom = ns * (ns - 1);
  U /= denom;
  V /= denom;

  double CCC = ((ns - 1) * (V - U)) / (U + (ns - 1) * V);

  // ===== Variance & CI =====
  arma::vec phi1_sum(ns, arma::fill::zeros);
  arma::vec phi2_sum(ns, arma::fill::zeros);

  for (int i = 0; i < ns; ++i) {
    for (int j = 0; j < ns; ++j) {
      if (i == j) continue;
      int idx = i * (ns - 1) + (j < i ? j : j - 1);
      phi1_sum[i] += phi1_rows[idx];
      phi2_sum[i] += phi2_rows[idx];
    }
  }

  phi1_sum /= (ns - 1);
  phi2_sum /= (ns - 1);

  arma::mat phiv(ns, 2);
  phiv.col(0) = phi1_sum;
  phiv.col(1) = phi2_sum;

  arma::rowvec UV = { U, V };
  arma::mat Saux(2, 2, arma::fill::zeros);
  for (int i = 0; i < ns; ++i) {
    arma::rowvec diff = phiv.row(i) - UV;
    Saux += diff.t() * diff;
  }

  arma::mat C; C.eye(2, 2); C(1, 1) = 2;
  arma::mat Smat = C * (Saux / (ns * ns)) * C;

  arma::rowvec dev(2);
  dev[0] = (-ns * (ns - 1) * V) / std::pow(U + (ns - 1) * V, 2);
  dev[1] = (ns * (ns - 1) * U) / std::pow(U + (ns - 1) * V, 2);

  double VarCCC = arma::as_scalar(dev * Smat * dev.t());

  // Fisher Z-based confidence interval
  double alpha = 1.0 - cl;
  double z = 0.5 * std::log((1 + CCC) / (1 - CCC));
  double VarZ = VarCCC / std::pow(1 - CCC * CCC, 2);
  double seZ = std::sqrt(VarZ);

  double zcrit = R::qnorm(1.0 - alpha / 2.0, 0.0, 1.0, 1, 0);
  double z_low = z - zcrit * seZ;
  double z_upp = z + zcrit * seZ;

  double ci_low = (std::exp(2 * z_low) - 1) / (std::exp(2 * z_low) + 1);
  double ci_upp = (std::exp(2 * z_upp) - 1) / (std::exp(2 * z_upp) + 1);

  return List::create(
    Named("CCC")     = CCC,
    Named("LL_CI")   = ci_low,
    Named("UL_CI")   = ci_upp,
    Named("SE_CCC")  = std::sqrt(VarCCC),
    Named("Z")       = z,
    Named("SE_Z")    = seZ
  );
}

// [[Rcpp::export]]
void set_omp_threads(const int n) {
#ifdef _OPENMP
  omp_set_num_threads(n);
#endif
}

// [[Rcpp::export]]
int get_omp_threads() {
#ifdef _OPENMP
  return omp_get_max_threads();
#else
  return 1;
#endif
}

