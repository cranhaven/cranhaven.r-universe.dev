// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
double get_cost_mean(List dat_smry, int s, int e) {
  arma::mat cumsum_x = dat_smry["cumsum_x"];  // p,n+1
  arma::cube cumsum_x2 = dat_smry["cumsum_x2"];  // p,p,n+1
  arma::mat Sigma = dat_smry["Sigma"];
  arma::mat Omega = dat_smry["Omega"];
  int p = cumsum_x.n_rows;
  int nse = e - s + 1;
  arma::vec temp = cumsum_x.col(e) - cumsum_x.col(s - 1);
  arma::mat Ase = (cumsum_x2.slice(e) - cumsum_x2.slice(s - 1)) - (temp * temp.t()) / nse;
  double out = nse * p * log(2 * arma::datum::pi) + nse * log(arma::det(Sigma)) + arma::trace(Omega * Ase);
  return(out);
}

// [[Rcpp::export]]
double get_cost_var(List dat_smry, int s, int e) {
  arma::mat cumsum_x = dat_smry["cumsum_x"];  // p,n+1
  arma::cube cumsum_x2 = dat_smry["cumsum_x2"];  // p,p,n+1
  arma::vec mu = dat_smry["mu"];
  int p = cumsum_x.n_rows;
  int nse = e - s + 1;
  arma::vec temp = cumsum_x.col(e) - cumsum_x.col(s - 1);
  arma::mat Ase = (cumsum_x2.slice(e) - cumsum_x2.slice(s - 1)) - mu * temp.t() - temp * mu.t() + nse * mu * mu.t();
  double out = nse * p * (log(2 * arma::datum::pi) + 1) + nse * log(arma::det(Ase) / nse);
  return(out);
}

// [[Rcpp::export]]
double get_cost_meanvar(List dat_smry, int s, int e) {
  arma::mat cumsum_x = dat_smry["cumsum_x"];  // p,n+1
  arma::cube cumsum_x2 = dat_smry["cumsum_x2"];  // p,p,n+1
  int p = cumsum_x.n_rows;
  int nse = e - s + 1;
  arma::vec temp = cumsum_x.col(e) - cumsum_x.col(s - 1);
  arma::mat Ase = (cumsum_x2.slice(e) - cumsum_x2.slice(s - 1)) - (temp * temp.t()) / nse;
  double out = nse * p * (log(2 * arma::datum::pi) + 1) + nse * log(arma::det(Ase) / nse);
  return(out);
}

// [[Rcpp::export]]
double get_cost_em(List dat_smry, int s, int e) {

  double out = 0;

  std::string em = dat_smry["em"];
  int nse = e - s + 1;

  if (em == "binom") {
    int N = dat_smry["N"];
    arma::vec cumsum_x = dat_smry["cumsum_x"];
    arma::vec cumsum_lh = dat_smry["cumsum_lh"];
    double sse = cumsum_x(e) - cumsum_x(s - 1);
    double mse = sse / (N * nse);
    double loglik = (cumsum_lh(e) - cumsum_lh(s - 1)) + sse * log(mse / (1 - mse)) + nse * N * log(1 - mse);
    out = -2 * loglik;
  }
  if (em == "multinom") {
    int N = dat_smry["N"];
    arma::mat cumsum_x = dat_smry["cumsum_x"];
    arma::vec cumsum_lh = dat_smry["cumsum_lh"];
    arma::rowvec sse = cumsum_x.row(e) - cumsum_x.row(s - 1);
    arma::rowvec mse = sse / (N * nse);
    double loglik = (cumsum_lh(e) - cumsum_lh(s - 1)) + arma::as_scalar(sse * arma::log(mse.t()));
    out = -2 * loglik;
  }
  if (em == "pois") {
    arma::vec cumsum_x = dat_smry["cumsum_x"];
    arma::vec cumsum_lh = dat_smry["cumsum_lh"];
    double sse = cumsum_x(e) - cumsum_x(s - 1);
    double mse = sse / nse;
    double loglik = (cumsum_lh(e) - cumsum_lh(s - 1)) + sse * log(mse) - sse;
    out = -2 * loglik;
  }
  if (em == "exp") {
    arma::vec cumsum_x = dat_smry["cumsum_x"];
    double sse = cumsum_x(e) - cumsum_x(s - 1);
    double lambda = nse / sse;
    double loglik = nse * (1 + log(lambda)) - sse * lambda;
    out = -2 * loglik;
  }
  if (em == "geom") {
    arma::vec cumsum_x = dat_smry["cumsum_x"];
    double sse = cumsum_x(e) - cumsum_x(s - 1);
    double geom_p = nse / sse;
    double loglik = sse * log(1 - geom_p) + nse * log(geom_p / (1 - geom_p));
    out = -2 * loglik;
  }
  return(out);
}

typedef double (*funcPtr)(List dat_smry, int s, int e);
XPtr<funcPtr> putFunPtrInXPtr(std::string model) {
  if (model == "mean") {
    return(XPtr<funcPtr>(new funcPtr(get_cost_mean)));
  } else if (model == "var") {
    return(XPtr<funcPtr>(new funcPtr(get_cost_var)));
  } else if (model == "meanvar") {
    return(XPtr<funcPtr>(new funcPtr(get_cost_meanvar)));
  } else if (model == "em") {
    return(XPtr<funcPtr>(new funcPtr(get_cost_em)));
  } else {
    return(XPtr<funcPtr>(R_NilValue));
  }
}

// [[Rcpp::export]]
List SN(List dat_smry, int n, int L, int d, std::string model) {

  List out;

  XPtr<funcPtr> xpfun = putFunPtrInXPtr(model);
  funcPtr get_cost = *xpfun;

  int Q = L + 1;

  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);

  for (int i = 1; i <= (n - d + 1); i++) {
    for (int j = i; j <= n; j++) {
      checkUserInterrupt();
      if (j >= (i + d - 1)) {
        all_seg(i, j) = get_cost(dat_smry, i, j);
      }
    }
  }

  arma::mat loss(Q + 1, n + 1); loss.fill(NA_REAL);
  loss.row(1) = all_seg.row(1);
  arma::mat V(Q + 1, n + 1); V.fill(NA_REAL);

  for (int q = 2; q <= Q; q++) {
    for (int j = q * d; j <= n; j++) {
      arma::vec loss_temp(j - q * d + 1); loss_temp.fill(NA_REAL);
      for (int v = (q - 1) * d; v <= (j - d); v++) {
        loss_temp(v - (q - 1) * d) = loss(q - 1, v) + all_seg(v + 1, j);
      }
      int v_min = loss_temp.index_min();
      loss(q, j) = loss_temp(v_min);
      V(q, j) = v_min + (q - 1) * d;
    }
  }

  arma::mat cps(Q + 1, L + 1); cps.fill(NA_REAL);
  cps.col(1) = V.col(n);
  if (Q >= 3) {
    for (int q = 3; q <= Q; q++) {
      for (int i = 2; i <= q - 1; i++) {
        cps(q, i) = V(q - (i - 1), cps(q, i - 1));
      }
    }
  }

  arma::mat cpt_cand = cps(arma::span(2, Q), arma::span(1, L));
  arma::mat temp = cpt_cand;
  int n_cpt_cand = cpt_cand.n_rows;
  for (int i = 0; i < n_cpt_cand; i++) {
    for (int j = 0; j <= i; j++) {
      cpt_cand(i, j) = temp(i, i - j);
    }
  }

  out["cpt_cand"] = cpt_cand;
  return (out);
}

// [[Rcpp::export]]
List BS(List dat_smry, int n, int L, int d, std::string model) {

  List out;

  XPtr<funcPtr> xpfun = putFunPtrInXPtr(model);
  funcPtr get_cost = *xpfun;

  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);
  all_seg(1, n) = get_cost(dat_smry, 1, n);
  arma::vec tau_hat_all(L + 2); tau_hat_all.fill(NA_REAL);
  tau_hat_all(0) = 0;  // -1
  for (int k = 1; k <= L; k++) {
    tau_hat_all(k) = n;
    arma::vec cost_cmp(k); cost_cmp.fill(NA_REAL);
    arma::vec cps_cmp(k); cps_cmp.fill(NA_REAL);
    for (int j = 0; j < k; j++) {
      arma::vec tau_hat_all_sort = arma::sort(tau_hat_all(arma::span(0, k)));
      int l = tau_hat_all_sort(j);
      int r = tau_hat_all_sort(j + 1);
      if (r - l >= 2 * d) {
        arma::vec t_lr(r - l); t_lr.fill(NA_REAL);
        arma::vec temp(r - l); temp.fill(NA_REAL);
        int count = 0;
        for (int t = l; t <= r; t++) {
          if ((t > l) & (t < r) & (std::min(t - l, r - t) >= d)) {
            count += 1;
            t_lr(count) = t;
            if (!arma::is_finite(all_seg(l + 1, t))) {
              all_seg(l + 1, t) = get_cost(dat_smry, l + 1, t);
            }
            if (!arma::is_finite(all_seg(t + 1, r))) {
              all_seg(t + 1, r) = get_cost(dat_smry, t + 1, r);
            }
            temp(count) = all_seg(l + 1, r) - (all_seg(l + 1, t) + all_seg(t + 1, r));
          }
        }
        arma::uword temp_which_max = temp(arma::span(0, count)).index_max();
        cost_cmp(j) = temp(temp_which_max);
        cps_cmp(j) = t_lr(temp_which_max);
      }
    }
    tau_hat_all(k) = cps_cmp(cost_cmp.index_max());
    if (!arma::is_finite(tau_hat_all(k))) {
      break;
    }
  }
  arma::vec tau_hat_finite = tau_hat_all(arma::find_finite(tau_hat_all));
  arma::vec tau_hat = tau_hat_finite(arma::find((tau_hat_finite > 0) && (tau_hat_finite < n)));

  int n_tau_hat = tau_hat.n_rows;
  arma::mat tau_cand(n_tau_hat, n_tau_hat); tau_cand.fill(NA_REAL);
  for (int i = 0; i < n_tau_hat; i++) {
    arma::vec tau_hat_i = tau_hat(arma::span(0, i));
    tau_cand(i, arma::span(0, i)) = arma::sort(tau_hat_i.t());
  }

  out["cpt_cand"] = tau_cand;
  out["cpt_cand_temp"] = tau_hat;
  return (out);
}

// [[Rcpp::export]]
List WBS(List dat_smry, int n, int L, int d, arma::mat lr_M, std::string model) {

  List out;

  XPtr<funcPtr> xpfun = putFunPtrInXPtr(model);
  funcPtr get_cost = *xpfun;

  int M = lr_M.n_rows;
  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);
  for (int i = 0; i < M; i++) {
    all_seg(lr_M(i, 0) + 1, lr_M(i, 1)) = get_cost(dat_smry, lr_M(i, 0) + 1, lr_M(i, 1));
  }
  arma::vec tau_hat_all(L + 2); tau_hat_all.fill(NA_REAL);
  tau_hat_all(0) = 0;
  for (int k = 1; k <= L; k++) {
    tau_hat_all(k) = n;
    arma::mat cost_cmp(k, M + 1); cost_cmp.fill(NA_REAL);
    arma::mat cps_cmp(k, M + 1); cps_cmp.fill(NA_REAL);
    for (int j = 0; j < k; j++) {
      arma::vec tau_hat_all_sort = arma::sort(tau_hat_all(arma::span(0, k)));
      int l = tau_hat_all_sort(j);
      int r = tau_hat_all_sort(j + 1);
      for (int i = 0; i <= M; i++) {
        checkUserInterrupt();
        int li, ri;
        if (i == M) {
          li = l;
          ri = r;
        } else {
          li = lr_M(i, 0);
          ri = lr_M(i, 1);
        }
        if ((l <= li) & (ri <= r)) {
          if (ri - li >= 2 * d) {
            arma::vec t_lr(ri - li); t_lr.fill(NA_REAL);
            arma::vec temp(ri - li); temp.fill(NA_REAL);
            int count = 0;
            if (!arma::is_finite(all_seg(li + 1, ri))) {
              all_seg(li + 1, ri) = get_cost(dat_smry, li + 1, ri);
            }
            for (int t = li; t <= ri; t++) {
              if ((t > li) & (t < ri) & (std::min(t - li, ri - t) >= d)) {
                count += 1;
                t_lr(count) = t;
                if (!arma::is_finite(all_seg(li + 1, t))) {
                  all_seg(li + 1, t) = get_cost(dat_smry, li + 1, t);
                }
                if (!arma::is_finite(all_seg(t + 1, ri))) {
                  all_seg(t + 1, ri) = get_cost(dat_smry, t + 1, ri);
                }
                temp(count) = all_seg(li + 1, ri) - (all_seg(li + 1, t) + all_seg(t + 1, ri));
              }
            }
            arma::uword temp_which_max = temp(arma::span(0, count)).index_max();
            cost_cmp(j, i) = temp(temp_which_max);
            cps_cmp(j, i) = t_lr(temp_which_max);
          }
        }
      }
    }
    arma::uword idx = cost_cmp.index_max();
    tau_hat_all(k) = cps_cmp(idx);
    if (!arma::is_finite(tau_hat_all(k))) {
      break;
    }
  }
  arma::vec tau_hat_finite = tau_hat_all(arma::find_finite(tau_hat_all));
  arma::vec tau_hat = tau_hat_finite(arma::find((tau_hat_finite > 0) && (tau_hat_finite < n)));

  int n_tau_hat = tau_hat.n_rows;
  arma::mat tau_cand(n_tau_hat, n_tau_hat); tau_cand.fill(NA_REAL);
  for (int i = 0; i < n_tau_hat; i++) {
    arma::vec tau_hat_i = tau_hat(arma::span(0, i));
    tau_cand(i, arma::span(0, i)) = arma::sort(tau_hat_i.t());
  }

  out["cpt_cand"] = tau_cand;
  out["cpt_cand_temp"] = tau_hat;
  return (out);
}

// [[Rcpp::export]]
List PELT(List dat_smry, int n, arma::vec pen_val, int d, double K, std::string model) {

  List out;

  XPtr<funcPtr> xpfun = putFunPtrInXPtr(model);
  funcPtr get_cost = *xpfun;

  int n_pen_val = pen_val.n_elem;

  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);
  arma::mat F_cost(n + 2, n_pen_val); F_cost.fill(NA_REAL);
  F_cost.row(1) = -pen_val.t();
  for (int ts = d; ts < 2 * d; ts++) {
    all_seg(1, ts) = get_cost(dat_smry, 1, ts);
    for (int b = 0; b < n_pen_val; b++) {
      F_cost(ts + 1, b) = all_seg(1, ts);
    }
  }
  arma::mat cp(n + 1, n_pen_val); cp.fill(0);
  arma::mat cps_final(n, n_pen_val); cps_final.fill(0);
  for (int b = 0; b < n_pen_val; b++) {
    arma::mat R(n + 1, n + 2); R.fill(NA_REAL);
    R(1, 2 * d) = 0;
    R(2, 2 * d) = d;
    for (int ts = 2 * d; ts <= n; ts++) {
      arma::vec R_ts = R.col(ts);
      arma::uvec idx_finite = arma::find_finite(R_ts);
      arma::vec R_finite = R_ts(idx_finite);
      int n_R_finite = R_finite.n_elem;
      arma::vec temp(n_R_finite); temp.fill(NA_REAL);
      for (int i = 0; i < n_R_finite; i++) {
        if (!arma::is_finite(all_seg(R_finite(i) + 1, ts))) {
          all_seg(R_finite(i) + 1, ts) = get_cost(dat_smry, R_finite(i) + 1, ts);
        }
        temp(i) = F_cost(R_finite(i) + 1, b) + all_seg(R_finite(i) + 1, ts) + pen_val(b);
      }
      arma::uword idx = temp.index_min();
      F_cost(ts + 1, b) = temp(idx);
      cp(ts, b) = R_finite(idx);
      arma::uvec idx_prune = arma::find(temp + K < F_cost(ts + 1, b) + pen_val(b));
      arma::vec R_prune = R_finite(idx_prune);
      R(arma::span(1, R_prune.n_elem), ts + 1) = R_prune;
      R(R_prune.n_elem + 1, ts + 1) = ts - (d - 1);
    }
    int cp0 = cp(n, b);
    int count = 0;
    while (cp0 > 0) {
      cps_final(count, b) = cp0;
      cp0 = cp(cp0, b);
      count += 1;
    }
  }

  out["cpt_cand"] = cps_final;
  return (out);
}
