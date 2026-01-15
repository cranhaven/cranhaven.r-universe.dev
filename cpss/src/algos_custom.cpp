// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
List SN_custom(List dat_smry, int n, int L, int d, Function get_cost) {

  List out;

  int Q = L + 1;

  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);

  for (int i = 1; i <= (n - d + 1); i++) {
    for (int j = i; j <= n; j++) {
      checkUserInterrupt();
      if (j >= (i + d - 1)) {
        all_seg(i, j) = as<double>(get_cost(dat_smry, i, j));
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
List BS_custom(List dat_smry, int n, int L, int d, Function get_cost) {

  List out;

  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);
  all_seg(1, n) = as<double>(get_cost(dat_smry, 1, n));
  arma::vec tau_hat_all(L + 2); tau_hat_all.fill(NA_REAL);
  tau_hat_all(0) = 0;
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
              all_seg(l + 1, t) = as<double>(get_cost(dat_smry, l + 1, t));
            }
            if (!arma::is_finite(all_seg(t + 1, r))) {
              all_seg(t + 1, r) = as<double>(get_cost(dat_smry, t + 1, r));
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
List WBS_custom(List dat_smry, int n, int L, int d, arma::mat lr_M, Function get_cost) {

  List out;

  int M = lr_M.n_rows;
  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);
  for (int i = 0; i < M; i++) {
    all_seg(lr_M(i, 0) + 1, lr_M(i, 1)) = as<double>(get_cost(dat_smry, lr_M(i, 0) + 1, lr_M(i, 1)));
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
              all_seg(li + 1, ri) = as<double>(get_cost(dat_smry, li + 1, ri));
            }
            for (int t = li; t <= ri; t++) {
              if ((t > li) & (t < ri) & (std::min(t - li, ri - t) >= d)) {
                count += 1;
                t_lr(count) = t;
                if (!arma::is_finite(all_seg(li + 1, t))) {
                  all_seg(li + 1, t) = as<double>(get_cost(dat_smry, li + 1, t));
                }
                if (!arma::is_finite(all_seg(t + 1, ri))) {
                  all_seg(t + 1, ri) = as<double>(get_cost(dat_smry, t + 1, ri));
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
List PELT_custom(List dat_smry, int n, arma::vec pen_val, int d, double K, Function get_cost) {

  List out;

  int n_pen_val = pen_val.n_elem;

  arma::mat all_seg(n + 1, n + 1); all_seg.fill(NA_REAL);
  arma::mat F_cost(n + 2, n_pen_val); F_cost.fill(NA_REAL);
  F_cost.row(1) = -pen_val.t();
  for (int ts = d; ts < 2 * d; ts++) {
    all_seg(1, ts) = as<double>(get_cost(dat_smry, 1, ts));
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
          all_seg(R_finite(i) + 1, ts) = as<double>(get_cost(dat_smry, R_finite(i) + 1, ts));
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
