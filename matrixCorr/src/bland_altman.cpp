// Thiago de Paula Oliveira
#include <RcppArmadillo.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo)]]

#include "matrixCorr_detail.h"
using matrixCorr_detail::moments::sample_var;

inline double mean_ba(const std::vector<double>& x) {
  const size_t n = x.size();
  if (n == 0) return NA_REAL;
  long double s = 0.0L;
  for (double v : x) s += v;
  return static_cast<double>(s / n);
}

// [[Rcpp::export]]
List bland_altman_cpp(NumericVector group1,
                      NumericVector group2,
                      double two        = 1.96,
                      int    mode       = 1,
                      double conf_level = 0.95) {
  if (group1.size() != group2.size())
    stop("Error in bland.altman.stats: groups differ in length.");
  if (two <= 0.0)
    stop("Error in bland.altman.stats: improper value of 'two'.");
  if (mode != 1 && mode != 2)
    stop("Error in bland.altman.stats: mode must be either 1 or 2.");
  if (!(conf_level > 0.0 && conf_level < 1.0))
    stop("conf_level must be in (0,1).");

  const int n_all = group1.size();

  // -- pairwise NA removal -----------------------------------------------------
  std::vector<double> g1; g1.reserve(n_all);
  std::vector<double> g2; g2.reserve(n_all);
  for (int i = 0; i < n_all; ++i) {
    double a = group1[i], b = group2[i];
    if (!NumericVector::is_na(a) && !NumericVector::is_na(b)) {
      g1.push_back(a);
      g2.push_back(b);
    }
  }
  const int n = static_cast<int>(g1.size());
  if (n < 2) {
    warning("Warning in bland.altman.stats: less than 2 data pairs after deleting NAs.");
  }

  // -- compute means and diffs -------------------------------------------------
  NumericVector means(n), diffs(n);
  for (int i = 0; i < n; ++i) {
    means[i] = (g1[i] + g2[i]) / 2.0;
    diffs[i] = (mode == 1) ? (g1[i] - g2[i]) : (g2[i] - g1[i]);
  }

  // -- summary stats -----------------------------------------------------------
  std::vector<double> diffs_std(diffs.begin(), diffs.end());
  const double mean_diffs = mean_ba(diffs_std);

  double sd_diffs = NA_REAL;
  if (n >= 2) {
    arma::vec diffs_view(diffs_std.data(), static_cast<arma::uword>(diffs_std.size()), /*copy_aux_mem*/ false);
    sd_diffs = std::sqrt(sample_var(diffs_view));
  }

  const double critical   = two * sd_diffs;
  const double lower      = mean_diffs - critical;
  const double upper      = mean_diffs + critical;

  // -- t quantiles for CIs ------------------------------
  const double t1 = R::qt((1.0 - conf_level) / 2.0, n - 1, /*lower_tail*/1, /*log_p*/0);
  const double t2 = R::qt((conf_level + 1.0) / 2.0,       n - 1, /*lower_tail*/1, /*log_p*/0);

  // SEs per Bland & Altman 1986
  const double se_limit = sd_diffs * std::sqrt(3.0 / n);
  const double se_mean  = sd_diffs / std::sqrt(static_cast<double>(n));

  NumericVector lines = NumericVector::create(
    _["lower.limit"] = lower,
    _["mean.diffs"]  = mean_diffs,
    _["upper.limit"] = upper
  );

  NumericVector CI_lines = NumericVector::create(
    _["lower.limit.ci.lower"] = lower + t1 * se_limit,
    _["lower.limit.ci.upper"] = lower + t2 * se_limit,
    _["mean.diff.ci.lower"]   = mean_diffs + t1 * se_mean,
    _["mean.diff.ci.upper"]   = mean_diffs + t2 * se_mean,
    _["upper.limit.ci.lower"] = upper + t1 * se_limit,
    _["upper.limit.ci.upper"] = upper + t2 * se_limit
  );

  // groups dataframe (after NA omission)
  DataFrame groups = DataFrame::create(
    _["group1"] = wrap(g1),
    _["group2"] = wrap(g2),
    _["stringsAsFactors"] = false
  );

  return List::create(
    _["means"]         = means,
    _["diffs"]         = diffs,
    _["groups"]        = groups,
    _["based.on"]      = n,
    _["lower.limit"]   = lower,
    _["mean.diffs"]    = mean_diffs,
    _["upper.limit"]   = upper,
    _["lines"]         = lines,
    _["CI.lines"]      = CI_lines,
    _["two"]           = two,
    _["critical.diff"] = critical
  );
}

// [[Rcpp::export]]
int ba_openmp_threads() {
  int n = 1;
#ifdef _OPENMP
  n = omp_get_max_threads();
#endif
  return n;
}
