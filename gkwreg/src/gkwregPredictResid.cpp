// gkwreg-predict-resid.cpp (v2)

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <map>
#include <string>
#include <sstream>
#include <iomanip>
#include <vector>

using namespace Rcpp;
using namespace arma;

// ========================
// Numeric constants
// ========================
const double eps_log       = 1e-15;
const double eps_pos       = 1e-10;
const double eps_prob      = 1e-12;
const double inf_repl      = 1e10;
const double max_exp       = 30.0;

// Iteration and quadrature constants
const int MAX_BISECTION_ITER = 100;
const int N_QUADRATURE_POINTS = 30;
const double BISECTION_TOL = 1e-8;
const double NUMERICAL_DERIV_H = 1e-6;
const double NUMERICAL_VAR_H = 1e-5;

// ========================
// Safe numerical operations
// ========================
double safeLog(const double x) {
  if (x <= 0.0) return -inf_repl;
  return std::log(x + eps_log);
}

double safeExp(const double x) {
  if (x > max_exp) return std::exp(max_exp);
  if (x < -max_exp) return std::exp(-max_exp);
  return std::exp(x);
}

double safePow(const double base, const double exponent) {
  if (base <= eps_pos) {
    return (exponent > 0.0) ? 0.0 : inf_repl;
  }
  if (std::fabs(exponent) > 1.0) {
    return safeExp(exponent * safeLog(base));
  }
  return std::pow(base, exponent);
}

// ========================
// Enforcement for domain
// ========================
double enforceMin(const double x, const double min_val) {
  return (x < min_val) ? min_val : x;
}

double enforceProbability(const double x) {
  const double upper = 1.0 - eps_prob;
  const double clippedUp = (x > upper) ? upper : x;
  return (clippedUp < eps_prob) ? eps_prob : clippedUp;
}

// ========================
// Link functions
// ========================
double fast_log_link(const double eta) {
  if (eta < -max_exp) return eps_pos;
  if (eta > max_exp) return std::exp(max_exp);
  return std::exp(eta);
}

double fast_logit_link(const double eta) {
  if (eta > max_exp) return 1.0 - eps_prob;
  if (eta < -max_exp) return eps_prob;
  return 1.0 / (1.0 + std::exp(-eta));
}

double fast_probit_link(const double eta) {
  if (eta > 5.0) return 1.0 - eps_prob;
  if (eta < -5.0) return eps_prob;
  return enforceProbability(R::pnorm(eta, 0.0, 1.0, 1, 0));
}

double fast_cloglog_link(const double eta) {
  if (eta > max_exp) return 1.0 - eps_prob;
  if (eta < -max_exp) return eps_prob;
  return enforceProbability(1.0 - safeExp(-safeExp(eta)));
}

double apply_positive_link(const double eta, const int link_type, const double scale_factor) {
  const double min_val = eps_pos;
  switch(link_type) {
  case 1:
    return enforceMin(fast_log_link(eta), min_val);
  case 2:
    return enforceMin(scale_factor * fast_logit_link(eta), min_val);
  case 3:
    return enforceMin(scale_factor * fast_probit_link(eta), min_val);
  case 4:
    return enforceMin(scale_factor * (0.5 + (std::atan(eta) / M_PI)), min_val);
  case 5:
    return enforceMin(scale_factor * fast_cloglog_link(eta), min_val);
  case 6:
    return enforceMin(eta, min_val);
  case 7:
    return enforceMin((eta > 0.0) ? eta * eta : 0.0, min_val);
  case 8:
    return enforceMin(1.0 / enforceMin(eta, 1e-6), min_val);
  case 9:
    return enforceMin(1.0 / std::sqrt(enforceMin(eta, 1e-6)), min_val);
  default:
    return enforceMin(fast_log_link(eta), min_val);
  }
}

// ========================
// Beta function utilities
// ========================
double logBeta(const double a, const double b) {
  const double aa = enforceMin(a, eps_pos);
  const double bb = enforceMin(b, eps_pos);
  if (aa > 100.0 && bb > 100.0) {
    return 0.5 * (std::log(2.0 * M_PI) - std::log(aa + bb)) +
      (aa - 0.5) * std::log(aa) +
      (bb - 0.5) * std::log(bb) -
      (aa + bb - 1.0) * std::log(aa + bb);
  }
  return R::lgammafn(aa) + R::lgammafn(bb) - R::lgammafn(aa + bb);
}

// ========================
// PDF calculation (log scale) for each family distribution
// ========================

// GKw distribution (Generalized Kumaraswamy)
double log_pdf_gkw(const double y,
                   const double alpha,
                   const double beta,
                   const double gamma,
                   const double delta,
                   const double lambda) {
  if (y <= eps_prob || y >= 1.0 - eps_prob) {
    return -inf_repl;
  }
  if (alpha <= eps_pos ||
      beta <= eps_pos ||
      gamma <= eps_pos ||
      delta <= eps_pos ||
      lambda <= eps_pos) {
    return -inf_repl;
  }

  const double log_y      = safeLog(y);
  const double log_lambda = safeLog(lambda);
  const double log_alpha  = safeLog(alpha);
  const double log_beta   = safeLog(beta);
  const double logB       = logBeta(gamma, delta + 1.0);
  const double ya         = safePow(y, alpha);
  const double one_minus_ya = enforceProbability(1.0 - ya);
  const double log_one_minus_ya = safeLog(one_minus_ya);
  const double one_minus_ya_b = safePow(one_minus_ya, beta);
  const double v          = enforceProbability(1.0 - one_minus_ya_b);
  const double log_v      = safeLog(v);
  const double v_lambda   = safePow(v, lambda);
  const double u          = enforceProbability(1.0 - v_lambda);
  const double log_u      = safeLog(u);

  const double term1 = log_lambda + log_alpha + log_beta - logB;
  const double term2 = (alpha - 1.0) * log_y;
  const double term3 = (beta - 1.0) * log_one_minus_ya;
  const double term4 = (gamma * lambda - 1.0) * log_v;
  const double term5 = delta * log_u;

  const double logf = term1 + term2 + term3 + term4 + term5;
  if (!std::isfinite(logf)) {
    return -inf_repl;
  }

  return logf;
}

// BKw distribution (Beta-Kumaraswamy)
double log_pdf_bkw(const double y,
                   const double alpha,
                   const double beta,
                   const double gamma,
                   const double delta,
                   const double lambda) {
  // BKw = GKw with lambda = 1
  return log_pdf_gkw(y, alpha, beta, gamma, delta, 1.0);
}

// KKw distribution (Kumaraswamy-Kumaraswamy)
double log_pdf_kkw(const double y,
                   const double alpha,
                   const double beta,
                   const double gamma,
                   const double delta,
                   const double lambda) {
  // KKw = GKw with gamma = 1
  return log_pdf_gkw(y, alpha, beta, 1.0, delta, lambda);
}

// EKw distribution (Exponentiated Kumaraswamy)
double log_pdf_ekw(const double y,
                   const double alpha,
                   const double beta,
                   const double gamma,
                   const double delta,
                   const double lambda) {
  // EKw = GKw with gamma = 1, delta = 0
  return log_pdf_gkw(y, alpha, beta, 1.0, 0.0, lambda);
}

// MC distribution (McDonald)
double log_pdf_mc(const double y,
                  const double alpha,
                  const double beta,
                  const double gamma,
                  const double delta,
                  const double lambda) {
  // MC = GKw with alpha = 1, beta = 1
  return log_pdf_gkw(y, 1.0, 1.0, gamma, delta, lambda);
}

// KW distribution (Kumaraswamy)
double log_pdf_kw(const double y,
                  const double alpha,
                  const double beta,
                  const double gamma,
                  const double delta,
                  const double lambda) {
  // KW = GKw with lambda = 1, gamma = 1, delta = 0
  if (y <= eps_prob || y >= 1.0 - eps_prob) {
    return -inf_repl;
  }
  if (alpha <= eps_pos || beta <= eps_pos) {
    return -inf_repl;
  }

  const double log_alpha = safeLog(alpha);
  const double log_beta = safeLog(beta);
  const double log_y = safeLog(y);
  const double ya = safePow(y, alpha);
  const double one_minus_ya = enforceProbability(1.0 - ya);
  const double log_one_minus_ya = safeLog(one_minus_ya);

  return log_alpha + log_beta + (alpha - 1.0) * log_y + (beta - 1.0) * log_one_minus_ya;
}

// Beta distribution
double log_pdf_beta(const double y,
                    const double alpha,
                    const double beta,
                    const double gamma,
                    const double delta,
                    const double lambda) {
  // Beta = GKw with alpha = 1, beta = 1, lambda = 1
  if (y <= eps_prob || y >= 1.0 - eps_prob) {
    return -inf_repl;
  }
  if (gamma <= eps_pos || delta <= eps_pos) {
    return -inf_repl;
  }

  const double logB = logBeta(gamma, delta + 1.0);
  const double term1 = -logB;
  const double term2 = (gamma - 1.0) * safeLog(y);
  const double term3 = delta * safeLog(1.0 - y);

  const double logf = term1 + term2 + term3;
  if (!std::isfinite(logf)) {
    return -inf_repl;
  }

  return logf;
}

// Generic dispatcher for log PDF
double log_pdf(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda,
               const std::string& family) {
  if (family == "gkw") {
    return log_pdf_gkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "bkw") {
    return log_pdf_bkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "kkw") {
    return log_pdf_kkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "kkw") {
    return log_pdf_kkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "ekw") {
    return log_pdf_ekw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "mc") {
    return log_pdf_mc(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "kw") {
    return log_pdf_kw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "beta") {
    return log_pdf_beta(y, alpha, beta, gamma, delta, lambda);
  } else {
    Rcpp::warning("Family not recognized. Using 'gkw' as default.");
    return log_pdf_gkw(y, alpha, beta, gamma, delta, lambda);
  }
}

// ========================
// PDF calculation (regular scale)
// ========================
double pdf_gkw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  return safeExp(log_pdf_gkw(y, alpha, beta, gamma, delta, lambda));
}

double pdf_bkw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  return safeExp(log_pdf_bkw(y, alpha, beta, gamma, delta, lambda));
}

double pdf_kkw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  return safeExp(log_pdf_kkw(y, alpha, beta, gamma, delta, lambda));
}

double pdf_ekw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  return safeExp(log_pdf_ekw(y, alpha, beta, gamma, delta, lambda));
}

double pdf_mc(const double y,
              const double alpha,
              const double beta,
              const double gamma,
              const double delta,
              const double lambda) {
  return safeExp(log_pdf_mc(y, alpha, beta, gamma, delta, lambda));
}

double pdf_kw(const double y,
              const double alpha,
              const double beta,
              const double gamma,
              const double delta,
              const double lambda) {
  return safeExp(log_pdf_kw(y, alpha, beta, gamma, delta, lambda));
}

double pdf_beta(const double y,
                const double alpha,
                const double beta,
                const double gamma,
                const double delta,
                const double lambda) {
  return safeExp(log_pdf_beta(y, alpha, beta, gamma, delta, lambda));
}

// General PDF function for all distributions
double pdf(const double y,
           const double alpha,
           const double beta,
           const double gamma,
           const double delta,
           const double lambda,
           const std::string& family) {
  return safeExp(log_pdf(y, alpha, beta, gamma, delta, lambda, family));
}

// ========================
// CDF calculation for each distribution
// ========================

// CDF for GKw
double cdf_gkw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  if (y <= 0.0) return 0.0;
  if (y >= 1.0) return 1.0;

  // Calculate v = 1 - (1 - y^alpha)^beta
  const double ya = safePow(y, alpha);
  const double one_minus_ya = enforceProbability(1.0 - ya);
  const double one_minus_ya_b = safePow(one_minus_ya, beta);
  const double v = enforceProbability(1.0 - one_minus_ya_b);

  // Calculate v^lambda and then use the beta regularized function
  const double v_lambda = safePow(v, lambda);
  return R::pbeta(v_lambda, gamma, delta + 1.0, 1, 0);
}

// CDF for BKw
double cdf_bkw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  // BKw = GKw with lambda = 1
  return cdf_gkw(y, alpha, beta, gamma, delta, 1.0);
}

// CDF for KKw
double cdf_kkw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  // KKw = GKw with gamma = 1
  if (y <= 0.0) return 0.0;
  if (y >= 1.0) return 1.0;

  // Closed form for KKw when gamma = 1
  const double ya = safePow(y, alpha);
  const double one_minus_ya = enforceProbability(1.0 - ya);
  const double one_minus_ya_b = safePow(one_minus_ya, beta);
  const double v = enforceProbability(1.0 - one_minus_ya_b);
  const double v_lambda = safePow(v, lambda);

  return 1.0 - safePow(1.0 - v_lambda, delta + 1.0);
}

// CDF for EKw
double cdf_ekw(const double y,
               const double alpha,
               const double beta,
               const double gamma,
               const double delta,
               const double lambda) {
  // EKw = GKw with gamma = 1, delta = 0
  if (y <= 0.0) return 0.0;
  if (y >= 1.0) return 1.0;

  // For EKw: F(y) = [1-(1-y^alpha)^beta]^lambda
  const double ya = safePow(y, alpha);
  const double one_minus_ya = enforceProbability(1.0 - ya);
  const double one_minus_ya_b = safePow(one_minus_ya, beta);
  const double v = enforceProbability(1.0 - one_minus_ya_b);

  return safePow(v, lambda);
}

// CDF for McDonald
double cdf_mc(const double y,
              const double alpha,
              const double beta,
              const double gamma,
              const double delta,
              const double lambda) {
  // MC = GKw with alpha = 1, beta = 1
  return cdf_gkw(y, 1.0, 1.0, gamma, delta, lambda);
}

// CDF for Kumaraswamy
double cdf_kw(const double y,
              const double alpha,
              const double beta,
              const double gamma,
              const double delta,
              const double lambda) {
  // KW = GKw with lambda = 1, gamma = 1, delta = 0
  if (y <= 0.0) return 0.0;
  if (y >= 1.0) return 1.0;

  // Closed form for Kumaraswamy: F(y) = 1 - (1 - y^alpha)^beta
  const double ya = safePow(y, alpha);
  const double one_minus_ya = enforceProbability(1.0 - ya);

  return 1.0 - safePow(one_minus_ya, beta);
}

// CDF for Beta
double cdf_beta(const double y,
                const double alpha,
                const double beta,
                const double gamma,
                const double delta,
                const double lambda) {
  // Beta = GKw with alpha = 1, beta = 1, lambda = 1
  if (y <= 0.0) return 0.0;
  if (y >= 1.0) return 1.0;

  // Use R's built-in pbeta for standard beta distribution
  return R::pbeta(y, gamma, delta + 1.0, 1, 0);
}

// Generic CDF dispatcher
double cdf(const double y,
           const double alpha,
           const double beta,
           const double gamma,
           const double delta,
           const double lambda,
           const std::string& family) {
  if (family == "gkw") {
    return cdf_gkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "bkw") {
    return cdf_bkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "kkw") {
    return cdf_kkw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "ekw") {
    return cdf_ekw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "mc") {
    return cdf_mc(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "kw") {
    return cdf_kw(y, alpha, beta, gamma, delta, lambda);
  } else if (family == "beta") {
    return cdf_beta(y, alpha, beta, gamma, delta, lambda);
  } else {
    Rcpp::warning("Family not recognized. Using 'gkw' as default.");
    return cdf_gkw(y, alpha, beta, gamma, delta, lambda);
  }
}

// ========================
// Mean calculation for each distribution
// ========================

// Mean calculation for GKw using numerical integration
double calc_mean_gkw(const double alpha,
                     const double beta,
                     const double gamma,
                     const double delta,
                     const double lambda,
                     const std::string& family = "gkw") {

  // Pre-calculated Gauss-Legendre quadrature points and weights (30-point)
  static const double points[N_QUADRATURE_POINTS] = {
    0.0052995325041789, 0.0277124884633837, 0.0671843988060841,
    0.1222977958224985, 0.1910618777986781, 0.2709916111713514,
    0.3591982246103705, 0.4524937450811611, 0.5475062549188389,
    0.6408017753896295, 0.7290083888286486, 0.8089381222013219,
    0.8777022041775015, 0.9328156011939159, 0.9722875115366163,
    0.9947004674958211, 0.0016634282895682, 0.0088218260005356,
    0.0216951734782546, 0.0401505773180499, 0.0640198684962854,
    0.0929719876996177, 0.1266873881927669, 0.1648092571058728,
    0.2069463985939003, 0.2526493772311021, 0.3014937918994291,
    0.3529709288365058, 0.4065775351876358, 0.4618179845446256
  };

  static const double weights[N_QUADRATURE_POINTS] = {
    0.0135762297058770, 0.0311267619693239, 0.0475792558412463,
    0.0623144856277781, 0.0747979944082893, 0.0845782596975012,
    0.0913017075224617, 0.0947253052275342, 0.0947253052275342,
    0.0913017075224617, 0.0845782596975012, 0.0747979944082893,
    0.0623144856277781, 0.0475792558412463, 0.0311267619693239,
    0.0135762297058770, 0.0042582355019693, 0.0098975679009239,
    0.0153793884993804, 0.0207860520784162, 0.0260583032078977,
    0.0311490754242281, 0.0360154830389962, 0.0406283004740704,
    0.0449535797324026, 0.0489611395857007, 0.0526254269148138,
    0.0559249517732422, 0.0588415244791467, 0.0613600687415760
  };

  double mean = 0.0;
  double total_weight = 0.0;

  for (int i = 0; i < N_QUADRATURE_POINTS; i++) {
    const double y_i = points[i];
    if (y_i < eps_prob || y_i > 1.0 - eps_prob)
      continue;

    const double logpdf_val = log_pdf(y_i, alpha, beta, gamma, delta, lambda, family);
    const double pdf_val = (logpdf_val > -max_exp) ? safeExp(logpdf_val) : 0.0;
    const double weight_i = weights[i];

    mean += y_i * weight_i * pdf_val;
    total_weight += weight_i * pdf_val;
  }

  if (total_weight > eps_pos) {
    mean /= total_weight;
  }

  return enforceProbability(mean);
}

// ========================
// Cache key helper
// ========================
std::string make_cache_key(const double alpha,
                           const double beta,
                           const double gamma,
                           const double delta,
                           const double lambda,
                           const std::string& family) {
  std::ostringstream key;
  key << std::fixed << std::setprecision(4)
      << alpha << "_" << beta << "_"
      << gamma << "_" << delta << "_"
      << lambda << "_" << family;
  return key.str();
}

// ========================
// Variance approximation for distributions
// ========================
double var_dist(const double mean,
                const double alpha,
                const double beta,
                const double gamma,
                const double delta,
                const double lambda,
                const std::string& family) {
  const double h = NUMERICAL_VAR_H;  // Step for numerical differentiation
  const double y_minus = std::max(eps_prob, mean - h);
  const double y_plus = std::min(1.0 - eps_prob, mean + h);

  const double f_minus = pdf(y_minus, alpha, beta, gamma, delta, lambda, family);
  const double f = pdf(mean, alpha, beta, gamma, delta, lambda, family);
  const double f_plus = pdf(y_plus, alpha, beta, gamma, delta, lambda, family);

  // Second derivative approximation
  const double d2F = (f_plus - 2.0 * f + f_minus) / (h * h);
  const double var = 1.0 / (std::fabs(d2F) + 1e-6);

  return std::min(0.25, std::max(1e-6, var));
}

// ========================
// Score function for mean parameter (numerical derivative)
// ========================
double score_mean_dist(const double y,
                       const double mean,
                       const double alpha,
                       const double beta,
                       const double gamma,
                       const double delta,
                       const double lambda,
                       const std::string& family) {
  const double h = NUMERICAL_DERIV_H;  // Small step size
  const double y_plus = std::min(1.0 - eps_prob, y + h);
  const double y_minus = std::max(eps_prob, y - h);

  const double ll_minus = log_pdf(y_minus, alpha, beta, gamma, delta, lambda, family);
  const double ll_plus = log_pdf(y_plus, alpha, beta, gamma, delta, lambda, family);

  // Central difference approximation
  const double score = (ll_plus - ll_minus) / (2.0 * h);
  return score;
}

// ========================
// Exported functions (PREDICT)
// ========================

// @title Calculate Parameters for the Generalized Kumaraswamy Distribution
//
// @description
// Computes the parameters (alpha, beta, gamma, delta, lambda) for each observation based on design matrices and regression coefficients,
// applying a positive link function as specified by link types and scale factors.
//
// @param X1 NumericMatrix design matrix for alpha.
// @param X2 NumericMatrix design matrix for beta.
// @param X3 NumericMatrix design matrix for gamma.
// @param X4 NumericMatrix design matrix for delta.
// @param X5 NumericMatrix design matrix for lambda.
// @param beta1 NumericVector regression coefficients for X1.
// @param beta2 NumericVector regression coefficients for X2.
// @param beta3 NumericVector regression coefficients for X3.
// @param beta4 NumericVector regression coefficients for X4.
// @param beta5 NumericVector regression coefficients for X5.
// @param link_types IntegerVector containing the link function type for each parameter.
// @param scale_factors NumericVector with scale factors for each parameter.
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericMatrix with n rows and 5 columns corresponding to the calculated parameters.
// [[Rcpp::export]]
NumericMatrix calculateParameters(const NumericMatrix& X1,
                                  const NumericMatrix& X2,
                                  const NumericMatrix& X3,
                                  const NumericMatrix& X4,
                                  const NumericMatrix& X5,
                                  const NumericVector& beta1,
                                  const NumericVector& beta2,
                                  const NumericVector& beta3,
                                  const NumericVector& beta4,
                                  const NumericVector& beta5,
                                  const IntegerVector& link_types,
                                  const NumericVector& scale_factors,
                                  const std::string& family) {
  const int n = X1.nrow();
  NumericMatrix params(n, 5); // [alpha, beta, gamma, delta, lambda]

  NumericVector eta1(n), eta2(n), eta3(n), eta4(n), eta5(n);

  // Calculate linear predictors X * beta for each design matrix
  for (int i = 0; i < n; i++) {
    double sum1 = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0, sum5 = 0.0;

    for (int j = 0; j < X1.ncol(); j++) {
      sum1 += X1(i, j) * beta1[j];
    }
    for (int j = 0; j < X2.ncol(); j++) {
      sum2 += X2(i, j) * beta2[j];
    }
    for (int j = 0; j < X3.ncol(); j++) {
      sum3 += X3(i, j) * beta3[j];
    }
    for (int j = 0; j < X4.ncol(); j++) {
      sum4 += X4(i, j) * beta4[j];
    }
    for (int j = 0; j < X5.ncol(); j++) {
      sum5 += X5(i, j) * beta5[j];
    }

    eta1[i] = sum1;
    eta2[i] = sum2;
    eta3[i] = sum3;
    eta4[i] = sum4;
    eta5[i] = sum5;
  }

  // Apply the link functions to transform linear predictors to parameters
  for (int i = 0; i < n; i++) {
    params(i, 0) = apply_positive_link(eta1[i], link_types[0], scale_factors[0]);
    params(i, 1) = apply_positive_link(eta2[i], link_types[1], scale_factors[1]);
    params(i, 2) = apply_positive_link(eta3[i], link_types[2], scale_factors[2]);
    params(i, 3) = apply_positive_link(eta4[i], link_types[3], scale_factors[3]);
    params(i, 4) = apply_positive_link(eta5[i], link_types[4], scale_factors[4]);
  }

  // For certain distributions, enforce parameter constraints based on the specific family
  if (family == "bkw") {
    // BKw: lambda = 1
    for (int i = 0; i < n; i++) {
      params(i, 4) = 1.0;
    }
  } else if (family == "kkw") {
    // KKw: gamma = 1
    for (int i = 0; i < n; i++) {
      params(i, 2) = 1.0;
    }
  } else if (family == "ekw") {
    // EKw: gamma = 1, delta = 0
    for (int i = 0; i < n; i++) {
      params(i, 2) = 1.0;
      params(i, 3) = 0.0;
    }
  } else if (family == "mc") {
    // MC: alpha = 1, beta = 1
    for (int i = 0; i < n; i++) {
      params(i, 0) = 1.0;
      params(i, 1) = 1.0;
    }
  } else if (family == "kw") {
    // KW: lambda = 1, gamma = 1, delta = 0
    for (int i = 0; i < n; i++) {
      params(i, 2) = 1.0;
      params(i, 3) = 0.0;
      params(i, 4) = 1.0;
    }
  } else if (family == "beta") {
    // Beta: alpha = 1, beta = 1, lambda = 1
    for (int i = 0; i < n; i++) {
      params(i, 0) = 1.0;
      params(i, 1) = 1.0;
      params(i, 4) = 1.0;
    }
  }

  return params;
}

// @title Calculate Means for Distribution
//
// @description
// Computes the mean of the distribution for each observation using numerical integration
// (quadrature) with caching to avoid redundant calculations.
//
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector containing the calculated means for each observation.
// [[Rcpp::export]]
NumericVector calculateMeans(const NumericMatrix& params,
                             const std::string& family = "gkw") {
  const int n = params.nrow();
  NumericVector means(n);
  std::map<std::string, double> cache;

  for (int i = 0; i < n; i++) {
    // Cache parameters for this observation
    const double alpha  = params(i, 0);
    const double beta   = params(i, 1);
    const double gamma  = params(i, 2);
    const double delta  = params(i, 3);
    const double lambda = params(i, 4);

    const std::string key = make_cache_key(alpha, beta, gamma, delta, lambda, family);

    const auto it = cache.find(key);
    if (it != cache.end()) {
      means[i] = it->second;
    } else {
      const double mean_val = calc_mean_gkw(alpha, beta, gamma, delta, lambda, family);
      cache[key] = mean_val;
      means[i] = mean_val;
    }
  }
  return means;
}

// @title Calculate Densities for Distribution
//
// @description
// Evaluates the density (or its logarithm) for each observation given the parameters.
//
// @param y NumericVector of observations.
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
// @param log Logical indicating whether to return the log-density (default FALSE).
//
// @return NumericVector containing the evaluated densities.
// [[Rcpp::export]]
NumericVector calculateDensities(const NumericVector& y,
                                 const NumericMatrix& params,
                                 const std::string& family = "gkw",
                                 const bool log = false) {
  const int n = y.size();
  NumericVector densities(n);

  for (int i = 0; i < n; i++) {
    const double logf = log_pdf(y[i], params(i, 0), params(i, 1),
                                params(i, 2), params(i, 3), params(i, 4), family);
    densities[i] = log ? logf : safeExp(logf);
  }
  return densities;
}

// @title Calculate Cumulative Probabilities for Distribution
//
// @description
// Computes the cumulative probabilities (CDF) for each observation given the parameters.
//
// @param y NumericVector of observations.
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector containing the evaluated cumulative probabilities.
// [[Rcpp::export]]
NumericVector calculateProbabilities(const NumericVector& y,
                                     const NumericMatrix& params,
                                     const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector probs(n);

  for (int i = 0; i < n; i++) {
    probs[i] = cdf(y[i], params(i, 0), params(i, 1),
                   params(i, 2), params(i, 3), params(i, 4), family);
  }
  return probs;
}

// @title Calculate Quantiles for Distribution
//
// @description
// Computes quantiles for the given probability levels using a bisection method for the first set
// of parameters in the matrix.
//
// @param probs NumericVector of probabilities (values in (0,1)).
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector containing the calculated quantiles.
// [[Rcpp::export]]
NumericVector calculateQuantiles(const NumericVector& probs,
                                 const NumericMatrix& params,
                                 const std::string& family = "gkw") {
  const int n = probs.size();
  NumericVector quantiles(n);

  // Validate input probabilities
  for (int i = 0; i < n; i++) {
    if (probs[i] < 0.0 || probs[i] > 1.0) {
      Rcpp::stop("All probabilities must be in [0,1]");
    }
  }

  // Extract parameters from first row (used for all quantile calculations)
  const double alpha  = params(0, 0);
  const double beta   = params(0, 1);
  const double gamma  = params(0, 2);
  const double delta  = params(0, 3);
  const double lambda = params(0, 4);

  for (int i = 0; i < n; i++) {
    const double p = probs[i];

    if (p <= eps_prob) {
      quantiles[i] = eps_prob;
      continue;
    }
    if (p >= 1.0 - eps_prob) {
      quantiles[i] = 1.0 - eps_prob;
      continue;
    }

    // Bisection method to find quantile
    double lower = eps_prob;
    double upper = 1.0 - eps_prob;
    double mid = 0.5;  // Initialize mid to avoid uninitialized variable warning
    double cdf_mid;

    for (int iter = 0; iter < MAX_BISECTION_ITER; iter++) {
      mid = (lower + upper) / 2.0;
      cdf_mid = cdf(mid, alpha, beta, gamma, delta, lambda, family);

      if (std::abs(cdf_mid - p) < BISECTION_TOL) break;

      if (cdf_mid < p)
        lower = mid;
      else
        upper = mid;
    }
    quantiles[i] = mid;
  }
  return quantiles;
}

// ========================
// Exported functions (RESIDUALS)
// ========================

// @title Calculate Response Residuals
//
// @description
// Computes the raw response residuals as the difference between the observed and fitted values.
//
// @param y NumericVector of observations.
// @param fitted NumericVector of fitted values.
//
// @return NumericVector of response residuals.
// [[Rcpp::export]]
NumericVector calculateResponseResiduals(const NumericVector& y,
                                         const NumericVector& fitted) {
  const int n = y.size();
  NumericVector residuals(n);
  for (int i = 0; i < n; i++) {
    residuals[i] = y[i] - fitted[i];
  }
  return residuals;
}

// @title Calculate Pearson Residuals
//
// @description
// Computes the Pearson residuals based on the observed values, fitted means, and the approximate variance of the distribution.
//
// @param y NumericVector of observations.
// @param fitted NumericVector of fitted values (means).
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector of Pearson residuals.
// [[Rcpp::export]]
NumericVector calculatePearsonResiduals(const NumericVector& y,
                                        const NumericVector& fitted,
                                        const NumericMatrix& params,
                                        const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector residuals(n);

  for (int i = 0; i < n; i++) {
    const double mu_i = fitted[i];
    const double var_i = var_dist(mu_i, params(i, 0), params(i, 1),
                                  params(i, 2), params(i, 3), params(i, 4), family);
    const double sd_i = std::sqrt(var_i);
    residuals[i] = (y[i] - mu_i) / (sd_i + eps_pos);
  }
  return residuals;
}

// @title Calculate Deviance Residuals
//
// @description
// Computes deviance residuals based on the log-likelihood of the observations.
//
// @param y NumericVector of observations.
// @param fitted NumericVector of fitted values (means).
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector of deviance residuals.
// [[Rcpp::export]]
NumericVector calculateDevianceResiduals(const NumericVector& y,
                                         const NumericVector& fitted,
                                         const NumericMatrix& params,
                                         const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector residuals(n);

  for (int i = 0; i < n; i++) {
    const double logf = log_pdf(y[i], params(i, 0), params(i, 1),
                                params(i, 2), params(i, 3), params(i, 4), family);
    const double sign_res = (y[i] - fitted[i] > 0.0) ? 1.0 : -1.0;
    residuals[i] = sign_res * std::sqrt(std::fabs(-2.0 * logf));
  }
  return residuals;
}

// @title Calculate Quantile Residuals
//
// @description
// Computes quantile residuals by transforming the cumulative distribution function (CDF) values to the standard normal quantiles.
//
// @param y NumericVector of observations.
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector of quantile residuals.
// [[Rcpp::export]]
NumericVector calculateQuantileResiduals(const NumericVector& y,
                                         const NumericMatrix& params,
                                         const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector residuals(n);

  for (int i = 0; i < n; i++) {
    const double cdf_val = cdf(y[i], params(i, 0), params(i, 1),
                               params(i, 2), params(i, 3), params(i, 4), family);
    double prob = enforceProbability(cdf_val);

    // Clamp to avoid extreme quantiles
    prob = std::max(0.001, std::min(0.999, prob));
    residuals[i] = R::qnorm(prob, 0.0, 1.0, 1, 0);
  }
  return residuals;
}

// @title Calculate Cox-Snell Residuals
//
// @description
// Computes Cox-Snell residuals defined as -log(1 - F(y)), where F is the cumulative distribution function.
//
// @param y NumericVector of observations.
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector of Cox-Snell residuals.
// [[Rcpp::export]]
NumericVector calculateCoxSnellResiduals(const NumericVector& y,
                                         const NumericMatrix& params,
                                         const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector residuals(n);

  for (int i = 0; i < n; i++) {
    double cdf_val = cdf(y[i], params(i, 0), params(i, 1),
                         params(i, 2), params(i, 3), params(i, 4), family);
    cdf_val = std::max(eps_prob, std::min(1.0 - eps_prob, cdf_val));
    residuals[i] = -std::log(1.0 - cdf_val);
  }
  return residuals;
}

// @title Calculate Score Residuals
//
// @description
// Computes score residuals based on the numerical derivative (score) of the log-likelihood with respect to the observation.
//
// @param y NumericVector of observations.
// @param fitted NumericVector of fitted values (means).
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector of score residuals.
// [[Rcpp::export]]
NumericVector calculateScoreResiduals(const NumericVector& y,
                                      const NumericVector& fitted,
                                      const NumericMatrix& params,
                                      const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector residuals(n);

  for (int i = 0; i < n; i++) {
    residuals[i] = score_mean_dist(y[i], fitted[i], params(i, 0), params(i, 1),
                                   params(i, 2), params(i, 3), params(i, 4), family);
  }
  return residuals;
}

// @title Calculate Modified Deviance Residuals
//
// @description
// Adjusts deviance residuals to have a distribution closer to N(0,1) by standardizing them.
//
// @param y NumericVector of observations.
// @param fitted NumericVector of fitted values (means).
// @param params NumericMatrix with parameters (columns: alpha, beta, gamma, delta, lambda).
// @param family String specifying the distribution family (default: "gkw").
//
// @return NumericVector of modified deviance residuals.
// [[Rcpp::export]]
NumericVector calculateModifiedDevianceResiduals(const NumericVector& y,
                                                 const NumericVector& fitted,
                                                 const NumericMatrix& params,
                                                 const std::string& family = "gkw") {
  const int n = y.size();
  NumericVector residuals(n);
  const NumericVector dev_res = calculateDevianceResiduals(y, fitted, params, family);

  // Calculate mean of deviance residuals
  double mean_dev = 0.0;
  for (int i = 0; i < n; i++) {
    mean_dev += dev_res[i];
  }
  mean_dev /= n;

  // Calculate standard deviation of deviance residuals
  double sd_dev = 0.0;
  for (int i = 0; i < n; i++) {
    const double diff = dev_res[i] - mean_dev;
    sd_dev += diff * diff;
  }
  sd_dev = std::sqrt(sd_dev / (n - 1));

  // Standardize deviance residuals
  for (int i = 0; i < n; i++) {
    residuals[i] = (dev_res[i] - mean_dev) / sd_dev;
  }
  return residuals;
}

// @title Calculate Partial Residuals
//
// @description
// Computes partial residuals for a selected covariate by adding the product of the regression coefficient and
// the corresponding design matrix value to the raw residual.
//
// @param y NumericVector of observations.
// @param fitted NumericVector of fitted values.
// @param X NumericMatrix of design matrix values.
// @param beta NumericVector of regression coefficients.
// @param covariate_idx Integer index for the selected covariate (0-indexed).
//
// @return NumericVector of partial residuals.
// [[Rcpp::export]]
NumericVector calculatePartialResiduals(const NumericVector& y,
                                        const NumericVector& fitted,
                                        const NumericMatrix& X,
                                        const NumericVector& beta,
                                        const int covariate_idx) {
  const int n = y.size();
  const int p = beta.size();
  NumericVector residuals(n);

  if (covariate_idx < 0 || covariate_idx >= p) {
    stop("covariate_idx must be between 0 and %d", p - 1);
  }

  const double beta_cov = beta[covariate_idx];

  for (int i = 0; i < n; i++) {
    residuals[i] = (y[i] - fitted[i]) + beta_cov * X(i, covariate_idx);
  }
  return residuals;
}
