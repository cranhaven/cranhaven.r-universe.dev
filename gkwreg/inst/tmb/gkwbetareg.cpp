// File: betareg.cpp
// ---------------------------------------------------------------------
//  Regression Model for the Beta (B) Distribution
//
//  This is a special case of the Generalized Kumaraswamy distribution
//  with α = β = λ = 1 fixed. The probability density function is:
//
//  f(x; γ, δ) = [x^(γ-1) * (1-x)^δ] / B(γ, δ+1)
//
//  where 0 < x < 1, γ > 0, δ > 0, and B(γ, δ+1) is the beta function:
//  B(γ, δ+1) = Γ(γ)Γ(δ+1)/Γ(γ+δ+1)
//
//  As noted in the paper, this is the special case of the GKw distribution
//  where α = β = λ = 1.
// ---------------------------------------------------------------------

#include <TMB.hpp>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <cassert>

// ========================
// Numeric constants
// ========================
template <class Type>
struct Constants {
  static const Type eps_log;
  static const Type eps_pos;
  static const Type eps_prob;
  static const Type inf_repl;
  static const Type max_exp;
};

template <class Type>
const Type Constants<Type>::eps_log  = Type(1e-15);
template <class Type>
const Type Constants<Type>::eps_pos  = Type(1e-10);
template <class Type>
const Type Constants<Type>::eps_prob = Type(1e-12);
template <class Type>
const Type Constants<Type>::inf_repl = Type(1e10);
template <class Type>
const Type Constants<Type>::max_exp  = Type(30);

// ========================
// Safe numerical operations
// ========================
template <class Type>
Type safeLog(const Type &x) {
  // Avoid log(<=0)
  if (x <= Type(0.0)) return -Constants<Type>::inf_repl;
  return log(x + Constants<Type>::eps_log);
}

template <class Type>
Type safeExp(const Type &x) {
  // Clamp exponent to avoid overflow
  if (x >  Constants<Type>::max_exp)  return exp(Constants<Type>::max_exp);
  if (x < -Constants<Type>::max_exp)  return exp(-Constants<Type>::max_exp);
  return exp(x);
}

template <class Type>
Type safePow(const Type &base, const Type &exponent) {
  // Protect base^exponent in extreme cases
  if (base <= Constants<Type>::eps_pos) {
    return (exponent > Type(0.0)) ? Type(0.0) : Constants<Type>::inf_repl;
  }
  if (fabs(exponent) > Type(1.0)) {
    return safeExp(exponent * safeLog(base));
  }
  return pow(base, exponent);
}

// ========================
// Enforce domain
// ========================
template <class Type>
Type enforceMin(const Type &x, const Type &min_val) {
  // max(x, min_val)
  return CppAD::CondExpLt(x, min_val, min_val, x);
}

template <class Type>
Type enforceProbability(const Type &x) {
  // Clamps x into (eps_prob, 1 - eps_prob)
  Type eps   = Constants<Type>::eps_prob;
  Type upper = Type(1.0) - eps;
  Type clippedUp = CppAD::CondExpGt(x, upper, upper, x);
  return CppAD::CondExpLt(clippedUp, eps, eps, clippedUp);
}

// ========================
// Link functions
// (1=log, 2=logit, 3=probit, 4=cauchy, 5=cloglog,
//  6=identity, 7=sqrt, 8=inverse, 9=inverse-square)
// ========================
template <class Type>
Type fast_log_link(const Type &eta) {
  return safeExp(eta);
}

template <class Type>
Type fast_logit_link(const Type &eta) {
  if (eta >  Constants<Type>::max_exp) return Type(1.0) - Constants<Type>::eps_prob;
  if (eta < -Constants<Type>::max_exp) return Constants<Type>::eps_prob;
  return Type(1.0) / (Type(1.0) + safeExp(-eta));
}

template <class Type>
Type fast_probit_link(const Type &eta) {
  if (eta >  Type(5.0))  return Type(1.0) - Constants<Type>::eps_prob;
  if (eta < -Type(5.0))  return Constants<Type>::eps_prob;
  return enforceProbability(pnorm(eta));
}

template <class Type>
Type fast_cloglog_link(const Type &eta) {
  if (eta >  Constants<Type>::max_exp) return Type(1.0) - Constants<Type>::eps_prob;
  if (eta < -Constants<Type>::max_exp) return Constants<Type>::eps_prob;
  return enforceProbability(Type(1.0) - safeExp(-safeExp(eta)));
}

template <class Type>
Type apply_positive_link(const Type &eta, const int &link_type, const Type &scale_factor) {
  Type min_val = Constants<Type>::eps_pos;
  switch (link_type) {
  case 1: // log
    return enforceMin(fast_log_link(eta), min_val);
  case 2: // logit
    return enforceMin(scale_factor * fast_logit_link(eta), min_val);
  case 3: // probit
    return enforceMin(scale_factor * fast_probit_link(eta), min_val);
  case 4: // cauchy
    // cdf cauchy = 0.5 + atan(eta)/pi
    return enforceMin(scale_factor * (Type(0.5) + (atan(eta)/Type(M_PI))), min_val);
  case 5: // cloglog
    return enforceMin(scale_factor * fast_cloglog_link(eta), min_val);
  case 6: // identity (clamped)
    return enforceMin(eta, min_val);
  case 7: // sqrt
    return enforceMin(CppAD::CondExpGt(eta, Type(0.0), eta*eta, Type(0.0)), min_val);
  case 8: // inverse
    return enforceMin(Type(1.0)/enforceMin(eta, Type(1e-6)), min_val);
  case 9: // inverse-square
    return enforceMin(Type(1.0)/sqrt(enforceMin(eta, Type(1e-6))), min_val);
  default:
    // fallback to log
    return enforceMin(fast_log_link(eta), min_val);
  }
}

// ========================
// Beta log-PDF - Implementation for Beta as a GKw special case
// ========================
template <class Type>
Type log_pdf_beta(const Type &y,
                  const Type &gamma,
                  const Type &delta) {
  if (y <= Constants<Type>::eps_prob || y >= Type(1.0)-Constants<Type>::eps_prob) {
    return -Constants<Type>::inf_repl;
  }
  if (gamma <= Constants<Type>::eps_pos ||
      delta <= Constants<Type>::eps_pos) {
    return -Constants<Type>::inf_repl;
  }

  // Compute log of beta function B(gamma, delta+1)
  Type log_beta_func = lgamma(gamma) + lgamma(delta + Type(1.0)) - lgamma(gamma + delta + Type(1.0));

  // Compute log-pdf according to the article's parameterization
  // f(x; γ, δ) = [x^(γ-1) * (1-x)^δ] / B(γ, δ+1)
  Type log_y = safeLog(y);
  Type log_1my = safeLog(Type(1.0) - y);

  Type logf = (gamma - Type(1.0)) * log_y + delta * log_1my - log_beta_func;

  if (!std::isfinite(asDouble(logf))) {
    return -Constants<Type>::inf_repl;
  }
  return logf;
}

// ========================
// Calculate CDF of Beta distribution
// ========================
template <class Type>
Type cdf_beta(const Type &y,
              const Type &gamma,
              const Type &delta) {
  if (y <= Constants<Type>::eps_prob) {
    return Type(0.0);
  }
  if (y >= Type(1.0) - Constants<Type>::eps_prob) {
    return Type(1.0);
  }

  // Use the regularized incomplete beta function
  // Note that we use delta+1 as per the paper's parameterization
  return pbeta(y, gamma, delta + Type(1.0));
}

// ========================
// Custom hash for vector<int>
// ========================
struct VectorIntHash {
  std::size_t operator()(const std::vector<int> &key) const {
    // Simple polynomial rolling hash
    std::size_t h = 0;
    for (auto &x : key) {
      h = 31*h + std::hash<int>()(x);
    }
    return h;
  }
};

struct VectorIntEq {
  bool operator()(const std::vector<int> &a, const std::vector<int> &b) const {
    if (a.size() != b.size()) return false;
    for (size_t i=0; i<a.size(); i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }
};

// ========================
// make_cache_key
// ========================
template <class Type>
std::vector<int> make_cache_key(const Type &gamma,
                                const Type &delta) {
  std::vector<int> key(2);
  key[0] = static_cast<int>(asDouble(gamma) * 100);
  key[1] = static_cast<int>(asDouble(delta) * 100);
  return key;
}

// ========================
// Exact mean for Beta
// ========================
template <class Type>
Type exact_mean_beta(const Type &gamma, const Type &delta) {
  // E(X) = γ/(γ+δ+1) for our parameterization
  return gamma / (gamma + delta + Type(1.0));
}

// ========================
// Process observation block helper function
// ========================
template <class Type>
void process_observation_block(
    Type& nll,
    vector<Type>& fitted,
    std::unordered_map<std::vector<int>, double, VectorIntHash, VectorIntEq>& meanCache,
    const vector<Type>& y,
    const vector<Type>& gammaVec,
    const vector<Type>& deltaVec,
    int start_idx,
    int end_idx,
    int calcFitted,
    int useMeanCache) {

  for (int i = start_idx; i < end_idx; i++) {
    Type gamma_i = gammaVec(i);
    Type delta_i = deltaVec(i);

    // Parameter validation: they must be positive
    if (gamma_i < Constants<Type>::eps_pos ||
        delta_i < Constants<Type>::eps_pos) {
      nll += Constants<Type>::inf_repl;
      continue;
    }

    // Compute log-likelihood for this observation
    Type logf = log_pdf_beta(y(i), gamma_i, delta_i);
    if (!std::isfinite(asDouble(logf))) {
      nll += Constants<Type>::inf_repl;
    } else {
      nll -= logf;
    }

    // If calcFitted==1, compute the mean value
    if (calcFitted == 1) {
      // For Beta, we use the exact formula for the mean
      fitted(i) = exact_mean_beta(gamma_i, delta_i);

      // Alternative approach with caching
      if (useMeanCache == 1) {
        std::vector<int> key = make_cache_key(gamma_i, delta_i);

        auto it = meanCache.find(key);
        if (it != meanCache.end()) {
          fitted(i) = Type(it->second);
        } else {
          Type mval = exact_mean_beta(gamma_i, delta_i);
          meanCache[key] = asDouble(mval);
          fitted(i) = mval;
        }
      }
    }
  }
}

// ========================
// Sequential chunk processing
// ========================
template <class Type>
void process_observations_in_chunks(
    Type& nll,
    vector<Type>& fitted,
    std::unordered_map<std::vector<int>, double, VectorIntHash, VectorIntEq>& meanCache,
    const vector<Type>& y,
    const vector<Type>& gammaVec,
    const vector<Type>& deltaVec,
    int n,
    int calcFitted,
    int useMeanCache,
    int userChunkSize) {

  // Default chunk size - this emulates the dynamic scheduling pattern
  int chunkSize = (userChunkSize > 0) ? userChunkSize : std::max(1, n / 20);

  for (int start = 0; start < n; start += chunkSize) {
    int end = std::min(start + chunkSize, n);
    process_observation_block(
      nll, fitted, meanCache, y,
      gammaVec, deltaVec,
      start, end, calcFitted, useMeanCache
    );
  }
}

// ========================
// objective_function
// ========================
template<class Type>
Type objective_function<Type>::operator() () {

  // == 1) DATA ==
  DATA_VECTOR(y);
  DATA_MATRIX(X1);
  DATA_MATRIX(X2);

  DATA_INTEGER(link_type1);
  DATA_INTEGER(link_type2);

  DATA_SCALAR(scale1);
  DATA_SCALAR(scale2);

  // Optional performance control parameters
  DATA_INTEGER(useMeanCache);  // 0 or 1
  DATA_INTEGER(calcFitted);    // 0 or 1
  DATA_INTEGER(userChunkSize); // Chunk size for scheduling

  // == 2) PARAMETERS ==
  PARAMETER_VECTOR(beta1);
  PARAMETER_VECTOR(beta2);

  // == 3) VALIDATION ==
  int n = y.size();
  if (n <= 0) return Constants<Type>::inf_repl;
  // Dimensions
  if ((X1.rows()!=n) || (X2.rows()!=n)) {
    return Constants<Type>::inf_repl;
  }
  if ((X1.cols()!=beta1.size()) || (X2.cols()!=beta2.size())) {
    return Constants<Type>::inf_repl;
  }

  // == 4) Linear predictors and transformed parameters ==
  vector<Type> eta1 = X1 * beta1;
  vector<Type> eta2 = X2 * beta2;

  // Allocate for parameters
  vector<Type> gammaVec(n), deltaVec(n);

  // Apply link functions
  for (int i=0; i<n; i++) {
    gammaVec(i) = apply_positive_link(eta1(i), link_type1, scale1);
    deltaVec(i) = apply_positive_link(eta2(i), link_type2, scale2);
  }

  // == Fitted vector? ==
  vector<Type> fitted;
  if (calcFitted == 1) {
    fitted.resize(n);
  }

  // == Mean caching using unordered_map ==
  typedef std::unordered_map<std::vector<int>, double, VectorIntHash, VectorIntEq> MeanMap;
  MeanMap meanCache;
  meanCache.reserve(std::min(n, 10000)); // heuristic

  // == 5) NLL via sequential processing with chunks ==
  Type nll(0.0);

  // If Type is double, we can use specific optimizations
  // (otherwise, TMB is in autodiff mode)
  if (isDouble<Type>::value) {
    process_observations_in_chunks(
      nll, fitted, meanCache, y,
      gammaVec, deltaVec,
      n, calcFitted, useMeanCache, userChunkSize
    );
  } else {
    // Autodiff mode: simple sequential loop
    for (int i=0; i<n; i++) {
      Type gamma_i = gammaVec(i);
      Type delta_i = deltaVec(i);

      if (gamma_i < Constants<Type>::eps_pos ||
          delta_i < Constants<Type>::eps_pos) {
        nll += Constants<Type>::inf_repl;
        continue;
      }

      Type logf = log_pdf_beta(y(i), gamma_i, delta_i);
      if (!std::isfinite(asDouble(logf))) {
        nll += Constants<Type>::inf_repl;
      } else {
        nll -= logf;
      }

      // If calcFitted==1, compute fitted with exact formula
      if (calcFitted == 1) {
        fitted(i) = exact_mean_beta(gamma_i, delta_i);
      }
    }
  }

  // == 6) Metrics ==
  int k = beta1.size() + beta2.size();
  Type deviance = Type(2.0)*nll;
  Type aic      = deviance + Type(2.0)*Type(k);
  Type bic      = deviance + Type(k)*log(Type(n));

  // == 7) REPORT ==
  ADREPORT(beta1);
  ADREPORT(beta2);

  // Calculate parameter means
  Type gamma_mean = gammaVec.sum()/Type(n);
  Type delta_mean = deltaVec.sum()/Type(n);

  // Fixed parameters for the specialized Beta (from page 4 of the paper)
  Type alpha_mean  = Type(1.0);
  Type beta_mean   = Type(1.0);
  Type lambda_mean = Type(1.0);

  REPORT(gamma_mean);
  REPORT(delta_mean);
  REPORT(alpha_mean);  // Report fixed α=1 for consistency
  REPORT(beta_mean);   // Report fixed β=1 for consistency
  REPORT(lambda_mean); // Report fixed λ=1 for consistency

  REPORT(nll);
  REPORT(deviance);
  REPORT(aic);
  REPORT(bic);

  REPORT(gammaVec);
  REPORT(deltaVec);

  if (calcFitted == 1) {
    REPORT(fitted);
  }

  // Cache size
  if (isDouble<Type>::value && (useMeanCache == 1)) {
    int cache_size = meanCache.size();
    REPORT(cache_size);
  }

  return nll;
}
