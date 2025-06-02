// File: ekwreg.cpp
// ---------------------------------------------------------------------
//  Regression Model for the Exponentiated Kumaraswamy (EKw) Distribution
//
//  This is a three-parameter special case of the Generalized Kumaraswamy distribution
//  with γ = 1 and δ = 0 fixed. The probability density function is:
//
//  f(x; α, β, λ) = λαβx^(α-1)(1-x^α)^(β-1)[1-(1-x^α)^β]^(λ-1)
//
//  where 0 < x < 1, and α, β, λ > 0.
//
//  The cumulative distribution function has a simple closed form:
//  F(x; α, β, λ) = [1-(1-x^α)^β]^λ
//
//  This distribution generalizes the Kumaraswamy distribution and allows
//  for additional flexibility in modeling bounded data.
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
// EKw log-PDF - Specialized for Exponentiated Kumaraswamy with γ=1 and δ=0 fixed
// ========================
template <class Type>
Type log_pdf_ekw(const Type &y,
                 const Type &alpha,
                 const Type &beta,
                 const Type &lambda) {
  if (y <= Constants<Type>::eps_prob || y >= Type(1.0)-Constants<Type>::eps_prob) {
    return -Constants<Type>::inf_repl;
  }
  if (alpha <= Constants<Type>::eps_pos ||
      beta  <= Constants<Type>::eps_pos ||
      lambda <= Constants<Type>::eps_pos) {
    return -Constants<Type>::inf_repl;
  }

  // Precompute components for efficiency and numerical stability
  Type log_alpha = safeLog(alpha);
  Type log_beta = safeLog(beta);
  Type log_lambda = safeLog(lambda);

  // Term 1: log(λαβ)
  Type term1 = log_lambda + log_alpha + log_beta;

  // Transform: y_alpha = y^α
  Type ya = safePow(y, alpha);

  // Transform: one_minus_ya = (1 - y^α)
  Type one_minus_ya = enforceProbability(Type(1.0) - ya);
  Type log_one_minus_ya = safeLog(one_minus_ya);

  // Term 2: (α-1)log(y)
  Type term2 = (alpha - Type(1.0)) * safeLog(y);

  // Term 3: (β-1)log(1-y^α)
  Type term3 = (beta - Type(1.0)) * log_one_minus_ya;

  // Transform: v = 1-(1-y^α)^β
  Type one_minus_ya_beta = safePow(one_minus_ya, beta);
  Type v = enforceProbability(Type(1.0) - one_minus_ya_beta);

  // Term 4: (λ-1)log(v)
  Type term4 = (lambda - Type(1.0)) * safeLog(v);

  // Combine all terms to get log-likelihood
  Type logf = term1 + term2 + term3 + term4;

  if (!std::isfinite(asDouble(logf))) {
    return -Constants<Type>::inf_repl;
  }
  return logf;
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
std::vector<int> make_cache_key(const Type &alpha,
                                const Type &beta,
                                const Type &lambda) {
  std::vector<int> key(3);
  key[0] = static_cast<int>(asDouble(alpha) * 100);
  key[1] = static_cast<int>(asDouble(beta)  * 100);
  key[2] = static_cast<int>(asDouble(lambda) * 100);
  return key;
}

// ========================
// Numeric approximation of mean (quadrature 30 pts)
// ========================
template <class Type>
Type calc_mean_ekw(const Type &alpha,
                   const Type &beta,
                   const Type &lambda) {
  const int n_points = 30;

  static const Type points[30] = {
    Type(0.0052995325041789), Type(0.0277124884633837), Type(0.0671843988060841),
    Type(0.1222977958224985), Type(0.1910618777986781), Type(0.2709916111713514),
    Type(0.3591982246103705), Type(0.4524937450811611), Type(0.5475062549188389),
    Type(0.6408017753896295), Type(0.7290083888286486), Type(0.8089381222013219),
    Type(0.8777022041775015), Type(0.9328156011939159), Type(0.9722875115366163),
    Type(0.9947004674958211), Type(0.0016634282895682), Type(0.0088218260005356),
    Type(0.0216951734782546), Type(0.0401505773180499), Type(0.0640198684962854),
    Type(0.0929719876996177), Type(0.1266873881927669), Type(0.1648092571058728),
    Type(0.2069463985939003), Type(0.2526493772311021), Type(0.3014937918994291),
    Type(0.3529709288365058), Type(0.4065775351876358), Type(0.4618179845446256)
  };

  static const Type weights[30] = {
    Type(0.0135762297058770), Type(0.0311267619693239), Type(0.0475792558412463),
    Type(0.0623144856277781), Type(0.0747979944082893), Type(0.0845782596975012),
    Type(0.0913017075224617), Type(0.0947253052275342), Type(0.0947253052275342),
    Type(0.0913017075224617), Type(0.0845782596975012), Type(0.0747979944082893),
    Type(0.0623144856277781), Type(0.0475792558412463), Type(0.0311267619693239),
    Type(0.0135762297058770), Type(0.0042582355019693), Type(0.0098975679009239),
    Type(0.0153793884993804), Type(0.0207860520784162), Type(0.0260583032078977),
    Type(0.0311490754242281), Type(0.0360154830389962), Type(0.0406283004740704),
    Type(0.0449535797324026), Type(0.0489611395857007), Type(0.0526254269148138),
    Type(0.0559249517732422), Type(0.0588415244791467), Type(0.0613600687415760)
  };

  Type mean(0.0), total_weight(0.0);

  for (int i = 0; i < n_points; i++) {
    Type y_i = points[i];
    if (y_i <= Constants<Type>::eps_prob || y_i >= Type(1.0)-Constants<Type>::eps_prob) {
      continue;
    }
    Type logpdf_val = log_pdf_ekw(y_i, alpha, beta, lambda);
    Type pdf_val    = (logpdf_val > -Constants<Type>::max_exp) ? safeExp(logpdf_val) : Type(0.0);
    mean         += y_i * weights[i] * pdf_val;
    total_weight += weights[i]       * pdf_val;
  }

  if (total_weight > Constants<Type>::eps_pos) {
    mean /= total_weight;
  }
  return enforceProbability(mean);
}

// ========================
// Calculate CDF of EKw distribution
// ========================
template <class Type>
Type cdf_ekw(const Type &y,
             const Type &alpha,
             const Type &beta,
             const Type &lambda) {
  if (y <= Constants<Type>::eps_prob) {
    return Type(0.0);
  }
  if (y >= Type(1.0) - Constants<Type>::eps_prob) {
    return Type(1.0);
  }

  // F(x; α, β, λ) = [1-(1-x^α)^β]^λ

  // Step 1: y^α
  Type ya = safePow(y, alpha);

  // Step 2: (1-y^α)
  Type one_minus_ya = Type(1.0) - ya;

  // Step 3: (1-y^α)^β
  Type one_minus_ya_beta = safePow(one_minus_ya, beta);

  // Step 4: 1-(1-y^α)^β
  Type v = Type(1.0) - one_minus_ya_beta;

  // Step 5: [1-(1-y^α)^β]^λ - Final CDF value
  return safePow(v, lambda);
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
    const vector<Type>& alphaVec,
    const vector<Type>& betaVec,
    const vector<Type>& lambdaVec,
    int start_idx,
    int end_idx,
    int calcFitted,
    int useMeanCache) {

  for (int i = start_idx; i < end_idx; i++) {
    Type alpha_i  = alphaVec(i);
    Type beta_i   = betaVec(i);
    Type lambda_i = lambdaVec(i);

    // Parameter validation: they must be positive
    if (alpha_i < Constants<Type>::eps_pos ||
        beta_i < Constants<Type>::eps_pos ||
        lambda_i < Constants<Type>::eps_pos) {
      nll += Constants<Type>::inf_repl;
      continue;
    }

    // Compute log-likelihood for this observation
    Type logf = log_pdf_ekw(y(i), alpha_i, beta_i, lambda_i);
    if (!std::isfinite(asDouble(logf))) {
      nll += Constants<Type>::inf_repl;
    } else {
      nll -= logf;
    }

    // If calcFitted==1, compute the mean value
    if (calcFitted == 1) {
      // Check if we should use caching (useMeanCache==1)
      if (useMeanCache == 1) {
        std::vector<int> key = make_cache_key(alpha_i, beta_i, lambda_i);

        auto it = meanCache.find(key);
        if (it != meanCache.end()) {
          fitted(i) = Type(it->second);
        } else {
          Type mval = calc_mean_ekw(alpha_i, beta_i, lambda_i);
          meanCache[key] = asDouble(mval);
          fitted(i) = mval;
        }
      } else {
        // No cache, calculate directly
        fitted(i) = calc_mean_ekw(alpha_i, beta_i, lambda_i);
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
    const vector<Type>& alphaVec,
    const vector<Type>& betaVec,
    const vector<Type>& lambdaVec,
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
      alphaVec, betaVec, lambdaVec,
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
  DATA_MATRIX(X3);

  DATA_INTEGER(link_type1);
  DATA_INTEGER(link_type2);
  DATA_INTEGER(link_type3);

  DATA_SCALAR(scale1);
  DATA_SCALAR(scale2);
  DATA_SCALAR(scale3);

  // Optional performance control parameters
  DATA_INTEGER(useMeanCache);  // 0 or 1
  DATA_INTEGER(calcFitted);    // 0 or 1
  DATA_INTEGER(userChunkSize); // Chunk size for scheduling

  // == 2) PARAMETERS ==
  PARAMETER_VECTOR(beta1);
  PARAMETER_VECTOR(beta2);
  PARAMETER_VECTOR(beta3);

  // == 3) VALIDATION ==
  int n = y.size();
  if (n <= 0) return Constants<Type>::inf_repl;
  // Dimensions
  if ((X1.rows()!=n) || (X2.rows()!=n) || (X3.rows()!=n)) {
    return Constants<Type>::inf_repl;
  }
  if ((X1.cols()!=beta1.size()) || (X2.cols()!=beta2.size()) ||
      (X3.cols()!=beta3.size())) {
    return Constants<Type>::inf_repl;
  }

  // == 4) Linear predictors and transformed parameters ==
  vector<Type> eta1 = X1 * beta1;
  vector<Type> eta2 = X2 * beta2;
  vector<Type> eta3 = X3 * beta3;

  // Allocate for parameters
  vector<Type> alphaVec(n), betaVec(n), lambdaVec(n);

  // Apply link functions
  for (int i=0; i<n; i++) {
    alphaVec(i)  = apply_positive_link(eta1(i), link_type1, scale1);
    betaVec(i)   = apply_positive_link(eta2(i), link_type2, scale2);
    lambdaVec(i) = apply_positive_link(eta3(i), link_type3, scale3);
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
      alphaVec, betaVec, lambdaVec,
      n, calcFitted, useMeanCache, userChunkSize
    );
  } else {
    // Autodiff mode: simple sequential loop
    for (int i=0; i<n; i++) {
      Type alpha_i  = alphaVec(i);
      Type beta_i   = betaVec(i);
      Type lambda_i = lambdaVec(i);

      if (alpha_i < Constants<Type>::eps_pos ||
          beta_i < Constants<Type>::eps_pos ||
          lambda_i < Constants<Type>::eps_pos) {
        nll += Constants<Type>::inf_repl;
        continue;
      }

      Type logf = log_pdf_ekw(y(i), alpha_i, beta_i, lambda_i);
      if (!std::isfinite(asDouble(logf))) {
        nll += Constants<Type>::inf_repl;
      } else {
        nll -= logf;
      }

      // If calcFitted==1, compute fitted without cache in autodiff mode
      if (calcFitted == 1) {
        fitted(i) = calc_mean_ekw(alpha_i, beta_i, lambda_i);
      }
    }
  }

  // == 6) Metrics ==
  int k = beta1.size() + beta2.size() + beta3.size();
  Type deviance = Type(2.0)*nll;
  Type aic      = deviance + Type(2.0)*Type(k);
  Type bic      = deviance + Type(k)*log(Type(n));

  // == 7) REPORT ==
  ADREPORT(beta1);
  ADREPORT(beta2);
  ADREPORT(beta3);

  // Calculate parameter means
  Type alpha_mean  = alphaVec.sum()/Type(n);
  Type beta_mean   = betaVec.sum()/Type(n);
  Type lambda_mean = lambdaVec.sum()/Type(n);
  // For EKw, γ is fixed at 1.0 and δ is fixed at 0.0
  Type gamma_mean  = Type(1.0);
  Type delta_mean  = Type(0.0);

  REPORT(alpha_mean);
  REPORT(beta_mean);
  REPORT(gamma_mean);  // Report fixed γ=1 for consistency
  REPORT(delta_mean);  // Report fixed δ=0 for consistency
  REPORT(lambda_mean);

  REPORT(nll);
  REPORT(deviance);
  REPORT(aic);
  REPORT(bic);

  REPORT(alphaVec);
  REPORT(betaVec);
  REPORT(lambdaVec);

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
