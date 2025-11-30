// File: bkwreg.cpp
// ---------------------------------------------------------------------
//  Regression Model for the Beta-Kumaraswamy (BKw) Distribution
//
//  This is a four-parameter special case of the Generalized Kumaraswamy distribution
//  with λ = 1 fixed. The probability density function is:
//
//  f(x; α, β, γ, δ) = (αβ/B(γ, δ+1)) x^(α-1) (1-x^α)^(β(δ+1)-1) [1-(1-x^α)^β]^(γ-1)
//
//  where 0 < x < 1, and α, β, γ, δ > 0, and B(γ, δ+1) is the beta function.
// ---------------------------------------------------------------------

#include <TMB.hpp>
#include <unordered_map>

// ========================
// Numeric constants - MINIMAL SET
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
// Safe numerical operations - INLINE, NO CondExp
// ========================
template <class Type>
inline Type safeLog(const Type &x) {
  return (x <= Constants<Type>::eps_pos) ? 
  -Constants<Type>::inf_repl : log(x);
}

template <class Type>
inline Type safeExp(const Type &x) {
  double x_d = asDouble(x);
  if (x_d >  30.0) return exp(Type(30.0));
  if (x_d < -30.0) return exp(Type(-30.0));
  return exp(x);
}

// ========================
// Log Beta function - OPTIMIZED
// ========================
template <class Type>
inline Type logBeta(const Type &a, const Type &b) {
  double a_d = asDouble(a);
  double b_d = asDouble(b);
  
  // Quick validation
  if (a_d <= 1e-10 || b_d <= 1e-10) {
    return -Constants<Type>::inf_repl;
  }
  
  // For large a,b, use Stirling's approximation (more stable)
  if (a_d > 100.0 && b_d > 100.0) {
    return Type(0.5) * (log(Type(2.0 * M_PI)) - log(a + b))
    + (a - Type(0.5)) * log(a)
    + (b - Type(0.5)) * log(b)
    - (a + b - Type(1.0)) * log(a + b);
  }
  
  // Standard formula: log[B(a,b)] = lgamma(a) + lgamma(b) - lgamma(a+b)
  return lgamma(a) + lgamma(b) - lgamma(a + b);
}

// ========================
// Link functions - SIMPLIFIED, INLINE, NO CondExp
// ========================
template <class Type>
inline Type apply_positive_link(const Type &eta, const int link_type, 
                                const Type &scale_factor) {
  Type result;
  Type min_val = Constants<Type>::eps_pos;
  
  switch (link_type) {
  case 1: // log
    result = exp(eta);
    break;
  case 2: // logit
    result = scale_factor / (Type(1.0) + exp(-eta));
    break;
  case 3: // probit
    result = scale_factor * pnorm(eta);
    break;
  case 4: // cauchy
    result = scale_factor * (Type(0.5) + atan(eta) / Type(M_PI));
    break;
  case 5: // cloglog
    result = scale_factor * (Type(1.0) - exp(-exp(eta)));
    break;
  case 6: // identity
    result = eta;
    break;
  case 7: // sqrt
    result = (asDouble(eta) > 0.0) ? eta * eta : Type(0.0);
    break;
  case 8: // inverse
    result = Type(1.0) / (eta + Type(1e-6));
    break;
  case 9: // inverse-square
    result = Type(1.0) / sqrt(eta + Type(1e-6));
    break;
  default:
    result = exp(eta);
  }
  
  return (asDouble(result) < asDouble(min_val)) ? min_val : result;
}

// ========================
// BKw log-PDF - CORE FUNCTION, OPTIMIZED
// Uses NATIVE pow(), no CondExp overhead
// ========================
template <class Type>
inline Type log_pdf_bkw(const Type &y, const Type &alpha, const Type &beta, 
                        const Type &gamma, const Type &delta) {
  // Quick validation using asDouble (fast in double mode)
  double y_d = asDouble(y);
  if (y_d <= 1e-12 || y_d >= 0.999999) return -Constants<Type>::inf_repl;
  
  double alpha_d = asDouble(alpha);
  double beta_d = asDouble(beta);
  double gamma_d = asDouble(gamma);
  double delta_d = asDouble(delta);
  
  if (alpha_d <= 1e-10 || beta_d <= 1e-10 || gamma_d <= 1e-10 || delta_d <= 1e-10) {
    return -Constants<Type>::inf_repl;
  }
  
  // Compute log-beta function: log[B(γ, δ+1)]
  Type logB_val = logBeta(gamma, delta + Type(1.0));
  
  // Check if logBeta is valid
  if (asDouble(logB_val) < -1e9) {
    return -Constants<Type>::inf_repl;
  }
  
  // Precompute logarithms
  Type log_alpha = log(alpha);
  Type log_beta = log(beta);
  
  // Term 1: log(αβ) - log[B(γ, δ+1)]
  Type term1 = log_alpha + log_beta - logB_val;
  
  // Term 2: (α-1)log(y)
  Type term2 = (alpha - Type(1.0)) * log(y);
  
  // Transform: ya = y^α (using NATIVE pow)
  Type ya = pow(y, alpha);
  
  // Transform: one_minus_ya = (1 - y^α)
  Type one_minus_ya = Type(1.0) - ya;
  
  // Safety check
  if (asDouble(one_minus_ya) <= 1e-12) {
    return -Constants<Type>::inf_repl;
  }
  
  Type log_one_minus_ya = log(one_minus_ya);
  
  // Term 3: (β(δ+1)-1)log(1-y^α)
  Type term3 = (beta * (delta + Type(1.0)) - Type(1.0)) * log_one_minus_ya;
  
  // Transform: one_minus_ya_beta = (1-y^α)^β
  Type one_minus_ya_beta = pow(one_minus_ya, beta);
  
  // Transform: v = 1 - (1-y^α)^β
  Type v = Type(1.0) - one_minus_ya_beta;
  
  // Safety check
  if (asDouble(v) <= 1e-12) {
    return -Constants<Type>::inf_repl;
  }
  
  // Term 4: (γ-1)log[v]
  Type term4 = (gamma - Type(1.0)) * log(v);
  
  return term1 + term2 + term3 + term4;
}

// ========================
// Cache structures (for compatibility)
// ========================
struct VectorIntHash {
  std::size_t operator()(const std::vector<int> &key) const {
    std::size_t h = 0;
    for (auto &x : key) h = 31*h + std::hash<int>()(x);
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

template <class Type>
inline std::vector<int> make_cache_key(const Type &alpha, const Type &beta, 
                                       const Type &gamma, const Type &delta) {
  std::vector<int> key(4);
  key[0] = static_cast<int>(asDouble(alpha) * 100);
  key[1] = static_cast<int>(asDouble(beta) * 100);
  key[2] = static_cast<int>(asDouble(gamma) * 100);
  key[3] = static_cast<int>(asDouble(delta) * 100);
  return key;
}

// ========================
// Numeric approximation of mean (quadrature 30 pts) - OPTIMIZED
// ========================
template <class Type>
inline Type calc_mean_bkw(const Type &alpha, const Type &beta, 
                          const Type &gamma, const Type &delta) {
  const int n_points = 30;
  
  static const double points_data[30] = {
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
  
  static const double weights_data[30] = {
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
  
  Type mean = Type(0.0);
  Type total_weight = Type(0.0);
  
  for (int i = 0; i < n_points; i++) {
    Type y_i = Type(points_data[i]);
    Type w_i = Type(weights_data[i]);
    
    Type logpdf_val = log_pdf_bkw(y_i, alpha, beta, gamma, delta);
    if (asDouble(logpdf_val) > -30.0) {
      Type pdf_val = exp(logpdf_val);
      mean += y_i * w_i * pdf_val;
      total_weight += w_i * pdf_val;
    }
  }
  
  if (asDouble(total_weight) > 1e-10) {
    mean /= total_weight;
  }
  
  // Clamp to (0,1)
  double mean_d = asDouble(mean);
  if (mean_d < 0.0001) mean = Type(0.0001);
  if (mean_d > 0.9999) mean = Type(0.9999);
  
  return mean;
}

// ========================
// objective_function
// ========================
template<class Type>
Type objective_function<Type>::operator() () {
  
  // == DATA ==
  DATA_VECTOR(y);
  DATA_MATRIX(X1);
  DATA_MATRIX(X2);
  DATA_MATRIX(X3);
  DATA_MATRIX(X4);
  
  DATA_INTEGER(link_type1);
  DATA_INTEGER(link_type2);
  DATA_INTEGER(link_type3);
  DATA_INTEGER(link_type4);
  
  DATA_SCALAR(scale1);
  DATA_SCALAR(scale2);
  DATA_SCALAR(scale3);
  DATA_SCALAR(scale4);
  
  DATA_INTEGER(useMeanCache);  // For compatibility
  DATA_INTEGER(calcFitted);
  DATA_INTEGER(userChunkSize);  // For compatibility
  (void) userChunkSize;
  
  // == PARAMETERS ==
  PARAMETER_VECTOR(beta1);
  PARAMETER_VECTOR(beta2);
  PARAMETER_VECTOR(beta3);
  PARAMETER_VECTOR(beta4);
  
  // == VALIDATION ==
  int n = y.size();
  if (n <= 0) return Constants<Type>::inf_repl;
  
  if ((X1.rows() != n) || (X2.rows() != n) || (X3.rows() != n) || (X4.rows() != n)) {
    return Constants<Type>::inf_repl;
  }
  if ((X1.cols() != beta1.size()) || (X2.cols() != beta2.size()) || 
      (X3.cols() != beta3.size()) || (X4.cols() != beta4.size())) {
    return Constants<Type>::inf_repl;
  }
  
  // == LINEAR PREDICTORS ==
  vector<Type> eta1 = X1 * beta1;
  vector<Type> eta2 = X2 * beta2;
  vector<Type> eta3 = X3 * beta3;
  vector<Type> eta4 = X4 * beta4;
  
  // == PARAMETER VECTORS ==
  vector<Type> alphaVec(n);
  vector<Type> betaVec(n);
  vector<Type> gammaVec(n);
  vector<Type> deltaVec(n);
  
  // Apply link functions ONCE
  for (int i = 0; i < n; i++) {
    alphaVec(i) = apply_positive_link(eta1(i), link_type1, scale1);
    betaVec(i) = apply_positive_link(eta2(i), link_type2, scale2);
    gammaVec(i) = apply_positive_link(eta3(i), link_type3, scale3);
    deltaVec(i) = apply_positive_link(eta4(i), link_type4, scale4);
  }
  
  // == FITTED VECTOR ==
  vector<Type> fitted(n);
  if (calcFitted == 1) {
    fitted.setZero();
  }
  
  // == CACHE DECLARATION ==
  typedef std::unordered_map<std::vector<int>, double, VectorIntHash, VectorIntEq> MeanMap;
  MeanMap meanCache;
  if (useMeanCache == 1 && calcFitted == 1) {
    meanCache.reserve(std::min(n, 10000));
  }
  
  // == NLL CALCULATION ==
  Type nll = Type(0.0);
  
  // ========================
  // KEY OPTIMIZATION: Branch on isDouble<Type>::value IS ALLOWED!
  // This is a COMPILE-TIME branch, not runtime
  // ========================
  if (isDouble<Type>::value) {
    // DOUBLE MODE - optimized path
    for (int i = 0; i < n; i++) {
      Type alpha_i = alphaVec(i);
      Type beta_i = betaVec(i);
      Type gamma_i = gammaVec(i);
      Type delta_i = deltaVec(i);
      
      // Compute log-likelihood
      Type logf = log_pdf_bkw(y(i), alpha_i, beta_i, gamma_i, delta_i);
      nll -= logf;
      
      // Compute fitted using numerical integration
      if (calcFitted == 1) {
        if (useMeanCache == 1) {
          std::vector<int> key = make_cache_key(alpha_i, beta_i, gamma_i, delta_i);
          
          auto it = meanCache.find(key);
          if (it != meanCache.end()) {
            fitted(i) = Type(it->second);
          } else {
            Type mval = calc_mean_bkw(alpha_i, beta_i, gamma_i, delta_i);
            meanCache[key] = asDouble(mval);
            fitted(i) = mval;
          }
        } else {
          fitted(i) = calc_mean_bkw(alpha_i, beta_i, gamma_i, delta_i);
        }
      }
    }
  } else {
    // AD MODE - still optimized
    for (int i = 0; i < n; i++) {
      Type alpha_i = alphaVec(i);
      Type beta_i = betaVec(i);
      Type gamma_i = gammaVec(i);
      Type delta_i = deltaVec(i);
      
      // Compute log-likelihood
      Type logf = log_pdf_bkw(y(i), alpha_i, beta_i, gamma_i, delta_i);
      nll -= logf;
      
      // Compute fitted using numerical integration
      if (calcFitted == 1) {
        fitted(i) = calc_mean_bkw(alpha_i, beta_i, gamma_i, delta_i);
      }
    }
  }
  
  // == METRICS ==
  int k = beta1.size() + beta2.size() + beta3.size() + beta4.size();
  Type deviance = Type(2.0) * nll;
  Type aic = deviance + Type(2.0) * Type(k);
  Type bic = deviance + Type(k) * log(Type(n));
  
  // == PARAMETER MEANS ==
  Type alpha_mean = alphaVec.sum() / Type(n);
  Type beta_mean = betaVec.sum() / Type(n);
  Type gamma_mean = gammaVec.sum() / Type(n);
  Type delta_mean = deltaVec.sum() / Type(n);
  
  // == ADREPORT ==
  ADREPORT(beta1);
  ADREPORT(beta2);
  ADREPORT(beta3);
  ADREPORT(beta4);
  
  // == REPORT ==
  REPORT(alpha_mean);
  REPORT(beta_mean);
  REPORT(gamma_mean);
  REPORT(delta_mean);
  
  REPORT(nll);
  REPORT(deviance);
  REPORT(aic);
  REPORT(bic);
  
  REPORT(alphaVec);
  REPORT(betaVec);
  REPORT(gammaVec);
  REPORT(deltaVec);
  
  if (calcFitted == 1) {
    REPORT(fitted);
  }
  
  // == CACHE SIZE REPORT - FULL COMPATIBILITY ===
  if (isDouble<Type>::value && (useMeanCache == 1)) {
    int cache_size = static_cast<int>(meanCache.size());
    REPORT(cache_size);
  }
  
  return nll;
}
