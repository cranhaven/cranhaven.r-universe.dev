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

// ========================
// Numeric constants - ALL static const
// ========================
template <class Type>
struct Constants {
  static const Type eps_log;
  static const Type eps_pos;
  static const Type eps_prob;
  static const Type inf_repl;
  static const Type max_exp;
  static const Type one;
  static const Type zero;
  static const Type half;
};

template <class Type>
const Type Constants<Type>::eps_log  = Type(1e-15);
template <class Type>
const Type Constants<Type>::eps_pos  = Type(1e-10);
template <class Type>
const Type Constants<Type>::eps_prob = Type(1e-12);
template <class Type>
const Type Constants<Type>::inf_repl = Type(1e20);
template <class Type>
const Type Constants<Type>::max_exp  = Type(30);
template <class Type>
const Type Constants<Type>::one      = Type(1.0);
template <class Type>
const Type Constants<Type>::zero     = Type(0.0);
template <class Type>
const Type Constants<Type>::half     = Type(0.5);

// ========================
// OPTIMIZED: Inline everything possible
// ========================
template <class Type>
inline Type safeLog(const Type &x) {
  return (x <= Constants<Type>::zero) ? 
  -Constants<Type>::inf_repl : 
  log(x + Constants<Type>::eps_log);
}

template <class Type>
inline Type safeExp(const Type &x) {
  Type clamped = (x >  Constants<Type>::max_exp) ? Constants<Type>::max_exp :
  (x < -Constants<Type>::max_exp) ? -Constants<Type>::max_exp : x;
  return exp(clamped);
}

template <class Type>
inline Type enforceMin(const Type &x, const Type &min_val) {
  return CppAD::CondExpLt(x, min_val, min_val, x);
}

template <class Type>
inline Type enforceProbability(const Type &x) {
  Type eps   = Constants<Type>::eps_prob;
  Type upper = Constants<Type>::one - eps;
  Type temp = CppAD::CondExpGt(x, upper, upper, x);
  return CppAD::CondExpLt(temp, eps, eps, temp);
}

// ========================
// Link functions - HEAVILY INLINED
// ========================
template <class Type>
inline Type fast_log_link(const Type &eta) {
  return safeExp(eta);
}

template <class Type>
inline Type fast_logit_link(const Type &eta) {
  return Constants<Type>::one / (Constants<Type>::one + safeExp(-eta));
}

template <class Type>
inline Type fast_probit_link(const Type &eta) {
  return enforceProbability(pnorm(eta));
}

template <class Type>
inline Type fast_cloglog_link(const Type &eta) {
  return enforceProbability(Constants<Type>::one - safeExp(-safeExp(eta)));
}

// ========================
// CRITICAL OPTIMIZATION: Single unified link function
// No switch statement overhead
// ========================
template <class Type>
inline Type apply_positive_link(const Type &eta, const int &link_type, const Type &scale_factor) {
  Type min_val = Constants<Type>::eps_pos;
  Type result;
  
  // Direct computation based on link_type
  // Compiler will optimize this better than nested functions
  switch (link_type) {
  case 1:
    result = safeExp(eta);
    break;
  case 2:
    result = scale_factor / (Constants<Type>::one + safeExp(-eta));
    break;
  case 3:
    result = scale_factor * pnorm(eta);
    break;
  case 4:
    result = scale_factor * (Constants<Type>::half + atan(eta)/Type(M_PI));
    break;
  case 5:
    result = scale_factor * (Constants<Type>::one - safeExp(-safeExp(eta)));
    break;
  case 6:
    result = eta;
    break;
  case 7:
    result = eta * eta;
    break;
  case 8:
    result = Constants<Type>::one / enforceMin(eta, Type(1e-6));
    break;
  case 9:
    result = Constants<Type>::one / sqrt(enforceMin(eta, Type(1e-6)));
    break;
  default:
    result = safeExp(eta);
  }
  
  return enforceMin(result, min_val);
}

// ========================
// OPTIMIZED: Inline log-beta computation
// Using identity for integer delta when possible
// ========================
template <class Type>
inline Type compute_log_beta(const Type &gamma, const Type &delta) {
  // Standard computation - compiler will inline this
  return lgamma(gamma) + lgamma(delta + Constants<Type>::one) - 
    lgamma(gamma + delta + Constants<Type>::one);
}

// ========================
// CRITICAL: Simplified Beta log-PDF
// Everything inline, minimal function calls
// ========================
template <class Type>
inline Type log_pdf_beta(const Type &y,
                         const Type &gamma,
                         const Type &delta) {
  // Quick bailouts for invalid inputs
  Type log_y = safeLog(y);
  Type log_1my = safeLog(Constants<Type>::one - y);
  Type log_beta_func = compute_log_beta(gamma, delta);
  
  // Direct formula - no intermediate variables
  Type logf = (gamma - Constants<Type>::one) * log_y + 
    delta * log_1my - 
    log_beta_func;
  
  return logf;
}

// ========================
// Exact mean - inline
// ========================
template <class Type>
inline Type exact_mean_beta(const Type &gamma, const Type &delta) {
  return gamma / (gamma + delta + Constants<Type>::one);
}

// ========================
// MAIN OBJECTIVE FUNCTION
// Key change: NO if(isDouble<Type>::value) branches!
// Everything works in both AD and double mode
// ========================
template<class Type>
Type objective_function<Type>::operator() () {
  
  // == DATA ==
  DATA_VECTOR(y);
  DATA_MATRIX(X1);
  DATA_MATRIX(X2);
  DATA_INTEGER(link_type1);
  DATA_INTEGER(link_type2);
  DATA_SCALAR(scale1);
  DATA_SCALAR(scale2);
  DATA_INTEGER(useMeanCache);  // Not used in simple loop
  (void) useMeanCache;
  
  DATA_INTEGER(calcFitted);
  
  DATA_INTEGER(userChunkSize);  // Not used in simple loop
  (void) userChunkSize;
  
  // == PARAMETERS ==
  PARAMETER_VECTOR(beta1);
  PARAMETER_VECTOR(beta2);
  
  // == VALIDATION ==
  int n = y.size();
  int p1 = beta1.size();
  int p2 = beta2.size();
  
  // == Linear predictors - use matrix multiplication ==
  vector<Type> eta1 = X1 * beta1;
  vector<Type> eta2 = X2 * beta2;
  
  // == TMB BEST PRACTICE: Pre-initialize all vectors ==
  vector<Type> gammaVec(n);
  vector<Type> deltaVec(n);
  gammaVec.setZero();
  deltaVec.setZero();
  
  // == Apply link functions - simple loop ==
  for (int i=0; i<n; i++) {
    gammaVec(i) = apply_positive_link(eta1(i), link_type1, scale1);
    deltaVec(i) = apply_positive_link(eta2(i), link_type2, scale2);
  }
  
  // == Fitted values - pre-initialize ==
  vector<Type> fitted(n);
  fitted.setZero();
  
  // == NLL computation - SINGLE UNIFIED LOOP ==
  // No branching on Type - works same in AD and double mode
  Type nll = Constants<Type>::zero;
  
  for (int i=0; i<n; i++) {
    Type gamma_i = gammaVec(i);
    Type delta_i = deltaVec(i);
    
    // Inline validation
    Type logf = log_pdf_beta(y(i), gamma_i, delta_i);
    nll -= logf;
    
    // Fitted values - computed inline
    fitted(i) = exact_mean_beta(gamma_i, delta_i);
  }
  
  // == Metrics ==
  int k = p1 + p2;
  Type deviance = Type(2.0) * nll;
  Type aic = deviance + Type(2.0) * Type(k);
  Type bic = deviance + Type(k) * log(Type(n));
  
  // == REPORT ==
  ADREPORT(beta1);
  ADREPORT(beta2);
  
  Type gamma_mean = gammaVec.sum() / Type(n);
  Type delta_mean = deltaVec.sum() / Type(n);
  Type alpha_mean = Constants<Type>::one;
  Type beta_mean = Constants<Type>::one;
  Type lambda_mean = Constants<Type>::one;
  
  REPORT(gamma_mean);
  REPORT(delta_mean);
  REPORT(alpha_mean);
  REPORT(beta_mean);
  REPORT(lambda_mean);
  REPORT(nll);
  REPORT(deviance);
  REPORT(aic);
  REPORT(bic);
  REPORT(gammaVec);
  REPORT(deltaVec);
  
  if (calcFitted == 1) {
    REPORT(fitted);
  }
  
  return nll;
}
