#include <TMB.hpp>

// Helper function to ensure a lower bound is met
template<class Type>
Type safe_lower(const Type & value, const Type & lower) {
  return CppAD::CondExpLt(value, lower, lower, value);
}

// Helper function to ensure a value does not exceed an upper bound
template<class Type>
Type safe_upper(const Type & value, const Type & upper) {
  return CppAD::CondExpGt(value, upper, upper, value);
}

// Helper function for stable computation of log1p for AD types
template<class Type>
Type safe_log1p(const Type& x) {
  return log(Type(1.0) + x);
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data: vector x (values in (0,1))
  DATA_VECTOR(x);
  // Family string, converted to integer for efficient in-template usage
  // 0="gkw", 1="bkw", 2="kkw", 3="ekw", 4="mc", 5="kw", 6="beta"
  DATA_INTEGER(family);

  int n = x.size();

  // Parameters on the log-scale
  PARAMETER(log_alpha);
  PARAMETER(log_beta);
  PARAMETER(log_gamma);
  PARAMETER(log_delta);
  PARAMETER(log_lambda);

  // Transform parameters to original scale
  Type alpha  = exp(log_alpha);
  Type beta   = exp(log_beta);
  Type gamma  = exp(log_gamma);
  Type delta  = exp(log_delta);
  Type lambda = exp(log_lambda);

  // Override parameters based on family
  // NOTE: We still keep the "fixed" parameters in the model for easier reporting
  // but their values will be explicitly set and not optimized due to the map in R.

  switch(family) {
  case 1: // BKw: λ = 1 fixed
    lambda = Type(1.0);
    break;
  case 2: // KKw: γ = 1 fixed
    gamma = Type(1.0);
    break;
  case 3: // EKw: γ = 1, δ = 0 fixed
    gamma = Type(1.0);
    delta = Type(0.0);
    break;
  case 4: // Mc: α = 1, β = 1 fixed
    alpha = Type(1.0);
    beta = Type(1.0);
    break;
  case 5: // Kw: γ = 1, δ = 0, λ = 1 fixed
    gamma = Type(1.0);
    delta = Type(0.0);
    lambda = Type(1.0);
    break;
  case 6: // Beta: α = 1, β = 1, λ = 1 fixed
    alpha = Type(1.0);
    beta = Type(1.0);
    lambda = Type(1.0);
    break;
  default: // case 0: GKw - all parameters are free
    break;
  }

  // Strict bounds to prevent numerical issues
  Type upper_bound = Type(20.0);
  alpha  = safe_upper(alpha, upper_bound);
  beta   = safe_upper(beta, upper_bound);
  gamma  = safe_upper(gamma, upper_bound);
  delta  = safe_upper(delta, upper_bound);
  lambda = safe_upper(lambda, upper_bound);

  Type lower_bound = Type(0.05);
  alpha  = safe_lower(alpha, lower_bound);
  beta   = safe_lower(beta, lower_bound);
  gamma  = safe_lower(gamma, lower_bound);

  // Delta can be zero in EKw family
  if (family != 3) { // Not EKw
    delta  = safe_lower(delta, lower_bound);
  } else {
    delta  = safe_lower(delta, Type(0.0));
  }

  lambda = safe_lower(lambda, lower_bound);

  // Tolerance for numerical safety
  Type eps = Type(1e-8);

  // Calculate the log Beta function:
  // \[
  // \log B(\gamma, \delta+1) = \log \Gamma(\gamma) + \log \Gamma(\delta+1) - \log \Gamma(\gamma+\delta+1)
  // \]
  Type log_beta_func = lgamma(gamma) + lgamma(delta + Type(1.0)) - lgamma(gamma + delta + Type(1.0));

  // Constant part of the log-likelihood:
  // \[
  // \ell_{\text{const}} = -n \log(\lambda \alpha \beta) + n \log B(\gamma, \delta+1)
  // \]
  // Note: We adjust this based on family
  Type const_nll;

  switch(family) {
  case 4: // Mc: α = β = 1
    const_nll = -n * log(lambda) + n * log_beta_func;
    break;
  case 5: // Kw: γ = 1, δ = 0, λ = 1
    const_nll = -n * (log(alpha) + log(beta));
    break;
  case 6: // Beta: α = β = λ = 1
    const_nll = n * log_beta_func;
    break;
  default: // Other families use the full form
    const_nll = -n * (log(lambda) + log(alpha) + log(beta)) + n * log_beta_func;
  break;
  }

  // Regularization penalty to ensure identifiability
  // Only apply to non-fixed parameters based on family
  Type reg_strength = Type(0.05);
  Type reg_penalty = Type(0.0);

  switch(family) {
  case 0: // GKw - all parameters regularized
    reg_penalty = reg_strength * (log_alpha*log_alpha + log_beta*log_beta +
      log_gamma*log_gamma + log_delta*log_delta +
      log_lambda*log_lambda);
    break;
  case 1: // BKw - all but lambda regularized
    reg_penalty = reg_strength * (log_alpha*log_alpha + log_beta*log_beta +
      log_gamma*log_gamma + log_delta*log_delta);
    break;
  case 2: // KKw - all but gamma regularized
    reg_penalty = reg_strength * (log_alpha*log_alpha + log_beta*log_beta +
      log_delta*log_delta + log_lambda*log_lambda);
    break;
  case 3: // EKw - alpha, beta, lambda regularized
    reg_penalty = reg_strength * (log_alpha*log_alpha + log_beta*log_beta +
      log_lambda*log_lambda);
    break;
  case 4: // Mc - gamma, delta, lambda regularized
    reg_penalty = reg_strength * (log_gamma*log_gamma + log_delta*log_delta +
      log_lambda*log_lambda);
    break;
  case 5: // Kw - only alpha, beta regularized
    reg_penalty = reg_strength * (log_alpha*log_alpha + log_beta*log_beta);
    break;
  case 6: // Beta - only gamma, delta regularized
    reg_penalty = reg_strength * (log_gamma*log_gamma + log_delta*log_delta);
    break;
  }

  const_nll += reg_penalty;

  // Initialize parallel accumulator with pointer to this objective function
  parallel_accumulator<Type> nll(this);
  // Add constant part to nll
  nll += const_nll;

  // Loop over observations with safe computations
  for(int i = 0; i < n; i++) {
    Type xi = x[i];

    // Check that xi is in the open interval (0,1)
    if(xi <= Type(0.0) || xi >= Type(1.0)) {
      return Type(INFINITY);
    }
    // Clamp xi to avoid boundary issues
    xi = safe_lower(xi, eps);
    xi = safe_upper(xi, Type(1.0) - eps);

    // Compute \(\log(x_i)\)
    Type log_xi = log(xi);

    // Compute \( x_{\alpha} = x_i^{\alpha} \) safely
    Type x_alpha = pow(xi, alpha);
    x_alpha = safe_upper(x_alpha, Type(1.0) - eps);

    // Compute \( 1 - x_i^{\alpha} \) safely and its logarithm using safe_log1p for stability
    Type one_minus_x_alpha = Type(1.0) - x_alpha;
    one_minus_x_alpha = safe_lower(one_minus_x_alpha, eps);
    Type log_one_minus_x_alpha = safe_log1p(-x_alpha); // equivalent to log(1 - x_alpha)

    // Compute \( (1 - x_i^{\alpha})^{\beta} \) safely
    Type one_minus_x_alpha_beta = pow(one_minus_x_alpha, beta);
    one_minus_x_alpha_beta = safe_lower(one_minus_x_alpha_beta, eps);
    one_minus_x_alpha_beta = safe_upper(one_minus_x_alpha_beta, Type(1.0) - eps);

    // Compute \( y = 1 - (1 - x_i^{\alpha})^{\beta} \) safely
    Type y = Type(1.0) - one_minus_x_alpha_beta;
    y = safe_lower(y, eps);
    y = safe_upper(y, Type(1.0) - eps);
    Type log_y = log(y);

    // Compute \( y^{\lambda} \) safely
    Type y_lambda = pow(y, lambda);
    y_lambda = safe_lower(y_lambda, eps);
    y_lambda = safe_upper(y_lambda, Type(1.0) - eps);

    // Compute \( 1 - y^{\lambda} \) safely and its logarithm using safe_log1p for stability
    Type one_minus_y_lambda = Type(1.0) - y_lambda;
    one_minus_y_lambda = safe_lower(one_minus_y_lambda, eps);
    Type log_one_minus_y_lambda = safe_log1p(-y_lambda); // equivalent to log(1 - y_lambda)

    // Observation-level negative log-likelihood components
    // We compute these differently based on family to avoid unnecessary computations

    switch(family) {
    case 4: // Mc: α = β = 1
      // For Mc, x_i^α = x_i, (1-x_i^α)^β = 1-x_i, and y = x_i
      // So the simplified negative log-likelihood is:
      nll -= (gamma * lambda - Type(1.0)) * log_xi;
      nll -= delta * log_one_minus_y_lambda;
      break;

    case 5: // Kw: γ = 1, δ = 0, λ = 1
      // For Kw, y = 1-(1-x_i^α)^β and y^λ = y
      // The simplified negative log-likelihood is:
      nll -= (alpha - Type(1.0)) * log_xi;
      nll -= (beta - Type(1.0)) * log_one_minus_x_alpha;
      break;

    case 6: // Beta: α = β = λ = 1
      // For Beta, we can use xi directly without transformations
      nll -= (gamma - Type(1.0)) * log_xi;
      nll -= delta * safe_log1p(-xi); // More stable than log(1-xi)
      break;

    default: // Other families use the full form
      // Observation-level negative log-likelihood:
      // \[
      // \ell_i = - (\alpha - 1) \log(x_i) - (\beta - 1) \log(1 - x_i^{\alpha})
      //         - (\gamma \lambda - 1) \log(y) - \delta \log(1 - y^{\lambda})
      // \]
      nll -= (alpha - Type(1.0)) * log_xi;
    nll -= (beta - Type(1.0)) * log_one_minus_x_alpha;
    nll -= (gamma * lambda - Type(1.0)) * log_y;
    nll -= delta * log_one_minus_y_lambda;
    break;
    }
  }

  // Final check to ensure nll is finite
  if(!R_FINITE(asDouble(nll))) {
    return Type(1e10);
  }

  // Report transformed parameters and diagnostics
  ADREPORT(alpha);
  ADREPORT(beta);
  ADREPORT(gamma);
  ADREPORT(delta);
  ADREPORT(lambda);
  REPORT(log_beta_func);
  REPORT(reg_penalty);
  REPORT(family);

  /*
   Note on Gradient and Hessian Extraction:
   The gradient and Hessian of the negative log-likelihood (with respect to the log-parameters)
   are automatically computed by TMB's automatic differentiation.
   To extract these in R, you can use:

   obj <- MakeADFun(data = list(x = data, family = family_integer),
   parameters = parameters,
   map = map,
   DLL="gkwmletmb")
   grad <- obj$gr(par)              # Gradient vector
   hess <- optimHess(par, obj$fn, obj$gr)  # Hessian matrix

   family_integer conversions:
   "gkw" = 0, "bkw" = 1, "kkw" = 2, "ekw" = 3, "mc" = 4, "kw" = 5, "beta" = 6

   The 'map' list should be used to fix parameters according to family.
   */

  return nll;
}
