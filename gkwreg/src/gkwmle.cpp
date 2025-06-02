// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <limits>
#include <algorithm>
#include <string>
#include <functional>
#include <vector>

using namespace Rcpp;
using namespace arma;

/*
* ===========================================================================
* NUMERIC STABILITY AUXILIARY FUNCTIONS
* ===========================================================================
* These functions ensure accurate numerical calculations even in extreme
* situations, near distribution boundaries, or with very small/large values.
*/

// Constants for numeric stability and precision
static const double EPSILON      = std::numeric_limits<double>::epsilon();
static const double DBL_MIN_SAFE = std::numeric_limits<double>::min() * 10.0;
static const double LOG_DBL_MIN  = std::log(DBL_MIN_SAFE);
static const double LOG_DBL_MAX  = std::log(std::numeric_limits<double>::max() / 10.0);
static const double LN2          = std::log(2.0); // More direct computation
static const double SQRT_EPSILON = std::sqrt(EPSILON); // More accurate calculation

/**
* log1mexp(u) calculates log(1 - exp(u)) with enhanced numerical stability
*
* This function is crucial for accurate calculations when u is negative and
* close to zero, where direct computation would suffer catastrophic cancellation.
* Uses different approximation methods depending on the range of u.
*
* @param u A negative value (log(x) where x < 1)
* @return log(1 - exp(u)), or NaN if u > 0
*/
inline double log1mexp(double u) {
// Input validation - u must be non-positive
if (u > 0.0) {
  return R_NaN;  // log(1 - exp(positive)) would yield log of negative number
}

// For values very close to 0, avoid potential instability
if (u > -SQRT_EPSILON) {
  return std::log(-u); // Approximation for u ≈ 0-
}

// For u in (-ln(2), 0], use log(-expm1(u)) for better accuracy
if (u > -LN2) {
  return std::log(-std::expm1(u));
}

// For u <= -ln(2), use log1p(-exp(u)) for better accuracy
return std::log1p(-std::exp(u));
}

/**
* log1pexp(x) calculates log(1 + exp(x)) with protection against overflow
*
* This function handles various regimes of x with appropriate approximations
* to maintain numerical stability across the entire real line.
*
* @param x Input value
* @return log(1 + exp(x)) calculated with numerical stability
*/
inline double log1pexp(double x) {
// Improved cutoff points based on numerical analysis
if (x > 700.0)    return x;                      // For very large x, log(1+exp(x)) ≈ x
if (x > 37.0)     return x + std::exp(-x);       // For large x, more efficient approximation
if (x > 18.0)     return x + std::log1p(std::exp(-x)); // For moderately large x
if (x > -37.0)    return std::log1p(std::exp(x));      // For moderate x
if (x > -700.0)   return std::exp(x);            // For negative x, where exp(x) is small but not negligible
return 0.0;                                       // For extremely negative x, where exp(x) ≈ 0
}

/**
* safe_log(x) computes log(x) with protection against invalid inputs
*
* @param x Input value
* @return log(x) or appropriate limiting value for x <= 0 or very small x
*/
inline double safe_log(double x) {
// Handle invalid or problematic inputs
if (x <= 0.0) {
  if (x == 0.0) return R_NegInf;  // Log of zero is -Infinity
  return R_NaN;                   // Log of negative is NaN
}

// Handle potential underflow
if (x < DBL_MIN_SAFE) return LOG_DBL_MIN + std::log(x / DBL_MIN_SAFE); // More accurate scaling

return std::log(x);
}

/**
* safe_exp(x) computes exp(x) with protection against overflow/underflow
*
* @param x Input value
* @return exp(x) or appropriate limiting value for extreme x
*/
inline double safe_exp(double x) {
// Handle extreme values to prevent overflow/underflow
if (x > LOG_DBL_MAX) return R_PosInf;  // Prevent overflow
if (x < LOG_DBL_MIN) {
  if (x < LOG_DBL_MIN - 10.0) return 0.0; // Far below threshold - return 0
  return DBL_MIN_SAFE * std::exp(x - LOG_DBL_MIN); // Scaled computation near threshold
}

return std::exp(x);
}

/**
* safe_pow(x, y) computes x^y with robust error handling
*
* Handles special cases and applies logarithmic transformation for stability with positive base.
* Also properly handles edge cases like 0^0, negative bases, and extreme values.
*
* @param x Base value
* @param y Exponent value
* @return x^y calculated with numerical stability
*/
inline double safe_pow(double x, double y) {
// Handle special cases
if (std::isnan(x) || std::isnan(y)) return R_NaN;

// Handle x = 0 cases
if (x == 0.0) {
  if (y > 0.0)  return 0.0;
  if (y == 0.0) return 1.0;   // 0^0 convention
  return R_PosInf;            // 0^negative is undefined/infinity
}

// Common trivial cases
if (x == 1.0 || y == 0.0) return 1.0;
if (y == 1.0) return x;

// Check for negative base with non-integer exponent (undefined in real domain)
if (x < 0.0) {
  // Check if y is effectively an integer
  double y_rounded = std::round(y);
  if (std::abs(y - y_rounded) > SQRT_EPSILON) {
    return R_NaN;  // Non-integer power of negative number
  }

  // Handle integer powers of negative numbers
  int y_int = static_cast<int>(y_rounded);
  double abs_result = std::pow(std::abs(x), std::abs(y));

  // Apply sign: negative^odd = negative, negative^even = positive
  bool negative_result = (y_int % 2 != 0);

  // Handle potential over/underflow
  if (y < 0) {
    if (abs_result > 1.0/DBL_MIN_SAFE && negative_result) return -R_PosInf;
    if (abs_result > 1.0/DBL_MIN_SAFE) return R_PosInf;
    return negative_result ? -1.0/abs_result : 1.0/abs_result;
  }

  return negative_result ? -abs_result : abs_result;
}

// For positive base, compute via logarithm for better numerical stability
if (std::abs(y) > 1e10) {
  // Handle extreme exponents separately
  double lx = std::log(x);
  if (lx < 0.0 && y > 0.0 && y * std::abs(lx) > LOG_DBL_MAX) return 0.0; // Very small result
  if (lx > 0.0 && y > 0.0 && y * lx > LOG_DBL_MAX) return R_PosInf;      // Very large result
  if (lx < 0.0 && y < 0.0 && y * lx < -LOG_DBL_MAX) return R_PosInf;     // Very large result
  if (lx > 0.0 && y < 0.0 && y * std::abs(lx) > LOG_DBL_MAX) return 0.0; // Very small result
}

// Normal case: compute via logarithm
double lx = std::log(x);
double log_result = y * lx;
return safe_exp(log_result);
}

/**
* Vector version of log1mexp for element-wise operations on arma::vec
*
* @param u Vector of input values
* @return Vector of log(1 - exp(u)) values
*/
inline arma::vec vec_log1mexp(const arma::vec& u) {
arma::vec result(u.n_elem);

// Process each element individually for maximum reliability
for (size_t i = 0; i < u.n_elem; ++i) {
  double ui = u(i);

  // Input validation - ui must be non-positive
  if (ui > 0.0) {
    result(i) = arma::datum::nan;
    continue;
  }

  // For values very close to 0, avoid potential instability
  if (ui > -SQRT_EPSILON) {
    result(i) = std::log(-ui);
    continue;
  }

  // For ui in (-ln(2), 0], use log(-expm1(ui)) for better accuracy
  if (ui > -LN2) {
    result(i) = std::log(-std::expm1(ui));
    continue;
  }

  // For ui <= -ln(2), use log1p(-exp(ui)) for better accuracy
  result(i) = std::log1p(-std::exp(ui));
}

return result;
}

/**
* Vector version of log1pexp for element-wise operations on arma::vec
*
* @param x Vector of input values
* @return Vector of log(1 + exp(x)) values
*/
inline arma::vec vec_log1pexp(const arma::vec& x) {
arma::vec result(x.n_elem);

// Process each element individually with optimized computation
for (size_t i = 0; i < x.n_elem; ++i) {
  double xi = x(i);

  // Apply appropriate approximation based on value range
  if (xi > 700.0) {
    result(i) = xi;
  } else if (xi > 37.0) {
    result(i) = xi + std::exp(-xi);
  } else if (xi > 18.0) {
    result(i) = xi + std::log1p(std::exp(-xi));
  } else if (xi > -37.0) {
    result(i) = std::log1p(std::exp(xi));
  } else if (xi > -700.0) {
    result(i) = std::exp(xi);
  } else {
    result(i) = 0.0;  // For extremely negative values
  }
}

return result;
}

/**
* Vector version of safe_log for element-wise operations on arma::vec
*
* @param x Vector of input values
* @return Vector of safe_log(x) values
*/
inline arma::vec vec_safe_log(const arma::vec& x) {
arma::vec result(x.n_elem);

// Process each element individually
for (size_t i = 0; i < x.n_elem; ++i) {
  double xi = x(i);

  // Handle invalid or problematic inputs
  if (xi < 0.0) {
    result(i) = arma::datum::nan;
  } else if (xi == 0.0) {
    result(i) = -arma::datum::inf;
  } else if (xi < DBL_MIN_SAFE) {
    // Handle potential underflow with better scaling
    result(i) = LOG_DBL_MIN + std::log(xi / DBL_MIN_SAFE);
  } else {
    result(i) = std::log(xi);
  }
}

return result;
}

/**
* Vector version of safe_exp for element-wise operations on arma::vec
*
* @param x Vector of input values
* @return Vector of safe_exp(x) values
*/
inline arma::vec vec_safe_exp(const arma::vec& x) {
arma::vec result(x.n_elem);

// Process each element individually
for (size_t i = 0; i < x.n_elem; ++i) {
  double xi = x(i);

  // Handle extreme values to prevent overflow/underflow
  if (xi > LOG_DBL_MAX) {
    result(i) = arma::datum::inf;
  } else if (xi < LOG_DBL_MIN - 10.0) {
    result(i) = 0.0;  // Far below threshold
  } else if (xi < LOG_DBL_MIN) {
    // Scaled computation near threshold for better accuracy
    result(i) = DBL_MIN_SAFE * std::exp(xi - LOG_DBL_MIN);
  } else {
    result(i) = std::exp(xi);
  }
}

return result;
}

/**
* Vector version of safe_pow for element-wise operations
*
* @param x Vector of base values
* @param y Single exponent value
* @return Vector of x[i]^y values
*/
inline arma::vec vec_safe_pow(const arma::vec& x, double y) {
arma::vec result(x.n_elem);

// Special case handling for trivial exponents
if (y == 0.0) {
  return arma::vec(x.n_elem, arma::fill::ones);
}

if (y == 1.0) {
  return x;
}

// Check if y is effectively an integer for negative base handling
bool y_is_int = (std::abs(y - std::round(y)) <= SQRT_EPSILON);
int y_int = static_cast<int>(std::round(y));
bool y_is_odd = y_is_int && (y_int % 2 != 0);

// Process each element individually
for (size_t i = 0; i < x.n_elem; ++i) {
  double xi = x(i);

  // Handle special cases
  if (std::isnan(xi)) {
    result(i) = arma::datum::nan;
    continue;
  }

  // Handle x = 0 cases
  if (xi == 0.0) {
    if (y > 0.0) {
      result(i) = 0.0;
    } else if (y == 0.0) {
      result(i) = 1.0;  // 0^0 convention
    } else {
      result(i) = arma::datum::inf;  // 0^negative
    }
    continue;
  }

  // Handle x = 1 case
  if (xi == 1.0) {
    result(i) = 1.0;
    continue;
  }

  // Handle negative base cases
  if (xi < 0.0) {
    if (!y_is_int) {
      // Non-integer power of negative not defined in reals
      result(i) = arma::datum::nan;
    } else {
      // Process integer powers of negative numbers
      double abs_xi = std::abs(xi);
      double abs_result = std::pow(abs_xi, std::abs(y));

      // Apply sign for odd powers
      if (y < 0) {
        if (y_is_odd) {
          result(i) = -1.0 / abs_result;
        } else {
          result(i) = 1.0 / abs_result;
        }
      } else {
        if (y_is_odd) {
          result(i) = -abs_result;
        } else {
          result(i) = abs_result;
        }
      }
    }
    continue;
  }

  // For positive base, compute via logarithm for better numerical stability
  // Handle extreme exponents separately
  if (std::abs(y) > 1e10) {
    double lx = std::log(xi);
    if (lx < 0.0 && y > 0.0 && y * std::abs(lx) > LOG_DBL_MAX) {
      result(i) = 0.0;  // Very small result
    } else if (lx > 0.0 && y > 0.0 && y * lx > LOG_DBL_MAX) {
      result(i) = arma::datum::inf;  // Very large result
    } else if (lx < 0.0 && y < 0.0 && y * lx < -LOG_DBL_MAX) {
      result(i) = arma::datum::inf;  // Very large result
    } else if (lx > 0.0 && y < 0.0 && y * std::abs(lx) > LOG_DBL_MAX) {
      result(i) = 0.0;  // Very small result
    } else {
      double log_result = y * lx;
      result(i) = safe_exp(log_result);
    }
  } else {
    // Normal case: compute via logarithm
    double log_result = y * std::log(xi);
    result(i) = safe_exp(log_result);
  }
}

return result;
}

/**
* Vector version of safe_pow with vector exponents
*
* @param x Vector of base values
* @param y Vector of exponent values (must match size of x)
* @return Vector of x[i]^y[i] values
*/
inline arma::vec vec_safe_pow(const arma::vec& x, const arma::vec& y) {
if (x.n_elem != y.n_elem) {
  Rcpp::stop("Vectors must have same length in vec_safe_pow");
}

arma::vec result(x.n_elem);

// Process element-wise with scalar function for maximum reliability
for (size_t i = 0; i < x.n_elem; ++i) {
  result(i) = safe_pow(x(i), y(i));
}

return result;
}

/**
* Checks if GKw parameters are in the valid domain
*
* Verifies that all parameters satisfy the constraints:
* alpha > 0, beta > 0, gamma > 0, delta >= 0, lambda > 0
*
* With strict=true, also enforces reasonable bounds to avoid numerical issues.
*
* @param alpha Shape parameter
* @param beta Shape parameter
* @param gamma Shape parameter
* @param delta Shape parameter
* @param lambda Shape parameter
* @param strict Whether to enforce additional bounds for numerical stability
* @return true if parameters are valid, false otherwise
*/
inline bool check_pars(double alpha,
                     double beta,
                     double gamma,
                     double delta,
                     double lambda,
                     bool strict = false) {
// Check for NaN values first
if (std::isnan(alpha) || std::isnan(beta) || std::isnan(gamma) ||
    std::isnan(delta) || std::isnan(lambda)) {
  return false;
}

// Basic parameter constraints
if (alpha <= 0.0 || beta <= 0.0 || gamma <= 0.0 || delta < 0.0 || lambda <= 0.0) {
  return false;
}

// Optional stricter constraints to avoid numerical issues
if (strict) {
  const double MIN_PARAM = 1e-5;
  const double MAX_PARAM = 1e5;

  if (alpha < MIN_PARAM || beta < MIN_PARAM || gamma < MIN_PARAM || lambda < MIN_PARAM ||
      (delta > 0.0 && delta < MIN_PARAM)) {
    return false;
  }
  if (alpha > MAX_PARAM || beta > MAX_PARAM || gamma > MAX_PARAM ||
      delta > MAX_PARAM || lambda > MAX_PARAM) {
    return false;
  }
}
return true;
}

/**
* Vector version of parameter checker for GKw distribution
*
* Checks all combinations of parameter values for validity.
*
* @param alpha Vector of alpha values
* @param beta Vector of beta values
* @param gamma Vector of gamma values
* @param delta Vector of delta values
* @param lambda Vector of lambda values
* @param strict Whether to enforce additional bounds for numerical stability
* @return arma::uvec of boolean values indicating parameter validity
*/
inline arma::uvec check_pars_vec(const arma::vec& alpha,
                               const arma::vec& beta,
                               const arma::vec& gamma,
                               const arma::vec& delta,
                               const arma::vec& lambda,
                               bool strict = false) {
// Find maximum length for broadcasting
size_t n = std::max({alpha.n_elem, beta.n_elem, gamma.n_elem,
                    delta.n_elem, lambda.n_elem});

arma::uvec valid(n, arma::fill::ones);

for (size_t i = 0; i < n; ++i) {
  // Get parameter values with proper cycling/broadcasting
  double a = alpha[i % alpha.n_elem];
  double b = beta[i % beta.n_elem];
  double g = gamma[i % gamma.n_elem];
  double d = delta[i % delta.n_elem];
  double l = lambda[i % lambda.n_elem];

  valid[i] = check_pars(a, b, g, d, l, strict);
}

return valid;
}


// // [[Rcpp::plugins(cpp11)]]
// // [[Rcpp::depends(RcppArmadillo)]]
// #include <RcppArmadillo.h>
// #include <cmath>
// #include <limits>
//
// #include <algorithm>
// #include <string>
// #include <functional>
// #include <vector>
//
// #include <stdexcept>
//
// using namespace Rcpp;
// using namespace arma;
//
//
// /*
// * ===========================================================================
// * NUMERIC STABILITY AUXILIARY FUNCTIONS
// * ===========================================================================
// * These functions ensure accurate numerical calculations even in extreme
// * situations, near distribution boundaries, or with very small/large values.
// */
//
// // Constants for numeric stability and precision
// static const double EPSILON      = std::numeric_limits<double>::epsilon();
// static const double DBL_MIN_SAFE = std::numeric_limits<double>::min() * 10.0;
// static const double LOG_DBL_MIN  = std::log(DBL_MIN_SAFE);
// static const double LOG_DBL_MAX  = std::log(std::numeric_limits<double>::max() / 10.0);
// static const double LN2          = 0.6931471805599453;
// static const double SQRT_EPSILON = 1.4901161193847656e-08; // sqrt(EPSILON)
//
// /**
// * log1mexp(u) calculates log(1 - exp(u)) with enhanced numerical stability
// *
// * This function is crucial for accurate calculations when u is negative and
// * close to zero, where direct computation would suffer catastrophic cancellation.
// * Uses different approximation methods depending on the range of u.
// *
// * @param u A negative value (log(x) where x < 1)
// * @return log(1 - exp(u)), or NaN if u > 0
// */
// inline double log1mexp(double u) {
// // Input validation - u must be non-positive
// if (u > 0.0) {
//   return R_NaN;  // log(1 - exp(positive)) would yield log of negative number
// }
//
// // For u in (-ln(2), 0], use log(-expm1(u)) for better accuracy
// if (u > -LN2) {
//   return std::log(-std::expm1(u));
// }
//
// // For u <= -ln(2), use log1p(-exp(u)) for better accuracy
// return std::log1p(-std::exp(u));
// }
//
// /**
// * log1pexp(x) calculates log(1 + exp(x)) with protection against overflow
// *
// * This function handles various regimes of x with appropriate approximations
// * to maintain numerical stability.
// *
// * @param x Input value
// * @return log(1 + exp(x)) calculated with numerical stability
// */
// inline double log1pexp(double x) {
// if (x > 700.0)    return x;                      // For very large x, log(1+exp(x)) ≈ x
// if (x > 18.0)     return x + std::log1p(std::exp(-x)); // For large x
// if (x > -37.0)    return std::log1p(std::exp(x));      // For moderate x
// return std::exp(x);                              // For very negative x, where exp(x) ≈ 0
// }
//
// /**
// * safe_log(x) computes log(x) with protection against invalid inputs
// *
// * @param x Input value
// * @return log(x) or appropriate limiting value for x <= 0 or very small x
// */
// inline double safe_log(double x) {
// if (x <= 0.0)        return R_NegInf;   // Log of non-positive number
// if (x < DBL_MIN_SAFE) return LOG_DBL_MIN; // Prevent underflow
// return std::log(x);
// }
//
// /**
// * safe_exp(x) computes exp(x) with protection against overflow/underflow
// *
// * @param x Input value
// * @return exp(x) or appropriate limiting value for extreme x
// */
// inline double safe_exp(double x) {
// if (x > LOG_DBL_MAX) return R_PosInf;  // Prevent overflow
// if (x < LOG_DBL_MIN) return 0.0;       // Prevent underflow
// return std::exp(x);
// }
//
// /**
// * safe_pow(x, y) computes x^y with robust error handling
// *
// * Handles special cases like negative base with non-integer exponent,
// * and uses logarithmic transformation for numerical stability with positive base.
// *
// * @param x Base value
// * @param y Exponent value
// * @return x^y calculated with numerical stability
// */
// inline double safe_pow(double x, double y) {
// // Handle special cases
// if (x == 0.0) {
//   if (y > 0.0)  return 0.0;
//   if (y == 0.0) return 1.0;   // 0^0 convention
//   return R_PosInf;            // 0^negative is undefined/infinity
// }
// if (x == 1.0 || y == 0.0) return 1.0;
// if (y == 1.0) return x;
//
// // Check for negative base with non-integer exponent (undefined in real domain)
// if (x < 0.0 && std::abs(y - std::round(y)) > SQRT_EPSILON) {
//   return R_NaN;
// }
//
// // For positive base, compute via logarithm for better numerical stability
// if (x > 0.0) {
//   double lx = std::log(x);
//   double log_result = y * lx;
//   return safe_exp(log_result);
// }
//
// // For negative base with integer exponent, use standard pow
// return std::pow(x, y);
// }
//
// /**
// * Vector version of log1mexp for element-wise operations on arma::vec
// *
// * @param u Vector of input values
// * @return Vector of log(1 - exp(u)) values
// */
// inline arma::vec vec_log1mexp(const arma::vec& u) {
// arma::vec result(u.n_elem);
// for (size_t i = 0; i < u.n_elem; ++i) {
//   result[i] = log1mexp(u[i]);
// }
// return result;
// }
//
// /**
// * Vector version of log1pexp for element-wise operations on arma::vec
// *
// * @param x Vector of input values
// * @return Vector of log(1 + exp(x)) values
// */
// inline arma::vec vec_log1pexp(const arma::vec& x) {
// arma::vec result(x.n_elem);
// for (size_t i = 0; i < x.n_elem; ++i) {
//   result[i] = log1pexp(x[i]);
// }
// return result;
// }
//
// /**
// * Vector version of safe_log for element-wise operations on arma::vec
// *
// * @param x Vector of input values
// * @return Vector of safe_log(x) values
// */
// inline arma::vec vec_safe_log(const arma::vec& x) {
// arma::vec result(x.n_elem);
// for (size_t i = 0; i < x.n_elem; ++i) {
//   result[i] = safe_log(x[i]);
// }
// return result;
// }
//
// /**
// * Vector version of safe_exp for element-wise operations on arma::vec
// *
// * @param x Vector of input values
// * @return Vector of safe_exp(x) values
// */
// inline arma::vec vec_safe_exp(const arma::vec& x) {
// arma::vec result(x.n_elem);
// for (size_t i = 0; i < x.n_elem; ++i) {
//   result[i] = safe_exp(x[i]);
// }
// return result;
// }
//
// /**
//  * Vector version of safe_pow with vector exponents
//  *
//  * @param x Vector of base values
//  * @param y Vector of exponent values (must match size of x)
//  * @return Vector of x[i]^y[i] values
//  */
// inline arma::vec vec_safe_pow(const arma::vec& x, const arma::vec& y) {
//   if (x.n_elem != y.n_elem) {
//     Rcpp::stop("Vectors must have same length in vec_safe_pow");
//   }
//
//   arma::vec result(x.n_elem);
//   for (size_t i = 0; i < x.n_elem; ++i) {
//     result[i] = safe_pow(x[i], y[i]);
//   }
//   return result;
// }
//
// /**
// * Checks if GKw parameters are in the valid domain
// *
// * Verifies that all parameters satisfy the constraints:
// * alpha > 0, beta > 0, gamma > 0, delta >= 0, lambda > 0
// *
// * With strict=true, also enforces reasonable bounds to avoid numerical issues.
// *
// * @param alpha Shape parameter
// * @param beta Shape parameter
// * @param gamma Shape parameter
// * @param delta Shape parameter
// * @param lambda Shape parameter
// * @param strict Whether to enforce additional bounds for numerical stability
// * @return true if parameters are valid, false otherwise
// */
// inline bool check_pars(double alpha,
//                      double beta,
//                      double gamma,
//                      double delta,
//                      double lambda,
//                      bool strict = false) {
// // Basic parameter constraints
// if (alpha <= 0.0 || beta <= 0.0 || gamma <= 0.0 || delta < 0.0 || lambda <= 0.0) {
//   return false;
// }
//
// // Optional stricter constraints to avoid numerical issues
// if (strict) {
//   const double MIN_PARAM = 1e-5;
//   const double MAX_PARAM = 1e5;
//
//   if (alpha < MIN_PARAM || beta < MIN_PARAM || gamma < MIN_PARAM || lambda < MIN_PARAM) {
//     return false;
//   }
//   if (alpha > MAX_PARAM || beta > MAX_PARAM || gamma > MAX_PARAM ||
//       delta > MAX_PARAM || lambda > MAX_PARAM) {
//     return false;
//   }
// }
// return true;
// }
//
// /**
// * Vector version of parameter checker for GKw distribution
// *
// * Checks all combinations of parameter values for validity.
// *
// * @param alpha Vector of alpha values
// * @param beta Vector of beta values
// * @param gamma Vector of gamma values
// * @param delta Vector of delta values
// * @param lambda Vector of lambda values
// * @param strict Whether to enforce additional bounds for numerical stability
// * @return arma::uvec of boolean values indicating parameter validity
// */
// inline arma::uvec check_pars_vec(const arma::vec& alpha,
//                                const arma::vec& beta,
//                                const arma::vec& gamma,
//                                const arma::vec& delta,
//                                const arma::vec& lambda,
//                                bool strict = false) {
// // Find maximum length for broadcasting
// size_t n = std::max({alpha.n_elem, beta.n_elem, gamma.n_elem,
//                     delta.n_elem, lambda.n_elem});
//
// arma::uvec valid(n, arma::fill::ones);
//
// for (size_t i = 0; i < n; ++i) {
//   // Get parameter values with proper cycling/broadcasting
//   double a = alpha[i % alpha.n_elem];
//   double b = beta[i % beta.n_elem];
//   double g = gamma[i % gamma.n_elem];
//   double d = delta[i % delta.n_elem];
//   double l = lambda[i % lambda.n_elem];
//
//   valid[i] = check_pars(a, b, g, d, l, strict);
// }
//
// return valid;
// }


/*
* ===========================================================================
* PRIMARY FUNCTIONS FOR GENERALIZED KUMARASWAMY DISTRIBUTION
* ===========================================================================
*/

//' @title Density of the Generalized Kumaraswamy Distribution
//' @author Lopes, J. E.
//' @keywords distribution density
//'
//' @description
//' Computes the probability density function (PDF) for the five-parameter
//' Generalized Kumaraswamy (GKw) distribution, defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned. Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{alpha}, \code{beta},
//'   \code{gamma}, \code{delta}, \code{lambda}). Returns \code{0} (or \code{-Inf}
//'   if \code{log_prob = TRUE}) for \code{x} outside the interval (0, 1), or
//'   \code{NaN} if parameters are invalid.
//'
//' @details
//' The probability density function of the Generalized Kumaraswamy (GKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), \code{delta} (\eqn{\delta}), and
//' \code{lambda} (\eqn{\lambda}) is given by:
//' \deqn{
//' f(x; \alpha, \beta, \gamma, \delta, \lambda) =
//'   \frac{\lambda \alpha \beta x^{\alpha-1}(1-x^{\alpha})^{\beta-1}}
//'        {B(\gamma, \delta+1)}
//'   [1-(1-x^{\alpha})^{\beta}]^{\gamma\lambda-1}
//'   [1-[1-(1-x^{\alpha})^{\beta}]^{\lambda}]^{\delta}
//' }
//' for \eqn{x \in (0,1)}, where \eqn{B(a, b)} is the Beta function
//' \code{\link[base]{beta}}.
//'
//' This distribution was proposed by Cordeiro & de Castro (2011) and includes
//' several other distributions as special cases:
//' \itemize{
//'   \item Kumaraswamy (Kw): \code{gamma = 1}, \code{delta = 0}, \code{lambda = 1}
//'   \item Exponentiated Kumaraswamy (EKw): \code{gamma = 1}, \code{delta = 0}
//'   \item Beta-Kumaraswamy (BKw): \code{lambda = 1}
//'   \item Generalized Beta type 1 (GB1 - implies McDonald): \code{alpha = 1}, \code{beta = 1}
//'   \item Beta distribution: \code{alpha = 1}, \code{beta = 1}, \code{lambda = 1}
//' }
//' The function includes checks for valid parameters and input values \code{x}.
//' It uses numerical stabilization for \code{x} close to 0 or 1.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//' *81*(7), 883-898.
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{pgkw}}, \code{\link{qgkw}}, \code{\link{rgkw}} (if these exist),
//' \code{\link[stats]{dbeta}}, \code{\link[stats]{integrate}}
//'
//' @examples
//' \donttest{
//' # Simple density evaluation at a point
//' dgkw(0.5, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1) # Kw case
//'
//' # Plot the PDF for various parameter sets
//' x_vals <- seq(0.01, 0.99, by = 0.01)
//'
//' # Standard Kumaraswamy (gamma=1, delta=0, lambda=1)
//' pdf_kw <- dgkw(x_vals, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//'
//' # Beta equivalent (alpha=1, beta=1, lambda=1) - Beta(gamma, delta+1)
//' pdf_beta <- dgkw(x_vals, alpha = 1, beta = 1, gamma = 2, delta = 3, lambda = 1)
//' # Compare with stats::dbeta
//' pdf_beta_check <- stats::dbeta(x_vals, shape1 = 2, shape2 = 3 + 1)
//' # max(abs(pdf_beta - pdf_beta_check)) # Should be close to zero
//'
//' # Exponentiated Kumaraswamy (gamma=1, delta=0)
//' pdf_ekw <- dgkw(x_vals, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 2)
//'
//' plot(x_vals, pdf_kw, type = "l", ylim = range(c(pdf_kw, pdf_beta, pdf_ekw)),
//'      main = "GKw Densities Examples", ylab = "f(x)", xlab="x", col = "blue")
//' lines(x_vals, pdf_beta, col = "red")
//' lines(x_vals, pdf_ekw, col = "green")
//' legend("topright", legend = c("Kw(2,3)", "Beta(2,4) equivalent", "EKw(2,3, lambda=2)"),
//'        col = c("blue", "red", "green"), lty = 1, bty = "n")
//'
//' # Log-density
//' log_pdf_val <- dgkw(0.5, 2, 3, 1, 0, 1, log_prob = TRUE)
//' print(log_pdf_val)
//' print(log(dgkw(0.5, 2, 3, 1, 0, 1))) # Should match
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dgkw(
 const arma::vec& x,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool log_prob = false
) {
// Convert NumericVector to arma::vec
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());
arma::vec lambda_vec(lambda.begin(), lambda.size());

// Find the maximum length for broadcasting
size_t n = std::max({x.n_elem, alpha_vec.n_elem, beta_vec.n_elem,
                   gamma_vec.n_elem, delta_vec.n_elem, lambda_vec.n_elem});

// Initialize result vector
arma::vec result(n);
if (log_prob) {
 result.fill(R_NegInf);
} else {
 result.fill(0.0);
}

// Process each element
for (size_t i = 0; i < n; ++i) {
 // Get parameter values with broadcasting/recycling
 double a = alpha_vec[i % alpha_vec.n_elem];
 double b = beta_vec[i % beta_vec.n_elem];
 double g = gamma_vec[i % gamma_vec.n_elem];
 double d = delta_vec[i % delta_vec.n_elem];
 double l = lambda_vec[i % lambda_vec.n_elem];
 double xi = x[i % x.n_elem];

 // Validate parameters
 if (!check_pars(a, b, g, d, l)) {
   Rcpp::warning("dgkw: invalid parameters at index %d (alpha,beta,gamma>0, delta>=0, lambda>0)", i+1);
   continue;
 }

 // Check if x is within (0,1)
 if (xi <= 0.0 || xi >= 1.0 || !R_finite(xi)) {
   continue;
 }

 // Numerical stability: avoid calculations very close to 0 or 1
 double x_near_zero = std::pow(SQRT_EPSILON, 1.0/a);
 double x_near_one = 1.0 - std::pow(SQRT_EPSILON, 1.0/a);

 if (xi < x_near_zero || xi > x_near_one) {
   continue;
 }

 // Precalculate common terms used in the PDF formula
 double log_beta_val = R::lgammafn(g) + R::lgammafn(d + 1.0) - R::lgammafn(g + d + 1.0);
 double log_const = std::log(l) + std::log(a) + std::log(b) - log_beta_val;
 double gamma_lambda = g * l;

 // Calculate x^α
 double log_xi = std::log(xi);
 double log_x_alpha = a * log_xi;
 double x_alpha = std::exp(log_x_alpha);

 // Check if x^α < 1 for numerical stability
 if (x_alpha >= 1.0 - SQRT_EPSILON) {
   continue;
 }

 // Calculate (1 - x^α)
 double log_one_minus_x_alpha = log1mexp(log_x_alpha);
 if (!R_finite(log_one_minus_x_alpha)) {
   continue;
 }

 // Calculate (1 - x^α)^β
 double log_one_minus_x_alpha_beta = b * log_one_minus_x_alpha;

 // Calculate 1 - (1 - x^α)^β
 double log_term1 = log1mexp(log_one_minus_x_alpha_beta);
 if (!R_finite(log_term1)) {
   continue;
 }

 // Calculate [1-(1-x^α)^β]^λ
 double log_term1_lambda = l * log_term1;

 // Calculate 1 - [1-(1-x^α)^β]^λ
 double log_term2 = log1mexp(log_term1_lambda);
 if (!R_finite(log_term2)) {
   continue;
 }

 // Assemble the full log-density expression
 double logdens = log_const +
   (a - 1.0) * log_xi +
   (b - 1.0) * log_one_minus_x_alpha +
   (gamma_lambda - 1.0) * log_term1 +
   d * log_term2;

 // Check for invalid result
 if (!R_finite(logdens)) {
   continue;
 }

 // Return log-density or density as requested
 result(i) = log_prob ? logdens : safe_exp(logdens);
}

return Rcpp::NumericVector(result.memptr(), result.memptr() + result.n_elem);
}

//' @title Generalized Kumaraswamy Distribution CDF
//' @author Lopes, J. E.
//' @keywords distribution cumulative
//'
//' @description
//' Computes the cumulative distribution function (CDF) for the five-parameter
//' Generalized Kumaraswamy (GKw) distribution, defined on the interval (0, 1).
//' Calculates \eqn{P(X \le q)}.
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms if
//'   \code{log_p = TRUE}. The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{q}, \code{alpha}, \code{beta},
//'   \code{gamma}, \code{delta}, \code{lambda}). Returns \code{0} (or \code{-Inf}
//'   if \code{log_p = TRUE}) for \code{q <= 0} and \code{1} (or \code{0} if
//'   \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN} for invalid
//'   parameters.
//'
//' @details
//' The cumulative distribution function (CDF) of the Generalized Kumaraswamy (GKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), \code{delta} (\eqn{\delta}), and
//' \code{lambda} (\eqn{\lambda}) is given by:
//' \deqn{
//' F(q; \alpha, \beta, \gamma, \delta, \lambda) =
//'   I_{x(q)}(\gamma, \delta+1)
//' }
//' where \eqn{x(q) = [1-(1-q^{\alpha})^{\beta}]^{\lambda}} and \eqn{I_x(a, b)}
//' is the regularized incomplete beta function, defined as:
//' \deqn{
//' I_x(a, b) = \frac{B_x(a, b)}{B(a, b)} = \frac{\int_0^x t^{a-1}(1-t)^{b-1} dt}{\int_0^1 t^{a-1}(1-t)^{b-1} dt}
//' }
//' This corresponds to the \code{\link[stats]{pbeta}} function in R, such that
//' \eqn{F(q; \alpha, \beta, \gamma, \delta, \lambda) = \code{pbeta}(x(q), \code{shape1} = \gamma, \code{shape2} = \delta+1)}.
//'
//' The GKw distribution includes several special cases, such as the Kumaraswamy,
//' Beta, and Exponentiated Kumaraswamy distributions (see \code{\link{dgkw}} for details).
//' The function utilizes numerical algorithms for computing the regularized
//' incomplete beta function accurately, especially near the boundaries.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{dgkw}}, \code{\link{qgkw}}, \code{\link{rgkw}},
//' \code{\link[stats]{pbeta}}
//'
//' @examples
//' \donttest{
//' # Simple CDF evaluation
//' prob <- pgkw(0.5, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1) # Kw case
//' print(prob)
//'
//' # Upper tail probability P(X > q)
//' prob_upper <- pgkw(0.5, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1,
//'                  lower_tail = FALSE)
//' print(prob_upper)
//' # Check: prob + prob_upper should be 1
//' print(prob + prob_upper)
//'
//' # Log probability
//' log_prob <- pgkw(0.5, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1,
//'                  log_p = TRUE)
//' print(log_prob)
//' # Check: exp(log_prob) should be prob
//' print(exp(log_prob))
//'
//' # Use of vectorized parameters
//' q_vals <- c(0.2, 0.5, 0.8)
//' alphas_vec <- c(0.5, 1.0, 2.0)
//' betas_vec <- c(1.0, 2.0, 3.0)
//' # Vectorizes over q, alpha, beta
//' pgkw(q_vals, alpha = alphas_vec, beta = betas_vec, gamma = 1, delta = 0.5, lambda = 0.5)
//'
//' # Plotting the CDF for special cases
//' x_seq <- seq(0.01, 0.99, by = 0.01)
//' # Standard Kumaraswamy CDF
//' cdf_kw <- pgkw(x_seq, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' # Beta distribution CDF equivalent (Beta(gamma, delta+1))
//' cdf_beta_equiv <- pgkw(x_seq, alpha = 1, beta = 1, gamma = 2, delta = 3, lambda = 1)
//' # Compare with stats::pbeta
//' cdf_beta_check <- stats::pbeta(x_seq, shape1 = 2, shape2 = 3 + 1)
//' # max(abs(cdf_beta_equiv - cdf_beta_check)) # Should be close to zero
//'
//' plot(x_seq, cdf_kw, type = "l", ylim = c(0, 1),
//'      main = "GKw CDF Examples", ylab = "F(x)", xlab = "x", col = "blue")
//' lines(x_seq, cdf_beta_equiv, col = "red", lty = 2)
//' legend("bottomright", legend = c("Kw(2,3)", "Beta(2,4) equivalent"),
//'        col = c("blue", "red"), lty = c(1, 2), bty = "n")
//'}
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pgkw(
 const arma::vec& q,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
// Convert NumericVector to arma::vec
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());
arma::vec lambda_vec(lambda.begin(), lambda.size());

// Find maximum length for broadcasting
size_t n = std::max({q.n_elem, alpha_vec.n_elem, beta_vec.n_elem,
                   gamma_vec.n_elem, delta_vec.n_elem, lambda_vec.n_elem});

// Initialize result vector
arma::vec result(n);

// Process each element
for (size_t i = 0; i < n; ++i) {
 // Get parameter values with broadcasting/recycling
 double a = alpha_vec[i % alpha_vec.n_elem];
 double b = beta_vec[i % beta_vec.n_elem];
 double g = gamma_vec[i % gamma_vec.n_elem];
 double d = delta_vec[i % delta_vec.n_elem];
 double l = lambda_vec[i % lambda_vec.n_elem];
 double qi = q[i % q.n_elem];

 // Check parameter validity
 if (!check_pars(a, b, g, d, l)) {
   result(i) = NA_REAL;
   Rcpp::warning("pgkw: invalid parameters at index %d (alpha,beta,gamma>0, delta>=0, lambda>0)", i+1);
   continue;
 }

 // Check domain boundaries
 if (!R_finite(qi) || qi <= 0.0) {
   result(i) = lower_tail ? (log_p ? R_NegInf : 0.0) : (log_p ? 0.0 : 1.0);
   continue;
 }

 if (qi >= 1.0) {
   result(i) = lower_tail ? (log_p ? 0.0 : 1.0) : (log_p ? R_NegInf : 0.0);
   continue;
 }

 // Compute CDF using stable numerical methods
 double log_qi = std::log(qi);
 double log_qi_alpha = a * log_qi;
 double qi_alpha = safe_exp(log_qi_alpha);

 // Calculate (1 - q^alpha) with numerical stability
 double one_minus_qi_alpha;
 if (qi_alpha < 0.5) {
   one_minus_qi_alpha = 1.0 - qi_alpha;
 } else {
   // If close to 1, use expm1 for better precision
   one_minus_qi_alpha = -std::expm1(log_qi_alpha);
 }

 // Boundary checks
 if (one_minus_qi_alpha <= 0.0) {
   result(i) = lower_tail ? (log_p ? 0.0 : 1.0) : (log_p ? R_NegInf : 0.0);
   continue;
 }

 if (one_minus_qi_alpha >= 1.0) {
   result(i) = lower_tail ? (log_p ? R_NegInf : 0.0) : (log_p ? 0.0 : 1.0);
   continue;
 }

 // Calculate (1 - q^alpha)^beta
 double log_oma = std::log(one_minus_qi_alpha);
 double log_oma_beta = b * log_oma;
 double oma_beta = safe_exp(log_oma_beta);

 // Calculate 1 - (1 - q^alpha)^beta
 double term = 1.0 - oma_beta;

 // Boundary checks
 if (term <= 0.0) {
   result(i) = lower_tail ? (log_p ? R_NegInf : 0.0) : (log_p ? 0.0 : 1.0);
   continue;
 }

 if (term >= 1.0) {
   result(i) = lower_tail ? (log_p ? 0.0 : 1.0) : (log_p ? R_NegInf : 0.0);
   continue;
 }

 // Calculate [1 - (1 - q^alpha)^beta]^lambda
 double log_term = std::log(term);
 double log_y = l * log_term;
 double y = safe_exp(log_y);

 // Boundary checks
 if (y <= 0.0) {
   result(i) = lower_tail ? (log_p ? R_NegInf : 0.0) : (log_p ? 0.0 : 1.0);
   continue;
 }

 if (y >= 1.0) {
   result(i) = lower_tail ? (log_p ? 0.0 : 1.0) : (log_p ? R_NegInf : 0.0);
   continue;
 }

 // Use pbeta for the final calculation
 double prob = R::pbeta(y, g, d + 1.0, /*lower_tail=*/true, /*log_p=*/false);

 // Adjust for upper tail if requested
 if (!lower_tail) {
   prob = 1.0 - prob;
 }

 // Convert to log scale if requested
 if (log_p) {
   if (prob <= 0.0) {
     prob = R_NegInf;
   } else if (prob >= 1.0) {
     prob = 0.0;
   } else {
     prob = std::log(prob);
   }
 }

 result(i) = prob;
}

return Rcpp::NumericVector(result.memptr(), result.memptr() + result.n_elem);
}


//' @title Generalized Kumaraswamy Distribution Quantile Function
//' @author Lopes, J. E.
//' @keywords distribution quantile
//'
//' @description
//' Computes the quantile function (inverse CDF) for the five-parameter
//' Generalized Kumaraswamy (GKw) distribution. Finds the value \code{x} such
//' that \eqn{P(X \le x) = p}, where \code{X} follows the GKw distribution.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le x)}, otherwise, \eqn{P(X > x)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{alpha}, \code{beta}, \code{gamma},
//'   \code{delta}, \code{lambda}). Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{alpha <= 0},
//'           \code{beta <= 0}, \code{gamma <= 0}, \code{delta < 0},
//'           \code{lambda <= 0}).
//'   }
//'
//' @details
//' The quantile function \eqn{Q(p)} is the inverse of the CDF \eqn{F(x)}.
//' Given \eqn{F(x) = I_{y(x)}(\gamma, \delta+1)} where
//' \eqn{y(x) = [1-(1-x^{\alpha})^{\beta}]^{\lambda}}, the quantile function is:
//' \deqn{
//' Q(p) = x = \left\{ 1 - \left[ 1 - \left( I^{-1}_{p}(\gamma, \delta+1) \right)^{1/\lambda} \right]^{1/\beta} \right\}^{1/\alpha}
//' }
//' where \eqn{I^{-1}_{p}(a, b)} is the inverse of the regularized incomplete beta
//' function, which corresponds to the quantile function of the Beta distribution,
//' \code{\link[stats]{qbeta}}.
//'
//' The computation proceeds as follows:
//' \enumerate{
//'   \item Calculate \code{y = stats::qbeta(p, shape1 = gamma, shape2 = delta + 1, lower.tail = lower_tail, log.p = log_p)}.
//'   \item Calculate \eqn{v = y^{1/\lambda}}.
//'   \item Calculate \eqn{w = (1 - v)^{1/\beta}}. Note: Requires \eqn{v \le 1}.
//'   \item Calculate \eqn{q = (1 - w)^{1/\alpha}}. Note: Requires \eqn{w \le 1}.
//' }
//' Numerical stability is maintained by handling boundary cases (\code{p = 0},
//' \code{p = 1}) directly and checking intermediate results (e.g., ensuring
//' arguments to powers are non-negative).
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{dgkw}}, \code{\link{pgkw}}, \code{\link{rgkw}},
//' \code{\link[stats]{qbeta}}
//'
//' @examples
//' \donttest{
//' # Basic quantile calculation (median)
//' median_val <- qgkw(0.5, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' print(median_val)
//'
//' # Computing multiple quantiles
//' probs <- c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
//' quantiles <- qgkw(probs, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' print(quantiles)
//'
//' # Upper tail quantile (e.g., find x such that P(X > x) = 0.1, which is 90th percentile)
//' q90 <- qgkw(0.1, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1,
//'             lower_tail = FALSE)
//' print(q90)
//' # Check: should match quantile for p = 0.9 with lower_tail = TRUE
//' print(qgkw(0.9, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1))
//'
//' # Log probabilities
//' median_logp <- qgkw(log(0.5), alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1,
//'                     log_p = TRUE)
//' print(median_logp) # Should match median_val
//'
//' # Vectorized parameters
//' alphas_vec <- c(0.5, 1.0, 2.0)
//' betas_vec <- c(1.0, 2.0, 3.0)
//' # Get median for 3 different GKw distributions
//' medians_vec <- qgkw(0.5, alpha = alphas_vec, beta = betas_vec, gamma = 1, delta = 0, lambda = 1)
//' print(medians_vec)
//'
//' # Verify inverse relationship with pgkw
//' p_val <- 0.75
//' x_val <- qgkw(p_val, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' p_check <- pgkw(x_val, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' print(paste("Calculated p:", p_check, " (Expected:", p_val, ")"))
//'}
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qgkw(
 const arma::vec& p,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
// Convert NumericVector to arma::vec
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());
arma::vec lambda_vec(lambda.begin(), lambda.size());

// Find maximum length for broadcasting
size_t n = std::max({p.n_elem, alpha_vec.n_elem, beta_vec.n_elem,
                   gamma_vec.n_elem, delta_vec.n_elem, lambda_vec.n_elem});

// Initialize result vector
arma::vec result(n);

// Process each element
for (size_t i = 0; i < n; ++i) {
 // Get parameter values with broadcasting/recycling
 double a = alpha_vec[i % alpha_vec.n_elem];
 double b = beta_vec[i % beta_vec.n_elem];
 double g = gamma_vec[i % gamma_vec.n_elem];
 double d = delta_vec[i % delta_vec.n_elem];
 double l = lambda_vec[i % lambda_vec.n_elem];
 double pp = p[i % p.n_elem];

 // Validate parameters
 if (!check_pars(a, b, g, d, l)) {
   result(i) = NA_REAL;
   Rcpp::warning("qgkw: invalid parameters at index %d (alpha,beta,gamma>0, delta>=0, lambda>0)", i+1);
   continue;
 }

 // Process log_p and lower_tail
 if (log_p) {
   if (pp > 0.0) {
     // log(p) > 0 implies p > 1, which is invalid
     result(i) = NA_REAL;
     continue;
   }
   pp = std::exp(pp);  // Convert from log-scale
 }

 if (!lower_tail) {
   pp = 1.0 - pp;  // Convert from upper tail to lower tail
 }

 // Check probability bounds
 if (!R_finite(pp) || pp < 0.0) {
   result(i) = 0.0;  // For p ≤ 0, quantile is 0
   continue;
 }

 if (pp > 1.0) {
   result(i) = 1.0;  // For p > 1, quantile is 1
   continue;
 }

 if (pp <= 0.0) {
   result(i) = 0.0;  // For p = 0, quantile is 0
   continue;
 }

 if (pp >= 1.0) {
   result(i) = 1.0;  // For p = 1, quantile is 1
   continue;
 }

 // Step 1: Find y = qbeta(p, γ, δ+1)
 double y = R::qbeta(pp, g, d + 1.0, /*lower_tail=*/true, /*log_p=*/false);

 // Check for boundary conditions
 if (y <= 0.0) {
   result(i) = 0.0;
   continue;
 }

 if (y >= 1.0) {
   result(i) = 1.0;
   continue;
 }

 // Step 2: Compute v = y^(1/λ)
 double v;
 if (l == 1.0) {
   v = y;  // Optimization for λ=1
 } else {
   v = safe_pow(y, 1.0/l);
 }

 // Step 3: Compute tmp = 1 - v
 double tmp = 1.0 - v;

 // Check for boundary conditions
 if (tmp <= 0.0) {
   result(i) = 1.0;
   continue;
 }

 if (tmp >= 1.0) {
   result(i) = 0.0;
   continue;
 }

 // Step 4: Compute tmp2 = tmp^(1/β)
 double tmp2;
 if (b == 1.0) {
   tmp2 = tmp;  // Optimization for β=1
 } else {
   tmp2 = safe_pow(tmp, 1.0/b);
 }

 // Check for boundary conditions
 if (tmp2 <= 0.0) {
   result(i) = 1.0;
   continue;
 }

 if (tmp2 >= 1.0) {
   result(i) = 0.0;
   continue;
 }

 // Step 5: Compute q = (1 - tmp2)^(1/α)
 double one_minus_tmp2 = 1.0 - tmp2;
 double qq;

 if (one_minus_tmp2 <= 0.0) {
   qq = 0.0;
 } else if (one_minus_tmp2 >= 1.0) {
   qq = 1.0;
 } else if (a == 1.0) {
   qq = one_minus_tmp2;  // Optimization for α=1
 } else {
   qq = safe_pow(one_minus_tmp2, 1.0/a);
 }

 // Final boundary check to ensure result is in (0,1)
 if (qq < 0.0) {
   qq = 0.0;
 } else if (qq > 1.0) {
   qq = 1.0;
 }

 result(i) = qq;
}

return Rcpp::NumericVector(result.memptr(), result.memptr() + result.n_elem);
}


//' @title Generalized Kumaraswamy Distribution Random Generation
//' @author Lopes, J. E.
//' @keywords distribution random
//'
//' @description
//' Generates random deviates from the five-parameter Generalized Kumaraswamy (GKw)
//' distribution defined on the interval (0, 1).
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//'
//' @return A vector of length \code{n} containing random deviates from the GKw
//'   distribution. The length of the result is determined by \code{n} and the
//'   recycling rule applied to the parameters (\code{alpha}, \code{beta},
//'   \code{gamma}, \code{delta}, \code{lambda}). Returns \code{NaN} if parameters
//'   are invalid (e.g., \code{alpha <= 0}, \code{beta <= 0}, \code{gamma <= 0},
//'   \code{delta < 0}, \code{lambda <= 0}).
//'
//' @details
//' The generation method relies on the transformation property: if
//' \eqn{V \sim \mathrm{Beta}(\gamma, \delta+1)}, then the random variable \code{X}
//' defined as
//' \deqn{
//' X = \left\{ 1 - \left[ 1 - V^{1/\lambda} \right]^{1/\beta} \right\}^{1/\alpha}
//' }
//' follows the GKw(\eqn{\alpha, \beta, \gamma, \delta, \lambda}) distribution.
//'
//' The algorithm proceeds as follows:
//' \enumerate{
//'   \item Generate \code{V} from \code{stats::rbeta(n, shape1 = gamma, shape2 = delta + 1)}.
//'   \item Calculate \eqn{v = V^{1/\lambda}}.
//'   \item Calculate \eqn{w = (1 - v)^{1/\beta}}.
//'   \item Calculate \eqn{x = (1 - w)^{1/\alpha}}.
//' }
//' Parameters (\code{alpha}, \code{beta}, \code{gamma}, \code{delta}, \code{lambda})
//' are recycled to match the length required by \code{n}. Numerical stability is
//' maintained by handling potential edge cases during the transformations.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{dgkw}}, \code{\link{pgkw}}, \code{\link{qgkw}},
//' \code{\link[stats]{rbeta}}, \code{\link[base]{set.seed}}
//'
//' @examples
//' \donttest{
//' set.seed(1234) # for reproducibility
//'
//' # Generate 1000 random values from a specific GKw distribution (Kw case)
//' x_sample <- rgkw(1000, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' summary(x_sample)
//'
//' # Histogram of generated values compared to theoretical density
//' hist(x_sample, breaks = 30, freq = FALSE, # freq=FALSE for density scale
//'      main = "Histogram of GKw(2,3,1,0,1) Sample", xlab = "x", ylim = c(0, 2.5))
//' curve(dgkw(x, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1),
//'       add = TRUE, col = "red", lwd = 2, n = 201)
//' legend("topright", legend = "Theoretical PDF", col = "red", lwd = 2, bty = "n")
//'
//' # Comparing empirical and theoretical quantiles (Q-Q plot)
//' prob_points <- seq(0.01, 0.99, by = 0.01)
//' theo_quantiles <- qgkw(prob_points, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1)
//' emp_quantiles <- quantile(x_sample, prob_points)
//'
//' plot(theo_quantiles, emp_quantiles, pch = 16, cex = 0.8,
//'      main = "Q-Q Plot for GKw(2,3,1,0,1)",
//'      xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles (n=1000)")
//' abline(a = 0, b = 1, col = "blue", lty = 2)
//'
//' # Using vectorized parameters: generate 1 value for each alpha
//' alphas_vec <- c(0.5, 1.0, 2.0)
//' n_param <- length(alphas_vec)
//' samples_vec <- rgkw(n_param, alpha = alphas_vec, beta = 2, gamma = 1, delta = 0, lambda = 1)
//' print(samples_vec) # One sample for each alpha value
//' # Result length matches n=3, parameters alpha recycled accordingly
//'
//' # Example with invalid parameters (should produce NaN)
//' invalid_sample <- rgkw(1, alpha = -1, beta = 2, gamma = 1, delta = 0, lambda = 1)
//' print(invalid_sample)
//'}
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rgkw(
 int n,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda
) {
// Convert NumericVector to arma::vec
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());
arma::vec lambda_vec(lambda.begin(), lambda.size());

// Count of parameter combinations for vectorization
size_t k = std::max({alpha_vec.n_elem, beta_vec.n_elem, gamma_vec.n_elem,
                   delta_vec.n_elem, lambda_vec.n_elem});

// Initialize result vector
arma::vec result(n);

// Process each element
for (int i = 0; i < n; ++i) {
 // Index for parameter combination (cycling through k combinations)
 size_t idx = i % k;

 // Get parameter values with broadcasting/recycling
 double a = alpha_vec[idx % alpha_vec.n_elem];
 double b = beta_vec[idx % beta_vec.n_elem];
 double g = gamma_vec[idx % gamma_vec.n_elem];
 double d = delta_vec[idx % delta_vec.n_elem];
 double l = lambda_vec[idx % lambda_vec.n_elem];

 // Validate parameters
 if (!check_pars(a, b, g, d, l)) {
   result(i) = NA_REAL;
   Rcpp::warning("rgkw: invalid parameters at index %d (alpha,beta,gamma>0, delta>=0, lambda>0)", idx+1);
   continue;
 }

 // Generate Beta(γ, δ+1) random value
 double vi = R::rbeta(g, d + 1.0);

 // Check for boundary conditions
 if (vi <= 0.0) {
   result(i) = 0.0;
   continue;
 }

 if (vi >= 1.0) {
   result(i) = 1.0;
   continue;
 }

 // Compute v = V^(1/λ)
 double vl;
 if (l == 1.0) {
   vl = vi;  // Optimization for λ=1
 } else {
   vl = safe_pow(vi, 1.0/l);
 }

 // Compute tmp = 1 - v
 double tmp = 1.0 - vl;

 // Check for boundary conditions
 if (tmp <= 0.0) {
   result(i) = 1.0;
   continue;
 }

 if (tmp >= 1.0) {
   result(i) = 0.0;
   continue;
 }

 // Compute tmp2 = tmp^(1/β)
 double tmp2;
 if (b == 1.0) {
   tmp2 = tmp;  // Optimization for β=1
 } else {
   tmp2 = safe_pow(tmp, 1.0/b);
 }

 // Check for boundary conditions
 if (tmp2 <= 0.0) {
   result(i) = 1.0;
   continue;
 }

 if (tmp2 >= 1.0) {
   result(i) = 0.0;
   continue;
 }

 // Compute x = (1 - tmp2)^(1/α)
 double one_minus_tmp2 = 1.0 - tmp2;
 double xx;

 if (one_minus_tmp2 <= 0.0) {
   xx = 0.0;
 } else if (one_minus_tmp2 >= 1.0) {
   xx = 1.0;
 } else if (a == 1.0) {
   xx = one_minus_tmp2;  // Optimization for α=1
 } else {
   xx = safe_pow(one_minus_tmp2, 1.0/a);
 }

 // Final boundary check to ensure result is in (0,1)
 if (xx < 0.0) {
   xx = 0.0;
 } else if (xx > 1.0) {
   xx = 1.0;
 }

 result(i) = xx;
}

return Rcpp::NumericVector(result.memptr(), result.memptr() + result.n_elem);
}





//' @title Negative Log-Likelihood for the Generalized Kumaraswamy Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize
//'
//' @description
//' Computes the negative log-likelihood function for the five-parameter
//' Generalized Kumaraswamy (GKw) distribution given a vector of observations.
//' This function is designed for use in optimization routines (e.g., maximum
//' likelihood estimation).
//'
//' @param par A numeric vector of length 5 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns a large positive
//'   value (e.g., \code{Inf}) if any parameter values in \code{par} are invalid
//'   according to their constraints, or if any value in \code{data} is not in
//'   the interval (0, 1).
//'
//' @details
//' The probability density function (PDF) of the GKw distribution is given in
//' \code{\link{dgkw}}. The log-likelihood function \eqn{\ell(\theta)} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n\ln(\lambda\alpha\beta) - n\ln B(\gamma,\delta+1) +
//'   \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\gamma\lambda-1)\ln(w_i) + \delta\ln(z_i)]
//' }
//' where \eqn{\theta = (\alpha, \beta, \gamma, \delta, \lambda)}, \eqn{B(a,b)}
//' is the Beta function (\code{\link[base]{beta}}), and:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
//' }
//' This function computes \eqn{-\ell(\theta|\mathbf{x})}.
//'
//' Numerical stability is prioritized using:
//' \itemize{
//'   \item \code{\link[base]{lbeta}} function for the log-Beta term.
//'   \item Log-transformations of intermediate terms (\eqn{v_i, w_i, z_i}) and
//'         use of \code{\link[base]{log1p}} where appropriate to handle values
//'         close to 0 or 1 accurately.
//'   \item Checks for invalid parameters and data.
//' }
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{dgkw}}, \code{\link{pgkw}}, \code{\link{qgkw}}, \code{\link{rgkw}},
//' \code{\link{grgkw}}, \code{\link{hsgkw}} (gradient and Hessian functions, if available),
//' \code{\link[stats]{optim}}, \code{\link[base]{lbeta}}, \code{\link[base]{log1p}}
//'
//' @examples
//' \donttest{
//' # Generate sample data from a known GKw distribution
//' set.seed(123)
//' true_par <- c(alpha = 2, beta = 3, gamma = 1.0, delta = 0.5, lambda = 0.5)
//' sample_data <- rgkw(100, alpha = true_par[1], beta = true_par[2],
//'                    gamma = true_par[3], delta = true_par[4], lambda = true_par[5])
//' hist(sample_data, breaks = 20, main = "Sample GKw Data")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess (can be crucial)
//' start_par <- c(1.5, 2.5, 1.2, 0.3, 0.6)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' # Ensure data is passed correctly to llgkw
//' mle_result <- stats::optim(par = start_par,
//'                            fn = llgkw,
//'                            method = "BFGS", # Method supporting bounds might be safer
//'                            hessian = TRUE,
//'                            data = sample_data)
//'
//' # Check convergence and results
//' if (mle_result$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par <- mle_result$par
//'   print("Estimated parameters:")
//'   print(mle_par)
//'   print("True parameters:")
//'   print(true_par)
//'
//'   # Standard errors from Hessian (optional)
//'   # fisher_info <- solve(mle_result$hessian) # Need positive definite Hessian
//'   # std_errors <- sqrt(diag(fisher_info))
//'   # print("Approximate Standard Errors:")
//'   # print(std_errors)
//'
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires the 'numDeriv' package and analytical functions 'grgkw', 'hsgkw'
//' if (requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grgkw") && exists("hsgkw") && mle_result$convergence == 0) {
//'
//'   cat("\nComparing Derivatives at MLE estimates:\n")
//'
//'   # Numerical derivatives
//'   num_grad <- numDeriv::grad(func = llgkw, x = mle_par, data = sample_data)
//'   num_hess <- numDeriv::hessian(func = llgkw, x = mle_par, data = sample_data)
//'
//'   # Analytical derivatives (assuming they exist)
//'   # Note: grgkw/hsgkw might compute derivatives of log-likelihood,
//'   # while llgkw is negative log-likelihood. Adjust signs if needed.
//'   # Assuming grgkw/hsgkw compute derivatives of NEGATIVE log-likelihood here:
//'   ana_grad <- grgkw(par = mle_par, data = sample_data)
//'   ana_hess <- hsgkw(par = mle_par, data = sample_data)
//'
//'   # Check differences (should be small if analytical functions are correct)
//'   cat("Difference between numerical and analytical gradient:\n")
//'   print(summary(abs(num_grad - ana_grad)))
//'
//'   cat("Difference between numerical and analytical Hessian:\n")
//'   print(summary(abs(num_hess - ana_hess)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison.\n")
//'    cat("Requires 'numDeriv' package and functions 'grgkw', 'hsgkw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llgkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
// Parameter extraction
double alpha = par[0];   // Shape parameter α > 0
double beta = par[1];    // Shape parameter β > 0
double gamma = par[2];   // Shape parameter γ > 0
double delta = par[3];   // Shape parameter δ > 0
double lambda = par[4];  // Shape parameter λ > 0

// Parameter validation - all parameters must be positive
if (alpha <= 0 || beta <= 0 || gamma <= 0 || delta <= 0 || lambda <= 0) {
 return R_NegInf;  // Return negative infinity for invalid parameters
}

// Convert data to arma::vec for more efficient operations
// Use aliasing (false) to avoid copying the data
// arma::vec x(data.begin(), data.size(), false);
arma::vec x = Rcpp::as<arma::vec>(data);

// Data validation - all values must be in the range (0,1)
if (arma::any(x <= 0) || arma::any(x >= 1)) {
 return R_NegInf;  // Return negative infinity for invalid data
}

int n = x.n_elem;  // Sample size

// Calculate log of Beta function for constant term
// Use R's lbeta function for better numerical stability
double log_beta_term = R::lbeta(gamma, delta + 1);

// Calculate the constant term: n*log(λαβ/B(γ,δ+1))
double constant_term = n * (std::log(lambda) + std::log(alpha) + std::log(beta) - log_beta_term);

// Calculate log(x) and sum (α-1)*log(x) terms
arma::vec log_x = arma::log(x);
double term1 = arma::sum((alpha - 1.0) * log_x);

// Calculate v = 1-x^α and sum (β-1)*log(v) terms
arma::vec x_alpha = arma::pow(x, alpha);
arma::vec v = 1.0 - x_alpha;
arma::vec log_v = arma::log(v);
double term2 = arma::sum((beta - 1.0) * log_v);

// Calculate w = 1-v^β = 1-(1-x^α)^β and sum (γλ-1)*log(w) terms
arma::vec v_beta = arma::pow(v, beta);
arma::vec w = 1.0 - v_beta;

// Handle numerical stability for log(w) when w is close to zero
arma::vec log_w(n);
for (int i = 0; i < n; i++) {
 if (w(i) < 1e-10) {
   // Use log1p for numerical stability: log(w) = log(1-v^β) = log1p(-v^β)
   log_w(i) = std::log1p(-v_beta(i));
 } else {
   log_w(i) = std::log(w(i));
 }
}
double term3 = arma::sum((gamma * lambda - 1.0) * log_w);

// Calculate z = 1-w^λ = 1-[1-(1-x^α)^β]^λ and sum δ*log(z) terms
arma::vec w_lambda = arma::pow(w, lambda);
arma::vec z = 1.0 - w_lambda;

// Handle numerical stability for log(z) when z is close to zero
arma::vec log_z(n);
for (int i = 0; i < n; i++) {
 if (z(i) < 1e-10) {
   // Use log1p for numerical stability: log(z) = log(1-w^λ) = log1p(-w^λ)
   log_z(i) = std::log1p(-w_lambda(i));
 } else {
   log_z(i) = std::log(z(i));
 }
}
double term4 = arma::sum(delta * log_z);

// Return final minus-log-likelihood: constant term + sum of all individual terms
return -(constant_term + term1 + term2 + term3 + term4);
}



//' @title Gradient of the Negative Log-Likelihood for the GKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient
//'
//' @description
//' Computes the gradient vector (vector of partial derivatives) of the negative
//' log-likelihood function for the five-parameter Generalized Kumaraswamy (GKw)
//' distribution. This provides the analytical gradient, often used for efficient
//' optimization via maximum likelihood estimation.
//'
//' @param par A numeric vector of length 5 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 5 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter:
//'   \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \gamma, -\partial \ell/\partial \delta, -\partial \ell/\partial \lambda)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i) +
//' \sum_{i=1}^{n}\left[x_i^{\alpha} \ln(x_i) \left(\frac{\beta-1}{v_i} -
//' \frac{(\gamma\lambda-1) \beta v_i^{\beta-1}}{w_i} +
//' \frac{\delta \lambda \beta v_i^{\beta-1} w_i^{\lambda-1}}{z_i}\right)\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - \sum_{i=1}^{n}\ln(v_i) +
//' \sum_{i=1}^{n}\left[v_i^{\beta} \ln(v_i) \left(\frac{\gamma\lambda-1}{w_i} -
//' \frac{\delta \lambda w_i^{\lambda-1}}{z_i}\right)\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma) - \psi(\gamma+\delta+1)] -
//' \lambda\sum_{i=1}^{n}\ln(w_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \delta} = n[\psi(\delta+1) - \psi(\gamma+\delta+1)] -
//' \sum_{i=1}^{n}\ln(z_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} -
//' \gamma\sum_{i=1}^{n}\ln(w_i) + \delta\sum_{i=1}^{n}\frac{w_i^{\lambda}\ln(w_i)}{z_i}
//' }
//'
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
//'   \item \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
//' }
//'
//' Numerical stability is ensured through careful implementation, including checks
//' for valid inputs and handling of intermediate calculations involving potentially
//' small or large numbers, often leveraging the Armadillo C++ library for efficiency.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{llgkw}} (negative log-likelihood),
//' \code{\link{hsgkw}} (Hessian matrix),
//' \code{\link{dgkw}} (density),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
//' \code{\link[base]{digamma}}
//'
//' @examples
//' \donttest{
//' # Generate sample data from a known GKw distribution
//' set.seed(123)
//' true_par <- c(alpha = 2, beta = 3, gamma = 1.0, delta = 0.5, lambda = 0.5)
//' sample_data <- rgkw(100, alpha = true_par[1], beta = true_par[2],
//'                    gamma = true_par[3], delta = true_par[4], lambda = true_par[5])
//'
//' # --- Use in Optimization (e.g., with optim using analytical gradient) ---
//' start_par <- c(1.5, 2.5, 1.2, 0.3, 0.6) # Initial guess
//'
//' # Optimization using analytical gradient
//' mle_result_gr <- stats::optim(par = start_par,
//'                               fn = llgkw,  # Objective function (Negative LL)
//'                               gr = grgkw,  # Gradient function
//'                               method = "BFGS", # Method using gradient
//'                               hessian = TRUE,
//'                               data = sample_data)
//'
//' if (mle_result_gr$convergence == 0) {
//'   print("Optimization with analytical gradient converged.")
//'   mle_par_gr <- mle_result_gr$par
//'   print("Estimated parameters:")
//'   print(mle_par_gr)
//' } else {
//'   warning("Optimization with analytical gradient failed!")
//' }
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' # Requires the 'numDeriv' package
//' if (requireNamespace("numDeriv", quietly = TRUE) && mle_result_gr$convergence == 0) {
//'
//'   cat("\nComparing Gradients at MLE estimates:\n")
//'
//'   # Numerical gradient of the negative log-likelihood function
//'   num_grad <- numDeriv::grad(func = llgkw, x = mle_par_gr, data = sample_data)
//'
//'   # Analytical gradient (output of grgkw)
//'   ana_grad <- grgkw(par = mle_par_gr, data = sample_data)
//'
//'   cat("Numerical Gradient:\n")
//'   print(num_grad)
//'   cat("Analytical Gradient:\n")
//'   print(ana_grad)
//'
//'   # Check differences (should be small)
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad - ana_grad)))
//'
//' } else {
//'   cat("\nSkipping gradient comparison (requires 'numDeriv' package or convergence).\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grgkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
// Parameter extraction
double alpha = par[0];   // Shape parameter α > 0
double beta = par[1];    // Shape parameter β > 0
double gamma = par[2];   // Shape parameter γ > 0
double delta = par[3];   // Shape parameter δ > 0
double lambda = par[4];  // Shape parameter λ > 0

// Parameter validation
if (alpha <= 0 || beta <= 0 || gamma <= 0 || delta <= 0 || lambda <= 0) {
 Rcpp::NumericVector grad(5, R_NaN);
 return grad;
}

// Data conversion and validation
arma::vec x = Rcpp::as<arma::vec>(data);

if (arma::any(x <= 0) || arma::any(x >= 1)) {
 Rcpp::NumericVector grad(5, R_NaN);
 return grad;
}

int n = x.n_elem;  // Sample size

// Initialize gradient vector
Rcpp::NumericVector grad(5, 0.0);

// Small constant to avoid numerical issues
double eps = std::numeric_limits<double>::epsilon() * 100;

// Compute transformations and intermediate values
arma::vec log_x = arma::log(x);                // log(x_i)
arma::vec x_alpha = arma::pow(x, alpha);       // x_i^α
arma::vec x_alpha_log_x = x_alpha % log_x;     // x_i^α * log(x_i)

// v_i = 1 - x_i^α
arma::vec v = 1.0 - x_alpha;
v = arma::clamp(v, eps, 1.0 - eps);            // Prevent numerical issues

arma::vec log_v = arma::log(v);                // log(v_i)
arma::vec v_beta_m1 = arma::pow(v, beta - 1.0); // v_i^(β-1)
arma::vec v_beta = arma::pow(v, beta);          // v_i^β
arma::vec v_beta_log_v = v_beta % log_v;        // v_i^β * log(v_i)

// w_i = 1 - v_i^β = 1 - (1-x_i^α)^β
arma::vec w = 1.0 - v_beta;
w = arma::clamp(w, eps, 1.0 - eps);            // Prevent numerical issues

arma::vec log_w = arma::log(w);                // log(w_i)
arma::vec w_lambda_m1 = arma::pow(w, lambda - 1.0); // w_i^(λ-1)
arma::vec w_lambda = arma::pow(w, lambda);          // w_i^λ
arma::vec w_lambda_log_w = w_lambda % log_w;        // w_i^λ * log(w_i)

// z_i = 1 - w_i^λ = 1 - [1-(1-x_i^α)^β]^λ
arma::vec z = 1.0 - w_lambda;
z = arma::clamp(z, eps, 1.0 - eps);            // Prevent numerical issues

arma::vec log_z = arma::log(z);                // log(z_i)

// Calculate partial derivatives for each parameter (for log-likelihood)

// ∂ℓ/∂α = n/α + Σᵢlog(xᵢ) - Σᵢ[xᵢ^α * log(xᵢ) * ((β-1)/vᵢ - (γλ-1) * β * vᵢ^(β-1) / wᵢ + δ * λ * β * vᵢ^(β-1) * wᵢ^(λ-1) / zᵢ)]
double d_alpha = n / alpha + arma::sum(log_x);

// Calculate the complex term in the α gradient
arma::vec alpha_term2 = (beta - 1.0) / v;                // (β-1)/v_i
arma::vec alpha_term3 = (gamma * lambda - 1.0) * beta * v_beta_m1 / w;  // (γλ-1) * β * v_i^(β-1) / w_i
arma::vec alpha_term4 = delta * lambda * beta * v_beta_m1 % w_lambda_m1 / z;  // δ * λ * β * v_i^(β-1) * w_i^(λ-1) / z_i

d_alpha -= arma::sum(x_alpha_log_x % (alpha_term2 - alpha_term3 + alpha_term4));

// ∂ℓ/∂β = n/β + Σᵢlog(vᵢ) - Σᵢ[vᵢ^β * log(vᵢ) * ((γλ-1) / wᵢ - δ * λ * wᵢ^(λ-1) / zᵢ)]
double d_beta = n / beta + arma::sum(log_v);

// Calculate the complex term in the β gradient
arma::vec beta_term2 = (gamma * lambda - 1.0) / w;       // (γλ-1) / w_i
arma::vec beta_term3 = delta * lambda * w_lambda_m1 / z; // δ * λ * w_i^(λ-1) / z_i

d_beta -= arma::sum(v_beta_log_v % (beta_term2 - beta_term3));

// ∂ℓ/∂γ = -n[ψ(γ) - ψ(γ+δ+1)] + λΣᵢlog(wᵢ)
double d_gamma = -n * (R::digamma(gamma) - R::digamma(gamma + delta + 1)) + lambda * arma::sum(log_w);

// ∂ℓ/∂δ = -n[ψ(δ+1) - ψ(γ+δ+1)] + Σᵢlog(zᵢ)
double d_delta = -n * (R::digamma(delta + 1) - R::digamma(gamma + delta + 1)) + arma::sum(log_z);

// ∂ℓ/∂λ = n/λ + γΣᵢlog(wᵢ) - δΣᵢ[(wᵢ^λ*log(wᵢ))/zᵢ]
double d_lambda = n / lambda + gamma * arma::sum(log_w) - delta * arma::sum(w_lambda_log_w / z);

// Since we're optimizing negative log-likelihood, negate all derivatives
grad[0] = -d_alpha;
grad[1] = -d_beta;
grad[2] = -d_gamma;
grad[3] = -d_delta;
grad[4] = -d_lambda;

return grad;
}




//' @title Hessian Matrix of the Negative Log-Likelihood for the GKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian
//'
//' @description
//' Computes the analytic Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the five-parameter Generalized
//' Kumaraswamy (GKw) distribution. This is typically used to estimate standard
//' errors of maximum likelihood estimates or in optimization algorithms.
//'
//' @param par A numeric vector of length 5 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 5x5 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, i.e., the matrix of second partial
//'   derivatives \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)}.
//'   Returns a 5x5 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function based on the GKw PDF (see \code{\link{dgkw}}).
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} is given by:
//' \deqn{
//' \ell(\theta) = n \ln(\lambda\alpha\beta) - n \ln B(\gamma, \delta+1)
//' + \sum_{i=1}^{n} [(\alpha-1) \ln(x_i)
//' + (\beta-1) \ln(v_i)
//' + (\gamma\lambda - 1) \ln(w_i)
//' + \delta \ln(z_i)]
//' }
//' where \eqn{\theta = (\alpha, \beta, \gamma, \delta, \lambda)}, \eqn{B(a,b)}
//' is the Beta function (\code{\link[base]{beta}}), and intermediate terms are:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
//' }
//' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}.
//'
//' Key properties of the returned matrix:
//' \itemize{
//'   \item Dimensions: 5x5.
//'   \item Symmetry: The matrix is symmetric.
//'   \item Ordering: Rows and columns correspond to the parameters in the order
//'     \eqn{\alpha, \beta, \gamma, \delta, \lambda}.
//'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
//' }
//' The exact analytical formulas for the second derivatives are implemented
//' directly (often derived using symbolic differentiation) for accuracy and
//' efficiency, typically using C++.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//' @seealso
//' \code{\link{llgkw}} (negative log-likelihood function),
//' \code{\link{grgkw}} (gradient vector),
//' \code{\link{dgkw}} (density function),
//' \code{\link{gkwreg}} (if provides regression fitting),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
//'
//' @examples
//' \donttest{
//' # Generate sample data from a known GKw distribution
//' set.seed(123)
//' true_par <- c(alpha = 2, beta = 3, gamma = 1.0, delta = 0.5, lambda = 0.5)
//' sample_data <- rgkw(100, alpha = true_par[1], beta = true_par[2],
//'                    gamma = true_par[3], delta = true_par[4], lambda = true_par[5])
//'
//' # --- Find MLE estimates (e.g., using optim) ---
//' start_par <- c(1.5, 2.5, 1.2, 0.3, 0.6) # Initial guess
//' mle_result <- stats::optim(par = start_par,
//'                            fn = llgkw,
//'                            method = "BFGS",
//'                            hessian = TRUE, # Ask optim for numerical Hessian
//'                            data = sample_data)
//'
//' if (mle_result$convergence == 0) {
//'   mle_par <- mle_result$par
//'   print("MLE parameters found:")
//'   print(mle_par)
//'
//'   # --- Compare analytical Hessian to numerical Hessian ---
//'   # Requires the 'numDeriv' package
//'   if (requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'     cat("\nComparing Hessians at MLE estimates:\n")
//'
//'     # Numerical Hessian from numDeriv applied to negative LL function
//'     num_hess <- numDeriv::hessian(func = llgkw, x = mle_par, data = sample_data)
//'
//'     # Analytical Hessian (output of hsgkw)
//'     ana_hess <- hsgkw(par = mle_par, data = sample_data)
//'
//'     cat("Numerical Hessian (from numDeriv):\n")
//'     print(round(num_hess, 4))
//'     cat("Analytical Hessian (from hsgkw):\n")
//'     print(round(ana_hess, 4))
//'
//'     # Check differences (should be small)
//'     cat("Max absolute difference between Hessians:\n")
//'     print(max(abs(num_hess - ana_hess)))
//'
//'     # Optional: Use analytical Hessian to estimate standard errors
//'     # Ensure Hessian is positive definite before inverting
//'     # fisher_info_analytic <- ana_hess # Hessian of negative LL
//'     # tryCatch({
//'     #   cov_matrix <- solve(fisher_info_analytic)
//'     #   std_errors <- sqrt(diag(cov_matrix))
//'     #   cat("Std. Errors from Analytical Hessian:\n")
//'     #   print(std_errors)
//'     # }, error = function(e) {
//'     #   warning("Could not invert analytical Hessian to get standard errors: ", e$message)
//'     # })
//'
//'   } else {
//'     cat("\nSkipping Hessian comparison (requires 'numDeriv' package).\n")
//'   }
//' } else {
//'   warning("Optimization did not converge. Hessian comparison skipped.")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hsgkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
// Parameter extraction
double alpha  = par[0];   // θ[0] = α
double beta   = par[1];   // θ[1] = β
double gamma  = par[2];   // θ[2] = γ
double delta  = par[3];   // θ[3] = δ
double lambda = par[4];   // θ[4] = λ

// Simple parameter validation (all > 0)
if(alpha <= 0 || beta <= 0 || gamma <= 0 || delta <= 0 || lambda <= 0) {
 Rcpp::NumericMatrix nanH(5,5);
 nanH.fill(R_NaN);
 return nanH;
}

// Data conversion and basic validation
arma::vec x = Rcpp::as<arma::vec>(data);
if(arma::any(x <= 0) || arma::any(x >= 1)) {
 Rcpp::NumericMatrix nanH(5,5);
 nanH.fill(R_NaN);
 return nanH;
}

int n = x.n_elem;  // sample size

// Initialize Hessian matrix H (of ℓ(θ)) as 5x5
arma::mat H(5,5, arma::fill::zeros);

// --- CONSTANT TERMS (do not depend on x) ---
// L1: n ln(λ)  => d²/dλ² = -n/λ²
H(4,4) += - n/(lambda*lambda);
// L2: n ln(α)  => d²/dα² = -n/α²
H(0,0) += - n/(alpha*alpha);
// L3: n ln(β)  => d²/dβ² = -n/β²
H(1,1) += - n/(beta*beta);
// L4: - n ln[B(γ, δ+1)]
//   d²/dγ² = -n [ψ₁(γ) - ψ₁(γ+δ+1)]  where ψ₁ is the trigamma function
H(2,2) += - n * ( R::trigamma(gamma) - R::trigamma(gamma+delta+1) );
//   d²/dδ² = -n [ψ₁(δ+1) - ψ₁(γ+δ+1)]
H(3,3) += - n * ( R::trigamma(delta+1) - R::trigamma(gamma+delta+1) );
//   Mixed derivative (γ,δ): = n ψ₁(γ+δ+1)
H(2,3) += n * R::trigamma(gamma+delta+1);
H(3,2) = H(2,3);
// L5: (α-1) Σ ln(x_i)  --> contributes only to first derivatives

// Accumulators for mixed derivatives with λ
double acc_gamma_lambda = 0.0;  // Sum of ln(w)
double acc_delta_lambda = 0.0;  // Sum of dz_dlambda / z
double acc_alpha_lambda = 0.0;  // For α,λ contributions
double acc_beta_lambda = 0.0;   // For β,λ contributions

// --- TERMS THAT INVOLVE THE OBSERVATIONS ---
// Loop over each observation to accumulate contributions from:
// L6: (β-1) Σ ln(v), where v = 1 - x^α
// L7: (γλ-1) Σ ln(w), where w = 1 - v^β
// L8: δ Σ ln(z), where z = 1 - w^λ
for (int i = 0; i < n; i++) {
 double xi    = x(i);
 double ln_xi = std::log(xi);

 // -- Compute A = x^α and its derivatives --
 double A = std::pow(xi, alpha);                  // A = x^α
 double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
 double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²

 // -- v = 1 - A and its derivatives --
 double v = 1.0 - A;                              // v = 1 - x^α
 double ln_v = std::log(v);                       // ln(v)
 double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
 double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²

 // --- L6: (β-1) ln(v) ---
 // Second derivative w.r.t. α: (β-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
 double d2L6_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
 // Mixed derivative: d²L6/(dα dβ) = d/dβ[(β-1)*(dv_dalpha/v)] = (dv_dalpha/v)
 double d2L6_dalpha_dbeta = dv_dalpha / v;

 // --- L7: (γλ - 1) ln(w), where w = 1 - v^β ---
 double v_beta = std::pow(v, beta);               // v^β
 double w = 1.0 - v_beta;                         // w = 1 - v^β
 double ln_w = std::log(w);                       // ln(w)
 // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
 double dw_dv = -beta * std::pow(v, beta - 1.0);
 // Chain rule: dw/dα = dw/dv * dv/dα
 double dw_dalpha = dw_dv * dv_dalpha;
 // Second derivative w.r.t. α for L7:
 // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
 // Computing d²w/dα²:
 //   dw/dα = -β * v^(β-1)*dv_dalpha,
 //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
 double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
                                 + std::pow(v, beta-1.0) * d2v_dalpha2);
 double d2L7_dalpha2 = (gamma * lambda - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
 // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
 double dw_dbeta = -v_beta * ln_v;
 // Second derivative w.r.t. β for L7:
 // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
 double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
 double d2L7_dbeta2 = (gamma * lambda - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
 // Mixed derivative L7 (α,β): d²/(dα dβ) ln(w) =
 //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
 // Approximate d/dβ dw_dalpha:
 double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
 double d2L7_dalpha_dbeta = (gamma * lambda - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));

 // --- L8: δ ln(z), where z = 1 - w^λ ---
 double w_lambda_val = std::pow(w, lambda);       // w^λ
 double z = 1.0 - w_lambda_val;                   // z = 1 - w^λ
 // Derivative w.r.t. α: dz/dα = -λ * w^(λ-1) * dw/dα
 double dz_dalpha = -lambda * std::pow(w, lambda-1.0) * dw_dalpha;
 // Second derivative w.r.t. α for L8:
 // d²z/dα² = -λ * [(λ-1)*w^(λ-2)*(dw/dα)² + w^(λ-1)*d²w/dα²]
 double d2z_dalpha2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * (dw_dalpha*dw_dalpha)
                                   + std::pow(w, lambda-1.0) * d2w_dalpha2);
 double d2L8_dalpha2 = delta * ((d2z_dalpha2 * z - dz_dalpha*dz_dalpha)/(z*z));

 // Derivative w.r.t. β: dz/dβ = -λ * w^(λ-1) * dw/dβ
 double dz_dbeta = -lambda * std::pow(w, lambda-1.0) * dw_dbeta;
 // Second derivative w.r.t. β for L8:
 // d²z/dβ² = -λ * [(λ-1)*w^(λ-2)*(dw/dβ)² + w^(λ-1)*d²w/dβ²]
 double d2z_dbeta2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * (dw_dbeta*dw_dbeta)
                                  + std::pow(w, lambda-1.0) * d2w_dbeta2);
 double d2L8_dbeta2 = delta * ((d2z_dbeta2 * z - dz_dbeta*dz_dbeta)/(z*z));

 // Mixed derivative L8 (α,β): d²/(dα dβ) ln(z)
 // = (d/dβ dz_dalpha)/z - (dz_dalpha*dz_dbeta)/(z*z)
 // Approximate d/dβ dz_dalpha = -λ * [(λ-1)*w^(λ-2)*(dw_dβ*dw_dα) + w^(λ-1)*(d/dβ dw_dalpha)]
 double d_dw_dalpha_dbeta_2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * dw_dbeta * dw_dalpha
                                           + std::pow(w, lambda-1.0) * d_dw_dalpha_dbeta);
 double d2L8_dalpha_dbeta = delta * ((d_dw_dalpha_dbeta_2 / z) - (dz_dalpha*dz_dbeta)/(z*z));

 // Derivatives of L8 with respect to λ:
 // d/dλ ln(z) = (1/z)*dz/dλ, with dz/dλ = -w^λ ln(w)
 double dz_dlambda = -w_lambda_val * ln_w;
 // d²/dλ² ln(z) = [d²z/dλ² * z - (dz_dlambda)²]/z² (assuming w constant in λ)
 double d2z_dlambda2 = -w_lambda_val * (ln_w * ln_w);
 double d2L8_dlambda2 = delta * ((d2z_dlambda2 * z - dz_dlambda*dz_dlambda)/(z*z));

 // Mixed derivative L8 (α,λ): d²/(dα dλ) ln(z) = (d/dα dz_dλ)/z - (dz_dλ*dz_dalpha)/(z*z)
 // Correct formula: sum of two terms, not multiplication
 double d_dalpha_dz_dlambda = -std::pow(w, lambda-1.0) * dw_dalpha -
   lambda * ln_w * std::pow(w, lambda-1.0) * dw_dalpha;
 double d2L8_dalpha_dlambda = delta * ((d_dalpha_dz_dlambda / z) - (dz_dlambda*dz_dalpha)/(z*z));

 // Mixed derivative L8 (β,λ): d²/(dβ dλ) ln(z) = (d/dβ dz_dλ)/z - (dz_dlambda*dz_dbeta)/(z*z)
 // Correct formula: sum of two terms, not multiplication
 double d_dbeta_dz_dlambda = -std::pow(w, lambda-1.0) * dw_dbeta -
   lambda * ln_w * std::pow(w, lambda-1.0) * dw_dbeta;
 double d2L8_dbeta_dlambda = delta * ((d_dbeta_dz_dlambda / z) - (dz_dlambda*dz_dbeta)/(z*z));

 // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
 // Index: 0 = α, 1 = β, 2 = γ, 3 = δ, 4 = λ

 // H(α,α): sum of L2, L6, L7, and L8 (constants already added)
 H(0,0) += d2L6_dalpha2 + d2L7_dalpha2 + d2L8_dalpha2;

 // H(α,β): mixed from L6, L7, and L8
 H(0,1) += d2L6_dalpha_dbeta + d2L7_dalpha_dbeta + d2L8_dalpha_dbeta;
 H(1,0) = H(0,1);

 // H(β,β): contributions from L3, L7, and L8
 H(1,1) += d2L7_dbeta2 + d2L8_dbeta2;

 // H(λ,λ): contains L1 and L8 (L1 already added)
 H(4,4) += d2L8_dlambda2;

 // H(γ,α): from L7 - derivative of ln(w) in α multiplied by λ factor of (γλ-1)
 H(2,0) += lambda * (dw_dalpha / w);
 H(0,2) = H(2,0);

 // H(γ,β): from L7 - derivative of ln(w) in β multiplied by λ
 H(2,1) += lambda * (dw_dbeta / w);
 H(1,2) = H(2,1);

 // H(δ,α): L8 - mixed derivative: d/dα ln(z)
 H(3,0) += dz_dalpha / z;
 H(0,3) = H(3,0);

 // H(δ,β): L8 - d/dβ ln(z)
 H(3,1) += dz_dbeta / z;
 H(1,3) = H(3,1);

 // Accumulating terms for mixed derivatives with λ
 // (α,λ): Term from L7 (γ contribution) + term from L8 (δ contribution)
 double term1_alpha_lambda = gamma * (dw_dalpha / w);
 double term2_alpha_lambda = d2L8_dalpha_dlambda;
 acc_alpha_lambda += term1_alpha_lambda + term2_alpha_lambda;

 // (β,λ): Term from L7 (γ contribution) + term from L8 (δ contribution)
 double term1_beta_lambda = gamma * (dw_dbeta / w);
 double term2_beta_lambda = d2L8_dbeta_dlambda;
 acc_beta_lambda += term1_beta_lambda + term2_beta_lambda;

 // (γ,λ): Contribution from L7 (γλ-1)*ln(w)
 acc_gamma_lambda += ln_w;

 // (δ,λ): Contribution from L8 δ*ln(z)
 acc_delta_lambda += dz_dlambda / z;
} // end of loop

// Applying mixed derivatives with λ
// Note: All signs are positive for log-likelihood (not negative log-likelihood)

// H(α,λ): Positive sign for log-likelihood
H(0,4) = acc_alpha_lambda;
H(4,0) = H(0,4);

// H(β,λ): Positive sign for log-likelihood
H(1,4) = acc_beta_lambda;
H(4,1) = H(1,4);

// H(γ,λ): Positive sign for log-likelihood
H(2,4) = acc_gamma_lambda;
H(4,2) = H(2,4);

// H(δ,λ): Positive sign for log-likelihood
H(3,4) = acc_delta_lambda;
H(4,3) = H(3,4);

// Returns the analytic Hessian matrix of the log-likelihood
return Rcpp::wrap(-H);
}






/*
----------------------------------------------------------------------------
REUSE OF NUMERIC STABILITY FUNCTIONS AND CHECKS
----------------------------------------------------------------------------
NOTE: We assume the following inline functions are already available, exactly as in gkwdist.cpp:
- log1mexp(double)
- log1pexp(double)
- safe_log(double)
- safe_exp(double)
- safe_pow(double,double)
etc.

The kkw distribution here sets γ=1 in the GKw(α,β,γ=1,δ,λ). So we only keep α>0, β>0,
δ≥0, λ>0, ignoring γ. We'll define a small parameter checker:

check_kkw_pars(alpha,beta,delta,lambda) => boolean
*/

// -----------------------------------------------------------------------------
// Parameter Checker for kkw Distribution
// kkw(α, β, 1, δ, λ) => alpha>0, beta>0, delta≥0, λ>0
// -----------------------------------------------------------------------------
inline bool check_kkw_pars(double alpha,
                       double beta,
                       double delta,
                       double lambda,
                       bool strict = false) {
if (alpha <= 0.0 || beta <= 0.0 || delta < 0.0 || lambda <= 0.0) {
return false;
}
if (strict) {
const double MINP = 1e-5;
const double MAXP = 1e5;
if (alpha < MINP || beta < MINP || lambda < MINP) {
  return false;
}
if (alpha > MAXP || beta > MAXP || delta > MAXP || lambda > MAXP) {
  return false;
}
}
return true;
}

/*
----------------------------------------------------------------------------
KUMARASWAMY-KUMARASWAMY DISTRIBUTION  kkw(α, β, 1, δ, λ)
----------------------------------------------------------------------------
PDF:
f(x) = λ α β (δ+1) x^(α-1) (1 - x^α)^(β-1)
[1 - (1 - x^α)^β]^(λ - 1)
{1 - [1 - (1 - x^α)^β]^λ}^δ ,  0 < x < 1.

CDF:
F(x) = 1 - { 1 - [1 - (1 - x^α)^β]^λ }^(δ+1).

QUANTILE (inverse CDF):
Solve F(x)=p => x = ...
We get
y = [1 - (1 - x^α)^β]^λ,
F(x)=1 - (1-y)^(δ+1),
=> (1-y)^(δ+1) = 1-p
=> y = 1 - (1-p)^(1/(δ+1))
=> (1 - x^α)^β = 1 - y
=> x^α = 1 - (1-y)^(1/β)
=> x = [1 - (1-y)^(1/β)]^(1/α).
with y = 1 - (1-p)^(1/(δ+1)) all raised to 1/λ in the general GKw, but here it's directly [1 - (1-p)^(1/(δ+1))] since γ=1. Actually we must be consistent with the formula from the article. Let's confirm carefully:

The table in the user's message says:
F(x) = 1 - [1 - (1 - x^α)^β]^λ)^(δ+1),
=> (1 - [1-(1-x^α)^β]^λ)^(δ+1) = 1 - p
=> 1 - [1-(1-x^α)^β]^λ = 1 - p^(1/(δ+1)) is not correct. We must do it carefully:

F(x)=1 - [1 - y]^ (δ+1) with y=[1-(1 - x^α)^β]^λ.
=> [1 - y]^(δ+1) = 1 - p => 1-y = (1 - p)^(1/(δ+1)) => y=1 - (1-p)^(1/(δ+1)).
Then y^(1/λ) if we had a general GKw, but here "y" itself is already [1-(1-x^α)^β]^λ. So to invert that we do y^(1/λ). Indeed, so that part is needed because the exponent λ is still free. So let's define:

y = [1 - (1 - x^α)^β]^λ
=> y^(1/λ) = 1 - (1 - x^α)^β
=> (1 - x^α)^β = 1 - y^(1/λ)

Then (1 - x^α)= [1 - y^(1/λ)]^(1/β).
=> x^α=1 - [1 - y^(1/λ)]^(1/β).
=> x=[1 - [1 - y^(1/λ)]^(1/β)]^(1/α).

So the quantile formula is indeed:

Qkkw(p)= [ 1 - [ 1 - ( 1 - (1-p)^(1/(δ+1)) )^(1/λ }^(1/β ) ]^(1/α).

We'll code it carefully.

RNG:
In the user's table, the recommended approach is:
V ~ Uniform(0,1)
U = 1 - (1 - V)^(1/(δ+1))    ( that is the portion for the (δ+1) exponent )
X= [1 - [1 - U^(1/λ}]^(1/β)]^(1/α)

LOG-LIKELIHOOD:
log f(x) = log(λ) + log(α) + log(β) + log(δ+1)
+ (α-1)*log(x)
+ (β-1)*log(1 - x^α)
+ (λ-1)*log(1 - (1 - x^α)^β)
+ δ* log(1 - [1-(1 - x^α)^β]^λ).
Then sum across data, multiply n to the constants, etc.
*/


// -----------------------------------------------------------------------------
// 1) dkkw: PDF of kkw
// -----------------------------------------------------------------------------

//' @title Density of the Kumaraswamy-Kumaraswamy (kkw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution density
//'
//' @description
//' Computes the probability density function (PDF) for the Kumaraswamy-Kumaraswamy
//' (kkw) distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
//' This distribution is defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned (\eqn{\log(f(x))}). Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{alpha}, \code{beta},
//'   \code{delta}, \code{lambda}). Returns \code{0} (or \code{-Inf} if
//'   \code{log_prob = TRUE}) for \code{x} outside the interval (0, 1), or
//'   \code{NaN} if parameters are invalid (e.g., \code{alpha <= 0}, \code{beta <= 0},
//'   \code{delta < 0}, \code{lambda <= 0}).
//'
//' @details
//' The Kumaraswamy-Kumaraswamy (kkw) distribution is a special case of the
//' five-parameter Generalized Kumaraswamy distribution (\code{\link{dgkw}})
//' obtained by setting the parameter \eqn{\gamma = 1}.
//'
//' The probability density function is given by:
//' \deqn{
//' f(x; \alpha, \beta, \delta, \lambda) = (\delta + 1) \lambda \alpha \beta x^{\alpha - 1} (1 - x^\alpha)^{\beta - 1} \bigl[1 - (1 - x^\alpha)^\beta\bigr]^{\lambda - 1} \bigl\{1 - \bigl[1 - (1 - x^\alpha)^\beta\bigr]^\lambda\bigr\}^{\delta}
//' }
//' for \eqn{0 < x < 1}. Note that \eqn{1/(\delta+1)} corresponds to the Beta function
//' term \eqn{B(1, \delta+1)} when \eqn{\gamma=1}.
//'
//' Numerical evaluation follows similar stability considerations as \code{\link{dgkw}}.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{dgkw}} (parent distribution density),
//' \code{\link{pkkw}}, \code{\link{qkkw}}, \code{\link{rkkw}} (if they exist),
//' \code{\link[stats]{dbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' x_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' delta_par <- 0.5
//' lambda_par <- 1.5
//'
//' # Calculate density
//' densities <- dkkw(x_vals, alpha_par, beta_par, delta_par, lambda_par)
//' print(densities)
//'
//' # Calculate log-density
//' log_densities <- dkkw(x_vals, alpha_par, beta_par, delta_par, lambda_par,
//'                        log_prob = TRUE)
//' print(log_densities)
//' # Check: should match log(densities)
//' print(log(densities))
//'
//' # Compare with dgkw setting gamma = 1
//' densities_gkw <- dgkw(x_vals, alpha_par, beta_par, gamma = 1.0,
//'                       delta_par, lambda_par)
//' print(paste("Max difference:", max(abs(densities - densities_gkw)))) # Should be near zero
//'
//' # Plot the density
//' curve_x <- seq(0.01, 0.99, length.out = 200)
//' curve_y <- dkkw(curve_x, alpha_par, beta_par, delta_par, lambda_par)
//' plot(curve_x, curve_y, type = "l", main = "kkw Density Example",
//'      xlab = "x", ylab = "f(x)", col = "blue")
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dkkw(
 const arma::vec& x,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool log_prob = false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

// broadcast
size_t N = std::max({x.n_elem,
                   a_vec.n_elem,
                   b_vec.n_elem,
                   d_vec.n_elem,
                   l_vec.n_elem});

arma::vec out(N);
out.fill(log_prob ? R_NegInf : 0.0);

for (size_t i = 0; i < N; ++i) {
 double a = a_vec[i % a_vec.n_elem];
 double b = b_vec[i % b_vec.n_elem];
 double dd = d_vec[i % d_vec.n_elem];
 double ll = l_vec[i % l_vec.n_elem];
 double xx = x[i % x.n_elem];

 if (!check_kkw_pars(a, b, dd, ll)) {
   // invalid => 0 or -Inf
   continue;
 }
 // domain
 if (xx <= 0.0 || xx >= 1.0 || !R_finite(xx)) {
   continue;
 }

 // Precompute logs for PDF
 // log(f(x)) = log(λ) + log(α) + log(β) + log(δ+1)
 //            + (α-1)*log(x)
 //            + (β-1)*log(1 - x^α)
 //            + (λ-1)*log(1 - (1 - x^α)^β)
 //            + δ* log(1 - [1 - (1 - x^α)^β]^λ)
 double logCst = std::log(ll) + std::log(a) + std::log(b) + std::log(dd + 1.0);

 // x^alpha
 double lx = std::log(xx);
 double log_xalpha = a * lx;  // log(x^alpha)= alpha*log(x)
 // 1 - x^alpha
 double log_1_minus_xalpha = log1mexp(log_xalpha); // stable
 if (!R_finite(log_1_minus_xalpha)) {
   continue;
 }

 // (β-1)* log(1 - x^alpha)
 double term1 = (b - 1.0) * log_1_minus_xalpha;

 // let A = (1 - x^alpha)^β => logA = b * log_1_minus_xalpha
 double logA = b * log_1_minus_xalpha;
 double log_1_minusA = log1mexp(logA); // stable => log(1 - A)
 if (!R_finite(log_1_minusA)) {
   continue;
 }
 // (λ-1)* log(1 - A)
 double term2 = (ll - 1.0) * log_1_minusA;

 // let B = [1 - (1 - x^alpha)^β]^λ => logB = λ*log(1 - A)
 double logB = ll * log_1_minusA;
 double log_1_minus_B = log1mexp(logB);
 if (!R_finite(log_1_minus_B)) {
   continue;
 }
 // δ * log( 1 - B )
 double term3 = dd * log_1_minus_B;

 double log_pdf = logCst
 + (a - 1.0)*lx
 + term1
 + term2
 + term3;

 if (log_prob) {
   out(i) = log_pdf;
 } else {
   out(i) = std::exp(log_pdf);
 }
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 2) pkkw: CDF of kkw
// -----------------------------------------------------------------------------

//' @title Cumulative Distribution Function (CDF) of the kkw Distribution
//' @author Lopes, J. E.
//' @keywords distribution cumulative
//'
//' @description
//' Computes the cumulative distribution function (CDF), \eqn{P(X \le q)}, for the
//' Kumaraswamy-Kumaraswamy (kkw) distribution with parameters \code{alpha}
//' (\eqn{\alpha}), \code{beta} (\eqn{\beta}), \code{delta} (\eqn{\delta}),
//' and \code{lambda} (\eqn{\lambda}). This distribution is defined on the
//' interval (0, 1).
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms/complements
//'   depending on \code{lower_tail} and \code{log_p}. The length of the result
//'   is determined by the recycling rule applied to the arguments (\code{q},
//'   \code{alpha}, \code{beta}, \code{delta}, \code{lambda}). Returns \code{0}
//'   (or \code{-Inf} if \code{log_p = TRUE}) for \code{q <= 0} and \code{1}
//'   (or \code{0} if \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN}
//'   for invalid parameters.
//'
//' @details
//' The Kumaraswamy-Kumaraswamy (kkw) distribution is a special case of the
//' five-parameter Generalized Kumaraswamy distribution (\code{\link{pgkw}})
//' obtained by setting the shape parameter \eqn{\gamma = 1}.
//'
//' The CDF of the GKw distribution is \eqn{F_{GKw}(q) = I_{y(q)}(\gamma, \delta+1)},
//' where \eqn{y(q) = [1-(1-q^{\alpha})^{\beta}]^{\lambda}} and \eqn{I_x(a,b)}
//' is the regularized incomplete beta function (\code{\link[stats]{pbeta}}).
//' Setting \eqn{\gamma=1} utilizes the property \eqn{I_x(1, b) = 1 - (1-x)^b},
//' yielding the kkw CDF:
//' \deqn{
//' F(q; \alpha, \beta, \delta, \lambda) = 1 - \bigl\{1 - \bigl[1 - (1 - q^\alpha)^\beta\bigr]^\lambda\bigr\}^{\delta + 1}
//' }
//' for \eqn{0 < q < 1}.
//'
//' The implementation uses this closed-form expression for efficiency and handles
//' \code{lower_tail} and \code{log_p} arguments appropriately.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{pgkw}} (parent distribution CDF),
//' \code{\link{dkkw}}, \code{\link{qkkw}}, \code{\link{rkkw}},
//' \code{\link[stats]{pbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' q_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' delta_par <- 0.5
//' lambda_par <- 1.5
//'
//' # Calculate CDF P(X <= q)
//' probs <- pkkw(q_vals, alpha_par, beta_par, delta_par, lambda_par)
//' print(probs)
//'
//' # Calculate upper tail P(X > q)
//' probs_upper <- pkkw(q_vals, alpha_par, beta_par, delta_par, lambda_par,
//'                      lower_tail = FALSE)
//' print(probs_upper)
//' # Check: probs + probs_upper should be 1
//' print(probs + probs_upper)
//'
//' # Calculate log CDF
//' log_probs <- pkkw(q_vals, alpha_par, beta_par, delta_par, lambda_par,
//'                    log_p = TRUE)
//' print(log_probs)
//' # Check: should match log(probs)
//' print(log(probs))
//'
//' # Compare with pgkw setting gamma = 1
//' probs_gkw <- pgkw(q_vals, alpha_par, beta_par, gamma = 1.0,
//'                   delta_par, lambda_par)
//' print(paste("Max difference:", max(abs(probs - probs_gkw)))) # Should be near zero
//'
//' # Plot the CDF
//' curve_q <- seq(0.01, 0.99, length.out = 200)
//' curve_p <- pkkw(curve_q, alpha_par, beta_par, delta_par, lambda_par)
//' plot(curve_q, curve_p, type = "l", main = "kkw CDF Example",
//'      xlab = "q", ylab = "F(q)", col = "blue", ylim = c(0, 1))
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pkkw(
 const arma::vec& q,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N = std::max({q.n_elem,
                   a_vec.n_elem,
                   b_vec.n_elem,
                   d_vec.n_elem,
                   l_vec.n_elem});

arma::vec out(N);

for (size_t i = 0; i < N; ++i) {
 double a = a_vec[i % a_vec.n_elem];
 double b = b_vec[i % b_vec.n_elem];
 double dd = d_vec[i % d_vec.n_elem];
 double ll = l_vec[i % l_vec.n_elem];
 double xx = q[i % q.n_elem];

 if (!check_kkw_pars(a, b, dd, ll)) {
   out(i) = NA_REAL;
   continue;
 }

 // boundaries
 if (!R_finite(xx) || xx <= 0.0) {
   // F(0) = 0
   double val0 = (lower_tail ? 0.0 : 1.0);
   out(i) = log_p ? std::log(val0) : val0;
   continue;
 }
 if (xx >= 1.0) {
   // F(1)=1
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = log_p ? std::log(val1) : val1;
   continue;
 }

 // x^alpha
 double lx = std::log(xx);
 double log_xalpha = a * lx;
 double xalpha = std::exp(log_xalpha);

 double one_minus_xalpha = 1.0 - xalpha;
 if (one_minus_xalpha <= 0.0) {
   // near 1 => F ~ 1
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = log_p ? std::log(val1) : val1;
   continue;
 }
 // (1 - x^alpha)^beta => ...
 double vbeta = std::pow(one_minus_xalpha, b);
 double y = 1.0 - vbeta;  // [1-(1 - x^alpha)^β]
 if (y <= 0.0) {
   // => F=0
   double val0 = (lower_tail ? 0.0 : 1.0);
   out(i) = log_p ? std::log(val0) : val0;
   continue;
 }
 if (y >= 1.0) {
   // => F=1
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = log_p ? std::log(val1) : val1;
   continue;
 }

 double ylambda = std::pow(y, ll);   // [1-(1-x^alpha)^β]^λ
 if (ylambda <= 0.0) {
   // => F=0
   double val0 = (lower_tail ? 0.0 : 1.0);
   out(i) = log_p ? std::log(val0) : val0;
   continue;
 }
 if (ylambda >= 1.0) {
   // => F=1
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = log_p ? std::log(val1) : val1;
   continue;
 }

 double outer = 1.0 - ylambda; // 1 - ...
 double cdfval = 1.0 - std::pow(outer, dd+1.0);

 if (!lower_tail) {
   cdfval = 1.0 - cdfval;
 }
 if (log_p) {
   cdfval = std::log(cdfval);
 }
 out(i) = cdfval;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 3) qkkw: Quantile of kkw
// -----------------------------------------------------------------------------

//' @title Quantile Function of the Kumaraswamy-Kumaraswamy (kkw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution quantile
//'
//' @description
//' Computes the quantile function (inverse CDF) for the Kumaraswamy-Kumaraswamy
//' (kkw) distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
//' It finds the value \code{q} such that \eqn{P(X \le q) = p}. This distribution
//' is a special case of the Generalized Kumaraswamy (GKw) distribution where
//' the parameter \eqn{\gamma = 1}.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are \eqn{p = P(X \le q)},
//'   otherwise, probabilities are \eqn{p = P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{alpha}, \code{beta}, \code{delta},
//'   \code{lambda}). Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{alpha <= 0},
//'           \code{beta <= 0}, \code{delta < 0}, \code{lambda <= 0}).
//'   }
//'   Boundary return values are adjusted accordingly for \code{lower_tail = FALSE}.
//'
//' @details
//' The quantile function \eqn{Q(p)} is the inverse of the CDF \eqn{F(q)}. The CDF
//' for the kkw (\eqn{\gamma=1}) distribution is (see \code{\link{pkkw}}):
//' \deqn{
//' F(q) = 1 - \bigl\{1 - \bigl[1 - (1 - q^\alpha)^\beta\bigr]^\lambda\bigr\}^{\delta + 1}
//' }
//' Inverting this equation for \eqn{q} yields the quantile function:
//' \deqn{
//' Q(p) = \left[ 1 - \left\{ 1 - \left[ 1 - (1 - p)^{1/(\delta+1)} \right]^{1/\lambda} \right\}^{1/\beta} \right]^{1/\alpha}
//' }
//' The function uses this closed-form expression and correctly handles the
//' \code{lower_tail} and \code{log_p} arguments by transforming \code{p}
//' appropriately before applying the formula.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{qgkw}} (parent distribution quantile function),
//' \code{\link{dkkw}}, \code{\link{pkkw}}, \code{\link{rkkw}},
//' \code{\link[stats]{qbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' p_vals <- c(0.1, 0.5, 0.9)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' delta_par <- 0.5
//' lambda_par <- 1.5
//'
//' # Calculate quantiles
//' quantiles <- qkkw(p_vals, alpha_par, beta_par, delta_par, lambda_par)
//' print(quantiles)
//'
//' # Calculate quantiles for upper tail probabilities P(X > q) = p
//' # e.g., for p=0.1, find q such that P(X > q) = 0.1 (90th percentile)
//' quantiles_upper <- qkkw(p_vals, alpha_par, beta_par, delta_par, lambda_par,
//'                          lower_tail = FALSE)
//' print(quantiles_upper)
//' # Check: qkkw(p, ..., lt=F) == qkkw(1-p, ..., lt=T)
//' print(qkkw(1 - p_vals, alpha_par, beta_par, delta_par, lambda_par))
//'
//' # Calculate quantiles from log probabilities
//' log_p_vals <- log(p_vals)
//' quantiles_logp <- qkkw(log_p_vals, alpha_par, beta_par, delta_par, lambda_par,
//'                         log_p = TRUE)
//' print(quantiles_logp)
//' # Check: should match original quantiles
//' print(quantiles)
//'
//' # Compare with qgkw setting gamma = 1
//' quantiles_gkw <- qgkw(p_vals, alpha_par, beta_par, gamma = 1.0,
//'                       delta_par, lambda_par)
//' print(paste("Max difference:", max(abs(quantiles - quantiles_gkw)))) # Should be near zero
//'
//' # Verify inverse relationship with pkkw
//' p_check <- 0.75
//' q_calc <- qkkw(p_check, alpha_par, beta_par, delta_par, lambda_par)
//' p_recalc <- pkkw(q_calc, alpha_par, beta_par, delta_par, lambda_par)
//' print(paste("Original p:", p_check, " Recalculated p:", p_recalc))
//' # abs(p_check - p_recalc) < 1e-9 # Should be TRUE
//'
//' # Boundary conditions
//' print(qkkw(c(0, 1), alpha_par, beta_par, delta_par, lambda_par)) # Should be 0, 1
//' print(qkkw(c(-Inf, 0), alpha_par, beta_par, delta_par, lambda_par, log_p = TRUE)) # Should be 0, 1
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qkkw(
 const arma::vec& p,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N = std::max({p.n_elem,
                   a_vec.n_elem,
                   b_vec.n_elem,
                   d_vec.n_elem,
                   l_vec.n_elem});

arma::vec out(N);

for (size_t i = 0; i < N; ++i) {
 double a = a_vec[i % a_vec.n_elem];
 double b = b_vec[i % b_vec.n_elem];
 double dd = d_vec[i % d_vec.n_elem];
 double ll = l_vec[i % l_vec.n_elem];
 double pp = p[i % p.n_elem];

 if (!check_kkw_pars(a, b, dd, ll)) {
   out(i) = NA_REAL;
   continue;
 }

 // Convert p if log_p
 if (log_p) {
   if (pp > 0.0) {
     // log(p)>0 => p>1 => invalid
     out(i) = NA_REAL;
     continue;
   }
   pp = std::exp(pp);
 }
 // if upper tail
 if (!lower_tail) {
   pp = 1.0 - pp;
 }

 // boundaries
 if (pp <= 0.0) {
   out(i) = 0.0;
   continue;
 }
 if (pp >= 1.0) {
   out(i) = 1.0;
   continue;
 }

 // formula:
 // F(x)=p => 1 - [1 - (1 - x^α)^β]^λ]^(δ+1) = p
 // => [1 - (1 - x^α)^β]^λ = 1 - (1-p)^(1/(δ+1))
 double tmp1 = 1.0 - std::pow(1.0 - pp, 1.0/(dd+1.0));
 if (tmp1 < 0.0)  tmp1=0.0;
 if (tmp1>1.0)    tmp1=1.0;  // safety

 // let T= tmp1^(1/λ)
 double T;
 if (ll==1.0) {
   T=tmp1;
 } else {
   T=std::pow(tmp1, 1.0/ll);
 }
 // => (1 - x^α)^β = 1 - T
 double M=1.0 - T;
 if (M<0.0)  M=0.0;
 if (M>1.0)  M=1.0;
 // => 1 - x^α= M^(1/β)
 double Mpow= std::pow(M, 1.0/b);
 double xalpha=1.0 - Mpow;
 if (xalpha<0.0)  xalpha=0.0;
 if (xalpha>1.0)  xalpha=1.0;

 // x= xalpha^(1/α) => actually => x= [1 - M^(1/β)]^(1/α)
 double xx;
 if (a==1.0) {
   xx=xalpha;
 } else {
   xx=std::pow(xalpha, 1.0/a);
 }

 if (xx<0.0)  xx=0.0;
 if (xx>1.0)  xx=1.0;

 out(i)= xx;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 4) rkkw: RNG for kkw
// -----------------------------------------------------------------------------

//' @title Random Number Generation for the kkw Distribution
//' @author Lopes, J. E.
//' @keywords distribution random
//'
//' @description
//' Generates random deviates from the Kumaraswamy-Kumaraswamy (kkw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
//' This distribution is a special case of the Generalized Kumaraswamy (GKw)
//' distribution where the parameter \eqn{\gamma = 1}.
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//'
//' @return A vector of length \code{n} containing random deviates from the kkw
//'   distribution. The length of the result is determined by \code{n} and the
//'   recycling rule applied to the parameters (\code{alpha}, \code{beta},
//'   \code{delta}, \code{lambda}). Returns \code{NaN} if parameters
//'   are invalid (e.g., \code{alpha <= 0}, \code{beta <= 0}, \code{delta < 0},
//'   \code{lambda <= 0}).
//'
//' @details
//' The generation method uses the inverse transform method based on the quantile
//' function (\code{\link{qkkw}}). The kkw quantile function is:
//' \deqn{
//' Q(p) = \left[ 1 - \left\{ 1 - \left[ 1 - (1 - p)^{1/(\delta+1)} \right]^{1/\lambda} \right\}^{1/\beta} \right]^{1/\alpha}
//' }
//' Random deviates are generated by evaluating \eqn{Q(p)} where \eqn{p} is a
//' random variable following the standard Uniform distribution on (0, 1)
//' (\code{\link[stats]{runif}}).
//'
//' This is equivalent to the general method for the GKw distribution
//' (\code{\link{rgkw}}) specialized for \eqn{\gamma=1}. The GKw method generates
//' \eqn{W \sim \mathrm{Beta}(\gamma, \delta+1)} and then applies transformations.
//' When \eqn{\gamma=1}, \eqn{W \sim \mathrm{Beta}(1, \delta+1)}, which can be
//' generated via \eqn{W = 1 - V^{1/(\delta+1)}} where \eqn{V \sim \mathrm{Unif}(0,1)}.
//' Substituting this \eqn{W} into the GKw transformation yields the same result
//' as evaluating \eqn{Q(1-V)} above (noting \eqn{p = 1-V} is also Uniform).
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' Devroye, L. (1986). *Non-Uniform Random Variate Generation*. Springer-Verlag.
//' (General methods for random variate generation).
//'
//' @seealso
//' \code{\link{rgkw}} (parent distribution random generation),
//' \code{\link{dkkw}}, \code{\link{pkkw}}, \code{\link{qkkw}},
//' \code{\link[stats]{runif}}, \code{\link[stats]{rbeta}}
//'
//' @examples
//' \donttest{
//' set.seed(2025) # for reproducibility
//'
//' # Generate 1000 random values from a specific kkw distribution
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' delta_par <- 0.5
//' lambda_par <- 1.5
//'
//' x_sample_kkw <- rkkw(1000, alpha = alpha_par, beta = beta_par,
//'                        delta = delta_par, lambda = lambda_par)
//' summary(x_sample_kkw)
//'
//' # Histogram of generated values compared to theoretical density
//' hist(x_sample_kkw, breaks = 30, freq = FALSE, # freq=FALSE for density
//'      main = "Histogram of kkw Sample", xlab = "x", ylim = c(0, 3.5))
//' curve(dkkw(x, alpha = alpha_par, beta = beta_par, delta = delta_par,
//'             lambda = lambda_par),
//'       add = TRUE, col = "red", lwd = 2, n = 201)
//' legend("topright", legend = "Theoretical PDF", col = "red", lwd = 2, bty = "n")
//'
//' # Comparing empirical and theoretical quantiles (Q-Q plot)
//' prob_points <- seq(0.01, 0.99, by = 0.01)
//' theo_quantiles <- qkkw(prob_points, alpha = alpha_par, beta = beta_par,
//'                         delta = delta_par, lambda = lambda_par)
//' emp_quantiles <- quantile(x_sample_kkw, prob_points, type = 7) # type 7 is default
//'
//' plot(theo_quantiles, emp_quantiles, pch = 16, cex = 0.8,
//'      main = "Q-Q Plot for kkw Distribution",
//'      xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles (n=1000)")
//' abline(a = 0, b = 1, col = "blue", lty = 2)
//'
//' # Compare summary stats with rgkw(..., gamma=1, ...)
//' # Note: individual values will differ due to randomness
//' x_sample_gkw <- rgkw(1000, alpha = alpha_par, beta = beta_par, gamma = 1.0,
//'                      delta = delta_par, lambda = lambda_par)
//' print("Summary stats for rkkw sample:")
//' print(summary(x_sample_kkw))
//' print("Summary stats for rgkw(gamma=1) sample:")
//' print(summary(x_sample_gkw)) # Should be similar
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rkkw(
 int n,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda
) {
if (n<=0) {
 Rcpp::stop("rkkw: n must be positive");
}

arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t k= std::max({a_vec.n_elem, b_vec.n_elem, d_vec.n_elem, l_vec.n_elem});
arma::vec out(n);

for (int i=0; i<n; i++) {
 size_t idx= i % k;
 double a= a_vec[idx % a_vec.n_elem];
 double b= b_vec[idx % b_vec.n_elem];
 double dd= d_vec[idx % d_vec.n_elem];
 double ll= l_vec[idx % l_vec.n_elem];

 if (!check_kkw_pars(a,b,dd,ll)) {
   out(i)= NA_REAL;
   Rcpp::warning("rkkw: invalid parameters at index %d", i+1);
   continue;
 }

 double V= R::runif(0.0,1.0);
 // U=1 - (1 - V)^(1/(δ+1))
 double U = 1.0 - std::pow(1.0 - V, 1.0/(dd+1.0));
 if (U<0.0)  U=0.0;
 if (U>1.0)  U=1.0;

 // x = {1 - [1 - U^(1/λ}]^(1/β)}^(1/α)
 double u_pow;
 if (ll==1.0) {
   u_pow=U;
 } else {
   u_pow=std::pow(U, 1.0/ll);
 }
 double bracket= 1.0- u_pow;
 if (bracket<0.0) bracket=0.0;
 if (bracket>1.0) bracket=1.0;
 double bracket2= std::pow(bracket, 1.0/b);
 double xalpha= 1.0 - bracket2;
 if (xalpha<0.0) xalpha=0.0;
 if (xalpha>1.0) xalpha=1.0;

 double xx;
 if (a==1.0) {
   xx=xalpha;
 } else {
   xx= std::pow(xalpha, 1.0/a);
   if (!R_finite(xx) || xx<0.0) xx=0.0;
   if (xx>1.0) xx=1.0;
 }
 out(i)=xx;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}





//' @title Negative Log-Likelihood for the kkw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize
//'
//' @description
//' Computes the negative log-likelihood function for the Kumaraswamy-Kumaraswamy
//' (kkw) distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}),
//' given a vector of observations. This distribution is a special case of the
//' Generalized Kumaraswamy (GKw) distribution where \eqn{\gamma = 1}.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{delta} (\eqn{\delta \ge 0}), \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
//'   if any parameter values in \code{par} are invalid according to their
//'   constraints, or if any value in \code{data} is not in the interval (0, 1).
//'
//' @details
//' The kkw distribution is the GKw distribution (\code{\link{dgkw}}) with \eqn{\gamma=1}.
//' Its probability density function (PDF) is:
//' \deqn{
//' f(x | \theta) = (\delta + 1) \lambda \alpha \beta x^{\alpha - 1} (1 - x^\alpha)^{\beta - 1} \bigl[1 - (1 - x^\alpha)^\beta\bigr]^{\lambda - 1} \bigl\{1 - \bigl[1 - (1 - x^\alpha)^\beta\bigr]^\lambda\bigr\}^{\delta}
//' }
//' for \eqn{0 < x < 1} and \eqn{\theta = (\alpha, \beta, \delta, \lambda)}.
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\delta+1) + \ln(\lambda) + \ln(\alpha) + \ln(\beta)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i) + \delta\ln(z_i)]
//' }
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
//' }
//' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
//' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
//' Numerical stability is maintained similarly to \code{\link{llgkw}}.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{llgkw}} (parent distribution negative log-likelihood),
//' \code{\link{dkkw}}, \code{\link{pkkw}}, \code{\link{qkkw}}, \code{\link{rkkw}},
//' \code{\link{grkkw}} (gradient, if available),
//' \code{\link{hskkw}} (Hessian, if available),
//' \code{\link[stats]{optim}}
//'
//' @examples
//' \donttest{
//' # Assuming existence of rkkw, grkkw, hskkw functions for kkw distribution
//'
//' # Generate sample data from a known kkw distribution
//' set.seed(123)
//' true_par_kkw <- c(alpha = 2, beta = 3, delta = 1.5, lambda = 0.5)
//' # Use rkkw if it exists, otherwise use rgkw with gamma=1
//' if (exists("rkkw")) {
//'   sample_data_kkw <- rkkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
//'                          delta = true_par_kkw[3], lambda = true_par_kkw[4])
//' } else {
//'   sample_data_kkw <- rgkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
//'                          gamma = 1, delta = true_par_kkw[3], lambda = true_par_kkw[4])
//' }
//' hist(sample_data_kkw, breaks = 20, main = "kkw(2, 3, 1.5, 0.5) Sample")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess
//' start_par_kkw <- c(1.5, 2.5, 1.0, 0.6)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' mle_result_kkw <- stats::optim(par = start_par_kkw,
//'                                fn = llkkw, # Use the kkw neg-log-likelihood
//'                                method = "BFGS",
//'                                hessian = TRUE,
//'                                data = sample_data_kkw)
//'
//' # Check convergence and results
//' if (mle_result_kkw$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par_kkw <- mle_result_kkw$par
//'   print("Estimated kkw parameters:")
//'   print(mle_par_kkw)
//'   print("True kkw parameters:")
//'   print(true_par_kkw)
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result_kkw$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires 'numDeriv' package and analytical functions 'grkkw', 'hskkw'
//' if (mle_result_kkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grkkw") && exists("hskkw")) {
//'
//'   cat("\nComparing Derivatives at kkw MLE estimates:\n")
//'
//'   # Numerical derivatives of llkkw
//'   num_grad_kkw <- numDeriv::grad(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
//'   num_hess_kkw <- numDeriv::hessian(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
//'
//'   # Analytical derivatives (assuming they return derivatives of negative LL)
//'   ana_grad_kkw <- grkkw(par = mle_par_kkw, data = sample_data_kkw)
//'   ana_hess_kkw <- hskkw(par = mle_par_kkw, data = sample_data_kkw)
//'
//'   # Check differences
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad_kkw - ana_grad_kkw)))
//'   cat("Max absolute difference between Hessians:\n")
//'   print(max(abs(num_hess_kkw - ana_hess_kkw)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison for kkw.\n")
//'    cat("Requires convergence, 'numDeriv' package and functions 'grkkw', 'hskkw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llkkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 4) return R_PosInf;

 double alpha = par[0];
 double beta = par[1];
 double delta = par[2];
 double lambda = par[3];

 if (!check_kkw_pars(alpha, beta, delta, lambda)) return R_PosInf;

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0.0) || arma::any(x >= 1.0)) return R_PosInf;

 int n = x.n_elem;

 // Stability constants
 const double min_eps = std::numeric_limits<double>::min() * 1e4;
 const double eps = 1e-10;
 // const double exp_threshold = -700.0;

 // Special case optimization: when delta = 0, use EKw implementation
 bool is_ekw = (delta < min_eps);

 // Safe parameter logs
 double log_alpha = safe_log(alpha);
 double log_beta = safe_log(beta);
 double log_lambda = safe_log(lambda);
 double log_delta_plus_1 = is_ekw ? 0.0 : std::log1p(delta);

 // Constant term
 double const_term = n * (log_lambda + log_alpha + log_beta + log_delta_plus_1);

 // Initialize component accumulators
 double sum_term1 = 0.0;  // (alpha-1) * sum(log(x))
 double sum_term2 = 0.0;  // (beta-1) * sum(log(1-x^alpha))
 double sum_term3 = 0.0;  // (lambda-1) * sum(log(1-(1-x^alpha)^beta))
 double sum_term4 = 0.0;  // delta * sum(log(1-[1-(1-x^alpha)^beta]^lambda))

 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Handle boundary cases
   if (xi < eps) xi = eps;
   if (xi > 1.0 - eps) xi = 1.0 - eps;

   double log_xi = std::log(xi);
   sum_term1 += (alpha - 1.0) * log_xi;

   // Calculate x^alpha stably
   double x_alpha, log_x_alpha;
   if (alpha * std::abs(log_xi) > 1.0) {
     // Use log-domain for potential overflow/underflow
     log_x_alpha = alpha * log_xi;
     x_alpha = std::exp(log_x_alpha);
   } else {
     x_alpha = std::pow(xi, alpha);
     log_x_alpha = std::log(x_alpha);
   }

   // Calculate v = 1-x^alpha stably
   double v, log_v;
   if (x_alpha > 0.9999) {
     // Use complementary calculation for x^alpha near 1
     v = -std::expm1(log_x_alpha);
     log_v = std::log(v);
   } else {
     v = 1.0 - x_alpha;
     log_v = log1p(-x_alpha); // More accurate than log(1-x^alpha)
   }

   // Ensure v is within valid range
   if (v < min_eps) {
     v = min_eps;
     log_v = std::log(v);
   }

   sum_term2 += (beta - 1.0) * log_v;

   // Calculate v^beta stably
   double v_beta, log_v_beta;
   if (beta * std::abs(log_v) > 1.0) {
     // Use log-domain for potential overflow/underflow
     log_v_beta = beta * log_v;
     v_beta = std::exp(log_v_beta);
   } else {
     v_beta = std::pow(v, beta);
     log_v_beta = std::log(v_beta);
   }

   // Calculate w = 1-v^beta stably
   double w, log_w;
   if (v_beta > 0.9999) {
     // Use complementary calculation for v^beta near 1
     w = -std::expm1(log_v_beta);
     log_w = std::log(w);
   } else {
     w = 1.0 - v_beta;
     log_w = log1p(-v_beta); // More accurate than log(1-v^beta)
   }

   // Ensure w is within valid range
   if (w < min_eps) {
     w = min_eps;
     log_w = std::log(w);
   }

   // Critical lambda handling - carefully treat lambda near 1
   if (std::abs(lambda - 1.0) < 1e-6) {
     if (lambda > 1.0) {
       // Use Taylor approximation for (lambda-1)*log(w) when lambda ≈ 1+
       sum_term3 += (lambda - 1.0) * log_w;
     } else if (lambda < 1.0) {
       // Use Taylor approximation for (lambda-1)*log(w) when lambda ≈ 1-
       sum_term3 += (lambda - 1.0) * log_w;
     }
     // When lambda = 1 exactly, term is zero
   } else {
     sum_term3 += (lambda - 1.0) * log_w;
   }

   // Skip last term calculation if delta ≈ 0 (EKw case)
   if (!is_ekw) {
     // Calculate w^lambda stably
     double w_lambda, log_w_lambda;
     if (lambda * std::abs(log_w) > 1.0) {
       // Use log-domain for potential overflow/underflow
       log_w_lambda = lambda * log_w;
       w_lambda = std::exp(log_w_lambda);
     } else {
       w_lambda = std::pow(w, lambda);
       log_w_lambda = std::log(w_lambda);
     }

     // Calculate z = 1-w^lambda stably
     double z, log_z;
     if (w_lambda > 0.9999) {
       // Use complementary calculation for w^lambda near 1
       z = -std::expm1(log_w_lambda);
       log_z = std::log(z);
     } else {
       z = 1.0 - w_lambda;
       log_z = log1p(-w_lambda); // More accurate than log(1-w^lambda)
     }

     // Ensure z is within valid range
     if (z < min_eps) {
       z = min_eps;
       log_z = std::log(z);
     }

     // Special case for very large delta
     if (delta > 1000.0) {
       // Scale to prevent overflow
       double scaled_delta = std::min(delta, 1000.0);
       sum_term4 += scaled_delta * log_z;
     } else {
       sum_term4 += delta * log_z;
     }
   }
 }

 double loglike = const_term + sum_term1 + sum_term2 + sum_term3 + sum_term4;

 // Guard against NaN/Inf
 if (!std::isfinite(loglike)) {
   return R_PosInf;
 }

 return -loglike;
}





//' @title Gradient of the Negative Log-Likelihood for the kkw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the Kumaraswamy-Kumaraswamy (kkw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
//' This distribution is the special case of the Generalized Kumaraswamy (GKw)
//' distribution where \eqn{\gamma = 1}. The gradient is typically used in
//' optimization algorithms for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{delta} (\eqn{\delta \ge 0}), \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 4 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter:
//'   \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \delta, -\partial \ell/\partial \lambda)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the kkw (\eqn{\gamma=1}) model are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
//' + (\beta-1)\sum_{i=1}^{n}\frac{x_i^{\alpha}\ln(x_i)}{v_i}
//' - (\lambda-1)\sum_{i=1}^{n}\frac{\beta v_i^{\beta-1} x_i^{\alpha}\ln(x_i)}{w_i}
//' + \delta\sum_{i=1}^{n}\frac{\lambda w_i^{\lambda-1} \beta v_i^{\beta-1} x_i^{\alpha}\ln(x_i)}{z_i}
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - \sum_{i=1}^{n}\ln(v_i)
//' + (\lambda-1)\sum_{i=1}^{n}\frac{v_i^{\beta}\ln(v_i)}{w_i}
//' - \delta\sum_{i=1}^{n}\frac{\lambda w_i^{\lambda-1} v_i^{\beta}\ln(v_i)}{z_i}
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \delta} = -\frac{n}{\delta+1} - \sum_{i=1}^{n}\ln(z_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} - \sum_{i=1}^{n}\ln(w_i)
//' + \delta\sum_{i=1}^{n}\frac{w_i^{\lambda}\ln(w_i)}{z_i}
//' }
//'
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
//' }
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the general GKw
//' gradient (\code{\link{grgkw}}) components for \eqn{\alpha, \beta, \delta, \lambda}
//' evaluated at \eqn{\gamma=1}. Note that the component for \eqn{\gamma} is omitted.
//' Numerical stability is maintained through careful implementation.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//' @seealso
//' \code{\link{grgkw}} (parent distribution gradient),
//' \code{\link{llkkw}} (negative log-likelihood for kkw),
//' \code{\link{hskkw}} (Hessian for kkw),
//' \code{\link{dkkw}} (density for kkw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rkkw, llkkw, grkkw, hskkw functions for kkw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_kkw <- c(alpha = 2, beta = 3, delta = 1.5, lambda = 0.5)
//' if (exists("rkkw")) {
//'   sample_data_kkw <- rkkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
//'                          delta = true_par_kkw[3], lambda = true_par_kkw[4])
//' } else {
//'   sample_data_kkw <- rgkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
//'                          gamma = 1, delta = true_par_kkw[3], lambda = true_par_kkw[4])
//' }
//'
//' # --- Find MLE estimates ---
//' start_par_kkw <- c(1.5, 2.5, 1.0, 0.6)
//' mle_result_kkw <- stats::optim(par = start_par_kkw,
//'                                fn = llkkw,
//'                                gr = grkkw, # Use analytical gradient for kkw
//'                                method = "BFGS",
//'                                hessian = TRUE,
//'                                data = sample_data_kkw)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_kkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_kkw <- mle_result_kkw$par
//'   cat("\nComparing Gradients for kkw at MLE estimates:\n")
//'
//'   # Numerical gradient of llkkw
//'   num_grad_kkw <- numDeriv::grad(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
//'
//'   # Analytical gradient from grkkw
//'   ana_grad_kkw <- grkkw(par = mle_par_kkw, data = sample_data_kkw)
//'
//'   cat("Numerical Gradient (kkw):\n")
//'   print(num_grad_kkw)
//'   cat("Analytical Gradient (kkw):\n")
//'   print(ana_grad_kkw)
//'
//'   # Check differences
//'   cat("Max absolute difference between kkw gradients:\n")
//'   print(max(abs(num_grad_kkw - ana_grad_kkw)))
//'
//' } else {
//'   cat("\nSkipping kkw gradient comparison.\n")
//' }
//'
//' # --- Optional: Compare with relevant components of GKw gradient ---
//' # Requires grgkw function
//' if (mle_result_kkw$convergence == 0 && exists("grgkw")) {
//'   # Create 5-param vector for grgkw (insert gamma=1)
//'   mle_par_gkw_equiv <- c(mle_par_kkw[1:2], gamma = 1.0, mle_par_kkw[3:4])
//'   ana_grad_gkw <- grgkw(par = mle_par_gkw_equiv, data = sample_data_kkw)
//'   # Extract components corresponding to alpha, beta, delta, lambda
//'   ana_grad_gkw_subset <- ana_grad_gkw[c(1, 2, 4, 5)]
//'
//'   cat("\nComparison with relevant components of GKw gradient:\n")
//'   cat("Max absolute difference:\n")
//'   print(max(abs(ana_grad_kkw - ana_grad_gkw_subset))) # Should be very small
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grkkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 4) {
   Rcpp::NumericVector grad(4, R_NaN);
   return grad;
 }

 double alpha = par[0];
 double beta = par[1];
 double delta = par[2];
 double lambda = par[3];

 if (alpha <= 0 || beta <= 0 || delta < 0 || lambda <= 0) {
   Rcpp::NumericVector grad(4, R_NaN);
   return grad;
 }

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::NumericVector grad(4, R_NaN);
   return grad;
 }

 int n = x.n_elem;
 Rcpp::NumericVector grad(4, 0.0);

 // Constants for numerical stability
 const double min_eps = 1e-15;
 const double eps = 1e-10;
 // const double exp_threshold = -700.0;

 // Initialize component accumulators
 double d_alpha = n / alpha;
 double d_beta = n / beta;
 double d_delta = n / (delta + 1.0);
 double d_lambda = n / lambda;

 // Special case for delta ≈ 0 (reduces to EKw)
 bool delta_near_zero = (delta < min_eps);

 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Ensure xi is within safe bounds
   if (xi < eps) xi = eps;
   if (xi > 1.0 - eps) xi = 1.0 - eps;

   double log_xi = std::log(xi);
   d_alpha += log_xi;

   // Calculate x^alpha in log domain for large alpha
   double x_alpha, x_alpha_log_x;
   if (alpha > 100.0 || alpha * std::abs(log_xi) > 1.0) {
     double log_x_alpha = alpha * log_xi;
     x_alpha = std::exp(log_x_alpha);
     x_alpha_log_x = x_alpha * log_xi;
   } else {
     x_alpha = std::pow(xi, alpha);
     x_alpha_log_x = x_alpha * log_xi;
   }

   // Calculate v = 1-x^alpha with complementary precision
   double v, log_v;
   if (x_alpha > 0.9995) {
     v = -std::expm1(alpha * log_xi);
     log_v = std::log(v);
   } else {
     v = 1.0 - x_alpha;
     log_v = log1p(-x_alpha);  // More precise than log(1-x^alpha)
   }

   // Ensure v is not too small
   v = std::max(v, eps);
   d_beta += log_v;

   // Calculate v^beta in log domain for large beta
   double v_beta, v_beta_m1, v_beta_log_v;
   if (beta > 100.0 || beta * std::abs(log_v) > 1.0) {
     double log_v_beta = beta * log_v;
     v_beta = std::exp(log_v_beta);
     v_beta_m1 = std::exp((beta - 1.0) * log_v);
     v_beta_log_v = v_beta * log_v;
   } else {
     v_beta = std::pow(v, beta);
     v_beta_m1 = std::pow(v, beta - 1.0);
     v_beta_log_v = v_beta * log_v;
   }

   // Calculate w = 1-v^beta with complementary precision
   double w, log_w;
   if (v_beta > 0.9995) {
     w = -std::expm1(beta * log_v);
     log_w = std::log(w);
   } else {
     w = 1.0 - v_beta;
     log_w = log1p(-v_beta);  // More precise than log(1-v^beta)
   }

   // Ensure w is not too small
   w = std::max(w, eps);
   d_lambda += log_w;

   // Handle lambda ≈ 1 (critical case)
   double lambda_factor = 0.0;
   if (std::abs(lambda - 1.0) > min_eps) {
     lambda_factor = lambda - 1.0;
   }

   // Calculate term for alpha gradient: (beta-1)/v
   double alpha_term1 = 0.0;
   if (std::abs(beta - 1.0) > min_eps) {
     alpha_term1 = (beta - 1.0) / v;
   }

   // Calculate term for alpha gradient: (lambda-1)*beta*v^(beta-1)/w
   double alpha_term2 = 0.0;
   if (lambda_factor != 0.0) {
     alpha_term2 = lambda_factor * beta * v_beta_m1 / w;
   }

   // For delta > 0, calculate additional terms
   if (!delta_near_zero) {
     // Calculate w^lambda in log domain for large lambda
     double w_lambda, w_lambda_m1, w_lambda_log_w;
     if (lambda > 100.0 || lambda * std::abs(log_w) > 1.0) {
       double log_w_lambda = lambda * log_w;
       w_lambda = std::exp(log_w_lambda);
       w_lambda_m1 = std::exp((lambda - 1.0) * log_w);
       w_lambda_log_w = w_lambda * log_w;
     } else {
       w_lambda = std::pow(w, lambda);
       w_lambda_m1 = std::pow(w, lambda - 1.0);
       w_lambda_log_w = w_lambda * log_w;
     }

     // Calculate z = 1-w^lambda with complementary precision
     double z, log_z;
     if (w_lambda > 0.9995) {
       z = -std::expm1(lambda * log_w);
       log_z = std::log(z);
     } else {
       z = 1.0 - w_lambda;
       log_z = log1p(-w_lambda);  // More precise than log(1-w^lambda)
     }

     // Ensure z is not too small
     z = std::max(z, eps);
     d_delta += log_z;

     // Calculate term for alpha gradient: delta*lambda*beta*v^(beta-1)*w^(lambda-1)/z
     double alpha_term3 = delta * lambda * beta * v_beta_m1 * w_lambda_m1 / z;

     // Prevent excessive values for large parameters
     if (delta > 1000.0 || lambda > 1000.0) {
       alpha_term3 = std::min(alpha_term3, 1000.0);
     }

     // Combine terms for alpha gradient
     d_alpha -= x_alpha_log_x * (alpha_term1 - alpha_term2 + alpha_term3);

     // Calculate term for beta gradient: (lambda-1)/w
     double beta_term1 = 0.0;
     if (lambda_factor != 0.0) {
       beta_term1 = lambda_factor / w;
     }

     // Calculate term for beta gradient: delta*lambda*w^(lambda-1)/z
     double beta_term2 = delta * lambda * w_lambda_m1 / z;

     // Prevent excessive values for large parameters
     if (delta > 1000.0 || lambda > 1000.0) {
       beta_term2 = std::min(beta_term2, 1000.0);
     }

     // Combine terms for beta gradient
     d_beta -= v_beta_log_v * (beta_term1 - beta_term2);

     // lambda gradient term: delta*(w^lambda*log(w))/z
     d_lambda -= delta * w_lambda_log_w / z;
   } else {
     // Simplified calculations for delta ≈ 0 (EKw case)
     d_alpha -= x_alpha_log_x * (alpha_term1 - alpha_term2);

     if (lambda_factor != 0.0) {
       d_beta -= v_beta_log_v * (lambda_factor / w);
     }
   }
 }

 // Return negative gradient for negative log-likelihood
 grad[0] = -d_alpha;
 grad[1] = -d_beta;
 grad[2] = -d_delta;
 grad[3] = -d_lambda;

 return grad;
}



//' @title Hessian Matrix of the Negative Log-Likelihood for the kkw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian
//'
//' @description
//' Computes the analytic 4x4 Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the Kumaraswamy-Kumaraswamy (kkw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
//' This distribution is the special case of the Generalized Kumaraswamy (GKw)
//' distribution where \eqn{\gamma = 1}. The Hessian is useful for estimating
//' standard errors and in optimization algorithms.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{delta} (\eqn{\delta \ge 0}), \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 4x4 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
//'   where \eqn{\theta = (\alpha, \beta, \delta, \lambda)}.
//'   Returns a 4x4 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function based on the kkw log-likelihood
//' (\eqn{\gamma=1} case of GKw, see \code{\link{llkkw}}):
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\delta+1) + \ln(\lambda) + \ln(\alpha) + \ln(\beta)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i) + \delta\ln(z_i)]
//' }
//' where \eqn{\theta = (\alpha, \beta, \delta, \lambda)} and intermediate terms are:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
//' }
//' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}
//' for \eqn{\theta_i, \theta_j \in \{\alpha, \beta, \delta, \lambda\}}.
//'
//' Key properties of the returned matrix:
//' \itemize{
//'   \item Dimensions: 4x4.
//'   \item Symmetry: The matrix is symmetric.
//'   \item Ordering: Rows and columns correspond to the parameters in the order
//'     \eqn{\alpha, \beta, \delta, \lambda}.
//'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
//' }
//' This corresponds to the relevant submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
//' evaluated at \eqn{\gamma=1}. The exact analytical formulas are implemented directly.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{hsgkw}} (parent distribution Hessian),
//' \code{\link{llkkw}} (negative log-likelihood for kkw),
//' \code{\link{grkkw}} (gradient for kkw),
//' \code{\link{dkkw}} (density for kkw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rkkw, llkkw, grkkw, hskkw functions for kkw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_kkw <- c(alpha = 2, beta = 3, delta = 1.5, lambda = 0.5)
//' if (exists("rkkw")) {
//'   sample_data_kkw <- rkkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
//'                          delta = true_par_kkw[3], lambda = true_par_kkw[4])
//' } else {
//'   sample_data_kkw <- rgkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
//'                          gamma = 1, delta = true_par_kkw[3], lambda = true_par_kkw[4])
//' }
//'
//' # --- Find MLE estimates ---
//' start_par_kkw <- c(1.5, 2.5, 1.0, 0.6)
//' mle_result_kkw <- stats::optim(par = start_par_kkw,
//'                                fn = llkkw,
//'                                gr = if (exists("grkkw")) grkkw else NULL,
//'                                method = "BFGS",
//'                                hessian = TRUE,
//'                                data = sample_data_kkw)
//'
//' # --- Compare analytical Hessian to numerical Hessian ---
//' if (mle_result_kkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("hskkw")) {
//'
//'   mle_par_kkw <- mle_result_kkw$par
//'   cat("\nComparing Hessians for kkw at MLE estimates:\n")
//'
//'   # Numerical Hessian of llkkw
//'   num_hess_kkw <- numDeriv::hessian(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
//'
//'   # Analytical Hessian from hskkw
//'   ana_hess_kkw <- hskkw(par = mle_par_kkw, data = sample_data_kkw)
//'
//'   cat("Numerical Hessian (kkw):\n")
//'   print(round(num_hess_kkw, 4))
//'   cat("Analytical Hessian (kkw):\n")
//'   print(round(ana_hess_kkw, 4))
//'
//'   # Check differences
//'   cat("Max absolute difference between kkw Hessians:\n")
//'   print(max(abs(num_hess_kkw - ana_hess_kkw)))
//'
//'   # Optional: Use analytical Hessian for Standard Errors
//'   # tryCatch({
//'   #   cov_matrix_kkw <- solve(ana_hess_kkw)
//'   #   std_errors_kkw <- sqrt(diag(cov_matrix_kkw))
//'   #   cat("Std. Errors from Analytical kkw Hessian:\n")
//'   #   print(std_errors_kkw)
//'   # }, error = function(e) {
//'   #   warning("Could not invert analytical kkw Hessian: ", e$message)
//'   # })
//'
//' } else {
//'   cat("\nSkipping kkw Hessian comparison.\n")
//'   cat("Requires convergence, 'numDeriv' package, and function 'hskkw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hskkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 4) {
   Rcpp::NumericMatrix nanH(4,4);
   nanH.fill(R_NaN);
   return nanH;
 }

 double alpha = par[0];
 double beta = par[1];
 double delta = par[2];
 double lambda = par[3];

 if (alpha <= 0 || beta <= 0 || delta < 0 || lambda <= 0) {
   Rcpp::NumericMatrix nanH(4,4);
   nanH.fill(R_NaN);
   return nanH;
 }

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::NumericMatrix nanH(4,4);
   nanH.fill(R_NaN);
   return nanH;
 }

 int n = x.n_elem;

 // Stability constants
 const double min_eps = 1e-15;
 const double eps = 1e-10;
 // const double exp_threshold = -700.0;
 const double max_contrib = 1e6;  // Limit for individual contributions

 // Initialize Hessian matrix
 arma::mat H(4,4, arma::fill::zeros);

 // Special case: delta ≈ 0 (reduces to EKw)
 bool delta_near_zero = (delta < min_eps);

 // Constant diagonal terms
 H(0,0) = -n/(alpha*alpha);
 H(1,1) = -n/(beta*beta);
 H(3,3) = -n/(lambda*lambda);

 // Handle delta term carefully
 if (delta_near_zero) {
   H(2,2) = -n;  // Limit as delta→0 of -n/(delta+1)²
 } else {
   H(2,2) = -n/std::pow(delta+1.0, 2.0);
 }

 // Accumulators for mixed derivatives
 double acc_alpha_lambda = 0.0;
 double acc_beta_lambda = 0.0;
 double acc_delta_lambda = 0.0;

 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Ensure xi is within safe bounds
   if (xi < eps) xi = eps;
   if (xi > 1.0 - eps) xi = 1.0 - eps;

   double log_xi = std::log(xi);

   // --- Calculate A = x^α and derivatives ---
   double A, dA_dalpha, d2A_dalpha2;

   if (alpha > 100.0 || alpha * std::abs(log_xi) > 1.0) {
     // Log-domain calculation for large alpha
     double log_A = alpha * log_xi;
     A = std::exp(log_A);
     dA_dalpha = A * log_xi;
     d2A_dalpha2 = A * log_xi * log_xi;
   } else {
     A = std::pow(xi, alpha);
     dA_dalpha = A * log_xi;
     d2A_dalpha2 = A * log_xi * log_xi;
   }

   // --- Calculate v = 1-A and derivatives ---
   double v, log_v, dv_dalpha, d2v_dalpha2;

   if (A > 0.9995) {
     // Complementary precision for A near 1
     v = -std::expm1(alpha * log_xi);
     log_v = std::log(v);
   } else {
     v = 1.0 - A;
     log_v = log1p(-A);  // More accurate than log(1-A)
   }

   // Ensure v is not too small
   v = std::max(v, eps);
   dv_dalpha = -dA_dalpha;
   d2v_dalpha2 = -d2A_dalpha2;

   // --- Terms for (β-1)ln(v) ---
   double d2L6_dalpha2 = 0.0;
   double d2L6_dalpha_dbeta = 0.0;

   if (std::abs(beta - 1.0) > min_eps) {
     double v_squared = v * v;
     d2L6_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / v_squared);
     d2L6_dalpha_dbeta = dv_dalpha / v;
   }

   // --- Calculate w = 1-v^β and derivatives ---
   double v_beta, v_beta_m1, v_beta_m2, w, log_w;

   if (beta > 100.0 || beta * std::abs(log_v) > 1.0) {
     // Log-domain calculation for large beta
     double log_v_beta = beta * log_v;
     v_beta = std::exp(log_v_beta);
     v_beta_m1 = std::exp((beta - 1.0) * log_v);
     v_beta_m2 = std::exp((beta - 2.0) * log_v);
   } else {
     v_beta = std::pow(v, beta);
     v_beta_m1 = std::pow(v, beta - 1.0);
     v_beta_m2 = std::pow(v, beta - 2.0);
   }

   if (v_beta > 0.9995) {
     // Complementary precision for v^β near 1
     w = -std::expm1(beta * log_v);
     log_w = std::log(w);
   } else {
     w = 1.0 - v_beta;
     log_w = log1p(-v_beta);  // More accurate than log(1-v_beta)
   }

   // Ensure w is not too small
   w = std::max(w, eps);
   double w_squared = w * w;

   // Derivatives of w
   double dw_dv = -beta * v_beta_m1;
   double dw_dalpha = dw_dv * dv_dalpha;
   double dw_dbeta = -v_beta * log_v;

   // Second derivatives of w
   double d2w_dalpha2 = -beta * ((beta - 1.0) * v_beta_m2 * (dv_dalpha * dv_dalpha) +
                                 v_beta_m1 * d2v_dalpha2);
   double d2w_dbeta2 = -v_beta * (log_v * log_v);
   double d_dw_dalpha_dbeta = -v_beta_m1 * (1.0 + beta * log_v) * dv_dalpha;

   // --- Terms for (λ-1)ln(w) ---
   double d2L7_dalpha2 = 0.0;
   double d2L7_dbeta2 = 0.0;
   double d2L7_dalpha_dbeta = 0.0;

   // Handle lambda near 1 carefully
   double lambda_minus_1 = lambda - 1.0;
   if (std::abs(lambda_minus_1) > min_eps) {
     d2L7_dalpha2 = lambda_minus_1 * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / w_squared);
     d2L7_dbeta2 = lambda_minus_1 * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta)) / w_squared);
     d2L7_dalpha_dbeta = lambda_minus_1 * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta) / w_squared);

     // Clamp extreme values
     d2L7_dalpha2 = std::min(std::max(d2L7_dalpha2, -max_contrib), max_contrib);
     d2L7_dbeta2 = std::min(std::max(d2L7_dbeta2, -max_contrib), max_contrib);
     d2L7_dalpha_dbeta = std::min(std::max(d2L7_dalpha_dbeta, -max_contrib), max_contrib);
   }

   // For delta ≈ 0, skip z calculations (EKw case)
   if (delta_near_zero) {
     // Update Hessian elements
     H(0,0) += d2L6_dalpha2 + d2L7_dalpha2;
     H(0,1) += d2L6_dalpha_dbeta + d2L7_dalpha_dbeta;
     H(1,0) = H(0,1);
     H(1,1) += d2L7_dbeta2;

     // Mixed derivatives with lambda (from L7 only)
     acc_alpha_lambda += dw_dalpha / w;
     acc_beta_lambda += dw_dbeta / w;
   } else {
     // --- Calculate z = 1-w^λ and derivatives ---
     double w_lambda, w_lambda_m1, w_lambda_m2, z;

     if (lambda > 100.0 || lambda * std::abs(log_w) > 1.0) {
       // Log-domain calculation for large lambda
       double log_w_lambda = lambda * log_w;
       w_lambda = std::exp(log_w_lambda);
       w_lambda_m1 = std::exp((lambda - 1.0) * log_w);
       w_lambda_m2 = std::exp((lambda - 2.0) * log_w);
     } else {
       w_lambda = std::pow(w, lambda);
       w_lambda_m1 = std::pow(w, lambda - 1.0);
       w_lambda_m2 = std::pow(w, lambda - 2.0);
     }

     if (w_lambda > 0.9995) {
       // Complementary precision for w^λ near 1
       z = -std::expm1(lambda * log_w);
       // double log_z = std::log(z);
     } else {
       z = 1.0 - w_lambda;
       // double log_z = log1p(-w_lambda);
     }

     // Ensure z is not too small
     z = std::max(z, eps);
     double z_squared = z * z;

     // First derivatives of z
     double dz_dalpha = -lambda * w_lambda_m1 * dw_dalpha;
     double dz_dbeta = -lambda * w_lambda_m1 * dw_dbeta;
     double dz_dlambda = -w_lambda * log_w;

     // Second derivatives of z
     double d2z_dalpha2 = -lambda * ((lambda - 1.0) * w_lambda_m2 * (dw_dalpha * dw_dalpha) +
                                     w_lambda_m1 * d2w_dalpha2);
     double d2z_dbeta2 = -lambda * ((lambda - 1.0) * w_lambda_m2 * (dw_dbeta * dw_dbeta) +
                                    w_lambda_m1 * d2w_dbeta2);
     double d2z_dlambda2 = -w_lambda * (log_w * log_w);

     // Mixed derivatives of z
     double d_dw_dalpha_dbeta_2 = -lambda * ((lambda - 1.0) * w_lambda_m2 * dw_dbeta * dw_dalpha +
                                             w_lambda_m1 * d_dw_dalpha_dbeta);
     double d_dalpha_dz_dlambda = -lambda * w_lambda_m1 * dw_dalpha * log_w -
       w_lambda * (dw_dalpha / w);
     double d_dbeta_dz_dlambda = -lambda * w_lambda_m1 * dw_dbeta * log_w -
       w_lambda * (dw_dbeta / w);

     // Terms for δln(z)
     double d2L8_dalpha2 = delta * ((d2z_dalpha2 * z - dz_dalpha * dz_dalpha) / z_squared);
     double d2L8_dbeta2 = delta * ((d2z_dbeta2 * z - dz_dbeta * dz_dbeta) / z_squared);
     double d2L8_dlambda2 = delta * ((d2z_dlambda2 * z - dz_dlambda * dz_dlambda) / z_squared);
     double d2L8_dalpha_dbeta = delta * ((d_dw_dalpha_dbeta_2 / z) - (dz_dalpha * dz_dbeta) / z_squared);
     double d2L8_dalpha_dlambda = delta * ((d_dalpha_dz_dlambda / z) - (dz_dlambda * dz_dalpha) / z_squared);
     double d2L8_dbeta_dlambda = delta * ((d_dbeta_dz_dlambda / z) - (dz_dlambda * dz_dbeta) / z_squared);
     double d2L8_ddelta_dlambda = dz_dlambda / z;

     // Clamp extreme values for numerical stability
     d2L8_dalpha2 = std::min(std::max(d2L8_dalpha2, -max_contrib), max_contrib);
     d2L8_dbeta2 = std::min(std::max(d2L8_dbeta2, -max_contrib), max_contrib);
     d2L8_dlambda2 = std::min(std::max(d2L8_dlambda2, -max_contrib), max_contrib);
     d2L8_dalpha_dbeta = std::min(std::max(d2L8_dalpha_dbeta, -max_contrib), max_contrib);
     d2L8_dalpha_dlambda = std::min(std::max(d2L8_dalpha_dlambda, -max_contrib), max_contrib);
     d2L8_dbeta_dlambda = std::min(std::max(d2L8_dbeta_dlambda, -max_contrib), max_contrib);

     // Update Hessian elements
     H(0,0) += d2L6_dalpha2 + d2L7_dalpha2 + d2L8_dalpha2;
     H(0,1) += d2L6_dalpha_dbeta + d2L7_dalpha_dbeta + d2L8_dalpha_dbeta;
     H(1,0) = H(0,1);
     H(1,1) += d2L7_dbeta2 + d2L8_dbeta2;
     H(3,3) += d2L8_dlambda2;

     // Mixed derivatives with delta
     H(0,2) += dz_dalpha / z;
     H(2,0) = H(0,2);
     H(1,2) += dz_dbeta / z;
     H(2,1) = H(1,2);

     // Accumulators for mixed derivatives with lambda
     acc_alpha_lambda += (dw_dalpha / w) + d2L8_dalpha_dlambda;
     acc_beta_lambda += (dw_dbeta / w) + d2L8_dbeta_dlambda;
     acc_delta_lambda += d2L8_ddelta_dlambda;
   }
 }

 // Apply mixed derivatives with lambda
 H(0,3) = acc_alpha_lambda;
 H(3,0) = H(0,3);
 H(1,3) = acc_beta_lambda;
 H(3,1) = H(1,3);
 H(2,3) = acc_delta_lambda;
 H(3,2) = H(2,3);

 // Final symmetry check
 for (int i = 0; i < 4; i++) {
   for (int j = i+1; j < 4; j++) {
     double avg = (H(i,j) + H(j,i)) / 2.0;
     H(i,j) = H(j,i) = avg;
   }
 }

 return Rcpp::wrap(-H);
}


// //' @title Negative Log-Likelihood for the kkw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize
// //'
// //' @description
// //' Computes the negative log-likelihood function for the Kumaraswamy-Kumaraswamy
// //' (kkw) distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}),
// //' given a vector of observations. This distribution is a special case of the
// //' Generalized Kumaraswamy (GKw) distribution where \eqn{\gamma = 1}.
// //'
// //' @param par A numeric vector of length 4 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{delta} (\eqn{\delta \ge 0}), \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a single \code{double} value representing the negative
// //'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
// //'   if any parameter values in \code{par} are invalid according to their
// //'   constraints, or if any value in \code{data} is not in the interval (0, 1).
// //'
// //' @details
// //' The kkw distribution is the GKw distribution (\code{\link{dgkw}}) with \eqn{\gamma=1}.
// //' Its probability density function (PDF) is:
// //' \deqn{
// //' f(x | \theta) = (\delta + 1) \lambda \alpha \beta x^{\alpha - 1} (1 - x^\alpha)^{\beta - 1} \bigl[1 - (1 - x^\alpha)^\beta\bigr]^{\lambda - 1} \bigl\{1 - \bigl[1 - (1 - x^\alpha)^\beta\bigr]^\lambda\bigr\}^{\delta}
// //' }
// //' for \eqn{0 < x < 1} and \eqn{\theta = (\alpha, \beta, \delta, \lambda)}.
// //' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
// //' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\delta+1) + \ln(\lambda) + \ln(\alpha) + \ln(\beta)]
// //' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i) + \delta\ln(z_i)]
// //' }
// //' where:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
// //' }
// //' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
// //' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
// //' Numerical stability is maintained similarly to \code{\link{llgkw}}.
// //'
// //' @references
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //' @seealso
// //' \code{\link{llgkw}} (parent distribution negative log-likelihood),
// //' \code{\link{dkkw}}, \code{\link{pkkw}}, \code{\link{qkkw}}, \code{\link{rkkw}},
// //' \code{\link{grkkw}} (gradient, if available),
// //' \code{\link{hskkw}} (Hessian, if available),
// //' \code{\link[stats]{optim}}
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rkkw, grkkw, hskkw functions for kkw distribution
// //'
// //' # Generate sample data from a known kkw distribution
// //' set.seed(123)
// //' true_par_kkw <- c(alpha = 2, beta = 3, delta = 1.5, lambda = 0.5)
// //' # Use rkkw if it exists, otherwise use rgkw with gamma=1
// //' if (exists("rkkw")) {
// //'   sample_data_kkw <- rkkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
// //'                          delta = true_par_kkw[3], lambda = true_par_kkw[4])
// //' } else {
// //'   sample_data_kkw <- rgkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
// //'                          gamma = 1, delta = true_par_kkw[3], lambda = true_par_kkw[4])
// //' }
// //' hist(sample_data_kkw, breaks = 20, main = "kkw(2, 3, 1.5, 0.5) Sample")
// //'
// //' # --- Maximum Likelihood Estimation using optim ---
// //' # Initial parameter guess
// //' start_par_kkw <- c(1.5, 2.5, 1.0, 0.6)
// //'
// //' # Perform optimization (minimizing negative log-likelihood)
// //' mle_result_kkw <- stats::optim(par = start_par_kkw,
// //'                                fn = llkkw, # Use the kkw neg-log-likelihood
// //'                                method = "BFGS",
// //'                                hessian = TRUE,
// //'                                data = sample_data_kkw)
// //'
// //' # Check convergence and results
// //' if (mle_result_kkw$convergence == 0) {
// //'   print("Optimization converged successfully.")
// //'   mle_par_kkw <- mle_result_kkw$par
// //'   print("Estimated kkw parameters:")
// //'   print(mle_par_kkw)
// //'   print("True kkw parameters:")
// //'   print(true_par_kkw)
// //' } else {
// //'   warning("Optimization did not converge!")
// //'   print(mle_result_kkw$message)
// //' }
// //'
// //' # --- Compare numerical and analytical derivatives (if available) ---
// //' # Requires 'numDeriv' package and analytical functions 'grkkw', 'hskkw'
// //' if (mle_result_kkw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("grkkw") && exists("hskkw")) {
// //'
// //'   cat("\nComparing Derivatives at kkw MLE estimates:\n")
// //'
// //'   # Numerical derivatives of llkkw
// //'   num_grad_kkw <- numDeriv::grad(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
// //'   num_hess_kkw <- numDeriv::hessian(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
// //'
// //'   # Analytical derivatives (assuming they return derivatives of negative LL)
// //'   ana_grad_kkw <- grkkw(par = mle_par_kkw, data = sample_data_kkw)
// //'   ana_hess_kkw <- hskkw(par = mle_par_kkw, data = sample_data_kkw)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between gradients:\n")
// //'   print(max(abs(num_grad_kkw - ana_grad_kkw)))
// //'   cat("Max absolute difference between Hessians:\n")
// //'   print(max(abs(num_hess_kkw - ana_hess_kkw)))
// //'
// //' } else {
// //'    cat("\nSkipping derivative comparison for kkw.\n")
// //'    cat("Requires convergence, 'numDeriv' package and functions 'grkkw', 'hskkw'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// double llkkw(const Rcpp::NumericVector& par,
//             const Rcpp::NumericVector& data) {
//  if (par.size()<4) {
//    return R_PosInf; // invalid
//  }
//  double a= par[0];
//  double b= par[1];
//  double dd= par[2];
//  double ll= par[3];
//
//  if (!check_kkw_pars(a,b,dd,ll)) {
//    return R_PosInf; // param invalid => +Inf for negative log-likelihood
//  }
//
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if (x.n_elem<1) {
//    return R_PosInf;
//  }
//  // check data in (0,1)
//  if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
//    return R_PosInf;
//  }
//
//  int n= x.n_elem;
//  // sum of log-likelihood
//
//  // constant part: n * [ log(λ)+ log(α)+ log(β)+ log(δ+1) ]
//  double cst = n*( std::log(ll) + std::log(a) + std::log(b) + std::log(dd+1.0) );
//
//  // sum parts
//  // sum((α-1)*log(x))
//  arma::vec lx = arma::log(x);
//  double term1= (a-1.0)*arma::sum(lx);
//
//  // sum((β-1)* log(1 - x^α))
//  arma::vec xalpha = arma::pow(x,a);
//  arma::vec log1mxalpha = arma::log(1.0 - xalpha);
//  double term2= (b-1.0)*arma::sum(log1mxalpha);
//
//  // sum((λ-1)* log(1 - (1 - x^α)^β))
//  arma::vec vbeta = arma::pow(1.0 - xalpha, b);
//  arma::vec one_minus_vbeta = 1.0 - vbeta;
//  // safe log
//  arma::vec log_omv = arma::log(one_minus_vbeta);
//  double term3= (ll-1.0)*arma::sum(log_omv);
//
//  // sum( δ* log(1 - [1-(1-x^α)^β]^λ ) )
//  // let w= one_minus_vbeta => [1-w]^λ => so 1- w^λ is the factor
//  arma::vec wlambda= arma::pow(one_minus_vbeta, ll);
//  arma::vec one_minus_wlambda= 1.0 - wlambda;
//  arma::vec log_1mw= arma::log(one_minus_wlambda);
//  double term4= dd* arma::sum(log_1mw);
//
//  double loglike = cst + term1 + term2 + term3 + term4;
//  // negative
//  return -loglike;
// }
//
//
// //' @title Gradient of the Negative Log-Likelihood for the kkw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize gradient
// //'
// //' @description
// //' Computes the gradient vector (vector of first partial derivatives) of the
// //' negative log-likelihood function for the Kumaraswamy-Kumaraswamy (kkw)
// //' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
// //' This distribution is the special case of the Generalized Kumaraswamy (GKw)
// //' distribution where \eqn{\gamma = 1}. The gradient is typically used in
// //' optimization algorithms for maximum likelihood estimation.
// //'
// //' @param par A numeric vector of length 4 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{delta} (\eqn{\delta \ge 0}), \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a numeric vector of length 4 containing the partial derivatives
// //'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
// //'   respect to each parameter:
// //'   \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \delta, -\partial \ell/\partial \lambda)}.
// //'   Returns a vector of \code{NaN} if any parameter values are invalid according
// //'   to their constraints, or if any value in \code{data} is not in the
// //'   interval (0, 1).
// //'
// //' @details
// //' The components of the gradient vector of the negative log-likelihood
// //' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the kkw (\eqn{\gamma=1}) model are:
// //'
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
// //' + (\beta-1)\sum_{i=1}^{n}\frac{x_i^{\alpha}\ln(x_i)}{v_i}
// //' - (\lambda-1)\sum_{i=1}^{n}\frac{\beta v_i^{\beta-1} x_i^{\alpha}\ln(x_i)}{w_i}
// //' + \delta\sum_{i=1}^{n}\frac{\lambda w_i^{\lambda-1} \beta v_i^{\beta-1} x_i^{\alpha}\ln(x_i)}{z_i}
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - \sum_{i=1}^{n}\ln(v_i)
// //' + (\lambda-1)\sum_{i=1}^{n}\frac{v_i^{\beta}\ln(v_i)}{w_i}
// //' - \delta\sum_{i=1}^{n}\frac{\lambda w_i^{\lambda-1} v_i^{\beta}\ln(v_i)}{z_i}
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \delta} = -\frac{n}{\delta+1} - \sum_{i=1}^{n}\ln(z_i)
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} - \sum_{i=1}^{n}\ln(w_i)
// //' + \delta\sum_{i=1}^{n}\frac{w_i^{\lambda}\ln(w_i)}{z_i}
// //' }
// //'
// //' where:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
// //' }
// //' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
// //' minimizing the negative log-likelihood. They correspond to the general GKw
// //' gradient (\code{\link{grgkw}}) components for \eqn{\alpha, \beta, \delta, \lambda}
// //' evaluated at \eqn{\gamma=1}. Note that the component for \eqn{\gamma} is omitted.
// //' Numerical stability is maintained through careful implementation.
// //'
// //' @references
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //' @seealso
// //' \code{\link{grgkw}} (parent distribution gradient),
// //' \code{\link{llkkw}} (negative log-likelihood for kkw),
// //' \code{\link{hskkw}} (Hessian for kkw),
// //' \code{\link{dkkw}} (density for kkw),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{grad}} (for numerical gradient comparison).
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rkkw, llkkw, grkkw, hskkw functions for kkw
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_kkw <- c(alpha = 2, beta = 3, delta = 1.5, lambda = 0.5)
// //' if (exists("rkkw")) {
// //'   sample_data_kkw <- rkkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
// //'                          delta = true_par_kkw[3], lambda = true_par_kkw[4])
// //' } else {
// //'   sample_data_kkw <- rgkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
// //'                          gamma = 1, delta = true_par_kkw[3], lambda = true_par_kkw[4])
// //' }
// //'
// //' # --- Find MLE estimates ---
// //' start_par_kkw <- c(1.5, 2.5, 1.0, 0.6)
// //' mle_result_kkw <- stats::optim(par = start_par_kkw,
// //'                                fn = llkkw,
// //'                                gr = grkkw, # Use analytical gradient for kkw
// //'                                method = "BFGS",
// //'                                hessian = TRUE,
// //'                                data = sample_data_kkw)
// //'
// //' # --- Compare analytical gradient to numerical gradient ---
// //' if (mle_result_kkw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE)) {
// //'
// //'   mle_par_kkw <- mle_result_kkw$par
// //'   cat("\nComparing Gradients for kkw at MLE estimates:\n")
// //'
// //'   # Numerical gradient of llkkw
// //'   num_grad_kkw <- numDeriv::grad(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
// //'
// //'   # Analytical gradient from grkkw
// //'   ana_grad_kkw <- grkkw(par = mle_par_kkw, data = sample_data_kkw)
// //'
// //'   cat("Numerical Gradient (kkw):\n")
// //'   print(num_grad_kkw)
// //'   cat("Analytical Gradient (kkw):\n")
// //'   print(ana_grad_kkw)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between kkw gradients:\n")
// //'   print(max(abs(num_grad_kkw - ana_grad_kkw)))
// //'
// //' } else {
// //'   cat("\nSkipping kkw gradient comparison.\n")
// //' }
// //'
// //' # --- Optional: Compare with relevant components of GKw gradient ---
// //' # Requires grgkw function
// //' if (mle_result_kkw$convergence == 0 && exists("grgkw")) {
// //'   # Create 5-param vector for grgkw (insert gamma=1)
// //'   mle_par_gkw_equiv <- c(mle_par_kkw[1:2], gamma = 1.0, mle_par_kkw[3:4])
// //'   ana_grad_gkw <- grgkw(par = mle_par_gkw_equiv, data = sample_data_kkw)
// //'   # Extract components corresponding to alpha, beta, delta, lambda
// //'   ana_grad_gkw_subset <- ana_grad_gkw[c(1, 2, 4, 5)]
// //'
// //'   cat("\nComparison with relevant components of GKw gradient:\n")
// //'   cat("Max absolute difference:\n")
// //'   print(max(abs(ana_grad_kkw - ana_grad_gkw_subset))) # Should be very small
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericVector grkkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha = par[0];   // Shape parameter α > 0
//  double beta = par[1];    // Shape parameter β > 0
//  double delta = par[2];   // Shape parameter δ > 0
//  double lambda = par[3];  // Shape parameter λ > 0
//
//  // Parameter validation
//  if (alpha <= 0 || beta <= 0 || delta <= 0 || lambda <= 0) {
//    Rcpp::NumericVector grad(4, R_NaN);
//    return grad;
//  }
//
//  // Data conversion and validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//
//  if (arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericVector grad(4, R_NaN);
//    return grad;
//  }
//
//  int n = x.n_elem;  // Sample size
//
//  // Initialize gradient vector
//  Rcpp::NumericVector grad(4, 0.0);
//
//  // Small constant to avoid numerical issues
//  double eps = std::numeric_limits<double>::epsilon() * 100;
//
//  // Compute transformations and intermediate values
//  arma::vec log_x = arma::log(x);                // log(x_i)
//  arma::vec x_alpha = arma::pow(x, alpha);       // x_i^α
//  arma::vec x_alpha_log_x = x_alpha % log_x;     // x_i^α * log(x_i)
//
//  // v_i = 1 - x_i^α
//  arma::vec v = 1.0 - x_alpha;
//  v = arma::clamp(v, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_v = arma::log(v);                // log(v_i)
//  arma::vec v_beta_m1 = arma::pow(v, beta - 1.0); // v_i^(β-1)
//  arma::vec v_beta = arma::pow(v, beta);          // v_i^β
//  arma::vec v_beta_log_v = v_beta % log_v;        // v_i^β * log(v_i)
//
//  // w_i = 1 - v_i^β = 1 - (1-x_i^α)^β
//  arma::vec w = 1.0 - v_beta;
//  w = arma::clamp(w, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_w = arma::log(w);                // log(w_i)
//  arma::vec w_lambda_m1 = arma::pow(w, lambda - 1.0); // w_i^(λ-1)
//  arma::vec w_lambda = arma::pow(w, lambda);          // w_i^λ
//  arma::vec w_lambda_log_w = w_lambda % log_w;        // w_i^λ * log(w_i)
//
//  // z_i = 1 - w_i^λ = 1 - [1-(1-x_i^α)^β]^λ
//  arma::vec z = 1.0 - w_lambda;
//  z = arma::clamp(z, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_z = arma::log(z);                // log(z_i)
//
//  // Calculate partial derivatives for each parameter (for log-likelihood)
//
//  // ∂ℓ/∂α = n/α + Σᵢlog(xᵢ) - (β-1)Σᵢ[xᵢ^α*log(xᵢ)/vᵢ] + (λ-1)Σᵢ[β*vᵢ^(β-1)*xᵢ^α*log(xᵢ)/wᵢ] - δΣᵢ[λ*wᵢ^(λ-1)*β*vᵢ^(β-1)*xᵢ^α*log(xᵢ)/zᵢ]
//  double d_alpha = n / alpha + arma::sum(log_x);
//
//  // Calculate terms separately with correct signs
//  arma::vec alpha_term1 = (beta - 1.0) / v;                       // (β-1)/v_i
//  arma::vec alpha_term2 = (lambda - 1.0) * beta * v_beta_m1 / w;  // (λ-1)*β*v_i^(β-1)/w_i
//  arma::vec alpha_term3 = delta * lambda * beta * v_beta_m1 % w_lambda_m1 / z;  // δ*λ*β*v_i^(β-1)*w_i^(λ-1)/z_i
//
//  // Combine terms with correct signs
//  d_alpha -= arma::sum(x_alpha_log_x % alpha_term1);  // Subtract term with (β-1)
//  d_alpha += arma::sum(x_alpha_log_x % alpha_term2);  // Add term with (λ-1)
//  d_alpha -= arma::sum(x_alpha_log_x % alpha_term3);  // Subtract term with δ
//
//  // ∂ℓ/∂β = n/β + Σᵢlog(vᵢ) - (λ-1)Σᵢ[vᵢ^β*log(vᵢ)/wᵢ] + δΣᵢ[λ*wᵢ^(λ-1)*vᵢ^β*log(vᵢ)/zᵢ]
//  double d_beta = n / beta + arma::sum(log_v);
//
//  // Calculate terms separately with correct signs
//  arma::vec beta_term1 = (lambda - 1.0) / w;       // (λ-1)/w_i
//  arma::vec beta_term2 = delta * lambda * w_lambda_m1 / z; // δ*λ*w_i^(λ-1)/z_i
//
//  // CORRECTED: These signs were wrong in the previous implementation
//  d_beta -= arma::sum(v_beta_log_v % beta_term1);  // Subtract term with (λ-1)
//  d_beta += arma::sum(v_beta_log_v % beta_term2);  // Add term with δ
//
//  // ∂ℓ/∂δ = n/(δ+1) + Σᵢlog(zᵢ)
//  double d_delta = n / (delta + 1.0) + arma::sum(log_z);
//
//  // ∂ℓ/∂λ = n/λ + Σᵢlog(wᵢ) - δΣᵢ[(wᵢ^λ*log(wᵢ))/zᵢ]
//  double d_lambda = n / lambda + arma::sum(log_w) - delta * arma::sum(w_lambda_log_w / z);
//
//  // Since we're optimizing negative log-likelihood, negate all derivatives
//  grad[0] = -d_alpha;
//  grad[1] = -d_beta;
//  grad[2] = -d_delta;
//  grad[3] = -d_lambda;
//
//  return grad;
// }
//
//
//
// //' @title Hessian Matrix of the Negative Log-Likelihood for the kkw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize hessian
// //'
// //' @description
// //' Computes the analytic 4x4 Hessian matrix (matrix of second partial derivatives)
// //' of the negative log-likelihood function for the Kumaraswamy-Kumaraswamy (kkw)
// //' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}).
// //' This distribution is the special case of the Generalized Kumaraswamy (GKw)
// //' distribution where \eqn{\gamma = 1}. The Hessian is useful for estimating
// //' standard errors and in optimization algorithms.
// //'
// //' @param par A numeric vector of length 4 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{delta} (\eqn{\delta \ge 0}), \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a 4x4 numeric matrix representing the Hessian matrix of the
// //'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
// //'   where \eqn{\theta = (\alpha, \beta, \delta, \lambda)}.
// //'   Returns a 4x4 matrix populated with \code{NaN} if any parameter values are
// //'   invalid according to their constraints, or if any value in \code{data} is
// //'   not in the interval (0, 1).
// //'
// //' @details
// //' This function calculates the analytic second partial derivatives of the
// //' negative log-likelihood function based on the kkw log-likelihood
// //' (\eqn{\gamma=1} case of GKw, see \code{\link{llkkw}}):
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\delta+1) + \ln(\lambda) + \ln(\alpha) + \ln(\beta)]
// //' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i) + \delta\ln(z_i)]
// //' }
// //' where \eqn{\theta = (\alpha, \beta, \delta, \lambda)} and intermediate terms are:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //'   \item \eqn{z_i = 1 - w_i^{\lambda} = 1 - [1-(1-x_i^{\alpha})^{\beta}]^{\lambda}}
// //' }
// //' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}
// //' for \eqn{\theta_i, \theta_j \in \{\alpha, \beta, \delta, \lambda\}}.
// //'
// //' Key properties of the returned matrix:
// //' \itemize{
// //'   \item Dimensions: 4x4.
// //'   \item Symmetry: The matrix is symmetric.
// //'   \item Ordering: Rows and columns correspond to the parameters in the order
// //'     \eqn{\alpha, \beta, \delta, \lambda}.
// //'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
// //' }
// //' This corresponds to the relevant submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
// //' evaluated at \eqn{\gamma=1}. The exact analytical formulas are implemented directly.
// //'
// //' @references
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //' @seealso
// //' \code{\link{hsgkw}} (parent distribution Hessian),
// //' \code{\link{llkkw}} (negative log-likelihood for kkw),
// //' \code{\link{grkkw}} (gradient for kkw),
// //' \code{\link{dkkw}} (density for kkw),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rkkw, llkkw, grkkw, hskkw functions for kkw
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_kkw <- c(alpha = 2, beta = 3, delta = 1.5, lambda = 0.5)
// //' if (exists("rkkw")) {
// //'   sample_data_kkw <- rkkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
// //'                          delta = true_par_kkw[3], lambda = true_par_kkw[4])
// //' } else {
// //'   sample_data_kkw <- rgkw(100, alpha = true_par_kkw[1], beta = true_par_kkw[2],
// //'                          gamma = 1, delta = true_par_kkw[3], lambda = true_par_kkw[4])
// //' }
// //'
// //' # --- Find MLE estimates ---
// //' start_par_kkw <- c(1.5, 2.5, 1.0, 0.6)
// //' mle_result_kkw <- stats::optim(par = start_par_kkw,
// //'                                fn = llkkw,
// //'                                gr = if (exists("grkkw")) grkkw else NULL,
// //'                                method = "BFGS",
// //'                                hessian = TRUE,
// //'                                data = sample_data_kkw)
// //'
// //' # --- Compare analytical Hessian to numerical Hessian ---
// //' if (mle_result_kkw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("hskkw")) {
// //'
// //'   mle_par_kkw <- mle_result_kkw$par
// //'   cat("\nComparing Hessians for kkw at MLE estimates:\n")
// //'
// //'   # Numerical Hessian of llkkw
// //'   num_hess_kkw <- numDeriv::hessian(func = llkkw, x = mle_par_kkw, data = sample_data_kkw)
// //'
// //'   # Analytical Hessian from hskkw
// //'   ana_hess_kkw <- hskkw(par = mle_par_kkw, data = sample_data_kkw)
// //'
// //'   cat("Numerical Hessian (kkw):\n")
// //'   print(round(num_hess_kkw, 4))
// //'   cat("Analytical Hessian (kkw):\n")
// //'   print(round(ana_hess_kkw, 4))
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between kkw Hessians:\n")
// //'   print(max(abs(num_hess_kkw - ana_hess_kkw)))
// //'
// //'   # Optional: Use analytical Hessian for Standard Errors
// //'   # tryCatch({
// //'   #   cov_matrix_kkw <- solve(ana_hess_kkw)
// //'   #   std_errors_kkw <- sqrt(diag(cov_matrix_kkw))
// //'   #   cat("Std. Errors from Analytical kkw Hessian:\n")
// //'   #   print(std_errors_kkw)
// //'   # }, error = function(e) {
// //'   #   warning("Could not invert analytical kkw Hessian: ", e$message)
// //'   # })
// //'
// //' } else {
// //'   cat("\nSkipping kkw Hessian comparison.\n")
// //'   cat("Requires convergence, 'numDeriv' package, and function 'hskkw'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hskkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha  = par[0];   // θ[0] = α
//  double beta   = par[1];   // θ[1] = β
//  double delta  = par[2];   // θ[2] = δ
//  double lambda = par[3];   // θ[3] = λ
//
//  // Simple parameter validation (all > 0)
//  if(alpha <= 0 || beta <= 0 || delta <= 0 || lambda <= 0) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  // Data conversion and basic validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if(arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  int n = x.n_elem;  // sample size
//
//  // Initialize Hessian matrix H (of ℓ(θ)) as 4x4
//  arma::mat H(4,4, arma::fill::zeros);
//
//  // --- CONSTANT TERMS (do not depend on x) ---
//  // L1: n ln(λ)  => d²/dλ² = -n/λ²
//  H(3,3) += -n/(lambda*lambda);
//  // L2: n ln(α)  => d²/dα² = -n/α²
//  H(0,0) += -n/(alpha*alpha);
//  // L3: n ln(β)  => d²/dβ² = -n/β²
//  H(1,1) += -n/(beta*beta);
//  // L4: n ln(δ+1) => d²/dδ² = -n/(δ+1)²
//  H(2,2) += -n/std::pow(delta+1.0, 2.0);
//
//  // Accumulators for mixed derivatives with λ
//  double acc_delta_lambda = 0.0;  // Sum of dz_dlambda / z
//  double acc_alpha_lambda = 0.0;  // For α,λ contributions
//  double acc_beta_lambda = 0.0;   // For β,λ contributions
//
//  // --- TERMS THAT INVOLVE THE OBSERVATIONS ---
//  // Loop over each observation to accumulate contributions from:
//  // L5: (α-1) Σ ln(x_i)  --> contributes only to first derivatives
//  // L6: (β-1) Σ ln(v), where v = 1 - x^α
//  // L7: (λ-1) Σ ln(w), where w = 1 - v^β
//  // L8: δ Σ ln(z), where z = 1 - w^λ
//  for (int i = 0; i < n; i++) {
//    double xi    = x(i);
//    double ln_xi = std::log(xi);
//
//    // -- Compute A = x^α and its derivatives --
//    double A = std::pow(xi, alpha);                  // A = x^α
//    double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
//    double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²
//
//    // -- v = 1 - A and its derivatives --
//    double v = 1.0 - A;                              // v = 1 - x^α
//    double ln_v = std::log(v);                       // ln(v)
//    double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
//    double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²
//
//    // --- L6: (β-1) ln(v) ---
//    // Second derivative w.r.t. α: (β-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
//    double d2L6_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
//    // Mixed derivative: d²L6/(dα dβ) = d/dβ[(β-1)*(dv_dalpha/v)] = (dv_dalpha/v)
//    double d2L6_dalpha_dbeta = dv_dalpha / v;
//
//    // --- L7: (λ - 1) ln(w), where w = 1 - v^β ---
//    double v_beta = std::pow(v, beta);               // v^β
//    double w = 1.0 - v_beta;                         // w = 1 - v^β
//    double ln_w = std::log(w);                       // ln(w)
//    // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
//    double dw_dv = -beta * std::pow(v, beta - 1.0);
//    // Chain rule: dw/dα = dw/dv * dv/dα
//    double dw_dalpha = dw_dv * dv_dalpha;
//    // Second derivative w.r.t. α for L7:
//    // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
//    // Computing d²w/dα²:
//    //   dw/dα = -β * v^(β-1)*dv_dalpha,
//    //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
//    double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
//                                    + std::pow(v, beta-1.0) * d2v_dalpha2);
//    double d2L7_dalpha2 = (lambda - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
//    // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
//    double dw_dbeta = -v_beta * ln_v;
//    // Second derivative w.r.t. β for L7:
//    // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
//    double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
//    double d2L7_dbeta2 = (lambda - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
//    // Mixed derivative L7 (α,β): d²/(dα dβ) ln(w) =
//    //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
//    double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
//    double d2L7_dalpha_dbeta = (lambda - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));
//
//    // --- L8: δ ln(z), where z = 1 - w^λ ---
//    double w_lambda_val = std::pow(w, lambda);       // w^λ
//    double z = 1.0 - w_lambda_val;                   // z = 1 - w^λ
//    // Derivative w.r.t. α: dz/dα = -λ * w^(λ-1) * dw/dα
//    double dz_dalpha = -lambda * std::pow(w, lambda-1.0) * dw_dalpha;
//    // Second derivative w.r.t. α for L8:
//    // d²z/dα² = -λ * [(λ-1)*w^(λ-2)*(dw/dα)² + w^(λ-1)*d²w/dα²]
//    double d2z_dalpha2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * (dw_dalpha*dw_dalpha)
//                                      + std::pow(w, lambda-1.0) * d2w_dalpha2);
//    double d2L8_dalpha2 = delta * ((d2z_dalpha2 * z - dz_dalpha*dz_dalpha)/(z*z));
//
//    // Derivative w.r.t. β: dz/dβ = -λ * w^(λ-1) * dw/dβ
//    double dz_dbeta = -lambda * std::pow(w, lambda-1.0) * dw_dbeta;
//    // Second derivative w.r.t. β for L8:
//    // d²z/dβ² = -λ * [(λ-1)*w^(λ-2)*(dw/dβ)² + w^(λ-1)*d²w/dβ²]
//    double d2z_dbeta2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * (dw_dbeta*dw_dbeta)
//                                     + std::pow(w, lambda-1.0) * d2w_dbeta2);
//    double d2L8_dbeta2 = delta * ((d2z_dbeta2 * z - dz_dbeta*dz_dbeta)/(z*z));
//
//    // Mixed derivative L8 (α,β): d²/(dα dβ) ln(z)
//    // = (d/dβ dz_dalpha)/z - (dz_dalpha*dz_dbeta)/(z*z)
//    double d_dw_dalpha_dbeta_2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * dw_dbeta * dw_dalpha
//                                              + std::pow(w, lambda-1.0) * d_dw_dalpha_dbeta);
//    double d2L8_dalpha_dbeta = delta * ((d_dw_dalpha_dbeta_2 / z) - (dz_dalpha*dz_dbeta)/(z*z));
//
//    // Derivatives of L8 with respect to λ:
//    // d/dλ ln(z) = (1/z)*dz/dλ, with dz/dλ = -w^λ ln(w)
//    double dz_dlambda = -w_lambda_val * ln_w;
//    // d²/dλ² ln(z) = [d²z/dλ² * z - (dz_dlambda)²]/z² (assuming w constant in λ)
//    double d2z_dlambda2 = -w_lambda_val * (ln_w * ln_w);
//    double d2L8_dlambda2 = delta * ((d2z_dlambda2 * z - dz_dlambda*dz_dlambda)/(z*z));
//
//    // Mixed derivative L8 (α,λ): d²/(dα dλ) ln(z) = (d/dα dz_dλ)/z - (dz_dλ*dz_dalpha)/(z*z)
//    double d_dalpha_dz_dlambda = -lambda * std::pow(w, lambda-1.0) * dw_dalpha * ln_w -
//      w_lambda_val * (dw_dalpha / w);
//    double d2L8_dalpha_dlambda = delta * ((d_dalpha_dz_dlambda / z) - (dz_dlambda*dz_dalpha)/(z*z));
//
//    // Mixed derivative L8 (β,λ): d²/(dβ dλ) ln(z) = (d/dβ dz_dλ)/z - (dz_dlambda*dz_dbeta)/(z*z)
//    double d_dbeta_dz_dlambda = -lambda * std::pow(w, lambda-1.0) * dw_dbeta * ln_w -
//      w_lambda_val * (dw_dbeta / w);
//    double d2L8_dbeta_dlambda = delta * ((d_dbeta_dz_dlambda / z) - (dz_dlambda*dz_dbeta)/(z*z));
//
//    // Mixed derivative L8 (δ,λ): d²/(dδ dλ) ln(z) = d/dλ [δ * (dz_dlambda / z)] = dz_dlambda / z
//    double d2L8_ddelta_dlambda = dz_dlambda / z;
//
//    // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
//    // Index: 0 = α, 1 = β, 2 = δ, 3 = λ
//
//    // H(α,α): sum of L2, L6, L7, and L8 (constants already added)
//    H(0,0) += d2L6_dalpha2 + d2L7_dalpha2 + d2L8_dalpha2;
//
//    // H(α,β): mixed from L6, L7, and L8
//    H(0,1) += d2L6_dalpha_dbeta + d2L7_dalpha_dbeta + d2L8_dalpha_dbeta;
//    H(1,0) = H(0,1);
//
//    // H(β,β): contributions from L3, L7, and L8
//    H(1,1) += d2L7_dbeta2 + d2L8_dbeta2;
//
//    // H(λ,λ): contributions from L1 and L8 (L1 already added)
//    H(3,3) += d2L8_dlambda2;
//
//    // H(α,δ): From L8 - d/dα ln(z)
//    H(0,2) += dz_dalpha / z;
//    H(2,0) = H(0,2);
//
//    // H(β,δ): From L8 - d/dβ ln(z)
//    H(1,2) += dz_dbeta / z;
//    H(2,1) = H(1,2);
//
//    // Accumulating terms for mixed derivatives with λ
//    // The term from L7 is Σ ln(w) derived w.r.t. α and Σ ln(w) derived w.r.t. β
//    acc_alpha_lambda += (dw_dalpha / w) + d2L8_dalpha_dlambda;
//    acc_beta_lambda += (dw_dbeta / w) + d2L8_dbeta_dlambda;
//    acc_delta_lambda += d2L8_ddelta_dlambda;
//
//  } // end of loop
//
//  // Applying mixed derivatives with λ
//  // H(α,λ):
//  H(0,3) = acc_alpha_lambda;
//  H(3,0) = H(0,3);
//
//  // H(β,λ):
//  H(1,3) = acc_beta_lambda;
//  H(3,1) = H(1,3);
//
//  // H(δ,λ):
//  H(2,3) = acc_delta_lambda;
//  H(3,2) = H(2,3);
//
//  // Returns the analytic Hessian matrix of the negative log-likelihood
//  return Rcpp::wrap(-H);
// }

// //' @title Analytic Hessian Matrix for Kumaraswamy-Kumaraswamy Distribution
// //'
// //' @description
// //' Computes the analytic Hessian matrix of the log-likelihood function for
// //' the Kumaraswamy-Kumaraswamy (KKw) distribution. This function provides
// //' exact second derivatives needed for optimization and inference.
// //'
// //' @param par Numeric vector of length 4 containing the parameters
// //'        (α, β, δ, λ) in that order. All parameters must be positive.
// //' @param data Numeric vector of observations, where all values must be
// //'        in the open interval (0,1).
// //'
// //' @return A 4×4 numeric matrix representing the Hessian of the negative
// //'         log-likelihood function. If parameters or data are invalid
// //'         (parameters ≤ 0 or data outside (0,1)), returns a matrix of
// //'         NaN values.
// //'
// //' @details
// //' The log-likelihood for the Kumaraswamy-Kumaraswamy distribution is:
// //'
// //' \deqn{
// //' \ell(\theta) = n \ln(\lambda) + n \ln(\alpha) + n \ln(\beta) + n \ln(\delta+1)
// //' + (\alpha-1) \sum \ln(x_i)
// //' + (\beta-1) \sum \ln(1 - x_i^\alpha)
// //' + (\lambda-1) \sum \ln\{1 - (1 - x_i^\alpha)^\beta\}
// //' + \delta \sum \ln\{1 - \{1 - (1 - x_i^\alpha)^\beta\}^\lambda\}
// //' }
// //'
// //' where γ is fixed at 1 for this distribution.
// //'
// //' The implementation computes all second derivatives analytically for each term.
// //' For computational efficiency, the following transformations are used:
// //' \itemize{
// //'   \item \deqn{A = x^α} and derivatives
// //'   \item \deqn{v = 1 - A}
// //'   \item \deqn{w = 1 - v^β}
// //'   \item \deqn{z = 1 - w^λ}
// //' }
// //'
// //' The returned Hessian matrix has the following structure:
// //' \itemize{
// //'   \item Rows/columns 1-4 correspond to α, β, δ, λ respectively
// //'   \item The matrix is symmetric (as expected for a Hessian)
// //'   \item The matrix represents second derivatives of the negative log-likelihood
// //' }
// //'
// //' This function is implemented in C++ for computational efficiency.
// //'
// //' @examples
// //' \donttest{
// //' # Generate sample data from a KKw distribution
// //' set.seed(123)
// //' x <- rkkw(100, 2, 3, 1.5, 0.5)
// //' hist(x, breaks = 20, main = "KKw(2, 3, 1.5, 0.5) Sample")
// //'
// //' # Use in optimization with Hessian-based methods
// //' result <- optim(c(0.5, 0.5, 0.5, 0.5), llkkw, method = "BFGS",
// //'                 hessian = TRUE, data = x)
// //'
// //' # Compare numerical and analytical derivatives
// //' num_grad <- numDeriv::grad(llkkw, x = result$par, data = x)
// //' num_hess <- numDeriv::hessian(llkkw, x = result$par, data = x)
// //'
// //' ana_grad <- grkkw(result$par, data = x)
// //' ana_hess <- hskkw(result$par, data = x)
// //'
// //' # Check differences (should be very small)
// //' round(num_grad - ana_grad, 4)
// //' round(num_hess - ana_hess, 4)
// //' }
// //'
// //'
// //' @references
// //' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes.
// //' Journal of Hydrology, 46(1-2), 79-88.
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
// //' Journal of Statistical Computation and Simulation, 81(7), 883-898.
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hskkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha  = par[0];   // θ[0] = α
//  double beta   = par[1];   // θ[1] = β
//  double delta  = par[2];   // θ[2] = δ
//  double lambda = par[3];   // θ[3] = λ
//
//  // Simple parameter validation (all > 0)
//  if(alpha <= 0 || beta <= 0 || delta <= 0 || lambda <= 0) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  // Data conversion and basic validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if(arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  int n = x.n_elem;  // sample size
//
//  // Initialize Hessian matrix H (of ℓ(θ)) as 4x4
//  arma::mat H(4,4, arma::fill::zeros);
//
//  // --- CONSTANT TERMS (do not depend on x) ---
//  // L1: n ln(λ)  => d²/dλ² = -n/λ²
//  H(3,3) += -n/(lambda*lambda);
//  // L2: n ln(α)  => d²/dα² = -n/α²
//  H(0,0) += -n/(alpha*alpha);
//  // L3: n ln(β)  => d²/dβ² = -n/β²
//  H(1,1) += -n/(beta*beta);
//  // L4: n ln(δ+1) => d²/dδ² = -n/(δ+1)²
//  H(2,2) += -n/std::pow(delta+1.0, 2.0);
//
//  // Accumulators for mixed derivatives with λ
//  double acc_delta_lambda = 0.0;  // Sum of dz_dlambda / z
//  double acc_alpha_lambda = 0.0;  // For α,λ contributions
//  double acc_beta_lambda = 0.0;   // For β,λ contributions
//
//  // --- TERMS THAT INVOLVE THE OBSERVATIONS ---
//  // Loop over each observation to accumulate contributions from:
//  // L5: (α-1) Σ ln(x_i)  --> contributes only to first derivatives
//  // L6: (β-1) Σ ln(v), where v = 1 - x^α
//  // L7: (λ-1) Σ ln(w), where w = 1 - v^β
//  // L8: δ Σ ln(z), where z = 1 - w^λ
//  for (int i = 0; i < n; i++) {
//    double xi    = x(i);
//    double ln_xi = std::log(xi);
//
//    // -- Compute A = x^α and its derivatives --
//    double A = std::pow(xi, alpha);                  // A = x^α
//    double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
//    double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²
//
//    // -- v = 1 - A and its derivatives --
//    double v = 1.0 - A;                              // v = 1 - x^α
//    double ln_v = std::log(v);                       // ln(v)
//    double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
//    double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²
//
//    // --- L6: (β-1) ln(v) ---
//    // First derivative w.r.t. α: (β-1) * (1/v)*dv_dalpha
//    double dL6_dalpha = (beta - 1.0) * (dv_dalpha / v);
//    // Second derivative w.r.t. α: (β-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
//    double d2L6_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
//    // First derivative w.r.t. β: dL6/dβ = ln(v)
//    double dL6_dbeta = ln_v;
//    // Mixed derivative: d²L6/(dα dβ) = d/dβ[(β-1)*(dv_dalpha/v)] = (dv_dalpha/v)
//    double d2L6_dalpha_dbeta = dv_dalpha / v;
//
//    // --- L7: (λ - 1) ln(w), where w = 1 - v^β ---
//    double v_beta = std::pow(v, beta);               // v^β
//    double w = 1.0 - v_beta;                         // w = 1 - v^β
//    double ln_w = std::log(w);                       // ln(w)
//    // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
//    double dw_dv = -beta * std::pow(v, beta - 1.0);
//    // Chain rule: dw/dα = dw/dv * dv/dα
//    double dw_dalpha = dw_dv * dv_dalpha;
//    // First derivative w.r.t. α: d/dα ln(w) = (1/w)*dw_dalpha
//    double dL7_dalpha = (lambda - 1.0) * (dw_dalpha / w);
//    // Second derivative w.r.t. α for L7:
//    // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
//    // Computing d²w/dα²:
//    //   dw/dα = -β * v^(β-1)*dv_dalpha,
//    //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
//    double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
//                                    + std::pow(v, beta-1.0) * d2v_dalpha2);
//    double d2L7_dalpha2 = (lambda - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
//    // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
//    double dw_dbeta = -v_beta * ln_v;
//    double dL7_dbeta = (lambda - 1.0) * (dw_dbeta / w);
//    // Second derivative w.r.t. β for L7:
//    // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
//    double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
//    double d2L7_dbeta2 = (lambda - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
//    // Mixed derivative L7 (α,β): d²/(dα dβ) ln(w) =
//    //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
//    // Approximate d/dβ dw_dalpha:
//    double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
//    double d2L7_dalpha_dbeta = (lambda - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));
//
//    // --- L8: δ ln(z), where z = 1 - w^λ ---
//    double w_lambda_val = std::pow(w, lambda);       // w^λ
//    double z = 1.0 - w_lambda_val;                   // z = 1 - w^λ
//    double ln_z = std::log(z);                       // ln(z)
//    // Derivative w.r.t. α: dz/dα = -λ * w^(λ-1) * dw/dα
//    double dz_dalpha = -lambda * std::pow(w, lambda-1.0) * dw_dalpha;
//    double dL8_dalpha = delta * (dz_dalpha / z);
//    // Second derivative w.r.t. α for L8:
//    // d²z/dα² = -λ * [(λ-1)*w^(λ-2)*(dw/dα)² + w^(λ-1)*d²w/dα²]
//    double d2w_dalpha2_dummy = d2w_dalpha2; // already calculated for L7
//    double d2z_dalpha2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * (dw_dalpha*dw_dalpha)
//                                      + std::pow(w, lambda-1.0) * d2w_dalpha2_dummy);
//    double d2L8_dalpha2 = delta * ((d2z_dalpha2 * z - dz_dalpha*dz_dalpha)/(z*z));
//
//    // Derivative w.r.t. β: dz/dβ = -λ * w^(λ-1) * dw/dβ
//    double dz_dbeta = -lambda * std::pow(w, lambda-1.0) * dw_dbeta;
//    double dL8_dbeta = delta * (dz_dbeta / z);
//    // Second derivative w.r.t. β for L8:
//    // d²z/dβ² = -λ * [(λ-1)*w^(λ-2)*(dw/dβ)² + w^(λ-1)*d²w/dβ²]
//    double d2z_dbeta2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * (dw_dbeta*dw_dbeta)
//                                     + std::pow(w, lambda-1.0) * d2w_dbeta2);
//    double d2L8_dbeta2 = delta * ((d2z_dbeta2 * z - dz_dbeta*dz_dbeta)/(z*z));
//
//    // Mixed derivative L8 (α,β): d²/(dα dβ) ln(z)
//    // = (d/dβ dz_dalpha)/z - (dz_dalpha*dz_dbeta)/(z*z)
//    // Approximate d/dβ dz_dalpha = -λ * [(λ-1)*w^(λ-2)*(dw_dβ*dw_dα) + w^(λ-1)*(d/dβ dw_dalpha)]
//    double d_dw_dalpha_dbeta_2 = -lambda * ((lambda - 1.0) * std::pow(w, lambda-2.0) * dw_dbeta * dw_dalpha
//                                              + std::pow(w, lambda-1.0) * d_dw_dalpha_dbeta);
//    double d2L8_dalpha_dbeta = delta * ((d_dw_dalpha_dbeta_2 / z) - (dz_dalpha*dz_dbeta)/(z*z));
//
//    // Derivatives of L8 with respect to λ:
//    // d/dλ ln(z) = (1/z)*dz/dλ, with dz/dλ = -w^λ ln(w)
//    double dz_dlambda = -w_lambda_val * ln_w;
//    double dL8_dlambda = delta * (dz_dlambda / z);
//    // d²/dλ² ln(z) = [d²z/dλ² * z - (dz_dlambda)²]/z² (assuming w constant in λ)
//    double d2z_dlambda2 = -w_lambda_val * (ln_w * ln_w);
//    double d2L8_dlambda2 = delta * ((d2z_dlambda2 * z - dz_dlambda*dz_dlambda)/(z*z));
//
//    // Mixed derivative L8 (α,λ): d²/(dα dλ) ln(z) = (d/dα dz_dλ)/z - (dz_dλ*dz_dalpha)/(z*z)
//    // The correct formula for d/dα[dz_dλ] = d/dα[-w^λ*ln(w)]
//    // = -λ*w^(λ-1)*dw/dα*ln(w) - w^λ*(1/w)*dw/dα
//    double d_dalpha_dz_dlambda = -lambda * std::pow(w, lambda-1.0) * dw_dalpha * ln_w -
//      w_lambda_val * (dw_dalpha / w);
//    double d2L8_dalpha_dlambda = delta * ((d_dalpha_dz_dlambda / z) - (dz_dlambda*dz_dalpha)/(z*z));
//
//    // Mixed derivative L8 (β,λ): d²/(dβ dλ) ln(z) = (d/dβ dz_dλ)/z - (dz_dlambda*dz_dbeta)/(z*z)
//    // The correct formula for d/dβ[dz_dλ] = d/dβ[-w^λ*ln(w)]
//    // = -λ*w^(λ-1)*dw/dβ*ln(w) - w^λ*(1/w)*dw/dβ
//    double d_dbeta_dz_dlambda = -lambda * std::pow(w, lambda-1.0) * dw_dbeta * ln_w -
//      w_lambda_val * (dw_dbeta / w);
//    double d2L8_dbeta_dlambda = delta * ((d_dbeta_dz_dlambda / z) - (dz_dlambda*dz_dbeta)/(z*z));
//
//    // Mixed derivative L8 (δ,λ): d²/(dδ dλ) ln(z) = d/dλ [δ * (dz_dlambda / z)] = dz_dlambda / z
//    double d2L8_ddelta_dlambda = dz_dlambda / z;
//
//    // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
//    // Index: 0 = α, 1 = β, 2 = δ, 3 = λ
//
//    // H(α,α): sum of L2, L6, L7, and L8 (constants already added)
//    H(0,0) += d2L6_dalpha2 + d2L7_dalpha2 + d2L8_dalpha2;
//
//    // H(α,β): mixed from L6, L7, and L8
//    H(0,1) += d2L6_dalpha_dbeta + d2L7_dalpha_dbeta + d2L8_dalpha_dbeta;
//    H(1,0) = H(0,1);
//
//    // H(β,β): contributions from L3, L7, and L8
//    H(1,1) += d2L7_dbeta2 + d2L8_dbeta2;
//
//    // H(λ,λ): contributions from L1 and L8 (L1 already added)
//    H(3,3) += d2L8_dlambda2;
//
//    // H(α,δ): From L8 - d/dα ln(z)
//    H(0,2) += dz_dalpha / z;
//    H(2,0) = H(0,2);
//
//    // H(β,δ): From L8 - d/dβ ln(z)
//    H(1,2) += dz_dbeta / z;
//    H(2,1) = H(1,2);
//
//    // Accumulating terms for mixed derivatives with λ
//    // The term from L7 is Σ ln(w) derived w.r.t. α and Σ ln(w) derived w.r.t. β
//    acc_alpha_lambda += (dw_dalpha / w) + d2L8_dalpha_dlambda;
//    acc_beta_lambda += (dw_dbeta / w) + d2L8_dbeta_dlambda;
//    acc_delta_lambda += d2L8_ddelta_dlambda;
//
//  } // end of loop
//
//  // Applying mixed derivatives with λ
//  // H(α,λ):
//  H(0,3) = acc_alpha_lambda;
//  H(3,0) = H(0,3);
//
//  // H(β,λ):
//  H(1,3) = acc_beta_lambda;
//  H(3,1) = H(1,3);
//
//  // H(δ,λ):
//  H(2,3) = acc_delta_lambda;
//  H(3,2) = H(2,3);
//
//  // Returns the analytic Hessian matrix of the negative log-likelihood
//  return Rcpp::wrap(-H);
// }





/*
----------------------------------------------------------------------------
REUSE OF THE NUMERIC STABILITY FUNCTIONS AND CHECKS
----------------------------------------------------------------------------
NOTE: We assume the following inline functions are already implemented
and available in the compilation environment, as requested:
- log1mexp(double)
- log1pexp(double)
- safe_log(double)
- safe_exp(double)
- safe_pow(double, double)
- check_pars(...) or any analogous checker
- etc.

For clarity, below we define a small parameter-check function specifically
for the Beta-Kumaraswamy (BKw) distribution, which needs alpha>0, beta>0,
gamma>0, delta>=0. We do NOT have lambda in BKw because it's fixed at 1.
*/

// -----------------------------------------------------------------------------
// Parameter Checker for Beta-Kumaraswamy (BKw) distribution
// -----------------------------------------------------------------------------
inline bool check_bkw_pars(double alpha,
                       double beta,
                       double gamma,
                       double delta,
                       bool strict = false) {
if (alpha <= 0.0 || beta <= 0.0 || gamma <= 0.0 || delta < 0.0) {
return false;
}

if (strict) {
// Optional stricter numeric bounds
const double MIN_PARAM = 1e-5;
const double MAX_PARAM = 1e5;
if (alpha < MIN_PARAM || beta < MIN_PARAM || gamma < MIN_PARAM || delta > MAX_PARAM) {
  return false;
}
if (alpha > MAX_PARAM || beta > MAX_PARAM || gamma > MAX_PARAM) {
  return false;
}
}
return true;
}

/*
----------------------------------------------------------------------------
BETA-KUMARASWAMY (BKw) DISTRIBUTION
----------------------------------------------------------------------------
PDF:
f(x; α, β, γ, δ) = (α β / B(γ, δ+1)) x^(α-1) (1 - x^α)^( β(δ+1) - 1 )
[ 1 - (1 - x^α)^β ]^(γ - 1)

CDF:
F(x; α, β, γ, δ) = I_{ [1 - (1 - x^α)^β ] } ( γ, δ + 1 )

QUANTILE:
Q(p; α, β, γ, δ) = { 1 - [1 - ( I^{-1}_{p}(γ, δ+1) ) ]^(1/β) }^(1/α)
(But see transformations step-by-step in code for numeric stability.)

RNG:
If V ~ Beta(γ, δ+1) then
X = { 1 - [1 - V ]^(1/β) }^(1/α)

LOG-LIKELIHOOD:
ℓ(θ) = n log(α β) - n log B(γ, δ+1)
+ Σ { (α-1) log(x_i) + [β(δ+1)-1] log(1 - x_i^α) + (γ - 1) log( 1 - (1 - x_i^α)^β ) }

This file defines:
- dbkw()  : density
- pbkw()  : cumulative distribution
- qbkw()  : quantile
- rbkw()  : random generation
- llbkw() : negative log-likelihood
*/


// -----------------------------------------------------------------------------
// 1) dbkw: PDF of Beta-Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Density of the Beta-Kumaraswamy (BKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution density
//'
//' @description
//' Computes the probability density function (PDF) for the Beta-Kumaraswamy
//' (BKw) distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}).
//' This distribution is defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned (\eqn{\log(f(x))}). Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{alpha}, \code{beta},
//'   \code{gamma}, \code{delta}). Returns \code{0} (or \code{-Inf} if
//'   \code{log_prob = TRUE}) for \code{x} outside the interval (0, 1), or
//'   \code{NaN} if parameters are invalid (e.g., \code{alpha <= 0}, \code{beta <= 0},
//'   \code{gamma <= 0}, \code{delta < 0}).
//'
//' @details
//' The probability density function (PDF) of the Beta-Kumaraswamy (BKw)
//' distribution is given by:
//' \deqn{
//' f(x; \alpha, \beta, \gamma, \delta) = \frac{\alpha \beta}{B(\gamma, \delta+1)} x^{\alpha - 1} \bigl(1 - x^\alpha\bigr)^{\beta(\delta+1) - 1} \bigl[1 - \bigl(1 - x^\alpha\bigr)^\beta\bigr]^{\gamma - 1}
//' }
//' for \eqn{0 < x < 1}, where \eqn{B(a,b)} is the Beta function
//' (\code{\link[base]{beta}}).
//'
//' The BKw distribution is a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{dgkw}}) obtained
//' by setting the parameter \eqn{\lambda = 1}.
//' Numerical evaluation is performed using algorithms similar to those for `dgkw`,
//' ensuring stability.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{dgkw}} (parent distribution density),
//' \code{\link{pbkw}}, \code{\link{qbkw}}, \code{\link{rbkw}} (other BKw functions),
//'
//' @examples
//' \donttest{
//' # Example values
//' x_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 1.5
//' gamma_par <- 1.0 # Equivalent to Kw when gamma=1
//' delta_par <- 0.5
//'
//' # Calculate density
//' densities <- dbkw(x_vals, alpha_par, beta_par, gamma_par, delta_par)
//' print(densities)
//'
//' # Calculate log-density
//' log_densities <- dbkw(x_vals, alpha_par, beta_par, gamma_par, delta_par,
//'                       log_prob = TRUE)
//' print(log_densities)
//' # Check: should match log(densities)
//' print(log(densities))
//'
//' # Compare with dgkw setting lambda = 1
//' densities_gkw <- dgkw(x_vals, alpha_par, beta_par, gamma = gamma_par,
//'                       delta = delta_par, lambda = 1.0)
//' print(paste("Max difference:", max(abs(densities - densities_gkw)))) # Should be near zero
//'
//' # Plot the density for different gamma values
//' curve_x <- seq(0.01, 0.99, length.out = 200)
//' curve_y1 <- dbkw(curve_x, alpha = 2, beta = 3, gamma = 0.5, delta = 1)
//' curve_y2 <- dbkw(curve_x, alpha = 2, beta = 3, gamma = 1.0, delta = 1)
//' curve_y3 <- dbkw(curve_x, alpha = 2, beta = 3, gamma = 2.0, delta = 1)
//'
//' plot(curve_x, curve_y1, type = "l", main = "BKw Density Examples (alpha=2, beta=3, delta=1)",
//'      xlab = "x", ylab = "f(x)", col = "blue", ylim = range(0, curve_y1, curve_y2, curve_y3))
//' lines(curve_x, curve_y2, col = "red")
//' lines(curve_x, curve_y3, col = "green")
//' legend("topright", legend = c("gamma=0.5", "gamma=1.0", "gamma=2.0"),
//'        col = c("blue", "red", "green"), lty = 1, bty = "n")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dbkw(
 const arma::vec& x,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 bool log_prob = false
) {
// Convert to arma::vec
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());

// Broadcast length
size_t n = std::max({x.n_elem,
                   alpha_vec.n_elem,
                   beta_vec.n_elem,
                   gamma_vec.n_elem,
                   delta_vec.n_elem});

// Result
arma::vec result(n);
result.fill(log_prob ? R_NegInf : 0.0);

for (size_t i = 0; i < n; ++i) {
 double a = alpha_vec[i % alpha_vec.n_elem];
 double b = beta_vec[i % beta_vec.n_elem];
 double g = gamma_vec[i % gamma_vec.n_elem];
 double d = delta_vec[i % delta_vec.n_elem];
 double xx = x[i % x.n_elem];

 // Check parameter validity
 if (!check_bkw_pars(a, b, g, d)) {
   // Invalid params => density = 0 or -Inf
   continue;
 }

 // Outside (0,1) => density=0 or log_density=-Inf
 if (xx <= 0.0 || xx >= 1.0 || !R_finite(xx)) {
   continue;
 }

 // PDF formula
 // f(x) = (alpha*beta / B(gamma, delta+1)) *
 //        x^(alpha-1) * (1 - x^alpha)^(beta*(delta+1) - 1) *
 //        [1 - (1 - x^alpha)^beta]^(gamma - 1)

 // Precompute log_B = lbeta(g, d+1)
 double logB = R::lbeta(g, d + 1.0);
 double log_const = std::log(a) + std::log(b) - logB;

 double lx = std::log(xx);
 double xalpha = a * lx;                    // log(x^alpha) = a * log(x)
 double log_1_minus_xalpha = log1mexp(xalpha);

 // (beta*(delta+1) - 1) * log(1 - x^alpha)
 double exponent1 = b * (d + 1.0) - 1.0;
 double term1 = exponent1 * log_1_minus_xalpha;

 // [1 - (1 - x^alpha)^beta]^(gamma - 1)
 // log(1 - (1 - x^alpha)^beta) = log1mexp( b * log(1 - x^alpha) )
 double log_1_minus_xalpha_beta = b * log_1_minus_xalpha;
 double log_bracket = log1mexp(log_1_minus_xalpha_beta);
 double exponent2 = g - 1.0;
 double term2 = exponent2 * log_bracket;

 // Full log pdf
 double log_pdf = log_const +
   (a - 1.0) * lx +
   term1 +
   term2;

 if (log_prob) {
   result(i) = log_pdf;
 } else {
   // exp safely
   result(i) = std::exp(log_pdf);
 }
}

return Rcpp::NumericVector(result.memptr(), result.memptr() + result.n_elem);
}


// -----------------------------------------------------------------------------
// 2) pbkw: CDF of Beta-Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Cumulative Distribution Function (CDF) of the Beta-Kumaraswamy (BKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution cumulative
//'
//' @description
//' Computes the cumulative distribution function (CDF), \eqn{P(X \le q)}, for the
//' Beta-Kumaraswamy (BKw) distribution with parameters \code{alpha} (\eqn{\alpha}),
//' \code{beta} (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta}
//' (\eqn{\delta}). This distribution is defined on the interval (0, 1) and is
//' a special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\lambda = 1}.
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms/complements
//'   depending on \code{lower_tail} and \code{log_p}. The length of the result
//'   is determined by the recycling rule applied to the arguments (\code{q},
//'   \code{alpha}, \code{beta}, \code{gamma}, \code{delta}). Returns \code{0}
//'   (or \code{-Inf} if \code{log_p = TRUE}) for \code{q <= 0} and \code{1}
//'   (or \code{0} if \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN}
//'   for invalid parameters.
//'
//' @details
//' The Beta-Kumaraswamy (BKw) distribution is a special case of the
//' five-parameter Generalized Kumaraswamy distribution (\code{\link{pgkw}})
//' obtained by setting the shape parameter \eqn{\lambda = 1}.
//'
//' The CDF of the GKw distribution is \eqn{F_{GKw}(q) = I_{y(q)}(\gamma, \delta+1)},
//' where \eqn{y(q) = [1-(1-q^{\alpha})^{\beta}]^{\lambda}} and \eqn{I_x(a,b)}
//' is the regularized incomplete beta function (\code{\link[stats]{pbeta}}).
//' Setting \eqn{\lambda=1} simplifies \eqn{y(q)} to \eqn{1 - (1 - q^\alpha)^\beta},
//' yielding the BKw CDF:
//' \deqn{
//' F(q; \alpha, \beta, \gamma, \delta) = I_{1 - (1 - q^\alpha)^\beta}(\gamma, \delta+1)
//' }
//' This is evaluated using the \code{\link[stats]{pbeta}} function.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{pgkw}} (parent distribution CDF),
//' \code{\link{dbkw}}, \code{\link{qbkw}}, \code{\link{rbkw}} (other BKw functions),
//' \code{\link[stats]{pbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' q_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 1.5
//' gamma_par <- 1.0
//' delta_par <- 0.5
//'
//' # Calculate CDF P(X <= q)
//' probs <- pbkw(q_vals, alpha_par, beta_par, gamma_par, delta_par)
//' print(probs)
//'
//' # Calculate upper tail P(X > q)
//' probs_upper <- pbkw(q_vals, alpha_par, beta_par, gamma_par, delta_par,
//'                     lower_tail = FALSE)
//' print(probs_upper)
//' # Check: probs + probs_upper should be 1
//' print(probs + probs_upper)
//'
//' # Calculate log CDF
//' log_probs <- pbkw(q_vals, alpha_par, beta_par, gamma_par, delta_par,
//'                   log_p = TRUE)
//' print(log_probs)
//' # Check: should match log(probs)
//' print(log(probs))
//'
//' # Compare with pgkw setting lambda = 1
//' probs_gkw <- pgkw(q_vals, alpha_par, beta_par, gamma = gamma_par,
//'                  delta = delta_par, lambda = 1.0)
//' print(paste("Max difference:", max(abs(probs - probs_gkw)))) # Should be near zero
//'
//' # Plot the CDF
//' curve_q <- seq(0.01, 0.99, length.out = 200)
//' curve_p <- pbkw(curve_q, alpha = 2, beta = 3, gamma = 0.5, delta = 1)
//' plot(curve_q, curve_p, type = "l", main = "BKw CDF Example",
//'      xlab = "q", ylab = "F(q)", col = "blue", ylim = c(0, 1))
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pbkw(
 const arma::vec& q,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 bool lower_tail = true,
 bool log_p = false
) {
// Convert
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());

// Broadcast
size_t n = std::max({q.n_elem,
                   alpha_vec.n_elem,
                   beta_vec.n_elem,
                   gamma_vec.n_elem,
                   delta_vec.n_elem});

arma::vec res(n);

for (size_t i = 0; i < n; ++i) {
 double a = alpha_vec[i % alpha_vec.n_elem];
 double b = beta_vec[i % beta_vec.n_elem];
 double g = gamma_vec[i % gamma_vec.n_elem];
 double d = delta_vec[i % delta_vec.n_elem];
 double xx = q[i % q.n_elem];

 if (!check_bkw_pars(a, b, g, d)) {
   res(i) = NA_REAL;
   continue;
 }

 if (!R_finite(xx) || xx <= 0.0) {
   // x=0 => F=0
   double prob0 = lower_tail ? 0.0 : 1.0;
   res(i) = log_p ? std::log(prob0) : prob0;
   continue;
 }

 if (xx >= 1.0) {
   // x=1 => F=1
   double prob1 = lower_tail ? 1.0 : 0.0;
   res(i) = log_p ? std::log(prob1) : prob1;
   continue;
 }

 // We want z = 1 - (1 - x^alpha)^beta
 double lx = std::log(xx);
 double xalpha = std::exp(a * lx);
 double one_minus_xalpha = 1.0 - xalpha;

 if (one_minus_xalpha <= 0.0) {
   // F(x) ~ 1 if x^alpha>=1
   double prob1 = lower_tail ? 1.0 : 0.0;
   res(i) = log_p ? std::log(prob1) : prob1;
   continue;
 }

 double temp = 1.0 - std::pow(one_minus_xalpha, b);
 if (temp <= 0.0) {
   double prob0 = lower_tail ? 0.0 : 1.0;
   res(i) = log_p ? std::log(prob0) : prob0;
   continue;
 }

 if (temp >= 1.0) {
   double prob1 = lower_tail ? 1.0 : 0.0;
   res(i) = log_p ? std::log(prob1) : prob1;
   continue;
 }

 // Then F(x) = pbeta(temp, gamma, delta+1, TRUE, FALSE)
 double val = R::pbeta(temp, g, d+1.0, true, false); // F
 if (!lower_tail) {
   val = 1.0 - val;
 }
 if (log_p) {
   val = std::log(val);
 }
 res(i) = val;
}

return Rcpp::NumericVector(res.memptr(), res.memptr() + res.n_elem);
}


// -----------------------------------------------------------------------------
// 3) qbkw: QUANTILE of Beta-Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Quantile Function of the Beta-Kumaraswamy (BKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution quantile
//'
//' @description
//' Computes the quantile function (inverse CDF) for the Beta-Kumaraswamy (BKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}).
//' It finds the value \code{q} such that \eqn{P(X \le q) = p}. This distribution
//' is a special case of the Generalized Kumaraswamy (GKw) distribution where
//' the parameter \eqn{\lambda = 1}.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are \eqn{p = P(X \le q)},
//'   otherwise, probabilities are \eqn{p = P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{alpha}, \code{beta}, \code{gamma}, \code{delta}).
//'   Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{alpha <= 0},
//'           \code{beta <= 0}, \code{gamma <= 0}, \code{delta < 0}).
//'   }
//'   Boundary return values are adjusted accordingly for \code{lower_tail = FALSE}.
//'
//' @details
//' The quantile function \eqn{Q(p)} is the inverse of the CDF \eqn{F(q)}. The CDF
//' for the BKw (\eqn{\lambda=1}) distribution is \eqn{F(q) = I_{y(q)}(\gamma, \delta+1)},
//' where \eqn{y(q) = 1 - (1 - q^\alpha)^\beta} and \eqn{I_z(a,b)} is the
//' regularized incomplete beta function (see \code{\link{pbkw}}).
//'
//' To find the quantile \eqn{q}, we first invert the outer Beta part: let
//' \eqn{y = I^{-1}_{p}(\gamma, \delta+1)}, where \eqn{I^{-1}_p(a,b)} is the
//' inverse of the regularized incomplete beta function, computed via
//' \code{\link[stats]{qbeta}}. Then, we invert the inner Kumaraswamy part:
//' \eqn{y = 1 - (1 - q^\alpha)^\beta}, which leads to \eqn{q = \{1 - (1-y)^{1/\beta}\}^{1/\alpha}}.
//' Substituting \eqn{y} gives the quantile function:
//' \deqn{
//' Q(p) = \left\{ 1 - \left[ 1 - I^{-1}_{p}(\gamma, \delta+1) \right]^{1/\beta} \right\}^{1/\alpha}
//' }
//' The function uses this formula, calculating \eqn{I^{-1}_{p}(\gamma, \delta+1)}
//' via \code{qbeta(p, gamma, delta + 1, ...)} while respecting the
//' \code{lower_tail} and \code{log_p} arguments.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' @seealso
//' \code{\link{qgkw}} (parent distribution quantile function),
//' \code{\link{dbkw}}, \code{\link{pbkw}}, \code{\link{rbkw}} (other BKw functions),
//' \code{\link[stats]{qbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' p_vals <- c(0.1, 0.5, 0.9)
//' alpha_par <- 2.0
//' beta_par <- 1.5
//' gamma_par <- 1.0
//' delta_par <- 0.5
//'
//' # Calculate quantiles
//' quantiles <- qbkw(p_vals, alpha_par, beta_par, gamma_par, delta_par)
//' print(quantiles)
//'
//' # Calculate quantiles for upper tail probabilities P(X > q) = p
//' quantiles_upper <- qbkw(p_vals, alpha_par, beta_par, gamma_par, delta_par,
//'                         lower_tail = FALSE)
//' print(quantiles_upper)
//' # Check: qbkw(p, ..., lt=F) == qbkw(1-p, ..., lt=T)
//' print(qbkw(1 - p_vals, alpha_par, beta_par, gamma_par, delta_par))
//'
//' # Calculate quantiles from log probabilities
//' log_p_vals <- log(p_vals)
//' quantiles_logp <- qbkw(log_p_vals, alpha_par, beta_par, gamma_par, delta_par,
//'                        log_p = TRUE)
//' print(quantiles_logp)
//' # Check: should match original quantiles
//' print(quantiles)
//'
//' # Compare with qgkw setting lambda = 1
//' quantiles_gkw <- qgkw(p_vals, alpha_par, beta_par, gamma = gamma_par,
//'                      delta = delta_par, lambda = 1.0)
//' print(paste("Max difference:", max(abs(quantiles - quantiles_gkw)))) # Should be near zero
//'
//' # Verify inverse relationship with pbkw
//' p_check <- 0.75
//' q_calc <- qbkw(p_check, alpha_par, beta_par, gamma_par, delta_par)
//' p_recalc <- pbkw(q_calc, alpha_par, beta_par, gamma_par, delta_par)
//' print(paste("Original p:", p_check, " Recalculated p:", p_recalc))
//' # abs(p_check - p_recalc) < 1e-9 # Should be TRUE
//'
//' # Boundary conditions
//' print(qbkw(c(0, 1), alpha_par, beta_par, gamma_par, delta_par)) # Should be 0, 1
//' print(qbkw(c(-Inf, 0), alpha_par, beta_par, gamma_par, delta_par, log_p = TRUE)) # Should be 0, 1
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qbkw(
 const arma::vec& p,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());

size_t n = std::max({p.n_elem,
                   alpha_vec.n_elem,
                   beta_vec.n_elem,
                   gamma_vec.n_elem,
                   delta_vec.n_elem});

arma::vec res(n);

for (size_t i = 0; i < n; ++i) {
 double a = alpha_vec[i % alpha_vec.n_elem];
 double b = beta_vec[i % beta_vec.n_elem];
 double g = gamma_vec[i % gamma_vec.n_elem];
 double d = delta_vec[i % delta_vec.n_elem];
 double pp = p[i % p.n_elem];

 if (!check_bkw_pars(a, b, g, d)) {
   res(i) = NA_REAL;
   continue;
 }

 // Convert from log_p if needed
 if (log_p) {
   if (pp > 0.0) {
     // log(p) > 0 => p>1 => invalid
     res(i) = NA_REAL;
     continue;
   }
   pp = std::exp(pp);
 }
 // Convert if upper tail
 if (!lower_tail) {
   pp = 1.0 - pp;
 }

 // Check boundaries
 if (pp <= 0.0) {
   res(i) = 0.0;
   continue;
 } else if (pp >= 1.0) {
   res(i) = 1.0;
   continue;
 }

 // We do: y = qbeta(pp, gamma, delta+1)
 double y = R::qbeta(pp, g, d+1.0, true, false);
 if (y <= 0.0) {
   res(i) = 0.0;
   continue;
 } else if (y >= 1.0) {
   res(i) = 1.0;
   continue;
 }

 // Then x = {1 - [1 - y]^(1/b)}^(1/a)
 double part = 1.0 - y;
 if (part <= 0.0) {
   res(i) = 1.0;
   continue;
 } else if (part >= 1.0) {
   res(i) = 0.0;
   continue;
 }

 double inner = std::pow(part, 1.0/b);
 double xval = 1.0 - inner;
 if (xval < 0.0)  xval = 0.0;
 if (xval > 1.0)  xval = 1.0;

 if (a == 1.0) {
   // small optimization
   res(i) = xval;
 } else {
   double qv = std::pow(xval, 1.0/a);
   if (qv < 0.0)      qv = 0.0;
   else if (qv > 1.0) qv = 1.0;
   res(i) = qv;
 }
}

return Rcpp::NumericVector(res.memptr(), res.memptr() + res.n_elem);
}


// -----------------------------------------------------------------------------
// 4) rbkw: RNG for Beta-Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Random Number Generation for the Beta-Kumaraswamy (BKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution random
//'
//' @description
//' Generates random deviates from the Beta-Kumaraswamy (BKw) distribution
//' with parameters \code{alpha} (\eqn{\alpha}), \code{beta} (\eqn{\beta}),
//' \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}). This distribution
//' is a special case of the Generalized Kumaraswamy (GKw) distribution where
//' the parameter \eqn{\lambda = 1}.
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//'
//' @return A vector of length \code{n} containing random deviates from the BKw
//'   distribution. The length of the result is determined by \code{n} and the
//'   recycling rule applied to the parameters (\code{alpha}, \code{beta},
//'   \code{gamma}, \code{delta}). Returns \code{NaN} if parameters
//'   are invalid (e.g., \code{alpha <= 0}, \code{beta <= 0}, \code{gamma <= 0},
//'   \code{delta < 0}).
//'
//' @details
//' The generation method uses the relationship between the GKw distribution and the
//' Beta distribution. The general procedure for GKw (\code{\link{rgkw}}) is:
//' If \eqn{W \sim \mathrm{Beta}(\gamma, \delta+1)}, then
//' \eqn{X = \{1 - [1 - W^{1/\lambda}]^{1/\beta}\}^{1/\alpha}} follows the
//' GKw(\eqn{\alpha, \beta, \gamma, \delta, \lambda}) distribution.
//'
//' For the BKw distribution, \eqn{\lambda=1}. Therefore, the algorithm simplifies to:
//' \enumerate{
//'   \item Generate \eqn{V \sim \mathrm{Beta}(\gamma, \delta+1)} using
//'         \code{\link[stats]{rbeta}}.
//'   \item Compute the BKw variate \eqn{X = \{1 - (1 - V)^{1/\beta}\}^{1/\alpha}}.
//' }
//' This procedure is implemented efficiently, handling parameter recycling as needed.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Devroye, L. (1986). *Non-Uniform Random Variate Generation*. Springer-Verlag.
//' (General methods for random variate generation).
//'
//' @seealso
//' \code{\link{rgkw}} (parent distribution random generation),
//' \code{\link{dbkw}}, \code{\link{pbkw}}, \code{\link{qbkw}} (other BKw functions),
//' \code{\link[stats]{rbeta}}
//'
//' @examples
//' \donttest{
//' set.seed(2026) # for reproducibility
//'
//' # Generate 1000 random values from a specific BKw distribution
//' alpha_par <- 2.0
//' beta_par <- 1.5
//' gamma_par <- 1.0
//' delta_par <- 0.5
//'
//' x_sample_bkw <- rbkw(1000, alpha = alpha_par, beta = beta_par,
//'                      gamma = gamma_par, delta = delta_par)
//' summary(x_sample_bkw)
//'
//' # Histogram of generated values compared to theoretical density
//' hist(x_sample_bkw, breaks = 30, freq = FALSE, # freq=FALSE for density
//'      main = "Histogram of BKw Sample", xlab = "x", ylim = c(0, 2.5))
//' curve(dbkw(x, alpha = alpha_par, beta = beta_par, gamma = gamma_par,
//'            delta = delta_par),
//'       add = TRUE, col = "red", lwd = 2, n = 201)
//' legend("topright", legend = "Theoretical PDF", col = "red", lwd = 2, bty = "n")
//'
//' # Comparing empirical and theoretical quantiles (Q-Q plot)
//' prob_points <- seq(0.01, 0.99, by = 0.01)
//' theo_quantiles <- qbkw(prob_points, alpha = alpha_par, beta = beta_par,
//'                        gamma = gamma_par, delta = delta_par)
//' emp_quantiles <- quantile(x_sample_bkw, prob_points, type = 7)
//'
//' plot(theo_quantiles, emp_quantiles, pch = 16, cex = 0.8,
//'      main = "Q-Q Plot for BKw Distribution",
//'      xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles (n=1000)")
//' abline(a = 0, b = 1, col = "blue", lty = 2)
//'
//' # Compare summary stats with rgkw(..., lambda=1, ...)
//' # Note: individual values will differ due to randomness
//' x_sample_gkw <- rgkw(1000, alpha = alpha_par, beta = beta_par, gamma = gamma_par,
//'                      delta = delta_par, lambda = 1.0)
//' print("Summary stats for rbkw sample:")
//' print(summary(x_sample_bkw))
//' print("Summary stats for rgkw(lambda=1) sample:")
//' print(summary(x_sample_gkw)) # Should be similar
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rbkw(
 int n,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta
) {
if (n <= 0) {
 Rcpp::stop("rbkw: n must be positive");
}

arma::vec alpha_vec(alpha.begin(), alpha.size());
arma::vec beta_vec(beta.begin(), beta.size());
arma::vec gamma_vec(gamma.begin(), gamma.size());
arma::vec delta_vec(delta.begin(), delta.size());

size_t k = std::max({alpha_vec.n_elem,
                   beta_vec.n_elem,
                   gamma_vec.n_elem,
                   delta_vec.n_elem});

arma::vec out(n);

for (int i = 0; i < n; ++i) {
 size_t idx = i % k;
 double a = alpha_vec[idx % alpha_vec.n_elem];
 double b = beta_vec[idx % beta_vec.n_elem];
 double g = gamma_vec[idx % gamma_vec.n_elem];
 double d = delta_vec[idx % delta_vec.n_elem];

 if (!check_bkw_pars(a, b, g, d)) {
   out(i) = NA_REAL;
   Rcpp::warning("rbkw: invalid parameters at index %d", i+1);
   continue;
 }

 // V ~ Beta(g, d+1)
 double V = R::rbeta(g, d + 1.0);
 // X = {1 - (1 - V)^(1/b)}^(1/a)
 double one_minus_V = 1.0 - V;
 if (one_minus_V <= 0.0) {
   out(i) = 1.0;
   continue;
 }
 if (one_minus_V >= 1.0) {
   out(i) = 0.0;
   continue;
 }

 double temp = std::pow(one_minus_V, 1.0/b);
 double xval = 1.0 - temp;
 if (xval < 0.0)  xval = 0.0;
 if (xval > 1.0)  xval = 1.0;

 if (a == 1.0) {
   out(i) = xval;
 } else {
   double rv = std::pow(xval, 1.0/a);
   if (rv < 0.0) rv = 0.0;
   if (rv > 1.0) rv = 1.0;
   out(i) = rv;
 }
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


//' @title Negative Log-Likelihood for Beta-Kumaraswamy (BKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize
//'
//' @description
//' Computes the negative log-likelihood function for the Beta-Kumaraswamy (BKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}),
//' given a vector of observations. This distribution is the special case of the
//' Generalized Kumaraswamy (GKw) distribution where \eqn{\lambda = 1}. This function
//' is typically used for maximum likelihood estimation via numerical optimization.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
//'   if any parameter values in \code{par} are invalid according to their
//'   constraints, or if any value in \code{data} is not in the interval (0, 1).
//'
//' @details
//' The Beta-Kumaraswamy (BKw) distribution is the GKw distribution (\code{\link{dgkw}})
//' with \eqn{\lambda=1}. Its probability density function (PDF) is:
//' \deqn{
//' f(x | \theta) = \frac{\alpha \beta}{B(\gamma, \delta+1)} x^{\alpha - 1} \bigl(1 - x^\alpha\bigr)^{\beta(\delta+1) - 1} \bigl[1 - \bigl(1 - x^\alpha\bigr)^\beta\bigr]^{\gamma - 1}
//' }
//' for \eqn{0 < x < 1}, \eqn{\theta = (\alpha, \beta, \gamma, \delta)}, and \eqn{B(a,b)}
//' is the Beta function (\code{\link[base]{beta}}).
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\alpha) + \ln(\beta) - \ln B(\gamma, \delta+1)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta(\delta+1)-1)\ln(v_i) + (\gamma-1)\ln(w_i)]
//' }
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//' }
//' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
//' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
//' Numerical stability is maintained similarly to \code{\link{llgkw}}.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{llgkw}} (parent distribution negative log-likelihood),
//' \code{\link{dbkw}}, \code{\link{pbkw}}, \code{\link{qbkw}}, \code{\link{rbkw}},
//' \code{grbkw} (gradient, if available),
//' \code{hsbkw} (Hessian, if available),
//' \code{\link[stats]{optim}}, \code{\link[base]{lbeta}}
//'
//' @examples
//' \donttest{
//'
//' # Generate sample data from a known BKw distribution
//' set.seed(2203)
//' true_par_bkw <- c(alpha = 2.0, beta = 1.5, gamma = 1.5, delta = 0.5)
//' sample_data_bkw <- rbkw(1000, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
//' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 1.5, 1.5, 0.5) Sample")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess
//' start_par_bkw <- c(1.8, 1.2, 1.1, 0.3)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' mle_result_bkw <- stats::optim(par = start_par_bkw,
//'                                fn = llbkw, # Use the BKw neg-log-likelihood
//'                                method = "BFGS", # Needs parameters > 0, consider L-BFGS-B
//'                                hessian = TRUE,
//'                                data = sample_data_bkw)
//'
//' # Check convergence and results
//' if (mle_result_bkw$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par_bkw <- mle_result_bkw$par
//'   print("Estimated BKw parameters:")
//'   print(mle_par_bkw)
//'   print("True BKw parameters:")
//'   print(true_par_bkw)
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result_bkw$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires 'numDeriv' package and analytical functions 'grbkw', 'hsbkw'
//' if (mle_result_bkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grbkw") && exists("hsbkw")) {
//'
//'   cat("\nComparing Derivatives at BKw MLE estimates:\n")
//'
//'   # Numerical derivatives of llbkw
//'   num_grad_bkw <- numDeriv::grad(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
//'   num_hess_bkw <- numDeriv::hessian(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
//'
//'   # Analytical derivatives (assuming they return derivatives of negative LL)
//'   ana_grad_bkw <- grbkw(par = mle_par_bkw, data = sample_data_bkw)
//'   ana_hess_bkw <- hsbkw(par = mle_par_bkw, data = sample_data_bkw)
//'
//'   # Check differences
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad_bkw - ana_grad_bkw)))
//'   cat("Max absolute difference between Hessians:\n")
//'   print(max(abs(num_hess_bkw - ana_hess_bkw)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison for BKw.\n")
//'    cat("Requires convergence, 'numDeriv' package and functions 'grbkw', 'hsbkw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llbkw(const Rcpp::NumericVector& par,
            const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 4) {
   return R_PosInf;
 }

 double a = par[0];  // alpha > 0
 double b = par[1];  // beta > 0
 double g = par[2];  // gamma > 0
 double d = par[3];  // delta >= 0

 // Basic parameter validation
 if (a <= 0.0 || b <= 0.0 || g <= 0.0 || d < 0.0) {
   return R_PosInf;
 }

 // Convert data to armadillo vector
 arma::vec x = Rcpp::as<arma::vec>(data);
 int n = x.n_elem;

 // Basic data validation
 if (n == 0 || arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
   return R_PosInf;
 }

 // ----- Compute log-likelihood with careful numerical handling -----

 // Compute log-beta term
 double logB = R::lbeta(g, d + 1.0);

 // Constant part: n * (log(a) + log(b) - logB)
 double ll_const = n * (std::log(a) + std::log(b) - logB);

 // ----- Term 1: (alpha - 1) * sum(log(x)) -----
 arma::vec lx = arma::log(x);
 double sum1 = (a - 1.0) * arma::sum(lx);

 // ----- Term 2: (beta*(delta+1) - 1) * sum(log(1 - x^alpha)) -----
 double exp1 = b * (d + 1.0) - 1.0;
 double sum2 = 0.0;

 // ----- Term 3: (gamma - 1) * sum(log(1 - (1 - x^alpha)^beta)) -----
 double sum3 = 0.0;

 // Small constant for numerical stability
 double eps = std::sqrt(std::numeric_limits<double>::epsilon());

 // Process each observation for terms 2 and 3
 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Compute x^alpha (more accurately in log domain for extreme values)
   double xa;
   if (a * std::log(xi) < -700.0) {
     xa = 0.0;  // Underflow protection
   } else if (a * std::log(xi) > 700.0) {
     xa = 1e300;  // Overflow protection - will lead to v ≈ 0
   } else {
     xa = std::pow(xi, a);
   }

   // Compute v = 1 - x^alpha (more accurately for x^alpha close to 1)
   double v;
   if (xa > 0.5) {
     v = std::max(1.0 - xa, eps);  // Ensure v > 0
   } else {
     v = 1.0 - xa;
   }

   // Restrict v to valid range for numerical stability
   v = std::max(std::min(v, 1.0 - eps), eps);

   // Term 2 contribution: (beta*(delta+1) - 1) * log(v)
   sum2 += exp1 * std::log(v);

   // Compute v^beta (more accurately in log domain for extreme values)
   double vb;
   if (b * std::log(v) < -700.0) {
     vb = 0.0;  // Underflow protection
   } else if (b * std::log(v) > 700.0) {
     vb = 1e300;  // Overflow protection - will lead to w ≈ 0
   } else {
     vb = std::pow(v, b);
   }

   // Compute w = 1 - v^beta (more accurately for v^beta close to 1)
   double w;
   if (vb > 0.5) {
     w = std::max(1.0 - vb, eps);  // Ensure w > 0
   } else {
     w = 1.0 - vb;
   }

   // Restrict w to valid range for numerical stability
   w = std::max(std::min(w, 1.0 - eps), eps);

   // Term 3 contribution: (gamma - 1) * log(w)
   if (g != 1.0) {  // Skip if gamma = 1
     sum3 += (g - 1.0) * std::log(w);
   }
 }

 // Combine all terms
 double ll = ll_const + sum1 + sum2 + sum3;

 // Final validity check
 if (!std::isfinite(ll)) {
   return R_PosInf;
 }

 // Return negative log-likelihood
 return -ll;
}






//' @title Gradient of the Negative Log-Likelihood for the BKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the Beta-Kumaraswamy (BKw) distribution
//' with parameters \code{alpha} (\eqn{\alpha}), \code{beta} (\eqn{\beta}),
//' \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}). This distribution
//' is the special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\lambda = 1}. The gradient is typically used in optimization algorithms
//' for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 4 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter:
//'   \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \gamma, -\partial \ell/\partial \delta)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the BKw (\eqn{\lambda=1}) model are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
//' + \sum_{i=1}^{n}\left[x_i^{\alpha} \ln(x_i) \left(\frac{\beta(\delta+1)-1}{v_i} -
//' \frac{(\gamma-1) \beta v_i^{\beta-1}}{w_i}\right)\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - (\delta+1)\sum_{i=1}^{n}\ln(v_i)
//' + \sum_{i=1}^{n}\left[\frac{(\gamma-1) v_i^{\beta} \ln(v_i)}{w_i}\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma) - \psi(\gamma+\delta+1)] -
//' \sum_{i=1}^{n}\ln(w_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \delta} = n[\psi(\delta+1) - \psi(\gamma+\delta+1)] -
//' \beta\sum_{i=1}^{n}\ln(v_i)
//' }
//'
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
//' }
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the general GKw
//' gradient (\code{\link{grgkw}}) components for \eqn{\alpha, \beta, \gamma, \delta}
//' evaluated at \eqn{\lambda=1}. Note that the component for \eqn{\lambda} is omitted.
//' Numerical stability is maintained through careful implementation.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' (Note: Specific gradient formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{grgkw}} (parent distribution gradient),
//' \code{\link{llbkw}} (negative log-likelihood for BKw),
//' \code{\link{hsbkw}} (Hessian for BKw, if available),
//' \code{\link{dbkw}} (density for BKw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
//' \code{\link[base]{digamma}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rbkw, llbkw, grbkw, hsbkw functions for BKw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_bkw <- c(alpha = 2, beta = 3, gamma = 1, delta = 0.5)
//' if (exists("rbkw")) {
//'   sample_data_bkw <- rbkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
//' } else {
//'   sample_data_bkw <- rgkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4], lambda = 1)
//' }
//' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 3, 1, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_bkw <- c(1.5, 2.5, 0.8, 0.3)
//' mle_result_bkw <- stats::optim(par = start_par_bkw,
//'                                fn = llbkw,
//'                                gr = grbkw, # Use analytical gradient for BKw
//'                                method = "BFGS",
//'                                hessian = TRUE,
//'                                data = sample_data_bkw)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_bkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_bkw <- mle_result_bkw$par
//'   cat("\nComparing Gradients for BKw at MLE estimates:\n")
//'
//'   # Numerical gradient of llbkw
//'   num_grad_bkw <- numDeriv::grad(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
//'
//'   # Analytical gradient from grbkw
//'   ana_grad_bkw <- grbkw(par = mle_par_bkw, data = sample_data_bkw)
//'
//'   cat("Numerical Gradient (BKw):\n")
//'   print(num_grad_bkw)
//'   cat("Analytical Gradient (BKw):\n")
//'   print(ana_grad_bkw)
//'
//'   # Check differences
//'   cat("Max absolute difference between BKw gradients:\n")
//'   print(max(abs(num_grad_bkw - ana_grad_bkw)))
//'
//' } else {
//'   cat("\nSkipping BKw gradient comparison.\n")
//' }
//'
//' # --- Optional: Compare with relevant components of GKw gradient ---
//' # Requires grgkw function
//' if (mle_result_bkw$convergence == 0 && exists("grgkw")) {
//'   # Create 5-param vector for grgkw (insert lambda=1)
//'   mle_par_gkw_equiv <- c(mle_par_bkw[1:4], lambda = 1.0)
//'   ana_grad_gkw <- grgkw(par = mle_par_gkw_equiv, data = sample_data_bkw)
//'   # Extract components corresponding to alpha, beta, gamma, delta
//'   ana_grad_gkw_subset <- ana_grad_gkw[c(1, 2, 3, 4)]
//'
//'   cat("\nComparison with relevant components of GKw gradient:\n")
//'   cat("Max absolute difference:\n")
//'   print(max(abs(ana_grad_bkw - ana_grad_gkw_subset))) # Should be very small
//' }
//'
//' }
//'
//' @export
//' @title Gradient of the Negative Log-Likelihood for the BKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the Beta-Kumaraswamy (BKw) distribution
//' with parameters \code{alpha} (\eqn{\alpha}), \code{beta} (\eqn{\beta}),
//' \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}). This distribution
//' is the special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\lambda = 1}. The gradient is typically used in optimization algorithms
//' for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 4 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter:
//'   \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \gamma, -\partial \ell/\partial \delta)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the BKw (\eqn{\lambda=1}) model are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
//' + \sum_{i=1}^{n}\left[x_i^{\alpha} \ln(x_i) \left(\frac{\beta(\delta+1)-1}{v_i} -
//' \frac{(\gamma-1) \beta v_i^{\beta-1}}{w_i}\right)\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - (\delta+1)\sum_{i=1}^{n}\ln(v_i)
//' + \sum_{i=1}^{n}\left[\frac{(\gamma-1) v_i^{\beta} \ln(v_i)}{w_i}\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma) - \psi(\gamma+\delta+1)] -
//' \sum_{i=1}^{n}\ln(w_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \delta} = n[\psi(\delta+1) - \psi(\gamma+\delta+1)] -
//' \beta\sum_{i=1}^{n}\ln(v_i)
//' }
//'
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//'   \item \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
//' }
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the general GKw
//' gradient (\code{\link{grgkw}}) components for \eqn{\alpha, \beta, \gamma, \delta}
//' evaluated at \eqn{\lambda=1}. Note that the component for \eqn{\lambda} is omitted.
//' Numerical stability is maintained through careful implementation.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' (Note: Specific gradient formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{grgkw}} (parent distribution gradient),
//' \code{\link{llbkw}} (negative log-likelihood for BKw),
//' \code{\link{hsbkw}} (Hessian for BKw, if available),
//' \code{\link{dbkw}} (density for BKw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
//' \code{\link[base]{digamma}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rbkw, llbkw, grbkw, hsbkw functions for BKw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_bkw <- c(alpha = 2, beta = 3, gamma = 1, delta = 0.5)
//' if (exists("rbkw")) {
//'   sample_data_bkw <- rbkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
//' } else {
//'   sample_data_bkw <- rgkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4], lambda = 1)
//' }
//' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 3, 1, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_bkw <- c(1.5, 2.5, 0.8, 0.3)
//' mle_result_bkw <- stats::optim(par = start_par_bkw,
//'                                fn = llbkw,
//'                                gr = grbkw, # Use analytical gradient for BKw
//'                                method = "BFGS",
//'                                hessian = TRUE,
//'                                data = sample_data_bkw)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_bkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_bkw <- mle_result_bkw$par
//'   cat("\nComparing Gradients for BKw at MLE estimates:\n")
//'
//'   # Numerical gradient of llbkw
//'   num_grad_bkw <- numDeriv::grad(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
//'
//'   # Analytical gradient from grbkw
//'   ana_grad_bkw <- grbkw(par = mle_par_bkw, data = sample_data_bkw)
//'
//'   cat("Numerical Gradient (BKw):\n")
//'   print(num_grad_bkw)
//'   cat("Analytical Gradient (BKw):\n")
//'   print(ana_grad_bkw)
//'
//'   # Check differences
//'   cat("Max absolute difference between BKw gradients:\n")
//'   print(max(abs(num_grad_bkw - ana_grad_bkw)))
//'
//' } else {
//'   cat("\nSkipping BKw gradient comparison.\n")
//' }
//'
//' # --- Optional: Compare with relevant components of GKw gradient ---
//' # Requires grgkw function
//' if (mle_result_bkw$convergence == 0 && exists("grgkw")) {
//'   # Create 5-param vector for grgkw (insert lambda=1)
//'   mle_par_gkw_equiv <- c(mle_par_bkw[1:4], lambda = 1.0)
//'   ana_grad_gkw <- grgkw(par = mle_par_gkw_equiv, data = sample_data_bkw)
//'   # Extract components corresponding to alpha, beta, gamma, delta
//'   ana_grad_gkw_subset <- ana_grad_gkw[c(1, 2, 3, 4)]
//'
//'   cat("\nComparison with relevant components of GKw gradient:\n")
//'   cat("Max absolute difference:\n")
//'   print(max(abs(ana_grad_bkw - ana_grad_gkw_subset))) # Should be very small
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Initialize gradient vector for early return cases
 Rcpp::NumericVector grad(4, R_NaN);

 // Parameter extraction and validation
 if (par.size() < 4) {
   Rcpp::warning("Parameter vector must have at least 4 elements for BKw");
   return grad;
 }

 double alpha = par[0];   // Shape parameter α > 0
 double beta = par[1];    // Shape parameter β > 0
 double gamma = par[2];   // Shape parameter γ > 0
 double delta = par[3];   // Shape parameter δ ≥ 0

 // Enhanced parameter validation with NaN checks
 if (std::isnan(alpha) || std::isnan(beta) || std::isnan(gamma) || std::isnan(delta) ||
     alpha <= 0 || beta <= 0 || gamma <= 0 || delta < 0) {
   Rcpp::warning("Invalid parameters in grbkw: all must be positive (delta can be zero)");
   return grad;
 }

 // Data conversion and validation
 arma::vec x;
 try {
   x = Rcpp::as<arma::vec>(data);
 } catch (...) {
   Rcpp::warning("Failed to convert data to arma::vec in grbkw");
   return grad;
 }

 // Comprehensive data validation
 if (x.n_elem == 0 || x.has_nan() || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::warning("Data must be strictly in (0,1) and non-empty for grbkw");
   return grad;
 }

 int n = x.n_elem;  // Sample size

 // Reset gradient to zeros (we'll compute actual values now)
 grad = Rcpp::NumericVector(4, 0.0);

 // Small constant for numerical stability - adaptive to machine precision
 double eps = std::sqrt(std::numeric_limits<double>::epsilon());

 // -------- Step 1: Compute base transformations safely --------

 // Compute log(x) safely
 arma::vec log_x = vec_safe_log(x);

 // Compute x^α safely
 arma::vec x_alpha = vec_safe_pow(x, alpha);

 // Compute x^α * log(x) with check for potential overflow
 arma::vec x_alpha_log_x = x_alpha % log_x;

 // ----- Step 2: Compute v_i = 1 - x_i^α and related terms -----

 // Use log1p and expm1 for better precision near boundaries
 arma::vec v(n);
 for (int i = 0; i < n; i++) {
   // v_i = 1 - x_i^α computed via v_i = -expm1(log(x_i^α)) for better precision
   if (x_alpha(i) < 0.5) {
     // Standard calculation is fine for smaller values
     v(i) = 1.0 - x_alpha(i);
   } else {
     // For x_i^α close to 1, use more accurate formula
     v(i) = -std::expm1(alpha * log_x(i));
   }

   // Ensure v is in valid range
   if (v(i) <= 0.0 || v(i) >= 1.0) {
     // Apply very cautious clamping only at extremes
     v(i) = std::max(std::min(v(i), 1.0 - eps), eps);
   }
 }

 // Compute log(v) and v^β terms with proper safeguards
 arma::vec log_v = vec_safe_log(v);

 // Compute v^(β-1) with safety for β close to 1
 arma::vec v_beta_m1;
 if (std::abs(beta - 1.0) < eps) {
   // For β ≈ 1, v^(β-1) ≈ 1
   v_beta_m1.ones(n);
 } else {
   v_beta_m1 = vec_safe_pow(v, beta - 1.0);
 }

 // Compute v^β safely
 arma::vec v_beta = v % v_beta_m1;  // More accurate than direct power

 // Compute v^β * log(v) with check for potential issues
 arma::vec v_beta_log_v = v_beta % log_v;

 // ----- Step 3: Compute w_i = 1 - v_i^β safely -----

 arma::vec w(n);
 arma::vec log_w(n);

 for (int i = 0; i < n; i++) {
   // Compute w_i = 1 - v_i^β more accurately for v_i^β close to 1
   if (v_beta(i) < 0.5) {
     w(i) = 1.0 - v_beta(i);
   } else {
     // For v_i^β close to 1, use log-domain calculation
     w(i) = -std::expm1(beta * log_v(i));
   }

   // Validate and apply safety bounds
   if (w(i) <= 0.0 || w(i) >= 1.0) {
     w(i) = std::max(std::min(w(i), 1.0 - eps), eps);
   }

   // Compute log(w) directly for better accuracy
   log_w(i) = std::log(w(i));
 }

 // ----- Step 4: Calculate partial derivatives with careful factoring -----

 // Compute digamma values once, with checks for large arguments
 double digamma_gamma, digamma_delta_plus_1, digamma_sum;

 if (gamma > 1e6 && delta > 1e6) {
   // For very large parameters, use asymptotic approximation of digamma
   digamma_gamma = std::log(gamma) - 1.0/(2.0*gamma);
   digamma_delta_plus_1 = std::log(delta+1.0) - 1.0/(2.0*(delta+1.0));
   digamma_sum = std::log(gamma+delta+1.0) - 1.0/(2.0*(gamma+delta+1.0));
 } else {
   // Use standard digamma for typical values
   digamma_gamma = R::digamma(gamma);
   digamma_delta_plus_1 = R::digamma(delta + 1.0);
   digamma_sum = R::digamma(gamma + delta + 1.0);
 }

 // ----- Calculate gradient components -----

 // d_alpha = n/α + Σᵢlog(xᵢ) - Σᵢ[xᵢ^α * log(xᵢ) * ((β(δ+1)-1)/vᵢ - (γ-1) * β * vᵢ^(β-1) / wᵢ)]
 double term_beta_delta = beta * (delta + 1.0) - 1.0;
 double term_gamma = gamma - 1.0;

 double d_alpha = n / alpha + arma::sum(log_x);

 for (int i = 0; i < n; i++) {
   double alpha_term = x_alpha_log_x(i) * (
     term_beta_delta / v(i) -
       term_gamma * beta * v_beta_m1(i) / w(i)
   );

   // Check for invalid values before adding
   if (std::isfinite(alpha_term)) {
     d_alpha -= alpha_term;
   }
 }

 // d_beta = n/β + (δ+1)Σᵢlog(vᵢ) - Σᵢ[(γ-1) * vᵢ^β * log(vᵢ) / wᵢ]
 double d_beta = n / beta + (delta + 1.0) * arma::sum(log_v);

 if (term_gamma != 0.0) {  // Skip calculation if γ=1 (term_gamma=0)
   for (int i = 0; i < n; i++) {
     double beta_term = term_gamma * v_beta_log_v(i) / w(i);

     // Check for invalid values before adding
     if (std::isfinite(beta_term)) {
       d_beta -= beta_term;
     }
   }
 }

 // d_gamma = -n[ψ(γ) - ψ(γ+δ+1)] + Σᵢlog(wᵢ)
 double d_gamma = -n * (digamma_gamma - digamma_sum) + arma::sum(log_w);

 // d_delta = -n[ψ(δ+1) - ψ(γ+δ+1)] + βΣᵢlog(vᵢ)
 double d_delta = -n * (digamma_delta_plus_1 - digamma_sum) + beta * arma::sum(log_v);

 // Final check for NaN/Inf values
 if (!std::isfinite(d_alpha) || !std::isfinite(d_beta) ||
     !std::isfinite(d_gamma) || !std::isfinite(d_delta)) {
     Rcpp::warning("Gradient calculation produced non-finite values in grbkw");
   return Rcpp::NumericVector(4, R_NaN);
 }

 // Since we're optimizing negative log-likelihood, negate all derivatives
 grad[0] = -d_alpha;
 grad[1] = -d_beta;
 grad[2] = -d_gamma;
 grad[3] = -d_delta;

 return grad;
}





//' @title Hessian Matrix of the Negative Log-Likelihood for the BKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian
//'
//' @description
//' Computes the analytic 4x4 Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the Beta-Kumaraswamy (BKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}).
//' This distribution is the special case of the Generalized Kumaraswamy (GKw)
//' distribution where \eqn{\lambda = 1}. The Hessian is useful for estimating
//' standard errors and in optimization algorithms.
//'
//' @param par A numeric vector of length 4 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 4x4 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
//'   where \eqn{\theta = (\alpha, \beta, \gamma, \delta)}.
//'   Returns a 4x4 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function based on the BKw log-likelihood
//' (\eqn{\lambda=1} case of GKw, see \code{\link{llbkw}}):
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\alpha) + \ln(\beta) - \ln B(\gamma, \delta+1)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta(\delta+1)-1)\ln(v_i) + (\gamma-1)\ln(w_i)]
//' }
//' where \eqn{\theta = (\alpha, \beta, \gamma, \delta)}, \eqn{B(a,b)}
//' is the Beta function (\code{\link[base]{beta}}), and intermediate terms are:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//' }
//' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}
//' for \eqn{\theta_i, \theta_j \in \{\alpha, \beta, \gamma, \delta\}}.
//'
//' Key properties of the returned matrix:
//' \itemize{
//'   \item Dimensions: 4x4.
//'   \item Symmetry: The matrix is symmetric.
//'   \item Ordering: Rows and columns correspond to the parameters in the order
//'     \eqn{\alpha, \beta, \gamma, \delta}.
//'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
//' }
//' This corresponds to the relevant 4x4 submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
//' evaluated at \eqn{\lambda=1}. The exact analytical formulas are implemented directly.
//'
//' @references
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' (Note: Specific Hessian formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{hsgkw}} (parent distribution Hessian),
//' \code{\link{llbkw}} (negative log-likelihood for BKw),
//' \code{\link{grbkw}} (gradient for BKw, if available),
//' \code{\link{dbkw}} (density for BKw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rbkw, llbkw, grbkw, hsbkw functions for BKw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_bkw <- c(alpha = 2, beta = 3, gamma = 1, delta = 0.5)
//' if (exists("rbkw")) {
//'   sample_data_bkw <- rbkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
//' } else {
//'   sample_data_bkw <- rgkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
//'                          gamma = true_par_bkw[3], delta = true_par_bkw[4], lambda = 1)
//' }
//' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 3, 1, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_bkw <- c(1.5, 2.5, 0.8, 0.3)
//' mle_result_bkw <- stats::optim(par = start_par_bkw,
//'                                fn = llbkw,
//'                                gr = if (exists("grbkw")) grbkw else NULL,
//'                                method = "BFGS",
//'                                hessian = TRUE, # Ask optim for numerical Hessian
//'                                data = sample_data_bkw)
//'
//' # --- Compare analytical Hessian to numerical Hessian ---
//' if (mle_result_bkw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("hsbkw")) {
//'
//'   mle_par_bkw <- mle_result_bkw$par
//'   cat("\nComparing Hessians for BKw at MLE estimates:\n")
//'
//'   # Numerical Hessian of llbkw
//'   num_hess_bkw <- numDeriv::hessian(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
//'
//'   # Analytical Hessian from hsbkw
//'   ana_hess_bkw <- hsbkw(par = mle_par_bkw, data = sample_data_bkw)
//'
//'   cat("Numerical Hessian (BKw):\n")
//'   print(round(num_hess_bkw, 4))
//'   cat("Analytical Hessian (BKw):\n")
//'   print(round(ana_hess_bkw, 4))
//'
//'   # Check differences
//'   cat("Max absolute difference between BKw Hessians:\n")
//'   print(max(abs(num_hess_bkw - ana_hess_bkw)))
//'
//'   # Optional: Use analytical Hessian for Standard Errors
//'   # tryCatch({
//'   #   cov_matrix_bkw <- solve(ana_hess_bkw)
//'   #   std_errors_bkw <- sqrt(diag(cov_matrix_bkw))
//'   #   cat("Std. Errors from Analytical BKw Hessian:\n")
//'   #   print(std_errors_bkw)
//'   # }, error = function(e) {
//'   #   warning("Could not invert analytical BKw Hessian: ", e$message)
//'   # })
//'
//' } else {
//'   cat("\nSkipping BKw Hessian comparison.\n")
//'   cat("Requires convergence, 'numDeriv' package, and function 'hsbkw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hsbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Initialize return matrix for error cases
 Rcpp::NumericMatrix nanH(4, 4);
 nanH.fill(R_NaN);

 // Parameter validation
 if (par.size() < 4) {
   Rcpp::warning("Parameter vector must have at least 4 elements for BKw");
   return nanH;
 }

 double alpha = par[0];   // Shape parameter α > 0
 double beta = par[1];    // Shape parameter β > 0
 double gamma = par[2];   // Shape parameter γ > 0
 double delta = par[3];   // Shape parameter δ ≥ 0

 // Enhanced parameter validation with NaN checks
 if (std::isnan(alpha) || std::isnan(beta) || std::isnan(gamma) || std::isnan(delta) ||
     alpha <= 0 || beta <= 0 || gamma <= 0 || delta < 0) {
   Rcpp::warning("Invalid parameters in hsbkw: alpha, beta, gamma must be positive; delta must be non-negative");
   return nanH;
 }

 // Data conversion and validation
 arma::vec x;
 try {
   x = Rcpp::as<arma::vec>(data);
 } catch (...) {
   Rcpp::warning("Failed to convert data to arma::vec in hsbkw");
   return nanH;
 }

 // Comprehensive data validation
 if (x.n_elem == 0 || x.has_nan() || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::warning("Data must be strictly in (0,1) and non-empty for hsbkw");
   return nanH;
 }

 int n = x.n_elem;  // Sample size

 // Initialize Hessian matrix H (of the log-likelihood) as 4x4
 arma::mat H(4, 4, arma::fill::zeros);

 // Small constant for numerical stability
 double eps = std::sqrt(std::numeric_limits<double>::epsilon());

 // ---------------------------------------------------------------------
 // STEP 1: Add constant terms (that don't depend on individual data points)
 // ---------------------------------------------------------------------

 // Second derivative of n*ln(α) with respect to α: -n/α²
 H(0, 0) = -n / (alpha * alpha);

 // Second derivative of n*ln(β) with respect to β: -n/β²
 H(1, 1) = -n / (beta * beta);

 // Compute trigamma values with protection for large arguments
 double trigamma_gamma, trigamma_delta_plus_1, trigamma_sum;

 if (gamma > 1e6 || delta > 1e6) {
   // For very large parameters, use asymptotic approximation of trigamma
   // ψ₁(x) ≈ 1/x + 1/(2x²) + ...
   trigamma_gamma = 1.0/gamma + 1.0/(2.0*gamma*gamma);
   trigamma_delta_plus_1 = 1.0/(delta+1.0) + 1.0/(2.0*(delta+1.0)*(delta+1.0));
   trigamma_sum = 1.0/(gamma+delta+1.0) + 1.0/(2.0*(gamma+delta+1.0)*(gamma+delta+1.0));
 } else {
   // Use standard trigamma for typical values
   trigamma_gamma = R::trigamma(gamma);
   trigamma_delta_plus_1 = R::trigamma(delta + 1.0);
   trigamma_sum = R::trigamma(gamma + delta + 1.0);
 }

 // Second derivative of -n*ln[B(γ,δ+1)] with respect to γ: -n*[ψ₁(γ) - ψ₁(γ+δ+1)]
 H(2, 2) = -n * (trigamma_gamma - trigamma_sum);

 // Second derivative of -n*ln[B(γ,δ+1)] with respect to δ: -n*[ψ₁(δ+1) - ψ₁(γ+δ+1)]
 H(3, 3) = -n * (trigamma_delta_plus_1 - trigamma_sum);

 // Mixed derivative (γ,δ): n*ψ₁(γ+δ+1)
 H(2, 3) = n * trigamma_sum;
 H(3, 2) = H(2, 3);  // Symmetric matrix

 // ---------------------------------------------------------------------
 // STEP 2: Loop through observations to accumulate data-dependent terms
 // ---------------------------------------------------------------------

 // Precompute common factor
 double beta_delta_factor = beta * (delta + 1.0) - 1.0;
 double gamma_minus_1 = gamma - 1.0;

 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Compute log(x) safely
   double ln_xi = safe_log(xi);

   // ---- Compute x^α and its derivatives more safely ----
   double A; // A = x^α
   double dA_dalpha; // dA/dα = x^α * ln(x)
   double d2A_dalpha2; // d²A/dα² = x^α * (ln(x))²

   // Use logarithmic domain for more stability
   double log_A = alpha * ln_xi;

   if (std::abs(log_A) > 700.0) {
     // For extreme values, handle specially
     if (log_A < -700.0) {
       A = 0.0;
       dA_dalpha = 0.0;
       d2A_dalpha2 = 0.0;
     } else {
       // Very large - handle with care
       A = safe_exp(log_A);
       dA_dalpha = A * ln_xi;
       d2A_dalpha2 = dA_dalpha * ln_xi;
     }
   } else {
     // Normal range - standard calculation
     A = std::exp(log_A);
     dA_dalpha = A * ln_xi;
     d2A_dalpha2 = dA_dalpha * ln_xi;
   }

   // ---- Compute v = 1-x^α and its derivatives safely ----
   double v; // v = 1 - x^α
   double ln_v; // ln(v)
   double dv_dalpha; // dv/dα = -x^α * ln(x)
   double d2v_dalpha2; // d²v/dα² = -x^α * (ln(x))²

   if (A > 0.5) {
     // For A close to 1, use more accurate computation
     v = -std::expm1(log_A);  // v = 1-e^(α*ln(x)) more accurately
     dv_dalpha = -dA_dalpha;
     d2v_dalpha2 = -d2A_dalpha2;
   } else {
     // Standard computation is fine for smaller A
     v = 1.0 - A;
     dv_dalpha = -dA_dalpha;
     d2v_dalpha2 = -d2A_dalpha2;
   }

   // Safety check - ensure v is strictly in (0,1)
   if (v <= eps || v >= 1.0 - eps) {
     v = std::max(std::min(v, 1.0 - eps), eps);
   }

   // Compute ln(v) safely
   ln_v = safe_log(v);

   // ---- Compute L5 derivatives: (β(δ+1)-1)*ln(v) ----
   // Second derivative w.r.t. α: (β(δ+1)-1) * [(d²v/dα² * v - (dv/dα)²) / v²]
   double d2L5_dalpha2 = 0.0;
   if (std::abs(beta_delta_factor) > eps) {
     double term = (d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v * v);
     if (std::isfinite(term)) {
       d2L5_dalpha2 = beta_delta_factor * term;
     }
   }

   // Mixed derivative: d²L5/(dα dβ) = (δ+1) * (dv_dalpha/v)
   double d2L5_dalpha_dbeta = (delta + 1.0) * (dv_dalpha / v);

   // Mixed derivative: d²L5/(dα dδ) = β * (dv_dalpha/v)
   double d2L5_dalpha_ddelta = beta * (dv_dalpha / v);

   // Mixed derivative: d²L5/(dβ dδ) = ln(v)
   double d2L5_dbeta_ddelta = ln_v;

   // ---- Compute w = 1-v^β and its derivatives safely ----
   double v_beta; // v^β
   double w; // w = 1 - v^β

   // Compute v^β safely using log domain when helpful
   if (beta > 100 || v < 0.01) {
     double log_v_beta = beta * ln_v;
     v_beta = safe_exp(log_v_beta);
   } else {
     v_beta = safe_pow(v, beta);
   }

   // Compute w = 1-v^β carefully
   if (v_beta > 0.5) {
     // For v_beta close to 1, use more accurate computation
     double log_v_beta = beta * ln_v;
     w = -std::expm1(log_v_beta);  // w = 1-e^(β*ln(v)) more accurately
   } else {
     // Standard computation is fine for smaller v_beta
     w = 1.0 - v_beta;
   }

   // Safety check - ensure w is strictly in (0,1)
   if (w <= eps || w >= 1.0 - eps) {
     w = std::max(std::min(w, 1.0 - eps), eps);
   }

   // Compute ln(w) safely
   // double ln_w; // ln(w)
   // double ln_w = safe_log(w);

   // ---- Derivatives for w ----
   // dw/dv = -β * v^(β-1)
   double v_beta_m1 = (beta > 1.0) ? v_beta / v : 1.0;  // v^(β-1)
   if (beta == 1.0) v_beta_m1 = 1.0;  // Special case

   double dw_dv = -beta * v_beta_m1;

   // Chain rule: dw/dα = dw/dv * dv/dα
   double dw_dalpha = dw_dv * dv_dalpha;

   // ---- Compute L6 derivatives: (γ-1)*ln(w) ----
   // Only compute if γ != 1 to avoid unnecessary work
   double d2L6_dalpha2 = 0.0;
   double d2L6_dbeta2 = 0.0;
   double d2L6_dalpha_dbeta = 0.0;
   double d2L6_dalpha_dgamma = 0.0;
   double d2L6_dbeta_dgamma = 0.0;

   if (std::abs(gamma_minus_1) > eps) {
     // Second derivative of w w.r.t. α
     double d2w_dalpha2 = -beta * ((beta - 1.0) * safe_pow(v, beta - 2.0) *
                                   (dv_dalpha * dv_dalpha) +
                                   v_beta_m1 * d2v_dalpha2);

     // Second derivative of ln(w) w.r.t. α
     double term_alpha2 = (d2w_dalpha2 * w - dw_dalpha * dw_dalpha) / (w * w);
     if (std::isfinite(term_alpha2)) {
       d2L6_dalpha2 = gamma_minus_1 * term_alpha2;
     }

     // Derivative w.r.t. β: d/dβ ln(w)
     double dw_dbeta = -v_beta * ln_v;

     // Second derivative of ln(w) w.r.t. β
     double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
     double term_beta2 = (d2w_dbeta2 * w - dw_dbeta * dw_dbeta) / (w * w);
     if (std::isfinite(term_beta2)) {
       d2L6_dbeta2 = gamma_minus_1 * term_beta2;
     }

     // Mixed derivative (α,β)
     double d_dw_dalpha_dbeta = -v_beta_m1 * (1.0 + beta * ln_v) * dv_dalpha;
     double mixed_term = (d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta) / (w * w);
     if (std::isfinite(mixed_term)) {
       d2L6_dalpha_dbeta = gamma_minus_1 * mixed_term;
     }

     // Mixed derivatives with γ
     d2L6_dalpha_dgamma = dw_dalpha / w;
     d2L6_dbeta_dgamma = dw_dbeta / w;
   }

   // ---- Accumulate contributions to the Hessian matrix ----
   // Check each contribution for finiteness before adding

   // H(α,α): contributions from L5 and L6
   if (std::isfinite(d2L5_dalpha2)) H(0, 0) += d2L5_dalpha2;
   if (std::isfinite(d2L6_dalpha2)) H(0, 0) += d2L6_dalpha2;

   // H(β,β): contributions from L6
   if (std::isfinite(d2L6_dbeta2)) H(1, 1) += d2L6_dbeta2;

   // H(α,β): mixed from L5 and L6
   if (std::isfinite(d2L5_dalpha_dbeta)) H(0, 1) += d2L5_dalpha_dbeta;
   if (std::isfinite(d2L6_dalpha_dbeta)) H(0, 1) += d2L6_dalpha_dbeta;
   H(1, 0) = H(0, 1);  // Symmetric

   // H(α,γ): mixed from L6
   if (std::isfinite(d2L6_dalpha_dgamma)) H(0, 2) += d2L6_dalpha_dgamma;
   H(2, 0) = H(0, 2);  // Symmetric

   // H(α,δ): mixed from L5
   if (std::isfinite(d2L5_dalpha_ddelta)) H(0, 3) += d2L5_dalpha_ddelta;
   H(3, 0) = H(0, 3);  // Symmetric

   // H(β,γ): mixed from L6
   if (std::isfinite(d2L6_dbeta_dgamma)) H(1, 2) += d2L6_dbeta_dgamma;
   H(2, 1) = H(1, 2);  // Symmetric

   // H(β,δ): mixed from L5
   if (std::isfinite(d2L5_dbeta_ddelta)) H(1, 3) += d2L5_dbeta_ddelta;
   H(3, 1) = H(1, 3);  // Symmetric
 }

 // Final safety check - verify the Hessian is valid
 if (!H.is_finite()) {
   Rcpp::warning("Hessian calculation produced non-finite values");
   return nanH;
 }

 // Return the analytic Hessian matrix of the negative log-likelihood
 return Rcpp::wrap(-H);
}



// // -----------------------------------------------------------------------------
// // 5) llbkw: Negative Log-Likelihood for Beta-Kumaraswamy
// // -----------------------------------------------------------------------------
//
// //' @title Negative Log-Likelihood for Beta-Kumaraswamy (BKw) Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize
// //'
// //' @description
// //' Computes the negative log-likelihood function for the Beta-Kumaraswamy (BKw)
// //' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}),
// //' given a vector of observations. This distribution is the special case of the
// //' Generalized Kumaraswamy (GKw) distribution where \eqn{\lambda = 1}. This function
// //' is typically used for maximum likelihood estimation via numerical optimization.
// //'
// //' @param par A numeric vector of length 4 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a single \code{double} value representing the negative
// //'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
// //'   if any parameter values in \code{par} are invalid according to their
// //'   constraints, or if any value in \code{data} is not in the interval (0, 1).
// //'
// //' @details
// //' The Beta-Kumaraswamy (BKw) distribution is the GKw distribution (\code{\link{dgkw}})
// //' with \eqn{\lambda=1}. Its probability density function (PDF) is:
// //' \deqn{
// //' f(x | \theta) = \frac{\alpha \beta}{B(\gamma, \delta+1)} x^{\alpha - 1} \bigl(1 - x^\alpha\bigr)^{\beta(\delta+1) - 1} \bigl[1 - \bigl(1 - x^\alpha\bigr)^\beta\bigr]^{\gamma - 1}
// //' }
// //' for \eqn{0 < x < 1}, \eqn{\theta = (\alpha, \beta, \gamma, \delta)}, and \eqn{B(a,b)}
// //' is the Beta function (\code{\link[base]{beta}}).
// //' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
// //' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\alpha) + \ln(\beta) - \ln B(\gamma, \delta+1)]
// //' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta(\delta+1)-1)\ln(v_i) + (\gamma-1)\ln(w_i)]
// //' }
// //' where:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //' }
// //' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
// //' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
// //' Numerical stability is maintained similarly to \code{\link{llgkw}}.
// //'
// //' @references
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //'
// //' @seealso
// //' \code{\link{llgkw}} (parent distribution negative log-likelihood),
// //' \code{\link{dbkw}}, \code{\link{pbkw}}, \code{\link{qbkw}}, \code{\link{rbkw}},
// //' \code{grbkw} (gradient, if available),
// //' \code{hsbkw} (Hessian, if available),
// //' \code{\link[stats]{optim}}, \code{\link[base]{lbeta}}
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rbkw, grbkw, hsbkw functions for BKw distribution
// //'
// //' # Generate sample data from a known BKw distribution
// //' set.seed(124)
// //' true_par_bkw <- c(alpha = 2.0, beta = 1.5, gamma = 1.0, delta = 0.5)
// //' # Use rbkw if it exists, otherwise use rgkw with lambda=1
// //' if (exists("rbkw")) {
// //'   sample_data_bkw <- rbkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
// //'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
// //' } else {
// //'   sample_data_bkw <- rgkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
// //'                          gamma = true_par_bkw[3], delta = true_par_bkw[4], lambda = 1)
// //' }
// //' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 1.5, 1.0, 0.5) Sample")
// //'
// //' # --- Maximum Likelihood Estimation using optim ---
// //' # Initial parameter guess
// //' start_par_bkw <- c(1.8, 1.2, 1.1, 0.3)
// //'
// //' # Perform optimization (minimizing negative log-likelihood)
// //' mle_result_bkw <- stats::optim(par = start_par_bkw,
// //'                                fn = llbkw, # Use the BKw neg-log-likelihood
// //'                                method = "BFGS", # Needs parameters > 0, consider L-BFGS-B
// //'                                hessian = TRUE,
// //'                                data = sample_data_bkw)
// //'
// //' # Check convergence and results
// //' if (mle_result_bkw$convergence == 0) {
// //'   print("Optimization converged successfully.")
// //'   mle_par_bkw <- mle_result_bkw$par
// //'   print("Estimated BKw parameters:")
// //'   print(mle_par_bkw)
// //'   print("True BKw parameters:")
// //'   print(true_par_bkw)
// //' } else {
// //'   warning("Optimization did not converge!")
// //'   print(mle_result_bkw$message)
// //' }
// //'
// //' # --- Compare numerical and analytical derivatives (if available) ---
// //' # Requires 'numDeriv' package and analytical functions 'grbkw', 'hsbkw'
// //' if (mle_result_bkw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("grbkw") && exists("hsbkw")) {
// //'
// //'   cat("\nComparing Derivatives at BKw MLE estimates:\n")
// //'
// //'   # Numerical derivatives of llbkw
// //'   num_grad_bkw <- numDeriv::grad(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
// //'   num_hess_bkw <- numDeriv::hessian(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
// //'
// //'   # Analytical derivatives (assuming they return derivatives of negative LL)
// //'   ana_grad_bkw <- grbkw(par = mle_par_bkw, data = sample_data_bkw)
// //'   ana_hess_bkw <- hsbkw(par = mle_par_bkw, data = sample_data_bkw)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between gradients:\n")
// //'   print(max(abs(num_grad_bkw - ana_grad_bkw)))
// //'   cat("Max absolute difference between Hessians:\n")
// //'   print(max(abs(num_hess_bkw - ana_hess_bkw)))
// //'
// //' } else {
// //'    cat("\nSkipping derivative comparison for BKw.\n")
// //'    cat("Requires convergence, 'numDeriv' package and functions 'grbkw', 'hsbkw'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// double llbkw(const Rcpp::NumericVector& par,
//             const Rcpp::NumericVector& data) {
//  if (par.size() < 4) {
//    return R_NegInf;
//  }
//  double a = par[0];  // alpha>0
//  double b = par[1];  // beta>0
//  double g = par[2];  // gamma>0
//  double d = par[3];  // delta>=0
//
//  if (!check_bkw_pars(a, b, g, d)) {
//    return R_NegInf;
//  }
//
//  // Convert data
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  // Check data
//  if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
//    return R_NegInf;
//  }
//
//  int n = x.n_elem;
//  double logB = R::lbeta(g, d + 1.0);
//
//  // sum( log(f(x)) ) = n*log(alpha*beta) - n*logB + ...
//  double ll_const = n * (std::log(a) + std::log(b) - logB);
//
//  // (alpha - 1)*sum(log(x))
//  arma::vec lx = arma::log(x);
//  double sum1 = (a - 1.0) * arma::sum(lx);
//
//  // exponent1 = beta*(delta+1) - 1
//  double exp1 = b*(d+1.0) - 1.0;
//  // sum( exp1 * log(1 - x^alpha) )
//  arma::vec xalpha = arma::pow(x, a);
//  arma::vec log_1_xalpha = arma::log(1.0 - xalpha);
//  double sum2 = exp1 * arma::sum(log_1_xalpha);
//
//  // exponent2 = (gamma - 1)
//  // sum( exponent2* log( 1 - (1 - x^alpha)^beta ) )
//  // (1 - x^alpha)^beta => vbeta
//  arma::vec vbeta = arma::pow(1.0 - xalpha, b);
//  arma::vec one_minus_vbeta = 1.0 - vbeta;
//  // safe log(1.0 - vbeta)
//  arma::vec log_omv(vbeta.n_elem);
//  for (int i = 0; i < (int)vbeta.n_elem; i++) {
//    double val = one_minus_vbeta[i];
//    if (val <= 0.0) {
//      return R_NegInf;  // invalid => -Inf
//    }
//    log_omv[i] = std::log(val);
//  }
//  double sum3 = (g - 1.0) * arma::sum(log_omv);
//
//  double ll = ll_const + sum1 + sum2 + sum3;
//
//  // negative log-likelihood
//  return -ll;
// }
//
//
// //' @title Gradient of the Negative Log-Likelihood for the BKw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize gradient
// //'
// //' @description
// //' Computes the gradient vector (vector of first partial derivatives) of the
// //' negative log-likelihood function for the Beta-Kumaraswamy (BKw) distribution
// //' with parameters \code{alpha} (\eqn{\alpha}), \code{beta} (\eqn{\beta}),
// //' \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}). This distribution
// //' is the special case of the Generalized Kumaraswamy (GKw) distribution where
// //' \eqn{\lambda = 1}. The gradient is typically used in optimization algorithms
// //' for maximum likelihood estimation.
// //'
// //' @param par A numeric vector of length 4 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a numeric vector of length 4 containing the partial derivatives
// //'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
// //'   respect to each parameter:
// //'   \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \gamma, -\partial \ell/\partial \delta)}.
// //'   Returns a vector of \code{NaN} if any parameter values are invalid according
// //'   to their constraints, or if any value in \code{data} is not in the
// //'   interval (0, 1).
// //'
// //' @details
// //' The components of the gradient vector of the negative log-likelihood
// //' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the BKw (\eqn{\lambda=1}) model are:
// //'
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
// //' + \sum_{i=1}^{n}\left[x_i^{\alpha} \ln(x_i) \left(\frac{\beta(\delta+1)-1}{v_i} -
// //' \frac{(\gamma-1) \beta v_i^{\beta-1}}{w_i}\right)\right]
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - (\delta+1)\sum_{i=1}^{n}\ln(v_i)
// //' + \sum_{i=1}^{n}\left[\frac{(\gamma-1) v_i^{\beta} \ln(v_i)}{w_i}\right]
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma) - \psi(\gamma+\delta+1)] -
// //' \sum_{i=1}^{n}\ln(w_i)
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \delta} = n[\psi(\delta+1) - \psi(\gamma+\delta+1)] -
// //' \beta\sum_{i=1}^{n}\ln(v_i)
// //' }
// //'
// //' where:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //'   \item \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
// //' }
// //' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
// //' minimizing the negative log-likelihood. They correspond to the general GKw
// //' gradient (\code{\link{grgkw}}) components for \eqn{\alpha, \beta, \gamma, \delta}
// //' evaluated at \eqn{\lambda=1}. Note that the component for \eqn{\lambda} is omitted.
// //' Numerical stability is maintained through careful implementation.
// //'
// //' @references
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //' (Note: Specific gradient formulas might be derived or sourced from additional references).
// //'
// //' @seealso
// //' \code{\link{grgkw}} (parent distribution gradient),
// //' \code{\link{llbkw}} (negative log-likelihood for BKw),
// //' \code{\link{hsbkw}} (Hessian for BKw, if available),
// //' \code{\link{dbkw}} (density for BKw),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
// //' \code{\link[base]{digamma}}.
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rbkw, llbkw, grbkw, hsbkw functions for BKw
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_bkw <- c(alpha = 2, beta = 3, gamma = 1, delta = 0.5)
// //' if (exists("rbkw")) {
// //'   sample_data_bkw <- rbkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
// //'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
// //' } else {
// //'   sample_data_bkw <- rgkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
// //'                          gamma = true_par_bkw[3], delta = true_par_bkw[4], lambda = 1)
// //' }
// //' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 3, 1, 0.5) Sample")
// //'
// //' # --- Find MLE estimates ---
// //' start_par_bkw <- c(1.5, 2.5, 0.8, 0.3)
// //' mle_result_bkw <- stats::optim(par = start_par_bkw,
// //'                                fn = llbkw,
// //'                                gr = grbkw, # Use analytical gradient for BKw
// //'                                method = "BFGS",
// //'                                hessian = TRUE,
// //'                                data = sample_data_bkw)
// //'
// //' # --- Compare analytical gradient to numerical gradient ---
// //' if (mle_result_bkw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE)) {
// //'
// //'   mle_par_bkw <- mle_result_bkw$par
// //'   cat("\nComparing Gradients for BKw at MLE estimates:\n")
// //'
// //'   # Numerical gradient of llbkw
// //'   num_grad_bkw <- numDeriv::grad(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
// //'
// //'   # Analytical gradient from grbkw
// //'   ana_grad_bkw <- grbkw(par = mle_par_bkw, data = sample_data_bkw)
// //'
// //'   cat("Numerical Gradient (BKw):\n")
// //'   print(num_grad_bkw)
// //'   cat("Analytical Gradient (BKw):\n")
// //'   print(ana_grad_bkw)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between BKw gradients:\n")
// //'   print(max(abs(num_grad_bkw - ana_grad_bkw)))
// //'
// //' } else {
// //'   cat("\nSkipping BKw gradient comparison.\n")
// //' }
// //'
// //' # --- Optional: Compare with relevant components of GKw gradient ---
// //' # Requires grgkw function
// //' if (mle_result_bkw$convergence == 0 && exists("grgkw")) {
// //'   # Create 5-param vector for grgkw (insert lambda=1)
// //'   mle_par_gkw_equiv <- c(mle_par_bkw[1:4], lambda = 1.0)
// //'   ana_grad_gkw <- grgkw(par = mle_par_gkw_equiv, data = sample_data_bkw)
// //'   # Extract components corresponding to alpha, beta, gamma, delta
// //'   ana_grad_gkw_subset <- ana_grad_gkw[c(1, 2, 3, 4)]
// //'
// //'   cat("\nComparison with relevant components of GKw gradient:\n")
// //'   cat("Max absolute difference:\n")
// //'   print(max(abs(ana_grad_bkw - ana_grad_gkw_subset))) # Should be very small
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericVector grbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha = par[0];   // Shape parameter α > 0
//  double beta = par[1];    // Shape parameter β > 0
//  double gamma = par[2];   // Shape parameter γ > 0
//  double delta = par[3];   // Shape parameter δ > 0
//
//  // Parameter validation
//  if (alpha <= 0 || beta <= 0 || gamma <= 0 || delta <= 0) {
//    Rcpp::NumericVector grad(4, R_NaN);
//    return grad;
//  }
//
//  // Data conversion and validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//
//  if (arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericVector grad(4, R_NaN);
//    return grad;
//  }
//
//  int n = x.n_elem;  // Sample size
//
//  // Initialize gradient vector
//  Rcpp::NumericVector grad(4, 0.0);
//
//  // Small constant to avoid numerical issues
//  double eps = std::numeric_limits<double>::epsilon() * 100;
//
//  // Compute transformations and intermediate values
//  arma::vec log_x = arma::log(x);                // log(x_i)
//  arma::vec x_alpha = arma::pow(x, alpha);       // x_i^α
//  arma::vec x_alpha_log_x = x_alpha % log_x;     // x_i^α * log(x_i)
//
//  // v_i = 1 - x_i^α
//  arma::vec v = 1.0 - x_alpha;
//  v = arma::clamp(v, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_v = arma::log(v);                // log(v_i)
//  arma::vec v_beta_m1 = arma::pow(v, beta - 1.0); // v_i^(β-1)
//  arma::vec v_beta = arma::pow(v, beta);          // v_i^β
//  arma::vec v_beta_log_v = v_beta % log_v;        // v_i^β * log(v_i)
//
//  // w_i = 1 - v_i^β = 1 - (1-x_i^α)^β
//  arma::vec w = 1.0 - v_beta;
//  w = arma::clamp(w, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_w = arma::log(w);                // log(w_i)
//
//  // Calculate partial derivatives for each parameter (for log-likelihood)
//
//  // ∂ℓ/∂α = n/α + Σᵢlog(xᵢ) - Σᵢ[xᵢ^α * log(xᵢ) * ((β(δ+1)-1)/vᵢ - (γ-1) * β * vᵢ^(β-1) / wᵢ)]
//  double d_alpha = n / alpha + arma::sum(log_x);
//
//  // Calculate the terms in the α gradient
//  arma::vec alpha_term1 = (beta * (delta + 1) - 1.0) / v;           // (β(δ+1)-1)/v_i
//  arma::vec alpha_term2 = (gamma - 1.0) * beta * v_beta_m1 / w;     // (γ-1) * β * v_i^(β-1) / w_i
//
//  d_alpha -= arma::sum(x_alpha_log_x % (alpha_term1 - alpha_term2));
//
//  // ∂ℓ/∂β = n/β + (δ+1)Σᵢlog(vᵢ) - Σᵢ[vᵢ^β * log(vᵢ) * ((γ-1) / wᵢ)]
//  double d_beta = n / beta + (delta + 1) * arma::sum(log_v);
//
//  // Calculate the term in the β gradient
//  arma::vec beta_term = (gamma - 1.0) / w;                          // (γ-1) / w_i
//
//  d_beta -= arma::sum(v_beta_log_v % beta_term);
//
//  // ∂ℓ/∂γ = -n[ψ(γ) - ψ(γ+δ+1)] + Σᵢlog(wᵢ)
//  double d_gamma = -n * (R::digamma(gamma) - R::digamma(gamma + delta + 1)) + arma::sum(log_w);
//
//  // ∂ℓ/∂δ = -n[ψ(δ+1) - ψ(γ+δ+1)] + βΣᵢlog(vᵢ)
//  double d_delta = -n * (R::digamma(delta + 1) - R::digamma(gamma + delta + 1)) + beta * arma::sum(log_v);
//
//  // Since we're optimizing negative log-likelihood, negate all derivatives
//  grad[0] = -d_alpha;
//  grad[1] = -d_beta;
//  grad[2] = -d_gamma;
//  grad[3] = -d_delta;
//
//  return grad;
// }
//
//
//
// //' @title Hessian Matrix of the Negative Log-Likelihood for the BKw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize hessian
// //'
// //' @description
// //' Computes the analytic 4x4 Hessian matrix (matrix of second partial derivatives)
// //' of the negative log-likelihood function for the Beta-Kumaraswamy (BKw)
// //' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), \code{gamma} (\eqn{\gamma}), and \code{delta} (\eqn{\delta}).
// //' This distribution is the special case of the Generalized Kumaraswamy (GKw)
// //' distribution where \eqn{\lambda = 1}. The Hessian is useful for estimating
// //' standard errors and in optimization algorithms.
// //'
// //' @param par A numeric vector of length 4 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a 4x4 numeric matrix representing the Hessian matrix of the
// //'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
// //'   where \eqn{\theta = (\alpha, \beta, \gamma, \delta)}.
// //'   Returns a 4x4 matrix populated with \code{NaN} if any parameter values are
// //'   invalid according to their constraints, or if any value in \code{data} is
// //'   not in the interval (0, 1).
// //'
// //' @details
// //' This function calculates the analytic second partial derivatives of the
// //' negative log-likelihood function based on the BKw log-likelihood
// //' (\eqn{\lambda=1} case of GKw, see \code{\link{llbkw}}):
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\alpha) + \ln(\beta) - \ln B(\gamma, \delta+1)]
// //' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta(\delta+1)-1)\ln(v_i) + (\gamma-1)\ln(w_i)]
// //' }
// //' where \eqn{\theta = (\alpha, \beta, \gamma, \delta)}, \eqn{B(a,b)}
// //' is the Beta function (\code{\link[base]{beta}}), and intermediate terms are:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //' }
// //' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}
// //' for \eqn{\theta_i, \theta_j \in \{\alpha, \beta, \gamma, \delta\}}.
// //'
// //' Key properties of the returned matrix:
// //' \itemize{
// //'   \item Dimensions: 4x4.
// //'   \item Symmetry: The matrix is symmetric.
// //'   \item Ordering: Rows and columns correspond to the parameters in the order
// //'     \eqn{\alpha, \beta, \gamma, \delta}.
// //'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
// //' }
// //' This corresponds to the relevant 4x4 submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
// //' evaluated at \eqn{\lambda=1}. The exact analytical formulas are implemented directly.
// //'
// //' @references
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //' (Note: Specific Hessian formulas might be derived or sourced from additional references).
// //'
// //' @seealso
// //' \code{\link{hsgkw}} (parent distribution Hessian),
// //' \code{\link{llbkw}} (negative log-likelihood for BKw),
// //' \code{\link{grbkw}} (gradient for BKw, if available),
// //' \code{\link{dbkw}} (density for BKw),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rbkw, llbkw, grbkw, hsbkw functions for BKw
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_bkw <- c(alpha = 2, beta = 3, gamma = 1, delta = 0.5)
// //' if (exists("rbkw")) {
// //'   sample_data_bkw <- rbkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
// //'                          gamma = true_par_bkw[3], delta = true_par_bkw[4])
// //' } else {
// //'   sample_data_bkw <- rgkw(100, alpha = true_par_bkw[1], beta = true_par_bkw[2],
// //'                          gamma = true_par_bkw[3], delta = true_par_bkw[4], lambda = 1)
// //' }
// //' hist(sample_data_bkw, breaks = 20, main = "BKw(2, 3, 1, 0.5) Sample")
// //'
// //' # --- Find MLE estimates ---
// //' start_par_bkw <- c(1.5, 2.5, 0.8, 0.3)
// //' mle_result_bkw <- stats::optim(par = start_par_bkw,
// //'                                fn = llbkw,
// //'                                gr = if (exists("grbkw")) grbkw else NULL,
// //'                                method = "BFGS",
// //'                                hessian = TRUE, # Ask optim for numerical Hessian
// //'                                data = sample_data_bkw)
// //'
// //' # --- Compare analytical Hessian to numerical Hessian ---
// //' if (mle_result_bkw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("hsbkw")) {
// //'
// //'   mle_par_bkw <- mle_result_bkw$par
// //'   cat("\nComparing Hessians for BKw at MLE estimates:\n")
// //'
// //'   # Numerical Hessian of llbkw
// //'   num_hess_bkw <- numDeriv::hessian(func = llbkw, x = mle_par_bkw, data = sample_data_bkw)
// //'
// //'   # Analytical Hessian from hsbkw
// //'   ana_hess_bkw <- hsbkw(par = mle_par_bkw, data = sample_data_bkw)
// //'
// //'   cat("Numerical Hessian (BKw):\n")
// //'   print(round(num_hess_bkw, 4))
// //'   cat("Analytical Hessian (BKw):\n")
// //'   print(round(ana_hess_bkw, 4))
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between BKw Hessians:\n")
// //'   print(max(abs(num_hess_bkw - ana_hess_bkw)))
// //'
// //'   # Optional: Use analytical Hessian for Standard Errors
// //'   # tryCatch({
// //'   #   cov_matrix_bkw <- solve(ana_hess_bkw)
// //'   #   std_errors_bkw <- sqrt(diag(cov_matrix_bkw))
// //'   #   cat("Std. Errors from Analytical BKw Hessian:\n")
// //'   #   print(std_errors_bkw)
// //'   # }, error = function(e) {
// //'   #   warning("Could not invert analytical BKw Hessian: ", e$message)
// //'   # })
// //'
// //' } else {
// //'   cat("\nSkipping BKw Hessian comparison.\n")
// //'   cat("Requires convergence, 'numDeriv' package, and function 'hsbkw'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hsbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha  = par[0];   // θ[0] = α
//  double beta   = par[1];   // θ[1] = β
//  double gamma  = par[2];   // θ[2] = γ
//  double delta  = par[3];   // θ[3] = δ
//
//  // Simple parameter validation (all > 0)
//  if(alpha <= 0 || beta <= 0 || gamma <= 0 || delta <= 0) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  // Data conversion and basic validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if(arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  int n = x.n_elem;  // sample size
//
//  // Initialize Hessian matrix H (of ℓ(θ)) as 4x4
//  arma::mat H(4,4, arma::fill::zeros);
//
//  // --- CONSTANT TERMS (do not depend on x) ---
//  // L1: n ln(α)  => d²/dα² = -n/α²
//  H(0,0) += -n/(alpha*alpha);
//  // L2: n ln(β)  => d²/dβ² = -n/β²
//  H(1,1) += -n/(beta*beta);
//  // L3: -n ln[B(γ, δ+1)]
//  //   d²/dγ² = -n [ψ₁(γ) - ψ₁(γ+δ+1)]  where ψ₁ is the trigamma function
//  H(2,2) += -n * (R::trigamma(gamma) - R::trigamma(gamma+delta+1));
//  //   d²/dδ² = -n [ψ₁(δ+1) - ψ₁(γ+δ+1)]
//  H(3,3) += -n * (R::trigamma(delta+1) - R::trigamma(gamma+delta+1));
//  //   Mixed derivative (γ,δ): = n ψ₁(γ+δ+1)
//  H(2,3) += n * R::trigamma(gamma+delta+1);
//  H(3,2) = H(2,3);
//
//  // --- TERMS THAT INVOLVE THE OBSERVATIONS ---
//  // Loop over each observation to accumulate contributions from:
//  // L4: (α-1) Σ ln(x_i)  --> contributes only to first derivatives
//  // L5: (β(δ+1)-1) Σ ln(v), where v = 1 - x^α
//  // L6: (γ-1) Σ ln(w), where w = 1 - v^β
//  for (int i = 0; i < n; i++) {
//    double xi    = x(i);
//    double ln_xi = std::log(xi);
//
//    // -- Compute A = x^α and its derivatives --
//    double A = std::pow(xi, alpha);                  // A = x^α
//    double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
//    double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²
//
//    // -- v = 1 - A and its derivatives --
//    double v = 1.0 - A;                              // v = 1 - x^α
//    double ln_v = std::log(v);                       // ln(v)
//    double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
//    double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²
//
//    // --- L5: (β(δ+1)-1) ln(v) ---
//    double beta_delta_factor = beta * (delta + 1.0) - 1.0;
//    // Second derivative w.r.t. α: (β(δ+1)-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
//    double d2L5_dalpha2 = beta_delta_factor * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
//    // Mixed derivative: d²L5/(dα dβ) = d/dβ[(β(δ+1)-1)*(dv_dalpha/v)] = (δ+1)*(dv_dalpha/v)
//    double d2L5_dalpha_dbeta = (delta + 1.0) * (dv_dalpha / v);
//    // Mixed derivative: d²L5/(dα dδ) = d/dδ[(β(δ+1)-1)*(dv_dalpha/v)] = β*(dv_dalpha/v)
//    double d2L5_dalpha_ddelta = beta * (dv_dalpha / v);
//    // Mixed derivative: d²L5/(dβ dδ) = d/dδ[(δ+1)*ln(v)] = ln(v)
//    double d2L5_dbeta_ddelta = ln_v;
//
//    // --- L6: (γ - 1) ln(w), where w = 1 - v^β ---
//    double v_beta = std::pow(v, beta);              // v^β
//    double w = 1.0 - v_beta;                        // w = 1 - v^β
//
//    // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
//    double dw_dv = -beta * std::pow(v, beta - 1.0);
//    // Chain rule: dw/dα = dw/dv * dv/dα
//    double dw_dalpha = dw_dv * dv_dalpha;
//    // Second derivative w.r.t. α for L6:
//    // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
//    // Computing d²w/dα²:
//    //   dw/dα = -β * v^(β-1)*dv_dalpha,
//    //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
//    double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
//                                    + std::pow(v, beta-1.0) * d2v_dalpha2);
//    double d2L6_dalpha2 = (gamma - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
//
//    // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
//    double dw_dbeta = -v_beta * ln_v;
//    // Second derivative w.r.t. β for L6:
//    // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
//    double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
//    double d2L6_dbeta2 = (gamma - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
//
//    // Mixed derivative L6 (α,β): d²/(dα dβ) ln(w) =
//    //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
//    double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
//    double d2L6_dalpha_dbeta = (gamma - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));
//
//    // Mixed derivatives with γ
//    double d2L6_dalpha_dgamma = dw_dalpha / w;
//    double d2L6_dbeta_dgamma = dw_dbeta / w;
//
//    // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
//    // Index: 0 = α, 1 = β, 2 = γ, 3 = δ
//
//    // H(α,α): sum of L1, L5, and L6 (constants already added)
//    H(0,0) += d2L5_dalpha2 + d2L6_dalpha2;
//
//    // H(β,β): contributions from L2, L5, and L6
//    H(1,1) += d2L6_dbeta2;
//
//    // H(α,β): mixed from L5 and L6
//    H(0,1) += d2L5_dalpha_dbeta + d2L6_dalpha_dbeta;
//    H(1,0) = H(0,1);
//
//    // H(α,γ): mixed from L6
//    H(0,2) += d2L6_dalpha_dgamma;
//    H(2,0) = H(0,2);
//
//    // H(α,δ): mixed from L5
//    H(0,3) += d2L5_dalpha_ddelta;
//    H(3,0) = H(0,3);
//
//    // H(β,γ): mixed from L6
//    H(1,2) += d2L6_dbeta_dgamma;
//    H(2,1) = H(1,2);
//
//    // H(β,δ): mixed from L5
//    H(1,3) += d2L5_dbeta_ddelta;
//    H(3,1) = H(1,3);
//
//  } // end of loop
//
//  // Returns the analytic Hessian matrix of the negative log-likelihood
//  return Rcpp::wrap(-H);
// }


// //' @title Analytic Hessian Matrix for Beta-Kumaraswamy Distribution
// //'
// //' @description
// //' Computes the analytic Hessian matrix of the log-likelihood function for
// //' the Beta-Kumaraswamy (BKw) distribution. This function provides
// //' exact second derivatives needed for optimization and inference.
// //'
// //' @param par Numeric vector of length 4 containing the parameters
// //'        (α, β, γ, δ) in that order. All parameters must be positive.
// //' @param data Numeric vector of observations, where all values must be
// //'        in the open interval (0,1).
// //'
// //' @return A 4×4 numeric matrix representing the Hessian of the negative
// //'         log-likelihood function. If parameters or data are invalid
// //'         (parameters ≤ 0 or data outside (0,1)), returns a matrix of
// //'         NaN values.
// //'
// //' @details
// //' The log-likelihood for the Beta-Kumaraswamy distribution is:
// //'
// //' \deqn{
// //' \ell(\theta) = n \ln(\alpha) + n \ln(\beta) - n \ln B(\gamma, \delta+1)
// //' + (\alpha-1) \sum \ln(x_i)
// //' + (\beta(\delta+1)-1) \sum \ln(1 - x_i^\alpha)
// //' + (\gamma-1) \sum \ln\{1 - (1 - x_i^\alpha)^\beta\}
// //' }
// //'
// //' where λ is fixed at 1 for this distribution.
// //'
// //' The implementation computes all second derivatives analytically for each term.
// //' For computational efficiency, the following transformations are used:
// //' \itemize{
// //'   \item \deqn{A = x^α} and derivatives
// //'   \item \deqn{v = 1 - A}
// //'   \item \deqn{w = 1 - v^β}
// //' }
// //'
// //' The returned Hessian matrix has the following structure:
// //' \itemize{
// //'   \item Rows/columns 1-4 correspond to α, β, γ, δ respectively
// //'   \item The matrix is symmetric (as expected for a Hessian)
// //'   \item The matrix represents second derivatives of the negative log-likelihood
// //' }
// //'
// //' This function is implemented in C++ for computational efficiency.
// //'
// //' @examples
// //' \donttest{
// //' # Generate sample data from a BKw distribution
// //' set.seed(123)
// //' x <- rbkw(100, 2, 3, 1, 0.5)
// //' hist(x, breaks = 20, main = "BKw(2, 3, 1, 0.5) Sample")
// //'
// //' # Use in optimization with Hessian-based methods
// //' result <- optim(c(0.5, 0.5, 0.5, 0.5), llbkw, method = "BFGS",
// //'                 hessian = TRUE, data = x)
// //'
// //' # Compare numerical and analytical derivatives
// //' num_grad <- numDeriv::grad(llbkw, x = result$par, data = x)
// //' num_hess <- numDeriv::hessian(llbkw, x = result$par, data = x)
// //'
// //' ana_grad <- grbkw(result$par, data = x)
// //' ana_hess <- hsbkw(result$par, data = x)
// //'
// //' # Check differences (should be very small)
// //' round(num_grad - ana_grad, 4)
// //' round(num_hess - ana_hess, 4)
// //'
// //' }
// //'
// //' @references
// //' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes.
// //' Journal of Hydrology, 46(1-2), 79-88.
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
// //' Journal of Statistical Computation and Simulation, 81(7), 883-898.
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hsbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha  = par[0];   // θ[0] = α
//  double beta   = par[1];   // θ[1] = β
//  double gamma  = par[2];   // θ[2] = γ
//  double delta  = par[3];   // θ[3] = δ
//
//  // Fixed parameter for BKw distribution
//  const double lambda = 1.0;  // λ=1 for BKw
//
//  // Simple parameter validation (all > 0)
//  if(alpha <= 0 || beta <= 0 || gamma <= 0 || delta <= 0) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  // Data conversion and basic validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if(arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericMatrix nanH(4,4);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  int n = x.n_elem;  // sample size
//
//  // Initialize Hessian matrix H (of ℓ(θ)) as 4x4
//  arma::mat H(4,4, arma::fill::zeros);
//
//  // --- CONSTANT TERMS (do not depend on x) ---
//  // L1: n ln(α)  => d²/dα² = -n/α²
//  H(0,0) += -n/(alpha*alpha);
//  // L2: n ln(β)  => d²/dβ² = -n/β²
//  H(1,1) += -n/(beta*beta);
//  // L3: -n ln[B(γ, δ+1)]
//  //   d²/dγ² = -n [ψ₁(γ) - ψ₁(γ+δ+1)]  where ψ₁ is the trigamma function
//  H(2,2) += -n * (R::trigamma(gamma) - R::trigamma(gamma+delta+1));
//  //   d²/dδ² = -n [ψ₁(δ+1) - ψ₁(γ+δ+1)]
//  H(3,3) += -n * (R::trigamma(delta+1) - R::trigamma(gamma+delta+1));
//  //   Mixed derivative (γ,δ): = n ψ₁(γ+δ+1)
//  H(2,3) += n * R::trigamma(gamma+delta+1);
//  H(3,2) = H(2,3);
//
//  // --- TERMS THAT INVOLVE THE OBSERVATIONS ---
//  // Loop over each observation to accumulate contributions from:
//  // L4: (α-1) Σ ln(x_i)  --> contributes only to first derivatives
//  // L5: (β(δ+1)-1) Σ ln(v), where v = 1 - x^α
//  // L6: (γ-1) Σ ln(w), where w = 1 - v^β
//  for (int i = 0; i < n; i++) {
//    double xi    = x(i);
//    double ln_xi = std::log(xi);
//
//    // -- Compute A = x^α and its derivatives --
//    double A = std::pow(xi, alpha);                  // A = x^α
//    double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
//    double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²
//
//    // -- v = 1 - A and its derivatives --
//    double v = 1.0 - A;                              // v = 1 - x^α
//    double ln_v = std::log(v);                       // ln(v)
//    double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
//    double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²
//
//    // --- L5: (β(δ+1)-1) ln(v) ---
//    double beta_delta_factor = beta * (delta + 1.0) - 1.0;
//    // First derivative w.r.t. α: (β(δ+1)-1) * (1/v)*dv_dalpha
//    double dL5_dalpha = beta_delta_factor * (dv_dalpha / v);
//    // Second derivative w.r.t. α: (β(δ+1)-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
//    double d2L5_dalpha2 = beta_delta_factor * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
//    // First derivative w.r.t. β: dL5/dβ = (δ+1) * ln(v)
//    double dL5_dbeta = (delta + 1.0) * ln_v;
//    // First derivative w.r.t. δ: dL5/dδ = β * ln(v)
//    double dL5_ddelta = beta * ln_v;
//    // Mixed derivative: d²L5/(dα dβ) = d/dβ[(β(δ+1)-1)*(dv_dalpha/v)] = (δ+1)*(dv_dalpha/v)
//    double d2L5_dalpha_dbeta = (delta + 1.0) * (dv_dalpha / v);
//    // Mixed derivative: d²L5/(dα dδ) = d/dδ[(β(δ+1)-1)*(dv_dalpha/v)] = β*(dv_dalpha/v)
//    double d2L5_dalpha_ddelta = beta * (dv_dalpha / v);
//    // Mixed derivative: d²L5/(dβ dδ) = d/dδ[(δ+1)*ln(v)] = ln(v)
//    double d2L5_dbeta_ddelta = ln_v;
//
//    // --- L6: (γ - 1) ln(w), where w = 1 - v^β ---
//    double v_beta = std::pow(v, beta);              // v^β
//    double w = 1.0 - v_beta;                        // w = 1 - v^β
//    double ln_w = std::log(w);                      // ln(w)
//    // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
//    double dw_dv = -beta * std::pow(v, beta - 1.0);
//    // Chain rule: dw/dα = dw/dv * dv/dα
//    double dw_dalpha = dw_dv * dv_dalpha;
//    // First derivative w.r.t. α: d/dα ln(w) = (1/w)*dw_dalpha
//    double dL6_dalpha = (gamma - 1.0) * (dw_dalpha / w);
//    // Second derivative w.r.t. α for L6:
//    // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
//    // Computing d²w/dα²:
//    //   dw/dα = -β * v^(β-1)*dv_dalpha,
//    //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
//    double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
//                                    + std::pow(v, beta-1.0) * d2v_dalpha2);
//    double d2L6_dalpha2 = (gamma - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
//    // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
//    double dw_dbeta = -v_beta * ln_v;
//    double dL6_dbeta = (gamma - 1.0) * (dw_dbeta / w);
//    // Second derivative w.r.t. β for L6:
//    // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
//    double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
//    double d2L6_dbeta2 = (gamma - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
//    // Mixed derivative L6 (α,β): d²/(dα dβ) ln(w) =
//    //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
//    // Approximate d/dβ dw_dalpha:
//    double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
//    double d2L6_dalpha_dbeta = (gamma - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));
//    // First derivative w.r.t. γ: dL6/dγ = ln(w)
//    double dL6_dgamma = ln_w;
//    // Mixed derivatives with γ
//    double d2L6_dalpha_dgamma = dw_dalpha / w;
//    double d2L6_dbeta_dgamma = dw_dbeta / w;
//
//    // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
//    // Index: 0 = α, 1 = β, 2 = γ, 3 = δ
//
//    // H(α,α): sum of L1, L5, and L6 (constants already added)
//    H(0,0) += d2L5_dalpha2 + d2L6_dalpha2;
//
//    // H(β,β): contributions from L2, L5, and L6
//    H(1,1) += d2L6_dbeta2;
//
//    // H(α,β): mixed from L5 and L6
//    H(0,1) += d2L5_dalpha_dbeta + d2L6_dalpha_dbeta;
//    H(1,0) = H(0,1);
//
//    // H(α,γ): mixed from L6
//    H(0,2) += d2L6_dalpha_dgamma;
//    H(2,0) = H(0,2);
//
//    // H(α,δ): mixed from L5
//    H(0,3) += d2L5_dalpha_ddelta;
//    H(3,0) = H(0,3);
//
//    // H(β,γ): mixed from L6
//    H(1,2) += d2L6_dbeta_dgamma;
//    H(2,1) = H(1,2);
//
//    // H(β,δ): mixed from L5
//    H(1,3) += d2L5_dbeta_ddelta;
//    H(3,1) = H(1,3);
//
//  } // end of loop
//
//  // Returns the analytic Hessian matrix of the negative log-likelihood
//  return Rcpp::wrap(-H);
// }




/*
----------------------------------------------------------------------------
NUMERIC STABILITY FUNCTIONS AND CHECKS
----------------------------------------------------------------------------
NOTE: We assume the following inline functions already exist in the compilation
environment (similarly to gkwdist.cpp), so we do NOT redefine them here:
- log1mexp(double)
- log1pexp(double)
- safe_log(double)
- safe_exp(double)
- safe_pow(double, double)
etc.

Also, we define here a small parameter checker for the "Exponentiated Kumaraswamy"
(EKw) family, interpreted as a sub-model of GKw with gamma=1 and delta=0,
leaving three free parameters: (alpha, beta, lambda).
*/

/*
----------------------------------------------------------------------------
EXPONENTIATED KUMARASWAMY (EKw) DISTRIBUTION
----------------------------------------------------------------------------

We interpret EKw(α, β, λ) as the GKw distribution with gamma=1 and delta=0.

* PDF:
f(x) = λ * α * β * x^(α-1) * (1 - x^α)^(β - 1) *
[1 - (1 - x^α)^β ]^(λ - 1),    for 0 < x < 1.

* CDF:
F(x) = [1 - (1 - x^α)^β ]^λ,         for 0 < x < 1.

* QUANTILE:
If p = F(x) = [1 - (1 - x^α)^β]^λ, then
p^(1/λ) = 1 - (1 - x^α)^β
(1 - x^α)^β = 1 - p^(1/λ)
x^α = 1 - [1 - p^(1/λ)]^(1/β)
x = {1 - [1 - p^(1/λ)]^(1/β)}^(1/α).

* RNG:
We can generate via the quantile method: U ~ Uniform(0,1), X= Q(U).

X = Q(U) = {1 - [1 - U^(1/λ)]^(1/β)}^(1/α).

* LOG-LIKELIHOOD:
The log-density for observation x in (0,1):
log f(x) = log(λ) + log(α) + log(β)
+ (α-1)*log(x)
+ (β-1)*log(1 - x^α)
+ (λ-1)*log(1 - (1 - x^α)^β).

Summation of log-likelihood over all x. We return negative of that for 'llekw'.
*/


// -----------------------------------------------------------------------------
// Parameter checker for EKw distribution
// EKw(α, β, λ):  all must be > 0
// -----------------------------------------------------------------------------
inline bool check_ekw_pars(double alpha, double beta, double lambda, bool strict=false) {
if (alpha <= 0.0 || beta <= 0.0 || lambda <= 0.0) {
return false;
}
if (strict) {
const double MINP = 1e-6;
const double MAXP = 1e6;
if (alpha < MINP || beta < MINP || lambda < MINP)  return false;
if (alpha > MAXP || beta > MAXP || lambda > MAXP)  return false;
}
return true;
}

// -----------------------------------------------------------------------------
// 1) dekw: PDF of Exponentiated Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Density of the Exponentiated Kumaraswamy (EKw) Distribution
//'
//' @author Lopes, J. E.
//' @keywords distribution density
//'
//' @description
//' Computes the probability density function (PDF) for the Exponentiated
//' Kumaraswamy (EKw) distribution with parameters \code{alpha} (\eqn{\alpha}),
//' \code{beta} (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}).
//' This distribution is defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lambda Shape parameter \code{lambda} > 0 (exponent parameter).
//'   Can be a scalar or a vector. Default: 1.0.
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned (\eqn{\log(f(x))}). Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{alpha}, \code{beta},
//'   \code{lambda}). Returns \code{0} (or \code{-Inf} if
//'   \code{log_prob = TRUE}) for \code{x} outside the interval (0, 1), or
//'   \code{NaN} if parameters are invalid (e.g., \code{alpha <= 0},
//'   \code{beta <= 0}, \code{lambda <= 0}).
//'
//' @details
//' The probability density function (PDF) of the Exponentiated Kumaraswamy (EKw)
//' distribution is given by:
//' \deqn{
//' f(x; \alpha, \beta, \lambda) = \lambda \alpha \beta x^{\alpha-1} (1 - x^\alpha)^{\beta-1} \bigl[1 - (1 - x^\alpha)^\beta \bigr]^{\lambda - 1}
//' }
//' for \eqn{0 < x < 1}.
//'
//' The EKw distribution is a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{dgkw}}) obtained
//' by setting the parameters \eqn{\gamma = 1} and \eqn{\delta = 0}.
//' When \eqn{\lambda = 1}, the EKw distribution reduces to the standard
//' Kumaraswamy distribution.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{dgkw}} (parent distribution density),
//' \code{\link{pekw}}, \code{\link{qekw}}, \code{\link{rekw}} (other EKw functions),
//'
//' @examples
//' \donttest{
//' # Example values
//' x_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' lambda_par <- 1.5 # Exponent parameter
//'
//' # Calculate density
//' densities <- dekw(x_vals, alpha_par, beta_par, lambda_par)
//' print(densities)
//'
//' # Calculate log-density
//' log_densities <- dekw(x_vals, alpha_par, beta_par, lambda_par, log_prob = TRUE)
//' print(log_densities)
//' # Check: should match log(densities)
//' print(log(densities))
//'
//' # Compare with dgkw setting gamma = 1, delta = 0
//' densities_gkw <- dgkw(x_vals, alpha_par, beta_par, gamma = 1.0, delta = 0.0,
//'                       lambda = lambda_par)
//' print(paste("Max difference:", max(abs(densities - densities_gkw)))) # Should be near zero
//'
//' # Plot the density for different lambda values
//' curve_x <- seq(0.01, 0.99, length.out = 200)
//' curve_y1 <- dekw(curve_x, alpha = 2, beta = 3, lambda = 0.5) # less peaked
//' curve_y2 <- dekw(curve_x, alpha = 2, beta = 3, lambda = 1.0) # standard Kw
//' curve_y3 <- dekw(curve_x, alpha = 2, beta = 3, lambda = 2.0) # more peaked
//'
//' plot(curve_x, curve_y2, type = "l", main = "EKw Density Examples (alpha=2, beta=3)",
//'      xlab = "x", ylab = "f(x)", col = "red", ylim = range(0, curve_y1, curve_y2, curve_y3))
//' lines(curve_x, curve_y1, col = "blue")
//' lines(curve_x, curve_y3, col = "green")
//' legend("topright", legend = c("lambda=0.5", "lambda=1.0 (Kw)", "lambda=2.0"),
//'        col = c("blue", "red", "green"), lty = 1, bty = "n")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dekw(
 const arma::vec& x,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& lambda,
 bool log_prob = false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N = std::max({ x.n_elem, a_vec.n_elem, b_vec.n_elem, l_vec.n_elem });
arma::vec out(N);
out.fill(log_prob ? R_NegInf : 0.0);

for (size_t i=0; i<N; i++) {
 double a = a_vec[i % a_vec.n_elem];
 double b = b_vec[i % b_vec.n_elem];
 double l = l_vec[i % l_vec.n_elem];
 double xx = x[i % x.n_elem];

 if (!check_ekw_pars(a, b, l)) {
   // invalid => PDF=0 or logPDF=-Inf
   continue;
 }
 // domain check
 if (xx <= 0.0 || xx >= 1.0 || !R_finite(xx)) {
   continue;
 }

 // log f(x) = log(lambda) + log(a) + log(b) + (a-1)*log(x)
 //            + (b-1)*log(1 - x^a)
 //            + (lambda-1)*log(1 - (1 - x^a)^b)
 double ll  = std::log(l);
 double la  = std::log(a);
 double lb  = std::log(b);
 double lx  = std::log(xx);

 double xalpha = a*lx; // log(x^a)
 double log_1_xalpha = log1mexp(xalpha); // log(1 - x^a)
 if (!R_finite(log_1_xalpha)) {
   continue;
 }

 double term2 = (b - 1.0)*log_1_xalpha; // (b-1)* log(1 - x^a)

 // let A= (1 - x^a)^b => logA= b*log_1_xalpha
 double logA = b*log_1_xalpha;
 double log_1_minus_A = log1mexp(logA); // log(1 - A)
 if (!R_finite(log_1_minus_A)) {
   continue;
 }
 double term3 = (l - 1.0)* log_1_minus_A;

 double log_pdf = ll + la + lb
 + (a - 1.0)* lx
 + term2
 + term3;

 if (log_prob) {
   out(i)= log_pdf;
 } else {
   out(i)= std::exp(log_pdf);
 }
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}

// -----------------------------------------------------------------------------
// 2) pekw: CDF of Exponentiated Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Cumulative Distribution Function (CDF) of the EKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution cumulative
//'
//' @description
//' Computes the cumulative distribution function (CDF), \eqn{P(X \le q)}, for the
//' Exponentiated Kumaraswamy (EKw) distribution with parameters \code{alpha}
//' (\eqn{\alpha}), \code{beta} (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}).
//' This distribution is defined on the interval (0, 1) and is a special case
//' of the Generalized Kumaraswamy (GKw) distribution where \eqn{\gamma = 1}
//' and \eqn{\delta = 0}.
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lambda Shape parameter \code{lambda} > 0 (exponent parameter).
//'   Can be a scalar or a vector. Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms/complements
//'   depending on \code{lower_tail} and \code{log_p}. The length of the result
//'   is determined by the recycling rule applied to the arguments (\code{q},
//'   \code{alpha}, \code{beta}, \code{lambda}). Returns \code{0} (or \code{-Inf}
//'   if \code{log_p = TRUE}) for \code{q <= 0} and \code{1} (or \code{0} if
//'   \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN} for invalid
//'   parameters.
//'
//' @details
//' The Exponentiated Kumaraswamy (EKw) distribution is a special case of the
//' five-parameter Generalized Kumaraswamy distribution (\code{\link{pgkw}})
//' obtained by setting parameters \eqn{\gamma = 1} and \eqn{\delta = 0}.
//'
//' The CDF of the GKw distribution is \eqn{F_{GKw}(q) = I_{y(q)}(\gamma, \delta+1)},
//' where \eqn{y(q) = [1-(1-q^{\alpha})^{\beta}]^{\lambda}} and \eqn{I_x(a,b)}
//' is the regularized incomplete beta function (\code{\link[stats]{pbeta}}).
//' Setting \eqn{\gamma=1} and \eqn{\delta=0} gives \eqn{I_{y(q)}(1, 1)}. Since
//' \eqn{I_x(1, 1) = x}, the CDF simplifies to \eqn{y(q)}:
//' \deqn{
//' F(q; \alpha, \beta, \lambda) = \bigl[1 - (1 - q^\alpha)^\beta \bigr]^\lambda
//' }
//' for \eqn{0 < q < 1}.
//' The implementation uses this closed-form expression for efficiency and handles
//' \code{lower_tail} and \code{log_p} arguments appropriately.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{pgkw}} (parent distribution CDF),
//' \code{\link{dekw}}, \code{\link{qekw}}, \code{\link{rekw}} (other EKw functions),
//'
//' @examples
//' \donttest{
//' # Example values
//' q_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' lambda_par <- 1.5
//'
//' # Calculate CDF P(X <= q)
//' probs <- pekw(q_vals, alpha_par, beta_par, lambda_par)
//' print(probs)
//'
//' # Calculate upper tail P(X > q)
//' probs_upper <- pekw(q_vals, alpha_par, beta_par, lambda_par,
//'                     lower_tail = FALSE)
//' print(probs_upper)
//' # Check: probs + probs_upper should be 1
//' print(probs + probs_upper)
//'
//' # Calculate log CDF
//' log_probs <- pekw(q_vals, alpha_par, beta_par, lambda_par, log_p = TRUE)
//' print(log_probs)
//' # Check: should match log(probs)
//' print(log(probs))
//'
//' # Compare with pgkw setting gamma = 1, delta = 0
//' probs_gkw <- pgkw(q_vals, alpha_par, beta_par, gamma = 1.0, delta = 0.0,
//'                  lambda = lambda_par)
//' print(paste("Max difference:", max(abs(probs - probs_gkw)))) # Should be near zero
//'
//' # Plot the CDF for different lambda values
//' curve_q <- seq(0.01, 0.99, length.out = 200)
//' curve_p1 <- pekw(curve_q, alpha = 2, beta = 3, lambda = 0.5)
//' curve_p2 <- pekw(curve_q, alpha = 2, beta = 3, lambda = 1.0) # standard Kw
//' curve_p3 <- pekw(curve_q, alpha = 2, beta = 3, lambda = 2.0)
//'
//' plot(curve_q, curve_p2, type = "l", main = "EKw CDF Examples (alpha=2, beta=3)",
//'      xlab = "q", ylab = "F(q)", col = "red", ylim = c(0, 1))
//' lines(curve_q, curve_p1, col = "blue")
//' lines(curve_q, curve_p3, col = "green")
//' legend("bottomright", legend = c("lambda=0.5", "lambda=1.0 (Kw)", "lambda=2.0"),
//'        col = c("blue", "red", "green"), lty = 1, bty = "n")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pekw(
 const arma::vec& q,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N = std::max({ q.n_elem, a_vec.n_elem, b_vec.n_elem, l_vec.n_elem });
arma::vec out(N);

for (size_t i=0; i<N; i++) {
 double a = a_vec[i % a_vec.n_elem];
 double b = b_vec[i % b_vec.n_elem];
 double l = l_vec[i % l_vec.n_elem];
 double xx = q[i % q.n_elem];

 if (!check_ekw_pars(a, b, l)) {
   out(i)= NA_REAL;
   continue;
 }

 // boundary
 if (!R_finite(xx) || xx <= 0.0) {
   double val0 = (lower_tail ? 0.0 : 1.0);
   out(i) = (log_p ? std::log(val0) : val0);
   continue;
 }
 if (xx >= 1.0) {
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = (log_p ? std::log(val1) : val1);
   continue;
 }

 // F(x)= [1 - (1 - x^a)^b]^lambda
 double lx = std::log(xx);
 double xalpha = std::exp(a*lx);
 double omx = 1.0 - xalpha;         // (1 - x^α)
 if (omx <= 0.0) {
   // => F=1
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = (log_p ? std::log(val1) : val1);
   continue;
 }
 double t = 1.0 - std::pow(omx, b);
 if (t <= 0.0) {
   // => F=0
   double val0 = (lower_tail ? 0.0 : 1.0);
   out(i) = (log_p ? std::log(val0) : val0);
   continue;
 }
 if (t >= 1.0) {
   // => F=1
   double val1 = (lower_tail ? 1.0 : 0.0);
   out(i) = (log_p ? std::log(val1) : val1);
   continue;
 }
 double val = std::pow(t, l);
 // F(x)=val => if not lower tail => 1-val
 if (!lower_tail) {
   val = 1.0 - val;
 }
 if (log_p) {
   val = std::log(val);
 }
 out(i) = val;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 3) qekw: Quantile of Exponentiated Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Quantile Function of the Exponentiated Kumaraswamy (EKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution quantile
//'
//' @description
//' Computes the quantile function (inverse CDF) for the Exponentiated
//' Kumaraswamy (EKw) distribution with parameters \code{alpha} (\eqn{\alpha}),
//' \code{beta} (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}).
//' It finds the value \code{q} such that \eqn{P(X \le q) = p}. This distribution
//' is a special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\gamma = 1} and \eqn{\delta = 0}.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lambda Shape parameter \code{lambda} > 0 (exponent parameter).
//'   Can be a scalar or a vector. Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are \eqn{p = P(X \le q)},
//'   otherwise, probabilities are \eqn{p = P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{alpha}, \code{beta}, \code{lambda}).
//'   Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{alpha <= 0},
//'           \code{beta <= 0}, \code{lambda <= 0}).
//'   }
//'   Boundary return values are adjusted accordingly for \code{lower_tail = FALSE}.
//'
//' @details
//' The quantile function \eqn{Q(p)} is the inverse of the CDF \eqn{F(q)}. The CDF
//' for the EKw (\eqn{\gamma=1, \delta=0}) distribution is \eqn{F(q) = [1 - (1 - q^\alpha)^\beta ]^\lambda}
//' (see \code{\link{pekw}}). Inverting this equation for \eqn{q} yields the
//' quantile function:
//' \deqn{
//' Q(p) = \left\{ 1 - \left[ 1 - p^{1/\lambda} \right]^{1/\beta} \right\}^{1/\alpha}
//' }
//' The function uses this closed-form expression and correctly handles the
//' \code{lower_tail} and \code{log_p} arguments by transforming \code{p}
//' appropriately before applying the formula. This is equivalent to the general
//' GKw quantile function (\code{\link{qgkw}}) evaluated with \eqn{\gamma=1, \delta=0}.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{qgkw}} (parent distribution quantile function),
//' \code{\link{dekw}}, \code{\link{pekw}}, \code{\link{rekw}} (other EKw functions),
//' \code{\link[stats]{qunif}}
//'
//' @examples
//' \donttest{
//' # Example values
//' p_vals <- c(0.1, 0.5, 0.9)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' lambda_par <- 1.5
//'
//' # Calculate quantiles
//' quantiles <- qekw(p_vals, alpha_par, beta_par, lambda_par)
//' print(quantiles)
//'
//' # Calculate quantiles for upper tail probabilities P(X > q) = p
//' quantiles_upper <- qekw(p_vals, alpha_par, beta_par, lambda_par,
//'                         lower_tail = FALSE)
//' print(quantiles_upper)
//' # Check: qekw(p, ..., lt=F) == qekw(1-p, ..., lt=T)
//' print(qekw(1 - p_vals, alpha_par, beta_par, lambda_par))
//'
//' # Calculate quantiles from log probabilities
//' log_p_vals <- log(p_vals)
//' quantiles_logp <- qekw(log_p_vals, alpha_par, beta_par, lambda_par,
//'                        log_p = TRUE)
//' print(quantiles_logp)
//' # Check: should match original quantiles
//' print(quantiles)
//'
//' # Compare with qgkw setting gamma = 1, delta = 0
//' quantiles_gkw <- qgkw(p_vals, alpha = alpha_par, beta = beta_par,
//'                      gamma = 1.0, delta = 0.0, lambda = lambda_par)
//' print(paste("Max difference:", max(abs(quantiles - quantiles_gkw)))) # Should be near zero
//'
//' # Verify inverse relationship with pekw
//' p_check <- 0.75
//' q_calc <- qekw(p_check, alpha_par, beta_par, lambda_par)
//' p_recalc <- pekw(q_calc, alpha_par, beta_par, lambda_par)
//' print(paste("Original p:", p_check, " Recalculated p:", p_recalc))
//' # abs(p_check - p_recalc) < 1e-9 # Should be TRUE
//'
//' # Boundary conditions
//' print(qekw(c(0, 1), alpha_par, beta_par, lambda_par)) # Should be 0, 1
//' print(qekw(c(-Inf, 0), alpha_par, beta_par, lambda_par, log_p = TRUE)) # Should be 0, 1
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qekw(
 const arma::vec& p,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N = std::max({ p.n_elem, a_vec.n_elem, b_vec.n_elem, l_vec.n_elem });
arma::vec out(N);

for (size_t i=0; i<N; i++){
 double a = a_vec[i % a_vec.n_elem];
 double b = b_vec[i % b_vec.n_elem];
 double l = l_vec[i % l_vec.n_elem];
 double pp = p[i % p.n_elem];

 if (!check_ekw_pars(a, b, l)) {
   out(i) = NA_REAL;
   continue;
 }

 // handle log_p
 if (log_p) {
   if (pp > 0.0) {
     // log(p)>0 => p>1 => invalid
     out(i) = NA_REAL;
     continue;
   }
   pp = std::exp(pp);
 }
 // handle tail
 if (!lower_tail) {
   pp = 1.0 - pp;
 }

 // boundaries
 if (pp <= 0.0) {
   out(i) = 0.0;
   continue;
 }
 if (pp >= 1.0) {
   out(i) = 1.0;
   continue;
 }

 // Q(p)= {1 - [1 - p^(1/λ)]^(1/β)}^(1/α)
 double step1 = std::pow(pp, 1.0/l);          // p^(1/λ)
 double step2 = 1.0 - step1;                  // 1 - p^(1/λ)
 if (step2 < 0.0) step2 = 0.0;
 double step3 = std::pow(step2, 1.0/b);       // [1 - p^(1/λ)]^(1/β)
 double step4 = 1.0 - step3;                  // 1 - ...
 if (step4 < 0.0) step4 = 0.0;

 double x;
 if (a == 1.0) {
   x = step4;
 } else {
   x = std::pow(step4, 1.0/a);
   if (x < 0.0) x = 0.0;
   if (x > 1.0) x = 1.0;
 }

 out(i) = x;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 4) rekw: RNG for Exponentiated Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Random Number Generation for the Exponentiated Kumaraswamy (EKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution random
//'
//' @description
//' Generates random deviates from the Exponentiated Kumaraswamy (EKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}). This distribution is a
//' special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\gamma = 1} and \eqn{\delta = 0}.
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lambda Shape parameter \code{lambda} > 0 (exponent parameter).
//'   Can be a scalar or a vector. Default: 1.0.
//'
//' @return A vector of length \code{n} containing random deviates from the EKw
//'   distribution. The length of the result is determined by \code{n} and the
//'   recycling rule applied to the parameters (\code{alpha}, \code{beta},
//'   \code{lambda}). Returns \code{NaN} if parameters
//'   are invalid (e.g., \code{alpha <= 0}, \code{beta <= 0}, \code{lambda <= 0}).
//'
//' @details
//' The generation method uses the inverse transform (quantile) method.
//' That is, if \eqn{U} is a random variable following a standard Uniform
//' distribution on (0, 1), then \eqn{X = Q(U)} follows the EKw distribution,
//' where \eqn{Q(u)} is the EKw quantile function (\code{\link{qekw}}):
//' \deqn{
//' Q(u) = \left\{ 1 - \left[ 1 - u^{1/\lambda} \right]^{1/\beta} \right\}^{1/\alpha}
//' }
//' This is computationally equivalent to the general GKw generation method
//' (\code{\link{rgkw}}) when specialized for \eqn{\gamma=1, \delta=0}, as the
//' required Beta(1, 1) random variate is equivalent to a standard Uniform(0, 1)
//' variate. The implementation generates \eqn{U} using \code{\link[stats]{runif}}
//' and applies the transformation above.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Devroye, L. (1986). *Non-Uniform Random Variate Generation*. Springer-Verlag.
//' (General methods for random variate generation).
//'
//' @seealso
//' \code{\link{rgkw}} (parent distribution random generation),
//' \code{\link{dekw}}, \code{\link{pekw}}, \code{\link{qekw}} (other EKw functions),
//' \code{\link[stats]{runif}}
//'
//' @examples
//' \donttest{
//' set.seed(2027) # for reproducibility
//'
//' # Generate 1000 random values from a specific EKw distribution
//' alpha_par <- 2.0
//' beta_par <- 3.0
//' lambda_par <- 1.5
//'
//' x_sample_ekw <- rekw(1000, alpha = alpha_par, beta = beta_par, lambda = lambda_par)
//' summary(x_sample_ekw)
//'
//' # Histogram of generated values compared to theoretical density
//' hist(x_sample_ekw, breaks = 30, freq = FALSE, # freq=FALSE for density
//'      main = "Histogram of EKw Sample", xlab = "x", ylim = c(0, 3.0))
//' curve(dekw(x, alpha = alpha_par, beta = beta_par, lambda = lambda_par),
//'       add = TRUE, col = "red", lwd = 2, n = 201)
//' legend("topright", legend = "Theoretical PDF", col = "red", lwd = 2, bty = "n")
//'
//' # Comparing empirical and theoretical quantiles (Q-Q plot)
//' prob_points <- seq(0.01, 0.99, by = 0.01)
//' theo_quantiles <- qekw(prob_points, alpha = alpha_par, beta = beta_par,
//'                        lambda = lambda_par)
//' emp_quantiles <- quantile(x_sample_ekw, prob_points, type = 7)
//'
//' plot(theo_quantiles, emp_quantiles, pch = 16, cex = 0.8,
//'      main = "Q-Q Plot for EKw Distribution",
//'      xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles (n=1000)")
//' abline(a = 0, b = 1, col = "blue", lty = 2)
//'
//' # Compare summary stats with rgkw(..., gamma=1, delta=0, ...)
//' # Note: individual values will differ due to randomness
//' x_sample_gkw <- rgkw(1000, alpha = alpha_par, beta = beta_par, gamma = 1.0,
//'                      delta = 0.0, lambda = lambda_par)
//' print("Summary stats for rekw sample:")
//' print(summary(x_sample_ekw))
//' print("Summary stats for rgkw(gamma=1, delta=0) sample:")
//' print(summary(x_sample_gkw)) # Should be similar
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rekw(
 int n,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 const Rcpp::NumericVector& lambda
) {
if (n <= 0) {
 Rcpp::stop("rekw: n must be positive");
}

arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t k = std::max({ a_vec.n_elem, b_vec.n_elem, l_vec.n_elem });
arma::vec out(n);

for (int i=0; i<n; i++){
 size_t idx = i % k;
 double a = a_vec[idx % a_vec.n_elem];
 double b = b_vec[idx % b_vec.n_elem];
 double l = l_vec[idx % l_vec.n_elem];

 if (!check_ekw_pars(a, b, l)) {
   out(i) = NA_REAL;
   Rcpp::warning("rekw: invalid parameters at index %d", i+1);
   continue;
 }

 double U = R::runif(0.0, 1.0);
 // X = Q(U)
 double step1 = std::pow(U, 1.0/l);
 double step2 = 1.0 - step1;
 if (step2 < 0.0) step2 = 0.0;
 double step3 = std::pow(step2, 1.0/b);
 double step4 = 1.0 - step3;
 if (step4 < 0.0) step4 = 0.0;

 double x;
 if (a == 1.0) {
   x = step4;
 } else {
   x = std::pow(step4, 1.0/a);
   if (!R_finite(x) || x < 0.0) x = 0.0;
   if (x > 1.0) x = 1.0;
 }

 out(i) = x;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 5) llekw: Negative Log-Likelihood of EKw
// -----------------------------------------------------------------------------



//' @title Negative Log-Likelihood for the Exponentiated Kumaraswamy (EKw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize
//'
//' @description
//' Computes the negative log-likelihood function for the Exponentiated
//' Kumaraswamy (EKw) distribution with parameters \code{alpha} (\eqn{\alpha}),
//' \code{beta} (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}), given a vector
//' of observations. This distribution is the special case of the Generalized
//' Kumaraswamy (GKw) distribution where \eqn{\gamma = 1} and \eqn{\delta = 0}.
//' This function is suitable for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 3 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
//'   if any parameter values in \code{par} are invalid according to their
//'   constraints, or if any value in \code{data} is not in the interval (0, 1).
//'
//' @details
//' The Exponentiated Kumaraswamy (EKw) distribution is the GKw distribution
//' (\code{\link{dekw}}) with \eqn{\gamma=1} and \eqn{\delta=0}. Its probability
//' density function (PDF) is:
//' \deqn{
//' f(x | \theta) = \lambda \alpha \beta x^{\alpha-1} (1 - x^\alpha)^{\beta-1} \bigl[1 - (1 - x^\alpha)^\beta \bigr]^{\lambda - 1}
//' }
//' for \eqn{0 < x < 1} and \eqn{\theta = (\alpha, \beta, \lambda)}.
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\lambda) + \ln(\alpha) + \ln(\beta)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i)]
//' }
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//' }
//' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
//' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
//' Numerical stability is maintained similarly to \code{\link{llgkw}}.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{llgkw}} (parent distribution negative log-likelihood),
//' \code{\link{dekw}}, \code{\link{pekw}}, \code{\link{qekw}}, \code{\link{rekw}},
//' \code{grekw} (gradient, if available),
//' \code{hsekw} (Hessian, if available),
//' \code{\link[stats]{optim}}
//'
//' @examples
//' \donttest{
//' # Assuming existence of rekw, grekw, hsekw functions for EKw distribution
//'
//' # Generate sample data from a known EKw distribution
//' set.seed(123)
//' true_par_ekw <- c(alpha = 2, beta = 3, lambda = 0.5)
//' # Use rekw if it exists, otherwise use rgkw with gamma=1, delta=0
//' if (exists("rekw")) {
//'   sample_data_ekw <- rekw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
//'                           lambda = true_par_ekw[3])
//' } else {
//'   sample_data_ekw <- rgkw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
//'                          gamma = 1, delta = 0, lambda = true_par_ekw[3])
//' }
//' hist(sample_data_ekw, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess
//' start_par_ekw <- c(1.5, 2.5, 0.8)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' # Use method="L-BFGS-B" for box constraints if needed (all params > 0)
//' mle_result_ekw <- stats::optim(par = start_par_ekw,
//'                                fn = llekw, # Use the EKw neg-log-likelihood
//'                                method = "BFGS", # Or "L-BFGS-B" with lower=1e-6
//'                                hessian = TRUE,
//'                                data = sample_data_ekw)
//'
//' # Check convergence and results
//' if (mle_result_ekw$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par_ekw <- mle_result_ekw$par
//'   print("Estimated EKw parameters:")
//'   print(mle_par_ekw)
//'   print("True EKw parameters:")
//'   print(true_par_ekw)
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result_ekw$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires 'numDeriv' package and analytical functions 'grekw', 'hsekw'
//' if (mle_result_ekw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grekw") && exists("hsekw")) {
//'
//'   cat("\nComparing Derivatives at EKw MLE estimates:\n")
//'
//'   # Numerical derivatives of llekw
//'   num_grad_ekw <- numDeriv::grad(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
//'   num_hess_ekw <- numDeriv::hessian(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
//'
//'   # Analytical derivatives (assuming they return derivatives of negative LL)
//'   ana_grad_ekw <- grekw(par = mle_par_ekw, data = sample_data_ekw)
//'   ana_hess_ekw <- hsekw(par = mle_par_ekw, data = sample_data_ekw)
//'
//'   # Check differences
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad_ekw - ana_grad_ekw)))
//'   cat("Max absolute difference between Hessians:\n")
//'   print(max(abs(num_hess_ekw - ana_hess_ekw)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison for EKw.\n")
//'    cat("Requires convergence, 'numDeriv' package and functions 'grekw', 'hsekw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llekw(const Rcpp::NumericVector& par,
            const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 3) return R_PosInf;

 double alpha = par[0];
 double beta = par[1];
 double lambda = par[2];

 if (!check_ekw_pars(alpha, beta, lambda)) return R_PosInf;

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1) return R_PosInf;
 if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) return R_PosInf;

 int n = x.n_elem;

 // Calculate log parameters for better precision
 double log_alpha = safe_log(alpha);
 double log_beta = safe_log(beta);
 double log_lambda = safe_log(lambda);

 // Constant term
 double const_term = n * (log_lambda + log_alpha + log_beta);

 // Initialize sum terms
 double sum_term1 = 0.0; // (alpha-1) * sum(log(x))
 double sum_term2 = 0.0; // (beta-1) * sum(log(1-x^alpha))
 double sum_term3 = 0.0; // (lambda-1) * sum(log(1-(1-x^alpha)^beta))

 for (int i = 0; i < n; i++) {
   double xi = x(i);
   double log_xi = std::log(xi);

   // Term 1: (alpha-1) * log(x)
   sum_term1 += (alpha - 1.0) * log_xi;

   // Stable calculation of x^alpha for large alpha
   double x_alpha;
   if (alpha > 100.0 || (alpha * log_xi < -700.0)) {
     x_alpha = std::exp(alpha * log_xi);
   } else {
     x_alpha = std::pow(xi, alpha);
   }

   // Stable calculation of (1-x^alpha) and log(1-x^alpha)
   double one_minus_x_alpha;
   double log_one_minus_x_alpha;

   if (x_alpha > 0.9995) {
     // For x^alpha close to 1, use complement approach
     one_minus_x_alpha = -std::expm1(alpha * log_xi);
     log_one_minus_x_alpha = std::log(one_minus_x_alpha);
   } else {
     one_minus_x_alpha = 1.0 - x_alpha;
     log_one_minus_x_alpha = std::log(one_minus_x_alpha);
   }

   // Term 2: (beta-1) * log(1-x^alpha)
   sum_term2 += (beta - 1.0) * log_one_minus_x_alpha;

   // Stable calculation of (1-x^alpha)^beta
   double v_beta;
   if (beta > 100.0 || (beta * log_one_minus_x_alpha < -700.0)) {
     v_beta = std::exp(beta * log_one_minus_x_alpha);
   } else {
     v_beta = std::pow(one_minus_x_alpha, beta);
   }

   // Stable calculation of [1-(1-x^alpha)^beta]
   double one_minus_v_beta;
   double log_one_minus_v_beta;

   if (v_beta > 0.9995) {
     // When (1-x^alpha)^beta is close to 1
     one_minus_v_beta = -std::expm1(beta * log_one_minus_x_alpha);
   } else {
     one_minus_v_beta = 1.0 - v_beta;
   }

   // CRITICAL: Handle extreme lambda values
   // Prevent underflow when one_minus_v_beta is small and lambda is large
   if (one_minus_v_beta < 1e-300) {
     one_minus_v_beta = 1e-300;
   }

   log_one_minus_v_beta = std::log(one_minus_v_beta);

   // Term 3: (lambda-1) * log(1-(1-x^alpha)^beta)
   // Special handling for lambda near 1
   if (std::abs(lambda - 1.0) < 1e-10) {
     // For lambda ≈ 1, avoid numerical cancellation
     if (std::abs(lambda - 1.0) > 1e-15) {
       sum_term3 += (lambda - 1.0) * log_one_minus_v_beta;
     }
     // For lambda = 1 (machine precision), term is zero
   } else if (lambda > 1000.0 && log_one_minus_v_beta < -0.01) {
     // Special case for very large lambda
     double scaled_term = std::max(log_one_minus_v_beta, -700.0 / lambda);
     sum_term3 += (lambda - 1.0) * scaled_term;
   } else {
     // Standard case
     sum_term3 += (lambda - 1.0) * log_one_minus_v_beta;
   }
 }

 // Combine all terms
 double loglike = const_term + sum_term1 + sum_term2 + sum_term3;

 return -loglike;
}



// -----------------------------------------------------------------------------
// 6) grekw: Gradient of Negative Log-Likelihood of EKw
// -----------------------------------------------------------------------------

//' @title Gradient of the Negative Log-Likelihood for the EKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the Exponentiated Kumaraswamy (EKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}). This distribution is the
//' special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\gamma = 1} and \eqn{\delta = 0}. The gradient is useful for optimization.
//'
//' @param par A numeric vector of length 3 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 3 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter: \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \lambda)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the EKw (\eqn{\gamma=1, \delta=0})
//' model are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
//' + \sum_{i=1}^{n}\left[x_i^{\alpha} \ln(x_i) \left(\frac{\beta-1}{v_i} -
//' \frac{(\lambda-1) \beta v_i^{\beta-1}}{w_i}\right)\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - \sum_{i=1}^{n}\ln(v_i)
//' + \sum_{i=1}^{n}\left[\frac{(\lambda-1) v_i^{\beta} \ln(v_i)}{w_i}\right]
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} - \sum_{i=1}^{n}\ln(w_i)
//' }
//'
//' where:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//' }
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the relevant components
//' of the general GKw gradient (\code{\link{grgkw}}) evaluated at \eqn{\gamma=1, \delta=0}.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' (Note: Specific gradient formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{grgkw}} (parent distribution gradient),
//' \code{\link{llekw}} (negative log-likelihood for EKw),
//' \code{hsekw} (Hessian for EKw, if available),
//' \code{\link{dekw}} (density for EKw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rekw, llekw, grekw, hsekw functions for EKw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_ekw <- c(alpha = 2, beta = 3, lambda = 0.5)
//' if (exists("rekw")) {
//'   sample_data_ekw <- rekw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
//'                           lambda = true_par_ekw[3])
//' } else {
//'   sample_data_ekw <- rgkw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
//'                           gamma = 1, delta = 0, lambda = true_par_ekw[3])
//' }
//' hist(sample_data_ekw, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_ekw <- c(1.5, 2.5, 0.8)
//' mle_result_ekw <- stats::optim(par = start_par_ekw,
//'                                fn = llekw,
//'                                gr = grekw, # Use analytical gradient for EKw
//'                                method = "BFGS",
//'                                hessian = TRUE,
//'                                data = sample_data_ekw)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_ekw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_ekw <- mle_result_ekw$par
//'   cat("\nComparing Gradients for EKw at MLE estimates:\n")
//'
//'   # Numerical gradient of llekw
//'   num_grad_ekw <- numDeriv::grad(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
//'
//'   # Analytical gradient from grekw
//'   ana_grad_ekw <- grekw(par = mle_par_ekw, data = sample_data_ekw)
//'
//'   cat("Numerical Gradient (EKw):\n")
//'   print(num_grad_ekw)
//'   cat("Analytical Gradient (EKw):\n")
//'   print(ana_grad_ekw)
//'
//'   # Check differences
//'   cat("Max absolute difference between EKw gradients:\n")
//'   print(max(abs(num_grad_ekw - ana_grad_ekw)))
//'
//' } else {
//'   cat("\nSkipping EKw gradient comparison.\n")
//' }
//'
//' # Example with Hessian comparison (if hsekw exists)
//' if (mle_result_ekw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) && exists("hsekw")) {
//'
//'   num_hess_ekw <- numDeriv::hessian(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
//'   ana_hess_ekw <- hsekw(par = mle_par_ekw, data = sample_data_ekw)
//'   cat("\nMax absolute difference between EKw Hessians:\n")
//'   print(max(abs(num_hess_ekw - ana_hess_ekw)))
//'
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 3) {
   Rcpp::NumericVector grad(3, R_NaN);
   return grad;
 }

 double alpha = par[0];
 double beta = par[1];
 double lambda = par[2];

 if (alpha <= 0 || beta <= 0 || lambda <= 0) {
   Rcpp::NumericVector grad(3, R_NaN);
   return grad;
 }

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::NumericVector grad(3, R_NaN);
   return grad;
 }

 int n = x.n_elem;
 Rcpp::NumericVector grad(3, 0.0);

 // Constants for numerical stability
 const double min_val = 1e-10;
 const double exp_threshold = -700.0;

 // Initialize component accumulators
 double d_alpha = n / alpha;
 double d_beta = n / beta;
 double d_lambda = n / lambda;

 for (int i = 0; i < n; i++) {
   double xi = x(i);
   double log_xi = std::log(xi);
   d_alpha += log_xi;  // Accumulate (α-1) * log(x_i) term

   // Compute x^α stably (use log domain for large alpha)
   double x_alpha;
   if (alpha > 100.0 || (alpha * log_xi < exp_threshold)) {
     x_alpha = std::exp(alpha * log_xi);
   } else {
     x_alpha = std::pow(xi, alpha);
   }

   // Compute v = 1-x^α with precision for x^α near 1
   double v;
   if (x_alpha > 0.9995) {
     v = -std::expm1(alpha * log_xi);  // More precise than 1.0 - x_alpha
   } else {
     v = 1.0 - x_alpha;
   }

   // Ensure v is not too small
   v = std::max(v, min_val);
   double log_v = std::log(v);
   d_beta += log_v;  // Accumulate (β-1) * log(v_i) term

   // Compute v^β stably
   double v_beta, v_beta_m1;
   if (beta > 100.0 || (beta * log_v < exp_threshold)) {
     double log_v_beta = beta * log_v;
     v_beta = std::exp(log_v_beta);
     v_beta_m1 = std::exp((beta - 1.0) * log_v);
   } else {
     v_beta = std::pow(v, beta);
     v_beta_m1 = std::pow(v, beta - 1.0);
   }

   // Compute w = 1-v^β with precision for v^β near 1
   double w;
   if (v_beta > 0.9995) {
     w = -std::expm1(beta * log_v);
   } else {
     w = 1.0 - v_beta;
   }

   // Ensure w is not too small
   w = std::max(w, min_val);
   double log_w = std::log(w);
   d_lambda += log_w;  // Accumulate (λ-1) * log(w_i) term

   // --- Alpha gradient component ---
   // Calculate x^α * log(x) term
   double x_alpha_log_x = x_alpha * log_xi;

   // Calculate (β-1)/v term - stable for β ≈ 1
   double alpha_term1 = 0.0;
   if (std::abs(beta - 1.0) > 1e-14) {
     alpha_term1 = (beta - 1.0) / v;
   }

   // Calculate (λ-1) * β * v^(β-1) / w term - with λ stability
   double alpha_term2 = 0.0;
   if (std::abs(lambda - 1.0) > 1e-14) {
     double lambda_factor = lambda - 1.0;
     // Clamp the factor for very large lambda to prevent overflow
     if (lambda > 1000.0) {
       lambda_factor = std::min(lambda_factor, 1000.0);
     }
     alpha_term2 = lambda_factor * beta * v_beta_m1 / w;
   }

   d_alpha -= x_alpha_log_x * (alpha_term1 - alpha_term2);

   // --- Beta gradient component ---
   // Calculate v^β * log(v) * (λ-1) / w term - with λ stability
   double beta_term = 0.0;
   if (std::abs(lambda - 1.0) > 1e-14) {
     double lambda_factor = lambda - 1.0;
     // Clamp the factor for very large lambda
     if (lambda > 1000.0) {
       lambda_factor = std::min(lambda_factor, 1000.0);
     }
     beta_term = v_beta * log_v * lambda_factor / w;
   }

   d_beta -= beta_term;
 }

 // Negate for negative log-likelihood
 grad[0] = -d_alpha;
 grad[1] = -d_beta;
 grad[2] = -d_lambda;

 return grad;
}



//' @title Hessian Matrix of the Negative Log-Likelihood for the EKw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian
//'
//' @description
//' Computes the analytic 3x3 Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the Exponentiated Kumaraswamy (EKw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
//' (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}). This distribution is the
//' special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\gamma = 1} and \eqn{\delta = 0}. The Hessian is useful for estimating
//' standard errors and in optimization algorithms.
//'
//' @param par A numeric vector of length 3 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 3x3 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
//'   where \eqn{\theta = (\alpha, \beta, \lambda)}.
//'   Returns a 3x3 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function based on the EKw log-likelihood
//' (\eqn{\gamma=1, \delta=0} case of GKw, see \code{\link{llekw}}):
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\lambda) + \ln(\alpha) + \ln(\beta)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i)]
//' }
//' where \eqn{\theta = (\alpha, \beta, \lambda)} and intermediate terms are:
//' \itemize{
//'   \item \eqn{v_i = 1 - x_i^{\alpha}}
//'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
//' }
//' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}
//' for \eqn{\theta_i, \theta_j \in \{\alpha, \beta, \lambda\}}.
//'
//' Key properties of the returned matrix:
//' \itemize{
//'   \item Dimensions: 3x3.
//'   \item Symmetry: The matrix is symmetric.
//'   \item Ordering: Rows and columns correspond to the parameters in the order
//'     \eqn{\alpha, \beta, \lambda}.
//'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
//' }
//' This corresponds to the relevant 3x3 submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
//' evaluated at \eqn{\gamma=1, \delta=0}. The exact analytical formulas are implemented directly.
//'
//' @references
//' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
//' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
//'
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//' (Note: Specific Hessian formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{hsgkw}} (parent distribution Hessian),
//' \code{\link{llekw}} (negative log-likelihood for EKw),
//' \code{grekw} (gradient for EKw, if available),
//' \code{\link{dekw}} (density for EKw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rekw, llekw, grekw, hsekw functions for EKw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_ekw <- c(alpha = 2, beta = 3, lambda = 0.5)
//' if (exists("rekw")) {
//'   sample_data_ekw <- rekw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
//'                           lambda = true_par_ekw[3])
//' } else {
//'   sample_data_ekw <- rgkw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
//'                          gamma = 1, delta = 0, lambda = true_par_ekw[3])
//' }
//' hist(sample_data_ekw, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_ekw <- c(1.5, 2.5, 0.8)
//' mle_result_ekw <- stats::optim(par = start_par_ekw,
//'                                fn = llekw,
//'                                gr = if (exists("grekw")) grekw else NULL,
//'                                method = "BFGS",
//'                                hessian = TRUE, # Ask optim for numerical Hessian
//'                                data = sample_data_ekw)
//'
//' # --- Compare analytical Hessian to numerical Hessian ---
//' if (mle_result_ekw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("hsekw")) {
//'
//'   mle_par_ekw <- mle_result_ekw$par
//'   cat("\nComparing Hessians for EKw at MLE estimates:\n")
//'
//'   # Numerical Hessian of llekw
//'   num_hess_ekw <- numDeriv::hessian(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
//'
//'   # Analytical Hessian from hsekw
//'   ana_hess_ekw <- hsekw(par = mle_par_ekw, data = sample_data_ekw)
//'
//'   cat("Numerical Hessian (EKw):\n")
//'   print(round(num_hess_ekw, 4))
//'   cat("Analytical Hessian (EKw):\n")
//'   print(round(ana_hess_ekw, 4))
//'
//'   # Check differences
//'   cat("Max absolute difference between EKw Hessians:\n")
//'   print(max(abs(num_hess_ekw - ana_hess_ekw)))
//'
//'   # Optional: Use analytical Hessian for Standard Errors
//'   # tryCatch({
//'   #   cov_matrix_ekw <- solve(ana_hess_ekw)
//'   #   std_errors_ekw <- sqrt(diag(cov_matrix_ekw))
//'   #   cat("Std. Errors from Analytical EKw Hessian:\n")
//'   #   print(std_errors_ekw)
//'   # }, error = function(e) {
//'   #   warning("Could not invert analytical EKw Hessian: ", e$message)
//'   # })
//'
//' } else {
//'   cat("\nSkipping EKw Hessian comparison.\n")
//'   cat("Requires convergence, 'numDeriv' package, and function 'hsekw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hsekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 3) {
   Rcpp::NumericMatrix nanH(3,3);
   nanH.fill(R_NaN);
   return nanH;
 }

 double alpha = par[0];
 double beta = par[1];
 double lambda = par[2];

 if (alpha <= 0 || beta <= 0 || lambda <= 0) {
   Rcpp::NumericMatrix nanH(3,3);
   nanH.fill(R_NaN);
   return nanH;
 }

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::NumericMatrix nanH(3,3);
   nanH.fill(R_NaN);
   return nanH;
 }

 int n = x.n_elem;
 arma::mat H(3,3, arma::fill::zeros);

 // Stability constants
 // const double eps = std::numeric_limits<double>::epsilon() * 100;
 const double min_v = 1e-10;  // Minimum value for v = 1-x^α
 const double min_w = 1e-10;  // Minimum value for w = 1-(1-x^α)^β
 const double exp_threshold = -700.0;  // Threshold for log-domain calculations

 // Constant terms (diagonal elements)
 H(0,0) = -n / (alpha * alpha);  // -n/α²
 H(1,1) = -n / (beta * beta);    // -n/β²
 H(2,2) = -n / (lambda * lambda); // -n/λ²

 // Special handling for lambda near 1 (critical case for stability)
 bool lambda_near_one = std::abs(lambda - 1.0) < 1e-8;

 for (int i = 0; i < n; i++) {
   double xi = x(i);
   double log_xi = std::log(xi);

   // Calculate x^α (A) and derivatives with log-domain for large alpha
   double A, dA_dalpha, d2A_dalpha2;
   if (alpha > 100.0 || (alpha * log_xi < exp_threshold)) {
     double log_A = alpha * log_xi;
     A = std::exp(log_A);
     dA_dalpha = A * log_xi;
     d2A_dalpha2 = A * log_xi * log_xi;
   } else {
     A = std::pow(xi, alpha);
     dA_dalpha = A * log_xi;
     d2A_dalpha2 = A * log_xi * log_xi;
   }

   // Calculate v = 1 - x^α with precision for x^α near 1
   double v;
   if (A > 0.9995) {
     v = -std::expm1(alpha * log_xi);  // More precise than 1.0 - A
   } else {
     v = 1.0 - A;
   }

   // Ensure v is not too small
   v = std::max(v, min_v);
   double log_v = std::log(v);

   double dv_dalpha = -dA_dalpha;
   double d2v_dalpha2 = -d2A_dalpha2;

   // L5 derivatives: (β-1) log(v)
   double d2L5_dalpha2 = 0.0;
   double d2L5_dalpha_dbeta = 0.0;

   if (beta != 1.0) {
     double v_squared = std::max(v * v, 1e-200); // Prevent division by zero
     d2L5_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / v_squared);
     d2L5_dalpha_dbeta = dv_dalpha / v;
   }

   // Calculate v^β with log-domain for large beta
   double v_beta, v_beta_m1, v_beta_m2;
   if (beta > 100.0 || (beta * log_v < exp_threshold)) {
     v_beta = std::exp(beta * log_v);
     v_beta_m1 = std::exp((beta - 1.0) * log_v);
     v_beta_m2 = std::exp((beta - 2.0) * log_v);
   } else {
     v_beta = std::pow(v, beta);
     v_beta_m1 = std::pow(v, beta - 1.0);
     v_beta_m2 = std::pow(v, beta - 2.0);
   }

   // Calculate w = 1 - v^β precisely for v^β near 1
   double w;
   if (v_beta > 0.9995) {
     w = -std::expm1(beta * log_v);
   } else {
     w = 1.0 - v_beta;
   }

   w = std::max(w, min_w);
   double w_squared = std::max(w * w, 1e-200); // Prevent division by zero

   // First derivatives of w
   double dw_dv = -beta * v_beta_m1;
   double dw_dalpha = dw_dv * dv_dalpha;
   double dw_dbeta = -v_beta * log_v;

   // Second derivatives of w
   double d2w_dalpha2 = -beta * ((beta - 1.0) * v_beta_m2 * (dv_dalpha * dv_dalpha) +
                                 v_beta_m1 * d2v_dalpha2);
   double d2w_dbeta2 = -v_beta * (log_v * log_v);
   double d_dw_dalpha_dbeta = -v_beta_m1 * (1.0 + beta * log_v) * dv_dalpha;

   // L6 derivatives: (λ-1) log(w)
   double d2L6_dalpha2 = 0.0;
   double d2L6_dbeta2 = 0.0;
   double d2L6_dalpha_dbeta = 0.0;
   double d2L6_dalpha_dlambda = 0.0;
   double d2L6_dbeta_dlambda = 0.0;

   // Critical lambda handling for stability
   if (lambda_near_one) {
     // For lambda ≈ 1, handle carefully to avoid cancellation errors
     if (std::abs(lambda - 1.0) > 1e-15) {
       double factor = lambda - 1.0;
       d2L6_dalpha2 = factor * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / w_squared);
       d2L6_dbeta2 = factor * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta)) / w_squared);
       d2L6_dalpha_dbeta = factor * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta) / w_squared);
     }
     // When λ = 1 (machine precision), these terms become zero
     d2L6_dalpha_dlambda = dw_dalpha / w;
     d2L6_dbeta_dlambda = dw_dbeta / w;
   } else {
     // Standard case
     d2L6_dalpha2 = (lambda - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / w_squared);
     d2L6_dbeta2 = (lambda - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta)) / w_squared);
     d2L6_dalpha_dbeta = (lambda - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta) / w_squared);
     d2L6_dalpha_dlambda = dw_dalpha / w;
     d2L6_dbeta_dlambda = dw_dbeta / w;
   }

   // Handle large lambda values (> 1000)
   if (lambda > 1000.0) {
     double max_val = 1000.0;
     d2L6_dalpha2 = std::min(std::max(d2L6_dalpha2, -max_val), max_val);
     d2L6_dbeta2 = std::min(std::max(d2L6_dbeta2, -max_val), max_val);
     d2L6_dalpha_dbeta = std::min(std::max(d2L6_dalpha_dbeta, -max_val), max_val);
   }

   // Accumulate contributions to Hessian
   H(0,0) += d2L5_dalpha2 + d2L6_dalpha2;
   H(0,1) += d2L5_dalpha_dbeta + d2L6_dalpha_dbeta;
   H(1,0) = H(0,1);
   H(1,1) += d2L6_dbeta2;
   H(0,2) += d2L6_dalpha_dlambda;
   H(2,0) = H(0,2);
   H(1,2) += d2L6_dbeta_dlambda;
   H(2,1) = H(1,2);
 }

 // Ensure perfect symmetry by averaging
 for (int i = 0; i < 3; i++) {
   for (int j = i+1; j < 3; j++) {
     double avg = (H(i,j) + H(j,i)) / 2.0;
     H(i,j) = H(j,i) = avg;
   }
 }

 return Rcpp::wrap(-H);
}

// //' @title Negative Log-Likelihood for the Exponentiated Kumaraswamy (EKw) Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize
// //'
// //' @description
// //' Computes the negative log-likelihood function for the Exponentiated
// //' Kumaraswamy (EKw) distribution with parameters \code{alpha} (\eqn{\alpha}),
// //' \code{beta} (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}), given a vector
// //' of observations. This distribution is the special case of the Generalized
// //' Kumaraswamy (GKw) distribution where \eqn{\gamma = 1} and \eqn{\delta = 0}.
// //' This function is suitable for maximum likelihood estimation.
// //'
// //' @param par A numeric vector of length 3 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a single \code{double} value representing the negative
// //'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
// //'   if any parameter values in \code{par} are invalid according to their
// //'   constraints, or if any value in \code{data} is not in the interval (0, 1).
// //'
// //' @details
// //' The Exponentiated Kumaraswamy (EKw) distribution is the GKw distribution
// //' (\code{\link{dekw}}) with \eqn{\gamma=1} and \eqn{\delta=0}. Its probability
// //' density function (PDF) is:
// //' \deqn{
// //' f(x | \theta) = \lambda \alpha \beta x^{\alpha-1} (1 - x^\alpha)^{\beta-1} \bigl[1 - (1 - x^\alpha)^\beta \bigr]^{\lambda - 1}
// //' }
// //' for \eqn{0 < x < 1} and \eqn{\theta = (\alpha, \beta, \lambda)}.
// //' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
// //' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\lambda) + \ln(\alpha) + \ln(\beta)]
// //' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i)]
// //' }
// //' where:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //' }
// //' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
// //' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
// //' Numerical stability is maintained similarly to \code{\link{llgkw}}.
// //'
// //' @references
// //' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
// //' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
// //'
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //'
// //' @seealso
// //' \code{\link{llgkw}} (parent distribution negative log-likelihood),
// //' \code{\link{dekw}}, \code{\link{pekw}}, \code{\link{qekw}}, \code{\link{rekw}},
// //' \code{grekw} (gradient, if available),
// //' \code{hsekw} (Hessian, if available),
// //' \code{\link[stats]{optim}}
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rekw, grekw, hsekw functions for EKw distribution
// //'
// //' # Generate sample data from a known EKw distribution
// //' set.seed(123)
// //' true_par_ekw <- c(alpha = 2, beta = 3, lambda = 0.5)
// //' # Use rekw if it exists, otherwise use rgkw with gamma=1, delta=0
// //' if (exists("rekw")) {
// //'   sample_data_ekw <- rekw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
// //'                           lambda = true_par_ekw[3])
// //' } else {
// //'   sample_data_ekw <- rgkw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
// //'                          gamma = 1, delta = 0, lambda = true_par_ekw[3])
// //' }
// //' hist(sample_data_ekw, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
// //'
// //' # --- Maximum Likelihood Estimation using optim ---
// //' # Initial parameter guess
// //' start_par_ekw <- c(1.5, 2.5, 0.8)
// //'
// //' # Perform optimization (minimizing negative log-likelihood)
// //' # Use method="L-BFGS-B" for box constraints if needed (all params > 0)
// //' mle_result_ekw <- stats::optim(par = start_par_ekw,
// //'                                fn = llekw, # Use the EKw neg-log-likelihood
// //'                                method = "BFGS", # Or "L-BFGS-B" with lower=1e-6
// //'                                hessian = TRUE,
// //'                                data = sample_data_ekw)
// //'
// //' # Check convergence and results
// //' if (mle_result_ekw$convergence == 0) {
// //'   print("Optimization converged successfully.")
// //'   mle_par_ekw <- mle_result_ekw$par
// //'   print("Estimated EKw parameters:")
// //'   print(mle_par_ekw)
// //'   print("True EKw parameters:")
// //'   print(true_par_ekw)
// //' } else {
// //'   warning("Optimization did not converge!")
// //'   print(mle_result_ekw$message)
// //' }
// //'
// //' # --- Compare numerical and analytical derivatives (if available) ---
// //' # Requires 'numDeriv' package and analytical functions 'grekw', 'hsekw'
// //' if (mle_result_ekw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("grekw") && exists("hsekw")) {
// //'
// //'   cat("\nComparing Derivatives at EKw MLE estimates:\n")
// //'
// //'   # Numerical derivatives of llekw
// //'   num_grad_ekw <- numDeriv::grad(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
// //'   num_hess_ekw <- numDeriv::hessian(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
// //'
// //'   # Analytical derivatives (assuming they return derivatives of negative LL)
// //'   ana_grad_ekw <- grekw(par = mle_par_ekw, data = sample_data_ekw)
// //'   ana_hess_ekw <- hsekw(par = mle_par_ekw, data = sample_data_ekw)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between gradients:\n")
// //'   print(max(abs(num_grad_ekw - ana_grad_ekw)))
// //'   cat("Max absolute difference between Hessians:\n")
// //'   print(max(abs(num_hess_ekw - ana_hess_ekw)))
// //'
// //' } else {
// //'    cat("\nSkipping derivative comparison for EKw.\n")
// //'    cat("Requires convergence, 'numDeriv' package and functions 'grekw', 'hsekw'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// double llekw(const Rcpp::NumericVector& par,
//             const Rcpp::NumericVector& data) {
//  if (par.size() < 3) {
//    return R_PosInf; // no param
//  }
//  double a = par[0];
//  double b = par[1];
//  double l = par[2];
//
//  if (!check_ekw_pars(a, b, l)) {
//    return R_PosInf; // invalid => +Inf
//  }
//
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if (x.n_elem < 1) {
//    return R_PosInf;
//  }
//  // check data
//  if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
//    return R_PosInf;
//  }
//
//  int n = x.n_elem;
//  // constant part: n* [ log(l) + log(a) + log(b) ]
//  double cst = n*( std::log(l) + std::log(a) + std::log(b) );
//
//  // sum( (a-1)* log(x_i ) )
//  arma::vec lx = arma::log(x);
//  double sum1 = (a-1.0)* arma::sum(lx);
//
//  // sum( (b-1)* log(1- x^a ) )
//  arma::vec xalpha = arma::pow(x, a);
//  arma::vec log_1_xalpha = arma::log(1.0 - xalpha);
//  double sum2 = (b-1.0)* arma::sum(log_1_xalpha);
//
//  // sum( (l-1)* log(1- (1- x^a)^b ) )
//  arma::vec vbeta = arma::pow((1.0 - xalpha), b);
//  arma::vec one_minus_vbeta = 1.0 - vbeta;
//  arma::vec log_1_mv = arma::log(one_minus_vbeta);
//  double sum3 = (l-1.0)* arma::sum(log_1_mv);
//
//  double loglike = cst + sum1 + sum2 + sum3;
//  // negative
//  return -loglike;
// }
//
// // -----------------------------------------------------------------------------
// // 6) grekw: Gradient of Negative Log-Likelihood of EKw
// // -----------------------------------------------------------------------------
//
// //' @title Gradient of the Negative Log-Likelihood for the EKw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize gradient
// //'
// //' @description
// //' Computes the gradient vector (vector of first partial derivatives) of the
// //' negative log-likelihood function for the Exponentiated Kumaraswamy (EKw)
// //' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}). This distribution is the
// //' special case of the Generalized Kumaraswamy (GKw) distribution where
// //' \eqn{\gamma = 1} and \eqn{\delta = 0}. The gradient is useful for optimization.
// //'
// //' @param par A numeric vector of length 3 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a numeric vector of length 3 containing the partial derivatives
// //'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
// //'   respect to each parameter: \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta, -\partial \ell/\partial \lambda)}.
// //'   Returns a vector of \code{NaN} if any parameter values are invalid according
// //'   to their constraints, or if any value in \code{data} is not in the
// //'   interval (0, 1).
// //'
// //' @details
// //' The components of the gradient vector of the negative log-likelihood
// //' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the EKw (\eqn{\gamma=1, \delta=0})
// //' model are:
// //'
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
// //' + \sum_{i=1}^{n}\left[x_i^{\alpha} \ln(x_i) \left(\frac{\beta-1}{v_i} -
// //' \frac{(\lambda-1) \beta v_i^{\beta-1}}{w_i}\right)\right]
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - \sum_{i=1}^{n}\ln(v_i)
// //' + \sum_{i=1}^{n}\left[\frac{(\lambda-1) v_i^{\beta} \ln(v_i)}{w_i}\right]
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} - \sum_{i=1}^{n}\ln(w_i)
// //' }
// //'
// //' where:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //' }
// //' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
// //' minimizing the negative log-likelihood. They correspond to the relevant components
// //' of the general GKw gradient (\code{\link{grgkw}}) evaluated at \eqn{\gamma=1, \delta=0}.
// //'
// //' @references
// //' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
// //' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
// //'
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //' (Note: Specific gradient formulas might be derived or sourced from additional references).
// //'
// //' @seealso
// //' \code{\link{grgkw}} (parent distribution gradient),
// //' \code{\link{llekw}} (negative log-likelihood for EKw),
// //' \code{hsekw} (Hessian for EKw, if available),
// //' \code{\link{dekw}} (density for EKw),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{grad}} (for numerical gradient comparison).
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rekw, llekw, grekw, hsekw functions for EKw
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_ekw <- c(alpha = 2, beta = 3, lambda = 0.5)
// //' if (exists("rekw")) {
// //'   sample_data_ekw <- rekw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
// //'                           lambda = true_par_ekw[3])
// //' } else {
// //'   sample_data_ekw <- rgkw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
// //'                           gamma = 1, delta = 0, lambda = true_par_ekw[3])
// //' }
// //' hist(sample_data_ekw, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
// //'
// //' # --- Find MLE estimates ---
// //' start_par_ekw <- c(1.5, 2.5, 0.8)
// //' mle_result_ekw <- stats::optim(par = start_par_ekw,
// //'                                fn = llekw,
// //'                                gr = grekw, # Use analytical gradient for EKw
// //'                                method = "BFGS",
// //'                                hessian = TRUE,
// //'                                data = sample_data_ekw)
// //'
// //' # --- Compare analytical gradient to numerical gradient ---
// //' if (mle_result_ekw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE)) {
// //'
// //'   mle_par_ekw <- mle_result_ekw$par
// //'   cat("\nComparing Gradients for EKw at MLE estimates:\n")
// //'
// //'   # Numerical gradient of llekw
// //'   num_grad_ekw <- numDeriv::grad(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
// //'
// //'   # Analytical gradient from grekw
// //'   ana_grad_ekw <- grekw(par = mle_par_ekw, data = sample_data_ekw)
// //'
// //'   cat("Numerical Gradient (EKw):\n")
// //'   print(num_grad_ekw)
// //'   cat("Analytical Gradient (EKw):\n")
// //'   print(ana_grad_ekw)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between EKw gradients:\n")
// //'   print(max(abs(num_grad_ekw - ana_grad_ekw)))
// //'
// //' } else {
// //'   cat("\nSkipping EKw gradient comparison.\n")
// //' }
// //'
// //' # Example with Hessian comparison (if hsekw exists)
// //' if (mle_result_ekw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) && exists("hsekw")) {
// //'
// //'   num_hess_ekw <- numDeriv::hessian(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
// //'   ana_hess_ekw <- hsekw(par = mle_par_ekw, data = sample_data_ekw)
// //'   cat("\nMax absolute difference between EKw Hessians:\n")
// //'   print(max(abs(num_hess_ekw - ana_hess_ekw)))
// //'
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericVector grekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double alpha = par[0];   // Shape parameter α > 0
//  double beta = par[1];    // Shape parameter β > 0
//  double lambda = par[2];  // Shape parameter λ > 0
//
//  // Parameter validation
//  if (alpha <= 0 || beta <= 0 || lambda <= 0) {
//    Rcpp::NumericVector grad(3, R_NaN);
//    return grad;
//  }
//
//  // Data conversion and validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//
//  if (arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericVector grad(3, R_NaN);
//    return grad;
//  }
//
//  int n = x.n_elem;  // Sample size
//
//  // Initialize gradient vector
//  Rcpp::NumericVector grad(3, 0.0);
//
//  // Small constant to avoid numerical issues
//  double eps = std::numeric_limits<double>::epsilon() * 100;
//
//  // Compute transformations and intermediate values
//  arma::vec log_x = arma::log(x);                // log(x_i)
//  arma::vec x_alpha = arma::pow(x, alpha);       // x_i^α
//  arma::vec x_alpha_log_x = x_alpha % log_x;     // x_i^α * log(x_i)
//
//  // v_i = 1 - x_i^α
//  arma::vec v = 1.0 - x_alpha;
//  v = arma::clamp(v, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_v = arma::log(v);                // log(v_i)
//  arma::vec v_beta_m1 = arma::pow(v, beta - 1.0); // v_i^(β-1)
//  arma::vec v_beta = arma::pow(v, beta);          // v_i^β
//  arma::vec v_beta_log_v = v_beta % log_v;        // v_i^β * log(v_i)
//
//  // w_i = 1 - v_i^β = 1 - (1-x_i^α)^β
//  arma::vec w = 1.0 - v_beta;
//  w = arma::clamp(w, eps, 1.0 - eps);            // Prevent numerical issues
//
//  arma::vec log_w = arma::log(w);                // log(w_i)
//
//  // Calculate partial derivatives for each parameter (for log-likelihood)
//
//  // ∂ℓ/∂α = n/α + Σᵢlog(xᵢ) - Σᵢ[xᵢ^α * log(xᵢ) * ((β-1)/vᵢ - (λ-1) * β * vᵢ^(β-1) / wᵢ)]
//  double d_alpha = n / alpha + arma::sum(log_x);
//
//  // Calculate the complex term in the α gradient
//  arma::vec alpha_term1 = (beta - 1.0) / v;                       // (β-1)/v_i
//  arma::vec alpha_term2 = (lambda - 1.0) * beta * v_beta_m1 / w;  // (λ-1) * β * v_i^(β-1) / w_i
//
//  d_alpha -= arma::sum(x_alpha_log_x % (alpha_term1 - alpha_term2));
//
//  // ∂ℓ/∂β = n/β + Σᵢlog(vᵢ) - Σᵢ[vᵢ^β * log(vᵢ) * ((λ-1) / wᵢ)]
//  double d_beta = n / beta + arma::sum(log_v);
//
//  // Calculate the term in the β gradient
//  arma::vec beta_term = (lambda - 1.0) / w;       // (λ-1) / w_i
//
//  d_beta -= arma::sum(v_beta_log_v % beta_term);
//
//  // ∂ℓ/∂λ = n/λ + Σᵢlog(wᵢ)
//  double d_lambda = n / lambda + arma::sum(log_w);
//
//  // Since we're optimizing negative log-likelihood, negate all derivatives
//  grad[0] = -d_alpha;
//  grad[1] = -d_beta;
//  grad[2] = -d_lambda;
//
//  return grad;
// }
//
//
// //' @title Hessian Matrix of the Negative Log-Likelihood for the EKw Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize hessian
// //'
// //' @description
// //' Computes the analytic 3x3 Hessian matrix (matrix of second partial derivatives)
// //' of the negative log-likelihood function for the Exponentiated Kumaraswamy (EKw)
// //' distribution with parameters \code{alpha} (\eqn{\alpha}), \code{beta}
// //' (\eqn{\beta}), and \code{lambda} (\eqn{\lambda}). This distribution is the
// //' special case of the Generalized Kumaraswamy (GKw) distribution where
// //' \eqn{\gamma = 1} and \eqn{\delta = 0}. The Hessian is useful for estimating
// //' standard errors and in optimization algorithms.
// //'
// //' @param par A numeric vector of length 3 containing the distribution parameters
// //'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}),
// //'   \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a 3x3 numeric matrix representing the Hessian matrix of the
// //'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
// //'   where \eqn{\theta = (\alpha, \beta, \lambda)}.
// //'   Returns a 3x3 matrix populated with \code{NaN} if any parameter values are
// //'   invalid according to their constraints, or if any value in \code{data} is
// //'   not in the interval (0, 1).
// //'
// //' @details
// //' This function calculates the analytic second partial derivatives of the
// //' negative log-likelihood function based on the EKw log-likelihood
// //' (\eqn{\gamma=1, \delta=0} case of GKw, see \code{\link{llekw}}):
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\lambda) + \ln(\alpha) + \ln(\beta)]
// //' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i) + (\lambda-1)\ln(w_i)]
// //' }
// //' where \eqn{\theta = (\alpha, \beta, \lambda)} and intermediate terms are:
// //' \itemize{
// //'   \item \eqn{v_i = 1 - x_i^{\alpha}}
// //'   \item \eqn{w_i = 1 - v_i^{\beta} = 1 - (1-x_i^{\alpha})^{\beta}}
// //' }
// //' The Hessian matrix returned contains the elements \eqn{- \frac{\partial^2 \ell(\theta | \mathbf{x})}{\partial \theta_i \partial \theta_j}}
// //' for \eqn{\theta_i, \theta_j \in \{\alpha, \beta, \lambda\}}.
// //'
// //' Key properties of the returned matrix:
// //' \itemize{
// //'   \item Dimensions: 3x3.
// //'   \item Symmetry: The matrix is symmetric.
// //'   \item Ordering: Rows and columns correspond to the parameters in the order
// //'     \eqn{\alpha, \beta, \lambda}.
// //'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
// //' }
// //' This corresponds to the relevant 3x3 submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
// //' evaluated at \eqn{\gamma=1, \delta=0}. The exact analytical formulas are implemented directly.
// //'
// //' @references
// //' Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2012). The exponentiated
// //' Kumaraswamy distribution. *Journal of the Franklin Institute*, *349*(3),
// //'
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //' (Note: Specific Hessian formulas might be derived or sourced from additional references).
// //'
// //' @seealso
// //' \code{\link{hsgkw}} (parent distribution Hessian),
// //' \code{\link{llekw}} (negative log-likelihood for EKw),
// //' \code{grekw} (gradient for EKw, if available),
// //' \code{\link{dekw}} (density for EKw),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rekw, llekw, grekw, hsekw functions for EKw
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_ekw <- c(alpha = 2, beta = 3, lambda = 0.5)
// //' if (exists("rekw")) {
// //'   sample_data_ekw <- rekw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
// //'                           lambda = true_par_ekw[3])
// //' } else {
// //'   sample_data_ekw <- rgkw(100, alpha = true_par_ekw[1], beta = true_par_ekw[2],
// //'                          gamma = 1, delta = 0, lambda = true_par_ekw[3])
// //' }
// //' hist(sample_data_ekw, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
// //'
// //' # --- Find MLE estimates ---
// //' start_par_ekw <- c(1.5, 2.5, 0.8)
// //' mle_result_ekw <- stats::optim(par = start_par_ekw,
// //'                                fn = llekw,
// //'                                gr = if (exists("grekw")) grekw else NULL,
// //'                                method = "BFGS",
// //'                                hessian = TRUE, # Ask optim for numerical Hessian
// //'                                data = sample_data_ekw)
// //'
// //' # --- Compare analytical Hessian to numerical Hessian ---
// //' if (mle_result_ekw$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("hsekw")) {
// //'
// //'   mle_par_ekw <- mle_result_ekw$par
// //'   cat("\nComparing Hessians for EKw at MLE estimates:\n")
// //'
// //'   # Numerical Hessian of llekw
// //'   num_hess_ekw <- numDeriv::hessian(func = llekw, x = mle_par_ekw, data = sample_data_ekw)
// //'
// //'   # Analytical Hessian from hsekw
// //'   ana_hess_ekw <- hsekw(par = mle_par_ekw, data = sample_data_ekw)
// //'
// //'   cat("Numerical Hessian (EKw):\n")
// //'   print(round(num_hess_ekw, 4))
// //'   cat("Analytical Hessian (EKw):\n")
// //'   print(round(ana_hess_ekw, 4))
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between EKw Hessians:\n")
// //'   print(max(abs(num_hess_ekw - ana_hess_ekw)))
// //'
// //'   # Optional: Use analytical Hessian for Standard Errors
// //'   # tryCatch({
// //'   #   cov_matrix_ekw <- solve(ana_hess_ekw)
// //'   #   std_errors_ekw <- sqrt(diag(cov_matrix_ekw))
// //'   #   cat("Std. Errors from Analytical EKw Hessian:\n")
// //'   #   print(std_errors_ekw)
// //'   # }, error = function(e) {
// //'   #   warning("Could not invert analytical EKw Hessian: ", e$message)
// //'   # })
// //'
// //' } else {
// //'   cat("\nSkipping EKw Hessian comparison.\n")
// //'   cat("Requires convergence, 'numDeriv' package, and function 'hsekw'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hsekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction - EKw has only 3 parameters
//  double alpha  = par[0];   // θ[0] = α
//  double beta   = par[1];   // θ[1] = β
//  double lambda = par[2];   // θ[2] = λ
//
//  // Simple parameter validation (all > 0)
//  if(alpha <= 0 || beta <= 0 || lambda <= 0) {
//    Rcpp::NumericMatrix nanH(3,3);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  // Data conversion and basic validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if(arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericMatrix nanH(3,3);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  int n = x.n_elem;  // sample size
//
//  // Initialize Hessian matrix H (of ℓ(θ)) as 3x3
//  arma::mat H(3,3, arma::fill::zeros);
//
//  // --- CONSTANT TERMS (do not depend on x) ---
//  // L1: n ln(λ)  => d²/dλ² = -n/λ²
//  H(2,2) += - n/(lambda*lambda);
//  // L2: n ln(α)  => d²/dα² = -n/α²
//  H(0,0) += - n/(alpha*alpha);
//  // L3: n ln(β)  => d²/dβ² = -n/β²
//  H(1,1) += - n/(beta*beta);
//
//  // --- TERMS THAT INVOLVE THE OBSERVATIONS ---
//  // Loop over each observation to accumulate contributions from:
//  // L4: (α-1) Σ ln(x_i) --> contributes only to first derivatives
//  // L5: (β-1) Σ ln(v), where v = 1 - x^α
//  // L6: (λ-1) Σ ln(w), where w = 1 - v^β
//  for (int i = 0; i < n; i++) {
//    double xi    = x(i);
//    double ln_xi = std::log(xi);
//
//    // -- Compute A = x^α and its derivatives --
//    double A = std::pow(xi, alpha);                  // A = x^α
//    double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
//    double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²
//
//    // -- v = 1 - A and its derivatives --
//    double v = 1.0 - A;                              // v = 1 - x^α
//    double ln_v = std::log(v);                       // ln(v)
//    double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
//    double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²
//
//    // --- L5: (β-1) ln(v) ---
//    // Second derivative w.r.t. α: (β-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
//    double d2L5_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
//    // Mixed derivative: d²L5/(dα dβ) = d/dβ[(β-1)*(dv_dalpha/v)] = (dv_dalpha/v)
//    double d2L5_dalpha_dbeta = dv_dalpha / v;
//
//    // --- L6: (λ - 1) ln(w), where w = 1 - v^β ---
//    double v_beta = std::pow(v, beta);               // v^β
//    double w = 1.0 - v_beta;                         // w = 1 - v^β
//
//    // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
//    double dw_dv = -beta * std::pow(v, beta - 1.0);
//    // Chain rule: dw/dα = dw/dv * dv/dα
//    double dw_dalpha = dw_dv * dv_dalpha;
//    // Second derivative w.r.t. α for L6:
//    // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
//    // Computing d²w/dα²:
//    //   dw/dα = -β * v^(β-1)*dv_dalpha,
//    //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
//    double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
//                                    + std::pow(v, beta-1.0) * d2v_dalpha2);
//    double d2L6_dalpha2 = (lambda - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
//    // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
//    double dw_dbeta = -v_beta * ln_v;
//    // Second derivative w.r.t. β for L6:
//    // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
//    double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
//    double d2L6_dbeta2 = (lambda - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
//    // Mixed derivative L6 (α,β): d²/(dα dβ) ln(w) =
//    //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
//    // Approximate d/dβ dw_dalpha:
//    double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
//    double d2L6_dalpha_dbeta = (lambda - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));
//
//    // Mixed derivatives with λ
//    // (α,λ): d²/(dα dλ) [λ ln(w)] = d/dλ[(λ-1)(dw_dalpha/w)] = dw_dalpha/w
//    double d2L6_dalpha_dlambda = dw_dalpha / w;
//
//    // (β,λ): d²/(dβ dλ) [λ ln(w)] = d/dλ[(λ-1)(dw_dbeta/w)] = dw_dbeta/w
//    double d2L6_dbeta_dlambda = dw_dbeta / w;
//
//    // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
//    // Index: 0 = α, 1 = β, 2 = λ
//
//    // H(α,α): sum of L2, L5, and L6 (constants already added)
//    H(0,0) += d2L5_dalpha2 + d2L6_dalpha2;
//
//    // H(α,β): mixed from L5 and L6
//    H(0,1) += d2L5_dalpha_dbeta + d2L6_dalpha_dbeta;
//    H(1,0) = H(0,1);
//
//    // H(β,β): contributions from L3 and L6
//    H(1,1) += d2L6_dbeta2;
//
//    // H(α,λ): mixed derivative from L6
//    H(0,2) += d2L6_dalpha_dlambda;
//    H(2,0) = H(0,2);
//
//    // H(β,λ): mixed derivative from L6
//    H(1,2) += d2L6_dbeta_dlambda;
//    H(2,1) = H(1,2);
//
//  } // end of loop
//
//  // Returns the analytic Hessian matrix of the negative log-likelihood
//  return Rcpp::wrap(-H);
// }



// //' @title Analytic Hessian Matrix for Exponentiated Kumaraswamy Distribution
// //'
// //' @description
// //' Computes the analytic Hessian matrix of the log-likelihood function for
// //' the Exponentiated Kumaraswamy (EKw) distribution. This function provides
// //' exact second derivatives needed for optimization and inference.
// //'
// //' @param par Numeric vector of length 3 containing the parameters
// //'        (α, β, λ) in that order. All parameters must be positive.
// //' @param data Numeric vector of observations, where all values must be
// //'        in the open interval (0,1).
// //'
// //' @return A 3×3 numeric matrix representing the Hessian of the negative
// //'         log-likelihood function. If parameters or data are invalid
// //'         (parameters ≤ 0 or data outside (0,1)), returns a matrix of
// //'         NaN values.
// //'
// //' @details
// //' The log-likelihood for the Exponentiated Kumaraswamy distribution is:
// //'
// //' \deqn{
// //' \ell(\theta) = n \ln(\lambda) + n \ln(\alpha) + n \ln(\beta)
// //' + (\alpha-1) \sum \ln(x_i)
// //' + (\beta-1) \sum \ln(1 - x_i^\alpha)
// //' + (\lambda-1) \sum \ln\{1 - (1 - x_i^\alpha)^\beta\}
// //' }
// //'
// //' The implementation computes all second derivatives analytically for each term.
// //' For computational efficiency, the following transformations are used:
// //' \itemize{
// //'   \item \deqn{A = x^α} and derivatives
// //'   \item \deqn{v = 1 - A}
// //'   \item \deqn{w = 1 - v^β}
// //' }
// //'
// //' The returned Hessian matrix has the following structure:
// //' \itemize{
// //'   \item Rows/columns 1-3 correspond to α, β, λ respectively
// //'   \item The matrix is symmetric (as expected for a Hessian)
// //'   \item The matrix represents second derivatives of the negative log-likelihood
// //' }
// //'
// //' This function is implemented in C++ for computational efficiency.
// //'
// //' @examples
// //' \donttest{
// //' # Generate sample data from an EKw distribution
// //' set.seed(123)
// //' x <- rekw(100, 2, 3, 0.5)
// //' hist(x, breaks = 20, main = "EKw(2, 3, 0.5) Sample")
// //'
// //' # Use in optimization with Hessian-based methods
// //' result <- optim(c(0.5, 0.5, 0.5), llekw, method = "BFGS",
// //'                 hessian = TRUE, data = x)
// //'
// //' # Compare numerical and analytical derivatives
// //' num_grad <- numDeriv::grad(llekw, x = result$par, data = x)
// //' num_hess <- numDeriv::hessian(llekw, x = result$par, data = x)
// //'
// //' ana_grad <- grekw(result$par, data = x)
// //' ana_hess <- hsekw(result$par, data = x)
// //'
// //' # Check differences (should be very small)
// //' round(num_grad - ana_grad, 4)
// //' round(num_hess - ana_hess, 4)
// //'
// //' }
// //'
// //'
// //' @references
// //' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes.
// //' Journal of Hydrology, 46(1-2), 79-88.
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
// //' Journal of Statistical Computation and Simulation, 81(7), 883-898.
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hsekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction - EKw has only 3 parameters
//  double alpha  = par[0];   // θ[0] = α
//  double beta   = par[1];   // θ[1] = β
//  double lambda = par[2];   // θ[2] = λ
//
//  // Simple parameter validation (all > 0)
//  if(alpha <= 0 || beta <= 0 || lambda <= 0) {
//    Rcpp::NumericMatrix nanH(3,3);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  // Data conversion and basic validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if(arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericMatrix nanH(3,3);
//    nanH.fill(R_NaN);
//    return nanH;
//  }
//
//  int n = x.n_elem;  // sample size
//
//  // Initialize Hessian matrix H (of ℓ(θ)) as 3x3
//  arma::mat H(3,3, arma::fill::zeros);
//
//  // --- CONSTANT TERMS (do not depend on x) ---
//  // L1: n ln(λ)  => d²/dλ² = -n/λ²
//  H(2,2) += - n/(lambda*lambda);
//  // L2: n ln(α)  => d²/dα² = -n/α²
//  H(0,0) += - n/(alpha*alpha);
//  // L3: n ln(β)  => d²/dβ² = -n/β²
//  H(1,1) += - n/(beta*beta);
//
//  // --- TERMS THAT INVOLVE THE OBSERVATIONS ---
//  // Loop over each observation to accumulate contributions from:
//  // L4: (α-1) Σ ln(x_i) --> contributes only to first derivatives
//  // L5: (β-1) Σ ln(v), where v = 1 - x^α
//  // L6: (λ-1) Σ ln(w), where w = 1 - v^β
//  for (int i = 0; i < n; i++) {
//    double xi    = x(i);
//    double ln_xi = std::log(xi);
//
//    // -- Compute A = x^α and its derivatives --
//    double A = std::pow(xi, alpha);                  // A = x^α
//    double dA_dalpha = A * ln_xi;                    // dA/dα = x^α ln(x)
//    double d2A_dalpha2 = A * ln_xi * ln_xi;          // d²A/dα² = x^α (ln(x))²
//
//    // -- v = 1 - A and its derivatives --
//    double v = 1.0 - A;                              // v = 1 - x^α
//    double ln_v = std::log(v);                       // ln(v)
//    double dv_dalpha = -dA_dalpha;                   // dv/dα = -dA/dα = -x^α ln(x)
//    double d2v_dalpha2 = -d2A_dalpha2;               // d²v/dα² = -d²A/dα² = -x^α (ln(x))²
//
//    // --- L5: (β-1) ln(v) ---
//    // First derivative w.r.t. α: (β-1) * (1/v)*dv_dalpha
//    double dL5_dalpha = (beta - 1.0) * (dv_dalpha / v);
//    // Second derivative w.r.t. α: (β-1)*[(d²v/dα²*v - (dv/dα)²)/v²]
//    double d2L5_dalpha2 = (beta - 1.0) * ((d2v_dalpha2 * v - dv_dalpha * dv_dalpha) / (v*v));
//    // Mixed derivative: d²L5/(dα dβ) = d/dβ[(β-1)*(dv_dalpha/v)] = (dv_dalpha/v)
//    double d2L5_dalpha_dbeta = dv_dalpha / v;
//
//    // --- L6: (λ - 1) ln(w), where w = 1 - v^β ---
//    double v_beta = std::pow(v, beta);               // v^β
//    double w = 1.0 - v_beta;                         // w = 1 - v^β
//    double ln_w = std::log(w);                       // ln(w)
//    // Derivative of w w.r.t. v: dw/dv = -β * v^(β-1)
//    double dw_dv = -beta * std::pow(v, beta - 1.0);
//    // Chain rule: dw/dα = dw/dv * dv/dα
//    double dw_dalpha = dw_dv * dv_dalpha;
//    // First derivative w.r.t. α: d/dα ln(w) = (1/w)*dw_dalpha
//    double dL6_dalpha = (lambda - 1.0) * (dw_dalpha / w);
//    // Second derivative w.r.t. α for L6:
//    // d²/dα² ln(w) = [d²w/dα² * w - (dw/dα)²] / w²
//    // Computing d²w/dα²:
//    //   dw/dα = -β * v^(β-1)*dv_dalpha,
//    //   d²w/dα² = -β * [(β-1)*v^(β-2)*(dv_dalpha)² + v^(β-1)*d²v_dalpha²]
//    double d2w_dalpha2 = -beta * ((beta - 1.0) * std::pow(v, beta-2.0) * (dv_dalpha * dv_dalpha)
//                                    + std::pow(v, beta-1.0) * d2v_dalpha2);
//    double d2L6_dalpha2 = (lambda - 1.0) * ((d2w_dalpha2 * w - (dw_dalpha * dw_dalpha)) / (w*w));
//    // Derivative w.r.t. β: d/dβ ln(w). Note: d/dβ(v^β) = v^β ln(v) => d/dβ w = -v^β ln(v)
//    double dw_dbeta = -v_beta * ln_v;
//    double dL6_dbeta = (lambda - 1.0) * (dw_dbeta / w);
//    // Second derivative w.r.t. β for L6:
//    // d²/dβ² ln(w) = [d²w/dβ² * w - (dw/dβ)²]/w², where d²w/dβ² = -v^β (ln(v))²
//    double d2w_dbeta2 = -v_beta * (ln_v * ln_v);
//    double d2L6_dbeta2 = (lambda - 1.0) * ((d2w_dbeta2 * w - (dw_dbeta * dw_dbeta))/(w*w));
//    // Mixed derivative L6 (α,β): d²/(dα dβ) ln(w) =
//    //   = d/dβ[(dw_dalpha)/w] = (d/dβ dw_dalpha)/w - (dw_dalpha*dw_dbeta)/(w*w)
//    // Approximate d/dβ dw_dalpha:
//    double d_dw_dalpha_dbeta = -std::pow(v, beta-1.0) * (1.0 + beta * ln_v) * dv_dalpha;
//    double d2L6_dalpha_dbeta = (lambda - 1.0) * ((d_dw_dalpha_dbeta / w) - (dw_dalpha * dw_dbeta)/(w*w));
//
//    // Mixed derivatives with λ
//    // (α,λ): d²/(dα dλ) [λ ln(w)] = d/dλ[(λ-1)(dw_dalpha/w)] = dw_dalpha/w
//    double d2L6_dalpha_dlambda = dw_dalpha / w;
//
//    // (β,λ): d²/(dβ dλ) [λ ln(w)] = d/dλ[(λ-1)(dw_dbeta/w)] = dw_dbeta/w
//    double d2L6_dbeta_dlambda = dw_dbeta / w;
//
//    // --- ACCUMULATING CONTRIBUTIONS TO THE HESSIAN MATRIX ---
//    // Index: 0 = α, 1 = β, 2 = λ
//
//    // H(α,α): sum of L2, L5, and L6 (constants already added)
//    H(0,0) += d2L5_dalpha2 + d2L6_dalpha2;
//
//    // H(α,β): mixed from L5 and L6
//    H(0,1) += d2L5_dalpha_dbeta + d2L6_dalpha_dbeta;
//    H(1,0) = H(0,1);
//
//    // H(β,β): contributions from L3 and L6
//    H(1,1) += d2L6_dbeta2;
//
//    // H(α,λ): mixed derivative from L6
//    H(0,2) += d2L6_dalpha_dlambda;
//    H(2,0) = H(0,2);
//
//    // H(β,λ): mixed derivative from L6
//    H(1,2) += d2L6_dbeta_dlambda;
//    H(2,1) = H(1,2);
//
//  } // end of loop
//
//  // Returns the analytic Hessian matrix of the negative log-likelihood
//  return Rcpp::wrap(-H);
// }





/*
----------------------------------------------------------------------------
IMPORTANT NOTE:
We assume the same numeric stability functions from gkwdist.cpp or a "core" file:
- log1mexp(double)
- safe_log(double)
- safe_exp(double)
- safe_pow(double,double)
- etc.
They are NOT redefined here to avoid duplication.
*/

/*
----------------------------------------------------------------------------
BETA POWER (BP) DISTRIBUTION: BP(γ, δ, λ)
----------------------------------------------------------------------------

This arises from GKw with α=1 and β=1, leaving three parameters: (γ>0, δ≥0, λ>0).

* PDF:
f(x; γ, δ, λ) = [ λ / B(γ, δ+1) ] * x^(γλ - 1) * (1 - x^λ)^δ,   0<x<1.

* CDF:
F(x; γ, δ, λ) = I_{x^λ}(γ, δ+1) = pbeta(x^λ, γ, δ+1).

* QUANTILE:
Q(p; γ, δ, λ) = [ qbeta(p, γ, δ+1) ]^(1/λ).

* RNG:
If U ~ Beta(γ, δ+1), then X = U^(1/λ).

* NEGATIVE LOG-LIKELIHOOD:
sum( -log f(x_i) )
where
log f(x) = log(λ) - log B(γ, δ+1)
+ (γ λ -1)* log(x)
+ δ * log(1 - x^λ).

We'll define five functions:
- dmc() : PDF
- pmc() : CDF
- qmc() : quantile
- rmc() : random generator
- llmc(): negative log-likelihood

We'll also define a param-checker for (γ, δ, λ).
*/

// -----------------------------------------------------------------------------
// Parameter checker for Beta Power distribution
// BP(γ>0, δ≥0, λ>0)
// -----------------------------------------------------------------------------
inline bool check_bp_pars(double gamma, double delta, double lambda, bool strict = false) {
if (gamma <= 0.0 || delta < 0.0 || lambda <= 0.0) {
return false;
}
if (strict) {
const double MINP=1e-8;
const double MAXP=1e8;
if (gamma<MINP || lambda<MINP) return false;
if (gamma>MAXP || delta>MAXP || lambda>MAXP) return false;
}
return true;
}

// -----------------------------------------------------------------------------
// 1) dmc: PDF of Beta Power McDonald
// -----------------------------------------------------------------------------

//' @title Density of the McDonald (Mc)/Beta Power Distribution Distribution
//' @author Lopes, J. E.
//' @keywords distribution density mcdonald
//'
//' @description
//' Computes the probability density function (PDF) for the McDonald (Mc)
//' distribution (also previously referred to as Beta Power) with parameters
//' \code{gamma} (\eqn{\gamma}), \code{delta} (\eqn{\delta}), and \code{lambda}
//' (\eqn{\lambda}). This distribution is defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned (\eqn{\log(f(x))}). Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{gamma}, \code{delta},
//'   \code{lambda}). Returns \code{0} (or \code{-Inf} if
//'   \code{log_prob = TRUE}) for \code{x} outside the interval (0, 1), or
//'   \code{NaN} if parameters are invalid (e.g., \code{gamma <= 0},
//'   \code{delta < 0}, \code{lambda <= 0}).
//'
//' @details
//' The probability density function (PDF) of the McDonald (Mc) distribution
//' is given by:
//' \deqn{
//' f(x; \gamma, \delta, \lambda) = \frac{\lambda}{B(\gamma,\delta+1)} x^{\gamma \lambda - 1} (1 - x^\lambda)^\delta
//' }
//' for \eqn{0 < x < 1}, where \eqn{B(a,b)} is the Beta function
//' (\code{\link[base]{beta}}).
//'
//' The Mc distribution is a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{dgkw}}) obtained
//' by setting the parameters \eqn{\alpha = 1} and \eqn{\beta = 1}.
//' It was introduced by McDonald (1984) and is related to the Generalized Beta
//' distribution of the first kind (GB1). When \eqn{\lambda=1}, it simplifies
//' to the standard Beta distribution with parameters \eqn{\gamma} and
//' \eqn{\delta+1}.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{dgkw}} (parent distribution density),
//' \code{\link{pmc}}, \code{\link{qmc}}, \code{\link{rmc}} (other Mc functions),
//' \code{\link[stats]{dbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' x_vals <- c(0.2, 0.5, 0.8)
//' gamma_par <- 2.0
//' delta_par <- 1.5
//' lambda_par <- 1.0 # Equivalent to Beta(gamma, delta+1)
//'
//' # Calculate density using dmc
//' densities <- dmc(x_vals, gamma_par, delta_par, lambda_par)
//' print(densities)
//' # Compare with Beta density
//' print(stats::dbeta(x_vals, shape1 = gamma_par, shape2 = delta_par + 1))
//'
//' # Calculate log-density
//' log_densities <- dmc(x_vals, gamma_par, delta_par, lambda_par, log_prob = TRUE)
//' print(log_densities)
//'
//' # Compare with dgkw setting alpha = 1, beta = 1
//' densities_gkw <- dgkw(x_vals, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                       delta = delta_par, lambda = lambda_par)
//' print(paste("Max difference:", max(abs(densities - densities_gkw)))) # Should be near zero
//'
//' # Plot the density for different lambda values
//' curve_x <- seq(0.01, 0.99, length.out = 200)
//' curve_y1 <- dmc(curve_x, gamma = 2, delta = 3, lambda = 0.5)
//' curve_y2 <- dmc(curve_x, gamma = 2, delta = 3, lambda = 1.0) # Beta(2, 4)
//' curve_y3 <- dmc(curve_x, gamma = 2, delta = 3, lambda = 2.0)
//'
//' plot(curve_x, curve_y2, type = "l", main = "McDonald (Mc) Density (gamma=2, delta=3)",
//'      xlab = "x", ylab = "f(x)", col = "red", ylim = range(0, curve_y1, curve_y2, curve_y3))
//' lines(curve_x, curve_y1, col = "blue")
//' lines(curve_x, curve_y3, col = "green")
//' legend("topright", legend = c("lambda=0.5", "lambda=1.0 (Beta)", "lambda=2.0"),
//'        col = c("blue", "red", "green"), lty = 1, bty = "n")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dmc(
 const arma::vec& x,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool log_prob = false
) {
arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N= std::max({ x.n_elem, g_vec.n_elem, d_vec.n_elem, l_vec.n_elem });
arma::vec out(N);

// Pre-fill
out.fill(log_prob ? R_NegInf : 0.0);

for (size_t i=0; i<N; i++){
 double gg= g_vec[i % g_vec.n_elem];
 double dd= d_vec[i % d_vec.n_elem];
 double ll= l_vec[i % l_vec.n_elem];
 double xx= x[i % x.n_elem];

 if (!check_bp_pars(gg,dd,ll)) {
   // invalid => pdf=0 or logpdf=-Inf
   continue;
 }
 // domain
 if (xx<=0.0 || xx>=1.0 || !R_finite(xx)) {
   continue;
 }

 // log f(x)= log(λ) - log( B(γ, δ+1) )
 //           + (γλ -1)* log(x)
 //           + δ * log(1 - x^λ)
 double logB = R::lbeta(gg, dd+1.0);
 double logCst= std::log(ll) - logB;

 // (γ λ -1)* log(x)
 double exponent= gg*ll - 1.0;
 double lx= std::log(xx);
 double term1= exponent* lx;

 // δ * log(1 - x^λ)
 double x_pow_l= std::pow(xx, ll);
 if (x_pow_l>=1.0) {
   // => pdf=0
   continue;
 }
 double log_1_minus_xpow= std::log(1.0 - x_pow_l);
 double term2= dd * log_1_minus_xpow;

 double log_pdf= logCst + term1 + term2;
 if (log_prob) {
   out(i)= log_pdf;
 } else {
   out(i)= std::exp(log_pdf);
 }
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 2) pmc: CDF of Beta Power
// -----------------------------------------------------------------------------

//' @title CDF of the McDonald (Mc)/Beta Power Distribution
//' @author Lopes, J. E.
//' @keywords distribution cumulative mcdonald
//'
//' @description
//' Computes the cumulative distribution function (CDF), \eqn{F(q) = P(X \le q)},
//' for the McDonald (Mc) distribution (also known as Beta Power) with
//' parameters \code{gamma} (\eqn{\gamma}), \code{delta} (\eqn{\delta}), and
//' \code{lambda} (\eqn{\lambda}). This distribution is defined on the interval
//' (0, 1) and is a special case of the Generalized Kumaraswamy (GKw)
//' distribution where \eqn{\alpha = 1} and \eqn{\beta = 1}.
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms/complements
//'   depending on \code{lower_tail} and \code{log_p}. The length of the result
//'   is determined by the recycling rule applied to the arguments (\code{q},
//'   \code{gamma}, \code{delta}, \code{lambda}). Returns \code{0} (or \code{-Inf}
//'   if \code{log_p = TRUE}) for \code{q <= 0} and \code{1} (or \code{0} if
//'   \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN} for invalid
//'   parameters.
//'
//' @details
//' The McDonald (Mc) distribution is a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{pgkw}}) obtained
//' by setting parameters \eqn{\alpha = 1} and \eqn{\beta = 1}.
//'
//' The CDF of the GKw distribution is \eqn{F_{GKw}(q) = I_{y(q)}(\gamma, \delta+1)},
//' where \eqn{y(q) = [1-(1-q^{\alpha})^{\beta}]^{\lambda}} and \eqn{I_x(a,b)}
//' is the regularized incomplete beta function (\code{\link[stats]{pbeta}}).
//' Setting \eqn{\alpha=1} and \eqn{\beta=1} simplifies \eqn{y(q)} to \eqn{q^\lambda},
//' yielding the Mc CDF:
//' \deqn{
//' F(q; \gamma, \delta, \lambda) = I_{q^\lambda}(\gamma, \delta+1)
//' }
//' This is evaluated using the \code{\link[stats]{pbeta}} function as
//' \code{pbeta(q^lambda, shape1 = gamma, shape2 = delta + 1)}.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{pgkw}} (parent distribution CDF),
//' \code{\link{dmc}}, \code{\link{qmc}}, \code{\link{rmc}} (other Mc functions),
//' \code{\link[stats]{pbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' q_vals <- c(0.2, 0.5, 0.8)
//' gamma_par <- 2.0
//' delta_par <- 1.5
//' lambda_par <- 1.0 # Equivalent to Beta(gamma, delta+1)
//'
//' # Calculate CDF P(X <= q) using pmc
//' probs <- pmc(q_vals, gamma_par, delta_par, lambda_par)
//' print(probs)
//' # Compare with Beta CDF
//' print(stats::pbeta(q_vals, shape1 = gamma_par, shape2 = delta_par + 1))
//'
//' # Calculate upper tail P(X > q)
//' probs_upper <- pmc(q_vals, gamma_par, delta_par, lambda_par,
//'                    lower_tail = FALSE)
//' print(probs_upper)
//' # Check: probs + probs_upper should be 1
//' print(probs + probs_upper)
//'
//' # Calculate log CDF
//' log_probs <- pmc(q_vals, gamma_par, delta_par, lambda_par, log_p = TRUE)
//' print(log_probs)
//' # Check: should match log(probs)
//' print(log(probs))
//'
//' # Compare with pgkw setting alpha = 1, beta = 1
//' probs_gkw <- pgkw(q_vals, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                   delta = delta_par, lambda = lambda_par)
//' print(paste("Max difference:", max(abs(probs - probs_gkw)))) # Should be near zero
//'
//' # Plot the CDF for different lambda values
//' curve_q <- seq(0.01, 0.99, length.out = 200)
//' curve_p1 <- pmc(curve_q, gamma = 2, delta = 3, lambda = 0.5)
//' curve_p2 <- pmc(curve_q, gamma = 2, delta = 3, lambda = 1.0) # Beta(2, 4)
//' curve_p3 <- pmc(curve_q, gamma = 2, delta = 3, lambda = 2.0)
//'
//' plot(curve_q, curve_p2, type = "l", main = "Mc/Beta Power CDF (gamma=2, delta=3)",
//'      xlab = "q", ylab = "F(q)", col = "red", ylim = c(0, 1))
//' lines(curve_q, curve_p1, col = "blue")
//' lines(curve_q, curve_p3, col = "green")
//' legend("bottomright", legend = c("lambda=0.5", "lambda=1.0 (Beta)", "lambda=2.0"),
//'        col = c("blue", "red", "green"), lty = 1, bty = "n")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pmc(
 const arma::vec& q,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N= std::max({ q.n_elem, g_vec.n_elem, d_vec.n_elem, l_vec.n_elem });
arma::vec out(N);

for (size_t i=0; i<N; i++){
 double gg= g_vec[i % g_vec.n_elem];
 double dd= d_vec[i % d_vec.n_elem];
 double ll= l_vec[i % l_vec.n_elem];
 double xx= q[i % q.n_elem];

 if (!check_bp_pars(gg,dd,ll)) {
   out(i)= NA_REAL;
   continue;
 }

 // boundaries
 if (!R_finite(xx) || xx<=0.0) {
   double val0= (lower_tail ? 0.0 : 1.0);
   out(i)= log_p ? std::log(val0) : val0;
   continue;
 }
 if (xx>=1.0) {
   double val1= (lower_tail ? 1.0 : 0.0);
   out(i)= log_p ? std::log(val1) : val1;
   continue;
 }

 double xpow= std::pow(xx, ll);
 // pbeta(xpow, gg, dd+1, TRUE, FALSE)
 double val= R::pbeta( xpow, gg, dd+1.0, true, false );
 if (!lower_tail) {
   val= 1.0 - val;
 }
 if (log_p) {
   val= std::log(val);
 }
 out(i)= val;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 3) qmc: Quantile of Beta Power
// -----------------------------------------------------------------------------

//' @title Quantile Function of the McDonald (Mc)/Beta Power Distribution
//' @author Lopes, J. E.
//' @keywords distribution quantile mcdonald
//'
//' @description
//' Computes the quantile function (inverse CDF) for the McDonald (Mc) distribution
//' (also known as Beta Power) with parameters \code{gamma} (\eqn{\gamma}),
//' \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}). It finds the
//' value \code{q} such that \eqn{P(X \le q) = p}. This distribution is a special
//' case of the Generalized Kumaraswamy (GKw) distribution where \eqn{\alpha = 1}
//' and \eqn{\beta = 1}.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are \eqn{p = P(X \le q)},
//'   otherwise, probabilities are \eqn{p = P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{gamma}, \code{delta}, \code{lambda}).
//'   Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{gamma <= 0},
//'           \code{delta < 0}, \code{lambda <= 0}).
//'   }
//'   Boundary return values are adjusted accordingly for \code{lower_tail = FALSE}.
//'
//' @details
//' The quantile function \eqn{Q(p)} is the inverse of the CDF \eqn{F(q)}. The CDF
//' for the Mc (\eqn{\alpha=1, \beta=1}) distribution is \eqn{F(q) = I_{q^\lambda}(\gamma, \delta+1)},
//' where \eqn{I_z(a,b)} is the regularized incomplete beta function (see \code{\link{pmc}}).
//'
//' To find the quantile \eqn{q}, we first invert the Beta function part: let
//' \eqn{y = I^{-1}_{p}(\gamma, \delta+1)}, where \eqn{I^{-1}_p(a,b)} is the
//' inverse computed via \code{\link[stats]{qbeta}}. We then solve \eqn{q^\lambda = y}
//' for \eqn{q}, yielding the quantile function:
//' \deqn{
//' Q(p) = \left[ I^{-1}_{p}(\gamma, \delta+1) \right]^{1/\lambda}
//' }
//' The function uses this formula, calculating \eqn{I^{-1}_{p}(\gamma, \delta+1)}
//' via \code{qbeta(p, gamma, delta + 1, ...)} while respecting the
//' \code{lower_tail} and \code{log_p} arguments. This is equivalent to the general
//' GKw quantile function (\code{\link{qgkw}}) evaluated with \eqn{\alpha=1, \beta=1}.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{qgkw}} (parent distribution quantile function),
//' \code{\link{dmc}}, \code{\link{pmc}}, \code{\link{rmc}} (other Mc functions),
//' \code{\link[stats]{qbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' p_vals <- c(0.1, 0.5, 0.9)
//' gamma_par <- 2.0
//' delta_par <- 1.5
//' lambda_par <- 1.0 # Equivalent to Beta(gamma, delta+1)
//'
//' # Calculate quantiles using qmc
//' quantiles <- qmc(p_vals, gamma_par, delta_par, lambda_par)
//' print(quantiles)
//' # Compare with Beta quantiles
//' print(stats::qbeta(p_vals, shape1 = gamma_par, shape2 = delta_par + 1))
//'
//' # Calculate quantiles for upper tail probabilities P(X > q) = p
//' quantiles_upper <- qmc(p_vals, gamma_par, delta_par, lambda_par,
//'                        lower_tail = FALSE)
//' print(quantiles_upper)
//' # Check: qmc(p, ..., lt=F) == qmc(1-p, ..., lt=T)
//' print(qmc(1 - p_vals, gamma_par, delta_par, lambda_par))
//'
//' # Calculate quantiles from log probabilities
//' log_p_vals <- log(p_vals)
//' quantiles_logp <- qmc(log_p_vals, gamma_par, delta_par, lambda_par, log_p = TRUE)
//' print(quantiles_logp)
//' # Check: should match original quantiles
//' print(quantiles)
//'
//' # Compare with qgkw setting alpha = 1, beta = 1
//' quantiles_gkw <- qgkw(p_vals, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                       delta = delta_par, lambda = lambda_par)
//' print(paste("Max difference:", max(abs(quantiles - quantiles_gkw)))) # Should be near zero
//'
//' # Verify inverse relationship with pmc
//' p_check <- 0.75
//' q_calc <- qmc(p_check, gamma_par, delta_par, lambda_par) # Use lambda != 1
//' p_recalc <- pmc(q_calc, gamma_par, delta_par, lambda_par)
//' print(paste("Original p:", p_check, " Recalculated p:", p_recalc))
//' # abs(p_check - p_recalc) < 1e-9 # Should be TRUE
//'
//' # Boundary conditions
//' print(qmc(c(0, 1), gamma_par, delta_par, lambda_par)) # Should be 0, 1
//' print(qmc(c(-Inf, 0), gamma_par, delta_par, lambda_par, log_p = TRUE)) # Should be 0, 1
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qmc(
 const arma::vec& p,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda,
 bool lower_tail=true,
 bool log_p=false
) {
arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t N= std::max({ p.n_elem, g_vec.n_elem, d_vec.n_elem, l_vec.n_elem });
arma::vec out(N);

for (size_t i=0; i<N; i++){
 double gg= g_vec[i % g_vec.n_elem];
 double dd= d_vec[i % d_vec.n_elem];
 double ll= l_vec[i % l_vec.n_elem];
 double pp= p[i % p.n_elem];

 if (!check_bp_pars(gg,dd,ll)) {
   out(i)= NA_REAL;
   continue;
 }

 // handle log_p
 if (log_p) {
   if (pp>0.0) {
     // log(p)>0 => p>1 => invalid
     out(i)= NA_REAL;
     continue;
   }
   pp= std::exp(pp);
 }
 // handle tail
 if (!lower_tail) {
   pp= 1.0 - pp;
 }

 // boundary
 if (pp<=0.0) {
   out(i)= 0.0;
   continue;
 }
 if (pp>=1.0) {
   out(i)= 1.0;
   continue;
 }

 // step1= R::qbeta(pp, gg, dd+1)
 double y= R::qbeta(pp, gg, dd+1.0, true, false);
 // step2= y^(1/λ)
 double xval;
 if (ll==1.0) {
   xval= y;
 } else {
   xval= std::pow(y, 1.0/ll);
 }
 if (!R_finite(xval) || xval<0.0) xval=0.0;
 if (xval>1.0) xval=1.0;
 out(i)= xval;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 4) rmc: RNG for Beta Power
// -----------------------------------------------------------------------------

//' @title Random Number Generation for the McDonald (Mc)/Beta Power Distribution
//' @author Lopes, J. E.
//' @keywords distribution random mcdonald
//'
//' @description
//' Generates random deviates from the McDonald (Mc) distribution (also known as
//' Beta Power) with parameters \code{gamma} (\eqn{\gamma}), \code{delta}
//' (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}). This distribution is a
//' special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\alpha = 1} and \eqn{\beta = 1}.
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param gamma Shape parameter \code{gamma} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param delta Shape parameter \code{delta} >= 0. Can be a scalar or a vector.
//'   Default: 0.0.
//' @param lambda Shape parameter \code{lambda} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//'
//' @return A vector of length \code{n} containing random deviates from the Mc
//'   distribution, with values in (0, 1). The length of the result is determined
//'   by \code{n} and the recycling rule applied to the parameters (\code{gamma},
//'   \code{delta}, \code{lambda}). Returns \code{NaN} if parameters
//'   are invalid (e.g., \code{gamma <= 0}, \code{delta < 0}, \code{lambda <= 0}).
//'
//' @details
//' The generation method uses the relationship between the GKw distribution and the
//' Beta distribution. The general procedure for GKw (\code{\link{rgkw}}) is:
//' If \eqn{W \sim \mathrm{Beta}(\gamma, \delta+1)}, then
//' \eqn{X = \{1 - [1 - W^{1/\lambda}]^{1/\beta}\}^{1/\alpha}} follows the
//' GKw(\eqn{\alpha, \beta, \gamma, \delta, \lambda}) distribution.
//'
//' For the Mc distribution, \eqn{\alpha=1} and \eqn{\beta=1}. Therefore, the
//' algorithm simplifies significantly:
//' \enumerate{
//'   \item Generate \eqn{U \sim \mathrm{Beta}(\gamma, \delta+1)} using
//'         \code{\link[stats]{rbeta}}.
//'   \item Compute the Mc variate \eqn{X = U^{1/\lambda}}.
//' }
//' This procedure is implemented efficiently, handling parameter recycling as needed.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Devroye, L. (1986). *Non-Uniform Random Variate Generation*. Springer-Verlag.
//' (General methods for random variate generation).
//'
//' @seealso
//' \code{\link{rgkw}} (parent distribution random generation),
//' \code{\link{dmc}}, \code{\link{pmc}}, \code{\link{qmc}} (other Mc functions),
//' \code{\link[stats]{rbeta}}
//'
//' @examples
//' \donttest{
//' set.seed(2028) # for reproducibility
//'
//' # Generate 1000 random values from a specific Mc distribution
//' gamma_par <- 2.0
//' delta_par <- 1.5
//' lambda_par <- 1.0 # Equivalent to Beta(gamma, delta+1)
//'
//' x_sample_mc <- rmc(1000, gamma = gamma_par, delta = delta_par,
//'                    lambda = lambda_par)
//' summary(x_sample_mc)
//'
//' # Histogram of generated values compared to theoretical density
//' hist(x_sample_mc, breaks = 30, freq = FALSE, # freq=FALSE for density
//'      main = "Histogram of Mc Sample (Beta Case)", xlab = "x")
//' curve(dmc(x, gamma = gamma_par, delta = delta_par, lambda = lambda_par),
//'       add = TRUE, col = "red", lwd = 2, n = 201)
//' curve(stats::dbeta(x, gamma_par, delta_par + 1), add=TRUE, col="blue", lty=2)
//' legend("topright", legend = c("Theoretical Mc PDF", "Theoretical Beta PDF"),
//'        col = c("red", "blue"), lwd = c(2,1), lty=c(1,2), bty = "n")
//'
//' # Comparing empirical and theoretical quantiles (Q-Q plot)
//' lambda_par_qq <- 0.7 # Use lambda != 1 for non-Beta case
//' x_sample_mc_qq <- rmc(1000, gamma = gamma_par, delta = delta_par,
//'                       lambda = lambda_par_qq)
//' prob_points <- seq(0.01, 0.99, by = 0.01)
//' theo_quantiles <- qmc(prob_points, gamma = gamma_par, delta = delta_par,
//'                       lambda = lambda_par_qq)
//' emp_quantiles <- quantile(x_sample_mc_qq, prob_points, type = 7)
//'
//' plot(theo_quantiles, emp_quantiles, pch = 16, cex = 0.8,
//'      main = "Q-Q Plot for Mc Distribution",
//'      xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles (n=1000)")
//' abline(a = 0, b = 1, col = "blue", lty = 2)
//'
//' # Compare summary stats with rgkw(..., alpha=1, beta=1, ...)
//' # Note: individual values will differ due to randomness
//' x_sample_gkw <- rgkw(1000, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                      delta = delta_par, lambda = lambda_par_qq)
//' print("Summary stats for rmc sample:")
//' print(summary(x_sample_mc_qq))
//' print("Summary stats for rgkw(alpha=1, beta=1) sample:")
//' print(summary(x_sample_gkw)) # Should be similar
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rmc(
 int n,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 const Rcpp::NumericVector& lambda
) {
if (n<=0) {
 Rcpp::stop("rmc: n must be positive");
}

arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());
arma::vec l_vec(lambda.begin(), lambda.size());

size_t k= std::max({ g_vec.n_elem, d_vec.n_elem, l_vec.n_elem });
arma::vec out(n);

for(int i=0; i<n; i++){
 size_t idx= i%k;
 double gg= g_vec[idx % g_vec.n_elem];
 double dd= d_vec[idx % d_vec.n_elem];
 double ll= l_vec[idx % l_vec.n_elem];

 if(!check_bp_pars(gg,dd,ll)) {
   out(i)= NA_REAL;
   Rcpp::warning("rmc: invalid parameters at index %d", i+1);
   continue;
 }

 double U= R::rbeta(gg, dd+1.0);
 double xval;
 if (ll==1.0) {
   xval= U;
 } else {
   xval= std::pow(U, 1.0/ll);
 }
 if (!R_finite(xval) || xval<0.0) xval=0.0;
 if (xval>1.0) xval=1.0;
 out(i)= xval;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 5) llmc: Negative Log-Likelihood for Beta Power
// -----------------------------------------------------------------------------


//' @title Negative Log-Likelihood for the McDonald (Mc)/Beta Power Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize mcdonald
//'
//' @description
//' Computes the negative log-likelihood function for the McDonald (Mc)
//' distribution (also known as Beta Power) with parameters \code{gamma}
//' (\eqn{\gamma}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}),
//' given a vector of observations. This distribution is the special case of the
//' Generalized Kumaraswamy (GKw) distribution where \eqn{\alpha = 1} and
//' \eqn{\beta = 1}. This function is suitable for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 3 containing the distribution parameters
//'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
//'   if any parameter values in \code{par} are invalid according to their
//'   constraints, or if any value in \code{data} is not in the interval (0, 1).
//'
//' @details
//' The McDonald (Mc) distribution is the GKw distribution (\code{\link{dmc}})
//' with \eqn{\alpha=1} and \eqn{\beta=1}. Its probability density function (PDF) is:
//' \deqn{
//' f(x | \theta) = \frac{\lambda}{B(\gamma,\delta+1)} x^{\gamma \lambda - 1} (1 - x^\lambda)^\delta
//' }
//' for \eqn{0 < x < 1}, \eqn{\theta = (\gamma, \delta, \lambda)}, and \eqn{B(a,b)}
//' is the Beta function (\code{\link[base]{beta}}).
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\lambda) - \ln B(\gamma, \delta+1)]
//' + \sum_{i=1}^{n} [(\gamma\lambda - 1)\ln(x_i) + \delta\ln(1 - x_i^\lambda)]
//' }
//' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
//' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
//' Numerical stability is maintained, including using the log-gamma function
//' (\code{\link[base]{lgamma}}) for the Beta function term.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' @seealso
//' \code{\link{llgkw}} (parent distribution negative log-likelihood),
//' \code{\link{dmc}}, \code{\link{pmc}}, \code{\link{qmc}}, \code{\link{rmc}},
//' \code{grmc} (gradient, if available),
//' \code{hsmc} (Hessian, if available),
//' \code{\link[stats]{optim}}, \code{\link[base]{lbeta}}
//'
//' @examples
//' \donttest{
//' # Assuming existence of rmc, grmc, hsmc functions for Mc distribution
//'
//' # Generate sample data from a known Mc distribution
//' set.seed(123)
//' true_par_mc <- c(gamma = 2, delta = 3, lambda = 0.5)
//' # Use rmc for data generation
//' sample_data_mc <- rmc(100, gamma = true_par_mc[1], delta = true_par_mc[2],
//'                       lambda = true_par_mc[3])
//' hist(sample_data_mc, breaks = 20, main = "Mc(2, 3, 0.5) Sample")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess
//' start_par_mc <- c(1.5, 2.5, 0.8)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' mle_result_mc <- stats::optim(par = start_par_mc,
//'                               fn = llmc, # Use the Mc neg-log-likelihood
//'                               method = "BFGS", # Or "L-BFGS-B" with lower=1e-6
//'                               hessian = TRUE,
//'                               data = sample_data_mc)
//'
//' # Check convergence and results
//' if (mle_result_mc$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par_mc <- mle_result_mc$par
//'   print("Estimated Mc parameters:")
//'   print(mle_par_mc)
//'   print("True Mc parameters:")
//'   print(true_par_mc)
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result_mc$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires 'numDeriv' package and analytical functions 'grmc', 'hsmc'
//' if (mle_result_mc$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grmc") && exists("hsmc")) {
//'
//'   cat("\nComparing Derivatives at Mc MLE estimates:\n")
//'
//'   # Numerical derivatives of llmc
//'   num_grad_mc <- numDeriv::grad(func = llmc, x = mle_par_mc, data = sample_data_mc)
//'   num_hess_mc <- numDeriv::hessian(func = llmc, x = mle_par_mc, data = sample_data_mc)
//'
//'   # Analytical derivatives (assuming they return derivatives of negative LL)
//'   ana_grad_mc <- grmc(par = mle_par_mc, data = sample_data_mc)
//'   ana_hess_mc <- hsmc(par = mle_par_mc, data = sample_data_mc)
//'
//'   # Check differences
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad_mc - ana_grad_mc)))
//'   cat("Max absolute difference between Hessians:\n")
//'   print(max(abs(num_hess_mc - ana_hess_mc)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison for Mc.\n")
//'    cat("Requires convergence, 'numDeriv' package and functions 'grmc', 'hsmc'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 3) return R_PosInf;

 double gamma = par[0];
 double delta = par[1];
 double lambda = par[2];

 if (!check_bp_pars(gamma, delta, lambda)) return R_PosInf;

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1) return R_PosInf;

 // Data boundary check
 if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) return R_PosInf;

 int n = x.n_elem;
 double loglike = 0.0;

 // Stability constants
 const double eps = 1e-10;
 // const double exp_threshold = -700.0;

 // Compute log(Beta(gamma, delta+1)) stably
 double log_B;
 if (gamma > 100.0 || delta > 100.0) {
   // For large parameters, use Stirling's approximation
   log_B = lgamma(gamma) + lgamma(delta + 1.0) - lgamma(gamma + delta + 1.0);
 } else {
   log_B = R::lbeta(gamma, delta + 1.0);
 }

 // Constant term: n*(log(lambda) - log(B(gamma, delta+1)))
 double log_lambda = safe_log(lambda);
 double const_term = n * (log_lambda - log_B);

 // Calculate gamma*lambda - 1.0 with precision for values near 1.0
 double gl_minus_1 = gamma * lambda - 1.0;

 // Initialize accumulators for sum terms
 double sum_term1 = 0.0;  // Sum of (gamma*lambda-1)*log(x)
 double sum_term2 = 0.0;  // Sum of delta*log(1-x^lambda)

 // Process each observation with careful numerical treatment
 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Handle observations near boundaries
   if (xi < eps) xi = eps;
   if (xi > 1.0 - eps) xi = 1.0 - eps;

   double log_xi = std::log(xi);

   // Term 1: (gamma*lambda-1)*log(x)
   // Special handling for large gamma*lambda
   sum_term1 += gl_minus_1 * log_xi;

   // Calculate x^lambda stably
   double x_lambda;
   if (lambda * std::abs(log_xi) > 1.0) {
     // Use log domain for potential overflow/underflow
     x_lambda = std::exp(lambda * log_xi);
   } else {
     x_lambda = std::pow(xi, lambda);
   }

   // Term 2: delta*log(1-x^lambda)
   // Use log1p for x^lambda close to 1 for better precision
   double log_1_minus_x_lambda;
   if (x_lambda > 0.9995) {
     // For x^lambda near 1, use complementary calculation
     log_1_minus_x_lambda = log1p(-x_lambda);
   } else {
     log_1_minus_x_lambda = std::log(1.0 - x_lambda);
   }

   // Special handling for large delta values
   if (delta > 1000.0 && log_1_minus_x_lambda < -0.01) {
     // Scale to prevent overflow with large delta
     double scaled_term = std::max(log_1_minus_x_lambda, -700.0 / delta);
     sum_term2 += delta * scaled_term;
   } else {
     sum_term2 += delta * log_1_minus_x_lambda;
   }
 }

 loglike = const_term + sum_term1 + sum_term2;

 // Check for invalid results
 if (!std::isfinite(loglike)) return R_PosInf;

 return -loglike;  // Return negative log-likelihood
}




//' @title Gradient of the Negative Log-Likelihood for the McDonald (Mc)/Beta Power Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient mcdonald
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the McDonald (Mc) distribution (also
//' known as Beta Power) with parameters \code{gamma} (\eqn{\gamma}), \code{delta}
//' (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}). This distribution is the
//' special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\alpha = 1} and \eqn{\beta = 1}. The gradient is useful for optimization.
//'
//' @param par A numeric vector of length 3 containing the distribution parameters
//'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 3 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter:
//'   \eqn{(-\partial \ell/\partial \gamma, -\partial \ell/\partial \delta, -\partial \ell/\partial \lambda)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the Mc (\eqn{\alpha=1, \beta=1})
//' model are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma+\delta+1) - \psi(\gamma)] -
//' \lambda\sum_{i=1}^{n}\ln(x_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \delta} = n[\psi(\gamma+\delta+1) - \psi(\delta+1)] -
//' \sum_{i=1}^{n}\ln(1-x_i^{\lambda})
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} - \gamma\sum_{i=1}^{n}\ln(x_i) +
//' \delta\sum_{i=1}^{n}\frac{x_i^{\lambda}\ln(x_i)}{1-x_i^{\lambda}}
//' }
//'
//' where \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the relevant components
//' of the general GKw gradient (\code{\link{grgkw}}) evaluated at \eqn{\alpha=1, \beta=1}.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' (Note: Specific gradient formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{grgkw}} (parent distribution gradient),
//' \code{\link{llmc}} (negative log-likelihood for Mc),
//' \code{hsmc} (Hessian for Mc, if available),
//' \code{\link{dmc}} (density for Mc),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
//' \code{\link[base]{digamma}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rmc, llmc, grmc, hsmc functions for Mc distribution
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_mc <- c(gamma = 2, delta = 3, lambda = 0.5)
//' sample_data_mc <- rmc(100, gamma = true_par_mc[1], delta = true_par_mc[2],
//'                       lambda = true_par_mc[3])
//' hist(sample_data_mc, breaks = 20, main = "Mc(2, 3, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_mc <- c(1.5, 2.5, 0.8)
//' mle_result_mc <- stats::optim(par = start_par_mc,
//'                               fn = llmc,
//'                               gr = grmc, # Use analytical gradient for Mc
//'                               method = "BFGS",
//'                               hessian = TRUE,
//'                               data = sample_data_mc)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_mc$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_mc <- mle_result_mc$par
//'   cat("\nComparing Gradients for Mc at MLE estimates:\n")
//'
//'   # Numerical gradient of llmc
//'   num_grad_mc <- numDeriv::grad(func = llmc, x = mle_par_mc, data = sample_data_mc)
//'
//'   # Analytical gradient from grmc
//'   ana_grad_mc <- grmc(par = mle_par_mc, data = sample_data_mc)
//'
//'   cat("Numerical Gradient (Mc):\n")
//'   print(num_grad_mc)
//'   cat("Analytical Gradient (Mc):\n")
//'   print(ana_grad_mc)
//'
//'   # Check differences
//'   cat("Max absolute difference between Mc gradients:\n")
//'   print(max(abs(num_grad_mc - ana_grad_mc)))
//'
//' } else {
//'   cat("\nSkipping Mc gradient comparison.\n")
//' }
//'
//' # Example with Hessian comparison (if hsmc exists)
//' if (mle_result_mc$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) && exists("hsmc")) {
//'
//'   num_hess_mc <- numDeriv::hessian(func = llmc, x = mle_par_mc, data = sample_data_mc)
//'   ana_hess_mc <- hsmc(par = mle_par_mc, data = sample_data_mc)
//'   cat("\nMax absolute difference between Mc Hessians:\n")
//'   print(max(abs(num_hess_mc - ana_hess_mc)))
//'
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter validation
 if (par.size() < 3) {
   Rcpp::NumericVector grad(3, R_NaN);
   return grad;
 }

 double gamma = par[0];
 double delta = par[1];
 double lambda = par[2];

 if (gamma <= 0 || delta < 0 || lambda <= 0) {
   Rcpp::NumericVector grad(3, R_NaN);
   return grad;
 }

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::NumericVector grad(3, R_NaN);
   return grad;
 }

 int n = x.n_elem;
 Rcpp::NumericVector grad(3, 0.0);

 // Stability constants
 const double eps = 1e-10;

 // Calculate digamma terms stably
 double digamma_gamma_delta_plus_1, digamma_gamma, digamma_delta_plus_1;

 // For large arguments, use asymptotic approximation of digamma
 if (gamma + delta > 100.0) {
   digamma_gamma_delta_plus_1 = std::log(gamma + delta + 1.0) - 1.0/(2.0*(gamma + delta + 1.0));
 } else {
   digamma_gamma_delta_plus_1 = R::digamma(gamma + delta + 1.0);
 }

 if (gamma > 100.0) {
   digamma_gamma = std::log(gamma) - 1.0/(2.0*gamma);
 } else {
   digamma_gamma = R::digamma(gamma);
 }

 if (delta > 100.0) {
   digamma_delta_plus_1 = std::log(delta + 1.0) - 1.0/(2.0*(delta + 1.0));
 } else {
   digamma_delta_plus_1 = R::digamma(delta + 1.0);
 }

 // Initialize accumulators
 double sum_log_x = 0.0;
 double sum_log_v = 0.0;
 double sum_term_lambda = 0.0;

 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Handle boundary values
   if (xi < eps) xi = eps;
   if (xi > 1.0 - eps) xi = 1.0 - eps;

   double log_xi = std::log(xi);
   sum_log_x += log_xi;

   // Calculate x^lambda stably
   double x_lambda;
   if (lambda > 100.0 || lambda * std::abs(log_xi) > 1.0) {
     x_lambda = std::exp(lambda * log_xi);
   } else {
     x_lambda = std::pow(xi, lambda);
   }

   // Calculate 1-x^lambda with precision for x^lambda near 1
   double v;
   if (x_lambda > 0.9995) {
     v = -std::expm1(lambda * log_xi);  // More precise than 1.0 - x_lambda
   } else {
     v = 1.0 - x_lambda;
   }

   // Ensure v is not too small
   v = std::max(v, eps);
   double log_v = std::log(v);
   sum_log_v += log_v;

   // Calculate term for lambda gradient: (x^lambda * log(x)) / (1-x^lambda)
   double lambda_term = (x_lambda * log_xi) / v;

   // Prevent extreme values that might lead to instability
   if (std::abs(lambda_term) > 1e6) {
     lambda_term = std::copysign(1e6, lambda_term);
   }

   sum_term_lambda += lambda_term;
 }

 // Compute gradient components
 double d_gamma = -n * (digamma_gamma_delta_plus_1 - digamma_gamma) - lambda * sum_log_x;
 double d_delta = -n * (digamma_gamma_delta_plus_1 - digamma_delta_plus_1) - sum_log_v;
 double d_lambda = -n / lambda - gamma * sum_log_x + delta * sum_term_lambda;

 // Alread negative gradient for negative log-likelihood
 grad[0] = d_gamma;
 grad[1] = d_delta;
 grad[2] = d_lambda;

 return grad;
}



//' @title Hessian Matrix of the Negative Log-Likelihood for the McDonald (Mc)/Beta Power Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian mcdonald
//'
//' @description
//' Computes the analytic 3x3 Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the McDonald (Mc) distribution
//' (also known as Beta Power) with parameters \code{gamma} (\eqn{\gamma}),
//' \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}). This distribution
//' is the special case of the Generalized Kumaraswamy (GKw) distribution where
//' \eqn{\alpha = 1} and \eqn{\beta = 1}. The Hessian is useful for estimating
//' standard errors and in optimization algorithms.
//'
//' @param par A numeric vector of length 3 containing the distribution parameters
//'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
//'   \code{lambda} (\eqn{\lambda > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 3x3 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
//'   where \eqn{\theta = (\gamma, \delta, \lambda)}.
//'   Returns a 3x3 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function (\eqn{-\ell(\theta|\mathbf{x})}).
//' The components are based on the second derivatives of the log-likelihood \eqn{\ell}
//' (derived from the PDF in \code{\link{dmc}}).
//'
//' **Note:** The formulas below represent the second derivatives of the positive
//' log-likelihood (\eqn{\ell}). The function returns the **negative** of these values.
//' Users should verify these formulas independently if using for critical applications.
//'
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \gamma^2} = -n[\psi'(\gamma) - \psi'(\gamma+\delta+1)]
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \gamma \partial \delta} = -n\psi'(\gamma+\delta+1)
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \gamma \partial \lambda} = \sum_{i=1}^{n}\ln(x_i)
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \delta^2} = -n[\psi'(\delta+1) - \psi'(\gamma+\delta+1)]
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \delta \partial \lambda} = -\sum_{i=1}^{n}\frac{x_i^{\lambda}\ln(x_i)}{1-x_i^{\lambda}}
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \lambda^2} = -\frac{n}{\lambda^2} -
//' \delta\sum_{i=1}^{n}\frac{x_i^{\lambda}[\ln(x_i)]^2}{(1-x_i^{\lambda})^2}
//' }
//'
//' where \eqn{\psi'(\cdot)} is the trigamma function (\code{\link[base]{trigamma}}).
//' (*Note: The formula for \eqn{\partial^2 \ell / \partial \lambda^2} provided in the source
//' comment was different and potentially related to the expected information matrix;
//' the formula shown here is derived from the gradient provided earlier. Verification
//' is recommended.*)
//'
//' The returned matrix is symmetric, with rows/columns corresponding to
//' \eqn{\gamma, \delta, \lambda}.
//'
//' @references
//' McDonald, J. B. (1984). Some generalized functions for the size distribution
//' of income. *Econometrica*, 52(3), 647-663.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' (Note: Specific Hessian formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{hsgkw}} (parent distribution Hessian),
//' \code{\link{llmc}} (negative log-likelihood for Mc),
//' \code{\link{grmc}} (gradient for Mc, if available),
//' \code{\link{dmc}} (density for Mc),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison),
//' \code{\link[base]{trigamma}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rmc, llmc, grmc, hsmc functions for Mc distribution
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_mc <- c(gamma = 2, delta = 3, lambda = 0.5)
//' sample_data_mc <- rmc(100, gamma = true_par_mc[1], delta = true_par_mc[2],
//'                       lambda = true_par_mc[3])
//' hist(sample_data_mc, breaks = 20, main = "Mc(2, 3, 0.5) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_mc <- c(1.5, 2.5, 0.8)
//' mle_result_mc <- stats::optim(par = start_par_mc,
//'                               fn = llmc,
//'                               gr = if (exists("grmc")) grmc else NULL,
//'                               method = "BFGS",
//'                               hessian = TRUE, # Ask optim for numerical Hessian
//'                               data = sample_data_mc)
//'
//' # --- Compare analytical Hessian to numerical Hessian ---
//' if (mle_result_mc$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("hsmc")) {
//'
//'   mle_par_mc <- mle_result_mc$par
//'   cat("\nComparing Hessians for Mc at MLE estimates:\n")
//'
//'   # Numerical Hessian of llmc
//'   num_hess_mc <- numDeriv::hessian(func = llmc, x = mle_par_mc, data = sample_data_mc)
//'
//'   # Analytical Hessian from hsmc
//'   ana_hess_mc <- hsmc(par = mle_par_mc, data = sample_data_mc)
//'
//'   cat("Numerical Hessian (Mc):\n")
//'   print(round(num_hess_mc, 4))
//'   cat("Analytical Hessian (Mc):\n")
//'   print(round(ana_hess_mc, 4))
//'
//'   # Check differences (monitor sign consistency)
//'   cat("Max absolute difference between Mc Hessians:\n")
//'   print(max(abs(num_hess_mc - ana_hess_mc)))
//'
//'   # Optional: Use analytical Hessian for Standard Errors
//'   # tryCatch({
//'   #   cov_matrix_mc <- solve(ana_hess_mc) # ana_hess_mc is already Hessian of negative LL
//'   #   std_errors_mc <- sqrt(diag(cov_matrix_mc))
//'   #   cat("Std. Errors from Analytical Mc Hessian:\n")
//'   #   print(std_errors_mc)
//'   # }, error = function(e) {
//'   #   warning("Could not invert analytical Mc Hessian: ", e$message)
//'   # })
//'
//' } else {
//'   cat("\nSkipping Mc Hessian comparison.\n")
//'   cat("Requires convergence, 'numDeriv' package, and function 'hsmc'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hsmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
 // Parameter extraction and validation
 if (par.size() < 3) {
   Rcpp::NumericMatrix hess(3, 3);
   hess.fill(R_NaN);
   return hess;
 }

 double gamma = par[0];
 double delta = par[1];
 double lambda = par[2];

 // Parameter validation
 if (gamma <= 0 || delta < 0 || lambda <= 0) {
   Rcpp::NumericMatrix hess(3, 3);
   hess.fill(R_NaN);
   return hess;
 }

 arma::vec x = Rcpp::as<arma::vec>(data);
 if (x.n_elem < 1 || arma::any(x <= 0) || arma::any(x >= 1)) {
   Rcpp::NumericMatrix hess(3, 3);
   hess.fill(R_NaN);
   return hess;
 }

 int n = x.n_elem;
 Rcpp::NumericMatrix hess(3, 3);

 // Stability constants
 const double eps = 1e-10;
 const double max_contrib = 1e6;  // Limit for individual contributions

 // Compute trigamma values stably
 // For large z: trigamma(z) ≈ 1/z + 1/(2z²) + O(1/z⁴)
 double trigamma_gamma, trigamma_delta_plus_1, trigamma_gamma_plus_delta_plus_1;

 if (gamma > 100.0) {
   // Asymptotic approximation: ψ'(z) ≈ 1/z + 1/(2z²)
   trigamma_gamma = 1.0/gamma + 1.0/(2.0*gamma*gamma);
 } else {
   trigamma_gamma = R::trigamma(gamma);
 }

 if (delta > 100.0) {
   // Asymptotic approximation for ψ'(δ+1)
   trigamma_delta_plus_1 = 1.0/(delta+1.0) + 1.0/(2.0*(delta+1.0)*(delta+1.0));
 } else {
   trigamma_delta_plus_1 = R::trigamma(delta + 1.0);
 }

 if (gamma + delta > 100.0) {
   // Asymptotic approximation for ψ'(γ+δ+1)
   double z = gamma + delta + 1.0;
   trigamma_gamma_plus_delta_plus_1 = 1.0/z + 1.0/(2.0*z*z);
 } else {
   trigamma_gamma_plus_delta_plus_1 = R::trigamma(gamma + delta + 1.0);
 }

 // Initialize accumulators for sums
 double sum_log_x = 0.0;
 double sum_x_lambda_log_x_div_v = 0.0;
 double sum_lambda_term = 0.0;

 // Calculate term-by-term to control numerical stability
 for (int i = 0; i < n; i++) {
   double xi = x(i);

   // Handle boundary values
   if (xi < eps) xi = eps;
   if (xi > 1.0 - eps) xi = 1.0 - eps;

   // Calculate log(x) stably
   double log_xi = std::log(xi);
   sum_log_x += log_xi;

   // Calculate x^lambda stably using log domain for large lambda
   double x_lambda;
   if (lambda > 100.0 || lambda * std::abs(log_xi) > 1.0) {
     double log_x_lambda = lambda * log_xi;
     x_lambda = std::exp(log_x_lambda);
   } else {
     x_lambda = std::pow(xi, lambda);
   }

   // Calculate v = 1-x^lambda with precision for x^lambda near 1
   double v;
   if (x_lambda > 0.9995) {
     // Use complementary calculation: 1-exp(a) = -expm1(a)
     v = -std::expm1(lambda * log_xi);
   } else {
     v = 1.0 - x_lambda;
   }

   // Ensure v is not too small
   v = std::max(v, eps);

   // Term for H[1,2] = ∂²ℓ/∂δ∂λ = Σ[x^λ*log(x)/(1-x^λ)]
   double term1 = (x_lambda * log_xi) / v;
   // Prevent extreme values
   term1 = std::min(std::max(term1, -max_contrib), max_contrib);
   sum_x_lambda_log_x_div_v += term1;

   // Calculate squared log with safe scaling
   double log_xi_squared = log_xi * log_xi;

   // Term for H[2,2] = ∂²ℓ/∂λ²
   // = n/λ² + δ*Σ[x^λ*(log(x))²/(1-x^λ)*(1 + x^λ/(1-x^λ))]
   // = n/λ² + δ*Σ[x^λ*(log(x))²/(1-x^λ)²]
   double term_ratio = x_lambda / v;
   double term_combined = 1.0 + term_ratio;  // = 1/(1-x^λ)

   // Prevent overflow in combined term for x^λ near 1
   if (term_combined > 1e6) {
     term_combined = 1e6;
   }

   double lambda_term = delta * x_lambda * log_xi_squared * term_combined / v;

   // Prevent extreme values
   lambda_term = std::min(std::max(lambda_term, -max_contrib), max_contrib);
   sum_lambda_term += lambda_term;
 }

 // Calculate Hessian components

 // H[0,0] = -∂²ℓ/∂γ² = n[ψ'(γ+δ+1) - ψ'(γ)]
 double h_gamma_gamma = n * (trigamma_gamma_plus_delta_plus_1 - trigamma_gamma);

 // H[0,1] = H[1,0] = -∂²ℓ/∂γ∂δ = n*ψ'(γ+δ+1)
 double h_gamma_delta = n * trigamma_gamma_plus_delta_plus_1;

 // H[0,2] = H[2,0] = -∂²ℓ/∂γ∂λ = Σlog(x)
 double h_gamma_lambda = sum_log_x;

 // H[1,1] = -∂²ℓ/∂δ² = n[ψ'(γ+δ+1) - ψ'(δ+1)]
 double h_delta_delta = n * (trigamma_gamma_plus_delta_plus_1 - trigamma_delta_plus_1);

 // H[1,2] = H[2,1] = -∂²ℓ/∂δ∂λ = -Σ[x^λ*log(x)/(1-x^λ)]
 double h_delta_lambda = -sum_x_lambda_log_x_div_v;

 // H[2,2] = -∂²ℓ/∂λ² = -n/λ² - δ*Σ[x^λ*(log(x))²/(1-x^λ)²]
 double h_lambda_lambda = -n / (lambda * lambda) - sum_lambda_term;

 // Fill the Hessian matrix (symmetric)
 hess(0, 0) = -h_gamma_gamma;
 hess(0, 1) = hess(1, 0) = -h_gamma_delta;
 hess(0, 2) = hess(2, 0) = -h_gamma_lambda;
 hess(1, 1) = -h_delta_delta;
 hess(1, 2) = hess(2, 1) = -h_delta_lambda;
 hess(2, 2) = -h_lambda_lambda;

 return hess;
}



// //' @title Negative Log-Likelihood for the McDonald (Mc)/Beta Power Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize mcdonald
// //'
// //' @description
// //' Computes the negative log-likelihood function for the McDonald (Mc)
// //' distribution (also known as Beta Power) with parameters \code{gamma}
// //' (\eqn{\gamma}), \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}),
// //' given a vector of observations. This distribution is the special case of the
// //' Generalized Kumaraswamy (GKw) distribution where \eqn{\alpha = 1} and
// //' \eqn{\beta = 1}. This function is suitable for maximum likelihood estimation.
// //'
// //' @param par A numeric vector of length 3 containing the distribution parameters
// //'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
// //'   \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a single \code{double} value representing the negative
// //'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
// //'   if any parameter values in \code{par} are invalid according to their
// //'   constraints, or if any value in \code{data} is not in the interval (0, 1).
// //'
// //' @details
// //' The McDonald (Mc) distribution is the GKw distribution (\code{\link{dmc}})
// //' with \eqn{\alpha=1} and \eqn{\beta=1}. Its probability density function (PDF) is:
// //' \deqn{
// //' f(x | \theta) = \frac{\lambda}{B(\gamma,\delta+1)} x^{\gamma \lambda - 1} (1 - x^\lambda)^\delta
// //' }
// //' for \eqn{0 < x < 1}, \eqn{\theta = (\gamma, \delta, \lambda)}, and \eqn{B(a,b)}
// //' is the Beta function (\code{\link[base]{beta}}).
// //' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
// //' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
// //' \deqn{
// //' \ell(\theta | \mathbf{x}) = n[\ln(\lambda) - \ln B(\gamma, \delta+1)]
// //' + \sum_{i=1}^{n} [(\gamma\lambda - 1)\ln(x_i) + \delta\ln(1 - x_i^\lambda)]
// //' }
// //' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
// //' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
// //' Numerical stability is maintained, including using the log-gamma function
// //' (\code{\link[base]{lgamma}}) for the Beta function term.
// //'
// //' @references
// //' McDonald, J. B. (1984). Some generalized functions for the size distribution
// //' of income. *Econometrica*, 52(3), 647-663.
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //'
// //' Kumaraswamy, P. (1980). A generalized probability density function for
// //' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
// //'
// //'
// //' @seealso
// //' \code{\link{llgkw}} (parent distribution negative log-likelihood),
// //' \code{\link{dmc}}, \code{\link{pmc}}, \code{\link{qmc}}, \code{\link{rmc}},
// //' \code{grmc} (gradient, if available),
// //' \code{hsmc} (Hessian, if available),
// //' \code{\link[stats]{optim}}, \code{\link[base]{lbeta}}
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rmc, grmc, hsmc functions for Mc distribution
// //'
// //' # Generate sample data from a known Mc distribution
// //' set.seed(123)
// //' true_par_mc <- c(gamma = 2, delta = 3, lambda = 0.5)
// //' # Use rmc for data generation
// //' sample_data_mc <- rmc(100, gamma = true_par_mc[1], delta = true_par_mc[2],
// //'                       lambda = true_par_mc[3])
// //' hist(sample_data_mc, breaks = 20, main = "Mc(2, 3, 0.5) Sample")
// //'
// //' # --- Maximum Likelihood Estimation using optim ---
// //' # Initial parameter guess
// //' start_par_mc <- c(1.5, 2.5, 0.8)
// //'
// //' # Perform optimization (minimizing negative log-likelihood)
// //' mle_result_mc <- stats::optim(par = start_par_mc,
// //'                               fn = llmc, # Use the Mc neg-log-likelihood
// //'                               method = "BFGS", # Or "L-BFGS-B" with lower=1e-6
// //'                               hessian = TRUE,
// //'                               data = sample_data_mc)
// //'
// //' # Check convergence and results
// //' if (mle_result_mc$convergence == 0) {
// //'   print("Optimization converged successfully.")
// //'   mle_par_mc <- mle_result_mc$par
// //'   print("Estimated Mc parameters:")
// //'   print(mle_par_mc)
// //'   print("True Mc parameters:")
// //'   print(true_par_mc)
// //' } else {
// //'   warning("Optimization did not converge!")
// //'   print(mle_result_mc$message)
// //' }
// //'
// //' # --- Compare numerical and analytical derivatives (if available) ---
// //' # Requires 'numDeriv' package and analytical functions 'grmc', 'hsmc'
// //' if (mle_result_mc$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("grmc") && exists("hsmc")) {
// //'
// //'   cat("\nComparing Derivatives at Mc MLE estimates:\n")
// //'
// //'   # Numerical derivatives of llmc
// //'   num_grad_mc <- numDeriv::grad(func = llmc, x = mle_par_mc, data = sample_data_mc)
// //'   num_hess_mc <- numDeriv::hessian(func = llmc, x = mle_par_mc, data = sample_data_mc)
// //'
// //'   # Analytical derivatives (assuming they return derivatives of negative LL)
// //'   ana_grad_mc <- grmc(par = mle_par_mc, data = sample_data_mc)
// //'   ana_hess_mc <- hsmc(par = mle_par_mc, data = sample_data_mc)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between gradients:\n")
// //'   print(max(abs(num_grad_mc - ana_grad_mc)))
// //'   cat("Max absolute difference between Hessians:\n")
// //'   print(max(abs(num_hess_mc - ana_hess_mc)))
// //'
// //' } else {
// //'    cat("\nSkipping derivative comparison for Mc.\n")
// //'    cat("Requires convergence, 'numDeriv' package and functions 'grmc', 'hsmc'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// double llmc(const Rcpp::NumericVector& par,
//            const Rcpp::NumericVector& data) {
//  if (par.size()<3) {
//    return R_PosInf;
//  }
//  double gg= par[0];
//  double dd= par[1];
//  double ll= par[2];
//
//  if(!check_bp_pars(gg, dd, ll)) {
//    return R_PosInf;
//  }
//
//  arma::vec x = Rcpp::as<arma::vec>(data);
//  if (x.n_elem<1) {
//    return R_PosInf;
//  }
//  if (arma::any(x<=0.0) || arma::any(x>=1.0)) {
//    return R_PosInf;
//  }
//
//  int n= x.n_elem;
//
//  // constant: n*( log(λ) - log B(γ, δ+1 ) )
//  double logB= R::lbeta(gg, dd+1.0);
//  double cst= n*( std::log(ll) - logB );
//
//  // sum( (γλ -1)* log(x_i ) )
//  // sum( δ * log(1 - x_i^λ ) )
//  double gl= gg*ll -1.0;
//
//  arma::vec lx= arma::log(x);
//  double sum1= gl * arma::sum(lx);
//
//  // sum( δ * log(1 - x^λ) )
//  arma::vec x_pow_l = arma::pow(x, ll);
//
//  arma::vec log1mxpw= arma::log(1.0 - x_pow_l);
//  double sum2= dd * arma::sum(log1mxpw);
//
//  double loglike= cst + sum1 + sum2;
//  return -loglike;
// }
//
//
//
// //' @title Gradient of the Negative Log-Likelihood for the McDonald (Mc)/Beta Power Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize gradient mcdonald
// //'
// //' @description
// //' Computes the gradient vector (vector of first partial derivatives) of the
// //' negative log-likelihood function for the McDonald (Mc) distribution (also
// //' known as Beta Power) with parameters \code{gamma} (\eqn{\gamma}), \code{delta}
// //' (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}). This distribution is the
// //' special case of the Generalized Kumaraswamy (GKw) distribution where
// //' \eqn{\alpha = 1} and \eqn{\beta = 1}. The gradient is useful for optimization.
// //'
// //' @param par A numeric vector of length 3 containing the distribution parameters
// //'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
// //'   \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a numeric vector of length 3 containing the partial derivatives
// //'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
// //'   respect to each parameter:
// //'   \eqn{(-\partial \ell/\partial \gamma, -\partial \ell/\partial \delta, -\partial \ell/\partial \lambda)}.
// //'   Returns a vector of \code{NaN} if any parameter values are invalid according
// //'   to their constraints, or if any value in \code{data} is not in the
// //'   interval (0, 1).
// //'
// //' @details
// //' The components of the gradient vector of the negative log-likelihood
// //' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the Mc (\eqn{\alpha=1, \beta=1})
// //' model are:
// //'
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma+\delta+1) - \psi(\gamma)] -
// //' \lambda\sum_{i=1}^{n}\ln(x_i)
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \delta} = n[\psi(\gamma+\delta+1) - \psi(\delta+1)] -
// //' \sum_{i=1}^{n}\ln(1-x_i^{\lambda})
// //' }
// //' \deqn{
// //' -\frac{\partial \ell}{\partial \lambda} = -\frac{n}{\lambda} - \gamma\sum_{i=1}^{n}\ln(x_i) +
// //' \delta\sum_{i=1}^{n}\frac{x_i^{\lambda}\ln(x_i)}{1-x_i^{\lambda}}
// //' }
// //'
// //' where \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
// //' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
// //' minimizing the negative log-likelihood. They correspond to the relevant components
// //' of the general GKw gradient (\code{\link{grgkw}}) evaluated at \eqn{\alpha=1, \beta=1}.
// //'
// //' @references
// //' McDonald, J. B. (1984). Some generalized functions for the size distribution
// //' of income. *Econometrica*, 52(3), 647-663.
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' (Note: Specific gradient formulas might be derived or sourced from additional references).
// //'
// //' @seealso
// //' \code{\link{grgkw}} (parent distribution gradient),
// //' \code{\link{llmc}} (negative log-likelihood for Mc),
// //' \code{hsmc} (Hessian for Mc, if available),
// //' \code{\link{dmc}} (density for Mc),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
// //' \code{\link[base]{digamma}}.
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rmc, llmc, grmc, hsmc functions for Mc distribution
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_mc <- c(gamma = 2, delta = 3, lambda = 0.5)
// //' sample_data_mc <- rmc(100, gamma = true_par_mc[1], delta = true_par_mc[2],
// //'                       lambda = true_par_mc[3])
// //' hist(sample_data_mc, breaks = 20, main = "Mc(2, 3, 0.5) Sample")
// //'
// //' # --- Find MLE estimates ---
// //' start_par_mc <- c(1.5, 2.5, 0.8)
// //' mle_result_mc <- stats::optim(par = start_par_mc,
// //'                               fn = llmc,
// //'                               gr = grmc, # Use analytical gradient for Mc
// //'                               method = "BFGS",
// //'                               hessian = TRUE,
// //'                               data = sample_data_mc)
// //'
// //' # --- Compare analytical gradient to numerical gradient ---
// //' if (mle_result_mc$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE)) {
// //'
// //'   mle_par_mc <- mle_result_mc$par
// //'   cat("\nComparing Gradients for Mc at MLE estimates:\n")
// //'
// //'   # Numerical gradient of llmc
// //'   num_grad_mc <- numDeriv::grad(func = llmc, x = mle_par_mc, data = sample_data_mc)
// //'
// //'   # Analytical gradient from grmc
// //'   ana_grad_mc <- grmc(par = mle_par_mc, data = sample_data_mc)
// //'
// //'   cat("Numerical Gradient (Mc):\n")
// //'   print(num_grad_mc)
// //'   cat("Analytical Gradient (Mc):\n")
// //'   print(ana_grad_mc)
// //'
// //'   # Check differences
// //'   cat("Max absolute difference between Mc gradients:\n")
// //'   print(max(abs(num_grad_mc - ana_grad_mc)))
// //'
// //' } else {
// //'   cat("\nSkipping Mc gradient comparison.\n")
// //' }
// //'
// //' # Example with Hessian comparison (if hsmc exists)
// //' if (mle_result_mc$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) && exists("hsmc")) {
// //'
// //'   num_hess_mc <- numDeriv::hessian(func = llmc, x = mle_par_mc, data = sample_data_mc)
// //'   ana_hess_mc <- hsmc(par = mle_par_mc, data = sample_data_mc)
// //'   cat("\nMax absolute difference between Mc Hessians:\n")
// //'   print(max(abs(num_hess_mc - ana_hess_mc)))
// //'
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericVector grmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double gamma = par[0];   // Shape parameter γ > 0
//  double delta = par[1];   // Shape parameter δ > 0
//  double lambda = par[2];  // Shape parameter λ > 0
//
//  // Parameter validation
//  if (gamma <= 0 || delta <= 0 || lambda <= 0) {
//    Rcpp::NumericVector grad(3, R_NaN);
//    return grad;
//  }
//
//  // Data conversion and validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//
//  if (arma::any(x <= 0) || arma::any(x >= 1)) {
//    Rcpp::NumericVector grad(3, R_NaN);
//    return grad;
//  }
//
//  int n = x.n_elem;  // Sample size
//
//  // Initialize gradient vector
//  Rcpp::NumericVector grad(3, 0.0);
//
//  // Small constant to avoid numerical issues
//  double eps = std::numeric_limits<double>::epsilon() * 100;
//
//  // Compute transformations and intermediate values
//  arma::vec log_x = arma::log(x);                   // log(x_i)
//  arma::vec x_lambda = arma::pow(x, lambda);        // x_i^λ
//  arma::vec x_lambda_log_x = x_lambda % log_x;      // x_i^λ * log(x_i)
//
//  // v_i = 1 - x_i^λ
//  arma::vec v = 1.0 - x_lambda;
//  v = arma::clamp(v, eps, 1.0 - eps);               // Prevent numerical issues
//
//  arma::vec log_v = arma::log(v);                   // log(1 - x_i^λ)
//
//  // Calculate partial derivatives for each parameter (for log-likelihood)
//
//  // ∂ℓ/∂γ = -n[ψ(γ+δ+1) - ψ(γ)] - λΣᵢlog(xᵢ)
//  double d_gamma = -n * (R::digamma(gamma + delta + 1) - R::digamma(gamma)) -
//    lambda * arma::sum(log_x);
//
//  // ∂ℓ/∂δ = -n[ψ(γ+δ+1) - ψ(δ+1)] - Σᵢlog(1-xᵢ^λ)
//  double d_delta = -n * (R::digamma(gamma + delta + 1) - R::digamma(delta + 1)) -
//    arma::sum(log_v);
//
//  // ∂ℓ/∂λ = -n/λ - γΣᵢlog(xᵢ) + δΣᵢ[(xᵢ^λ*log(xᵢ))/(1-xᵢ^λ)]
//  double d_lambda = -n / lambda - gamma * arma::sum(log_x) +
//    delta * arma::sum(x_lambda_log_x / v);
//
//  // Since we're optimizing negative log-likelihood, negate all derivatives
//  grad[0] = -d_gamma;
//  grad[1] = -d_delta;
//  grad[2] = -d_lambda;
//
//  return Rcpp::wrap(grad);
// }
//
//
//
//
// //' @title Hessian Matrix of the Negative Log-Likelihood for the McDonald (Mc)/Beta Power Distribution
// //' @author Lopes, J. E.
// //' @keywords distribution likelihood optimize hessian mcdonald
// //'
// //' @description
// //' Computes the analytic 3x3 Hessian matrix (matrix of second partial derivatives)
// //' of the negative log-likelihood function for the McDonald (Mc) distribution
// //' (also known as Beta Power) with parameters \code{gamma} (\eqn{\gamma}),
// //' \code{delta} (\eqn{\delta}), and \code{lambda} (\eqn{\lambda}). This distribution
// //' is the special case of the Generalized Kumaraswamy (GKw) distribution where
// //' \eqn{\alpha = 1} and \eqn{\beta = 1}. The Hessian is useful for estimating
// //' standard errors and in optimization algorithms.
// //'
// //' @param par A numeric vector of length 3 containing the distribution parameters
// //'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}),
// //'   \code{lambda} (\eqn{\lambda > 0}).
// //' @param data A numeric vector of observations. All values must be strictly
// //'   between 0 and 1 (exclusive).
// //'
// //' @return Returns a 3x3 numeric matrix representing the Hessian matrix of the
// //'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
// //'   where \eqn{\theta = (\gamma, \delta, \lambda)}.
// //'   Returns a 3x3 matrix populated with \code{NaN} if any parameter values are
// //'   invalid according to their constraints, or if any value in \code{data} is
// //'   not in the interval (0, 1).
// //'
// //' @details
// //' This function calculates the analytic second partial derivatives of the
// //' negative log-likelihood function (\eqn{-\ell(\theta|\mathbf{x})}).
// //' The components are based on the second derivatives of the log-likelihood \eqn{\ell}
// //' (derived from the PDF in \code{\link{dmc}}).
// //'
// //' **Note:** The formulas below represent the second derivatives of the positive
// //' log-likelihood (\eqn{\ell}). The function returns the **negative** of these values.
// //' Users should verify these formulas independently if using for critical applications.
// //'
// //' \deqn{
// //' \frac{\partial^2 \ell}{\partial \gamma^2} = -n[\psi'(\gamma) - \psi'(\gamma+\delta+1)]
// //' }
// //' \deqn{
// //' \frac{\partial^2 \ell}{\partial \gamma \partial \delta} = -n\psi'(\gamma+\delta+1)
// //' }
// //' \deqn{
// //' \frac{\partial^2 \ell}{\partial \gamma \partial \lambda} = \sum_{i=1}^{n}\ln(x_i)
// //' }
// //' \deqn{
// //' \frac{\partial^2 \ell}{\partial \delta^2} = -n[\psi'(\delta+1) - \psi'(\gamma+\delta+1)]
// //' }
// //' \deqn{
// //' \frac{\partial^2 \ell}{\partial \delta \partial \lambda} = -\sum_{i=1}^{n}\frac{x_i^{\lambda}\ln(x_i)}{1-x_i^{\lambda}}
// //' }
// //' \deqn{
// //' \frac{\partial^2 \ell}{\partial \lambda^2} = -\frac{n}{\lambda^2} -
// //' \delta\sum_{i=1}^{n}\frac{x_i^{\lambda}[\ln(x_i)]^2}{(1-x_i^{\lambda})^2}
// //' }
// //'
// //' where \eqn{\psi'(\cdot)} is the trigamma function (\code{\link[base]{trigamma}}).
// //' (*Note: The formula for \eqn{\partial^2 \ell / \partial \lambda^2} provided in the source
// //' comment was different and potentially related to the expected information matrix;
// //' the formula shown here is derived from the gradient provided earlier. Verification
// //' is recommended.*)
// //'
// //' The returned matrix is symmetric, with rows/columns corresponding to
// //' \eqn{\gamma, \delta, \lambda}.
// //'
// //' @references
// //' McDonald, J. B. (1984). Some generalized functions for the size distribution
// //' of income. *Econometrica*, 52(3), 647-663.
// //'
// //' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
// //' distributions. *Journal of Statistical Computation and Simulation*,
// //'
// //' (Note: Specific Hessian formulas might be derived or sourced from additional references).
// //'
// //' @seealso
// //' \code{\link{hsgkw}} (parent distribution Hessian),
// //' \code{\link{llmc}} (negative log-likelihood for Mc),
// //' \code{\link{grmc}} (gradient for Mc, if available),
// //' \code{\link{dmc}} (density for Mc),
// //' \code{\link[stats]{optim}},
// //' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison),
// //' \code{\link[base]{trigamma}}.
// //'
// //' @examples
// //' \donttest{
// //' # Assuming existence of rmc, llmc, grmc, hsmc functions for Mc distribution
// //'
// //' # Generate sample data
// //' set.seed(123)
// //' true_par_mc <- c(gamma = 2, delta = 3, lambda = 0.5)
// //' sample_data_mc <- rmc(100, gamma = true_par_mc[1], delta = true_par_mc[2],
// //'                       lambda = true_par_mc[3])
// //' hist(sample_data_mc, breaks = 20, main = "Mc(2, 3, 0.5) Sample")
// //'
// //' # --- Find MLE estimates ---
// //' start_par_mc <- c(1.5, 2.5, 0.8)
// //' mle_result_mc <- stats::optim(par = start_par_mc,
// //'                               fn = llmc,
// //'                               gr = if (exists("grmc")) grmc else NULL,
// //'                               method = "BFGS",
// //'                               hessian = TRUE, # Ask optim for numerical Hessian
// //'                               data = sample_data_mc)
// //'
// //' # --- Compare analytical Hessian to numerical Hessian ---
// //' if (mle_result_mc$convergence == 0 &&
// //'     requireNamespace("numDeriv", quietly = TRUE) &&
// //'     exists("hsmc")) {
// //'
// //'   mle_par_mc <- mle_result_mc$par
// //'   cat("\nComparing Hessians for Mc at MLE estimates:\n")
// //'
// //'   # Numerical Hessian of llmc
// //'   num_hess_mc <- numDeriv::hessian(func = llmc, x = mle_par_mc, data = sample_data_mc)
// //'
// //'   # Analytical Hessian from hsmc
// //'   ana_hess_mc <- hsmc(par = mle_par_mc, data = sample_data_mc)
// //'
// //'   cat("Numerical Hessian (Mc):\n")
// //'   print(round(num_hess_mc, 4))
// //'   cat("Analytical Hessian (Mc):\n")
// //'   print(round(ana_hess_mc, 4))
// //'
// //'   # Check differences (monitor sign consistency)
// //'   cat("Max absolute difference between Mc Hessians:\n")
// //'   print(max(abs(num_hess_mc - ana_hess_mc)))
// //'
// //'   # Optional: Use analytical Hessian for Standard Errors
// //'   # tryCatch({
// //'   #   cov_matrix_mc <- solve(ana_hess_mc) # ana_hess_mc is already Hessian of negative LL
// //'   #   std_errors_mc <- sqrt(diag(cov_matrix_mc))
// //'   #   cat("Std. Errors from Analytical Mc Hessian:\n")
// //'   #   print(std_errors_mc)
// //'   # }, error = function(e) {
// //'   #   warning("Could not invert analytical Mc Hessian: ", e$message)
// //'   # })
// //'
// //' } else {
// //'   cat("\nSkipping Mc Hessian comparison.\n")
// //'   cat("Requires convergence, 'numDeriv' package, and function 'hsmc'.\n")
// //' }
// //'
// //' }
// //'
// //' @export
// // [[Rcpp::export]]
// Rcpp::NumericMatrix hsmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
//  // Parameter extraction
//  double gamma = par[0];   // Shape parameter γ > 0
//  double delta = par[1];   // Shape parameter δ > 0
//  double lambda = par[2];  // Shape parameter λ > 0
//
//  // Initialize Hessian matrix
//  Rcpp::NumericMatrix hess(3, 3);
//
//  // Parameter validation
//  if (gamma <= 0 || delta <= 0 || lambda <= 0) {
//    hess.fill(R_NaN);
//    return hess;
//  }
//
//  // Data conversion and validation
//  arma::vec x = Rcpp::as<arma::vec>(data);
//
//  if (arma::any(x <= 0) || arma::any(x >= 1)) {
//    hess.fill(R_NaN);
//    return hess;
//  }
//
//  int n = x.n_elem;  // Sample size
//
//  // Small constant to avoid numerical issues
//  double eps = std::numeric_limits<double>::epsilon() * 100;
//
//  // Compute transformations and intermediate values
//  arma::vec log_x = arma::log(x);                   // log(x_i)
//  arma::vec log_x_squared = arma::square(log_x);    // [log(x_i)]²
//  arma::vec x_lambda = arma::pow(x, lambda);        // x_i^λ
//  arma::vec x_lambda_log_x = x_lambda % log_x;      // x_i^λ * log(x_i)
//
//  // v_i = 1 - x_i^λ
//  arma::vec v = 1.0 - x_lambda;
//  v = arma::clamp(v, eps, 1.0 - eps);               // Prevent numerical issues
//
//  // Additional terms for second derivatives
//  arma::vec term_ratio = x_lambda / v;              // x_i^λ / (1-x_i^λ)
//  arma::vec term_combined = 1.0 + term_ratio;       // 1 + x_i^λ/(1-x_i^λ)
//
//  // Compute trigamma values
//  // trigamma(x) = d²(log(Gamma(x)))/dx² = d(digamma(x))/dx
//  double trigamma_gamma = R::trigamma(gamma);
//  double trigamma_delta_plus_1 = R::trigamma(delta + 1.0);
//  double trigamma_gamma_plus_delta_plus_1 = R::trigamma(gamma + delta + 1.0);
//
//  // Calculate the Hessian components for negative log-likelihood
//
//  // H[0,0] = ∂²ℓ/∂γ² = -n[ψ'(γ+δ+1) - ψ'(γ)]
//  double h_gamma_gamma = -n * (trigamma_gamma_plus_delta_plus_1 - trigamma_gamma);
//
//  // H[0,1] = H[1,0] = ∂²ℓ/∂γ∂δ = -n*ψ'(γ+δ+1)
//  double h_gamma_delta = -n * trigamma_gamma_plus_delta_plus_1;
//
//  // H[0,2] = H[2,0] = ∂²ℓ/∂γ∂λ = -Σᵢlog(xᵢ)
//  double h_gamma_lambda = -arma::sum(log_x);
//
//  // H[1,1] = ∂²ℓ/∂δ² = -n[ψ'(γ+δ+1) - ψ'(δ+1)]
//  double h_delta_delta = -n * (trigamma_gamma_plus_delta_plus_1 - trigamma_delta_plus_1);
//
//  // H[1,2] = H[2,1] = ∂²ℓ/∂δ∂λ = Σᵢ[x_i^λ*log(x_i)/(1-x_i^λ)]
//  double h_delta_lambda = arma::sum(x_lambda_log_x / v);
//
//  // H[2,2] = ∂²ℓ/∂λ² = n/λ² + δ*Σᵢ[x_i^λ*[log(x_i)]²/(1-x_i^λ)*(1 + x_i^λ/(1-x_i^λ))]
//  double h_lambda_lambda = n / (lambda * lambda);
//
//  arma::vec lambda_term = delta * x_lambda % log_x_squared % term_combined / v;
//  h_lambda_lambda += arma::sum(lambda_term);
//
//  // Fill the Hessian matrix (symmetric)
//  hess(0, 0) = -h_gamma_gamma;
//  hess(0, 1) = hess(1, 0) = -h_gamma_delta;
//  hess(0, 2) = hess(2, 0) = -h_gamma_lambda;
//  hess(1, 1) = -h_delta_delta;
//  hess(1, 2) = hess(2, 1) = -h_delta_lambda;
//  hess(2, 2) = -h_lambda_lambda;
//
//  return Rcpp::wrap(hess);
// }







/*
----------------------------------------------------------------------------
ASSUMPTIONS:
We rely on the same numeric stability functions from a "core" or gkwdist.cpp file:
- log1mexp(double)
- safe_log(double)
- safe_exp(double)
- safe_pow(double,double)
etc.

----------------------------------------------------------------------------
KUMARASWAMY (Kw) DISTRIBUTION
----------------------------------------------------------------------------

Parameters: alpha>0, beta>0.

* PDF:
f(x) = alpha * beta * x^(alpha -1) * (1 - x^alpha)^(beta -1),  for 0<x<1.

* CDF:
F(x)= 1 - (1 - x^alpha)^beta.

* QUANTILE:
Q(p)= {1 - [1 - p]^(1/beta)}^(1/alpha).

* RANDOM GENERATION:
If V ~ Uniform(0,1), then X= {1 - [1 - V]^(1/beta)}^(1/alpha).

* NEGATIVE LOG-LIKELIHOOD:
sum over i of -log( f(x_i) ).
log f(x_i)= log(alpha) + log(beta) + (alpha-1)*log(x_i) + (beta-1)*log(1 - x_i^alpha).

*/

// -----------------------------------------------------------------------------
// Parameter checker for Kumaraswamy
// alpha>0, beta>0
// -----------------------------------------------------------------------------
inline bool check_kw_pars(double alpha, double beta, bool strict=false) {
if (alpha <=0.0 || beta <=0.0) {
return false;
}
if (strict) {
const double MINP=1e-6, MAXP=1e6;
if (alpha<MINP || beta<MINP) return false;
if (alpha>MAXP || beta>MAXP) return false;
}
return true;
}

// -----------------------------------------------------------------------------
// 1) dkw: PDF of Kumaraswamy
// -----------------------------------------------------------------------------


//' @title Density of the Kumaraswamy (Kw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution density kumaraswamy
//'
//' @description
//' Computes the probability density function (PDF) for the two-parameter
//' Kumaraswamy (Kw) distribution with shape parameters \code{alpha} (\eqn{\alpha})
//' and \code{beta} (\eqn{\beta}). This distribution is defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned (\eqn{\log(f(x))}). Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{alpha}, \code{beta}).
//'   Returns \code{0} (or \code{-Inf} if \code{log_prob = TRUE}) for \code{x}
//'   outside the interval (0, 1), or \code{NaN} if parameters are invalid
//'   (e.g., \code{alpha <= 0}, \code{beta <= 0}).
//'
//' @details
//' The probability density function (PDF) of the Kumaraswamy (Kw) distribution
//' is given by:
//' \deqn{
//' f(x; \alpha, \beta) = \alpha \beta x^{\alpha-1} (1 - x^\alpha)^{\beta-1}
//' }
//' for \eqn{0 < x < 1}, \eqn{\alpha > 0}, and \eqn{\beta > 0}.
//'
//' The Kumaraswamy distribution is identical to the Generalized Kumaraswamy (GKw)
//' distribution (\code{\link{dgkw}}) with parameters \eqn{\gamma = 1},
//' \eqn{\delta = 0}, and \eqn{\lambda = 1}. It is also a special case of the
//' Exponentiated Kumaraswamy (\code{\link{dekw}}) with \eqn{\lambda = 1}, and
//' the Kumaraswamy-Kumaraswamy (\code{\link{dkkw}}) with \eqn{\delta = 0}
//' and \eqn{\lambda = 1}.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//'
//' @seealso
//' \code{\link{dgkw}} (parent distribution density),
//' \code{\link{dekw}}, \code{\link{dkkw}},
//' \code{\link{pkw}}, \code{\link{qkw}}, \code{\link{rkw}} (other Kw functions),
//' \code{\link[stats]{dbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' x_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//'
//' # Calculate density using dkw
//' densities <- dkw(x_vals, alpha_par, beta_par)
//' print(densities)
//'
//' # Calculate log-density
//' log_densities <- dkw(x_vals, alpha_par, beta_par, log_prob = TRUE)
//' print(log_densities)
//' # Check: should match log(densities)
//' print(log(densities))
//'
//' # Compare with dgkw setting gamma = 1, delta = 0, lambda = 1
//' densities_gkw <- dgkw(x_vals, alpha_par, beta_par, gamma = 1.0, delta = 0.0,
//'                       lambda = 1.0)
//' print(paste("Max difference:", max(abs(densities - densities_gkw)))) # Should be near zero
//'
//' # Plot the density for different shape parameter combinations
//' curve_x <- seq(0.001, 0.999, length.out = 200)
//' plot(curve_x, dkw(curve_x, alpha = 2, beta = 3), type = "l",
//'      main = "Kumaraswamy Density Examples", xlab = "x", ylab = "f(x)",
//'      col = "blue", ylim = c(0, 4))
//' lines(curve_x, dkw(curve_x, alpha = 3, beta = 2), col = "red")
//' lines(curve_x, dkw(curve_x, alpha = 0.5, beta = 0.5), col = "green") # U-shaped
//' lines(curve_x, dkw(curve_x, alpha = 5, beta = 1), col = "purple") # J-shaped
//' lines(curve_x, dkw(curve_x, alpha = 1, beta = 3), col = "orange") # J-shaped (reversed)
//' legend("top", legend = c("a=2, b=3", "a=3, b=2", "a=0.5, b=0.5", "a=5, b=1", "a=1, b=3"),
//'        col = c("blue", "red", "green", "purple", "orange"), lty = 1, bty = "n", ncol = 2)
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dkw(
 const arma::vec& x,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 bool log_prob=false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());

size_t N= std::max({ x.n_elem, a_vec.n_elem, b_vec.n_elem });
arma::vec out(N);

out.fill(log_prob ? R_NegInf : 0.0);

for (size_t i=0; i<N; i++){
 double a= a_vec[i % a_vec.n_elem];
 double b= b_vec[i % b_vec.n_elem];
 double xx= x[i % x.n_elem];

 if (!check_kw_pars(a,b)) {
   // invalid => pdf=0 or logpdf=-Inf
   continue;
 }
 if (xx<=0.0 || xx>=1.0 || !R_finite(xx)) {
   // outside domain => 0 or -Inf
   continue;
 }

 // log f(x)= log(a)+ log(b) + (a-1)* log(x) + (b-1)* log(1- x^a)
 double la= std::log(a);
 double lb= std::log(b);

 double lx= std::log(xx);
 double xalpha= a* lx; // log(x^a)
 // log(1- x^a)= log1mexp(xalpha)
 double log_1_xalpha= log1mexp(xalpha);
 if (!R_finite(log_1_xalpha)) {
   continue;
 }

 double log_pdf= la + lb + (a-1.0)* lx + (b-1.0)* log_1_xalpha;

 if (log_prob) {
   out(i)= log_pdf;
 } else {
   out(i)= std::exp(log_pdf);
 }
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 2) pkw: CDF of Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Cumulative Distribution Function (CDF) of the Kumaraswamy (Kw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution cumulative kumaraswamy
//'
//' @description
//' Computes the cumulative distribution function (CDF), \eqn{P(X \le q)}, for the
//' two-parameter Kumaraswamy (Kw) distribution with shape parameters \code{alpha}
//' (\eqn{\alpha}) and \code{beta} (\eqn{\beta}). This distribution is defined
//' on the interval (0, 1).
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms/complements
//'   depending on \code{lower_tail} and \code{log_p}. The length of the result
//'   is determined by the recycling rule applied to the arguments (\code{q},
//'   \code{alpha}, \code{beta}). Returns \code{0} (or \code{-Inf} if
//'   \code{log_p = TRUE}) for \code{q <= 0} and \code{1} (or \code{0} if
//'   \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN} for invalid
//'   parameters.
//'
//' @details
//' The cumulative distribution function (CDF) of the Kumaraswamy (Kw)
//' distribution is given by:
//' \deqn{
//' F(x; \alpha, \beta) = 1 - (1 - x^\alpha)^\beta
//' }
//' for \eqn{0 < x < 1}, \eqn{\alpha > 0}, and \eqn{\beta > 0}.
//'
//' The Kw distribution is a special case of several generalized distributions:
//' \itemize{
//'  \item Generalized Kumaraswamy (\code{\link{pgkw}}) with \eqn{\gamma=1, \delta=0, \lambda=1}.
//'  \item Exponentiated Kumaraswamy (\code{\link{pekw}}) with \eqn{\lambda=1}.
//'  \item Kumaraswamy-Kumaraswamy (\code{\link{pkkw}}) with \eqn{\delta=0, \lambda=1}.
//' }
//' The implementation uses the closed-form expression for efficiency.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//'
//' @seealso
//' \code{\link{pgkw}}, \code{\link{pekw}}, \code{\link{pkkw}} (related generalized CDFs),
//' \code{\link{dkw}}, \code{\link{qkw}}, \code{\link{rkw}} (other Kw functions),
//' \code{\link[stats]{pbeta}}
//'
//' @examples
//' \donttest{
//' # Example values
//' q_vals <- c(0.2, 0.5, 0.8)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//'
//' # Calculate CDF P(X <= q) using pkw
//' probs <- pkw(q_vals, alpha_par, beta_par)
//' print(probs)
//'
//' # Calculate upper tail P(X > q)
//' probs_upper <- pkw(q_vals, alpha_par, beta_par, lower_tail = FALSE)
//' print(probs_upper)
//' # Check: probs + probs_upper should be 1
//' print(probs + probs_upper)
//'
//' # Calculate log CDF
//' log_probs <- pkw(q_vals, alpha_par, beta_par, log_p = TRUE)
//' print(log_probs)
//' # Check: should match log(probs)
//' print(log(probs))
//'
//' # Compare with pgkw setting gamma = 1, delta = 0, lambda = 1
//' probs_gkw <- pgkw(q_vals, alpha_par, beta_par, gamma = 1.0, delta = 0.0,
//'                   lambda = 1.0)
//' print(paste("Max difference:", max(abs(probs - probs_gkw)))) # Should be near zero
//'
//' # Plot the CDF for different shape parameter combinations
//' curve_q <- seq(0.001, 0.999, length.out = 200)
//' plot(curve_q, pkw(curve_q, alpha = 2, beta = 3), type = "l",
//'      main = "Kumaraswamy CDF Examples", xlab = "q", ylab = "F(q)",
//'      col = "blue", ylim = c(0, 1))
//' lines(curve_q, pkw(curve_q, alpha = 3, beta = 2), col = "red")
//' lines(curve_q, pkw(curve_q, alpha = 0.5, beta = 0.5), col = "green")
//' lines(curve_q, pkw(curve_q, alpha = 5, beta = 1), col = "purple")
//' lines(curve_q, pkw(curve_q, alpha = 1, beta = 3), col = "orange")
//' legend("bottomright", legend = c("a=2, b=3", "a=3, b=2", "a=0.5, b=0.5", "a=5, b=1", "a=1, b=3"),
//'        col = c("blue", "red", "green", "purple", "orange"), lty = 1, bty = "n", ncol = 2)
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pkw(
 const arma::vec& q,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 bool lower_tail=true,
 bool log_p=false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());

size_t N= std::max({ q.n_elem, a_vec.n_elem, b_vec.n_elem });
arma::vec out(N);

for (size_t i=0; i<N; i++){
 double a= a_vec[i % a_vec.n_elem];
 double b= b_vec[i % b_vec.n_elem];
 double xx= q[i % q.n_elem];

 if (!check_kw_pars(a,b)) {
   out(i)= NA_REAL;
   continue;
 }

 // boundary
 if (!R_finite(xx) || xx<=0.0) {
   double val0= (lower_tail ? 0.0 : 1.0);
   out(i)= log_p ? std::log(val0) : val0;
   continue;
 }
 if (xx>=1.0) {
   double val1= (lower_tail ? 1.0 : 0.0);
   out(i)= log_p ? std::log(val1) : val1;
   continue;
 }

 double xalpha= std::pow(xx, a);
 double tmp= 1.0 - std::pow( (1.0 - xalpha), b );
 if (tmp<=0.0) {
   double val0= (lower_tail ? 0.0 : 1.0);
   out(i)= log_p ? std::log(val0) : val0;
   continue;
 }
 if (tmp>=1.0) {
   double val1= (lower_tail ? 1.0 : 0.0);
   out(i)= log_p ? std::log(val1) : val1;
   continue;
 }

 double val= tmp;
 if (!lower_tail) {
   val= 1.0- val;
 }
 if (log_p) {
   val= std::log(val);
 }
 out(i)= val;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}

// -----------------------------------------------------------------------------
// 3) qkw: Quantile of Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Quantile Function of the Kumaraswamy (Kw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution quantile kumaraswamy
//'
//' @description
//' Computes the quantile function (inverse CDF) for the two-parameter
//' Kumaraswamy (Kw) distribution with shape parameters \code{alpha} (\eqn{\alpha})
//' and \code{beta} (\eqn{\beta}). It finds the value \code{q} such that
//' \eqn{P(X \le q) = p}.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are \eqn{p = P(X \le q)},
//'   otherwise, probabilities are \eqn{p = P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{alpha}, \code{beta}).
//'   Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{alpha <= 0},
//'           \code{beta <= 0}).
//'   }
//'   Boundary return values are adjusted accordingly for \code{lower_tail = FALSE}.
//'
//' @details
//' The quantile function \eqn{Q(p)} is the inverse of the CDF \eqn{F(q)}. The CDF
//' for the Kumaraswamy distribution is \eqn{F(q) = 1 - (1 - q^\alpha)^\beta}
//' (see \code{\link{pkw}}). Inverting this equation for \eqn{q} yields the
//' quantile function:
//' \deqn{
//' Q(p) = \left\{ 1 - (1 - p)^{1/\beta} \right\}^{1/\alpha}
//' }
//' The function uses this closed-form expression and correctly handles the
//' \code{lower_tail} and \code{log_p} arguments by transforming \code{p}
//' appropriately before applying the formula. This is equivalent to the general
//' GKw quantile function (\code{\link{qgkw}}) evaluated with \eqn{\gamma=1, \delta=0, \lambda=1}.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//'
//' @seealso
//' \code{\link{qgkw}} (parent distribution quantile function),
//' \code{\link{dkw}}, \code{\link{pkw}}, \code{\link{rkw}} (other Kw functions),
//' \code{\link[stats]{qbeta}}, \code{\link[stats]{qunif}}
//'
//' @examples
//' \donttest{
//' # Example values
//' p_vals <- c(0.1, 0.5, 0.9)
//' alpha_par <- 2.0
//' beta_par <- 3.0
//'
//' # Calculate quantiles using qkw
//' quantiles <- qkw(p_vals, alpha_par, beta_par)
//' print(quantiles)
//'
//' # Calculate quantiles for upper tail probabilities P(X > q) = p
//' quantiles_upper <- qkw(p_vals, alpha_par, beta_par, lower_tail = FALSE)
//' print(quantiles_upper)
//' # Check: qkw(p, ..., lt=F) == qkw(1-p, ..., lt=T)
//' print(qkw(1 - p_vals, alpha_par, beta_par))
//'
//' # Calculate quantiles from log probabilities
//' log_p_vals <- log(p_vals)
//' quantiles_logp <- qkw(log_p_vals, alpha_par, beta_par, log_p = TRUE)
//' print(quantiles_logp)
//' # Check: should match original quantiles
//' print(quantiles)
//'
//' # Compare with qgkw setting gamma = 1, delta = 0, lambda = 1
//' quantiles_gkw <- qgkw(p_vals, alpha = alpha_par, beta = beta_par,
//'                      gamma = 1.0, delta = 0.0, lambda = 1.0)
//' print(paste("Max difference:", max(abs(quantiles - quantiles_gkw)))) # Should be near zero
//'
//' # Verify inverse relationship with pkw
//' p_check <- 0.75
//' q_calc <- qkw(p_check, alpha_par, beta_par)
//' p_recalc <- pkw(q_calc, alpha_par, beta_par)
//' print(paste("Original p:", p_check, " Recalculated p:", p_recalc))
//' # abs(p_check - p_recalc) < 1e-9 # Should be TRUE
//'
//' # Boundary conditions
//' print(qkw(c(0, 1), alpha_par, beta_par)) # Should be 0, 1
//' print(qkw(c(-Inf, 0), alpha_par, beta_par, log_p = TRUE)) # Should be 0, 1
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qkw(
 const arma::vec& p,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta,
 bool lower_tail=true,
 bool log_p=false
) {
arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());

size_t N= std::max({ p.n_elem, a_vec.n_elem, b_vec.n_elem });
arma::vec out(N);

for (size_t i=0; i<N; i++){
 double a= a_vec[i % a_vec.n_elem];
 double b= b_vec[i % b_vec.n_elem];
 double pp= p[i % p.n_elem];

 if (!check_kw_pars(a,b)) {
   out(i)= NA_REAL;
   continue;
 }

 // convert if log
 if (log_p) {
   if (pp>0.0) {
     // invalid => p>1
     out(i)= NA_REAL;
     continue;
   }
   pp= std::exp(pp);
 }
 if (!lower_tail) {
   pp= 1.0 - pp;
 }

 // boundary
 if (pp<=0.0) {
   out(i)= 0.0;
   continue;
 }
 if (pp>=1.0) {
   out(i)= 1.0;
   continue;
 }

 // x= {1 - [1 - p]^(1/beta)}^(1/alpha)
 double step1= 1.0 - pp;
 if (step1<0.0) step1=0.0;
 double step2= std::pow(step1, 1.0/b);
 double step3= 1.0 - step2;
 if (step3<0.0) step3=0.0;

 double xval;
 if (a==1.0) {
   xval= step3;
 } else {
   xval= std::pow(step3, 1.0/a);
   if (!R_finite(xval)|| xval<0.0) xval=0.0;
   if (xval>1.0) xval=1.0;
 }
 out(i)= xval;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 4) rkw: Random Generation from Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Random Number Generation for the Kumaraswamy (Kw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution random kumaraswamy
//'
//' @description
//' Generates random deviates from the two-parameter Kumaraswamy (Kw)
//' distribution with shape parameters \code{alpha} (\eqn{\alpha}) and
//' \code{beta} (\eqn{\beta}).
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param alpha Shape parameter \code{alpha} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//' @param beta Shape parameter \code{beta} > 0. Can be a scalar or a vector.
//'   Default: 1.0.
//'
//' @return A vector of length \code{n} containing random deviates from the Kw
//'   distribution, with values in (0, 1). The length of the result is determined
//'   by \code{n} and the recycling rule applied to the parameters (\code{alpha},
//'   \code{beta}). Returns \code{NaN} if parameters are invalid (e.g.,
//'   \code{alpha <= 0}, \code{beta <= 0}).
//'
//' @details
//' The generation method uses the inverse transform (quantile) method.
//' That is, if \eqn{U} is a random variable following a standard Uniform
//' distribution on (0, 1), then \eqn{X = Q(U)} follows the Kw distribution,
//' where \eqn{Q(p)} is the Kw quantile function (\code{\link{qkw}}):
//' \deqn{
//' Q(p) = \left\{ 1 - (1 - p)^{1/\beta} \right\}^{1/\alpha}
//' }
//' The implementation generates \eqn{U} using \code{\link[stats]{runif}}
//' and applies this transformation. This is equivalent to the general GKw
//' generation method (\code{\link{rgkw}}) evaluated at \eqn{\gamma=1, \delta=0, \lambda=1}.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//'
//' Devroye, L. (1986). *Non-Uniform Random Variate Generation*. Springer-Verlag.
//' (General methods for random variate generation).
//'
//' @seealso
//' \code{\link{rgkw}} (parent distribution random generation),
//' \code{\link{dkw}}, \code{\link{pkw}}, \code{\link{qkw}} (other Kw functions),
//' \code{\link[stats]{runif}}
//'
//' @examples
//' \donttest{
//' set.seed(2029) # for reproducibility
//'
//' # Generate 1000 random values from a specific Kw distribution
//' alpha_par <- 2.0
//' beta_par <- 3.0
//'
//' x_sample_kw <- rkw(1000, alpha = alpha_par, beta = beta_par)
//' summary(x_sample_kw)
//'
//' # Histogram of generated values compared to theoretical density
//' hist(x_sample_kw, breaks = 30, freq = FALSE, # freq=FALSE for density
//'      main = "Histogram of Kw Sample", xlab = "x", ylim = c(0, 2.5))
//' curve(dkw(x, alpha = alpha_par, beta = beta_par),
//'       add = TRUE, col = "red", lwd = 2, n = 201)
//' legend("top", legend = "Theoretical PDF", col = "red", lwd = 2, bty = "n")
//'
//' # Comparing empirical and theoretical quantiles (Q-Q plot)
//' prob_points <- seq(0.01, 0.99, by = 0.01)
//' theo_quantiles <- qkw(prob_points, alpha = alpha_par, beta = beta_par)
//' emp_quantiles <- quantile(x_sample_kw, prob_points, type = 7)
//'
//' plot(theo_quantiles, emp_quantiles, pch = 16, cex = 0.8,
//'      main = "Q-Q Plot for Kw Distribution",
//'      xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles (n=1000)")
//' abline(a = 0, b = 1, col = "blue", lty = 2)
//'
//' # Compare summary stats with rgkw(..., gamma=1, delta=0, lambda=1)
//' # Note: individual values will differ due to randomness
//' x_sample_gkw <- rgkw(1000, alpha = alpha_par, beta = beta_par, gamma = 1.0,
//'                      delta = 0.0, lambda = 1.0)
//' print("Summary stats for rkw sample:")
//' print(summary(x_sample_kw))
//' print("Summary stats for rgkw(gamma=1, delta=0, lambda=1) sample:")
//' print(summary(x_sample_gkw)) # Should be similar
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rkw(
 int n,
 const Rcpp::NumericVector& alpha,
 const Rcpp::NumericVector& beta
) {
if (n<=0) {
 Rcpp::stop("rkw: n must be positive");
}

arma::vec a_vec(alpha.begin(), alpha.size());
arma::vec b_vec(beta.begin(), beta.size());

size_t k= std::max({ a_vec.n_elem, b_vec.n_elem });
arma::vec out(n);

for (int i=0; i<n; i++){
 size_t idx= i % k;
 double a= a_vec[idx % a_vec.n_elem];
 double b= b_vec[idx % b_vec.n_elem];

 if (!check_kw_pars(a,b)) {
   out(i)= NA_REAL;
   Rcpp::warning("rkw: invalid parameters at index %d", i+1);
   continue;
 }

 double U= R::runif(0.0,1.0);
 // X= {1 - [1 - U]^(1/beta)}^(1/alpha)
 double step1= 1.0 - U;
 if (step1<0.0) step1=0.0;
 double step2= std::pow(step1, 1.0/b);
 double step3= 1.0 - step2;
 if (step3<0.0) step3=0.0;

 double x;
 if (a==1.0) {
   x= step3;
 } else {
   x= std::pow(step3, 1.0/a);
   if (!R_finite(x)|| x<0.0) x=0.0;
   if (x>1.0) x=1.0;
 }
 out(i)= x;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}

// -----------------------------------------------------------------------------
// 5) llkw: Negative Log-Likelihood for Kumaraswamy
// -----------------------------------------------------------------------------

//' @title Negative Log-Likelihood of the Kumaraswamy (Kw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize kumaraswamy
//'
//' @description
//' Computes the negative log-likelihood function for the two-parameter
//' Kumaraswamy (Kw) distribution with parameters \code{alpha} (\eqn{\alpha})
//' and \code{beta} (\eqn{\beta}), given a vector of observations. This function
//' is suitable for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 2 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
//'   if any parameter values in \code{par} are invalid according to their
//'   constraints, or if any value in \code{data} is not in the interval (0, 1).
//'
//' @details
//' The Kumaraswamy (Kw) distribution's probability density function (PDF) is
//' (see \code{\link{dkw}}):
//' \deqn{
//' f(x | \theta) = \alpha \beta x^{\alpha-1} (1 - x^\alpha)^{\beta-1}
//' }
//' for \eqn{0 < x < 1} and \eqn{\theta = (\alpha, \beta)}.
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = n[\ln(\alpha) + \ln(\beta)]
//' + \sum_{i=1}^{n} [(\alpha-1)\ln(x_i) + (\beta-1)\ln(v_i)]
//' }
//' where \eqn{v_i = 1 - x_i^{\alpha}}.
//' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
//' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
//' It is equivalent to the negative log-likelihood of the GKw distribution
//' (\code{\link{llgkw}}) evaluated at \eqn{\gamma=1, \delta=0, \lambda=1}.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//'
//' @seealso
//' \code{\link{llgkw}} (parent distribution negative log-likelihood),
//' \code{\link{dkw}}, \code{\link{pkw}}, \code{\link{qkw}}, \code{\link{rkw}},
//' \code{grkw} (gradient, if available),
//' \code{hskw} (Hessian, if available),
//' \code{\link[stats]{optim}}
//'
//' @examples
//' \donttest{
//' # Assuming existence of rkw, grkw, hskw functions for Kw distribution
//'
//' # Generate sample data from a known Kw distribution
//' set.seed(123)
//' true_par_kw <- c(alpha = 2, beta = 3)
//' sample_data_kw <- rkw(100, alpha = true_par_kw[1], beta = true_par_kw[2])
//' hist(sample_data_kw, breaks = 20, main = "Kw(2, 3) Sample")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess
//' start_par_kw <- c(1.5, 2.5)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' # Use method="L-BFGS-B" for box constraints (params > 0)
//' mle_result_kw <- stats::optim(par = start_par_kw,
//'                               fn = llkw, # Use the Kw neg-log-likelihood
//'                               method = "L-BFGS-B",
//'                               lower = c(1e-6, 1e-6), # Lower bounds > 0
//'                               hessian = TRUE,
//'                               data = sample_data_kw)
//'
//' # Check convergence and results
//' if (mle_result_kw$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par_kw <- mle_result_kw$par
//'   print("Estimated Kw parameters:")
//'   print(mle_par_kw)
//'   print("True Kw parameters:")
//'   print(true_par_kw)
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result_kw$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires 'numDeriv' package and analytical functions 'grkw', 'hskw'
//' if (mle_result_kw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grkw") && exists("hskw")) {
//'
//'   cat("\nComparing Derivatives at Kw MLE estimates:\n")
//'
//'   # Numerical derivatives of llkw
//'   num_grad_kw <- numDeriv::grad(func = llkw, x = mle_par_kw, data = sample_data_kw)
//'   num_hess_kw <- numDeriv::hessian(func = llkw, x = mle_par_kw, data = sample_data_kw)
//'
//'   # Analytical derivatives (assuming they return derivatives of negative LL)
//'   ana_grad_kw <- grkw(par = mle_par_kw, data = sample_data_kw)
//'   ana_hess_kw <- hskw(par = mle_par_kw, data = sample_data_kw)
//'
//'   # Check differences
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad_kw - ana_grad_kw)))
//'   cat("Max absolute difference between Hessians:\n")
//'   print(max(abs(num_hess_kw - ana_hess_kw)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison for Kw.\n")
//'    cat("Requires convergence, 'numDeriv' package and functions 'grkw', 'hskw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llkw(const Rcpp::NumericVector& par,
         const Rcpp::NumericVector& data) {
if (par.size()<2) {
 return R_PosInf;
}
double a= par[0];
double b= par[1];

if (!check_kw_pars(a,b)) {
 return R_PosInf;
}

arma::vec x= Rcpp::as<arma::vec>(data);
if (x.n_elem<1) {
 return R_PosInf;
}
if (arma::any(x<=0.0) || arma::any(x>=1.0)) {
 return R_PosInf;
}

int n= x.n_elem;
// constant: n*( log(a)+ log(b) )
double cst= n*( std::log(a) + std::log(b) );

// sum( (a-1)* log(x_i ) )
arma::vec lx= arma::log(x);
double sum1= (a-1.0)* arma::sum(lx);

// sum( (b-1)* log(1- x^a) )
arma::vec xalpha= arma::pow(x,a);
arma::vec log_1_xalpha= arma::log(1.0 - xalpha);
double sum2= (b-1.0)* arma::sum(log_1_xalpha);

double loglike= cst + sum1 + sum2;
// negative
return -loglike;
}



//' @title Gradient of the Negative Log-Likelihood for the Kumaraswamy (Kw) Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient kumaraswamy
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the two-parameter Kumaraswamy (Kw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}) and \code{beta}
//' (\eqn{\beta}). This provides the analytical gradient often used for efficient
//' optimization via maximum likelihood estimation.
//'
//' @param par A numeric vector of length 2 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 2 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter: \eqn{(-\partial \ell/\partial \alpha, -\partial \ell/\partial \beta)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' The components of the gradient vector of the negative log-likelihood
//' (\eqn{-\nabla \ell(\theta | \mathbf{x})}) for the Kw model are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \alpha} = -\frac{n}{\alpha} - \sum_{i=1}^{n}\ln(x_i)
//' + (\beta-1)\sum_{i=1}^{n}\frac{x_i^{\alpha}\ln(x_i)}{v_i}
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \beta} = -\frac{n}{\beta} - \sum_{i=1}^{n}\ln(v_i)
//' }
//'
//' where \eqn{v_i = 1 - x_i^{\alpha}}.
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the relevant components
//' of the general GKw gradient (\code{\link{grgkw}}) evaluated at \eqn{\gamma=1, \delta=0, \lambda=1}.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//' (Note: Specific gradient formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{grgkw}} (parent distribution gradient),
//' \code{\link{llkw}} (negative log-likelihood for Kw),
//' \code{hskw} (Hessian for Kw, if available),
//' \code{\link{dkw}} (density for Kw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rkw, llkw, grkw, hskw functions for Kw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_kw <- c(alpha = 2, beta = 3)
//' sample_data_kw <- rkw(100, alpha = true_par_kw[1], beta = true_par_kw[2])
//' hist(sample_data_kw, breaks = 20, main = "Kw(2, 3) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_kw <- c(1.5, 2.5)
//' mle_result_kw <- stats::optim(par = start_par_kw,
//'                               fn = llkw,
//'                               gr = grkw, # Use analytical gradient for Kw
//'                               method = "L-BFGS-B", # Recommended for bounds
//'                               lower = c(1e-6, 1e-6),
//'                               hessian = TRUE,
//'                               data = sample_data_kw)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_kw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_kw <- mle_result_kw$par
//'   cat("\nComparing Gradients for Kw at MLE estimates:\n")
//'
//'   # Numerical gradient of llkw
//'   num_grad_kw <- numDeriv::grad(func = llkw, x = mle_par_kw, data = sample_data_kw)
//'
//'   # Analytical gradient from grkw
//'   ana_grad_kw <- grkw(par = mle_par_kw, data = sample_data_kw)
//'
//'   cat("Numerical Gradient (Kw):\n")
//'   print(num_grad_kw)
//'   cat("Analytical Gradient (Kw):\n")
//'   print(ana_grad_kw)
//'
//'   # Check differences
//'   cat("Max absolute difference between Kw gradients:\n")
//'   print(max(abs(num_grad_kw - ana_grad_kw)))
//'
//' } else {
//'   cat("\nSkipping Kw gradient comparison.\n")
//' }
//'
//' # Example with Hessian comparison (if hskw exists)
//' if (mle_result_kw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) && exists("hskw")) {
//'
//'   num_hess_kw <- numDeriv::hessian(func = llkw, x = mle_par_kw, data = sample_data_kw)
//'   ana_hess_kw <- hskw(par = mle_par_kw, data = sample_data_kw)
//'   cat("\nMax absolute difference between Kw Hessians:\n")
//'   print(max(abs(num_hess_kw - ana_hess_kw)))
//'
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
// Parameter extraction
double alpha = par[0];   // Shape parameter α > 0
double beta = par[1];    // Shape parameter β > 0

// Parameter validation
if (alpha <= 0 || beta <= 0) {
 Rcpp::NumericVector grad(2, R_NaN);
 return grad;
}

// Data conversion and validation
arma::vec x = Rcpp::as<arma::vec>(data);

if (arma::any(x <= 0) || arma::any(x >= 1)) {
 Rcpp::NumericVector grad(2, R_NaN);
 return grad;
}

int n = x.n_elem;  // Sample size

// Initialize gradient vector
Rcpp::NumericVector grad(2, 0.0);

// Small constant to avoid numerical issues
double eps = std::numeric_limits<double>::epsilon() * 100;

// Compute transformations and intermediate values
arma::vec log_x = arma::log(x);                // log(x_i)
arma::vec x_alpha = arma::pow(x, alpha);       // x_i^α
arma::vec x_alpha_log_x = x_alpha % log_x;     // x_i^α * log(x_i)

// v_i = 1 - x_i^α
arma::vec v = 1.0 - x_alpha;
v = arma::clamp(v, eps, 1.0 - eps);            // Prevent numerical issues

arma::vec log_v = arma::log(v);                // log(1-x_i^α)

// Calculate partial derivatives for each parameter (for log-likelihood)

// ∂ℓ/∂α = n/α + Σᵢlog(xᵢ) - Σᵢ[(β-1)xᵢ^α*log(xᵢ)/(1-xᵢ^α)]
double d_alpha = n / alpha + arma::sum(log_x);

// Calculate the term for α gradient
arma::vec alpha_term = (beta - 1.0) * x_alpha_log_x / v;

d_alpha -= arma::sum(alpha_term);

// ∂ℓ/∂β = n/β + Σᵢlog(1-xᵢ^α)
double d_beta = n / beta + arma::sum(log_v);

// Since we're optimizing negative log-likelihood, negate all derivatives
grad[0] = -d_alpha;
grad[1] = -d_beta;

return grad;
}



//' @title Hessian Matrix of the Negative Log-Likelihood for the Kw Distribution
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian kumaraswamy
//'
//' @description
//' Computes the analytic 2x2 Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the two-parameter Kumaraswamy (Kw)
//' distribution with parameters \code{alpha} (\eqn{\alpha}) and \code{beta}
//' (\eqn{\beta}). The Hessian is useful for estimating standard errors and in
//' optimization algorithms.
//'
//' @param par A numeric vector of length 2 containing the distribution parameters
//'   in the order: \code{alpha} (\eqn{\alpha > 0}), \code{beta} (\eqn{\beta > 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 2x2 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
//'   where \eqn{\theta = (\alpha, \beta)}.
//'   Returns a 2x2 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function (\eqn{-\ell(\theta|\mathbf{x})}). The components
//' are the negative of the second derivatives of the log-likelihood \eqn{\ell}
//' (derived from the PDF in \code{\link{dkw}}).
//'
//' Let \eqn{v_i = 1 - x_i^{\alpha}}. The second derivatives of the positive log-likelihood (\eqn{\ell}) are:
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \alpha^2} = -\frac{n}{\alpha^2} -
//' (\beta-1)\sum_{i=1}^{n}\frac{x_i^{\alpha}(\ln(x_i))^2}{v_i^2}
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \alpha \partial \beta} = -
//' \sum_{i=1}^{n}\frac{x_i^{\alpha}\ln(x_i)}{v_i}
//' }
//' \deqn{
//' \frac{\partial^2 \ell}{\partial \beta^2} = -\frac{n}{\beta^2}
//' }
//' The function returns the Hessian matrix containing the negative of these values.
//'
//' Key properties of the returned matrix:
//' \itemize{
//'   \item Dimensions: 2x2.
//'   \item Symmetry: The matrix is symmetric.
//'   \item Ordering: Rows and columns correspond to the parameters in the order
//'     \eqn{\alpha, \beta}.
//'   \item Content: Analytic second derivatives of the *negative* log-likelihood.
//' }
//' This corresponds to the relevant 2x2 submatrix of the 5x5 GKw Hessian (\code{\link{hsgkw}})
//' evaluated at \eqn{\gamma=1, \delta=0, \lambda=1}.
//'
//' @references
//' Kumaraswamy, P. (1980). A generalized probability density function for
//' double-bounded random processes. *Journal of Hydrology*, *46*(1-2), 79-88.
//'
//'
//' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution
//' with some tractability advantages. *Statistical Methodology*, *6*(1), 70-81.
//'
//' (Note: Specific Hessian formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{hsgkw}} (parent distribution Hessian),
//' \code{\link{llkw}} (negative log-likelihood for Kw),
//' \code{grkw} (gradient for Kw, if available),
//' \code{\link{dkw}} (density for Kw),
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison).
//'
//' @examples
//' \donttest{
//' # Assuming existence of rkw, llkw, grkw, hskw functions for Kw
//'
//' # Generate sample data
//' set.seed(123)
//' true_par_kw <- c(alpha = 2, beta = 3)
//' sample_data_kw <- rkw(100, alpha = true_par_kw[1], beta = true_par_kw[2])
//' hist(sample_data_kw, breaks = 20, main = "Kw(2, 3) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_kw <- c(1.5, 2.5)
//' mle_result_kw <- stats::optim(par = start_par_kw,
//'                               fn = llkw,
//'                               gr = if (exists("grkw")) grkw else NULL,
//'                               method = "L-BFGS-B",
//'                               lower = c(1e-6, 1e-6),
//'                               hessian = TRUE, # Ask optim for numerical Hessian
//'                               data = sample_data_kw)
//'
//' # --- Compare analytical Hessian to numerical Hessian ---
//' if (mle_result_kw$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("hskw")) {
//'
//'   mle_par_kw <- mle_result_kw$par
//'   cat("\nComparing Hessians for Kw at MLE estimates:\n")
//'
//'   # Numerical Hessian of llkw
//'   num_hess_kw <- numDeriv::hessian(func = llkw, x = mle_par_kw, data = sample_data_kw)
//'
//'   # Analytical Hessian from hskw
//'   ana_hess_kw <- hskw(par = mle_par_kw, data = sample_data_kw)
//'
//'   cat("Numerical Hessian (Kw):\n")
//'   print(round(num_hess_kw, 4))
//'   cat("Analytical Hessian (Kw):\n")
//'   print(round(ana_hess_kw, 4))
//'
//'   # Check differences
//'   cat("Max absolute difference between Kw Hessians:\n")
//'   print(max(abs(num_hess_kw - ana_hess_kw)))
//'
//'   # Optional: Use analytical Hessian for Standard Errors
//'   # tryCatch({
//'   #   cov_matrix_kw <- solve(ana_hess_kw) # ana_hess_kw is already Hessian of negative LL
//'   #   std_errors_kw <- sqrt(diag(cov_matrix_kw))
//'   #   cat("Std. Errors from Analytical Kw Hessian:\n")
//'   #   print(std_errors_kw)
//'   # }, error = function(e) {
//'   #   warning("Could not invert analytical Kw Hessian: ", e$message)
//'   # })
//'
//' } else {
//'   cat("\nSkipping Kw Hessian comparison.\n")
//'   cat("Requires convergence, 'numDeriv' package, and function 'hskw'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hskw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data) {
// Parameter extraction
double alpha = par[0];   // Shape parameter α > 0
double beta = par[1];    // Shape parameter β > 0

// Initialize Hessian matrix
Rcpp::NumericMatrix hess(2, 2);

// Parameter validation
if (alpha <= 0 || beta <= 0) {
 hess.fill(R_NaN);
 return hess;
}

// Data conversion and validation
arma::vec x = Rcpp::as<arma::vec>(data);

if (arma::any(x <= 0) || arma::any(x >= 1)) {
 hess.fill(R_NaN);
 return hess;
}

int n = x.n_elem;  // Sample size

// Small constant to avoid numerical issues
double eps = std::numeric_limits<double>::epsilon() * 100;

// Compute transformations and intermediate values
arma::vec log_x = arma::log(x);                  // log(x_i)
arma::vec log_x_squared = arma::square(log_x);   // (log(x_i))²
arma::vec x_alpha = arma::pow(x, alpha);         // x_i^α
arma::vec x_alpha_log_x = x_alpha % log_x;       // x_i^α * log(x_i)

// v_i = 1 - x_i^α
arma::vec v = 1.0 - x_alpha;
v = arma::clamp(v, eps, 1.0 - eps);              // Prevent numerical issues

// Additional terms for second derivatives
arma::vec term_ratio = x_alpha / v;              // x_i^α / (1-x_i^α)
arma::vec term_combined = 1.0 + term_ratio;      // 1 + x_i^α/(1-x_i^α)

// Calculate the Hessian components for negative log-likelihood

// H[0,0] = ∂²ℓ/∂α² = -n/α² - Σᵢ[(β-1)x_i^α(log(x_i))²/(1-x_i^α)(1 + x_i^α/(1-x_i^α))]
double h_alpha_alpha = -n / (alpha * alpha);
arma::vec d2a_terms = (beta - 1.0) * x_alpha % log_x_squared % term_combined / v;
h_alpha_alpha -= arma::sum(d2a_terms);

// H[0,1] = H[1,0] = ∂²ℓ/∂α∂β = -Σᵢ[x_i^α*log(x_i)/(1-x_i^α)]
double h_alpha_beta = -arma::sum(x_alpha_log_x / v);

// H[1,1] = ∂²ℓ/∂β² = -n/β²
double h_beta_beta = -n / (beta * beta);

// Fill the Hessian matrix (symmetric)
hess(0, 0) = -h_alpha_alpha;
hess(0, 1) = hess(1, 0) = -h_alpha_beta;
hess(1, 1) = -h_beta_beta;

return hess;
}


/*
----------------------------------------------------------------------------
NOTE:
We assume the same numeric stability functions from a core or gkwdist.cpp:
- log1mexp(double)
- safe_log(double)
- safe_exp(double)
- safe_pow(double,double)
etc.
They are not redefined here to avoid duplication.
*/

/*
----------------------------------------------------------------------------
BETA DISTRIBUTION: Beta(γ, δ)
----------------------------------------------------------------------------

We use parameters gamma (γ) and delta (δ), both > 0, consistent with GKw family.
Domain: x in (0,1).

* PDF:
f(x;γ,δ) = x^(γ-1) * (1-x)^δ / B(γ,δ+1),  for 0<x<1.

* CDF:
F(x;γ,δ) = pbeta(x, γ, δ+1).

* QUANTILE:
Q(p;γ,δ) = qbeta(p, γ, δ+1).

* RNG:
X = rbeta(γ, δ+1).

* NEGATIVE LOG-LIKELIHOOD:
For data x_i in (0,1),
log f(x_i) = (γ-1)*log(x_i) + δ*log(1-x_i) - ln B(γ,δ+1).
Summation => negative => used in MLE.

*/


// -----------------------------------------------------------------------------
// Parameter checker for Beta(gamma>0, delta>0)
// -----------------------------------------------------------------------------
inline bool check_beta_pars(double gamma, double delta, bool strict=false) {
if (gamma <= 0.0 || delta <= 0.0) {
  return false;
}
if (strict) {
  const double MINP = 1e-7, MAXP = 1e7;
  if (gamma < MINP || delta < MINP) return false;
  if (gamma > MAXP || delta > MAXP) return false;
}
return true;
}


// -----------------------------------------------------------------------------
// 1) dbeta_: PDF for Beta distribution
// -----------------------------------------------------------------------------

//' @title Density of the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution density beta
//'
//' @description
//' Computes the probability density function (PDF) for the standard Beta
//' distribution, using a parameterization common in generalized distribution
//' families. The distribution is parameterized by \code{gamma} (\eqn{\gamma}) and
//' \code{delta} (\eqn{\delta}), corresponding to the standard Beta distribution
//' with shape parameters \code{shape1 = gamma} and \code{shape2 = delta + 1}.
//' The distribution is defined on the interval (0, 1).
//'
//' @param x Vector of quantiles (values between 0 and 1).
//' @param gamma First shape parameter (\code{shape1}), \eqn{\gamma > 0}. Can be a
//'   scalar or a vector. Default: 1.0.
//' @param delta Second shape parameter is \code{delta + 1} (\code{shape2}), requires
//'   \eqn{\delta \ge 0} so that \code{shape2 >= 1}. Can be a scalar or a vector.
//'   Default: 0.0 (leading to \code{shape2 = 1}).
//' @param log_prob Logical; if \code{TRUE}, the logarithm of the density is
//'   returned (\eqn{\log(f(x))}). Default: \code{FALSE}.
//'
//' @return A vector of density values (\eqn{f(x)}) or log-density values
//'   (\eqn{\log(f(x))}). The length of the result is determined by the recycling
//'   rule applied to the arguments (\code{x}, \code{gamma}, \code{delta}).
//'   Returns \code{0} (or \code{-Inf} if \code{log_prob = TRUE}) for \code{x}
//'   outside the interval (0, 1), or \code{NaN} if parameters are invalid
//'   (e.g., \code{gamma <= 0}, \code{delta < 0}).
//'
//' @details
//' The probability density function (PDF) calculated by this function corresponds
//' to a standard Beta distribution \eqn{Beta(\gamma, \delta+1)}:
//' \deqn{
//' f(x; \gamma, \delta) = \frac{x^{\gamma-1} (1-x)^{(\delta+1)-1}}{B(\gamma, \delta+1)} = \frac{x^{\gamma-1} (1-x)^{\delta}}{B(\gamma, \delta+1)}
//' }
//' for \eqn{0 < x < 1}, where \eqn{B(a,b)} is the Beta function
//' (\code{\link[base]{beta}}).
//'
//' This specific parameterization arises as a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{dgkw}}) obtained
//' by setting the parameters \eqn{\alpha = 1}, \eqn{\beta = 1}, and \eqn{\lambda = 1}.
//' It is therefore equivalent to the McDonald (Mc)/Beta Power distribution
//' (\code{\link{dmc}}) with \eqn{\lambda = 1}.
//'
//' Note the difference in the second parameter compared to \code{\link[stats]{dbeta}},
//' where \code{dbeta(x, shape1, shape2)} uses \code{shape2} directly. Here,
//' \code{shape1 = gamma} and \code{shape2 = delta + 1}.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' @seealso
//' \code{\link[stats]{dbeta}} (standard R implementation),
//' \code{\link{dgkw}} (parent distribution density),
//' \code{\link{dmc}} (McDonald/Beta Power density),
//' \code{pbeta_}, \code{qbeta_}, \code{rbeta_} (other functions for this parameterization, if they exist).
//'
//' @examples
//' \donttest{
//' # Example values
//' x_vals <- c(0.2, 0.5, 0.8)
//' gamma_par <- 2.0 # Corresponds to shape1
//' delta_par <- 3.0 # Corresponds to shape2 - 1
//' shape1 <- gamma_par
//' shape2 <- delta_par + 1
//'
//' # Calculate density using dbeta_
//' densities <- dbeta_(x_vals, gamma_par, delta_par)
//' print(densities)
//'
//' # Compare with stats::dbeta
//' densities_stats <- stats::dbeta(x_vals, shape1 = shape1, shape2 = shape2)
//' print(paste("Max difference vs stats::dbeta:", max(abs(densities - densities_stats))))
//'
//' # Compare with dgkw setting alpha=1, beta=1, lambda=1
//' densities_gkw <- dgkw(x_vals, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                       delta = delta_par, lambda = 1.0)
//' print(paste("Max difference vs dgkw:", max(abs(densities - densities_gkw))))
//'
//' # Compare with dmc setting lambda=1
//' densities_mc <- dmc(x_vals, gamma = gamma_par, delta = delta_par, lambda = 1.0)
//' print(paste("Max difference vs dmc:", max(abs(densities - densities_mc))))
//'
//' # Calculate log-density
//' log_densities <- dbeta_(x_vals, gamma_par, delta_par, log_prob = TRUE)
//' print(log_densities)
//' print(stats::dbeta(x_vals, shape1 = shape1, shape2 = shape2, log = TRUE))
//'
//' # Plot the density
//' curve_x <- seq(0.001, 0.999, length.out = 200)
//' curve_y <- dbeta_(curve_x, gamma = 2, delta = 3) # Beta(2, 4)
//' plot(curve_x, curve_y, type = "l", main = "Beta(2, 4) Density via dbeta_",
//'      xlab = "x", ylab = "f(x)", col = "blue")
//' curve(stats::dbeta(x, 2, 4), add=TRUE, col="red", lty=2)
//' legend("topright", legend=c("dbeta_(gamma=2, delta=3)", "stats::dbeta(shape1=2, shape2=4)"),
//'        col=c("blue", "red"), lty=c(1,2), bty="n")
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dbeta_(
 const arma::vec& x,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 bool log_prob = false
) {
arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());

size_t N = std::max({x.n_elem, g_vec.n_elem, d_vec.n_elem});
arma::vec out(N);
out.fill(log_prob ? R_NegInf : 0.0);

for (size_t i = 0; i < N; i++) {
 double g = g_vec[i % g_vec.n_elem];
 double d = d_vec[i % d_vec.n_elem];
 double xx = x[i % x.n_elem];

 if (!check_beta_pars(g, d)) {
   continue; // => 0 or -Inf
 }
 if (xx <= 0.0 || xx >= 1.0 || !R_finite(xx)) {
   continue;
 }

 // logBeta = R::lbeta(g, d+1)
 double lB = R::lbeta(g, d + 1.0);
 // log pdf = (g-1)*log(x) + d*log(1-x) - lB
 double lx = std::log(xx);
 double one_minus_x = 1.0 - xx;
 if (one_minus_x <= 0.0) {
   // => out of domain, effectively => 0
   continue;
 }
 double log_1_minus_x = std::log(one_minus_x);

 double log_pdf = (g - 1.0) * lx + d * log_1_minus_x - lB;

 if (log_prob) {
   out(i) = log_pdf;
 } else {
   out(i) = std::exp(log_pdf);
 }
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 2) pbeta_: CDF for Beta
// -----------------------------------------------------------------------------


//' @title CDF of the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution cumulative beta
//'
//' @description
//' Computes the cumulative distribution function (CDF), \eqn{F(q) = P(X \le q)},
//' for the standard Beta distribution, using a parameterization common in
//' generalized distribution families. The distribution is parameterized by
//' \code{gamma} (\eqn{\gamma}) and \code{delta} (\eqn{\delta}), corresponding to
//' the standard Beta distribution with shape parameters \code{shape1 = gamma}
//' and \code{shape2 = delta + 1}.
//'
//' @param q Vector of quantiles (values generally between 0 and 1).
//' @param gamma First shape parameter (\code{shape1}), \eqn{\gamma > 0}. Can be a
//'   scalar or a vector. Default: 1.0.
//' @param delta Second shape parameter is \code{delta + 1} (\code{shape2}), requires
//'   \eqn{\delta \ge 0} so that \code{shape2 >= 1}. Can be a scalar or a vector.
//'   Default: 0.0 (leading to \code{shape2 = 1}).
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are
//'   \eqn{P(X \le q)}, otherwise, \eqn{P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \eqn{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of probabilities, \eqn{F(q)}, or their logarithms/complements
//'   depending on \code{lower_tail} and \code{log_p}. The length of the result
//'   is determined by the recycling rule applied to the arguments (\code{q},
//'   \code{gamma}, \code{delta}). Returns \code{0} (or \code{-Inf} if
//'   \code{log_p = TRUE}) for \code{q <= 0} and \code{1} (or \code{0} if
//'   \code{log_p = TRUE}) for \code{q >= 1}. Returns \code{NaN} for invalid
//'   parameters.
//'
//' @details
//' This function computes the CDF of a Beta distribution with parameters
//' \code{shape1 = gamma} and \code{shape2 = delta + 1}. It is equivalent to
//' calling \code{stats::pbeta(q, shape1 = gamma, shape2 = delta + 1,
//' lower.tail = lower_tail, log.p = log_p)}.
//'
//' This distribution arises as a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{pgkw}}) obtained
//' by setting \eqn{\alpha = 1}, \eqn{\beta = 1}, and \eqn{\lambda = 1}.
//' It is therefore also equivalent to the McDonald (Mc)/Beta Power distribution
//' (\code{\link{pmc}}) with \eqn{\lambda = 1}.
//'
//' The function likely calls R's underlying \code{pbeta} function but ensures
//' consistent parameter recycling and handling within the C++ environment,
//' matching the style of other functions in the related families.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' @seealso
//' \code{\link[stats]{pbeta}} (standard R implementation),
//' \code{\link{pgkw}} (parent distribution CDF),
//' \code{\link{pmc}} (McDonald/Beta Power CDF),
//' \code{dbeta_}, \code{qbeta_}, \code{rbeta_} (other functions for this parameterization, if they exist).
//'
//' @examples
//' \donttest{
//' # Example values
//' q_vals <- c(0.2, 0.5, 0.8)
//' gamma_par <- 2.0 # Corresponds to shape1
//' delta_par <- 3.0 # Corresponds to shape2 - 1
//' shape1 <- gamma_par
//' shape2 <- delta_par + 1
//'
//' # Calculate CDF using pbeta_
//' probs <- pbeta_(q_vals, gamma_par, delta_par)
//' print(probs)
//'
//' # Compare with stats::pbeta
//' probs_stats <- stats::pbeta(q_vals, shape1 = shape1, shape2 = shape2)
//' print(paste("Max difference vs stats::pbeta:", max(abs(probs - probs_stats))))
//'
//' # Compare with pgkw setting alpha=1, beta=1, lambda=1
//' probs_gkw <- pgkw(q_vals, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                   delta = delta_par, lambda = 1.0)
//' print(paste("Max difference vs pgkw:", max(abs(probs - probs_gkw))))
//'
//' # Compare with pmc setting lambda=1
//' probs_mc <- pmc(q_vals, gamma = gamma_par, delta = delta_par, lambda = 1.0)
//' print(paste("Max difference vs pmc:", max(abs(probs - probs_mc))))
//'
//' # Calculate upper tail P(X > q)
//' probs_upper <- pbeta_(q_vals, gamma_par, delta_par, lower_tail = FALSE)
//' print(probs_upper)
//' print(stats::pbeta(q_vals, shape1, shape2, lower.tail = FALSE))
//'
//' # Calculate log CDF
//' log_probs <- pbeta_(q_vals, gamma_par, delta_par, log_p = TRUE)
//' print(log_probs)
//' print(stats::pbeta(q_vals, shape1, shape2, log.p = TRUE))
//'
//' # Plot the CDF
//' curve_q <- seq(0.001, 0.999, length.out = 200)
//' curve_p <- pbeta_(curve_q, gamma = 2, delta = 3) # Beta(2, 4)
//' plot(curve_q, curve_p, type = "l", main = "Beta(2, 4) CDF via pbeta_",
//'      xlab = "q", ylab = "F(q)", col = "blue")
//' curve(stats::pbeta(x, 2, 4), add=TRUE, col="red", lty=2)
//' legend("bottomright", legend=c("pbeta_(gamma=2, delta=3)", "stats::pbeta(shape1=2, shape2=4)"),
//'        col=c("blue", "red"), lty=c(1,2), bty="n")
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pbeta_(
 const arma::vec& q,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());

size_t N = std::max({q.n_elem, g_vec.n_elem, d_vec.n_elem});
arma::vec out(N);

for (size_t i = 0; i < N; i++) {
 double g = g_vec[i % g_vec.n_elem];
 double d = d_vec[i % d_vec.n_elem];
 double qq = q[i % q.n_elem];

 if (!check_beta_pars(g, d)) {
   out(i) = NA_REAL;
   continue;
 }

 // boundary
 if (!R_finite(qq) || qq <= 0.0) {
   double v0 = lower_tail ? 0.0 : 1.0;
   out(i) = (log_p ? std::log(v0) : v0);
   continue;
 }
 if (qq >= 1.0) {
   double v1 = lower_tail ? 1.0 : 0.0;
   out(i) = (log_p ? std::log(v1) : v1);
   continue;
 }

 // call R's pbeta with adjusted parameters for GKw-style Beta
 double val = R::pbeta(qq, g, d + 1.0, lower_tail, false); // not log
 if (log_p) {
   val = std::log(val);
 }
 out(i) = val;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 3) qbeta_: Quantile function for Beta
// -----------------------------------------------------------------------------

//' @title Quantile Function of the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution quantile beta
//'
//' @description
//' Computes the quantile function (inverse CDF) for the standard Beta
//' distribution, using a parameterization common in generalized distribution
//' families. It finds the value \code{q} such that \eqn{P(X \le q) = p}. The
//' distribution is parameterized by \code{gamma} (\eqn{\gamma}) and \code{delta}
//' (\eqn{\delta}), corresponding to the standard Beta distribution with shape
//' parameters \code{shape1 = gamma} and \code{shape2 = delta + 1}.
//'
//' @param p Vector of probabilities (values between 0 and 1).
//' @param gamma First shape parameter (\code{shape1}), \eqn{\gamma > 0}. Can be a
//'   scalar or a vector. Default: 1.0.
//' @param delta Second shape parameter is \code{delta + 1} (\code{shape2}), requires
//'   \eqn{\delta \ge 0} so that \code{shape2 >= 1}. Can be a scalar or a vector.
//'   Default: 0.0 (leading to \code{shape2 = 1}).
//' @param lower_tail Logical; if \code{TRUE} (default), probabilities are \eqn{p = P(X \le q)},
//'   otherwise, probabilities are \eqn{p = P(X > q)}.
//' @param log_p Logical; if \code{TRUE}, probabilities \code{p} are given as
//'   \eqn{\log(p)}. Default: \code{FALSE}.
//'
//' @return A vector of quantiles corresponding to the given probabilities \code{p}.
//'   The length of the result is determined by the recycling rule applied to
//'   the arguments (\code{p}, \code{gamma}, \code{delta}).
//'   Returns:
//'   \itemize{
//'     \item \code{0} for \code{p = 0} (or \code{p = -Inf} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{1} for \code{p = 1} (or \code{p = 0} if \code{log_p = TRUE},
//'           when \code{lower_tail = TRUE}).
//'     \item \code{NaN} for \code{p < 0} or \code{p > 1} (or corresponding log scale).
//'     \item \code{NaN} for invalid parameters (e.g., \code{gamma <= 0},
//'           \code{delta < 0}).
//'   }
//'   Boundary return values are adjusted accordingly for \code{lower_tail = FALSE}.
//'
//' @details
//' This function computes the quantiles of a Beta distribution with parameters
//' \code{shape1 = gamma} and \code{shape2 = delta + 1}. It is equivalent to
//' calling \code{stats::qbeta(p, shape1 = gamma, shape2 = delta + 1,
//' lower.tail = lower_tail, log.p = log_p)}.
//'
//' This distribution arises as a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{qgkw}}) obtained
//' by setting \eqn{\alpha = 1}, \eqn{\beta = 1}, and \eqn{\lambda = 1}.
//' It is therefore also equivalent to the McDonald (Mc)/Beta Power distribution
//' (\code{\link{qmc}}) with \eqn{\lambda = 1}.
//'
//' The function likely calls R's underlying \code{qbeta} function but ensures
//' consistent parameter recycling and handling within the C++ environment,
//' matching the style of other functions in the related families. Boundary
//' conditions (p=0, p=1) are handled explicitly.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' @seealso
//' \code{\link[stats]{qbeta}} (standard R implementation),
//' \code{\link{qgkw}} (parent distribution quantile function),
//' \code{\link{qmc}} (McDonald/Beta Power quantile function),
//' \code{dbeta_}, \code{pbeta_}, \code{rbeta_} (other functions for this parameterization, if they exist).
//'
//' @examples
//' \donttest{
//' # Example values
//' p_vals <- c(0.1, 0.5, 0.9)
//' gamma_par <- 2.0 # Corresponds to shape1
//' delta_par <- 3.0 # Corresponds to shape2 - 1
//' shape1 <- gamma_par
//' shape2 <- delta_par + 1
//'
//' # Calculate quantiles using qbeta_
//' quantiles <- qbeta_(p_vals, gamma_par, delta_par)
//' print(quantiles)
//'
//' # Compare with stats::qbeta
//' quantiles_stats <- stats::qbeta(p_vals, shape1 = shape1, shape2 = shape2)
//' print(paste("Max difference vs stats::qbeta:", max(abs(quantiles - quantiles_stats))))
//'
//' # Compare with qgkw setting alpha=1, beta=1, lambda=1
//' quantiles_gkw <- qgkw(p_vals, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                       delta = delta_par, lambda = 1.0)
//' print(paste("Max difference vs qgkw:", max(abs(quantiles - quantiles_gkw))))
//'
//' # Compare with qmc setting lambda=1
//' quantiles_mc <- qmc(p_vals, gamma = gamma_par, delta = delta_par, lambda = 1.0)
//' print(paste("Max difference vs qmc:", max(abs(quantiles - quantiles_mc))))
//'
//' # Calculate quantiles for upper tail
//' quantiles_upper <- qbeta_(p_vals, gamma_par, delta_par, lower_tail = FALSE)
//' print(quantiles_upper)
//' print(stats::qbeta(p_vals, shape1, shape2, lower.tail = FALSE))
//'
//' # Calculate quantiles from log probabilities
//' log_p_vals <- log(p_vals)
//' quantiles_logp <- qbeta_(log_p_vals, gamma_par, delta_par, log_p = TRUE)
//' print(quantiles_logp)
//' print(stats::qbeta(log_p_vals, shape1, shape2, log.p = TRUE))
//'
//' # Verify inverse relationship with pbeta_
//' p_check <- 0.75
//' q_calc <- qbeta_(p_check, gamma_par, delta_par)
//' p_recalc <- pbeta_(q_calc, gamma_par, delta_par)
//' print(paste("Original p:", p_check, " Recalculated p:", p_recalc))
//' # abs(p_check - p_recalc) < 1e-9 # Should be TRUE
//'
//' # Boundary conditions
//' print(qbeta_(c(0, 1), gamma_par, delta_par)) # Should be 0, 1
//' print(qbeta_(c(-Inf, 0), gamma_par, delta_par, log_p = TRUE)) # Should be 0, 1
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qbeta_(
 const arma::vec& p,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta,
 bool lower_tail = true,
 bool log_p = false
) {
arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());

size_t N = std::max({p.n_elem, g_vec.n_elem, d_vec.n_elem});
arma::vec out(N);

for (size_t i = 0; i < N; i++) {
 double g = g_vec[i % g_vec.n_elem];
 double d = d_vec[i % d_vec.n_elem];
 double pp = p[i % p.n_elem];

 if (!check_beta_pars(g, d)) {
   out(i) = NA_REAL;
   continue;
 }

 // handle log_p
 if (log_p) {
   if (pp > 0.0) {
     // => p>1
     out(i) = NA_REAL;
     continue;
   }
   pp = std::exp(pp);
 }
 // handle lower_tail
 if (!lower_tail) {
   pp = 1.0 - pp;
 }

 // boundaries
 if (pp <= 0.0) {
   out(i) = 0.0;
   continue;
 }
 if (pp >= 1.0) {
   out(i) = 1.0;
   continue;
 }

 // Use adjusted parameters for GKw-style Beta
 double val = R::qbeta(pp, g, d + 1.0, true, false); // returns not log
 out(i) = val;
}

return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 4) rbeta_: RNG for Beta distribution
// -----------------------------------------------------------------------------

//' @title Random Generation for the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution random beta
//'
//' @description
//' Generates random deviates from the standard Beta distribution, using a
//' parameterization common in generalized distribution families. The distribution
//' is parameterized by \code{gamma} (\eqn{\gamma}) and \code{delta} (\eqn{\delta}),
//' corresponding to the standard Beta distribution with shape parameters
//' \code{shape1 = gamma} and \code{shape2 = delta + 1}. This is a special case
//' of the Generalized Kumaraswamy (GKw) distribution where \eqn{\alpha = 1},
//' \eqn{\beta = 1}, and \eqn{\lambda = 1}.
//'
//' @param n Number of observations. If \code{length(n) > 1}, the length is
//'   taken to be the number required. Must be a non-negative integer.
//' @param gamma First shape parameter (\code{shape1}), \eqn{\gamma > 0}. Can be a
//'   scalar or a vector. Default: 1.0.
//' @param delta Second shape parameter is \code{delta + 1} (\code{shape2}), requires
//'   \eqn{\delta \ge 0} so that \code{shape2 >= 1}. Can be a scalar or a vector.
//'   Default: 0.0 (leading to \code{shape2 = 1}, i.e., Uniform).
//'
//' @return A numeric vector of length \code{n} containing random deviates from the
//'   Beta(\eqn{\gamma, \delta+1}) distribution, with values in (0, 1). The length
//'   of the result is determined by \code{n} and the recycling rule applied to
//'   the parameters (\code{gamma}, \code{delta}). Returns \code{NaN} if parameters
//'   are invalid (e.g., \code{gamma <= 0}, \code{delta < 0}).
//'
//' @details
//' This function generates samples from a Beta distribution with parameters
//' \code{shape1 = gamma} and \code{shape2 = delta + 1}. It is equivalent to
//' calling \code{stats::rbeta(n, shape1 = gamma, shape2 = delta + 1)}.
//'
//' This distribution arises as a special case of the five-parameter
//' Generalized Kumaraswamy (GKw) distribution (\code{\link{rgkw}}) obtained
//' by setting \eqn{\alpha = 1}, \eqn{\beta = 1}, and \eqn{\lambda = 1}.
//' It is therefore also equivalent to the McDonald (Mc)/Beta Power distribution
//' (\code{\link{rmc}}) with \eqn{\lambda = 1}.
//'
//' The function likely calls R's underlying \code{rbeta} function but ensures
//' consistent parameter recycling and handling within the C++ environment,
//' matching the style of other functions in the related families.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' Devroye, L. (1986). *Non-Uniform Random Variate Generation*. Springer-Verlag.
//'
//' @seealso
//' \code{\link[stats]{rbeta}} (standard R implementation),
//' \code{\link{rgkw}} (parent distribution random generation),
//' \code{\link{rmc}} (McDonald/Beta Power random generation),
//' \code{dbeta_}, \code{pbeta_}, \code{qbeta_} (other functions for this parameterization, if they exist).
//'
//' @examples
//' \donttest{
//' set.seed(2030) # for reproducibility
//'
//' # Generate 1000 samples using rbeta_
//' gamma_par <- 2.0 # Corresponds to shape1
//' delta_par <- 3.0 # Corresponds to shape2 - 1
//' shape1 <- gamma_par
//' shape2 <- delta_par + 1
//'
//' x_sample <- rbeta_(1000, gamma = gamma_par, delta = delta_par)
//' summary(x_sample)
//'
//' # Compare with stats::rbeta
//' x_sample_stats <- stats::rbeta(1000, shape1 = shape1, shape2 = shape2)
//' # Visually compare histograms or QQ-plots
//' hist(x_sample, main="rbeta_ Sample", freq=FALSE, breaks=30)
//' curve(dbeta_(x, gamma_par, delta_par), add=TRUE, col="red", lwd=2)
//' hist(x_sample_stats, main="stats::rbeta Sample", freq=FALSE, breaks=30)
//' curve(stats::dbeta(x, shape1, shape2), add=TRUE, col="blue", lwd=2)
//' # Compare summary stats (should be similar due to randomness)
//' print(summary(x_sample))
//' print(summary(x_sample_stats))
//'
//' # Compare summary stats with rgkw(alpha=1, beta=1, lambda=1)
//' x_sample_gkw <- rgkw(1000, alpha = 1.0, beta = 1.0, gamma = gamma_par,
//'                      delta = delta_par, lambda = 1.0)
//' print("Summary stats for rgkw(a=1,b=1,l=1) sample:")
//' print(summary(x_sample_gkw))
//'
//' # Compare summary stats with rmc(lambda=1)
//' x_sample_mc <- rmc(1000, gamma = gamma_par, delta = delta_par, lambda = 1.0)
//' print("Summary stats for rmc(l=1) sample:")
//' print(summary(x_sample_mc))
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rbeta_(
 int n,
 const Rcpp::NumericVector& gamma,
 const Rcpp::NumericVector& delta
) {
if (n <= 0) {
 Rcpp::stop("rbeta_: n must be positive");
}

arma::vec g_vec(gamma.begin(), gamma.size());
arma::vec d_vec(delta.begin(), delta.size());

size_t k = std::max({g_vec.n_elem, d_vec.n_elem});
arma::vec out(n);

for (int i = 0; i < n; i++) {
 size_t idx = i % k;
 double g = g_vec[idx % g_vec.n_elem];
 double d = d_vec[idx % d_vec.n_elem];

 if (!check_beta_pars(g, d)) {
   out(i) = NA_REAL;
   Rcpp::warning("rbeta_: invalid parameters at index %d", i+1);
   continue;
 }

 // Use adjusted parameters for GKw-style Beta
 double val = R::rbeta(g, d + 1.0);
 out(i) = val;
}
return Rcpp::NumericVector(out.memptr(), out.memptr() + out.n_elem);
}


// -----------------------------------------------------------------------------
// 5) llbeta: Negative Log-Likelihood for Beta
// -----------------------------------------------------------------------------

//' @title Negative Log-Likelihood for the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize beta
//'
//' @description
//' Computes the negative log-likelihood function for the standard Beta
//' distribution, using a parameterization common in generalized distribution
//' families. The distribution is parameterized by \code{gamma} (\eqn{\gamma}) and
//' \code{delta} (\eqn{\delta}), corresponding to the standard Beta distribution
//' with shape parameters \code{shape1 = gamma} and \code{shape2 = delta + 1}.
//' This function is suitable for maximum likelihood estimation.
//'
//' @param par A numeric vector of length 2 containing the distribution parameters
//'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a single \code{double} value representing the negative
//'   log-likelihood (\eqn{-\ell(\theta|\mathbf{x})}). Returns \code{Inf}
//'   if any parameter values in \code{par} are invalid according to their
//'   constraints, or if any value in \code{data} is not in the interval (0, 1).
//'
//' @details
//' This function calculates the negative log-likelihood for a Beta distribution
//' with parameters \code{shape1 = gamma} (\eqn{\gamma}) and \code{shape2 = delta + 1} (\eqn{\delta+1}).
//' The probability density function (PDF) is:
//' \deqn{
//' f(x | \gamma, \delta) = \frac{x^{\gamma-1} (1-x)^{\delta}}{B(\gamma, \delta+1)}
//' }
//' for \eqn{0 < x < 1}, where \eqn{B(a,b)} is the Beta function (\code{\link[base]{beta}}).
//' The log-likelihood function \eqn{\ell(\theta | \mathbf{x})} for a sample
//' \eqn{\mathbf{x} = (x_1, \dots, x_n)} is \eqn{\sum_{i=1}^n \ln f(x_i | \theta)}:
//' \deqn{
//' \ell(\theta | \mathbf{x}) = \sum_{i=1}^{n} [(\gamma-1)\ln(x_i) + \delta\ln(1-x_i)] - n \ln B(\gamma, \delta+1)
//' }
//' where \eqn{\theta = (\gamma, \delta)}.
//' This function computes and returns the *negative* log-likelihood, \eqn{-\ell(\theta|\mathbf{x})},
//' suitable for minimization using optimization routines like \code{\link[stats]{optim}}.
//' It is equivalent to the negative log-likelihood of the GKw distribution
//' (\code{\link{llgkw}}) evaluated at \eqn{\alpha=1, \beta=1, \lambda=1}, and also
//' to the negative log-likelihood of the McDonald distribution (\code{\link{llmc}})
//' evaluated at \eqn{\lambda=1}. The term \eqn{\ln B(\gamma, \delta+1)} is typically
//' computed using log-gamma functions (\code{\link[base]{lgamma}}) for numerical stability.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//'
//' @seealso
//' \code{\link{llgkw}}, \code{\link{llmc}} (related negative log-likelihoods),
//' \code{dbeta_}, \code{pbeta_}, \code{qbeta_}, \code{rbeta_},
//' \code{grbeta} (gradient, if available),
//' \code{hsbeta} (Hessian, if available),
//' \code{\link[stats]{optim}}, \code{\link[base]{lbeta}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rbeta_, llbeta, grbeta, hsbeta functions
//'
//' # Generate sample data from a Beta(2, 4) distribution
//' # (gamma=2, delta=3 in this parameterization)
//' set.seed(123)
//' true_par_beta <- c(gamma = 2, delta = 3)
//' sample_data_beta <- rbeta_(100, gamma = true_par_beta[1], delta = true_par_beta[2])
//' hist(sample_data_beta, breaks = 20, main = "Beta(2, 4) Sample")
//'
//' # --- Maximum Likelihood Estimation using optim ---
//' # Initial parameter guess
//' start_par_beta <- c(1.5, 2.5)
//'
//' # Perform optimization (minimizing negative log-likelihood)
//' # Use method="L-BFGS-B" for box constraints (params > 0 / >= 0)
//' mle_result_beta <- stats::optim(par = start_par_beta,
//'                                fn = llbeta, # Use the custom Beta neg-log-likelihood
//'                                method = "L-BFGS-B",
//'                                lower = c(1e-6, 1e-6), # Bounds: gamma>0, delta>=0
//'                                hessian = TRUE,
//'                                data = sample_data_beta)
//'
//' # Check convergence and results
//' if (mle_result_beta$convergence == 0) {
//'   print("Optimization converged successfully.")
//'   mle_par_beta <- mle_result_beta$par
//'   print("Estimated Beta parameters (gamma, delta):")
//'   print(mle_par_beta)
//'   print("True Beta parameters (gamma, delta):")
//'   print(true_par_beta)
//'   cat(sprintf("MLE corresponds approx to Beta(%.2f, %.2f)\n",
//'       mle_par_beta[1], mle_par_beta[2] + 1))
//'
//' } else {
//'   warning("Optimization did not converge!")
//'   print(mle_result_beta$message)
//' }
//'
//' # --- Compare numerical and analytical derivatives (if available) ---
//' # Requires 'numDeriv' package and analytical functions 'grbeta', 'hsbeta'
//' if (mle_result_beta$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("grbeta") && exists("hsbeta")) {
//'
//'   cat("\nComparing Derivatives at Beta MLE estimates:\n")
//'
//'   # Numerical derivatives of llbeta
//'   num_grad_beta <- numDeriv::grad(func = llbeta, x = mle_par_beta, data = sample_data_beta)
//'   num_hess_beta <- numDeriv::hessian(func = llbeta, x = mle_par_beta, data = sample_data_beta)
//'
//'   # Analytical derivatives (assuming they return derivatives of negative LL)
//'   ana_grad_beta <- grbeta(par = mle_par_beta, data = sample_data_beta)
//'   ana_hess_beta <- hsbeta(par = mle_par_beta, data = sample_data_beta)
//'
//'   # Check differences
//'   cat("Max absolute difference between gradients:\n")
//'   print(max(abs(num_grad_beta - ana_grad_beta)))
//'   cat("Max absolute difference between Hessians:\n")
//'   print(max(abs(num_hess_beta - ana_hess_beta)))
//'
//' } else {
//'    cat("\nSkipping derivative comparison for Beta.\n")
//'    cat("Requires convergence, 'numDeriv' pkg & functions 'grbeta', 'hsbeta'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
double llbeta(const Rcpp::NumericVector& par,
           const Rcpp::NumericVector& data) {
if (par.size() < 2) {
 return R_PosInf;
}
double gamma = par[0]; // gamma > 0
double delta = par[1]; // delta > 0

if (!check_beta_pars(gamma, delta)) {
 return R_PosInf;
}

arma::vec x = Rcpp::as<arma::vec>(data);
if (x.n_elem < 1) {
 return R_PosInf;
}
// domain check
if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
 return R_PosInf;
}

int n = x.n_elem;
// Use correct parameterization for GKw-style Beta
double logB = R::lbeta(gamma, delta + 1.0);
// constant => -n*logB
double cst = -double(n) * logB;

// sum((gamma-1)*log(x_i) + delta*log(1-x_i)), i=1..n
arma::vec lx = arma::log(x);
arma::vec l1mx = arma::log(1.0 - x);

double sum1 = (gamma - 1.0) * arma::sum(lx);
double sum2 = delta * arma::sum(l1mx);  // Corrected: no subtraction of 1.0

double loglike = cst + sum1 + sum2; // that's the log-likelihood

// We must return negative
return -loglike;
}


//' @title Gradient of the Negative Log-Likelihood for the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize gradient beta
//'
//' @description
//' Computes the gradient vector (vector of first partial derivatives) of the
//' negative log-likelihood function for the standard Beta distribution, using
//' a parameterization common in generalized distribution families. The
//' distribution is parameterized by \code{gamma} (\eqn{\gamma}) and \code{delta}
//' (\eqn{\delta}), corresponding to the standard Beta distribution with shape
//' parameters \code{shape1 = gamma} and \code{shape2 = delta + 1}.
//' The gradient is useful for optimization algorithms.
//'
//' @param par A numeric vector of length 2 containing the distribution parameters
//'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a numeric vector of length 2 containing the partial derivatives
//'   of the negative log-likelihood function \eqn{-\ell(\theta | \mathbf{x})} with
//'   respect to each parameter: \eqn{(-\partial \ell/\partial \gamma, -\partial \ell/\partial \delta)}.
//'   Returns a vector of \code{NaN} if any parameter values are invalid according
//'   to their constraints, or if any value in \code{data} is not in the
//'   interval (0, 1).
//'
//' @details
//' This function calculates the gradient of the negative log-likelihood for a
//' Beta distribution with parameters \code{shape1 = gamma} (\eqn{\gamma}) and
//' \code{shape2 = delta + 1} (\eqn{\delta+1}). The components of the gradient
//' vector (\eqn{-\nabla \ell(\theta | \mathbf{x})}) are:
//'
//' \deqn{
//' -\frac{\partial \ell}{\partial \gamma} = n[\psi(\gamma) - \psi(\gamma+\delta+1)] -
//' \sum_{i=1}^{n}\ln(x_i)
//' }
//' \deqn{
//' -\frac{\partial \ell}{\partial \delta} = n[\psi(\delta+1) - \psi(\gamma+\delta+1)] -
//' \sum_{i=1}^{n}\ln(1-x_i)
//' }
//'
//' where \eqn{\psi(\cdot)} is the digamma function (\code{\link[base]{digamma}}).
//' These formulas represent the derivatives of \eqn{-\ell(\theta)}, consistent with
//' minimizing the negative log-likelihood. They correspond to the relevant components
//' of the general GKw gradient (\code{\link{grgkw}}) evaluated at \eqn{\alpha=1, \beta=1, \lambda=1}.
//' Note the parameterization: the standard Beta shape parameters are \eqn{\gamma} and \eqn{\delta+1}.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' (Note: Specific gradient formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{grgkw}}, \code{\link{grmc}} (related gradients),
//' \code{\link{llbeta}} (negative log-likelihood function),
//' \code{hsbeta} (Hessian, if available),
//' \code{dbeta_}, \code{pbeta_}, \code{qbeta_}, \code{rbeta_},
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{grad}} (for numerical gradient comparison),
//' \code{\link[base]{digamma}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rbeta_, llbeta, grbeta, hsbeta functions
//'
//' # Generate sample data from a Beta(2, 4) distribution
//' # (gamma=2, delta=3 in this parameterization)
//' set.seed(123)
//' true_par_beta <- c(gamma = 2, delta = 3)
//' sample_data_beta <- rbeta_(100, gamma = true_par_beta[1], delta = true_par_beta[2])
//' hist(sample_data_beta, breaks = 20, main = "Beta(2, 4) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_beta <- c(1.5, 2.5)
//' mle_result_beta <- stats::optim(par = start_par_beta,
//'                                fn = llbeta,
//'                                gr = grbeta, # Use analytical gradient
//'                                method = "L-BFGS-B",
//'                                lower = c(1e-6, 1e-6), # Bounds: gamma>0, delta>=0
//'                                hessian = TRUE,
//'                                data = sample_data_beta)
//'
//' # --- Compare analytical gradient to numerical gradient ---
//' if (mle_result_beta$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE)) {
//'
//'   mle_par_beta <- mle_result_beta$par
//'   cat("\nComparing Gradients for Beta at MLE estimates:\n")
//'
//'   # Numerical gradient of llbeta
//'   num_grad_beta <- numDeriv::grad(func = llbeta, x = mle_par_beta, data = sample_data_beta)
//'
//'   # Analytical gradient from grbeta
//'   ana_grad_beta <- grbeta(par = mle_par_beta, data = sample_data_beta)
//'
//'   cat("Numerical Gradient (Beta):\n")
//'   print(num_grad_beta)
//'   cat("Analytical Gradient (Beta):\n")
//'   print(ana_grad_beta)
//'
//'   # Check differences
//'   cat("Max absolute difference between Beta gradients:\n")
//'   print(max(abs(num_grad_beta - ana_grad_beta)))
//'
//' } else {
//'   cat("\nSkipping Beta gradient comparison.\n")
//' }
//'
//' # Example with Hessian comparison (if hsbeta exists)
//' if (mle_result_beta$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) && exists("hsbeta")) {
//'
//'   num_hess_beta <- numDeriv::hessian(func = llbeta, x = mle_par_beta, data = sample_data_beta)
//'   ana_hess_beta <- hsbeta(par = mle_par_beta, data = sample_data_beta)
//'   cat("\nMax absolute difference between Beta Hessians:\n")
//'   print(max(abs(num_hess_beta - ana_hess_beta)))
//'
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector grbeta(const Rcpp::NumericVector& par,
                        const Rcpp::NumericVector& data) {
Rcpp::NumericVector grad(2, R_PosInf); // initialize with Inf

if (par.size() < 2) {
 return grad;
}

double gamma = par[0];
double delta = par[1];

if (!check_beta_pars(gamma, delta)) {
 return grad;
}

arma::vec x = Rcpp::as<arma::vec>(data);
if (x.n_elem < 1) {
 return grad;
}

// domain check
if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
 return grad;
}

int n = x.n_elem;

// Calculate digamma terms with correct parameterization for GKw-style Beta
double dig_g = R::digamma(gamma);
double dig_d = R::digamma(delta + 1.0);  // Corrected: digamma(δ+1)
double dig_gd = R::digamma(gamma + delta + 1.0);  // Corrected: digamma(γ+δ+1)

// Sum of log terms
arma::vec lx = arma::log(x);
arma::vec l1mx = arma::log(1.0 - x);
double sum_lx = arma::sum(lx);
double sum_l1mx = arma::sum(l1mx);

// Partial derivatives for negative log-likelihood
grad[0] = n * (dig_g - dig_gd) - sum_lx; // wrt gamma
grad[1] = n * (dig_d - dig_gd) - sum_l1mx; // wrt delta

return grad; // Already negated for negative log-likelihood
}


//' @title Hessian Matrix of the Negative Log-Likelihood for the Beta Distribution (gamma, delta+1 Parameterization)
//' @author Lopes, J. E.
//' @keywords distribution likelihood optimize hessian beta
//'
//' @description
//' Computes the analytic 2x2 Hessian matrix (matrix of second partial derivatives)
//' of the negative log-likelihood function for the standard Beta distribution,
//' using a parameterization common in generalized distribution families. The
//' distribution is parameterized by \code{gamma} (\eqn{\gamma}) and \code{delta}
//' (\eqn{\delta}), corresponding to the standard Beta distribution with shape
//' parameters \code{shape1 = gamma} and \code{shape2 = delta + 1}. The Hessian
//' is useful for estimating standard errors and in optimization algorithms.
//'
//' @param par A numeric vector of length 2 containing the distribution parameters
//'   in the order: \code{gamma} (\eqn{\gamma > 0}), \code{delta} (\eqn{\delta \ge 0}).
//' @param data A numeric vector of observations. All values must be strictly
//'   between 0 and 1 (exclusive).
//'
//' @return Returns a 2x2 numeric matrix representing the Hessian matrix of the
//'   negative log-likelihood function, \eqn{-\partial^2 \ell / (\partial \theta_i \partial \theta_j)},
//'   where \eqn{\theta = (\gamma, \delta)}.
//'   Returns a 2x2 matrix populated with \code{NaN} if any parameter values are
//'   invalid according to their constraints, or if any value in \code{data} is
//'   not in the interval (0, 1).
//'
//' @details
//' This function calculates the analytic second partial derivatives of the
//' negative log-likelihood function (\eqn{-\ell(\theta|\mathbf{x})}) for a Beta
//' distribution with parameters \code{shape1 = gamma} (\eqn{\gamma}) and
//' \code{shape2 = delta + 1} (\eqn{\delta+1}). The components of the Hessian
//' matrix (\eqn{-\mathbf{H}(\theta)}) are:
//'
//' \deqn{
//' -\frac{\partial^2 \ell}{\partial \gamma^2} = n[\psi'(\gamma) - \psi'(\gamma+\delta+1)]
//' }
//' \deqn{
//' -\frac{\partial^2 \ell}{\partial \gamma \partial \delta} = -n\psi'(\gamma+\delta+1)
//' }
//' \deqn{
//' -\frac{\partial^2 \ell}{\partial \delta^2} = n[\psi'(\delta+1) - \psi'(\gamma+\delta+1)]
//' }
//'
//' where \eqn{\psi'(\cdot)} is the trigamma function (\code{\link[base]{trigamma}}).
//' These formulas represent the second derivatives of \eqn{-\ell(\theta)},
//' consistent with minimizing the negative log-likelihood. They correspond to
//' the relevant 2x2 submatrix of the general GKw Hessian (\code{\link{hsgkw}})
//' evaluated at \eqn{\alpha=1, \beta=1, \lambda=1}. Note the parameterization
//' difference from the standard Beta distribution (\code{shape2 = delta + 1}).
//'
//' The returned matrix is symmetric.
//'
//' @references
//' Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). *Continuous Univariate
//' Distributions, Volume 2* (2nd ed.). Wiley.
//'
//' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized
//' distributions. *Journal of Statistical Computation and Simulation*,
//'
//' (Note: Specific Hessian formulas might be derived or sourced from additional references).
//'
//' @seealso
//' \code{\link{hsgkw}}, \code{\link{hsmc}} (related Hessians),
//' \code{\link{llbeta}} (negative log-likelihood function),
//' \code{grbeta} (gradient, if available),
//' \code{dbeta_}, \code{pbeta_}, \code{qbeta_}, \code{rbeta_},
//' \code{\link[stats]{optim}},
//' \code{\link[numDeriv]{hessian}} (for numerical Hessian comparison),
//' \code{\link[base]{trigamma}}.
//'
//' @examples
//' \donttest{
//' # Assuming existence of rbeta_, llbeta, grbeta, hsbeta functions
//'
//' # Generate sample data from a Beta(2, 4) distribution
//' # (gamma=2, delta=3 in this parameterization)
//' set.seed(123)
//' true_par_beta <- c(gamma = 2, delta = 3)
//' sample_data_beta <- rbeta_(100, gamma = true_par_beta[1], delta = true_par_beta[2])
//' hist(sample_data_beta, breaks = 20, main = "Beta(2, 4) Sample")
//'
//' # --- Find MLE estimates ---
//' start_par_beta <- c(1.5, 2.5)
//' mle_result_beta <- stats::optim(par = start_par_beta,
//'                                fn = llbeta,
//'                                gr = if (exists("grbeta")) grbeta else NULL,
//'                                method = "L-BFGS-B",
//'                                lower = c(1e-6, 1e-6), # Bounds: gamma>0, delta>=0
//'                                hessian = TRUE, # Ask optim for numerical Hessian
//'                                data = sample_data_beta)
//'
//' # --- Compare analytical Hessian to numerical Hessian ---
//' if (mle_result_beta$convergence == 0 &&
//'     requireNamespace("numDeriv", quietly = TRUE) &&
//'     exists("hsbeta")) {
//'
//'   mle_par_beta <- mle_result_beta$par
//'   cat("\nComparing Hessians for Beta at MLE estimates:\n")
//'
//'   # Numerical Hessian of llbeta
//'   num_hess_beta <- numDeriv::hessian(func = llbeta, x = mle_par_beta, data = sample_data_beta)
//'
//'   # Analytical Hessian from hsbeta
//'   ana_hess_beta <- hsbeta(par = mle_par_beta, data = sample_data_beta)
//'
//'   cat("Numerical Hessian (Beta):\n")
//'   print(round(num_hess_beta, 4))
//'   cat("Analytical Hessian (Beta):\n")
//'   print(round(ana_hess_beta, 4))
//'
//'   # Check differences
//'   cat("Max absolute difference between Beta Hessians:\n")
//'   print(max(abs(num_hess_beta - ana_hess_beta)))
//'
//'   # Optional: Use analytical Hessian for Standard Errors
//'   # tryCatch({
//'   #   cov_matrix_beta <- solve(ana_hess_beta) # ana_hess_beta is already Hessian of negative LL
//'   #   std_errors_beta <- sqrt(diag(cov_matrix_beta))
//'   #   cat("Std. Errors from Analytical Beta Hessian:\n")
//'   #   print(std_errors_beta)
//'   # }, error = function(e) {
//'   #   warning("Could not invert analytical Beta Hessian: ", e$message)
//'   # })
//'
//' } else {
//'   cat("\nSkipping Beta Hessian comparison.\n")
//'   cat("Requires convergence, 'numDeriv' package, and function 'hsbeta'.\n")
//' }
//'
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix hsbeta(const Rcpp::NumericVector& par,
                        const Rcpp::NumericVector& data) {
Rcpp::NumericMatrix hess(2, 2);
// Initialize with Inf
for (int i = 0; i < 2; i++) {
 for (int j = 0; j < 2; j++) {
   hess(i, j) = R_PosInf;
 }
}

if (par.size() < 2) {
 return hess;
}

double gamma = par[0];
double delta = par[1];

if (!check_beta_pars(gamma, delta)) {
 return hess;
}

arma::vec x = Rcpp::as<arma::vec>(data);
if (x.n_elem < 1) {
 return hess;
}

// domain check
if (arma::any(x <= 0.0) || arma::any(x >= 1.0)) {
 return hess;
}

int n = x.n_elem;

// Calculate trigamma terms with correct parameterization for GKw-style Beta
double trig_g = R::trigamma(gamma);
double trig_d = R::trigamma(delta + 1.0);  // Corrected: trigamma(δ+1)
double trig_gd = R::trigamma(gamma + delta + 1.0);  // Corrected: trigamma(γ+δ+1)

// Second derivatives for negative log-likelihood
hess(0, 0) = n * (trig_g - trig_gd);  // d²/dγ²
hess(1, 1) = n * (trig_d - trig_gd);  // d²/dδ²
hess(0, 1) = hess(1, 0) = -n * trig_gd;  // d²/dγdδ

return hess; // Already for negative log-likelihood
}










//' @title Generate Regularized Positive Definite Approximation of a Hessian
//' @description Creates a positive definite approximation of a Hessian matrix
//' using eigendecomposition and selective eigenvalue adjustment with enhanced
//' numerical stability.
//'
//' @param H Input Hessian matrix (arma::mat)
//' @param min_eigenval Minimum eigenvalue threshold (default: 1e-6)
//' @return Positive definite approximation of H
//' @noRd
arma::mat makePositiveDefinite(arma::mat H, double min_eigenval = 1e-6) {
// Special case for 1x1 matrices for efficiency
if (H.n_rows == 1 && H.n_cols == 1) {
 return arma::mat(1, 1, arma::fill::value(std::max(H(0,0), min_eigenval)));
}

// Verify matrix dimensions
if (H.n_rows != H.n_cols) {
 Rcpp::warning("Non-square matrix provided to makePositiveDefinite. Returning diagonal matrix.");
 arma::mat diag_matrix = arma::eye(H.n_rows, H.n_rows);
 diag_matrix *= min_eigenval;
 return diag_matrix;
}

// Ensure symmetry by averaging with transpose using a numerically stable approach
// double sym_tol = 1e-8;
double max_asym = 0.0;

// Check asymmetry level
for (unsigned int i = 0; i < H.n_rows; i++) {
 for (unsigned int j = i+1; j < H.n_cols; j++) {
   max_asym = std::max(max_asym, std::abs(H(i,j) - H(j,i)));
 }
}

// Warning if severe asymmetry is detected
if (max_asym > 1e-3) {
 Rcpp::warning("Significant asymmetry detected in Hessian matrix (max diff: %f)", max_asym);
}

// Force symmetry
H = 0.5 * (H + H.t());

// Attempt Cholesky decomposition first (faster for already positive definite matrices)
arma::mat R;
bool is_pd = arma::chol(R, H);

if (is_pd) {
 // Matrix is already positive definite, verify if eigenvalues meet minimum threshold
 arma::vec eigval;
 bool eig_success = arma::eig_sym(eigval, H);

 if (eig_success && arma::min(eigval) >= min_eigenval) {
   return H;  // Already meets all requirements
 }
}

// Perform eigendecomposition with enhanced numerical stability
arma::vec eigval;
arma::mat eigvec;
bool success = arma::eig_sym(eigval, eigvec, H);

if (!success) {
 // Enhanced fallback if eigendecomposition fails
 arma::mat H_diag = H;
 double max_diag = 0.0;

 // Find maximum diagonal element for scaling
 for (unsigned int i = 0; i < H.n_rows; i++) {
   max_diag = std::max(max_diag, std::abs(H(i,i)));
 }

 // Add scaled regularization
 double reg_val = std::max(min_eigenval, 1e-4 * max_diag);
 return H + reg_val * arma::eye(size(H));
}

// Find the smallest eigenvalue
double min_eig = arma::min(eigval);
double max_eig = arma::max(eigval);

// Check condition number and apply stronger regularization if needed
double condition_number = max_eig / std::max(std::abs(min_eig), 1e-12);
bool needs_strong_reg = (condition_number > 1e10) || (min_eig < -1e3 * min_eigenval);

// If extreme conditioning or large negative eigenvalues, use modified approach
if (needs_strong_reg) {
 // Apply progressive regularization for better numerical stability
 // double shift = std::max(min_eigenval, -min_eig + min_eigenval * 10.0);

 // Adaptive scaling based on eigenvalue magnitude
 double scale_factor = std::max(0.01, std::min(1.0, 1e6 / condition_number));

 for (unsigned int i = 0; i < eigval.n_elem; i++) {
   if (eigval(i) < min_eigenval) {
     // Progressive adjustment: stronger for more negative values
     double neg_factor = (eigval(i) < 0) ? std::abs(eigval(i)) / (std::abs(min_eig) + 1e-10) : 0.0;
     eigval(i) = min_eigenval * (1.0 + 10.0 * neg_factor * scale_factor);
   }
 }
} else {
 // Standard adjustment for mild cases
 for (unsigned int i = 0; i < eigval.n_elem; i++) {
   if (eigval(i) < min_eigenval) {
     eigval(i) = min_eigenval;
   }
 }
}

// Reconstruct matrix: H = Q * D * Q'
arma::mat D = arma::diagmat(eigval);
arma::mat H_pd = eigvec * D * eigvec.t();

// Ensure perfect symmetry (counteract any accumulating numerical errors)
H_pd = 0.5 * (H_pd + H_pd.t());

// Verify result is positive definite (in debug mode or with high tolerance)
#ifdef DEBUG
is_pd = arma::chol(R, H_pd);
if (!is_pd) {
 Rcpp::warning("Resulting matrix is still not positive definite");
 // Add last-resort regularization
 H_pd += 10.0 * min_eigenval * arma::eye(size(H_pd));
}
#endif

return H_pd;
}


//' @title Calculate Numerical Hessian with Advanced Adaptive Step Size
//' @description Computes a robust numerical approximation of the Hessian matrix
//' using adaptive finite difference methods with cross-validation.
//'
//' @param params Parameter vector
//' @param data Data vector
//' @param ll_func Log-likelihood function
//' @param gr_func Gradient function (used for validation)
//' @param min_step Minimum step size (default: 1e-8)
//' @param base_step Base step size (default: 1e-5)
//' @return Numerical approximation of the Hessian matrix
//' @noRd
arma::mat numHessianAdaptive(
 Rcpp::NumericVector params,
 Rcpp::NumericVector data,
 std::function<double(Rcpp::NumericVector, Rcpp::NumericVector)> ll_func,
 std::function<Rcpp::NumericVector(Rcpp::NumericVector, Rcpp::NumericVector)> gr_func,
 double min_step = 1e-8,
 double base_step = 1e-5
) {
int n = params.size();
arma::mat hessian(n, n, arma::fill::zeros);

// Evaluate function at center point with safeguards
double f0 = ll_func(params, data);
if (!R_finite(f0)) {
 Rcpp::warning("Function evaluation at center point is not finite");
 f0 = 0.0;  // Neutral value to allow continued computation
}

// Calculate gradient at current point for validation and method selection
Rcpp::NumericVector g0;
bool valid_gradient = true;

try {
 g0 = gr_func(params, data);

 // Check if gradient values are finite
 for (int i = 0; i < n; i++) {
   if (!R_finite(g0[i])) {
     valid_gradient = false;
     break;
   }
 }
} catch (...) {
 valid_gradient = false;
}

if (!valid_gradient) {
 Rcpp::warning("Analytic gradient invalid or unavailable, using fully numerical approach");
 // Create placeholder gradient for the algorithm to continue
 g0 = Rcpp::NumericVector(n, 0.0);
}

// Compute scale factors for parameters to improve numerical behavior
std::vector<double> param_scales(n);
double param_norm = 0.0;
for (int i = 0; i < n; i++) {
 param_scales[i] = std::max(1.0, std::abs(params[i]));
 param_norm += std::pow(params[i] / param_scales[i], 2);
}
param_norm = std::sqrt(param_norm);

// Compute adaptive step sizes based on parameter magnitudes with enhanced heuristics
std::vector<double> step_sizes(n);
for (int i = 0; i < n; i++) {
 // Base step size proportional to parameter magnitude but bounded
 step_sizes[i] = std::max(min_step, std::min(base_step, base_step * std::abs(params[i])));

 // Ensure step isn't too large relative to parameter value
 if (params[i] != 0 && step_sizes[i] > 0.01 * std::abs(params[i])) {
   step_sizes[i] = 0.01 * std::abs(params[i]);
 }

 // Ensure step is well above floating point precision for the parameter magnitude
 double eps_precision = std::abs(params[i]) * std::numeric_limits<double>::epsilon() * 100.0;
 step_sizes[i] = std::max(step_sizes[i], eps_precision);

 // Final safety check
 step_sizes[i] = std::max(min_step, step_sizes[i]);
}

// Cache for function evaluations to avoid redundant computations
std::map<std::vector<int>, double> evaluation_cache;

// Helper to retrieve or compute function value with caching
auto cached_eval = [&](const Rcpp::NumericVector& p) -> double {
 // Create key for cache
 std::vector<int> key;
 for (int i = 0; i < n; i++) {
   // Hash based on offset from original parameters
   // Scale to avoid floating point comparison issues
   key.push_back(static_cast<int>((p[i] - params[i]) * 1e10));
 }

 auto it = evaluation_cache.find(key);
 if (it != evaluation_cache.end()) {
   return it->second;  // Return cached value
 }

 // Compute and cache
 double value = ll_func(p, data);

 // Sanitize result - replace non-finite values with a penalty value
 if (!R_finite(value)) {
   double penalty = f0 + 1e6 * param_norm;  // Large penalty proportional to parameter size
   value = penalty;
 }

 evaluation_cache[key] = value;
 return value;
};

// Compute diagonal elements using central difference
for (int i = 0; i < n; i++) {
 double eps = step_sizes[i];
 double best_estimate = 0.0;
 double lowest_error = R_PosInf;
 double actual_eps_used = eps;

 // Declare params_p and params_m in the outer scope so they're available for Richardson
 Rcpp::NumericVector params_p;
 Rcpp::NumericVector params_m;
 double f_p, f_m;

 // Try multiple step sizes for better accuracy
 for (int step_iter = 0; step_iter < 3; step_iter++) {
   // Adjust step size for current iteration
   double current_eps = eps;
   if (step_iter == 1) current_eps *= 0.1;
   if (step_iter == 2) current_eps *= 10.0;

   params_p = Rcpp::clone(params);
   params_m = Rcpp::clone(params);

   params_p[i] += current_eps;
   params_m[i] -= current_eps;

   f_p = cached_eval(params_p);
   f_m = cached_eval(params_m);

   // Standard central difference formula for second derivative
   double h2 = current_eps * current_eps;
   double estimate = (f_p - 2.0*f0 + f_m) / h2;

   // Validate with gradient if available
   if (valid_gradient) {
     double grad_i = g0[i];
     double diff_approx = (f_p - f_m) / (2.0 * current_eps);

     // Estimate error by comparing with analytical gradient
     double error = std::abs(grad_i - diff_approx);
     double relative_error = error / (std::abs(grad_i) + std::abs(diff_approx) + 1e-10);

     // Keep the estimate with lowest error
     if (relative_error < lowest_error) {
       lowest_error = relative_error;
       best_estimate = estimate;
       actual_eps_used = current_eps;

       // Also update the points for Richardson when we store the best estimate
       // This ensures params_p and params_m correspond to the epsilon that will be used
       if (step_iter != 1) { // If not the middle step size, recalculate
         params_p = Rcpp::clone(params);
         params_m = Rcpp::clone(params);
         params_p[i] += actual_eps_used;
         params_m[i] -= actual_eps_used;
         f_p = cached_eval(params_p);
         f_m = cached_eval(params_m);
       }
     }
   } else {
     // Without valid gradient, use the most refined step size
     if (step_iter == 1) {  // Middle iteration has smallest step size
       best_estimate = estimate;
       actual_eps_used = current_eps;
     }
   }
 }

 // Use Richardson extrapolation for higher accuracy if appropriate
 if (valid_gradient && lowest_error < 0.05) {
   // Additional points for Richardson extrapolation
   Rcpp::NumericVector params_p2 = Rcpp::clone(params);
   Rcpp::NumericVector params_m2 = Rcpp::clone(params);

   params_p2[i] += 2.0 * actual_eps_used;
   params_m2[i] -= 2.0 * actual_eps_used;

   double f_p2 = cached_eval(params_p2);
   double f_m2 = cached_eval(params_m2);

   // Standard formula
   double h2 = actual_eps_used * actual_eps_used;
   double standard = (f_p - 2.0*f0 + f_m) / h2;

   // Wider formula
   double h2_wide = 4.0 * h2;  // (2h)²
   double wider = (f_p2 - 2.0*f0 + f_m2) / h2_wide;

   // Richardson extrapolation: eliminate leading error term
   best_estimate = (4.0/3.0) * standard - (1.0/3.0) * wider;
 }

 // Sanity check and fallback for the diagonal elements
 if (!R_finite(best_estimate) || std::abs(best_estimate) > 1e6 / param_scales[i]) {
   // Use a regularized estimate based on gradient if available
   if (valid_gradient) {
     Rcpp::NumericVector params_p = Rcpp::clone(params);
     params_p[i] += actual_eps_used;

     Rcpp::NumericVector grad_p = gr_func(params_p, data);

     // Compute directional derivative differences
     double dir_diff = 0.0;
     bool valid_dir_diff = true;

     for (int k = 0; k < n; k++) {
       if (!R_finite(grad_p[k])) {
         valid_dir_diff = false;
         break;
       }
       dir_diff += (grad_p[k] - g0[k]) / actual_eps_used;
     }

     if (valid_dir_diff) {
       best_estimate = dir_diff;
     } else {
       // Last resort: use a small positive value
       best_estimate = 1.0 / param_scales[i];
     }
   } else {
     // Without valid gradient, use a small positive value
     best_estimate = 1.0 / param_scales[i];
   }
 }

 // Assign to diagonal
 hessian(i, i) = best_estimate;
}

// Compute off-diagonal elements using mixed partial derivative formula
for (int i = 0; i < n; i++) {
 for (int j = 0; j < i; j++) {
   double eps_i = step_sizes[i];
   double eps_j = step_sizes[j];

   // Adjust step sizes if parameters have very different magnitudes
   double mag_ratio = param_scales[i] / param_scales[j];
   if (mag_ratio > 100.0 || mag_ratio < 0.01) {
     double geom_mean = std::sqrt(eps_i * eps_j);
     eps_i = std::min(eps_i, geom_mean);
     eps_j = std::min(eps_j, geom_mean);
   }

   Rcpp::NumericVector params_pp = Rcpp::clone(params);
   Rcpp::NumericVector params_pm = Rcpp::clone(params);
   Rcpp::NumericVector params_mp = Rcpp::clone(params);
   Rcpp::NumericVector params_mm = Rcpp::clone(params);

   params_pp[i] += eps_i; params_pp[j] += eps_j;
   params_pm[i] += eps_i; params_pm[j] -= eps_j;
   params_mp[i] -= eps_i; params_mp[j] += eps_j;
   params_mm[i] -= eps_i; params_mm[j] -= eps_j;

   double f_pp = cached_eval(params_pp);
   double f_pm = cached_eval(params_pm);
   double f_mp = cached_eval(params_mp);
   double f_mm = cached_eval(params_mm);

   // Mixed partial formula
   double mixed_estimate = (f_pp - f_pm - f_mp + f_mm) / (4.0 * eps_i * eps_j);

   // Alternative calculation using gradient if available
   bool use_alternative = false;
   double alt_estimate = 0.0;

   if (valid_gradient) {
     try {
       Rcpp::NumericVector grad_p = gr_func(params_pp, data);
       Rcpp::NumericVector grad_m = gr_func(params_mm, data);

       bool valid_grads = true;
       for (int k = 0; k < n; k++) {
         if (!R_finite(grad_p[k]) || !R_finite(grad_m[k])) {
           valid_grads = false;
           break;
         }
       }

       if (valid_grads) {
         // Alternative estimate using directional derivatives
         double dir_diff_i = (grad_p[i] - grad_m[i]) / (2.0 * eps_i);
         double dir_diff_j = (grad_p[j] - grad_m[j]) / (2.0 * eps_j);

         alt_estimate = 0.5 * (dir_diff_i + dir_diff_j);
         use_alternative = true;
       }
     } catch (...) {
       // Failed to compute alternative
     }
   }

   // Choose estimate with fallback strategy
   double final_estimate = mixed_estimate;

   if (!R_finite(mixed_estimate) || std::abs(mixed_estimate) > 1e6 / (param_scales[i] * param_scales[j])) {
     if (use_alternative && R_finite(alt_estimate)) {
       final_estimate = alt_estimate;
     } else {
       // As last resort, use geometric mean of diagonal elements with reduced magnitude
       final_estimate = std::sqrt(std::abs(hessian(i,i) * hessian(j,j))) * 0.1;
       // Preserve sign from original estimate if valid
       if (R_finite(mixed_estimate) && mixed_estimate != 0.0) {
         final_estimate *= std::copysign(1.0, mixed_estimate);
       }
     }
   }

   hessian(i, j) = final_estimate;
   hessian(j, i) = final_estimate; // Ensure symmetry
 }
}

// Final validation and corrections

// Check for NaN or Inf values
bool has_invalid = false;
for (int i = 0; i < n && !has_invalid; i++) {
 for (int j = 0; j < n; j++) {
   if (!R_finite(hessian(i, j))) {
     has_invalid = true;
     break;
   }
 }
}

// Fix any remaining invalid values
if (has_invalid) {
 Rcpp::warning("Numerical Hessian contains invalid values; applying regularization");

 // Use diagonal dominance to ensure positive definiteness
 for (int i = 0; i < n; i++) {
   if (!R_finite(hessian(i, i)) || hessian(i, i) <= 0) {
     hessian(i, i) = 1.0 / param_scales[i]; // Fallback positive value
   }

   double row_sum = 0.0;
   for (int j = 0; j < n; j++) {
     if (i != j) {
       if (!R_finite(hessian(i, j))) {
         hessian(i, j) = 0.0; // Zero out invalid off-diagonals
         hessian(j, i) = 0.0; // Maintain symmetry
       }
       row_sum += std::abs(hessian(i, j));
     }
   }

   // Ensure diagonal dominance for numerical stability
   if (std::abs(hessian(i, i)) <= row_sum) {
     hessian(i, i) = row_sum + 1e-3 / param_scales[i];
   }
 }
}

// Return the final, robust Hessian approximation
return hessian;
}


//' @title Calculate Adaptive Parameter Scaling Factors
//' @description Computes intelligent scaling factors for parameters to improve
//' numerical stability in optimization, with automatic detection of parameter
//' patterns and adaptive thresholds.
//'
//' @param params Parameter vector
//' @return Vector of scaling factors
//' @noRd
Rcpp::NumericVector computeScalingFactors(Rcpp::NumericVector params) {
int n = params.size();
Rcpp::NumericVector scaling_factors(n, 1.0);

// Handle empty parameter vector
if (n == 0) return scaling_factors;

// Calculate parameter statistics for adaptive scaling
double param_max = 0.0;
double param_min = R_PosInf;
double geom_mean = 0.0;
int non_zero_count = 0;

for (int i = 0; i < n; i++) {
 double abs_param = std::abs(params[i]);

 // Skip zeros for statistics
 if (abs_param > 0) {
   param_max = std::max(param_max, abs_param);
   param_min = std::min(param_min, abs_param);
   geom_mean += std::log(abs_param);
   non_zero_count++;
 }
}

// Set adaptive thresholds
double upper_threshold = 100.0;  // Default upper threshold
double lower_threshold = 0.01;   // Default lower threshold

// Use adaptive thresholds if we have enough non-zero parameters
if (non_zero_count > 1) {
 geom_mean = std::exp(geom_mean / non_zero_count);

 // Calculate magnitude range
 double magnitude_range = param_max / std::max(param_min, std::numeric_limits<double>::min());

 // If parameters span many orders of magnitude, use more aggressive scaling
 if (magnitude_range > 1e6) {
   // For extreme ranges, use statistics-based thresholds
   upper_threshold = std::min(100.0, std::max(10.0, 10.0 * geom_mean));
   lower_threshold = std::max(0.01, std::min(0.1, 0.1 * geom_mean));
 }
 else if (magnitude_range > 1e4) {
   // For wide ranges, adjust thresholds based on geometric mean
   upper_threshold = std::min(100.0, std::max(10.0, 5.0 * geom_mean));
   lower_threshold = std::max(0.01, std::min(0.1, 0.2 * geom_mean));
 }
}

// Apply scaling factors with adaptive thresholds
for (int i = 0; i < n; i++) {
 double abs_param = std::abs(params[i]);

 // Apply scaling for very large parameters
 if (abs_param > upper_threshold) {
   // Nonlinear scaling for extreme values to prevent excessive scaling
   if (abs_param > 1e4 * upper_threshold) {
     // Use square root scaling for extremely large values
     scaling_factors[i] = upper_threshold / std::sqrt(abs_param * upper_threshold);
   } else {
     // Standard scaling
     scaling_factors[i] = upper_threshold / abs_param;
   }
 }
 // Apply scaling for very small non-zero parameters
 else if (abs_param < lower_threshold && abs_param > 0) {
   // Nonlinear scaling for extreme values to prevent excessive scaling
   if (abs_param < lower_threshold * 1e-4) {
     // Use square root scaling for extremely small values
     scaling_factors[i] = std::sqrt(lower_threshold / abs_param);
   } else {
     // Standard scaling
     scaling_factors[i] = lower_threshold / abs_param;
   }
 }

 // Handle zero parameters specially - leave scaling at 1.0
 // as multiplying by any factor won't change zero
}

// Detect if parameters have extreme differences that might cause issues
if (n > 1) {
 double max_scaling = 1.0;
 double min_scaling = 1.0;

 for (int i = 0; i < n; i++) {
   if (scaling_factors[i] != 1.0) {
     max_scaling = std::max(max_scaling, scaling_factors[i]);
     min_scaling = std::min(min_scaling, scaling_factors[i]);
   }
 }

 // If scaling factors themselves vary too much, moderate them
 if (max_scaling / min_scaling > 1e6) {
   for (int i = 0; i < n; i++) {
     if (scaling_factors[i] > 1.0) {
       // Reduce very large upscaling factors using a logarithmic damping
       scaling_factors[i] = std::pow(scaling_factors[i], 0.75);
     }
     else if (scaling_factors[i] < 1.0 && scaling_factors[i] > 0) {
       // Reduce very small downscaling factors using a logarithmic damping
       scaling_factors[i] = std::pow(scaling_factors[i], 0.75);
     }
   }
 }
}

return scaling_factors;
}



//' @title Enhanced Line Search with Wolfe Conditions
//' @description Performs a robust line search that satisfies Wolfe conditions to
//' ensure sufficient decrease and curvature conditions, with improved numerical
//' stability and adaptive bracketing strategies.
//'
//' @param params Current parameter vector
//' @param search_dir Search direction
//' @param data Data vector
//' @param ll_func Log-likelihood function
//' @param gr_func Gradient function
//' @param alpha_init Initial step size (default: 1.0)
//' @param c1 Parameter for sufficient decrease condition (default: 1e-4)
//' @param c2 Parameter for curvature condition (default: 0.9)
//' @param max_iter Maximum line search iterations (default: 20)
//' @return Step size satisfying Wolfe conditions
//' @noRd
double lineSearchWolfe(
 Rcpp::NumericVector params,
 Rcpp::NumericVector search_dir,
 Rcpp::NumericVector data,
 std::function<double(Rcpp::NumericVector, Rcpp::NumericVector)> ll_func,
 std::function<Rcpp::NumericVector(Rcpp::NumericVector, Rcpp::NumericVector)> gr_func,
 double alpha_init = 1.0,
 double c1 = 1e-4,
 double c2 = 0.9,
 int max_iter = 20
) {
const int n_params = params.size();

// Safety bounds for step size
const double alpha_min = 1e-10;
const double alpha_max = 1e3;

// Safeguard alpha_init to be reasonable
alpha_init = std::min(std::max(alpha_init, alpha_min), alpha_max);

// Evaluate function and gradient at starting point with error handling
double f0;
Rcpp::NumericVector g0;

try {
 f0 = ll_func(params, data);
 if (!R_finite(f0)) {
   Rcpp::warning("Initial function value is not finite");
   return alpha_min;
 }

 g0 = gr_func(params, data);
 for (int i = 0; i < n_params; i++) {
   if (!R_finite(g0[i])) {
     Rcpp::warning("Initial gradient contains non-finite values");
     return alpha_min;
   }
 }
} catch (...) {
 Rcpp::warning("Exception during initial function/gradient evaluation");
 return alpha_min;
}

// Calculate directional derivative and search direction norm
double dir_deriv = 0.0;
double search_dir_norm = 0.0;

for (int i = 0; i < n_params; i++) {
 dir_deriv += g0[i] * search_dir[i];
 search_dir_norm += search_dir[i] * search_dir[i];
}
search_dir_norm = std::sqrt(search_dir_norm);

// Adjust c1 and c2 based on problem characteristics (if needed)
if (std::abs(dir_deriv) / search_dir_norm < 1e-4) {
 // For very flat regions, relax the conditions slightly
 c1 = std::min(c1, 1e-6);
 c2 = std::max(c2, 0.95);
}

// Ensure search direction is a descent direction
if (dir_deriv >= 0) {
 // Not a descent direction, return small step
 return alpha_min;
}

// Scale initial step size based on search direction norm for better initial guess
if (search_dir_norm > 100.0) {
 alpha_init = std::min(alpha_init, 10.0/search_dir_norm);
} else if (search_dir_norm < 0.01) {
 alpha_init = std::min(alpha_max, std::max(alpha_init, 0.1/search_dir_norm));
}

// Initialize the bracketing phase
double alpha = alpha_init;
double alpha_prev = 0.0;
double f_prev = f0;

// Cache to avoid redundant function evaluations
struct CacheEntry {
 double alpha;
 double f_val;
 Rcpp::NumericVector gradient;
 bool has_gradient;

 CacheEntry(double a, double f, Rcpp::NumericVector g, bool has_g) :
   alpha(a), f_val(f), gradient(g), has_gradient(has_g) {}
};

std::vector<CacheEntry> evaluations;
evaluations.reserve(max_iter + 1);

// Helper function to compute new parameters
auto compute_new_params = [&](double step_size) -> Rcpp::NumericVector {
 Rcpp::NumericVector new_params = Rcpp::clone(params);
 for (int i = 0; i < n_params; i++) {
   new_params[i] += step_size * search_dir[i];
 }
 return new_params;
};

// Helper to find cached evaluation
auto find_cached = [&](double a) -> std::vector<CacheEntry>::iterator {
 const double cache_tol = 1e-14;
 for (auto it = evaluations.begin(); it != evaluations.end(); ++it) {
   if (std::abs(it->alpha - a) < cache_tol) {
     return it;
   }
 }
 return evaluations.end();
};

// Helper to evaluate function and optionally gradient at a point
auto evaluate = [&](double a, bool need_gradient) -> CacheEntry {
 // Check cache first
 auto cached = find_cached(a);
 if (cached != evaluations.end()) {
   if (need_gradient && !cached->has_gradient) {
     // Need to compute gradient for cached point
     try {
       Rcpp::NumericVector new_params = compute_new_params(a);
       cached->gradient = gr_func(new_params, data);
       cached->has_gradient = true;
     } catch (...) {
       // Keep existing entry if gradient fails
     }
   }
   return *cached;
 }

 // New evaluation needed
 Rcpp::NumericVector new_params = compute_new_params(a);
 double f_val;
 Rcpp::NumericVector g_val;
 bool has_g = false;

 try {
   f_val = ll_func(new_params, data);
   if (!R_finite(f_val)) {
     f_val = R_PosInf; // Mark as invalid
   }

   if (need_gradient) {
     g_val = gr_func(new_params, data);
     has_g = true;

     // Check gradient validity
     for (int i = 0; i < n_params; i++) {
       if (!R_finite(g_val[i])) {
         has_g = false;
         break;
       }
     }
   }
 } catch (...) {
   f_val = R_PosInf;
   has_g = false;
 }

 CacheEntry entry(a, f_val, g_val, has_g);
 evaluations.push_back(entry);
 return entry;
};

// Line search iterations
for (int iter = 0; iter < max_iter; iter++) {
 // Evaluate function at new point
 CacheEntry eval = evaluate(alpha, false);
 double f_new = eval.f_val;

 // Check for invalid function values
 if (!R_finite(f_new) || f_new == R_PosInf) {
   // Rapidly reduce step size for invalid regions
   alpha = 0.1 * alpha;
   if (alpha < alpha_min) {
     return alpha_min;
   }
   continue;
 }

 // Check Armijo condition (sufficient decrease)
 bool armijo_condition = f_new <= f0 + c1 * alpha * dir_deriv;

 if (!armijo_condition) {
   // Backtracking with safeguarded polynomial interpolation
   if (iter == 0) {
     // First iteration, use safeguarded quadratic interpolation
     double denominator = 2.0 * (f_new - f0 - dir_deriv * alpha);

     if (std::abs(denominator) < 1e-10) {
       // Avoid division by very small number
       alpha = 0.5 * alpha;
     } else {
       // Quadratic interpolation
       double alpha_quad = -dir_deriv * alpha * alpha / denominator;

       // Safeguard the interpolated value
       if (alpha_quad < 0.1 * alpha || alpha_quad > 0.9 * alpha || !R_finite(alpha_quad)) {
         alpha = 0.5 * alpha;  // Fallback
       } else {
         alpha = alpha_quad;
       }
     }
   } else {
     // Subsequent iterations, use safeguarded cubic interpolation
     double d1 = dir_deriv + (f_new - f0) / alpha;
     double d2 = (f_prev - f0) / alpha_prev + dir_deriv;

     // Check for nearly identical slopes
     if (std::abs(d1 - d2) < 1e-10 || std::abs(alpha - alpha_prev) < 1e-10) {
       alpha = 0.5 * alpha;  // Simple backtracking
     } else {
       double a = (d1 - d2) / (alpha - alpha_prev);
       double b = d1 - a * alpha;

       // Compute minimum of cubic polynomial with safeguards
       double alpha_cubic;

       if (std::abs(a) < 1e-10) {
         // Nearly quadratic case
         if (std::abs(b) < 1e-10) {
           alpha_cubic = 0.5 * alpha;  // Fallback
         } else {
           alpha_cubic = -dir_deriv / (2.0 * b);
         }
       } else {
         double disc = b * b - 3.0 * a * dir_deriv;

         if (disc < 0 || !R_finite(disc)) {
           // Discriminant invalid, use simple backtracking
           alpha_cubic = 0.5 * alpha;
         } else {
           alpha_cubic = (-b + std::sqrt(disc)) / (3.0 * a);

           // Check if we got the wrong root
           if (alpha_cubic <= 0 || alpha_cubic > alpha) {
             alpha_cubic = (-b - std::sqrt(disc)) / (3.0 * a);
           }
         }
       }

       // Safeguard the interpolated value
       if (alpha_cubic <= 0.1 * alpha || alpha_cubic >= 0.9 * alpha || !R_finite(alpha_cubic)) {
         alpha = 0.5 * alpha;  // Fallback
       } else {
         alpha = alpha_cubic;
       }
     }
   }

   // Additional safeguards for new alpha
   if (alpha < alpha_min) {
     return alpha_min;  // Step too small, terminate
   }

   // Safety check to avoid too small steps
   if (alpha < 0.1 * alpha_prev) {
     alpha = 0.1 * alpha_prev;
   }

   // Save current values for next iteration
   alpha_prev = alpha;
   f_prev = f_new;
   continue;
 }

 // Armijo condition is satisfied, now check curvature condition
 CacheEntry eval_with_grad = evaluate(alpha, true);

 // Check if we could compute a valid gradient
 if (!eval_with_grad.has_gradient) {
   // No valid gradient, accept the point anyway if it decreased the function
   if (f_new < f0) {
     return alpha;
   } else {
     // Try smaller step
     alpha = 0.5 * alpha;
     continue;
   }
 }

 // Compute new directional derivative
 double dir_deriv_new = 0.0;
 for (int i = 0; i < n_params; i++) {
   dir_deriv_new += eval_with_grad.gradient[i] * search_dir[i];
 }

 // Check Wolfe curvature condition
 bool wolfe_condition = std::abs(dir_deriv_new) <= c2 * std::abs(dir_deriv);

 if (wolfe_condition) {
   return alpha;  // Both conditions satisfied
 }

 // Curvature condition not satisfied, adjust interval
 if (dir_deriv_new * dir_deriv > 0) {
   // Derivative not changing sign, increase alpha
   alpha_prev = alpha;

   // More intelligent expansion strategy
   if (f_new < f_prev) {
     // Function still decreasing, be more aggressive
     alpha = std::min(alpha_max, 2.0 * alpha);
   } else {
     // Function increasing, be more conservative
     alpha = std::min(alpha_max, 1.5 * alpha);
   }

   f_prev = f_new;
 } else {
   // Derivative changed sign, we bracketed a minimum
   // Use bisection with more weight to the current point
   double alpha_temp = alpha;
   double weight = 0.6;  // Bias toward current point if it's better

   if (f_new < f_prev) {
     weight = 0.7;  // Even more bias if current point is much better
   }

   alpha = (1.0 - weight) * alpha_prev + weight * alpha;
   alpha_prev = alpha_temp;
   f_prev = f_new;
 }

 // Check for convergence based on step size
 if (std::abs(alpha - alpha_prev) < alpha_min) {
   return alpha;  // Steps are becoming too small
 }
}

// Check final point quality after max iterations
CacheEntry last_eval = evaluate(alpha, false);

// If we didn't satisfy Wolfe conditions but found a good improvement, accept it
if (last_eval.f_val < f0) {
 return alpha;
}

// Otherwise, find best alpha from our evaluations
double best_alpha = alpha_min;
double best_value = f0;

for (const auto& eval : evaluations) {
 if (R_finite(eval.f_val) && eval.f_val < best_value) {
   best_value = eval.f_val;
   best_alpha = eval.alpha;
 }
}

return best_alpha;
}



//' @title Force Matrix Symmetry
//' @description Ensures perfect matrix symmetry for numerical stability
//' @param M Input matrix to symmetrize
//' @return Symmetrized matrix
//' @noRd
inline arma::mat forceSymmetry(const arma::mat& M) {
return 0.5 * (M + M.t());
}

//' @title Enhanced Trust Region Update Using Levenberg-Marquardt
//' @description Performs robust parameter updates using an advanced trust region method
//' with Levenberg-Marquardt implementation, incorporating numerical safeguards and
//' adaptive strategies for improved convergence.
//'
//' @param params Current parameter vector
//' @param data Data vector
//' @param ll_func Log-likelihood function
//' @param gr_func Gradient function
//' @param hs_func Hessian function
//' @param trust_radius Current trust region radius (default: 1.0)
//' @param eta Acceptance threshold for step ratio (default: 0.1)
//' @param enforce_bounds Whether to enforce parameter bounds (default: true)
//' @param min_param_val Minimum parameter value (default: 1e-5)
//' @param max_param_val Maximum parameter value (default: 1e5)
//' @return List containing new parameters and trust region information
//' @noRd
Rcpp::List trustRegionUpdate(
 Rcpp::NumericVector params,
 Rcpp::NumericVector data,
 std::function<double(Rcpp::NumericVector, Rcpp::NumericVector)> ll_func,
 std::function<Rcpp::NumericVector(Rcpp::NumericVector, Rcpp::NumericVector)> gr_func,
 std::function<Rcpp::NumericMatrix(Rcpp::NumericVector, Rcpp::NumericVector)> hs_func,
 double trust_radius = 1.0,
 double eta = 0.1,
 bool enforce_bounds = true,
 double min_param_val = 1e-5,
 double max_param_val = 1e5
) {
int n = params.size();

// Ensure trust radius is within reasonable bounds
// This prevents numerical issues with extremely small or large step sizes
trust_radius = std::max(1e-8, std::min(trust_radius, 100.0));

// Create result container with default values for error cases
// This ensures we always return a valid result even if computation fails
Rcpp::List result;
result["new_params"] = Rcpp::clone(params);  // Default: no change
result["accept"] = false;
result["trust_radius"] = trust_radius * 0.5;  // Default: reduce trust region
result["actual_reduction"] = 0.0;
result["ratio"] = 0.0;

// Evaluate function and gradient at current point
double f0;
Rcpp::NumericVector g0;
// bool valid_evaluation = true;

try {
 f0 = ll_func(params, data);
 g0 = gr_func(params, data);

 // Check for non-finite values
 if (!R_finite(f0)) {
   Rcpp::warning("Non-finite function value at current point");
   return result;
 }

 for (int i = 0; i < n; i++) {
   if (!R_finite(g0[i])) {
     Rcpp::warning("Non-finite gradient value(s) at current point");
     return result;
   }
 }
} catch (...) {
 Rcpp::warning("Exception during function/gradient evaluation");
 return result;
}

// Convert gradient to Armadillo vector
arma::vec g_arma(n);
double grad_norm = 0.0;

for (int i = 0; i < n; i++) {
 g_arma(i) = g0[i];
 grad_norm += g0[i] * g0[i];
}
grad_norm = std::sqrt(grad_norm);

// Skip update if gradient is effectively zero (we're at a stationary point)
if (grad_norm < 1e-10) {
 result["accept"] = true;  // Accept current point (it's already optimal)
 return result;
}

// Compute parameter scale factors for better conditioning
// This helps handle parameters with very different magnitudes
arma::vec param_scales(n);
for (int i = 0; i < n; i++) {
 param_scales(i) = std::max(1.0, std::abs(params[i]));
}

// Retrieve and validate Hessian
Rcpp::NumericMatrix rcpp_hessian;
arma::mat H(n, n);
bool use_hessian = true;

try {
 rcpp_hessian = hs_func(params, data);

 // Check for non-finite values
 for (int i = 0; i < n; i++) {
   for (int j = 0; j < n; j++) {
     if (!R_finite(rcpp_hessian(i, j))) {
       use_hessian = false;
       break;
     }
   }
   if (!use_hessian) break;
 }

 if (use_hessian) {
   H = Rcpp::as<arma::mat>(rcpp_hessian);

   // Check for unreasonable values in Hessian
   double max_abs_hess = 0.0;
   for (int i = 0; i < n; i++) {
     for (int j = 0; j < n; j++) {
       max_abs_hess = std::max(max_abs_hess, std::abs(H(i, j)));
     }
   }

   // Reject extreme values which may cause numerical instability
   if (max_abs_hess > 1e7) {
     use_hessian = false;
   }
 }
} catch (...) {
 use_hessian = false;
}

// Fallback if Hessian is invalid or computation failed
if (!use_hessian) {
 // Use scaled identity matrix if Hessian is invalid
 H = arma::diagmat(arma::vec(n, arma::fill::ones));

 // Scale diagonal elements by gradient magnitudes for better conditioning
 for (int i = 0; i < n; i++) {
   double scale = std::max(1.0, std::sqrt(std::abs(g_arma(i))));
   H(i, i) = scale;
 }
}

// Ensure H is symmetric before making it positive definite
H = forceSymmetry(H);

// Ensure H is positive definite with scaled regularization
try {
 H = makePositiveDefinite(H, 1e-5);
} catch (...) {
 // If regularization fails, use a robust diagonal approximation
 for (int i = 0; i < n; i++) {
   for (int j = 0; j < n; j++) {
     if (i != j) H(i, j) = 0.0;
     else H(i, i) = std::max(1.0, std::abs(H(i, i)));
   }
 }
}

// Precondition the system for better numerical stability
// This helps solve the linear system more accurately
arma::vec scaled_g = g_arma;
arma::mat scaled_H = H;

for (int i = 0; i < n; i++) {
 double scale = param_scales(i);
 scaled_g(i) /= scale;

 for (int j = 0; j < n; j++) {
   scaled_H(i, j) = H(i, j) * scale / param_scales(j);
 }
}

// Ensure scaled_H is perfectly symmetric for eigendecomposition
scaled_H = forceSymmetry(scaled_H);

// Solve the trust region subproblem using Levenberg-Marquardt
arma::vec scaled_p;
double lambda = 0.0;
bool solve_success = false;

// Initial lambda based on Hessian eigenvalues and gradient norm
arma::vec eigval;
bool eig_success = arma::eig_sym(eigval, scaled_H);

if (eig_success) {
 double min_eig = arma::min(eigval);
 double max_eig = arma::max(eigval);

 // Set initial lambda based on eigenvalue range and gradient
 if (min_eig <= 1e-5) {
   // Matrix is near-singular or indefinite
   lambda = std::max(1e-5, -min_eig + 1e-4);
 } else {
   // Use smaller regularization if matrix is already positive definite
   lambda = std::min(min_eig, 1e-3 * max_eig);
 }

 // Adjust lambda based on gradient norm
 lambda = std::max(lambda, 1e-6 * grad_norm);
} else {
 // Fallback if eigenvalue computation fails
 lambda = 0.1 * grad_norm;
}

// Advanced binary search strategy with adaptive bounds
double lambda_low = 0.0;
double lambda_high = 1e8;
double target_step_ratio = 0.8; // Target ratio of step norm to trust radius

// Maximum number of binary search iterations
const int max_search_iter = 30;

// Store best solution found so far
arma::vec best_p;
double best_lambda = -1.0;
double best_ratio = 0.0; // Ratio of step norm to trust radius

for (int iter = 0; iter < max_search_iter; iter++) {
 // Solve the linear system with current lambda
 try {
   solve_success = arma::solve(scaled_p, scaled_H + lambda * arma::eye<arma::mat>(n, n), -scaled_g);
 } catch (...) {
   solve_success = false;
 }

 // Check if solve was successful and step is valid
 if (solve_success) {
   double step_norm = arma::norm(scaled_p);
   double step_ratio = step_norm / trust_radius;

   // Keep track of best solution so far
   if (best_lambda < 0 || std::abs(step_ratio - target_step_ratio) < std::abs(best_ratio - target_step_ratio)) {
     best_p = scaled_p;
     best_lambda = lambda;
     best_ratio = step_ratio;
   }

   // If we're close enough to the target ratio, stop early
   if (std::abs(step_ratio - target_step_ratio) < 0.05 || std::abs(lambda_high - lambda_low) < 1e-6 * (1.0 + lambda)) {
     break;
   }

   // Binary search update
   if (step_norm > trust_radius) {
     // Step too large, increase lambda
     lambda_low = lambda;

     // Adaptive update based on how far we are from target
     double factor = step_ratio / target_step_ratio;
     lambda = std::min(lambda_high, std::max(lambda * factor, lambda + (lambda_high - lambda_low) * 0.25));
   } else {
     // Step within trust region, decrease lambda if it's too small
     lambda_high = lambda;

     // Adaptive update based on how far we are from target
     if (step_ratio < 0.1 * target_step_ratio) {
       // Step much too small, be more aggressive
       lambda = std::max(lambda_low, lambda * 0.1);
     } else {
       // Step somewhat small, make moderate adjustment
       double factor = step_ratio / target_step_ratio;
       lambda = std::max(lambda_low, lambda * factor);
     }
   }
 } else {
   // Solver failed, increase lambda substantially
   lambda_low = lambda;
   lambda = std::min(lambda_high, lambda * 10.0);

   // If lambda is getting very large, adjust the upper bound
   if (lambda > 1e6) lambda_high = std::min(lambda_high, lambda * 2.0);
 }

 // Handle convergence edge cases
 if (lambda_high < 1e-10) {
   lambda_high = 1e-2;
   lambda = 1e-3;
 }

 if (lambda < 1e-12) {
   lambda = 1e-6;
 }

 // Check if bounds are too close
 if (lambda_high / lambda_low < 1.01) {
   break;
 }
}

// If we found at least one valid solution, use the best one
if (best_lambda >= 0) {
 scaled_p = best_p;
 solve_success = true;
} else {
 // Fallback to steepest descent if no valid solution found
 scaled_p = -scaled_g * (trust_radius / arma::norm(scaled_g));
 solve_success = true;
}

// Unscale the step
arma::vec p(n);
for (int i = 0; i < n; i++) {
 p(i) = scaled_p(i) * param_scales(i);
}

// Convert step to NumericVector
Rcpp::NumericVector step = Rcpp::wrap(p);

// Apply step to get new parameters
Rcpp::NumericVector new_params = Rcpp::clone(params);
for (int i = 0; i < n; i++) {
 new_params[i] += step[i];
}

// Apply bounds with intelligent handling for different parameter types
if (enforce_bounds) {
 for (int i = 0; i < n; i++) {
   // Detect if this is a delta-type parameter (non-negative but can be zero)
   bool is_delta = (i == 3); // Default assumption for delta parameter

   // More intelligent check based on parameter name or position if available
   // (This could be enhanced with parameter type information if available)

   if (is_delta) {
     // For delta parameters (non-negative)
     new_params[i] = std::max(0.0, new_params[i]);
   } else {
     // For strictly positive parameters
     new_params[i] = std::max(min_param_val, new_params[i]);
   }

   // Upper bound for all parameters
   new_params[i] = std::min(max_param_val, new_params[i]);
 }
}

// Evaluate function at new point with error handling
double f_new;
bool valid_new_point = true;

try {
 f_new = ll_func(new_params, data);
 if (!R_finite(f_new)) {
   valid_new_point = false;
 }
} catch (...) {
 valid_new_point = false;
}

// Fallback for invalid new point
if (!valid_new_point) {
 // Try a smaller step (half the size)
 for (int i = 0; i < n; i++) {
   new_params[i] = params[i] + 0.5 * step[i];
 }

 // Re-apply bounds
 if (enforce_bounds) {
   for (int i = 0; i < n; i++) {
     bool is_delta = (i == 3);
     if (is_delta) {
       new_params[i] = std::max(0.0, new_params[i]);
     } else {
       new_params[i] = std::max(min_param_val, new_params[i]);
     }
     new_params[i] = std::min(max_param_val, new_params[i]);
   }
 }

 // Try evaluating at reduced step
 try {
   f_new = ll_func(new_params, data);
   valid_new_point = R_finite(f_new);
 } catch (...) {
   valid_new_point = false;
 }

 // If still invalid, reject the step
 if (!valid_new_point) {
   result["new_params"] = params;
   result["trust_radius"] = 0.1 * trust_radius;  // Reduce trust region more aggressively
   return result;
 }
}

// Compute actual reduction and predicted reduction
double actual_reduction = f0 - f_new;

// More numerically stable quadratic model prediction
double pred_reduction = 0.0;

// Direct calculation of pred_reduction = -g^T p - 0.5 * p^T H p
pred_reduction = -arma::dot(g_arma, p);

// Avoid potential catastrophic cancellation in the Hessian term
arma::vec Hp = H * p;
pred_reduction -= 0.5 * arma::dot(p, Hp);

// Compute ratio with safeguards
double rho = 0.0;

if (pred_reduction > 1e-10) {
 rho = actual_reduction / pred_reduction;
} else if (actual_reduction > 1e-10) {
 // Positive reduction but prediction was tiny - count as success
 rho = 2.0;  // Arbitrary value > 1
} else if (std::abs(actual_reduction) < 1e-10 && std::abs(pred_reduction) < 1e-10) {
 // Both reductions effectively zero - neutral
 rho = 1.0;
}

// Adaptive trust radius update based on ratio quality
if (rho < 0.25) {
 // Poor agreement, reduce radius more aggressively for very bad ratios
 if (rho < 0.0) {
   trust_radius = std::max(1e-8, 0.25 * trust_radius);
 } else {
   trust_radius = std::max(1e-8, 0.5 * trust_radius);
 }
} else if (rho > 0.75 && arma::norm(p) >= 0.8 * trust_radius) {
 // Good agreement and step near boundary, increase radius
 // More conservative growth for already large trust regions
 if (trust_radius > 10.0) {
   trust_radius = std::min(1.5 * trust_radius, 100.0);
 } else {
   trust_radius = std::min(2.0 * trust_radius, 10.0);
 }
} else if (0.25 <= rho && rho <= 0.75) {
 // Modest agreement, keep radius but consider slight increase if step is large
 if (arma::norm(p) >= 0.9 * trust_radius) {
   trust_radius = std::min(1.1 * trust_radius, 10.0);
 }
}

// Acceptance decision with more nuanced criteria
bool accept = false;

if (rho > eta) {
 // Standard acceptance criteria
 accept = true;
} else if (rho > 0 && actual_reduction > 0 && arma::norm(p) < 0.1 * trust_radius) {
 // Accept small steps with any positive reduction
 accept = true;
}

// Final results
result["new_params"] = new_params;
result["accept"] = accept;
result["trust_radius"] = trust_radius;
result["actual_reduction"] = actual_reduction;
result["ratio"] = rho;

return result;
}


//' @title Advanced Initialization for GKw Family Parameters
//' @description Generates intelligent initial parameter estimates for GKw
//' family distributions based on theoretical moment relationships and
//' distribution properties from Carrasco et al. (2010).
//'
//' @param data Data vector in (0,1)
//' @param family Distribution family (default: "gkw")
//' @param robust Logical; if TRUE, uses robust statistics for initialization (default: TRUE)
//' @return Vector of initial parameter estimates
//' @noRd
Rcpp::NumericVector smartInitGkw(
 Rcpp::NumericVector data,
 std::string family = "gkw",
 bool robust = true
) {
// Convert family to lowercase
std::string family_lower = family;
std::transform(family_lower.begin(), family_lower.end(),
              family_lower.begin(), ::tolower);

int n_data = data.size();

// Calculate summary statistics with optional robustness
double data_mean = 0.0;
double data_var = 0.0;
double data_min = 1.0;
double data_max = 0.0;

// First pass: mean, min, max
for (int i = 0; i < n_data; i++) {
 data_mean += data[i];
 data_min = std::min(data_min, data[i]);
 data_max = std::max(data_max, data[i]);
}
data_mean /= n_data;

// Second pass: variance, skewness, kurtosis
double m2 = 0.0;  // 2nd central moment
double m3 = 0.0;  // 3rd central moment
double m4 = 0.0;  // 4th central moment

for (int i = 0; i < n_data; i++) {
 double dev = data[i] - data_mean;
 double dev2 = dev * dev;
 m2 += dev2;
 m3 += dev2 * dev;
 m4 += dev2 * dev2;
}

m2 /= n_data;
m3 /= n_data;
m4 /= n_data;

data_var = m2;
double data_skew = m3 / std::pow(m2, 1.5);
double data_kurt = m4 / (m2 * m2) - 3.0;  // Excess kurtosis

// Use robust statistics if requested
if (robust && n_data > 10) {
 // Sort data for quantile-based statistics (trimmed mean, etc.)
 std::vector<double> sorted_data(data.begin(), data.end());
 std::sort(sorted_data.begin(), sorted_data.end());

 int trim_level = static_cast<int>(0.1 * n_data);  // 10% trimming

 // Calculate trimmed mean
 double trimmed_mean = 0.0;
 for (int i = trim_level; i < n_data - trim_level; i++) {
   trimmed_mean += sorted_data[i];
 }
 trimmed_mean /= (n_data - 2 * trim_level);

 // Calculate median
 double median;
 if (n_data % 2 == 0) {
   median = (sorted_data[n_data/2 - 1] + sorted_data[n_data/2]) / 2.0;
 } else {
   median = sorted_data[n_data/2];
 }

 // Calculate interquartile range for robust variance
 int q1_idx = n_data / 4;
 int q3_idx = 3 * n_data / 4;
 double q1 = sorted_data[q1_idx];
 double q3 = sorted_data[q3_idx];
 double iqr = q3 - q1;

 // Estimate robust variance using IQR (assumes approximately normal distribution)
 double robust_var = std::pow(iqr / 1.349, 2);

 // Use robust statistics if they seem more reliable
 if (robust_var > 0 && robust_var < data_var * 3) {
   data_var = robust_var;
 }

 // Use median instead of mean if distribution is highly skewed
 if (std::abs(data_skew) > 1.0) {
   data_mean = median;
 }
}

// Compute beta parameters using method of moments
// Based on E(X) and Var(X) for beta distribution
double alpha_beta = 0.0;
double beta_beta = 0.0;

if (data_var < data_mean * (1 - data_mean)) {
 // Valid moment estimates exist
 alpha_beta = data_mean * (1 - data_mean) / data_var - 1;
 alpha_beta = std::max(0.5, alpha_beta);

 beta_beta = alpha_beta * (1 - data_mean) / data_mean;
 beta_beta = std::max(0.5, beta_beta);
} else {
 // Use alternative based on range and mean (approximate method)
 alpha_beta = 1.0 + 4.0 * (data_mean - data_min) / (data_max - data_min);
 beta_beta = 1.0 + 4.0 * (data_max - data_mean) / (data_max - data_min);
}

// Compute Kumaraswamy parameters using theoretical relationship
// The relationship is approximate: shape1 ≈ √α and shape2 ≈ √β for similar shapes
double shape1_kw = std::sqrt(alpha_beta);
double shape2_kw = std::sqrt(beta_beta);

// Refine parameters based on distribution shape characteristics
double adjustment_factor = 0.0;

// Use skewness to refine shape adjustment (following theoretical moment relationships)
if (std::abs(data_skew) > 0.1) {
 // Skewness adjustment - Using the known relationship between
 // skewness and shape parameters
 adjustment_factor = std::min(0.3, std::abs(data_skew) * 0.1);

 if (data_skew > 0) {
   // Right-skewed (positive skew): Lower shape1, higher shape2
   shape1_kw = std::max(0.5, shape1_kw * (1.0 - adjustment_factor));
   shape2_kw = std::min(20.0, shape2_kw * (1.0 + adjustment_factor));
 } else {
   // Left-skewed (negative skew): Higher shape1, lower shape2
   shape1_kw = std::min(20.0, shape1_kw * (1.0 + adjustment_factor));
   shape2_kw = std::max(0.5, shape2_kw * (1.0 - adjustment_factor));
 }
}

// Use kurtosis for additional refinement
if (std::abs(data_kurt) > 0.5) {
 // Kurtosis adjustment
 adjustment_factor = std::min(0.2, std::abs(data_kurt) * 0.05);

 if (data_kurt > 0) {
   // Leptokurtic (heavy tails): Reduce both shapes to allow heavier tails
   shape1_kw = std::max(0.5, shape1_kw * (1.0 - adjustment_factor));
   shape2_kw = std::max(0.5, shape2_kw * (1.0 - adjustment_factor));
 } else {
   // Platykurtic (light tails): Increase both shapes for lighter tails
   shape1_kw = std::min(20.0, shape1_kw * (1.0 + adjustment_factor));
   shape2_kw = std::min(20.0, shape2_kw * (1.0 + adjustment_factor));
 }
}

// Adjust based on data concentration using the range
double range_factor = (data_max - data_min) / 0.9;

if (range_factor < 0.5) {
 // Highly concentrated data - Increase shape parameters
 double concentration_factor = 1.0 + 2.0 * (0.5 - range_factor);
 shape1_kw = std::min(25.0, shape1_kw * concentration_factor);
 shape2_kw = std::min(25.0, shape2_kw * concentration_factor);
} else if (range_factor > 0.95) {
 // Very dispersed data - Decrease shape parameters
 double dispersion_factor = 0.7 + 0.3 * std::min(1.0, range_factor - 0.95);
 shape1_kw = std::max(0.3, shape1_kw * dispersion_factor);
 shape2_kw = std::max(0.3, shape2_kw * dispersion_factor);
}

// Initialize additional parameters for the GKw family
double gamma_param = 1.0;
double delta_param = 0.2;  // Small but positive
double lambda_param = 1.0;

// For more complex distributions, initialize additional parameters
// based on theoretical insights from Carrasco et al. (2010)
if (family_lower == "gkw" || family_lower == "kkw" ||
   family_lower == "ekw" || family_lower == "mc") {

 // Lambda affects tails and moments significantly
 // Use data range and kurtosis to inform lambda estimate
 if (range_factor > 0.9 || std::abs(data_kurt) > 1.0) {
   // Wide range or high kurtosis suggests potential for lambda > 1
   lambda_param = 1.0 + std::min(15.0, range_factor * 10.0 * std::abs(data_kurt));
 }

 // For kkw and gkw, delta can be adjusted based on tail behavior
 if (family_lower == "gkw" || family_lower == "kkw" || family_lower == "bkw") {
   // Delta controls tail behavior
   // Higher values for more flexible tails, especially with extreme values
   if (data_kurt > 0.5) {
     delta_param = std::min(2.0, 0.2 + data_kurt * 0.2);
   }
 }

 // For gkw and bkw, gamma can be adjusted based on mode location
 if (family_lower == "gkw" || family_lower == "bkw") {
   // Gamma affects the mode location and skewness
   // Adjust based on skewness and mean/median relationship
   if (data_skew < -0.5) {
     gamma_param = std::max(0.5, 1.0 - std::abs(data_skew) * 0.2);
   } else if (data_skew > 0.5) {
     gamma_param = std::min(2.0, 1.0 + data_skew * 0.2);
   }
 }
}

// Create parameter vector based on family
Rcpp::NumericVector result;

if (family_lower == "gkw") {
 result = Rcpp::NumericVector(5);
 result[0] = shape1_kw;        // alpha
 result[1] = shape2_kw;        // beta
 result[2] = gamma_param;      // gamma
 result[3] = delta_param;      // delta
 result[4] = lambda_param;     // lambda
} else if (family_lower == "bkw") {
 result = Rcpp::NumericVector(4);
 result[0] = shape1_kw;        // alpha
 result[1] = shape2_kw;        // beta
 result[2] = gamma_param;      // gamma
 result[3] = delta_param;      // delta
} else if (family_lower == "kkw") {
 result = Rcpp::NumericVector(4);
 result[0] = shape1_kw;        // alpha
 result[1] = shape2_kw;        // beta
 result[2] = delta_param;      // delta
 result[3] = lambda_param;     // lambda
} else if (family_lower == "ekw") {
 result = Rcpp::NumericVector(3);
 result[0] = shape1_kw;        // alpha
 result[1] = shape2_kw;        // beta
 result[2] = lambda_param;     // lambda
} else if (family_lower == "mc" || family_lower == "mcdonald" || family_lower == "bp") {
 result = Rcpp::NumericVector(3);
 result[0] = alpha_beta;       // gamma
 result[1] = std::max(0.0, beta_beta - 1.0);  // delta
 result[2] = lambda_param;     // lambda
} else if (family_lower == "kw") {
 result = Rcpp::NumericVector(2);
 result[0] = shape1_kw;        // alpha
 result[1] = shape2_kw;        // beta
} else if (family_lower == "beta") {
 result = Rcpp::NumericVector(2);
 result[0] = alpha_beta;       // gamma
 result[1] = std::max(0.0, beta_beta - 1.0);  // delta
}

return result;
}


// Declaração de funções externas para log-verossimilhança, gradiente e hessiana
// para cada família de distribuição
// Essas declarações são necessárias para que o compilador reconheça essas funções
extern double llgkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grgkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hsgkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);

extern double llbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hsbkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);

extern double llkkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grkkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hskkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);

extern double llekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hsekw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);

extern double llmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hsmc(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);

extern double llkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grkw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hskw(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);

extern double llbeta(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericVector grbeta(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);
extern Rcpp::NumericMatrix hsbeta(const Rcpp::NumericVector& par, const Rcpp::NumericVector& data);


//' @title Enhanced Newton-Raphson Optimization for GKw Family Distributions
//' @author Enhanced by Lopes, J. E.
//' @keywords distribution optimization likelihood mle newton-raphson kumaraswamy mcdonald beta
//'
//' @description
//' An industrial-strength implementation of maximum likelihood estimation (MLE)
//' for the parameters of any distribution in the Generalized Kumaraswamy (GKw) family.
//' This function incorporates multiple advanced numerical techniques including trust region
//' methods, eigenvalue-based regularization, adaptive scaling, and sophisticated line
//' search to ensure robust convergence even for challenging datasets or extreme parameter values.
//'
//' @details
//' This enhanced algorithm provides robust parameter estimation for the Generalized
//' Kumaraswamy distribution and its subfamilies. The function implements several
//' advanced numerical optimization techniques to maximize the likelihood function
//' reliably even in difficult cases.
//'
//' \subsection{The GKw Family of Distributions}{
//' The Generalized Kumaraswamy (GKw) distribution, introduced by Carrasco, Ferrari,
//' and Cordeiro (2010), is a flexible five-parameter continuous distribution defined
//' on the standard unit interval (0,1). Its probability density function is given by:
//'
//' \deqn{f(x; \alpha, \beta, \gamma, \delta, \lambda) = \frac{\lambda\alpha\beta x^{\alpha-1}}{B(\gamma, \delta+1)}(1-x^{\alpha})^{\beta-1}[1-(1-x^{\alpha})^{\beta}]^{\gamma\lambda-1}\{1-[1-(1-x^{\alpha})^{\beta}]^{\lambda}\}^{\delta}}
//'
//' where \eqn{\alpha, \beta, \gamma, \lambda > 0} and \eqn{\delta \geq 0} are the model
//' parameters, and \eqn{B(\gamma, \delta+1)} is the beta function.
//'
//' The GKw distribution encompasses several important special cases:
//' \itemize{
//'   \item\bold{GKw} (5 parameters): \eqn{\alpha, \beta, \gamma, \delta, \lambda}
//'   \item\bold{BKw} (4 parameters): \eqn{\alpha, \beta, \gamma, \delta} (with \eqn{\lambda = 1})
//'   \item\bold{KKw} (4 parameters): \eqn{\alpha, \beta, \delta, \lambda} (with \eqn{\gamma = 1})
//'   \item\bold{EKw} (3 parameters): \eqn{\alpha, \beta, \lambda} (with \eqn{\gamma = 1, \delta = 0})
//'   \item\bold{Mc}  (3 parameters): \eqn{\gamma, \delta, \lambda} (with \eqn{\alpha = 1, \beta = 1})
//'   \item\bold{Kw}  (2 parameters): \eqn{\alpha, \beta} (with \eqn{\gamma = 1, \delta = 0, \lambda = 1})
//'   \item\bold{Beta}(2 parameters): \eqn{\gamma, \delta} (with \eqn{\alpha = 1, \beta = 1, \lambda = 1})
//' }
//' }
//'
//' \subsection{Trust Region Method with Levenberg-Marquardt Algorithm}{
//' The trust region approach restricts parameter updates to a region where the quadratic
//' approximation of the objective function is trusted to be accurate. This algorithm
//' implements the Levenberg-Marquardt variant, which solves the subproblem:
//'
//' \deqn{\min_p m_k(p) = -\nabla \ell(\theta_k)^T p + \frac{1}{2}p^T H_k p}
//' \deqn{\text{subject to } \|p\| \leq \Delta_k}
//'
//' where \eqn{\nabla \ell(\theta_k)} is the gradient, \eqn{H_k} is the Hessian, and \eqn{\Delta_k}
//' is the trust region radius at iteration \eqn{k}.
//'
//' The Levenberg-Marquardt approach adds a regularization parameter \eqn{\lambda} to the
//' Hessian, solving:
//'
//' \deqn{(H_k + \lambda I)p = -\nabla \ell(\theta_k)}
//'
//' The parameter \eqn{\lambda} controls the step size and direction:
//' \itemize{
//'   \item When \eqn{\lambda} is large, the step approaches a scaled steepest descent direction.
//'   \item When \eqn{\lambda} is small, the step approaches the Newton direction.
//' }
//'
//' The algorithm dynamically adjusts \eqn{\lambda} based on the agreement between the quadratic model and the actual function:
//'
//' \deqn{\rho_k = \frac{f(\theta_k) - f(\theta_k + p_k)}{m_k(0) - m_k(p_k)}}
//'
//' The trust region radius is updated according to:
//' \itemize{
//'   \item If \eqn{\rho_k < 0.25}, reduce the radius: \eqn{\Delta_{k+1} = 0.5\Delta_k}
//'   \item If \eqn{\rho_k > 0.75} and \eqn{\|p_k\| \approx \Delta_k}, increase the radius: \eqn{\Delta_{k+1} = 2\Delta_k}
//'   \item Otherwise, leave the radius unchanged: \eqn{\Delta_{k+1} = \Delta_k}
//' }
//'
//' The step is accepted if \eqn{\rho_k > \eta} (typically \eqn{\eta = 0.1}).
//' }
//'
//' \subsection{Eigenvalue-Based Hessian Regularization}{
//' For the Newton-Raphson method to converge, the Hessian matrix must be positive definite.
//' This algorithm uses eigendecomposition to create a positive definite approximation that
//' preserves the Hessian's structure:
//'
//' \deqn{H = Q\Lambda Q^T}
//'
//' where \eqn{Q} contains the eigenvectors and \eqn{\Lambda} is a diagonal matrix of eigenvalues.
//'
//' The regularized Hessian is constructed by:
//'
//' \deqn{\tilde{H} = Q\tilde{\Lambda}Q^T}
//'
//' where \eqn{\tilde{\Lambda}} contains modified eigenvalues:
//'
//' \deqn{\tilde{\lambda}_i = \max(\lambda_i, \epsilon)}
//'
//' with \eqn{\epsilon} being a small positive threshold (typically \eqn{10^{-6}}).
//'
//' This approach is superior to diagonal loading (\eqn{H + \lambda I}) as it:
//' \itemize{
//'   \item Preserves the eigenvector structure of the original Hessian
//'   \item Minimally modifies the eigenvalues needed to ensure positive definiteness
//'   \item Better maintains the directional information in the Hessian
//' }
//' }
//'
//' \subsection{Parameter Scaling for Numerical Stability}{
//' When parameters have widely different magnitudes, optimization can become numerically
//' unstable. The adaptive scaling system transforms the parameter space to improve conditioning:
//'
//' \deqn{\theta_i^{scaled} = s_i \theta_i}
//'
//' where scaling factors \eqn{s_i} are determined by:
//' \itemize{
//'   \item For large parameters (\eqn{|\theta_i| > 100}): \eqn{s_i = 100/|\theta_i|}
//'   \item For small parameters (\eqn{0 < |\theta_i| < 0.01}): \eqn{s_i = 0.01/|\theta_i|}
//'   \item Otherwise: \eqn{s_i = 1}
//' }
//'
//' The optimization is performed in the scaled space, with appropriate transformations
//' for the gradient and Hessian:
//'
//' \deqn{\nabla \ell(\theta^{scaled})_i = \frac{1}{s_i}\nabla \ell(\theta)_i}
//' \deqn{H(\theta^{scaled})_{ij} = \frac{1}{s_i s_j}H(\theta)_{ij}}
//'
//' The final results are back-transformed to the original parameter space before being returned.
//' }
//'
//' \subsection{Line Search with Wolfe Conditions}{
//' The line search procedure ensures sufficient decrease in the objective function when
//' taking a step in the search direction. The implementation uses Wolfe conditions which
//' include both:
//'
//' 1. Sufficient decrease (Armijo) condition:
//' \deqn{f(\theta_k + \alpha p_k) \leq f(\theta_k) + c_1 \alpha \nabla f(\theta_k)^T p_k}
//'
//' 2. Curvature condition:
//' \deqn{|\nabla f(\theta_k + \alpha p_k)^T p_k| \leq c_2 |\nabla f(\theta_k)^T p_k|}
//'
//' where \eqn{0 < c_1 < c_2 < 1}, typically \eqn{c_1 = 10^{-4}} and \eqn{c_2 = 0.9}.
//'
//' The step length \eqn{\alpha} is determined using polynomial interpolation:
//' \itemize{
//'   \item First iteration: quadratic interpolation
//'   \item Subsequent iterations: cubic interpolation using function and derivative values
//' }
//'
//' The cubic polynomial model has the form:
//' \deqn{a\alpha^3 + b\alpha^2 + c\alpha + d}
//'
//' The algorithm computes coefficients from values at two points, then finds the minimizer
//' of this polynomial subject to bounds to ensure convergence.
//' }
//'
//' \subsection{Adaptive Numerical Differentiation}{
//' When analytical derivatives are unreliable, the algorithm uses numerical differentiation
//' with adaptive step sizes based on parameter magnitudes:
//'
//' \deqn{h_i = \max(h_{min}, \min(h_{base}, h_{base} \cdot |\theta_i|))}
//'
//' where \eqn{h_{min}} is a minimum step size (typically \eqn{10^{-8}}), \eqn{h_{base}}
//' is a base step size (typically \eqn{10^{-5}}), and \eqn{\theta_i} is the parameter value.
//'
//' For computing diagonal Hessian elements, the central difference formula is used:
//'
//' \deqn{\frac{\partial^2 f}{\partial \theta_i^2} \approx \frac{f(\theta + h_i e_i) - 2f(\theta) + f(\theta - h_i e_i)}{h_i^2}}
//'
//' For mixed partial derivatives:
//'
//' \deqn{\frac{\partial^2 f}{\partial \theta_i \partial \theta_j} \approx \frac{f(\theta + h_i e_i + h_j e_j) - f(\theta + h_i e_i - h_j e_j) - f(\theta - h_i e_i + h_j e_j) + f(\theta - h_i e_i - h_j e_j)}{4h_i h_j}}
//'
//' The algorithm validates numerical differentiation by comparing with existing gradients
//' and adaptively adjusts step sizes when discrepancies are detected.
//' }
//'
//' \subsection{Stochastic Perturbation}{
//' To escape flat regions or local minima, the algorithm implements controlled stochastic
//' perturbation when progress stalls (detected by monitoring gradient norm changes):
//'
//' \deqn{\theta_i^{new} = \theta_i + \Delta\theta_i}
//'
//' where the perturbation \eqn{\Delta\theta_i} combines:
//' \itemize{
//'   \item A directed component opposite to the gradient: \eqn{-\text{sign}(\nabla \ell_i) \cdot 0.05 \cdot |\theta_i|}
//'   \item A random noise component: \eqn{U(-0.05|\theta_i|, 0.05|\theta_i|)}
//' }
//'
//' The perturbation is applied when:
//' \itemize{
//'   \item The relative change in gradient norm is below a threshold for several consecutive iterations
//'   \item The algorithm appears to be stuck in a non-optimal region
//' }
//'
//' The perturbation is accepted only if it improves the objective function value.
//' }
//'
//' \subsection{Multi-Start Strategy}{
//' For particularly challenging optimization landscapes, the algorithm can employ multiple
//' starting points:
//'
//' \itemize{
//'   \item Initial values are generated using moment-based estimation and domain knowledge about each distribution family
//'   \item Each initial point is randomly perturbed to explore different regions of the parameter space
//'   \item Independent optimization runs are performed from each starting point
//'   \item The best result (based on likelihood value and convergence status) is returned
//' }
//'
//' This approach increases the probability of finding the global optimum or a high-quality
//' local optimum, particularly for complex models with many parameters.
//' }
//'
//' \subsection{Advanced Parameter Initialization}{
//' Intelligent starting values are critical for convergence in complex models. The algorithm
//' uses data-driven initialization based on:
//'
//' \itemize{
//'   \item Method of moments estimators for beta parameters:
//'     \deqn{\alpha + \beta = \frac{\bar{x}(1-\bar{x})}{s^2} - 1}
//'     \deqn{\alpha = (\alpha + \beta)\bar{x}}
//'
//'   \item Transformations to Kumaraswamy parameters:
//'     \deqn{a_{Kw} = \sqrt{\alpha_{Beta}}}
//'     \deqn{b_{Kw} = \sqrt{\beta_{Beta}}}
//'
//'   \item Adjustments based on data skewness (detected via mean relative to 0.5)
//'
//'   \item Corrections based on data dispersion (range relative to (0,1) interval)
//' }
//'
//' The transformations between beta and Kumaraswamy parameters leverage the similarities
//' between these distributions while accounting for their parametric differences.
//' }
//'
//' \subsection{Hybrid Optimization Strategy}{
//' The algorithm can dynamically switch between trust region and Newton-Raphson methods
//' based on optimization progress:
//'
//' \itemize{
//'   \item Early iterations: trust region method for stability and global convergence properties
//'   \item Later iterations (when close to optimum): Newton-Raphson with line search for quadratic convergence rate
//' }
//'
//' The switching criteria are based on iteration count and gradient norm, with additional
//' logic to handle cases where one method consistently fails.
//' }
//'
//' @param start A numeric vector containing initial values for the parameters.
//'   If NULL, automatic initialization is used based on the dataset characteristics.
//'   The length and order must correspond to the selected \code{family}
//'   (e.g., \code{c(alpha, beta, gamma, delta, lambda)} for "gkw"; \code{c(alpha, beta)} for "kw";
//'   \code{c(gamma, delta)} for "beta").
//' @param data A numeric vector containing the observed data. All values must
//'   be strictly between 0 and 1.
//' @param family A character string specifying the distribution family. One of
//'   \code{"gkw"}, \code{"bkw"}, \code{"kkw"}, \code{"ekw"}, \code{"mc"},
//'   \code{"kw"}, or \code{"beta"}. Default: \code{"gkw"}.
//' @param tol Convergence tolerance. The algorithm stops when the Euclidean norm
//'   of the gradient is below this value, or if relative changes in parameters
//'   or the negative log-likelihood are below this threshold across consecutive
//'   iterations. Default: \code{1e-6}.
//' @param max_iter Maximum number of iterations allowed. Default: \code{100}.
//' @param verbose Logical; if \code{TRUE}, prints detailed progress information
//'   during optimization, including iteration number, negative log-likelihood,
//'   gradient norm, and step adjustment details. Default: \code{FALSE}.
//' @param optimization_method Character string specifying the optimization method:
//'   "trust-region" (default), "newton-raphson", or "hybrid" (starts with trust-region,
//'   switches to newton-raphson near convergence).
//' @param enforce_bounds Logical; if \code{TRUE}, parameter values are constrained
//'   to stay within \code{min_param_val}, \code{max_param_val} (and \eqn{\delta \ge 0})
//'   during optimization. Default: \code{TRUE}.
//' @param min_param_val Minimum allowed value for parameters constrained to be
//'   strictly positive (\eqn{\alpha, \beta, \gamma, \lambda}). Default: \code{1e-5}.
//' @param max_param_val Maximum allowed value for all parameters. Default: \code{1e5}.
//' @param adaptive_scaling Logical; if \code{TRUE}, parameters are automatically scaled
//'   to improve numerical stability. Default: \code{TRUE}.
//' @param use_stochastic_perturbation Logical; if \code{TRUE}, applies random perturbations
//'   when optimization stalls. Default: \code{TRUE}.
//' @param get_num_hess Logical; if \code{TRUE}, computes and returns a numerical
//'   approximation of the Hessian at the solution. Default: \code{FALSE}.
//' @param multi_start_attempts Integer specifying the number of different starting points
//'   to try if initial optimization fails to converge. Default: \code{3}.
//' @param eigenvalue_hessian_reg Logical; if \code{TRUE}, uses eigenvalue-based
//'   regularization for the Hessian matrix. Default: \code{TRUE}.
//' @param max_backtrack Integer specifying the maximum number of backtracking steps
//'   in line search. Default: \code{20}.
//' @param initial_trust_radius Initial radius for trust region method. Default: \code{1.0}.
//'
//' @return A list object of class \code{gkw_fit} containing the following components:
//' \item{parameters}{A named numeric vector with the estimated parameters.}
//' \item{loglik}{The maximized value of the log-likelihood function.}
//' \item{iterations}{Number of iterations performed.}
//' \item{converged}{Logical flag indicating whether the algorithm converged successfully.}
//' \item{param_history}{A matrix where rows represent iterations and columns represent parameter values.}
//' \item{loglik_history}{A vector of log-likelihood values at each iteration.}
//' \item{gradient}{The gradient vector at the final parameter estimates.}
//' \item{hessian}{The analytical Hessian matrix at the final parameter estimates.}
//' \item{std_errors}{A named numeric vector of approximate standard errors for the parameters.}
//' \item{aic}{Akaike Information Criterion.}
//' \item{bic}{Bayesian Information Criterion.}
//' \item{aicc}{AIC with small sample correction.}
//' \item{n}{The sample size.}
//' \item{status}{A character string indicating the termination status.}
//' \item{z_values}{A named numeric vector of Z-statistics for parameter significance testing.}
//' \item{p_values}{A named numeric vector of two-sided p-values corresponding to the Z-statistics.}
//' \item{param_names}{A character vector of the parameter names.}
//' \item{family}{The distribution family used.}
//' \item{optimization_method}{The optimization method used.}
//' \item{numeric_hessian}{The numerically approximated Hessian at the solution (if requested).}
//' \item{condition_number}{The condition number of the final Hessian, a measure of parameter identifiability.}
//' \item{scaling_factors}{The scaling factors used for parameters (if adaptive scaling was enabled).}
//'
//' @section Warning:
//' Although this implementation is highly robust, fitting complex distributions can still be challenging.
//' For best results:
//' \itemize{
//'   \item{Try multiple starting values if results seem suboptimal}
//'   \item{Examine diagnostic information carefully, especially condition numbers and standard errors}
//'   \item{Be cautious of parameter estimates at or very near boundaries}
//'   \item{Consider model simplification if convergence is consistently problematic}
//'   \item{For the full GKw model with 5 parameters, convergence may be sensitive to starting values}
//'   \item{High condition numbers (>1e6) may indicate parameter redundancy or weak identifiability}
//' }
//'
//' @references
//' Carrasco, J. M. F., Ferrari, S. L. P., & Cordeiro, G. M. (2010). A new generalized Kumaraswamy
//' distribution. arXiv preprint arXiv:1004.0911.
//'
//' Nocedal, J., & Wright, S. J. (2006). Numerical Optimization (2nd ed.). Springer.
//'
//' Conn, A. R., Gould, N. I. M., & Toint, P. L. (2000). Trust Region Methods. MPS-SIAM Series on Optimization.
//'
//' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded
//' random processes. Journal of Hydrology, 46(1-2), 79-88.
//'
//' @examples
//' \donttest{
//' # Generate sample data from a Beta(2,5) distribution for testing
//' set.seed(123)
//' sample_data <- rbeta_(200, 2, 5)
//'
//' # Automatic initialization (recommended for beginners)
//' result_auto <- nrgkw(NULL, sample_data, family = "beta", verbose = FALSE)
//' print(result_auto$parameters)
//' print(result_auto$loglik)
//'
//' # Compare different optimization methods
//' methods <- c("trust-region", "newton-raphson", "hybrid")
//' results <- list()
//'
//' for (method in methods) {
//'   results[[method]] <- nrgkw(NULL, sample_data, family = "beta",
//'                                optimization_method = method)
//'   cat(sprintf("Method: %s, AIC: %.4f\n", method, results[[method]]$aic))
//' }
//'
//' # Fit the full GKw model with diagnostic information
//' gkw_result <- nrgkw(NULL, sample_data, family = "gkw",
//'                       verbose = FALSE, get_num_hess = TRUE)
//'
//' # Examine parameter identifiability through condition number
//' cat(sprintf("Condition number: %.2e\n", gkw_result$condition_number))
//' print(gkw_result)
//'
//' # Compare with simpler models using information criteria
//' cat("Information criteria comparison:\n")
//' cat(sprintf("GKw: AIC=%.4f, BIC=%.4f\n", gkw_result$aic, gkw_result$bic))
//' cat(sprintf("Beta: AIC=%.4f, BIC=%.4f\n",
//'            results[["trust-region"]]$aic, results[["trust-region"]]$bic))
//'}
//' @export
// [[Rcpp::export]]
Rcpp::List nrgkw(
 Rcpp::Nullable<Rcpp::NumericVector> start = R_NilValue,
 Rcpp::NumericVector data = Rcpp::NumericVector::create(),
 std::string family = "gkw",
 double tol = 1e-6,
 int max_iter = 100,
 bool verbose = false,
 std::string optimization_method = "trust-region",
 bool enforce_bounds = true,
 double min_param_val = 1e-5,
 double max_param_val = 1e5,
 bool adaptive_scaling = true,
 bool use_stochastic_perturbation = true,
 bool get_num_hess = false,
 int multi_start_attempts = 3,
 bool eigenvalue_hessian_reg = true,
 int max_backtrack = 20,
 double initial_trust_radius = 1.0
) {
// Final result will be a list with different components
Rcpp::List result;

// Convert optimization method to lowercase
std::transform(optimization_method.begin(), optimization_method.end(),
              optimization_method.begin(), ::tolower);

// Validate optimization method
if (optimization_method != "trust-region" &&
   optimization_method != "newton-raphson" &&
   optimization_method != "hybrid") {
 Rcpp::stop("Invalid optimization_method. Use 'trust-region', 'newton-raphson', or 'hybrid'.");
}

// Convert family to lowercase for case-insensitive comparison
std::string family_lower = family;
std::transform(family_lower.begin(), family_lower.end(), family_lower.begin(), ::tolower);

// Determine number of parameters based on family
int n_params = 0;
Rcpp::CharacterVector param_names;

if (family_lower == "gkw") {
 n_params = 5;
 param_names = Rcpp::CharacterVector::create("alpha", "beta", "gamma", "delta", "lambda");
} else if (family_lower == "bkw") {
 n_params = 4;
 param_names = Rcpp::CharacterVector::create("alpha", "beta", "gamma", "delta");
} else if (family_lower == "kkw") {
 n_params = 4;
 param_names = Rcpp::CharacterVector::create("alpha", "beta", "delta", "lambda");
} else if (family_lower == "ekw") {
 n_params = 3;
 param_names = Rcpp::CharacterVector::create("alpha", "beta", "lambda");
} else if (family_lower == "mc" || family_lower == "mcdonald" || family_lower == "bp") {
 n_params = 3;
 param_names = Rcpp::CharacterVector::create("gamma", "delta", "lambda");
} else if (family_lower == "kw") {
 n_params = 2;
 param_names = Rcpp::CharacterVector::create("alpha", "beta");
} else if (family_lower == "beta") {
 n_params = 2;
 param_names = Rcpp::CharacterVector::create("gamma", "delta");
} else {
 Rcpp::stop("Unknown family: '" + family + "'. Available options are 'gkw', 'bkw', 'kkw', 'ekw', 'mc', 'kw', 'beta'.");
}

// Check for valid data
int n_data = data.size();
if (n_data < n_params) {
 Rcpp::stop("At least " + std::to_string(n_params) + " data points are needed to estimate " +
   std::to_string(n_params) + " parameters");
}

// Check if all data are in the interval (0,1)
for (int i = 0; i < n_data; i++) {
 if (data[i] <= 0.0 || data[i] >= 1.0 || !R_finite(data[i])) {
   Rcpp::stop("All data must be in the interval (0,1)");
 }
}

// Handle automatic initialization or user-provided start values
Rcpp::NumericVector user_start;

if (start.isNull()) {
 user_start = smartInitGkw(data, family);

 if (verbose) {
   Rcpp::Rcout << "Auto-initialized parameters: ";
   for (int i = 0; i < user_start.size(); i++) {
     Rcpp::Rcout << user_start[i] << " ";
   }
   Rcpp::Rcout << std::endl;
 }
} else {
 user_start = Rcpp::as<Rcpp::NumericVector>(start);

 // Validate initial parameters size
 if (user_start.size() != n_params) {
   Rcpp::stop("Invalid number of parameters for '" + family + "'. Expected " +
     std::to_string(n_params) + ", got " + std::to_string(user_start.size()));
 }
}

// Convert family-specific parameters to standard GKw format
Rcpp::NumericVector params(5); // Always use 5 parameters internally (GKw format)

// Set default values based on fixed parameters in specific families
params[0] = 1.0; // α = 1 default
params[1] = 1.0; // β = 1 default
params[2] = 1.0; // γ = 1 default
params[3] = 0.0; // δ = 0 default
params[4] = 1.0; // λ = 1 default

// Fill with provided parameters based on family
if (family_lower == "gkw") {
 for (int j = 0; j < 5; j++) {
   params[j] = user_start[j];
 }
} else if (family_lower == "bkw") {
 // α, β, γ, δ with λ = 1
 for (int j = 0; j < 4; j++) {
   params[j] = user_start[j];
 }
 params[4] = 1.0; // λ fixed at 1
} else if (family_lower == "kkw") {
 // α, β, δ, λ with γ = 1
 params[0] = user_start[0]; // α
 params[1] = user_start[1]; // β
 params[2] = 1.0;           // γ fixed at 1
 params[3] = user_start[2]; // δ
 params[4] = user_start[3]; // λ
} else if (family_lower == "ekw") {
 // α, β, λ with γ = 1, δ = 0
 params[0] = user_start[0]; // α
 params[1] = user_start[1]; // β
 params[2] = 1.0;           // γ fixed at 1
 params[3] = 0.0;           // δ fixed at 0
 params[4] = user_start[2]; // λ
} else if (family_lower == "mc" || family_lower == "mcdonald" || family_lower == "bp") {
 // γ, δ, λ with α = 1, β = 1
 params[0] = 1.0;           // α fixed at 1
 params[1] = 1.0;           // β fixed at 1
 params[2] = user_start[0]; // γ
 params[3] = user_start[1]; // δ
 params[4] = user_start[2]; // λ
} else if (family_lower == "kw") {
 // α, β with γ = 1, δ = 0, λ = 1
 params[0] = user_start[0]; // α
 params[1] = user_start[1]; // β
 params[2] = 1.0;           // γ fixed at 1
 params[3] = 0.0;           // δ fixed at 0
 params[4] = 1.0;           // λ fixed at 1
} else if (family_lower == "beta") {
 // γ, δ with α = 1, β = 1, λ = 1
 params[0] = 1.0;           // α fixed at 1
 params[1] = 1.0;           // β fixed at 1
 params[2] = user_start[0]; // γ
 params[3] = user_start[1]; // δ
 params[4] = 1.0;           // λ fixed at 1
}

// Apply constraints to initial parameters if needed
if (enforce_bounds) {
 for (int j = 0; j < 5; j++) {
   if (j == 3) { // delta
     params[j] = std::max(0.0, params[j]);
   } else { // other parameters must be > 0
     params[j] = std::max(min_param_val, params[j]);
   }
   params[j] = std::min(max_param_val, params[j]);
 }
}

// Define function pointers based on family
std::function<double(Rcpp::NumericVector, Rcpp::NumericVector)> ll_func;
std::function<Rcpp::NumericVector(Rcpp::NumericVector, Rcpp::NumericVector)> gr_func;
std::function<Rcpp::NumericMatrix(Rcpp::NumericVector, Rcpp::NumericVector)> hs_func;

// Assign appropriate functions based on family - usando expressões lambda
if (family_lower == "gkw") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llgkw(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grgkw(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hsgkw(p, d); };
} else if (family_lower == "bkw") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llbkw(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grbkw(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hsbkw(p, d); };
} else if (family_lower == "kkw") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llkkw(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grkkw(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hskkw(p, d); };
} else if (family_lower == "ekw") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llekw(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grekw(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hsekw(p, d); };
} else if (family_lower == "mc" || family_lower == "mcdonald" || family_lower == "bp") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llmc(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grmc(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hsmc(p, d); };
} else if (family_lower == "kw") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llkw(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grkw(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hskw(p, d); };
} else if (family_lower == "beta") {
 ll_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return llbeta(p, d); };
 gr_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return grbeta(p, d); };
 hs_func = [](Rcpp::NumericVector p, Rcpp::NumericVector d) { return hsbeta(p, d); };
}

// Function to extract relevant parameters for specific family
auto extractParams = [&](const Rcpp::NumericVector& full_params) -> Rcpp::NumericVector {
 Rcpp::NumericVector result;

 if (family_lower == "gkw") {
   result = Rcpp::NumericVector(5);
   for (int j = 0; j < 5; j++) result[j] = full_params[j];
 } else if (family_lower == "bkw") {
   result = Rcpp::NumericVector(4);
   for (int j = 0; j < 4; j++) result[j] = full_params[j];
 } else if (family_lower == "kkw") {
   result = Rcpp::NumericVector(4);
   result[0] = full_params[0]; // α
   result[1] = full_params[1]; // β
   result[2] = full_params[3]; // δ
   result[3] = full_params[4]; // λ
 } else if (family_lower == "ekw") {
   result = Rcpp::NumericVector(3);
   result[0] = full_params[0]; // α
   result[1] = full_params[1]; // β
   result[2] = full_params[4]; // λ
 } else if (family_lower == "mc" || family_lower == "mcdonald" || family_lower == "bp") {
   result = Rcpp::NumericVector(3);
   result[0] = full_params[2]; // γ
   result[1] = full_params[3]; // δ
   result[2] = full_params[4]; // λ
 } else if (family_lower == "kw") {
   result = Rcpp::NumericVector(2);
   result[0] = full_params[0]; // α
   result[1] = full_params[1]; // β
 } else if (family_lower == "beta") {
   result = Rcpp::NumericVector(2);
   result[0] = full_params[2]; // γ
   result[1] = full_params[3]; // δ
 }

 return result;
};

// Get family-specific parameters
Rcpp::NumericVector family_params = extractParams(params);

// Compute parameter scaling factors if enabled
Rcpp::NumericVector scaling_factors(family_params.size(), 1.0);
if (adaptive_scaling) {
 scaling_factors = computeScalingFactors(family_params);

 if (verbose) {
   Rcpp::Rcout << "Parameter scaling factors: ";
   for (int i = 0; i < scaling_factors.size(); i++) {
     Rcpp::Rcout << scaling_factors[i] << " ";
   }
   Rcpp::Rcout << std::endl;
 }

 // Apply scaling to initial parameters
 Rcpp::NumericVector scaled_params = Rcpp::clone(family_params);
 for (int i = 0; i < family_params.size(); i++) {
   scaled_params[i] *= scaling_factors[i];
 }
 family_params = scaled_params;

 // Update full parameter vector with scaled values
 params = Rcpp::clone(params);
 Rcpp::NumericVector full_scaling(5, 1.0);

 if (family_lower == "gkw") {
   for (int j = 0; j < 5; j++) full_scaling[j] = scaling_factors[j];
 } else if (family_lower == "bkw") {
   for (int j = 0; j < 4; j++) full_scaling[j] = scaling_factors[j];
 } else if (family_lower == "kkw") {
   full_scaling[0] = scaling_factors[0]; // α
   full_scaling[1] = scaling_factors[1]; // β
   full_scaling[3] = scaling_factors[2]; // δ
   full_scaling[4] = scaling_factors[3]; // λ
 } else if (family_lower == "ekw") {
   full_scaling[0] = scaling_factors[0]; // α
   full_scaling[1] = scaling_factors[1]; // β
   full_scaling[4] = scaling_factors[2]; // λ
 } else if (family_lower == "mc") {
   full_scaling[2] = scaling_factors[0]; // γ
   full_scaling[3] = scaling_factors[1]; // δ
   full_scaling[4] = scaling_factors[2]; // λ
 } else if (family_lower == "kw") {
   full_scaling[0] = scaling_factors[0]; // α
   full_scaling[1] = scaling_factors[1]; // β
 } else if (family_lower == "beta") {
   full_scaling[2] = scaling_factors[0]; // γ
   full_scaling[3] = scaling_factors[1]; // δ
 }

 for (int i = 0; i < 5; i++) {
   params[i] *= full_scaling[i];
 }
}

// Create scaled versions of likelihood and gradient functions
auto scaled_ll_func = [&](Rcpp::NumericVector params, Rcpp::NumericVector data) -> double {
 if (adaptive_scaling) {
   Rcpp::NumericVector unscaled_params = Rcpp::clone(params);

   // Unscale parameters before evaluation
   for (int i = 0; i < params.size(); i++) {
     unscaled_params[i] /= scaling_factors[i];
   }

   return ll_func(unscaled_params, data);
 } else {
   return ll_func(params, data);
 }
};

auto scaled_gr_func = [&](Rcpp::NumericVector params, Rcpp::NumericVector data) -> Rcpp::NumericVector {
 if (adaptive_scaling) {
   Rcpp::NumericVector unscaled_params = Rcpp::clone(params);

   // Unscale parameters
   for (int i = 0; i < params.size(); i++) {
     unscaled_params[i] /= scaling_factors[i];
   }

   // Get gradient at unscaled point
   Rcpp::NumericVector grad = gr_func(unscaled_params, data);

   // Adjust gradient for scaling
   for (int i = 0; i < grad.size(); i++) {
     grad[i] /= scaling_factors[i];
   }

   return grad;
 } else {
   return gr_func(params, data);
 }
};

auto scaled_hs_func = [&](Rcpp::NumericVector params, Rcpp::NumericVector data) -> Rcpp::NumericMatrix {
 if (adaptive_scaling) {
   Rcpp::NumericVector unscaled_params = Rcpp::clone(params);

   // Unscale parameters
   for (int i = 0; i < params.size(); i++) {
     unscaled_params[i] /= scaling_factors[i];
   }

   // Get Hessian at unscaled point
   Rcpp::NumericMatrix hess = hs_func(unscaled_params, data);

   // Adjust Hessian for scaling
   for (int i = 0; i < hess.nrow(); i++) {
     for (int j = 0; j < hess.ncol(); j++) {
       hess(i, j) /= (scaling_factors[i] * scaling_factors[j]);
     }
   }

   return hess;
 } else {
   return hs_func(params, data);
 }
};

// Calculate initial log-likelihood
double initial_loglik = scaled_ll_func(family_params, data);
if (!R_finite(initial_loglik) || initial_loglik == R_PosInf) {
 Rcpp::stop("Initial log-likelihood is infinite or NaN. Check the initial parameters.");
}

// Parameter and log-likelihood history for diagnostics
Rcpp::NumericMatrix param_history(max_iter + 1, n_params);
Rcpp::NumericVector loglik_history(max_iter + 1);

// Initialize history with initial values
for (int j = 0; j < n_params; j++) {
 param_history(0, j) = family_params[j];
}
loglik_history[0] = initial_loglik;

// Variables for convergence control
bool converged = false;
int iter = 0;
double prev_loglik = initial_loglik;

// Best result tracking
double best_loglik = initial_loglik;
Rcpp::NumericVector best_params = Rcpp::clone(family_params);

// Trust region radius
double trust_radius = initial_trust_radius;

// Tracking variables for algorithm adaptation
int consecutive_tr_failures = 0;
const int max_consecutive_failures = 3;

int stagnation_counter = 0;
const int stagnation_limit = 5;
double prev_grad_norm = 0.0;

// Main optimization loop
while (!converged && iter < max_iter) {
 iter++;

 // Calculate current log-likelihood and gradient
 double current_loglik = scaled_ll_func(family_params, data);
 Rcpp::NumericVector gradient = scaled_gr_func(family_params, data);

 // Check gradient validity
 bool valid_gradient = true;
 for (int j = 0; j < n_params; j++) {
   if (!R_finite(gradient[j])) {
     valid_gradient = false;
     break;
   }
 }

 if (!valid_gradient) {
   if (verbose) {
     Rcpp::Rcout << "Warning: Invalid gradient in iteration " << iter << std::endl;
   }

   // Use numerical gradient as fallback
   for (int j = 0; j < n_params; j++) {
     double eps = std::max(1e-6, 1e-6 * std::abs(family_params[j]));

     Rcpp::NumericVector params_p = Rcpp::clone(family_params);
     Rcpp::NumericVector params_m = Rcpp::clone(family_params);

     params_p[j] += eps;
     params_m[j] -= eps;

     double f_p = scaled_ll_func(params_p, data);
     double f_m = scaled_ll_func(params_m, data);

     if (R_finite(f_p) && R_finite(f_m)) {
       gradient[j] = (f_p - f_m) / (2 * eps);
     } else {
       // If numerical gradient also fails, use small step
       gradient[j] = 0.001 * ((j % 2 == 0) ? 1.0 : -1.0); // Alternating signs
     }
   }
 }

 // Calculate gradient norm for convergence check
 double grad_norm = 0.0;
 for (int j = 0; j < n_params; j++) {
   grad_norm += gradient[j] * gradient[j];
 }
 grad_norm = std::sqrt(grad_norm);

 // Check for stagnation
 if (std::abs(grad_norm - prev_grad_norm) < 0.01 * grad_norm) {
   stagnation_counter++;
 } else {
   stagnation_counter = 0;
 }
 prev_grad_norm = grad_norm;

 // Gradient convergence check
 if (grad_norm < tol) {
   converged = true;
   if (verbose) {
     Rcpp::Rcout << "Convergence detected: gradient norm (" << grad_norm
                 << ") < tolerance (" << tol << ")" << std::endl;
   }
   break;
 }

 // Determine optimization method for this iteration
 std::string current_method = optimization_method;

 // For hybrid method, adapt based on progress
 if (current_method == "hybrid") {
   if (iter > max_iter / 2 || grad_norm < 0.1) {
     current_method = "newton-raphson"; // Switch to Newton-Raphson near convergence
   } else {
     current_method = "trust-region";   // Use trust-region initially
   }

   if (verbose && iter > 1) {
     Rcpp::Rcout << "Hybrid method using: " << current_method << std::endl;
   }
 }

 // Switch methods if trust region is failing consistently
 if (consecutive_tr_failures >= max_consecutive_failures && current_method == "trust-region") {
   current_method = "newton-raphson";
   if (verbose) {
     Rcpp::Rcout << "Temporarily switching to Newton-Raphson after consecutive failures." << std::endl;
   }
 }

 // Apply stochastic perturbation if stagnating
 if (use_stochastic_perturbation && stagnation_counter >= stagnation_limit) {
   if (verbose) {
     Rcpp::Rcout << "Applying stochastic perturbation after " << stagnation_counter
                 << " iterations of stagnation." << std::endl;
   }

   // Create perturbed parameters
   Rcpp::NumericVector new_params = Rcpp::clone(family_params);

   GetRNGstate(); // Initialize R's RNG
   for (int j = 0; j < n_params; j++) {
     // Perturbation proportional to parameter and opposite to gradient
     double range = 0.1 * std::abs(family_params[j]);
     double direction = -1.0 * ((gradient[j] > 0) ? 1 : -1);
     double noise = R::runif(-0.5, 0.5);

     // Combined perturbation
     double perturb = direction * range * (0.5 + 0.5 * noise);
     new_params[j] += perturb;
   }
   PutRNGstate();

   // Apply bounds
   if (enforce_bounds) {
     for (int j = 0; j < n_params; j++) {
       bool is_delta = (family_lower == "gkw" && j == 3) ||
         (family_lower == "bkw" && j == 3) ||
         (family_lower == "kkw" && j == 2) ||
         (family_lower == "mc" && j == 1) ||
         (family_lower == "beta" && j == 1);

       if (is_delta) {
         new_params[j] = std::max(0.0, new_params[j]);
       } else {
         new_params[j] = std::max(min_param_val, new_params[j]);
       }
       new_params[j] = std::min(max_param_val, new_params[j]);
     }
   }

   // Evaluate at perturbed point
   double new_loglik = scaled_ll_func(new_params, data);

   // Accept if better
   if (R_finite(new_loglik) && new_loglik < current_loglik) {
     family_params = new_params;
     current_loglik = new_loglik;
     stagnation_counter = 0;

     if (verbose) {
       Rcpp::Rcout << "Stochastic perturbation successful, new loglik: " << -new_loglik << std::endl;
     }

     // Update history
     for (int j = 0; j < n_params; j++) {
       param_history(iter, j) = family_params[j];
     }
     loglik_history[iter] = new_loglik;

     // Update best result if improved
     if (new_loglik < best_loglik) {
       best_loglik = new_loglik;
       best_params = Rcpp::clone(family_params);
     }

     // Skip to next iteration
     continue;
   } else if (verbose) {
     Rcpp::Rcout << "Stochastic perturbation unsuccessful, continuing with standard update." << std::endl;
   }
 }

 // Parameter update using selected method
 Rcpp::NumericVector new_params;
 double new_loglik;
 bool update_success = false;

 if (current_method == "trust-region") {
   // Trust region update
   Rcpp::NumericMatrix rcpp_hessian = scaled_hs_func(family_params, data);

   // Check Hessian validity
   bool valid_hessian = true;
   for (int i = 0; i < n_params && valid_hessian; i++) {
     for (int j = 0; j < n_params; j++) {
       if (!R_finite(rcpp_hessian(i, j))) {
         valid_hessian = false;
         break;
       }
     }
   }

   if (!valid_hessian) {
     if (verbose) {
       Rcpp::Rcout << "Warning: Invalid Hessian, using numerical approximation." << std::endl;
     }

     // Use numerical Hessian
     arma::mat num_hessian = numHessianAdaptive(family_params, data,
                                                scaled_ll_func, scaled_gr_func);
     rcpp_hessian = Rcpp::wrap(num_hessian);
   }

   // Apply Hessian regularization if enabled
   if (eigenvalue_hessian_reg) {
     arma::mat H = Rcpp::as<arma::mat>(rcpp_hessian);
     arma::mat H_pd = makePositiveDefinite(H);
     rcpp_hessian = Rcpp::wrap(H_pd);

     if (verbose) {
       Rcpp::Rcout << "Applied eigenvalue-based Hessian regularization." << std::endl;
     }
   }

   // Trust region update
   Rcpp::List tr_result = trustRegionUpdate(
     family_params, data, scaled_ll_func, scaled_gr_func,
     [&](Rcpp::NumericVector p, Rcpp::NumericVector d) { return rcpp_hessian; },
     trust_radius, 0.1, enforce_bounds, min_param_val, max_param_val
   );

   new_params = Rcpp::as<Rcpp::NumericVector>(tr_result["new_params"]);
   trust_radius = Rcpp::as<double>(tr_result["trust_radius"]);
   bool accept = Rcpp::as<bool>(tr_result["accept"]);
   double ratio = Rcpp::as<double>(tr_result["ratio"]);

   new_loglik = scaled_ll_func(new_params, data);

   if (verbose) {
     Rcpp::Rcout << "Trust region step: radius = " << trust_radius
                 << ", ratio = " << ratio
                 << ", accept = " << (accept ? "true" : "false") << std::endl;
   }

   if (accept) {
     update_success = true;
     consecutive_tr_failures = 0;
   } else {
     consecutive_tr_failures++;

     // Fallback to steepest descent
     if (verbose) {
       Rcpp::Rcout << "Trust region step rejected, trying steepest descent." << std::endl;
     }

     new_params = Rcpp::clone(family_params);
     double gd_step = 0.1 * trust_radius / std::max(1.0, grad_norm);

     for (int j = 0; j < n_params; j++) {
       new_params[j] -= gd_step * gradient[j];
     }

     // Apply bounds
     if (enforce_bounds) {
       for (int j = 0; j < n_params; j++) {
         bool is_delta = (family_lower == "gkw" && j == 3) ||
           (family_lower == "bkw" && j == 3) ||
           (family_lower == "kkw" && j == 2) ||
           (family_lower == "mc" && j == 1) ||
           (family_lower == "beta" && j == 1);

         if (is_delta) {
           new_params[j] = std::max(0.0, new_params[j]);
         } else {
           new_params[j] = std::max(min_param_val, new_params[j]);
         }
         new_params[j] = std::min(max_param_val, new_params[j]);
       }
     }

     new_loglik = scaled_ll_func(new_params, data);

     if (R_finite(new_loglik) && new_loglik < current_loglik) {
       update_success = true;
       if (verbose) {
         Rcpp::Rcout << "Gradient descent successful." << std::endl;
       }
     }
   }
 } else { // Newton-Raphson update
   Rcpp::NumericMatrix rcpp_hessian = scaled_hs_func(family_params, data);

   // Check Hessian validity
   bool valid_hessian = true;
   for (int i = 0; i < n_params && valid_hessian; i++) {
     for (int j = 0; j < n_params; j++) {
       if (!R_finite(rcpp_hessian(i, j))) {
         valid_hessian = false;
         break;
       }
     }
   }

   if (!valid_hessian) {
     if (verbose) {
       Rcpp::Rcout << "Warning: Invalid Hessian, using numerical approximation." << std::endl;
     }

     // Use numerical Hessian
     arma::mat num_hessian = numHessianAdaptive(family_params, data,
                                                scaled_ll_func, scaled_gr_func);
     rcpp_hessian = Rcpp::wrap(num_hessian);
   }

   // Convert to arma for numerical operations
   arma::mat hessian = Rcpp::as<arma::mat>(rcpp_hessian);
   arma::vec grad_vec = Rcpp::as<arma::vec>(gradient);

   if (eigenvalue_hessian_reg) {
     // Enhanced regularization
     hessian = makePositiveDefinite(hessian);
   } else {
     // Simple diagonal addition
     hessian.diag() += 1e-6 * (1.0 + arma::max(arma::abs(hessian.diag())));
   }

   // Compute Newton direction
   arma::vec direction;
   bool solve_success = arma::solve(direction, hessian, -grad_vec);

   if (!solve_success) {
     if (verbose) {
       Rcpp::Rcout << "Warning: Failed to solve for Newton direction, using gradient." << std::endl;
     }

     // Use negative gradient if Newton direction cannot be computed
     direction = -grad_vec;

     // Scale the direction to control step size
     double scale_factor = 1.0 / std::max(1.0, arma::norm(direction));
     direction *= scale_factor;
   }

   // Use line search for step size
   Rcpp::NumericVector search_direction = Rcpp::wrap(direction);
   double alpha = lineSearchWolfe(
     family_params, search_direction, data,
     scaled_ll_func, scaled_gr_func, 1.0, 1e-4, 0.9, max_backtrack
   );

   if (verbose) {
     Rcpp::Rcout << "Line search: alpha = " << alpha << std::endl;
   }

   // Update parameters
   new_params = Rcpp::clone(family_params);
   for (int j = 0; j < n_params; j++) {
     new_params[j] += alpha * search_direction[j];
   }

   // Apply bounds if needed
   if (enforce_bounds) {
     for (int j = 0; j < n_params; j++) {
       bool is_delta = (family_lower == "gkw" && j == 3) ||
         (family_lower == "bkw" && j == 3) ||
         (family_lower == "kkw" && j == 2) ||
         (family_lower == "mc" && j == 1) ||
         (family_lower == "beta" && j == 1);

       if (is_delta) {
         new_params[j] = std::max(0.0, new_params[j]);
       } else {
         new_params[j] = std::max(min_param_val, new_params[j]);
       }
       new_params[j] = std::min(max_param_val, new_params[j]);
     }
   }

   // Evaluate at new point
   new_loglik = scaled_ll_func(new_params, data);

   if (R_finite(new_loglik) && new_loglik <= current_loglik + 1e-4) {
     update_success = true;
   }
 }

 // If update failed, try random perturbation as last resort
 if (!update_success) {
   if (verbose) {
     Rcpp::Rcout << "Update failed, applying random perturbation." << std::endl;
   }

   new_params = Rcpp::clone(family_params);
   GetRNGstate();
   for (int j = 0; j < n_params; j++) {
     // Small random perturbation
     double range = R::runif(0.01, 0.05) * std::abs(family_params[j]);
     double perturb = R::runif(-range, range);
     new_params[j] += perturb;
   }
   PutRNGstate();

   // Apply bounds
   if (enforce_bounds) {
     for (int j = 0; j < n_params; j++) {
       bool is_delta = (family_lower == "gkw" && j == 3) ||
         (family_lower == "bkw" && j == 3) ||
         (family_lower == "kkw" && j == 2) ||
         (family_lower == "mc" && j == 1) ||
         (family_lower == "beta" && j == 1);

       if (is_delta) {
         new_params[j] = std::max(0.0, new_params[j]);
       } else {
         new_params[j] = std::max(min_param_val, new_params[j]);
       }
       new_params[j] = std::min(max_param_val, new_params[j]);
     }
   }

   new_loglik = scaled_ll_func(new_params, data);

   if (R_finite(new_loglik) && new_loglik < current_loglik + 1e-2) {
     update_success = true;
     if (verbose) {
       Rcpp::Rcout << "Random perturbation successful." << std::endl;
     }
   } else {
     // If even random perturbation fails, use best parameters so far
     new_params = best_params;
     new_loglik = best_loglik;

     if (verbose) {
       Rcpp::Rcout << "All recovery attempts failed, using best parameters so far." << std::endl;
     }
   }
 }

 // Update parameters and history
 for (int j = 0; j < n_params; j++) {
   family_params[j] = new_params[j];
   param_history(iter, j) = family_params[j];
 }
 loglik_history[iter] = new_loglik;

 // Update best result if improved
 if (new_loglik < best_loglik) {
   best_loglik = new_loglik;
   for (int j = 0; j < n_params; j++) {
     best_params[j] = family_params[j];
   }
 }

 // Check parameter change convergence
 double param_change = 0.0;
 double param_rel_change = 0.0;
 for (int j = 0; j < n_params; j++) {
   double delta = family_params[j] - param_history(iter-1, j);
   param_change += std::pow(delta, 2);
   if (std::abs(family_params[j]) > 1e-10) {
     param_rel_change += std::pow(delta / family_params[j], 2);
   } else {
     param_rel_change += std::pow(delta, 2);
   }
 }
 param_change = std::sqrt(param_change);
 param_rel_change = std::sqrt(param_rel_change / n_params);

 // Check log-likelihood change convergence
 double loglik_change = std::abs(prev_loglik - new_loglik);
 double loglik_rel_change = loglik_change / (std::abs(prev_loglik) + 1e-10);
 prev_loglik = new_loglik;

 if (verbose) {
   Rcpp::Rcout << "Iteration " << iter
               << ", Log-likelihood: " << -new_loglik
               << ", Change: " << loglik_change
               << ", Rel. Change: " << loglik_rel_change
               << ", Gradient Norm: " << grad_norm
               << std::endl;

   Rcpp::Rcout << "Parameters: ";
   for (int j = 0; j < n_params; j++) {
     Rcpp::Rcout << family_params[j] << " ";
   }
   Rcpp::Rcout << std::endl;
 }

 // Check convergence criteria
 if (param_change < tol || param_rel_change < tol ||
     loglik_change < tol || loglik_rel_change < tol) {
   converged = true;
   if (verbose) {
     Rcpp::Rcout << "Convergence detected:" << std::endl;
     if (param_change < tol) Rcpp::Rcout << "- Absolute parameter change < tolerance" << std::endl;
     if (param_rel_change < tol) Rcpp::Rcout << "- Relative parameter change < tolerance" << std::endl;
     if (loglik_change < tol) Rcpp::Rcout << "- Absolute log-likelihood change < tolerance" << std::endl;
     if (loglik_rel_change < tol) Rcpp::Rcout << "- Relative log-likelihood change < tolerance" << std::endl;
   }
 }
}

// If not converged, use the best parameters found
if (!converged) {
 family_params = best_params;
 if (verbose) {
   Rcpp::Rcout << "Did not fully converge, using the best parameters found." << std::endl;
 }
}

// Unscale parameters if scaling was used
if (adaptive_scaling) {
 // Unscale final parameters
 for (int j = 0; j < n_params; j++) {
   family_params[j] /= scaling_factors[j];
 }

 // Unscale parameter history
 for (int i = 0; i <= iter; i++) {
   for (int j = 0; j < n_params; j++) {
     param_history(i, j) /= scaling_factors[j];
   }
 }

 if (verbose) {
   Rcpp::Rcout << "Final unscaled parameters: ";
   for (int j = 0; j < n_params; j++) {
     Rcpp::Rcout << family_params[j] << " ";
   }
   Rcpp::Rcout << std::endl;
 }
}

// Prepare final result
Rcpp::NumericMatrix final_param_history(iter + 1, n_params);
Rcpp::NumericVector final_loglik_history(iter + 1);

for (int i = 0; i <= iter; i++) {
 for (int j = 0; j < n_params; j++) {
   final_param_history(i, j) = param_history(i, j);
 }
 final_loglik_history[i] = loglik_history[i];
}

// Final computations with best parameters
double final_loglik = ll_func(family_params, data);
Rcpp::NumericVector final_gradient = gr_func(family_params, data);
Rcpp::NumericMatrix final_hessian = hs_func(family_params, data);

// Check final Hessian validity
bool valid_final_hessian = true;
for (int i = 0; i < n_params && valid_final_hessian; i++) {
 for (int j = 0; j < n_params; j++) {
   if (!R_finite(final_hessian(i, j))) {
     valid_final_hessian = false;
     break;
   }
 }
}

// Use numerical approximation if analytical Hessian is invalid
if (!valid_final_hessian) {
 if (verbose) {
   Rcpp::Rcout << "Final analytical Hessian invalid, using numerical approximation." << std::endl;
 }

 arma::mat num_hessian = numHessianAdaptive(family_params, data, ll_func, gr_func);
 final_hessian = Rcpp::wrap(num_hessian);
}

// Calculate numerical Hessian if requested
Rcpp::NumericMatrix numeric_hessian;
if (get_num_hess) {
 arma::mat num_hessian = numHessianAdaptive(family_params, data, ll_func, gr_func);
 numeric_hessian = Rcpp::wrap(num_hessian);
 result["numeric_hessian"] = numeric_hessian;
}

// Calculate standard errors using eigenvalue-regularized Hessian
Rcpp::NumericVector std_errors(n_params, NA_REAL);
bool valid_se = true;

// Calculate condition number of the Hessian
double condition_number = NA_REAL;
arma::mat arma_hessian = Rcpp::as<arma::mat>(final_hessian);
arma::vec eigval;
bool eig_success = arma::eig_sym(eigval, arma_hessian);

if (eig_success) {
 double max_eigval = arma::max(eigval);
 double min_eigval = arma::min(eigval);

 if (min_eigval > 0) {
   condition_number = max_eigval / min_eigval;
 } else {
   condition_number = R_PosInf;
 }
}

// Use robust approach to calculate standard errors
try {
 // Make Hessian positive definite
 arma::mat hessian_pd = makePositiveDefinite(arma_hessian);

 // Compute covariance matrix
 arma::mat cov_matrix;
 bool inv_success = false;

 // Try various inversion methods
 try {
   cov_matrix = arma::inv_sympd(hessian_pd);
   inv_success = true;
 } catch (...) {
   try {
     cov_matrix = arma::inv(hessian_pd);
     inv_success = true;
   } catch (...) {
     try {
       cov_matrix = arma::pinv(hessian_pd);
       inv_success = true;
     } catch (...) {
       inv_success = false;
     }
   }
 }

 // Extract standard errors if inversion succeeded
 if (inv_success) {
   arma::vec diag_cov = cov_matrix.diag();

   for (int j = 0; j < n_params; j++) {
     if (diag_cov(j) > 0) {
       std_errors[j] = std::sqrt(diag_cov(j));
     } else {
       std_errors[j] = NA_REAL;
     }
   }
 } else {
   valid_se = false;
 }
} catch (...) {
 valid_se = false;
}

if (!valid_se && verbose) {
 Rcpp::Rcout << "Could not calculate standard errors. The Hessian may not be positive definite." << std::endl;
}

// Calculate information criteria
double aic = 2 * n_params + 2 * final_loglik;
double bic = n_params * std::log(n_data) + 2 * final_loglik;
double aicc = aic;
if (n_data > n_params + 1) {
 aicc += 2 * n_params * (n_params + 1) / (n_data - n_params - 1);
}

// Fill the result
result["parameters"] = family_params;
result["loglik"] = -final_loglik;
result["iterations"] = iter;
result["converged"] = converged;
result["param_history"] = final_param_history;
result["loglik_history"] = -final_loglik_history;
result["gradient"] = final_gradient;
result["hessian"] = final_hessian;
result["std_errors"] = std_errors;
result["aic"] = aic;
result["bic"] = bic;
result["aicc"] = aicc;
result["n"] = n_data;
result["family"] = family;
result["optimization_method"] = optimization_method;

if (adaptive_scaling) {
 result["scaling_factors"] = scaling_factors;
}

if (eig_success) {
 result["condition_number"] = condition_number;
}

if (!converged && !result.containsElementNamed("status")) {
 result["status"] = "max_iterations_reached";
} else if (converged) {
 result["status"] = "success";
}

// Calculate statistical significance metrics
Rcpp::NumericVector z_values(n_params, NA_REAL);
Rcpp::NumericVector p_values(n_params, NA_REAL);

if (valid_se) {
 for (int j = 0; j < n_params; j++) {
   if (std_errors[j] != NA_REAL && std_errors[j] > 0) {
     z_values[j] = family_params[j] / std_errors[j];
     p_values[j] = 2.0 * R::pnorm(-std::abs(z_values[j]), 0.0, 1.0, 1, 0);
   }
 }
 result["z_values"] = z_values;
 result["p_values"] = p_values;
}

// Set parameter names
colnames(final_param_history) = param_names;
result["param_names"] = param_names;

// Multi-start optimization if needed
if (!converged && multi_start_attempts > 0) {
 if (verbose) {
   Rcpp::Rcout << "Initial optimization did not converge. Trying with different starting values..." << std::endl;
 }

 bool multi_start_success = false;
 Rcpp::List best_result = Rcpp::clone(result);
 double best_multi_loglik = -Rcpp::as<double>(result["loglik"]);

 for (int attempt = 0; attempt < multi_start_attempts; attempt++) {
   if (verbose) {
     Rcpp::Rcout << "Multi-start attempt " << (attempt + 1) << " of " << multi_start_attempts << std::endl;
   }

   // Generate new starting values
   Rcpp::NumericVector new_start = smartInitGkw(data, family);

   // Perturb the starting values
   GetRNGstate();
   for (int j = 0; j < new_start.size(); j++) {
     // Perturb by up to ±30%
     double perturb_factor = R::runif(0.7, 1.3);
     new_start[j] *= perturb_factor;

     // Ensure valid values
     if (j == new_start.size() - 1 && (family_lower == "bkw" || family_lower == "beta")) {
       // For delta parameter
       new_start[j] = std::max(0.0, new_start[j]);
     } else {
       new_start[j] = std::max(min_param_val, new_start[j]);
     }
     new_start[j] = std::min(max_param_val, new_start[j]);
   }
   PutRNGstate();

   if (verbose) {
     Rcpp::Rcout << "New starting values: ";
     for (int j = 0; j < new_start.size(); j++) {
       Rcpp::Rcout << new_start[j] << " ";
     }
     Rcpp::Rcout << std::endl;
   }

   // Try optimization with new starting values
   try {
     Rcpp::List new_result = nrgkw(
       new_start, data, family, tol, max_iter / 2,
       verbose, optimization_method, enforce_bounds,
       min_param_val, max_param_val, adaptive_scaling,
       use_stochastic_perturbation, false, 0, // No further multi-start
       eigenvalue_hessian_reg, max_backtrack, initial_trust_radius
     );

     bool new_converged = Rcpp::as<bool>(new_result["converged"]);
     double new_multi_loglik = -Rcpp::as<double>(new_result["loglik"]);

     if (verbose) {
       Rcpp::Rcout << "Multi-start result: converged = " << (new_converged ? "true" : "false")
                   << ", loglik = " << -new_multi_loglik << std::endl;
     }

     // Keep the best result
     if (new_converged || (!multi_start_success && new_multi_loglik < best_multi_loglik)) {
       best_result = new_result;
       best_multi_loglik = new_multi_loglik;
       multi_start_success = new_converged;

       if (verbose) {
         Rcpp::Rcout << "Found better solution with multi-start!" << std::endl;
       }

       // Stop early if converged
       if (multi_start_success) {
         break;
       }
     }
   } catch (...) {
     if (verbose) {
       Rcpp::Rcout << "Multi-start attempt failed with an exception." << std::endl;
     }
   }
 }

 // Return the best result from multi-start attempts
 if (multi_start_success || best_multi_loglik < -Rcpp::as<double>(result["loglik"])) {
   return best_result;
 }
}

return result;
}
