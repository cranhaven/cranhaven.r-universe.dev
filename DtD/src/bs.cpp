#include "bs.h"
#include <Rcpp.h>
#include <math.h>

inline double BS_call_cpp_comp(
    const double V, const double T, const double r,
    const double vol, const double log_D, const double D_present){
  double denom = vol * std::sqrt(T);
  double d = (std::log(V) - log_D + (r + vol * vol / 2) *  T) / denom;

  return
    R::pnorm(d, 0, 1, 1, 0) * V - R::pnorm(d - denom, 0, 1, 1, 0) * D_present;
}

// [[Rcpp::export]]
double BS_call_cpp(
    const double V, const double D, const double T, const double r,
    const double vol){
  return BS_call_cpp_comp(V, T, r, vol, std::log(D), D * std::exp(-r * T));
}



/*
 * Bisection method. See
 *     https://en.wikipedia.org/wiki/Bisection_method#Algorithm
 * We do not have an upper bound though
 */
double BS_call_cpp_inv(
    const double S, const double D, const double T, const double r,
    const double vol, const double tol,
    double V_min, double V_max, double V_mid){
  const double log_D = std::log(D);
  const double D_present = D * std::exp(-r * T);

  // define function
  bool scale = std::abs(S) > tol;
  auto func = [&](double x){
    double diff = BS_call_cpp_comp(x, T, r, vol, log_D, D_present) - S;
    return(scale ? diff / S : S);
  };

  // quick check
  if(V_min < 0)
    Rcpp::stop("Invalid `V_min`");
  if(V_min > V_max or V_mid <= V_min or V_mid >= V_max)
    Rcpp::stop("Invalid `V_min`, `V_max` and `V_mid`");

  // check that min and max are different to start with
  int n = 0L;
  double f_min, f_max, f_mid;

  while(true){
    f_min = func(V_min);
    f_max = func(V_max);

    if(std::signbit(f_min) != std::signbit(f_max) and
         (f_max - f_min) / (std::abs(f_min) + 1e-8) > 1e-8)
      break;

    V_max = 2 * V_max - V_min;
    V_min = V_min / 2;

    if(++n > 1000L)
      Rcpp::stop("Failed to find valid `V_min` and `V_max`");
  }

  // Bisection method
  for(n = 0L; n < 1000L; ++n){
    f_mid = func(V_mid);

    if(std::abs(f_mid) < tol)
      return V_mid;

    if(std::signbit(f_mid) == std::signbit(f_min)){
      V_min = V_mid;
    } else
      V_max = V_mid;

    V_mid = (V_min + V_max) / 2;
  }

  Rcpp::stop("Failed to invert BS call");
}
