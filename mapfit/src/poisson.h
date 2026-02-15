#ifndef MAPFIT_POISSON_H
#define MAPFIT_POISSON_H

#include <cmath>
#include "traits.h"

namespace poi {

static const double pi = 4.0 * atan(1.0);
static const double NormalQ_LowerQ = 3.0;
static const double NormalQ_UpperQ = 37.0;
// static const double NormalQ_LowerLogP = -689.0;
// static const double NormalQ_UpperLogP = -6.6;
static const double NormalQ_eps = 1.0e-8;
static const double LOG2PIOVER2 = log(2*pi) / 2.0;
static const double POISSON_LAMBDA_MIN = 3.0;
static const int POISSON_RIGHT_MAX = 23;

inline
  double normalt(double x) {
    double x2 = x * x;
    double tmp = x;
    double sum = 1.0 / tmp;
    tmp *= x2;
    sum -= 1.0 / tmp;
    tmp *= x2;
    sum += 3.0 / tmp;
    tmp *= x2;
    sum -= 15.0 / tmp;
    tmp *= x2;
    sum += 105.0 / tmp;
    return (log(sum) - x2/2.0 - LOG2PIOVER2);
  }

inline
  double normalq(double p) {
    const double leps = log(p);
    //		assert(leps <= NORMALQ_UPPER_LOGP && leps >= NORMALQ_LOWER_LOGP);
    double l = NormalQ_LowerQ;
    double u = NormalQ_UpperQ;
    double m = (l + u) / 2;
    double fm = normalt(m) - leps;
    while (std::abs(fm) > NormalQ_eps) {
      if (fm > 0) {
        l = m;
      } else {
        u = m;
      }
      m = (l + u)/2;
      fm = normalt(m) - leps;
    }
    return m;
  }

/**
 * @brief Compute the right bound of Poisson range
 * for a given error tolerance
 * @param lambda a Poisson rate (mean)
 * @param eps a value of error tolerance
 * @return right bound is a return value
 * */
inline
  int rightbound(double lambda, double eps) {
    if (std::fpclassify(lambda) == FP_ZERO) {
      return 0;
    }
    if (lambda < POISSON_LAMBDA_MIN) {
      double tmp = std::exp(-lambda);
      double total = tmp;
      int right = 0;
      for (int k=1; k<=POISSON_RIGHT_MAX; k++) {
        right += 1;
        tmp *= lambda / right;
        total += tmp;
        if (total + eps >= 1)
          break;
      }
      return right;
    } else {
      double z = normalq(eps);
      double tmp = z + std::sqrt(4 * lambda - 1);
      return static_cast<int>(tmp * tmp / 4 + 1);
    }
  }

/**
 * @brief Compute poisson probability vector
 * @param lambda a Poisson parameter
 * @param left an int of left bound
 * @param right an int of right bound
 * @param prob Poisson probabilities from left to right bounds
 * @return weight value, i.e., exact pmf is prob[i]/weight
 * */

template<typename T>
double pmf(double lambda, int left, int right, T& prob) {
  static const double pi = 4.0 * atan(1.0);
  static const double LOG2PIOVER2 = log(2*pi) / 2.0;
  using traits = vector_traits<T>;
  double* value = traits::value(prob);
  // assert(n >= right - left + 1);
  
  int mode = static_cast<int>(lambda);
  if (mode >= 1) {
    value[mode-left] = std::exp(-lambda + mode * std::log(lambda)
                                  - LOG2PIOVER2 - (mode + 1.0/2.0) * std::log(mode) + mode);
  } else {
    value[mode-left] = std::exp(-lambda);
  }
  // -- down --
  for (int j=mode; j>=left+1; j--) {
    value[j-1-left] = value[j-left] * j / lambda;
  }
  // -- up --
  for (int j=mode; j<=right-1; j++) {
    value[j+1-left] = value[j-left] * lambda / (j+1);
  }
  // -- compute W --
  double weight = 0.0;
  int s = left;
  int t = right;
  while (s < t) {
    if (value[s-left] <= value[t-left]) {
      weight += value[s-left];
      s++;
    } else {
      weight += value[t-left];
      t--;
    }
  }
  weight += value[s-left];
  return weight;
}

template<typename T1, typename T2>
double cpmf(double lambda, int left, int right, T1& prob, T2& cprob) {
  using traits1 = vector_traits<T1>;
  double* poi = traits1::value(prob);
  using traits2 = vector_traits<T2>;
  double* cpoi = traits2::value(cprob);
  
  double weight = pmf(lambda, left, right, prob);
  cpoi[right-left] = 0.0;
  for (int k=right-1; k>=left; k--) {
    cpoi[k-left] = cpoi[k+1-left] + poi[k+1];
  }
  return weight;
}

}

#endif
