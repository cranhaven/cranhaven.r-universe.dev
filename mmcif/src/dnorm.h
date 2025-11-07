#ifndef DPNORM_H
#define DPNORM_H

namespace ghqCpp {
inline double dnrm_log(double const x){
  // dput(sqrt(.Machine$double.xmax / 10))
  constexpr double sqrt_double_max{4.23992114886859e+153},
                      log_sqrt_2pi{0.918938533204673};
  return x > sqrt_double_max ? -std::numeric_limits<double>::infinity()
                             : -log_sqrt_2pi - x * x / 2;
}

inline double dnrm_log(double const x, double const sd){
  return dnrm_log(x / sd) - log(sd);
}
}

#endif
