#include <cpp11.hpp>
#include <cfloat> // for DBL_EPSILON
#include <Rmath.h>

using namespace cpp11;


const double M_1_E = exp(-1.0);

//' Difference in Log-Space
//'
//' difference of two values in log-space
//' @param lx log of first value
//' @param ly log of second value
//' @returns difference in log-space: `log(exp(lx)-exp(ly))`
//' @export
[[cpp11::register]]
double logspace_sub_cpp(double lx, double ly) {
  // use logspace_sub from Rmath
  return logspace_sub(lx, ly);
}

//' Difference in log-space
//'
//' log-space difference of first two values in a vector
//' @param lxy vector with two values
//' @returns difference in log-space: `log(exp(lxy[2nd]) - exp(lxy[1st]))`
//' @export
[[cpp11::register]]
double logspace_sub2_cpp(doubles lxy) {
  // use logspace_sub from Rmath
  return logspace_sub(lxy[1], lxy[0]);
}

//' Lambert W function, its principal real-valued branch
//'
//' Compute the principal real-valued branch of the Lambert W function, W_0(x), i.e. W_0(x) satisfies W_0(x) * exp(W_0(x)) = x.
//' The function is real valued and defined for the real numbers x >= -exp(-1).
//' 
//' @details
//' The implementation is based on a recursive approximation method described by Iacono and Boyd (2017). We use start values as described by Loczi (2021) and perform a fixed number of iterations.
//' Currently, the implementation is not vectorized. You would need to apply it element-wise to a vector, e.g. using `vapply` or `Vectorize`.
//' @param x input double value
//' @returns W_0(x) or NaN if x is out of range (x <= -exp(-1))
//' @reference Roberto Iacono and John P. Boyd. 2017. New approximations to the principal real-valued branch of the Lambert W-function. Adv. Comput. Math. 43, 6 (December  2017), 1403–1436. https://doi.org/10.1007/s10444-017-9530-3
//' @reference Explicit and recursive estimates of the Lambert W function, Lajos Loczi, arXiv:2008.06122v3 [math.NA] 20 May 2021
//' @export
[[cpp11::register]]
double lambertW0_cpp(double x) {

  
  // special cases
  if (std::fabs(x) < DBL_EPSILON) {
    return 0.0;
  }
  
  if (std::fabs(x - M_E) < DBL_EPSILON) {
    return 1.0;
  }

  if (std::fabs(x + M_1_E) < DBL_EPSILON) {
    return -1.0;
  }

  if (x < -M_1_E) {
    // argument x too small, real valued W_0(x) is not defined for x <= -exp(-1)
    //cpp11::stop("Lambert W function on principal branch is not defined for x <= -exp(-1)");
    return NAN;
  }

  double b_next;

  // start value
  if (x < 0) {
    // negative x
    const double ex = M_E * x;
    const double s1pex = sqrt(1+ex);
    b_next = ex * log(1.0 + s1pex) / (s1pex * (1.0 + s1pex));
  } else if (x < M_E) {
    // medium x
    b_next = x / M_E;
  } else {
    // large x
    b_next = log(x) - log(log(x));
  }

  // some iterations of recursion
  // we have more iterations for smaller x, as convergence is slower there
  //XXX could check for convergence instead of fixed number..
  const int NBR_OF_ITERATIONS = (x > M_E) ? 5 : 9;

  for (int i = 0; i < NBR_OF_ITERATIONS; i++) {
    b_next = b_next / (1.0 + b_next) * (1.0 + log(x / b_next));
  }
  return b_next;
}

//' Objective function in order to find the upper limit of the uniform censoring distribution for the delayed Weibull distribution
//'
//' The function variable x stands for ((Z-delay) / scale)^shape, where Z is the upper limit of the uniform censoring distribution 
//' A root gives a solution for x, which can be used to solve for Z. 
//' For more details of the derivation, check the in vignette "delayed-dist.Rmd". We give the objective function on log-scale for greater numerically robustness.
//' @param x variable in the root function, defined as ((Z-delay) / scale)^shape, where Z is the upper limit of the uniform censoring distribution
//' @param shape shape parameter of the Weibull distribution
//' @param cens_prob censoring probability
//' @returns value of the root function at x
[[cpp11::register]]
double rootF_cens_unif_weib_cpp(double x, double shape, double cens_prob) {
  
  const double shapeInv = 1.0 / shape;
  return log(shapeInv) - shapeInv * log(x) + pgamma(x, shapeInv, 1.0, 1, 1) + lgammafn(shapeInv) - log(cens_prob);
  //lower_inc_gamma(k,x) on original scale in R's C-fn: pgamma(x, k, 1.0, 1, 0) * exp(lgammafn(k));
}

