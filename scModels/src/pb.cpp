#include "shared.h"
#define N_TERMS(x) (x / 1000 + 1) * 2000
using namespace Rcpp;

// kummer fn: Taylor series method
double kummer_taylor(double x, double a, double b) {
  mpfr::mpreal aj = 1;
  mpfr::mpreal sj = aj;
  mpfr::mpreal tol = 1e-6, err = 1.0, j = 0.0;
  mpfr::mpreal aj1 = 0.0, sj1 = 0.0;
  mpfr::mpreal x_mp = mpfr::mpreal(x), a_mp = mpfr::mpreal(a), b_mp = mpfr::mpreal(b);
  while (err > tol) {
    aj1 = aj*(a+j)*x/((b+j)*(j+1));
    sj1 = sj + aj1;
    aj = aj1;
    sj = sj1;
    err = mpfr::abs(aj1);
    j = j+1;
  }
  return mpfr::log(sj).toDouble();
}


// Exponential transformation of the kummer fn for -ve x
// kummer_(x=-c, a=alpha+x, b=beta+alpha+x)
// since positive x is easier to compute
// M(a,b,x) = exp(x)M(b-a,b,-x)
double kummer_exp(double x, double a, double b) {
  return x + kummer_taylor(-x, b-a, b);
}


// kummer series using mpfr
// returns only logarithmic values
double kummer_(double x, double a, double b) {
  if(!validKummerParameters(a, b)) {
    return R_NaN;
  }
  if(x < 0) {
    return(kummer_exp(x, a, b));
  }
  else {
    return(kummer_taylor(x, a, b));
  }
}


// density function
double dpb_(double x, double alpha, double beta, double c, const bool& log_p, bool& throw_warning) {
  if( isInadmissible(x) || isInadmissible(alpha) || isInadmissible(beta) || isInadmissible(c) )
    return x+alpha+beta+c;

  if( !isInteger(x) || x < 0  || traits::is_infinite<REALSXP>(x) )
    return 0;

  if(!validPbParameters(alpha, beta, c)) {
    throw_warning = true;
    return R_NaN;
  }

  double cre = kummer_(-c, alpha+x, beta+alpha+x);
  double res;
  if(isInadmissible(cre))
    return R_NaN;

  if(x <= 0) {
    res = cre;
  } else {
    int sign = (x-1 < 0) ? -1 : 1;
    int x2 = (x-1 > 0) ? (int)std::floor(x-1) : (int)std::floor(1-x);
    double num = 0, denom = 0;
    for(int i=0; i <= x2; i++) {
      num += log((alpha + sign*i));
      denom += log(alpha + beta + sign * i);
    }
    num += x * log(c);
    denom += lgamma(x+1);
    res = num-denom+cre;
  }
  if(log_p)
    return res;
  else
    return exp(res);
}

// distribution function
double ppb_(double x, double alpha, double beta, double c, bool& throw_warning) {
  if( isInadmissible(x) || isInadmissible(alpha) || isInadmissible(beta) || isInadmissible(c) )
    return x+alpha+beta+c;

  if(!validPbParameters(alpha, beta, c)) {
    throw_warning = true;
    return R_NaN;
  }

  if( !isInteger(x) )
    return 0;
  if(traits::is_infinite<REALSXP>(x))
    return 1;
  double res = 0;
  for(int i = 0; i <= x; i++) {
    res += dpb_(i, alpha, beta, c, false, throw_warning);
  }
  return res;
}

// distribution function array
double* ppb_(double alpha, double beta, double c) {
  double *res = (double *)std::malloc(Q_LIMIT * sizeof(double));
  bool throw_warning = false;
  res[0] = dpb_(0, alpha, beta, c, false, throw_warning);
  for(int i = 1; i < Q_LIMIT; i++) {
    res[i] = res[i-1] + dpb_(i, alpha, beta, c, false, throw_warning);
  }
  return res;
}

// quantiles for single parameters
double qpb_(double p, double *p_distr) {
  if(isInadmissible(p))
    return NA_REAL;
  if(!validProbability(p) || isInadmissible(p_distr[0])){
    warning("NaNs produced");
    return R_NaN;
  }

  if(p == 0.0)
    return 0.0;
  if(p == 1.0 || p > p_distr[Q_LIMIT-1])
    return R_PosInf;

  for(int i = 1; i < Q_LIMIT; i++) {
    if(p > p_distr[i-1] && p < p_distr[i]) {
      return i;
    }
  }

  return R_PosInf;
}

// quantiles for vectorised parameters
double qpb_(double p, double alpha, double beta, double c) {
  if(isInadmissible(p) || isInadmissible(alpha) || isInadmissible(beta) || isInadmissible(c))
    return NA_REAL;
  if(!validProbability(p)){
    warning("NaNs produced");
    return R_NaN;
  }

  if(p == 0.0)
    return 0.0;

  double *p_distr = ppb_(alpha, beta, c);

  if(p == 1.0 || p > p_distr[Q_LIMIT-1])
    return R_PosInf;

  for(int i = 1; i < Q_LIMIT; i++) {
    if(p > p_distr[i-1] && p < p_distr[i]) {
      return i;
    }
  }

  return R_PosInf;
}

// random number generator
double rpb_(double alpha, double beta, double c, bool& throw_warning) {
  if(isInadmissible(alpha) || isInadmissible(beta) || isInadmissible(c)) {
    throw_warning = true;
    return NA_REAL;
  }

  if(!validPbParameters(alpha, beta, c)) {
    throw_warning = true;
    return R_NaN;
  }

  NumericVector poissonParameter = rbeta(1, alpha, beta) * c;
  NumericVector t = rpois(1, poissonParameter[0]);

  return t[0];
}

//' Kummer's (confluent hypergeometric) function in log-scale
//'
//' Kummer's function (also: confluent hypergeometric function of the first kind)
//' for numeric (non-complex) values and input parameters in log-scale.
//' @param x numeric value or vector
//' @param a,b numeric parameters of the Kummer function
//' @name chf_1F1
//' @rdname chf_1F1
//' @export
//' @details Note that the output is in log-scale. So the evaluated function is:
//' \deqn{\log \left[\sum_{n=0}^\infty \frac{a^{(n)} x^n}{ b^(n) n!}\right]}{log [ \sum from n to \infty (a^(n) x^n)/ (b^(n) n!)]}
//' where \eqn{a^{(n)}}{a^(n)} and \eqn{b^{(n)}}{b^(n)} describe the rising factorial.
//' @examples
//' x <- chf_1F1(-100:100, 5, 7)
//' plot(-100:100, x, type='l')
// [[Rcpp::export]]
NumericVector chf_1F1(NumericVector x, NumericVector a, NumericVector b) {
    if(min(NumericVector::create(x.length(), a.length(), b.length())) < 1) {
      return NumericVector(0);
    }
    int n = max(NumericVector::create(x.length(), a.length(), b.length()));
    NumericVector res(n);
    for(int i = 0; i < n; i++) {
      res[i] = kummer_(GETV(x, i), GETV(a, i), GETV(b, i));
    }
    return res;
}


// [[Rcpp::export]]
NumericVector cpp_dpb(NumericVector& x, NumericVector& alpha, NumericVector& beta, NumericVector& c, const bool& log_p = false) {
  if(std::min({x.length(), alpha.length(), beta.length(), c.length()}) < 1) {
    return NumericVector(0);
  }

  int n = std::max({x.length(), alpha.length(), beta.length(), c.length()});
  NumericVector p(n);
  bool throw_warning = false;

  for(int i = 0; i < n; i++) {
    p[i] = dpb_(GETV(x, i), GETV(alpha, i), GETV(beta, i), GETV(c, i), log_p, throw_warning);
  }

  if(throw_warning)
    warning("NaNs produced");

  return p;
}



//[[Rcpp::export]]
NumericVector cpp_ppb(NumericVector& q, NumericVector& alpha, NumericVector& beta, NumericVector& c, const bool& lower_tail, const bool& log_p) {
  if(std::min({ q.length(), alpha.length(), beta.length(), c.length() }) < 1) {
    return NumericVector(0);
  }

  int n = std::max({ q.length(), alpha.length(), beta.length(), c.length() });
  NumericVector p(n);

  bool throw_warning = false;

  for(int i = 0; i < n; i++) {
    p[i] = ppb_(GETV(q, i), GETV(alpha, i), GETV(beta, i), GETV(c, i), throw_warning);
  }

  if(!lower_tail)
    p = 1.0 - p;

  if(log_p)
    p = log(p);

  if(throw_warning)
    warning("NaNs produced");

  return p;
}


// [[Rcpp::export]]
NumericVector cpp_rpb(const int& n, NumericVector& alpha, NumericVector& beta, NumericVector& c) {
  if(std::min({ alpha.length(), beta.length(), c.length() }) < 1) {
    warning("NAs produced");
    return NumericVector(n, NA_REAL);
  }

  NumericVector x(n);
  bool throw_warning = false;

  for(int i = 0; i < n; i++) {
    x[i] = rpb_(GETV(alpha, i), GETV(beta, i), GETV(c, i), throw_warning);
  }

  if(throw_warning)
    warning("NAs produced");

  return x;
}


// [[Rcpp::export]]
NumericVector cpp_qpb(NumericVector& p, NumericVector& alpha, NumericVector& beta, NumericVector& c, const bool& lower_tail, const bool& log_p) {
  if(std::min({ p.length(), alpha.length(), beta.length(), c.length() }) < 1) {
    return NumericVector(0);
  }

  int n = std::max({ p.length(), alpha.length(), beta.length(), c.length()});
  NumericVector res(n);

  if(log_p)
    p = exp(p);

  if(lower_tail)
    p = 1.0 - p;

  if (min(alpha) == max(alpha) && min(beta) == max(beta) && min(c) == max(c)) {
    // single parameters
    // optmized to compute cdf only once
    if(isInadmissible(alpha[0]) || isInadmissible(beta[0]) || isInadmissible(c[0])) {
      return NumericVector(n, NA_REAL);
    } else {
      double* p_distr = ppb_(min(na_omit(alpha)), min(na_omit(beta)), min(na_omit(c)));
      for(int i = 0; i < n; i++) {
        res[i] = qpb_(GETV(p, i), p_distr);
      }
    }
  } else {
    // vectorised parameters
    for(int i = 0; i < n; i++) {
      res[i] = qpb_(GETV(p, i), GETV(alpha, i), GETV(beta, i), GETV(c, i));
    }
  }
  return res;
}
