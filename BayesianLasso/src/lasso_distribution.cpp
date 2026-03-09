
#define ARMA_DONT_USE_WRAPPER
#include <RcppArmadillo.h>
#include <RcppNumerical.h>

#include <R.h>
#include <Rmath.h>
#include <limits>
#include <cmath>


#include "zeta.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

using namespace Rcpp;
using namespace arma;
using namespace Numer;
using namespace std;

////////////////////////////////////////////////////////////////////////////////

// The inverse logit function


double expit_c(double x) {
  double val = 1/(1 + exp(-x));
  return val;
}

////////////////////////////////////////////////////////////////////////////////

/*
 * returns the square of the inverse mills of standard normal
 * target precision is 7 significant figures
 * Copyright Jonathon Tidswell 2023
 */


// Note that this function came about because of a missunderstanding between JO and JT


double sq_inv_mills_7sf(double d)
{
  if (std::isnan(d)) {
    return d;
  }

  /* rounds to zero below */
  if (d <= -27.2634433268347819989685) /* -0x1.b437105991ad6p+4 */ {
    return 0.0;
  }

  /* rounds to infinity above */
  if (d >= 1.3407807929942595611e+154) /* 0x1.FFFFFFFFFFFFFp+511 */ {
    return std::numeric_limits<double>::infinity();
  }

  double x2 = d*d;
  double r;
  if (d > 1.5) {
    double n = (0.357281 + d*(-8.95124 + d*(-2.90376 - d*0.00294599)));
    double d_new = d*(5.61022 + d * (7.8889 + d*(2.57007 + d)));
    r = x2 + 2 + n/d_new;
  }
  else if (d >= -1.5) {
    double x4 = x2 * x2;
    double x5 = x4 * d;
    double x9 = x5 * x4;
    double p3 = 0.000035771324407754916758 + d*(-3.8662919322316002634e-7 + d*(-2.4574905559814705078e-6 + d*1.9265933077879793021e-7));
    double p2 = -0.0023723764046347950774 + d*(0.0013680353093153751458 + d*(-0.00016324707618948820525 + d*-0.000076906159596354901117));
    double p1 = 1.0158982962795006307 + d*(0.57923464351427745891 + d*(0.10826597083146804511 + d*-0.012769648639760260240));
    r = 0.63661977158024931898 + d*p1 + x5*p2 + x9*p3;
  }
  else {
    double e = exp(-1.75 - x2);
    double n, d_new;
    if (d > -3.25) {
      n = 22.0568418282594038 + d*(22.5297095609090682 + d*(7.58487104794511579 + d*1.132790581231484012));
      d_new = 9.14828510368260972 + d*(-7.22734552571356488 + d*(5.345507757496535817 + d*(2.361111154527247909 + d)));
      r = 1 + n/d_new;
      r *= e;
    }
    else if (d > -5.375) {
      n = 1916.659771331656702 + d*(1963.475148469715265 + d*(687.067983478182725 + d*84.3712332217208528));
      d_new = -22058.44351946254263 + d*(-22784.02980288270901 + d*(-8006.73277242310447 + d*(-982.264108750278376 + d)));
      r = 1 + n/d_new;
      r *= e;
    }
    else if (d < -26.9) {
      /* redo to handle error differently since sub-normals require extra ... */
      constexpr double e2 = 1.713354874154990676330947e-305; /* 0x1.8102c2ed6b86ap-1013 == exp(-1.75 - 700) */
      e = exp(700 - x2);
      r = 1 - 0.08412653858383935951908352;
      r *= e;
      r *= e2;
    } else {
      r = 1 - 0.084126503799765755896;
      r *= e;
    }
  }
  return r;
}

////////////////////////////////////////////////////////////////////////////////


/*
 * returns the the mills ratio for positive x
 * target precision is 7 significant figures
 * Copyright Jonathon Tidswell 2023
 */


double right_mills_7sf(double d)
{
  constexpr double ic0 = 0.7978845608028653558798921; // sqrt (2/ pi )

  static const double p[] = { -1.12643475305799970055164642552926e-06,
                              26.81429811665765648114383134062559067694,
                              18.29336030983142542299653149128468446642,
                              5.58111735915492754757719601092611634864,
                              0.79787503529789429999743769828129023563,
                              1.914179115901329712299805428058e-08 };

  static const double q[] = { 58.87671219132194287109318373473336268934,
                              104.80902166512815500819200358588558548367,
                              80.67484673653551809206654268432287397133,
                              34.2906558057946726110203459419681364754,
                              8.24728325495053757189664039333639636203, 1.0 };

  double num = p[0] + d*(p[1] + d*(p[2] + d*(p[3] + d*(p[4] + d*p[5]))));
  double denom = q[0] + d*(q[1] + d*(q[2] + d*(q[3] + d*(q[4] + d)))); // note q[5] = 1

  double m = num / denom;
  m = m+1;
  m = m/(d+ic0);
  return m;
}

////////////////////////////////////////////////////////////////////////////////

/*
 * returns the the mills ratio for positive x
 * target precision is 12 significant figures
 * Copyright Jonathon Tidswell 2023
 */


double right_mills_12sf(double d)
{
  static const double p[] = {46697.7602201933, 69339.6909002865, 50590.6980372328,
                             23184.62760379742, 7236.31450136984, 1572.136841909630,
                             232.9967987466022, 21.74833514806325, 1.000000000000095};
  static const double q[] = {37259.42190376593, 85053.78630172011, 89598.92885811838,
                             57370.93777717682, 24713.27114352290, 7467.311205544661,
                             1593.885178714749, 233.9967987305447, 21.74833514813385,
                             1.0};

  double num = p[0] + d*(p[1] + d*(p[2] + d*(p[3] + d*(p[4] + d*(p[5] + d*(p[6] + d*(p[7] + d*p[8])))))));
  double denom = q[0] + d*(q[1] + d*(q[2] + d*(q[3] + d*(q[4] + d*(q[5] + d*(q[6] + d*(q[7] + d*(q[8] + d))))))));
  double m = num / denom;

  // warning intermediate overflow for d > 1.75e34, but asymptotic is close enough
  m = (d < 1.75e34 ? m : 1/d);
  return m;
}

////////////////////////////////////////////////////////////////////////////////

/*
 * Calculate the Mail's Ratio the regular way for x<0 and uses function 
 * right_mills_12sf(double x) for x>0
 */

// Standard normal PDF
inline double phi(double d) {
  return std::exp(-0.5 * d * d) / std::sqrt(2.0 * M_PI);
}

// Standard normal CDF using R's built-in pnorm
inline double Phi(double d) {
  return R::pnorm(d, 0.0, 1.0, 1, 0);  // lower.tail = TRUE, log.p = FALSE
}

// [[Rcpp::export]]
double MillsRatio(double d) {
  if (d > 0.0) {
    return right_mills_12sf(d);
  } else {
    double phi_val = phi(d);
    double Phi_val = 1.0 - Phi(d);  // same as upper tail
    if (phi_val == 0.0) return NA_REAL; // avoid divide by 0
    return Phi_val / phi_val;
  }
}

////////////////////////////////////////////////////////////////////////////////


int check_abc(double a, double b, double c)
{
  //Rcout << "a: " << a << "\n";
  //Rcout << "b: " << b << "\n";
  //Rcout << "c: " << c << "\n";

  int error_type = 0;

  bool a_nan = (std::isnan(a)),
    b_nan = (std::isnan(b)),
    c_nan = (std::isnan(c));

  bool cond_nan = (a_nan || b_nan || c_nan);
  if (cond_nan) {
    stop("a, b, or c is a NaN.");
    error_type = 1;
  }

  bool a_inf = (!std::isfinite(a)),
    b_inf = (!std::isfinite(b)),
    c_inf = (!std::isfinite(c));

  if ((a_inf?1:0) + (b_inf?1:0)  + (c_inf?1:0) > 1) {
    stop("a, b, or c is a infinite.");
    error_type = 2;
  }

  if ((a==0)&&(b==0)) {
    stop("Either a=0 or b=0, but not both.");
    error_type = 3;
  }

  if ((a<0)) {
    stop("a is negative.");
    error_type = 4;
  }

  if ((c<0)) {
    stop("c is negative.");
    error_type = 4;
  }

  return error_type;
}


////////////////////////////////////////////////////////////////////////////////

/**
 * Returns a list with elements
 * z - the normalizing constant of the lasso distribition with parameters a, b, and c
 * m_plus - the mills ratio (Phi(-x)/phi(x)) evaludated at x = (c - b)/sqrt(a)
 * m_minus - the mills ratio (Phi(-x)/phi(x)) evaludated at x = (c + b)/sqrt(a)
 */


double zcalc_7sf( double a, double b, double c, int type)
{
  //Rcout << "a: " << a << "\n";
  //Rcout << "b: " << b << "\n";
  //Rcout << "c: " << c << "\n";

  int error_type = check_abc( a,  b,  c);

  // calculate v1 and v2
  // handle extreme values
  double sigma2 = 1 / a;
  double sigma = sqrt(sigma2);
  double b_abs = fabs(b);
  double v1 = (c - b_abs) * sigma; // this wont overflow
  double v2 = (c + b_abs); // this might overflow
  if (!std::isinf(v2)) {
    v2 = v2 * sigma;
  }
  else {
    // this may underflow, and slower, so use as fallback
    v2 = c * sigma + b_abs * sigma;
  }

  // deal with sign of v1
  bool flipSign = v1 < 0;
  v1 = fabs(v1);

  // calculate two (right) mills ratios (prefer use SIMD)

  double m1 = 0.0;
  double m2 = 0.0;
  //if (type==1) {
  //  m1 = right_mills_7sf(v1);
  //  m2 = right_mills_7sf(v2);
  //}
  //if (type==2) {
    m1 = right_mills_12sf(v1);
    m2 = right_mills_12sf(v2);
  //}

  m1 = (flipSign ? -m1 : m1);
  double z = m1 + m2;
  if (flipSign) {
    constexpr double M_SQRT_2PI = 2.506628274631000502415765;
    double phi_inv = M_SQRT_2PI * exp(v1 * v1 / 2);
    z = z + phi_inv;
    m1 = phi_inv + m1;
  }
  z = z*sigma;

  return z;
}



Rcpp::List calculate_lasso_dist_stats_c_v1(double a, double b, double c)
{
  double mu_plus = (b - c)/a;
  double mu_minus = (b + c)/a;
  double sigma2 = 1/a;
  double sigma = sqrt(sigma2);

  double r_plus = mu_plus/sigma;
  double r_minus = mu_minus/sigma;

  double z_plus  = zeta_c(1, r_plus);
  double z_minus = zeta_c(1,-r_minus);

  double log_pm = R::pnorm(-(b + c)*sigma, 0.0, 1.0, 1, 1);
  double log_pp = R::pnorm( (b - c)*sigma, 0.0, 1.0, 1, 1);
  double w = expit_c(log_pm - log_pp + 2*b*c*sigma2);

  return List::create(_["mu_plus"] = mu_plus,
                      _["mu_minus"] = mu_minus,
                      _["sigma2"] = sigma2,
                      _["sigma"] = sigma,
                      _["z_plus"] = z_plus,
                      _["z_minus"] = z_minus,
                      _["r_plus"] = r_plus,
                      _["r_minus"] = r_minus,
                      _["w"] = w);
}


////////////////////////////////////////////////////////////////////////////////


Rcpp::List calculate_lasso_dist_stats_c_v2(double a, double b, double c)
{
  int error_type = check_abc( a, b, c);
  int type = 2;

  // Will stop if there is something funny with a, b or c
  // So we can proceed knowing there isn't anything "wrong" with a, b or c.

  // calculate v1 and v2
  // handle extreme values
  double sigma2 = 1 / a;
  double sigma = sqrt(sigma2);
  double b_abs = fabs(b);
  double v1 = (c - b_abs) * sigma; // this wont overflow
  double v2 = (c + b_abs); // this might overflow
  if (!std::isinf(v2)) {
    v2 = v2 * sigma;
  }
  else {
    // this may underflow, and slower, so use as fallback
    v2 = c * sigma + b_abs * sigma;
  }

  // deal with sign of v1
  bool flipSign = v1 < 0;
  v1 = fabs(v1);

  // calculate two (right) mills ratios (prefer use SIMD)
  double m1 = 0.0;
  double m2 = 0.0;
  if (type==1) {
    m1 = right_mills_7sf(v1);
    m2 = right_mills_7sf(v2);
  }
  if (type==2) {
    m1 = right_mills_12sf(v1);
    m2 = right_mills_12sf(v2);
  }

  m1 = (flipSign ? -m1 : m1);
  double z = m1 + m2;
  if (flipSign) {
    constexpr double M_SQRT_2PI = 2.506628274631000502415765;
    double phi_inv = M_SQRT_2PI * exp(v1 * v1 / 2);
    z = z + phi_inv;
    m1 = phi_inv + m1;
  }

  double m_plus = (b >= 0) ? m1 : m2;
  double m_minus = (b >= 0) ? m2 : m1;
  z = z*sigma;

  double mu_plus = (b - c)/a;
  double mu_minus = (b + c)/a;

  double r_plus = mu_plus/sigma;
  double r_minus = mu_minus/sigma;

  double z_plus;
  if (m_plus!=0) {
    z_plus = 1/m_plus;
  } else {
    z_plus = zeta_c(1, r_plus);
  }

  double z_minus;
  if (m_minus!=0) {
    z_minus = 1/m_minus;
  } else {
    z_minus = zeta_c(1,-r_minus);
  }

  double w;

  bool m_plus_inf = (!std::isfinite(m_plus));
  bool m_minus_inf = (!std::isfinite(m_plus));
  bool cond = (m_plus_inf || (m_plus==0) || m_minus_inf || (m_minus==0));

  if (cond) {
    double log_pm = R::pnorm(-(b + c)*sigma, 0.0, 1.0, 1, 1);
    double log_pp = R::pnorm( (b - c)*sigma, 0.0, 1.0, 1, 1);
    w = expit_c(log_pm - log_pp + 2*b*c*sigma2);
  } else {
    w = 1.0/(m_plus/m_minus + 1.0);
  }

  return List::create(_["v1"] = v1,
                      _["v2"] = v2,
                      _["m1"] = m1,
                      _["m2"] = m2,
                      _["m_plus"] = m_plus,
                      _["m_minus"] = m_minus,
                      _["z"] = z,
                      _["mu_plus"] = mu_plus,
                      _["mu_minus"] = mu_minus,
                      _["sigma2"] = sigma2,
                      _["sigma"] = sigma,
                      _["z_plus"] = z_plus,
                      _["z_minus"] = z_minus,
                      _["r_plus"] = r_plus,
                      _["r_minus"] = r_minus,
                      _["w"] = w);
}

////////////////////////////////////////////////////////////////////////////////


double logSumExp_c(arma::vec vx) {
  double M = max(vx);
  double val = M + log(sum(exp(vx - M)));
  return val;
}

////////////////////////////////////////////////////////////////////////////////

// return the normalizing constant


double zlasso_c_v1(double a, double b, double c, bool logarithm)
{
  List res = calculate_lasso_dist_stats_c_v1(a, b, c);
  double r_plus = res["r_plus"];
  double r_minus = res["r_minus"];
  double sigma = res["sigma"];

  double log_inv_z_plus  = R::pnorm5( r_plus, 0.0, 1.0, 1, 1) - R::dnorm4( r_plus, 0.0, 1.0, 1);
  double log_inv_z_minus = R::pnorm5(-r_minus,0.0, 1.0, 1, 1) - R::dnorm4(-r_minus,0.0, 1.0, 1);

  arma::vec vx(2);
  vx[0] = log_inv_z_plus;
  vx[1] = log_inv_z_minus;
  double val = log(sigma) + logSumExp_c(vx);
  if (logarithm) {
    return val;
  }
  val = exp(val);
  return val;
}

// [[Rcpp::export]]
double zlasso(double a, double b, double c, bool logarithm)
{
  int type=1;
  double z = zcalc_7sf(a, b, c, type);

  if (!std::isfinite(z)) {
    return zlasso_c_v1( a, b, c, logarithm);
  }

  if (logarithm) {
    return log(z);
  }
  return z;
}

////////////////////////////////////////////////////////////////////////////////

// Note: a>0, c>0


arma::vec dlasso_c_v1(arma::vec x, double a, double b, double c, bool logarithm)
{
  double log_Z = zlasso_c_v1(a, b, c, true);
  arma::vec val =  -0.5*a*x%x + b*x - c*abs(x) - log_Z;

  if (logarithm) {
    return val;
  }
  val = exp(val);
  return val;
}

////////////////////////////////////////////////////////////////////////////////

// Note: a>0, c>0


arma::vec dlasso_internal(arma::vec x, double a, double b, double c, bool logarithm)
{
  int type = 2;
  double z = zcalc_7sf(a, b, c, type);

  if (!std::isfinite(z)) {
    return dlasso_c_v1( x, a, b, c, logarithm);
    //stop("z is not finite");
  }
  arma::vec val =  -0.5*a*x%x + b*x - c*abs(x);
  if (logarithm) {
    return val - log(z);
  }
  val = exp(val)/z;
  return val;
}

// [[Rcpp::export]]
Rcpp::NumericVector dlasso(NumericVector x, double a, double b, double c, bool logarithm) {
  arma::vec x_arma = Rcpp::as<arma::vec>(x);
  arma::vec result = dlasso_internal(x_arma, a, b, c,logarithm);
  Rcpp::NumericVector out(result.begin(), result.end());  
  return out;
}

////////////////////////////////////////////////////////////////////////////////


arma::vec plasso_c_v1(arma::vec q, double a, double b, double c)
{
  List res = calculate_lasso_dist_stats_c_v1(a, b, c);
  double w = res["w"];
  double mu_plus = res["mu_plus"];
  double mu_minus = res["mu_minus"];
  double r_minus = res["r_minus"];
  double r_plus = res["r_plus"];
  double sigma = res["sigma"];

  int n = q.n_elem;
  arma::vec val = zeros(n);

  double con1 = R::pnorm5(-r_minus, 0.0, 1.0, 1, 1);
  double con2 = R::pnorm5(r_plus, 0.0, 1.0, 1, 1);

  for (int i = 0; i < n; ++i)
  {
    if (q[i]<=0) {
      val[i] = w*exp( R::pnorm5((q[i]-mu_minus)/sigma, 0.0, 1.0, 1, 1) - con1);
    } else {
      val[i] = w + (1.0 - w)*(1.0 - exp( R::pnorm5((mu_plus - q[i])/sigma, 0.0, 1.0, 1, 1) - con2));
    }
  }

  return val;
}


arma::vec plasso_internal(arma::vec q, double a, double b, double c)
{
  List res = calculate_lasso_dist_stats_c_v2(a, b, c);
  double w = res["w"];
  double mu_plus = res["mu_plus"];
  double mu_minus = res["mu_minus"];
  double m_plus = res["m_plus"];
  double m_minus = res["m_minus"];
  double r_minus = res["r_minus"];
  double r_plus = res["r_plus"];
  double sigma = res["sigma"];

  int n = q.n_elem;
  arma::vec val = zeros(n);

  double con1 = R::pnorm5(-r_minus, 0.0, 1.0, 1, 1);
  double con2 = R::pnorm5(r_plus, 0.0, 1.0, 1, 1);

  //if (!arma::is_finite(x)) {
  //Rcout << "w:" << w << " \n";
  //Rcout << "mu_plus:" << mu_plus << " \n";
  //Rcout << "mu_minus:" << mu_minus << " \n";
  //Rcout << "m_plus:" << m_plus << " \n";
  //Rcout << "m_minus:" << m_minus << " \n";
  //Rcout << "r_minus:" << r_minus << " \n";
  //Rcout << "r_plus:" << r_plus << " \n";
  //  stop("qlasso_fast_c_v1 - returned value is not finite");
  //}

  for (int i = 0; i < n; ++i)
  {
    if (q[i]<=0) {
      val[i] = w*exp( R::pnorm5((q[i]-mu_minus)/sigma, 0.0, 1.0, 1, 1) - con1);
    } else {
      val[i] = w + (1.0 - w)*(1.0 - exp( R::pnorm5((mu_plus - q[i])/sigma, 0.0, 1.0, 1, 1) - con2));
    }
  }

  return val;
}


// [[Rcpp::export]]
Rcpp::NumericVector plasso(NumericVector q, double a, double b, double c) {
  arma::vec q_arma = Rcpp::as<arma::vec>(q);
  arma::vec result = plasso_internal(q_arma, a, b, c);
  Rcpp::NumericVector out(result.begin(), result.end());  
  return out;
}

////////////////////////////////////////////////////////////////////////////////


arma::vec qlasso_fast_c_v1(arma::vec p, double a, double b, double c)
{
  double sigma2 = 1/a;
  double sigma = sqrt(sigma2);
  double log_pm = R::pnorm5(-(b + c)*sigma, 0.0, 1.0, 1, 1);
  double log_pp = R::pnorm5( (b - c)*sigma, 0.0, 1.0, 1, 1);
  double w = expit_c(log_pm - log_pp + 2*b*c*sigma2);
  double length = p.n_elem;
  arma::vec x(length);
  double p_new;
  double xi;
  for(int i = 0; i < length; ++i){
    if (p[i]<=w) {
      xi = exp(log_pm);
      p_new = xi*p[i]/w;
      x[i] =  (b + c)*sigma2 + sigma*R::qnorm5( p_new, 0.0, 1.0, 1, 0);
    } else {
      xi = exp(log_pp);
      p_new = xi*(1 - p[i])/(1-w);
      x[i] =  (b - c)*sigma2 - sigma*R::qnorm5( p_new, 0.0, 1.0, 1, 0);
    }
  }
  //if (!arma::is_finite(x)) {
  //  Rcout << "p:" << p << " \n";
  //  Rcout << "w:" << w << " \n";
  //  Rcout << "p_new:" << p_new << " \n";
  //  Rcout << "a:" << a << " \n";
  //  Rcout << "b:" << b << " \n";
  //  Rcout << "c:" << c << " \n";
  //  stop("qlasso_fast_c_v1 - returned value is not finite");
  //}
  return x;
}

////////////////////////////////////////////////////////////////////////////////


arma::vec qlasso_internal(arma::vec p, double a, double b, double c)
{
  //////////////////////////////////////////////////////////////////////////////

  // calculate v1 and v2
  // handle extreme values
  double sigma2 = 1 / a;
  double sigma = sqrt(sigma2);
  double b_abs = fabs(b);
  double v1 = (c - b_abs) * sigma; // this wont overflow
  double v2 = (c + b_abs); // this might overflow
  if (!std::isinf(v2)) {
    v2 = v2 * sigma;
  }
  else {
    // this may underflow, and slower, so use as fallback
    v2 = c * sigma + b_abs * sigma;
  }

  // deal with sign of v1
  bool flipSign = v1 < 0;
  v1 = fabs(v1);

  // calculate two (right) mills ratios (prefer use SIMD)
  double m1 = 0.0;
  double m2 = 0.0;
  //if (type==1) {
  //  m1 = right_mills_7sf(v1);
  //  m2 = right_mills_7sf(v2);
  //}
  //if (type==2) {
  m1 = right_mills_12sf(v1);
  m2 = right_mills_12sf(v2);
  //}

  constexpr double M_SQRT_2PI = 2.506628274631000502415765;
  double phi_inv;
  double m3 = (flipSign ? -m1 : m1);
  if (flipSign) {
    phi_inv = M_SQRT_2PI * exp(v1 * v1 / 2);
    m1 = phi_inv + m3;
  }

  double m_plus = (b >= 0) ? m1 : m2;
  double m_minus = (b >= 0) ? m2 : m1;

  //////////////////////////////////////////////////////////////////////////////

  double w = 1.0/(m_plus/m_minus + 1.0);
  double length = p.n_elem;
  arma::vec x(length);
  double p_new;
  double log_p;
  for(int i = 0; i < length; ++i){
    if (p[i]<=w) {
      double v3 = (c + b)*sigma;

      double phi_minus = exp(-v3 * v3 / 2)/M_SQRT_2PI;

      if (flipSign & (b < 0)) {
        // Save us from overflow (I hope)
        p_new = (exp(v1*v1/2 - v3*v3/2) + m3*phi_minus)*p[i]/w;
      } else {
        p_new = m_minus*p[i]*phi_minus/w;
      }

      if (p_new>0) {
        x[i] =  (b + c)*sigma2 + sigma*R::qnorm5( p_new, 0.0, 1.0, 1, 0);
      } else {
        // Save us from underflow
        log_p = R::pnorm5(-(b + c)*sigma, 0.0, 1.0, 1, 1) + log(p[i]) - log(w);
        x[i] =  (b + c)*sigma2 + sigma*R::qnorm5( log_p, 0.0, 1.0, 1, 1);
      }

    } else {

      double v4 = (c - b)*sigma;
      double phi_plus = exp(-v4 * v4 / 2)/M_SQRT_2PI;



      if (flipSign & (b > 0)) {
        // Save us from overflow (I hope)
        p_new = (exp(v1*v1/2 - v4*v4/2) + m3*phi_plus)*(1 - p[i])/(1 - w);
      } else {
        p_new = m_plus*(1-p[i])*phi_plus/(1 - w);
      }


      if (p_new>0) {
        x[i] =  (b - c)*sigma2 - sigma*R::qnorm5( p_new, 0.0, 1.0, 1, 0);
      } else {
        // Save us from underflow
        log_p = R::pnorm5( (b - c)*sigma, 0.0, 1.0, 1, 1) + log(1-p[i]) - log(1-w);
        x[i] =  (b - c)*sigma2 - sigma*R::qnorm5( log_p, 0.0, 1.0, 1, 1);
      }
    }

    if (!std::isfinite(x[i])) {
      //  Rcout << "w:" << w << " \n";
      //  Rcout << "p:" << p[i] << " \n";
      //  Rcout << "p_new:" << p_new << " \n";
      //  Rcout << "w:" << w << " \n";
      //  Rcout << "p:" << p[i] << " \n";
      //  Rcout << "p_new:" << p_new << " \n";

      //  Rcout << "a:" << a << " \n";
      //  Rcout << "b:" << b << " \n";
      //  Rcout << "c:" << c << " \n";
      stop("The returned value is not finite");
    }
  }


  return x;
}


// [[Rcpp::export]]
Rcpp::NumericVector qlasso(NumericVector p, double a, double b, double c) {
  arma::vec p_arma = Rcpp::as<arma::vec>(p);
  arma::vec result = qlasso_internal(p_arma, a, b, c);
  Rcpp::NumericVector out(result.begin(), result.end());  
  return out;
}

////////////////////////////////////////////////////////////////////////////////


arma::vec rlasso_fast_c_v1(double n, double a, double b, double c) {

  arma::vec x(n);
  arma::vec p(n);  // Create a n-element vector
  for(int i =0; i < n; ++i){
    p[i] = R::runif( 0.0, 1.0);
  }
  x = qlasso_fast_c_v1(p, a, b, c);
  return x;
}

////////////////////////////////////////////////////////////////////////////////


arma::vec rlasso_internal(double n, double a, double b, double c) {

  arma::vec x(n);
  arma::vec p(n);  // Create a n-element vector
  for(int i =0; i < n; ++i){
    p[i] = R::runif(0.0, 1.0);
  }
  x = qlasso_internal(p, a, b, c);
  return x;
}


// [[Rcpp::export]]
Rcpp::NumericVector rlasso(double n, double a, double b, double c) {
  arma::vec result = rlasso_internal(n, a, b, c);
  Rcpp::NumericVector out(result.begin(), result.end());  
  return out;
}

////////////////////////////////////////////////////////////////////////////////

// return the expected value


double elasso_c_v1(double a, double b, double c)
{
  List res = calculate_lasso_dist_stats_c_v1(a, b, c);
  double r_plus = res["r_plus"];
  double r_minus = res["r_minus"];
  double mu_plus = res["mu_plus"];
  double mu_minus = res["mu_minus"];
  double sigma = res["sigma"];
  double w = res["w"];

  double z_plus = zeta_c(1, r_plus);
  double z_minus = zeta_c(1,-r_minus);
  double e_plus  =  mu_plus  + sigma*z_plus;
  double e_minus =  mu_minus - sigma*z_minus;
  double val = w*e_minus + (1.0-w)*e_plus;
  return val;
}

////////////////////////////////////////////////////////////////////////////////

// return the expected value

// [[Rcpp::export]]
double elasso(double a, double b, double c)
{
  List res = calculate_lasso_dist_stats_c_v2(a, b, c);

  double mu_plus = res["mu_plus"];
  double mu_minus = res["mu_minus"];
  double m_plus = res["m_plus"];
  double m_minus = res["m_minus"];
  double sigma = res["sigma"];
  double w = res["w"];

  double z_plus = 1/m_plus;
  double z_minus = 1/m_minus;
  double e_plus  =  mu_plus  + sigma*z_plus;
  double e_minus =  mu_minus - sigma*z_minus;
  double val = w*e_minus + (1.0-w)*e_plus;
  return val;
}

////////////////////////////////////////////////////////////////////////////////

// return the variance


double vlasso_c_v1(double a, double b, double c)
{
  List res = calculate_lasso_dist_stats_c_v1(a, b, c);
  double r_plus = res["r_plus"];
  double r_minus = res["r_minus"];
  double mu_plus = res["mu_plus"];
  double mu_minus = res["mu_minus"];
  double sigma = res["sigma"];
  double sigma2 = res["sigma2"];
  double w = res["w"];

  double z_plus = zeta_c(1, r_plus);
  double z_minus = zeta_c(1,-r_minus);
  double e_plus = mu_plus  + sigma*z_plus;
  double e_minus = mu_minus - sigma*z_minus;

  double v_plus  = sigma2*(1 + zeta_c(2, r_plus));
  double v_minus = sigma2*(1 + zeta_c(2,-r_minus));

  double val = w*(v_minus + e_minus*e_minus)  + (1.0 - w)*(v_plus + e_plus*e_plus);
  val = val - pow( w*e_minus + (1-w)*e_plus, 2.0);

  return val;
}

// [[Rcpp::export]]
double vlasso(double a, double b, double c)
{
  List res = calculate_lasso_dist_stats_c_v2(a, b, c);
  double r_plus = res["r_plus"];
  double r_minus = res["r_minus"];
  double m_plus = res["m_plus"];
  double m_minus = res["m_minus"];
  double mu_plus = res["mu_plus"];
  double mu_minus = res["mu_minus"];

  double sigma = res["sigma"];
  double sigma2 = res["sigma2"];
  double w = res["w"];

  double z1_plus = 1/m_plus;
  double z1_minus = 1/m_minus;


  double e_plus = mu_plus  + sigma*z1_plus;
  double e_minus = mu_minus - sigma*z1_minus;

  double z2_plus = -r_plus*z1_plus - z1_plus*z1_plus;
  double z2_minus = r_minus*z1_minus - z1_minus*z1_minus;



  double v_plus  = sigma2*(1 + z2_plus);
  double v_minus = sigma2*(1 + z2_minus);

  double val = w*(v_minus + e_minus*e_minus)  + (1.0 - w)*(v_plus + e_plus*e_plus);
  val = val - pow( w*e_minus + (1-w)*e_plus, 2.0);

  return val;
}

////////////////////////////////////////////////////////////////////////////////

// Return the mode of the lasso distribution


arma::vec mlasso_internal(arma::vec a, arma::vec b, arma::vec c)
{
  int n = a.n_elem;
  arma::vec vmode = zeros(n);
  arma::vec vx(2);
  for (int i = 0; i < n; ++i)
  {
    vx[0] = abs(b[i]) - c[i];
    vx[1] = 0.0;
    vmode[i] = max(vx)*sign(b[i])/a[i];
  }
  return vmode;
}

// [[Rcpp::export]]
Rcpp::NumericVector mlasso(NumericVector a, NumericVector b, NumericVector c) {
  arma::vec a_arma = Rcpp::as<arma::vec>(a);
  arma::vec b_arma = Rcpp::as<arma::vec>(b);
  arma::vec c_arma = Rcpp::as<arma::vec>(c);
  arma::vec result = mlasso_internal(a_arma, b_arma, c_arma);
  Rcpp::NumericVector out(result.begin(), result.end());  
  return out;
}


////////////////////////////////////////////////////////////////////////////////



