// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// Müller-Wang kernels
// Form is: Type_k_mu_nu

// [[Rcpp::export]]
arma::vec kern_fcn_MW200(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q_denom{ pow(1 + q, 3) };
  double out{ };
  double u{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    
    out = 2 / q_denom * (
          (- 2*q + 2*q2 + 2) +
          (3 - 3*q) * u);
      
      uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW210(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q_denom{ pow(1 + q, 4) };
  double out{ };
  double u{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    
    out = 6 * (u + 1) / q_denom * (
          (- 2*q + 3*q2 + 1) +
          (2 - 4*q) * u);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW220(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q_denom{ pow(1 + q, 6) };
  double out{ };
  double u{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    
    out = 60 * (1 + u) * (1 + u) * (q - u) / q_denom * (
          (2*q2 - 2*q + 1) +
          (- 3*q + 2) * u);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW320(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q_denom{ pow(1 + q, 8) };
  double out{ };
  double u{ };
  double u2{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    
    out = 60 * (1 + u) * (1 + u) * (q - u) / q_denom * (
          (10*q4 - 30*q3 + 39*q2 - 16*q + 3) + 
          (- 35*q3 + 84*q2 - 63*q + 14) * u +
          (28*q2 - 56*q + 14) * u2);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW321(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q_denom{ pow(1 + q, 8) };
  double out{ };
  double u{ };
  double u2{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    
    out = - 420 * (1 + u) * (1 + u) * (q - u) / q_denom * (
          (- 5*q3 + 12*q2 - 9*q + 2) +
          (19*q2 - 26*q + 11) * u + 
          (- 16*q + 12) * u2);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW420(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q6{ q5 * q };
  double q_denom{ pow(1 + q, 10) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    u3  = u2 * u;
    
    out = 420 * (1 + u) * (1 + u) * (q - u) / q_denom * (
          (5*q6 - 30*q5 + 77*q4 - 84*q3 + 45*q2 - 10*q + 1) +
          (- 30*q5 + 152*q4 - 280*q3 + 216*q2 - 70*q + 8) * u +
          (54*q4 - 240*q3 + 300*q2 - 144*q + 18) * u2 +
          (- 30*q3 + 120*q2 - 90*q + 12) * u3);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW421(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q_denom{ pow(1 + q, 10) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    u3  = u2 * u;
    
    out = -840 * (1 + u) * (1 + u) * (q - u) / q_denom * (
          (- 15*q5 + 76*q4 - 140*q3 + 108*q2 - 35*q + 4) +
          (97*q4 - 344*q3 + 444*q2 - 212*q + 37) * u +
          (- 183*q3 + 480*q2 - 381*q + 90) * u2 + 
          (105*q2 - 210*q + 63) * u3);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW422(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q_denom{ pow(1 + q, 10) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    u3  = u2 * u;
    
    out = 5040 * (1 + u) * (1 + u) * (q - u) / q_denom * (
          (9*q4 - 40*q3 + 50*q2 - 24*q + 3) +
          (- 61*q3 + 160*q2 - 127*q + 30) * u +
          (119*q2 - 182*q + 77) * u2 +
          (- 70*q + 56) * u3);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW533(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q_denom{ pow(1 + q, 14) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  double u4{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    u3  = u2 * u;
    u4 = u3 * u;
    
    out = -720720 * (q - u) * (q - u) * (1 + u) * (1 + u) * (1 + u) / q_denom *(
      (- 16*q5 + 105*q4 - 210*q3 +  175*q2 -   60*q +   6) +
                (157*q4 - 672*q3 +  903*q2 -  478*q +  78) * u +
                       (- 510*q3 + 1407*q2 - 1200*q + 315) * u2 +
                                  ( 671*q2 - 1122*q + 495) * u3 +
                                          (-  308*q + 264) * u4);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_MW644(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q6{ q5 * q };
  double q_denom{ pow(1 + q, 18) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  double u4{ };
  double u5{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u  = uVec(i);
    u2 = u * u;
    u3 = u2 * u;
    u4 = u3 * u;
    u5 = u4 * u;
    
    out = 147026880 * pow(q - u, 3) * pow(1 + u, 4) / q_denom * (
      (25*q6 - 216*q5 +  612*q4 -  768*q3 +  459*q2 -  120*q +   10) + 
            (- 321*q5 + 1944*q4 - 3912*q3 + 3420*q2 - 1293*q +  160) * u +
                       (1482*q4 - 6240*q3 + 8658*q2 - 4836*q +  884) * u2 +
                               (- 3146*q3 + 8892*q2 - 7878*q + 2184) * u3 +
                                           (3120*q2 - 5460*q + 2470) * u4 +
                                                   (- 1170*q + 1040) * u5);
 
    uOut(i) = out;
  }
  return uOut;
}
