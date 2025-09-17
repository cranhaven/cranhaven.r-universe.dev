// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// Truncated kernels
// Form is: Type_k_mu_nu

// [[Rcpp::export]]
arma::vec kern_fcn_T220(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q_denom{ pow(1 + q, 5) };
  double out{ };
  double u{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    
    out = 30 * (1 + u) * (1 + u) * (1 - u) * (1 - u) / q_denom * (
          (30*q4 - 90*q3 + 96*q2 - 48*q + 16) +
          (- 35*q3 + 105*q2 - 105*q + 35) * u ) /
          (5*q4 - 40*q3 + 126*q2 - 168*q + 81);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_T321(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q6{ q5 * q };
  double q7{ q6 * q };
  double q_denom{ pow(1 + q, 7) };
  double out{ };
  double u{ };
  double u2{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    
    out = - 840 * (1 + u) * (1 + u) * (1 - u) * (1 - u) / q_denom * (
          (- 175*q7  + 1400*q6 - 4830*q5 + 9240*q4 - 10395*q3 + 6720*q2 -
                          2240*q + 280) +
          (480*q6 - 3840*q5 + 12800*q4 - 21760*q3 + 19968*q2
                          - 9664*q + 2048) * u +
          (- 315*q5 + 2520*q4 - 8190*q3 + 12600*q2 - 9135*q + 2520) * u2) /
          (25*q6 - 375*q5 + 2370*q4 - 7550*q3 + 12765*q2 - 10875*q + 3672);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_T420(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q6{ q5 * q };
  double q7{ q6 * q };
  double q8{ q7 * q };
  double q9{ q8 * q };
  double q10{ q9 * q };
  double q11{ q10 * q };
  double q12{ q11 * q };
  double q_denom{ pow(1 + q, 9) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    u3  = u2 * u;
    
    out = 840 * (1 + u) * (1 + u) * (1 - u) * (1 - u) / q_denom * (
          (1960*q12 - 29400*q11 + 203952*q10 - 864080*q9 +  + 2477160*q8 -
                      4982040*q7 + 7056480*q6 - 6950304*q5 + 4644000*q4 -
                      2012000*q3 + 514560*q2 - 64320*q + 4288) +
          (- 8820*q11 + 132300*q10 - 905310*q9 + 3701250*q8 - 9930060*q7 +
                      18132660*q6 - 22702176*q5 + 19202400*q4 - 10553760*q3 +
                      3497760*q2 - 606690*q + 40446) * u +
          (12600*q10 - 189000*q9 + 1280160*q8 - 5090400*q7 + 12910200*q6 -
                      21486600*q5 + 23479920*q4 - 16440720*q3 + 6923520*q2 -
                      1500480*q + 100032) * u2 +
          (- 5775*q9 + 86625*q8 - 582120*q7 + 2263800*q6 - 5470542*q5 + 
                      8322930*q4 - 7780080*q3 + 4158000*q2 - 1063755*q +
                      70917) * u3) /
          (105*q8 - 2520*q7 + 26236*q6 - 146664*q5 + 479670*q4 - 
                      941800*q3 + 1089660*q2 - 682968*q + 178537);
    
    uOut(i) = out;
  }
  return uOut;
}

// [[Rcpp::export]]
arma::vec kern_fcn_T422(arma::vec& uVec, double q = 1)
{
  arma::uword n_size{ uVec.size() };
  arma::vec uOut(n_size);
  
  double q2{ q * q };
  double q3{ q2 * q };
  double q4{ q3 * q };
  double q5{ q4 * q };
  double q6{ q5 * q };
  double q7{ q6 * q };
  double q8{ q7 * q };
  double q9{ q8 * q };
  double q10{ q9 * q };
  double q_denom{ pow(1 + q, 9) };
  double out{ };
  double u{ };
  double u2{ };
  double u3{ };
  
  for (int i{ 0 }; i < n_size; ++i)
  {
    u   = uVec(i);
    u2  = u * u;
    u3  = u2 * u;
    
    out = 5040 * (1 + u) * (1 + u) * (1 - u) * (1 - u) / q_denom * (
          (4200*q10 - 63000*q9 + 426720*q8 - 1696800*q7 + 4303400*q6 -
                        7162200*q5 + 7826640*q4 - 5480240*q3 + 2307840*q2 -
                        500160*q + 33344) +
          (- 19845*q9 + 297675*q8 - 1968120*q7 + 7295400*q6 - 16618266*q5 +
                        24117030*q4 - 22251600*q3 + 12514320*q2 - 3848985*q +
                        482391) * u +
          (29400*q8 - 441000*q7 + 2865520*q6 - 10054800*q5 + 20784120*q4 -
                        26072200*q3 + 19568640*q2 - 8137920*q +
                        1460032) * u2 +
          (- 13860*q7 + 207900*q6 - 1333332*q5 + 4476780*q4 - 8468460*q3 +
                        9050580*q2 - 5086620*q + 1167012) * u3) /
          (105*q8 - 2520*q7 + 26236*q6 - 146664*q5 + 479670*q4 -
                        941800*q3 + 1089660*q2 - 682968*q + 178537);
    
    uOut(i) = out;
  }
  return uOut;
}
