// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "DCSmooth_types.h"
#include "DCSmooth_kernels.h"

// using namespace Rcpp;

//-------------------------Use Kernel Functions-------------------------------//

// [[Rcpp::export]]
Rcpp::XPtr<funcPtr> kernel_fcn_assign(std::string fstr) {
  if (fstr == "MW_200")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW200)));
  else if (fstr == "MW_210")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW210)));
  else if (fstr == "MW_220")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW220)));
  else if (fstr == "MW_321")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW320)));
  else if (fstr == "MW_420")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW420)));
  else if (fstr == "MW_421")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW421)));
  else if (fstr == "MW_422")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW422)));
  else if (fstr == "MW_533")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW533)));
  else if (fstr == "MW_644")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_MW644)));
  
  else if (fstr == "M_200")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M200)));
  else if (fstr == "M_210")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M210)));
  else if (fstr == "M_220")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M220)));
  else if (fstr == "M_321")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M321)));
  else if (fstr == "M_420")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M420)));
  else if (fstr == "M_421")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M421)));
  else if (fstr == "M_422")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M422)));
  else if (fstr == "M_533")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M533)));
  else if (fstr == "M_644")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_M644)));
  
  else if (fstr == "T_220")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_T220)));
  else if (fstr == "T_321")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_T321)));
  else if (fstr == "T_420")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_T420)));
  else if (fstr == "T_422")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&kern_fcn_T422)));
  else
    return Rcpp::XPtr<funcPtr>(R_NilValue); // runtime error as NULL no XPtr
}

// [[Rcpp::export]]
arma::vec kernel_fcn_use(arma::vec x, double q, SEXP xpsexp)
{
  Rcpp::XPtr<funcPtr> xpfun(xpsexp);
  funcPtr fun = *xpfun;
  arma::vec y = fun(x, q);
  return y; 
}

//-----------------------Weight Functions for LPR-----------------------------//

// Truncated
arma::vec weights_T(arma::vec& u, double q = 1, int mu = 2)
{
  arma::vec weights_out{ pow(1 + u, mu) % pow(1 - u, mu) };
  return weights_out;
}

// Müller
arma::vec weights_M(arma::vec& u, double q = 1, int mu = 2)
{
  arma::vec weights_out{ pow(1 + u, mu) % pow(q - u, mu) };
  return weights_out;
}

// Müller-Wang
arma::vec weights_MW(arma::vec& u, double q = 1, int mu = 2)
{
  arma::vec weights_out{ pow(1 + u, mu) % pow(q - u, std::max(mu - 1, 0)) };
  return weights_out;
}

// [[Rcpp::export]]
Rcpp::XPtr<weightPtr> weight_fcn_assign(std::string fstr) {
  if (fstr == "T")
    return(Rcpp::XPtr<weightPtr>(new weightPtr(&weights_T)));
  else if (fstr == "M")
    return(Rcpp::XPtr<weightPtr>(new weightPtr(&weights_M)));
  else if (fstr == "MW")
    return(Rcpp::XPtr<weightPtr>(new weightPtr(&weights_MW)));
  else
    return Rcpp::XPtr<weightPtr>(R_NilValue); // runtime error as NULL no XPtr
}
