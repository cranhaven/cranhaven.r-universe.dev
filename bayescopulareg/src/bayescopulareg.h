
#ifndef BAYESCOPULAREG_H
#define BAYESCOPULAREG_H

#include <RcppDist.h>
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;



// Functions
arma::vec linkinv_cpp( 
  arma::vec const& eta, 
  std::string const& linkname 
);

double loglik_cpp(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& distname,
    const std::string& linkname,
    const int& n
);


double logPowerPrior_cpp (
    arma::vec const& y0,
    arma::mat const& X0,
    arma::vec const& beta,
    double const& phi,
    double const& b0,
    std::string const& distname,
    std::string const& linkname,
    int const& n0
);

double logInitPrior_cpp ( 
    arma::vec const& beta,
    double const& phi,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    int const& p
);



arma::vec cdf_cpp(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& distname,
    const std::string& linkname,
    const int& n
  
);




arma::vec conv_to_normal(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& distname,
    std::string const& linkname,
    int const& n
);





List condnormal_cpp(
    arma::mat Z,
    arma::mat Gamma,
    int const& j
);


double logPost (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat const& Z,
    arma::mat const& Gammainv,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
);



List update_params (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat Gammainv,
    double const& c0,
    arma::mat const& S0beta,
    double const& sigma0logphi,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
);



arma::mat update_Z(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gammainv,
    std::string const& distname,
    const std::string& linkname,
    const int& n,
    int const& j
);

arma::mat update_Gamma (
    arma::mat const& Z,
    int const& n,
    int const& v0,
    arma::mat const& V0
);


arma::vec sample_y (
    arma::vec const& z,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& distname,
    std::string const& linkname,
    int const& n
);



arma::vec phi_rwmh (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gammainv,
    double const& sigma0logphi,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
);



List update_params_slice (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat Gammainv,
    double const& c0,
    arma::vec const& w,
    double const& sigma0logphi,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
);





#endif
