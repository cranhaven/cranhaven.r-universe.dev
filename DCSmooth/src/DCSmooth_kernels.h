#include <RcppArmadillo.h>

// Müller-Wang Kernels
arma::vec kern_fcn_MW200(arma::vec&, double);
arma::vec kern_fcn_MW210(arma::vec&, double);
arma::vec kern_fcn_MW220(arma::vec&, double);
arma::vec kern_fcn_MW320(arma::vec&, double);
arma::vec kern_fcn_MW321(arma::vec&, double);
arma::vec kern_fcn_MW420(arma::vec&, double);
arma::vec kern_fcn_MW421(arma::vec&, double);
arma::vec kern_fcn_MW422(arma::vec&, double);
arma::vec kern_fcn_MW533(arma::vec&, double);
arma::vec kern_fcn_MW644(arma::vec&, double);

arma::vec kern_fcn_M200(arma::vec&, double);
arma::vec kern_fcn_M210(arma::vec&, double);
arma::vec kern_fcn_M220(arma::vec&, double);
arma::vec kern_fcn_M321(arma::vec&, double);
arma::vec kern_fcn_M420(arma::vec&, double);
arma::vec kern_fcn_M421(arma::vec&, double);
arma::vec kern_fcn_M422(arma::vec&, double);
arma::vec kern_fcn_M533(arma::vec&, double);
arma::vec kern_fcn_M644(arma::vec&, double);

arma::vec kern_fcn_T220(arma::vec&, double);
arma::vec kern_fcn_T321(arma::vec&, double);
arma::vec kern_fcn_T420(arma::vec&, double);
arma::vec kern_fcn_T422(arma::vec&, double);
