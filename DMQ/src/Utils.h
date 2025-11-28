#ifndef UTILS_H
#define UTILS_H
double Sign(double dX);
double abs3(double dX);
arma::cube array2cube(SEXP myArray);
arma::mat Get_Gamma_tilde(arma::mat mGamma, arma::vec vScaling, int iJ, int iTau_star) ;
#endif
