#ifndef DCSMOOTH_H
#define DCSMOOTH_H

arma::mat weightMatrix(arma::colvec weights, arma::mat matrix);
arma::mat xMatrix(arma::colvec xVector, int polyOrder);
arma::mat np_matrix(SEXP kernFcnPtr, int p, int n);
arma::vec m_weights(arma::mat npMatrix, arma::vec u, int drv);
int factorialFunction(int value);

arma::vec weights_T(arma::vec&, double, int);
arma::vec weights_M(arma::vec&, double, int);
arma::vec weights_MW(arma::vec&, double, int);

arma::vec kernFkt_MW200(arma::vec&, double);
arma::vec kernFkt_MW210(arma::vec&, double);
arma::vec kernFkt_MW220(arma::vec&, double);
arma::vec kernFkt_MW320(arma::vec&, double);
arma::vec kernFkt_MW420(arma::vec&, double);
arma::vec kernFkt_MW422(arma::vec&, double);


#endif
