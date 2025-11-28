#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

double Sign(double dX) {
  double dOut = 1.0;
  if (dX < 0) {
    dOut = -1.0;
  }
  return dOut;
}

double abs3(double dX) {
  double dOut = dX;
  if (dX < 0) {
    dOut = -dX;
  }
  return dOut;
}

arma::cube array2cube(SEXP myArray)
{
  Rcpp::NumericVector vecArray(myArray);
  Rcpp::IntegerVector arrayDims = vecArray.attr("dim");
  arma::cube cubeArray(vecArray.begin(), arrayDims[0], arrayDims[1], arrayDims[2], false);
  return(cubeArray);
}

//[[Rcpp::export]]
arma::mat Get_Gamma_tilde(arma::mat mGamma, arma::vec vScaling, int iJ, int iTau_star) {

  // arma::mat mA_i = zeros(iJ, iJ);
  arma::mat mC = zeros(iJ, iJ);

  int j;
  int k;
  for (j = 0; j < iJ; j++) {

    // Rf_PrintValue(wrap(j));

    // mA_i(j,j) = 1.0/vScaling(j);
    if (j == iTau_star) {
      mC.row(j).zeros();
    }

    if (j < iTau_star) {
      for (k = 0; k <= j; k++) {
        mC(j, k) = -1.0/vScaling(j);
      }
    }
    if (j > iTau_star) {
      for (k = j; k < iJ; k++) {
        mC(j, k) = 1.0/vScaling(j);
      }
    }

  }

  arma::mat mGamma_tilde = mGamma * mC;

  return mC;
  // return mGamma_tilde;
}

double mymin(double dA, double dB) {

  double dOut = dA;
  if (dB < dA) {
    dOut = dB;
  }

  return dOut;
}

double mymax(double dA, double dB) {

  double dOut = dA;
  if (dB > dA) {
    dOut = dB;
  }

  return dOut;
}

//[[Rcpp::export]]
arma::mat ScoreVariance_C(arma::vec vTau, arma::vec vBeta, arma::mat mIndex) {

  int iJ = vTau.size();
  int iK = vBeta.size();

  arma::mat mVar(iJ*iK, iJ*iK, fill::zeros);

  int i;
  int j;

  arma::rowvec vIndex_i(2);
  arma::rowvec vIndex_j(2);

  for (i = 0; i < iJ*iK; i++) {
    for (j = 0; j < iJ*iK; j++) {
      vIndex_i = mIndex.row(i);
      vIndex_j = mIndex.row(j);
      if (vIndex_i(1) == vIndex_j(1)) {
        mVar(i, j) = vTau(vIndex_i(1)) * mymin(vBeta(vIndex_i(0)), vBeta(vIndex_j(0))) * (1.0 - mymax(vBeta(vIndex_i(0)), vBeta(vIndex_j(0))));
      } else {
        mVar(i, j) = mymin(vTau(vIndex_i(1)), vTau(vIndex_j(1))) * (mymin(vBeta(vIndex_i(0)), vBeta(vIndex_j(0))) - vBeta(vIndex_i(0)) * vBeta(vIndex_j(0)));
      }
    }
  }

  return mVar;

}
