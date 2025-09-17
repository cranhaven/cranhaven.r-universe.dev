// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppThread)]]
#include <RcppThread.h>

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "coeffMatrix.h"

using namespace arma;

// [[Rcpp::export]]
arma::mat wedgeExport( const arma::mat& A,  const arma::mat& B){

 SpMat<double> a(A);
 SpMat<double> b(B);
 return(arma::mat(wedge(a,b)));

}

// [[Rcpp::export]]
arma::cx_rowvec wedgeExportConv(arma::cx_rowvec A, arma::cx_rowvec B, arma::cx_double y){

  cx_rowvec res = wedgeConv(A,B,y);

  return(res);
}

// [[Rcpp::export]]
Rcpp::List alignCoeffs(Rcpp::List &coeffs,std::string type){
  unsigned int numCoeffs = coeffs.length();

  uword maxSizeR = 0;
  uword maxSizeC = 0;

  if(type == "default"){

    /*
     *  find max row/col present, resize smaller matrices and then
     *  align so that max x exponent is aligned with larger matrix
     */

    Mat<double> currCoeff = Rcpp::as<Mat<double>>(coeffs[0]);
    vec coeffRowSizes(numCoeffs);
    vec coeffColSizes(numCoeffs);

    for(unsigned int i = 0; i < numCoeffs; i++){
      Mat<double> currCoeff = Rcpp::as<Mat<double>>(coeffs[i]);
      coeffRowSizes[i] = currCoeff.n_rows;
      coeffColSizes[i] = currCoeff.n_cols;
    }

    maxSizeR = coeffRowSizes.max();
    maxSizeC = coeffColSizes.max();


    for(unsigned int i = 0; i < numCoeffs; i++){
      Mat<double> currCoeff = Rcpp::as<Mat<double>>(coeffs[i]);
      if(currCoeff.n_rows != maxSizeR || currCoeff.n_cols != maxSizeC){
        int coeffs_cols = currCoeff.n_cols;
        currCoeff.resize( maxSizeR, maxSizeC);
        currCoeff = shift(currCoeff,maxSizeC - coeffs_cols, 1);
        coeffs[i] = currCoeff;
      }
    }

  } else if(type == "yEvaluated"){

    /*
     *  only consider max col for vector with resize and
     *  shift for max x exponent term
     */

    cx_rowvec currCoeff = Rcpp::as<cx_rowvec>(coeffs[0]);
    vec coeffColSizes(numCoeffs);

    for(unsigned int i = 0; i < numCoeffs; i++){
      cx_rowvec currCoeff = Rcpp::as<cx_rowvec>(coeffs[i]);
      coeffColSizes[i] = currCoeff.n_elem;
    }

    maxSizeC = coeffColSizes.max();

      for(unsigned int i = 0; i < numCoeffs; i++){
        cx_rowvec currCoeff = Rcpp::as<cx_rowvec>(coeffs[i]);
        if(currCoeff.n_elem != maxSizeC){
          int coeffs_cols = currCoeff.n_elem;
          currCoeff.resize(maxSizeC);
          coeffs[i] = shift(currCoeff, maxSizeC - coeffs_cols);
        }

    }

  } else if(type == "tipLabel"){

    cx_fmat currCoeff = Rcpp::as<cx_fmat>(coeffs[0]);
    vec coeffRowSizes(numCoeffs);
    vec coeffColSizes(numCoeffs);

    for(unsigned int i = 0; i < numCoeffs; i++){
      cx_fmat currCoeff = Rcpp::as<cx_fmat>(coeffs[i]);
      coeffRowSizes[i] = currCoeff.n_rows;
      coeffColSizes[i] = currCoeff.n_cols;
    }

    maxSizeR = coeffRowSizes.max();
    maxSizeC = coeffColSizes.max();

      for(unsigned int i = 0; i < numCoeffs; i++){
        cx_fmat currCoeff = Rcpp::as<cx_fmat>(coeffs[i]);
        if(currCoeff.n_rows != maxSizeR || currCoeff.n_cols != maxSizeC){
          currCoeff.resize( maxSizeR, maxSizeC);
          coeffs[i] = currCoeff;
        }

    }
  }

  return(coeffs);
}

// [[Rcpp::export]]
Rcpp::List coeffMatList(std::vector<std::vector<std::string>> wedgeOrders,std::string type, arma::cx_double y, std::string tipLabA = " ", std::string tipLabB = " ", int nThreads = -1){
  int numCoeffs = wedgeOrders.size();

  size_t numThreads = std::thread::hardware_concurrency();
  if(nThreads != -1){
    numThreads = nThreads;
  }

  Rcpp::List output(numCoeffs);

    if(type == "default"){
      arma::field<arma::mat> coeffs(numCoeffs);

      RcppThread::parallelFor(0, numCoeffs, [&coeffs, &wedgeOrders] (unsigned int i) {
        coeffs[i] = coeffMatrixReal(wedgeOrders[i]);
      },numThreads,0);

      output = Rcpp::wrap(coeffs);
    } else if(type == "yEvaluated"){
      arma::field<arma::cx_rowvec> coeffs(numCoeffs);

      RcppThread::parallelFor(0, numCoeffs, [&coeffs, &wedgeOrders, &y] (unsigned int i) {
        coeffs[i] = coeffMatrixComplex(wedgeOrders[i],y);
      },numThreads,0);

      output = Rcpp::wrap(coeffs);
    } else if(type == "tipLabel"){
      arma::field<arma::cx_mat> coeffs(numCoeffs);

      RcppThread::parallelFor(0, numCoeffs, [&coeffs, &wedgeOrders, &tipLabA, &tipLabB] (unsigned int i) {
        coeffs[i] = coeffMatrixTipLabel(wedgeOrders[i], tipLabA, tipLabB);
      },numThreads,0);

      output = Rcpp::wrap(coeffs);
    }


  return(output);
}



