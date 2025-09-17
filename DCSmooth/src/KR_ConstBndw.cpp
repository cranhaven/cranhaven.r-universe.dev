// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "DCSmooth_types.h"
#include "DCSmooth_kernels.h"

using namespace Rcpp;

//---------------------------------------------------------------------------//
//                    Kernel Regression Functions                            //
//---------------------------------------------------------------------------//

// function smoothes over the rows of a matrix yMat, conditional on columns

// [[Rcpp::export]]
arma::mat KRSmooth_matrix2(arma::mat yMat, double h, int drv,  SEXP kernFcnPtr)
{
  int nRow{ static_cast<int>(yMat.n_rows) };  // number of conditional Time-Series
  int nCol{ static_cast<int>(yMat.n_cols) };  // number of observations per Time-Series
  int bndw{ std::max(static_cast<int>(h * nCol), 2) }; // calculate absolute bandwidth, decimals will be dumped
  int windowWidth{ 2*bndw + 1 };              // width of estimation window

  arma::mat yMatOut(nRow, nCol);              // matrix for results

  // enable Kernel function
  XPtr<funcPtr> xpfun(kernFcnPtr);
  funcPtr kernFcn = *xpfun;

  // smoothing over interior values
  arma::colvec  uInterior{ arma::linspace(-bndw, bndw, windowWidth) / 
                           (h * nCol) }; // vector from -1 to 1 to compute weights
  arma::colvec  weightsInterior{ kernFcn(uInterior, 1 ) / 
                                 (nCol * std::pow(h, drv + 1.0)) };              // computation of weights (put in kernel-function later)
  arma::mat     yMatInterior(nRow, windowWidth);                            // empty matrix for use inside loop
  
  // Loops smooth over the columns, conditional on rows. That is, every row is
  // consiedered to be an individual time series. To speed up computation, the
  // smoothing order is inverted (computing of weights only once per column, as
  // the weights are the same on a grid)
  for (int colIndex{ bndw }; colIndex < (nCol - bndw); ++colIndex)                  // outer loop over columns
  {
    yMatInterior = yMat.cols(colIndex - bndw, colIndex + bndw);
    yMatOut.col(colIndex) = yMatInterior * weightsInterior;
  }

  // smoothing over boundaries
  for (int colIndex{ 0 }; colIndex < bndw; ++colIndex)
  {
    double q = static_cast<double>(colIndex)/bndw;
    arma::colvec  uBound(arma::regspace(colIndex, -bndw) / (h * nCol));
    arma::colvec  weightsBound{ kernFcn(uBound, q) / 
                              (nCol * std::pow(h, drv + 1.0)) };
    
    arma::mat     yLeftMat{ yMat.cols(0, uBound.n_rows - 1) };
    arma::mat     yRightMat{ yMat.cols(nCol - uBound.n_rows, nCol - 1) };
    
    yMatOut.col(colIndex) = yLeftMat * weightsBound;
    yMatOut.col(nCol - colIndex - 1) = std::pow(-1, drv) * yRightMat * 
                                       arma::reverse(weightsBound);
  }
  return yMatOut;
}

//---------------------------------------------------------------------------//

// [[Rcpp::export]]
arma::mat KR_dcs_const0(arma::mat yMat, arma::colvec hVec,
                    arma::colvec drvVec, SEXP kernFcnPtrX, SEXP kernFcnPtrT)
{
  // Smoothing over cond. on rows first (e.g. over single days).
  // Thus, drv and order is (1) instead of (0) here (depending on t)
  arma::mat mMatTemp{ KRSmooth_matrix2(yMat, hVec(1), drvVec(1), kernFcnPtrT) };
  // Smoothing over cols, drv and order is (0) (depending on x)
  arma::mat yMatOut{ KRSmooth_matrix2(mMatTemp.t(), hVec(0), drvVec(0), 
                                      kernFcnPtrX) };

  return yMatOut.t();
}
