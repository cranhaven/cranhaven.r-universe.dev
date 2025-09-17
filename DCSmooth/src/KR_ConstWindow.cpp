// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "DCSmooth_types.h"

using namespace Rcpp;

arma::vec kernFkt_MW200(arma::vec&, double);
arma::vec kernFkt_MW210(arma::vec&, double);
arma::vec kernFkt_MW220(arma::vec&, double);
arma::vec kernFkt_MW320(arma::vec&, double);
arma::vec kernFkt_MW420(arma::vec&, double);
arma::vec kernFkt_MW422(arma::vec&, double);

//---------------------------------------------------------------------------//
//                    Kernel Regression Functions                            //
//---------------------------------------------------------------------------//

// function smoothes over the rows of a matrix yMat, conditional on columns




// [[Rcpp::export]]
arma::mat KRSmooth_matrix(arma::mat yMat, double h, int drv, SEXP kernFcnPtr) 
{
  int nRow{ static_cast<int>(yMat.n_rows) };                // number of conditional Time-Series
  int nCol{ static_cast<int>(yMat.n_cols) };                // number of observations per Time-Series
  int bndw{ std::max(static_cast<int>(h * nCol), 2) }; // calculate absolute bandwidth, decimals will be dumped
  int windowWidth{ 2*bndw + 1 };          // width of estimation window

  arma::mat yMatOut(nRow, nCol);          // matrix for results

  // enable Kernel function
  XPtr<funcPtr> xpfun(kernFcnPtr);
  funcPtr kernFcn = *xpfun;
  
  // smoothing over boundaries
  arma::mat    yLeftMat{ yMat.cols(0, windowWidth - 1) };
  arma::mat    yRightMat{ yMat.cols(nCol - windowWidth, nCol - 1) };
  
  for (int colIndex{ 0 }; colIndex < bndw + 1; ++colIndex)
  {
    if (colIndex >= (nCol/2) + 1)
    {
      break;
    }
    
    // calculate weights for kernel regression
    double h_window{ 2*h - colIndex/(nCol - 1.0) };
    double q{ 2*h / h_window - 1 };
    arma::colvec uBound{ arma::regspace(colIndex, colIndex + 1 - windowWidth) /
      ((nCol - 1) * h_window) };
    arma::colvec weightsBound{ kernFcn(uBound, q) /
                               ((nCol - 1) * std::pow(h_window, drv + 1.0)) };

    // calculation of estimates (complete columns)
    yMatOut.col(colIndex) = yLeftMat * weightsBound;
    yMatOut.col(nCol - colIndex - 1) =  std::pow(-1, drv) * 
                                        yRightMat * reverse(weightsBound);
  }

  // smoothing over interior values
  arma::colvec  uInterior{ arma::regspace(-bndw, bndw) / (h * (nCol - 1)) };   // vector from -1 to 1 to compute weights
  arma::colvec  weightsInterior{ kernFcn(uInterior, 1) / 
                                 ((nCol - 1) * std::pow(h, drv + 1.0)) };           // computation of weights (put in kernel-function later)

  // Loops smooth over the columns, conditional on rows. That is, every row is
  // considered to be an individual time series. To speed up computation, the
  // smoothing order is inverted (computing of weights only once per column, as
  // the weights are the same on a grid)
  for (int colIndex{ bndw + 1 }; colIndex < (nCol - bndw - 1); ++colIndex) // outer loop over columns
  {
    arma::mat yMatInterior{ yMat.cols(colIndex - bndw, colIndex + bndw) };
    yMatOut.col(colIndex) = yMatInterior * weightsInterior;
  }
  
  return yMatOut;
}

//---------------------------------------------------------------------------//

// [[Rcpp::export]]
arma::mat KR_dcs_const1(arma::mat yMat, arma::colvec hVec,
              arma::icolvec drvVec, SEXP kernFcnPtrX, SEXP kernFcnPtrT)
{
  // Smoothing over cond. on rows first (e.g. over single days).
  // Thus, drv and order is (1) instead of (0) here (depending on t)
  arma::mat mMatTemp{ KRSmooth_matrix(yMat, hVec(1), drvVec(1), kernFcnPtrT) };
  // Smoothing over cols, drv and order is (0) (depending on x)
  arma::mat yMatOut{ KRSmooth_matrix(mMatTemp.t(), hVec(0), drvVec(0), 
                                     kernFcnPtrX) };

  return yMatOut.t();
}
