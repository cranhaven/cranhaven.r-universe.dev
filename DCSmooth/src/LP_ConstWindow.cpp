// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include "DCSmooth.h"
#include "DCSmooth_types.h"

using namespace Rcpp;

//---------------------------------------------------------------------------//

// function smoothes over the rows of a matrix yMat, conditional on columns

// [[Rcpp::export]]
arma::mat LPSmooth_matrix_BMod(const arma::mat yMat, const double h,
                               const int polyOrder, const int drv,
                               const int mu, SEXP weightFcnPtr)
{
  int nRow{ static_cast<int>(yMat.n_rows) };    // number of conditional Time-Series
  int nCol{ static_cast<int>(yMat.n_cols) };    // number of observations per Time-Series
  int bndw{ std::max(static_cast<int>(h * nCol), polyOrder + 1) };
  // calculate absolute bandwidth
  int windowWidth{ std::min(2*bndw + 1, nCol) };  // width of estimation window
  arma::mat yMatOut(nRow, nCol);  // matrix for results
  
  // enable weight function
  XPtr<weightPtr> xpfun(weightFcnPtr);
  weightPtr weightFcn = *xpfun;
  
  // smoothing over boundaries
  arma::colvec  xBound{ (arma::regspace(1, windowWidth)) / (nCol - 1) };
  arma::colvec  xAdd(windowWidth);
  xAdd.fill(1.0 / (nCol - 1));
  
  for (int colIndex{ 0 }; colIndex < bndw + 1; ++colIndex)
  {
    if (colIndex >= (nCol/2) + 1)     // break condition, if bndw > 0.5*nCol
    {
      break;
    }
    
    // calculate weights for linear regression
    xBound = xBound - xAdd;
    double h_window{ 2*h + xBound(0) }; // uses first value of xBound, which is actually -x
    double q{ 2*h / h_window - 1 };
    
    arma::colvec uBound{ - xBound / h_window };
    arma::colvec wBound{ weightFcn(uBound, q, mu) };
    
    arma::mat xMatBound{ xMatrix(xBound, polyOrder) };
    arma::mat xMatWeight{ weightMatrix(wBound, xMatBound) };
    arma::mat weightsMat{ arma::inv(xMatWeight.t() * xMatBound) *
                          xMatWeight.t() };
    
    arma::rowvec weightsLeft{ factorialFunction(drv) * weightsMat.row(drv) };
    arma::rowvec weightsRight{ std::pow(-1, drv) * weightsLeft };
    
    // calculation of estimates (complete column)
    yMatOut.col(colIndex) = yMat.cols(0, xBound.n_rows - 1) * weightsLeft.t();
    yMatOut.col(nCol - colIndex - 1) = yMat.cols(nCol -
      xBound.n_rows, nCol - 1) * arma::reverse(weightsRight.t());
  }
  
  if (h < 0.5)
  {
    // calculate weights for interior smoothing
    arma::colvec xVec{ arma::regspace(-bndw, bndw) / (nCol - 1) };
    arma::colvec uVec{ - xVec / h };
    arma::colvec wVec{ weightFcn(uVec, 1, mu) };
    
    arma::mat xMatInterior{ xMatrix(xVec, polyOrder) };
    arma::mat xWeightsInterior{ weightMatrix(wVec, xMatInterior) };
    arma::mat weightsMat{ arma::inv(xWeightsInterior.t() * xMatInterior) *
      xWeightsInterior.t() * factorialFunction(drv) };
    arma::rowvec weightsInterior{ weightsMat.row(drv) };
    
    // Loops smooth over the columns, conditional on rows. That is, every row is
    // considered to be an individual time series. To speed up computation, the
    // smoothing order is inverted (computing of weights only once per column, as
    // the weights are the same on a grid)
    for (int colIndex{ bndw + 1 }; colIndex < (nCol - bndw - 1); ++colIndex)
    {
      arma::mat yInterior{ yMat.cols(colIndex - bndw, colIndex + bndw) };
      yMatOut.col(colIndex) = yInterior * weightsInterior.t();
    }
  }
  
  return yMatOut;
}

//---------------------------------------------------------------------------//

// [[Rcpp::export]]
arma::mat LP_dcs_const1_BMod(arma::mat yMat, arma::colvec hVec,
                        arma::icolvec polyOrderVec, arma::icolvec drvVec,
                        arma::icolvec muVec, SEXP weightFcnPtr_x, 
                        SEXP weightFcnPtr_t)
{
  // Smoothing over cond. on rows first (e.g. over single days).
  // Thus, drv and order is (1) instead of (0) here (depending on t)
  arma::mat mMatTemp{ LPSmooth_matrix_BMod(yMat, hVec(1), polyOrderVec(1),
                                           drvVec(1), muVec(1),
                                           weightFcnPtr_t) };
  // Smoothing over cols, drv and order is (0) (depending on x)
  arma::mat yMatOut{ LPSmooth_matrix_BMod(mMatTemp.t(), hVec(0), 
                                          polyOrderVec(0), drvVec(0), muVec(0),
                                          weightFcnPtr_x) };
  
  return yMatOut.t();
}
