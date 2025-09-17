// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include "DCSmooth.h"
#include "DCSmooth_types.h"

using namespace Rcpp;

//---------------------------------------------------------------------------//

arma::mat weightMatrix(arma::colvec weights, arma::mat matrix)
{
  arma::mat matrixOut{ arma::mat(matrix.n_rows, matrix.n_cols) };
  for (int j{ 0 }; j < matrix.n_cols; ++j)
  {
    matrixOut.col(j) = weights % matrix.col(j);
  }
  
  return matrixOut;
}

//---------------------------------------------------------------------------//

// rewrite x-Vector as x-Matrix for lm model, x can be a vector before this function

// [[Rcpp::export]]
arma::mat xMatrix(arma::colvec xVector, int polyOrder)
{
  arma::mat returnMatrix{ arma::ones(xVector.n_rows, polyOrder + 1) };
  
  // put powers of x into matrix depending on order (local linear: order = 1)
  for (int indexOrder{ 0 }; indexOrder < polyOrder; ++indexOrder)
    returnMatrix.col(indexOrder + 1) = pow(xVector, indexOrder + 1);
  
  return returnMatrix;
}

//----------------------------------------------------------------------------//

// // Calculates K_p - Kernels
// 
// // [[Rcpp::export]]
// arma::mat np_matrix(SEXP kernFcnPtr, int p, int n = 500)
// {
//   // enable Kernel function
//   XPtr<funcPtr> xpfun(kernFcnPtr);
//   funcPtr kernFcn = *xpfun;
//   
//   arma::vec uVec{ arma::linspace(-1, 1, n) };
//   arma::mat matrixOut;
//   matrixOut.zeros(p + 1, p + 1);
//   
//   arma::vec kernVec{ kernFcn(uVec, 1) };
//   
//   for (int i{ 0 }; i <= p; ++i)
//   {
//     for (int j { i }; j <= p; ++j)
//     {
//       matrixOut(i, j) = sum(pow(uVec, i + j) % kernVec);
//     }
//   }
//   
//   return matrixOut/(0.5*(n - 1));
// }
// 
// // [[Rcpp::export]]
// arma::vec m_weights(arma::mat npMatrix, arma::vec u, int drv)
// {
//   int p = npMatrix.n_rows - 1;
//   arma::mat mpMatrix{ npMatrix };
//   arma::vec weightsOut(u.n_rows);
//   arma::mat uMatrix{ xMatrix(u, p) };
//   
//   for (int i{ 0 }; i < u.n_rows; ++i)
//   {
//     mpMatrix.col(drv) = uMatrix.row(i).t();
//     weightsOut(i) = arma::det(mpMatrix)/arma::det(npMatrix);
//   }
//   
//   return weightsOut;
// }

// arma::vec lejWeights(arma::vec u, int p, int drv, SEXP kernFcnPtr)
// {
//   arma::mat npMat{ npMatrix(kernFcnPtr, p) };
//   arma::vec weightsOut{ mWeights(npMat, u, drv) };
//   
//   return weightsOut;
// }

//----------------------------------------------------------------------------//

int factorialFunction(int value)
{
  int outValue{ 1 };
  
  for(int count{ 1 }; count <= value; ++count)
  {
    outValue *= count;
  }
  
  return outValue;
}

//[[Rcpp::export]]
NumericVector cumsum_part_reverse(arma::rowvec vec_1, arma::colvec vec_2)
{
  if (vec_1.n_elem != vec_2.n_elem)
  {
    stop("lengths differ");
  }
  
  NumericVector vec_out(vec_1.n_elem);
  
  for(int i{ 0 }; i < vec_1.n_elem; ++i)
  {
    vec_out(i) = as_scalar(vec_1.subvec(0, i) *
                  arma::reverse(vec_2.subvec(0, i)));
  }
  
  return vec_out;
}
