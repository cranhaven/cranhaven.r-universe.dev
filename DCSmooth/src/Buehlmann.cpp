// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include "DCSmooth.h"
#include "DCSmooth_types.h"

using namespace Rcpp;

arma::mat acfMatrix_cpp(const arma::mat y_Mat);

//----------------------------------------------------------------------------//

//-------------------------------Main Function--------------------------------//

// //[[Rcpp::export]]
// arma::vec specDens_cpp(arma::mat Y, arma::vec omega)
// {
//   int nX{ Y.n_rows };
//   int nT{ Y.n_cols };
//
//   arma::mat y_Mat{ Y - accu(Y)/(nX * nT) };
//   arma::mat acfMat{ acfMatrix_cpp(y_Mat) };
//
//   // initial bandwidth values
//   arma::vec hVec(2);
//   arma::vec hVecInfl(2);
//   arma::vec hVecTemp(2);
//   arma::vec inflFct(2);
//   hVec(0) = trunc(nX/2); hVec(1) = trunc(nT/2);
//   inflFct(0) = 1/pow(nX, 2.0/12); inflFct(1) = 1/pow(nT, 2.0/12);
//
//   // global step
//   for (int g{ 0 }; g < 22; g++)
//   {
//     hVecTemp = hVec;
//     hVecInfl = trunc(hVec % inflFct) + 1;
//     //hVec = globalBndwEst(acfMat, hLag = hVecInfl);
//     if (arma::approx_equal(hVec, hVecTemp, "absdiff", 0.001))
//     {
//       break;
//     }
//   }
//
//   // local step
//   // hVecInfl = trunc(hVec % inflFct) + 1;
//   // hOpt = localBndwEst(acfMat, hVecInfl, omega);
//   //
//   // arma::vec drv0(2, fill::zeros);
//   // double specDensOut = specDensEst(acfMat, drv = drv0, hLag = hOpt,
//   //                                  omega = omega);
//   //
//   // arma::vec returnVec(3);
//   // returnVec(0) = specDensOut;
//   // returnVec.subvec(1, 2) = hOpt;
//
//   return hVecInfl;
// }

//-----------------------------Calculation of acf-----------------------------//

// estimation of acf matrix for s = 0,...,nX; u = 0,...,nT

//[[Rcpp::export]]
arma::mat acfMatrix_quarter2(const arma::mat y_Mat)
{
  int nRow{ static_cast<int>(y_Mat.n_rows) };
  int nCol{ static_cast<int>(y_Mat.n_cols) };
  arma::mat y0_Mat{ y_Mat };//- accu(y_Mat)/(nRow * nCol) };
  arma::mat R_out;

  R_out.zeros(nRow, nCol);

  for (int i{ 0 }; i < nRow; i++)
  {
    for (int j{ 0 }; j < nCol; j++)
    {
      arma::mat y0{ y0_Mat.submat(0, 0, nRow - i - 1, nCol - j - 1)};
      arma::mat y1{ y0_Mat.submat(i, j, nRow - 1, nCol - 1)};
      R_out(i, j) = accu(y0 % y1);
    }
  }

  return R_out/(nRow * nCol);
}

// arma::mat acfMatrix_quarter(const arma::mat y_Mat)
// {
//   int nRow{ y_Mat.n_rows };
//   int nCol{ y_Mat.n_cols };
//
//   arma::mat R_out;
//   R_out.zeros(nRow, nCol);
//
//   for (int i{ 0 }; i < nRow; i++)
//   {
//     for (int j{ 0 }; j < nCol; j++)
//     {
//       arma::mat y0{ y_Mat.submat(0, 0, nRow - i - 1, nCol - j - 1)};
//       arma::mat y1{ y_Mat.submat(i, j, nRow - 1, nCol - 1)};
//       R_out(i, j) = accu(y0 % y1);
//     }
//   }
//
//   return R_out/(nRow * nCol);
// }
//
//
// // estimation of complete acf matrix
// //[[Rcpp::export]]
// arma::mat acfMatrix_cpp(const arma::mat y_Mat)
// {
//   int nRow{ y_Mat.n_rows };
//   int nCol{ y_Mat.n_cols };
//
//   arma::mat y0_Mat{ y_Mat - arma::accu(y_Mat)/(nRow * nCol) };
//   // top right submatrix
//   arma::mat y1_Mat{ reverse(y0_Mat, 0) };
//
//   arma::mat acf0_Mat{ acfMatrix_quarter(y0_Mat) };
//   arma::mat acf1_Mat{ acfMatrix_quarter(y1_Mat) };
//
//   arma::mat acfMat_out;
//   acfMat_out.zeros(2*nRow - 1, 2*nCol - 1);
//   acfMat_out.submat(0, 0, nRow - 1, nCol - 1) = reverse(reverse(acf0_Mat, 1), 0);
//   acfMat_out.submat(nRow - 1, nCol - 1, 2*nRow - 2, 2*nCol - 2) = acf0_Mat;
//   acfMat_out.submat(nRow - 1, 0, 2*nRow - 2, nCol - 1) =
//                                                 reverse(reverse(acf1_Mat, 1), 0);
//   acfMat_out.submat(0, nCol - 1, nRow - 1, 2*nCol - 2) = acf1_Mat;
//
//
//   return acfMat_out;
// }
//
// //------------------------Estimation of Spectral Density----------------------//
//
// //
