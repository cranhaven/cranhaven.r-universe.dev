/*
 * Functions which are related with MED criterion
 */
# ifndef MEDFUNC_H_
# define MEDFUNC_H_
# include "mathfunc.h"
# include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppEigen;
using namespace Eigen;
using namespace std;

/*
 * compute the logarithm of generalized distance between two points
 */
double getLogGenDis(const VectorXd & x, const VectorXd & y, const double s)
{
  VectorXd temp = x - y;
  try
  {
    if(x.size() != y.size())
    {
      throw std::length_error("vectors must have the same length.");
    }
    if(s > 1e-6)
    {
      temp = temp.array().abs().pow(s);
      return log(temp.sum()) / s;
    }
    else
    {
      temp = temp.array().abs().log() / x.size();
      return temp.sum();
    }
  }catch(std::exception &ex)
  {
    forward_exception_to_r(ex);
    return -1.0;
  }
}

/*
 * compute the logarithm of generalized distanses between one point and another points list
 */
VectorXd getLogGenDis(const MatrixXd & x, const VectorXd & y, const double s)
{
  VectorXd res(x.rows());
  for(int i = 0; i < res.size(); i++)
  {
    res(i) = -1.0;
  }
  int n(y.size());
  try
  {
    if(x.cols() != n)
    {
      throw std::length_error("The columns of x must be same as the size of vector y");
    }
    VectorXd temp(x.cols());
    for(int i = 0; i < res.size(); i++)
    {
      temp = x.row(i);
      res(i) = getLogGenDis(temp, y, s);
    }
    return res;
  }catch(std::exception &ex)
  {
    forward_exception_to_r(ex);
    return res;
  }
}

/*
 * translate points matrix with variance matrix
 */
MatrixXd translateMatrix(MatrixXd & SigmaMatrix, MatrixXd & x)
{
  MatrixXd res(x.rows(), x.cols());
  res = x * SigmaMatrix;
  return res;
}

/*
 * translate point with variance matrix
 */
VectorXd translateVector(MatrixXd & SigmaMatrix, VectorXd & x)
{
  return x.transpose() * SigmaMatrix;
}

/*
 * compute the sqrt inverse of variance matrix
 */
MatrixXd sqrtInvMatrix(MatrixXd & SigmaMatrix)
{
  SelfAdjointEigenSolver<MatrixXd> res(SigmaMatrix);
  VectorXd eigenvalues = res.eigenvalues();
  eigenvalues = 1.0 / eigenvalues.array().sqrt();
  return res.eigenvectors() * eigenvalues.asDiagonal()  * res.eigenvectors().transpose();
}

/*
 * choose MED points from candidate according to MED criterion which consider using Mahalanobis distance.
 */
List chooseMED(MatrixXd & candidates, // candidates 
               VectorXd & candlf, // corresponding logarithm density functions at candidates
               int n, // number of MED points
               MatrixXd & Sigmak, // variance matrix estimated in last step
               double gamma, //
               double s //
                 )
{
  int N(candidates.rows()); // number of candidates
  int MaxIndex(0); // index with maximum value
  int dim = candidates.cols();
  MatrixXd MEDPoints(n, candidates.cols()); // MED points
  VectorXd lfMED(n); // corresponding logarithm density functions at MED points
  MatrixXd SqrtSigmaInv = sqrtInvMatrix(Sigmak);
  VectorXd tempPoint(candidates.cols());
  VectorXd tempCrit(N);
  VectorXd rowMinCrit = MatrixXd::Constant(N, 1, 1e16);
  double templf(0.0);
  
  /*
   * Choose the point with maximum logarithm density value as first MED point
   */
  MaxIndex = argMax(candlf);
  tempPoint = candidates.row(MaxIndex);
  templf = candlf(MaxIndex);
  MEDPoints.row(0) = tempPoint;
  lfMED(0) = templf;
  
  /*
   * Search MED points sequentially one point at a time
   */
  MatrixXd transCand = translateMatrix(SqrtSigmaInv, candidates);
  VectorXd transtempPoint = translateVector(SqrtSigmaInv, tempPoint);
  MatrixXd CritCandMEDMatrix = MatrixXd::Constant(N, n, 1e16);
  for(int i = 1; i < n; i++)
  {
    tempCrit = 0.5 * gamma * (templf + candlf.array()) + dim * getLogGenDis(transCand, transtempPoint, s).array();
    rowMinCrit = compareMin(rowMinCrit, tempCrit);
    CritCandMEDMatrix.col(i) = tempCrit;
    MaxIndex = argMax(rowMinCrit);
    tempPoint = candidates.row(MaxIndex);
    templf = candlf(MaxIndex);
    MEDPoints.row(i) = tempPoint;
    lfMED(i) = templf;
    transtempPoint = translateVector(SqrtSigmaInv, tempPoint);
  }
  return List::create(Named("points")=MEDPoints, Named("logf")=lfMED); // , Named("criterion")=CritCandMEDMatrix
}
# endif
