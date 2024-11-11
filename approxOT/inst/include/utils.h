#ifndef _UTILS_H
#define _UTILS_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <vector> 
#include <functional> 
#include <algorithm> 
#include <iostream>
#include <cmath>
#include <numeric>
#include <boost/noncopyable.hpp>
#include "approxOT_types.h"
#include "sort.h"

using namespace Rcpp;
using namespace RcppEigen;

using Eigen::MatrixXd;
using Eigen::MatrixXi;
using Eigen::ArrayXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::SparseMatrix;
using Eigen::RowVectorXd;
using Eigen::Lower;
using Eigen::Upper;
using Eigen::Ref;
using Eigen::Map;
using Rcpp::as;
typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> MatrixX;
typedef Eigen::Matrix<double, Eigen::Dynamic, 1> VectorX;

typedef Map<const MatrixX> MapMat;
typedef Map<const VectorX> MapVec;
typedef Eigen::Triplet<double> T;
typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::SparseVector<double> SpVec;
typedef Eigen::SparseMatrix<double> SpMat;
typedef Map<MatrixXd> MapMatd;
typedef Map<VectorXd> MapVecd;
//typedef Eigen::Map<const MatrixXd> MapMatdnc;
//typedef Eigen::Map<const VectorXd> MapVecdnc;
typedef Map<ArrayXd>  MapArrayd;
typedef Eigen::SparseMatrix<double, Eigen::RowMajor> SpMatR;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> SpMatIntR;

   
VectorXd cumsum(const VectorXd& x);

VectorXd cumsumrev(const VectorXd& x);

bool stopRule(const VectorXd& cur, const VectorXd& prev, const double& tolerance);

bool stopRule(const SpVec& cur, const SpVec& prev, const double& tolerance);

bool stopRuleMat(const MatrixXd& cur, const MatrixXd& prev, const double& tolerance);

bool nonZero(const refMatConst & v);

int countNonZero(const refMatConst & v);

bool nonFinite(const refMatConst & v);

bool nonFiniteDist(const refMatConst & v);

int  compare(const MatrixXi & a, const MatrixXi & b, VectorXi & idx_col);

inline MatrixXd crossprod(const MatrixXd & X) {
  int P = X.cols();
  return( MatrixXd(P, P).setZero().selfadjointView<Lower>().rankUpdate(X.adjoint()));
}

inline MatrixXd tcrossprod(const MatrixXd & X) {
  int P = X.rows();
  return( MatrixXd(P, P).setZero().selfadjointView<Lower>().rankUpdate(X));
}


matrix covariance(const refMatConst & samples, const refVecConst & mean);
matrix covariance(const refMatConst & samples);

void which(const matrixI & basis, int N, int M, matrixI & index);
void which_nonzero(const matrix & basis, int N, int M, matrixI & index);
// template <typename Derived>
// double median(const Eigen::EigenBase<Derived>& X);

double sinkhorn_converge(const vector & u, const vector & u_old);
double sinkhorn_converge_log(const vector & f, const vector & f_old);
double dist_approx_ot(const refVecConst & mass_a, const refVecConst & mass_b,
                const vector & r, const vector & c, int p);
double rho_ot(const vector & a, const vector & b);
vector rho_vec(const vector & a, const vector & b);
double rho(double a, double b);

void argmin_f(const refVecConst & mass_a, 
              const refVecConst & mass_b, 
              const matrix & exp_cost,
              vector & u, vector & v, vector & y_u, vector & y_v, vector & u_hat, vector & v_hat);

// template <class RandAccessIter>
// double median(RandAccessIter begin, RandAccessIter end);

double median(refMat A);
double median(const matrix & A);
double median(matrix & A);

#endif
