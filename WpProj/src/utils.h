#ifndef _oem_UTILS_H
#define _oem_UTILS_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <vector> 
#include <functional> 
#include <algorithm> 
#include <iostream>
#include <cmath>
#include <numeric>
#include <boost/noncopyable.hpp>
#include "WpProj_types.h"
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

   
double threshold(double num);

VectorXd cumsum(const VectorXd& x);

VectorXd cumsumrev(const VectorXd& x);

// MATRIX PRODUCTS

VectorXd sliced_crossprod(const MatrixXd& X, const VectorXd& y, const VectorXi& idx);

VectorXd sliced_matvecprod(const MatrixXd& A, const VectorXd& b, const std::vector<int>& idx);

void sliced_crossprod_inplace(VectorXd &res, const MatrixXd& X, const VectorXd& y, const std::vector<int>& idx);


//computes X'WX where W is diagonal (input w as vector)
MatrixXd XtWX(const MapMatd& xx, const MatrixXd& ww);

//computes XWX' where W is diagonal (input w as vector)
MatrixXd XWXt(const MapMatd& xx, const MatrixXd& ww);

//SpMat X'WX where W is diagonal (input w as vector)
SpMat XtWX(const MSpMat& xx, const MatrixXd& ww);

//computes XWX' where W is diagonal (input w as vector)
SpMat XWXt(const MSpMat& xx, const MatrixXd& ww);

//computes X'X 
//MatrixXd XtX(const MapMatd &xx);

//MatrixXd XtX(MapMat &xx);

//MatrixXd XtX(const MapMat &xx);

void XtX(MatrixXd &xTx, const MatrixXd &xx);

MatrixXd XtX(const MatrixXd &xx);

//MatrixXd XtX(MatrixXd &xx);

//computes XX'
//MatrixXd XXt(const MapMatd& xx);

//MatrixXd XXt(MapMat& xx);

//MatrixXd XXt(const MapMat& xx);

MatrixXd XXt(const MapMat& xx);

//MatrixXd XXt(MatrixXd& xx);

//computes X'X 
MatrixXd XtX_scaled(const MapMatd &xx, RowVectorXd &colmeans, RowVectorXd &colstd);

//computes XX'
MatrixXd XXt_scaled(const MapMatd& xx, RowVectorXd &colmeans, RowVectorXd &colstd);

//computes X'X 
SpMat XtX(const MSpMat& xx);

//computes XX'
SpMat XXt(const MSpMat& xx);

// soft thresholding

/*
void soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty);

void soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty);

void soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact);

void soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact, double &d);

void soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact);

void soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact, double &d);

void soft_threshold_mcp(VectorXd &res, const VectorXd &vec, const double &penalty, 
                        VectorXd &pen_fact, double &d, double &alpha);
 
 */

void update_active_set(VectorXd &u, std::vector<int> &active, std::vector<int> &inactive,
                       double &lambdak, double &lambdakminus1, const int &penalty);

void initiate_active_set(VectorXd &u, std::vector<int> &active, std::vector<int> &inactive,
                         double &lambdak, double &lambdamax, const int &nvars, const int &penalty);

void block_soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty,
                                 const int &ngroups, VectorXi &unique_grps, VectorXi &grps);

void block_soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty,
                          const int &ngroups, VectorXi &unique_grps, VectorXi &grps);

/*
void block_soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty,
                                 const int &ngroups, const MapVeci &unique_grps, const MapVeci &grps);
 */ 
  
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
/*
template <typename T, typename T2>
T extract(const T2& full, const T& ind)
{
    int num_indices = ind.innerSize();
    T target(num_indices);
    for (int i = 0; i < num_indices; i++)
    {
        target[i] = full[ind[i]];
    }
    return target;
}
*/

void mu_update(const refMatConst & X, 
               const refMatConst & result, 
               const refMatConst & theta, 
               matrix & mu,
               const Rcpp::CharacterVector & method);

matrix covariance(const refMatConst & samples, const refVecConst & mean);
matrix covariance(const refMatConst & samples);

void which(const matrixI & basis, int N, int M, matrixI & index);
void which_nonzero(const matrix & basis, int N, int M, matrixI & index);
// template <typename Derived>
// double median(const Eigen::EigenBase<Derived>& X);
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

#endif
