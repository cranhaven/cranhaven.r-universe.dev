#ifndef SOLVER_TYPES_H
#define SOLVER_TYPES_H

#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::Map<const Eigen::MatrixXd> MapMat;
typedef Eigen::Map<const Eigen::VectorXd> MapVec;
typedef Eigen::Map<Eigen::SparseMatrix<double>> MapSparseMat;
typedef Eigen::Map<Eigen::SparseVector<double>> MapSparseVec;
typedef Eigen::MatrixXd MatXd;
typedef Eigen::VectorXd VecXd;
typedef Eigen::VectorXi VecXi;
typedef Eigen::ArrayXd ArrayXd;
typedef Eigen::Array<bool, Eigen::Dynamic, 1> ArrayXb;

#endif // SOLVER_TYPES_H
