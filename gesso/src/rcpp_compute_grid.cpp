#include <bigmemory/MatrixAccessor.hpp>
#include <Rcpp.h>
#include <RcppEigen.h>
#include <string.h>

#include "SolverTypes.h"
#include "Solver.h"
#include "GaussianSolver.h"
#include "BinomialSolver.h"

// [[Rcpp::depends(RcppEigen, BH, bigmemory)]]

template <typename TG>
double computeLambdaMaxRcpp(const TG& G,
                            const Eigen::Map<VecXd>& E,
                            const Eigen::Map<VecXd>& Y,
                            const Eigen::Map<MatXd>& C,
                            const Eigen::Map<VecXd>& weights,
                            const Rcpp::LogicalVector& normalize,
                            const std::string& family) {
  
  const int max_iterations = 100;
  const int n = Y.rows();
  VecXd nu(n);
  VecXd normalize_weights_g;
  double normalize_weights_e;
  
  if (family == "gaussian") {
    GaussianSolver<TG> solver(G, E, Y, C, weights, normalize[0]);
    solver.update_intercept();
    solver.get_residual(nu);
    normalize_weights_g = solver.get_normalize_weights_g();
    normalize_weights_e = solver.get_normalize_weights_e();
  } 
  else if (family == "binomial") {
    BinomialSolver<TG> solver(G, E, Y, C, weights, normalize[0]);
    solver.update_intercept(0, max_iterations, nu);
    normalize_weights_g = solver.get_normalize_weights_g();
    normalize_weights_e = solver.get_normalize_weights_e();
  }
  nu = nu.cwiseProduct(weights);
  VecXd tmp = (nu.transpose() * G).cwiseAbs().transpose().cwiseProduct(normalize_weights_g);
  const double max_G_by_nu = tmp.maxCoeff();
  tmp = (normalize_weights_e * nu.cwiseProduct(E).transpose() * G).cwiseAbs().transpose().cwiseProduct(normalize_weights_g);
  const double max_GxE_by_nu = tmp.maxCoeff();
  const double lambda_max = std::max(max_G_by_nu, max_GxE_by_nu);
  return lambda_max;
}

// [[Rcpp::export]]
double computeLambdaMax(SEXP G,
                        const Eigen::Map<Eigen::VectorXd>& E,
                        const Eigen::Map<Eigen::VectorXd>& Y,
                        const Eigen::Map<Eigen::MatrixXd>& C,
                        const Eigen::Map<Eigen::VectorXd>& weights,
                        const Rcpp::LogicalVector& normalize,
                        const std::string& family,
                        int mattype_g) {
  if (mattype_g == 1) {
    return computeLambdaMaxRcpp<MapSparseMat>(Rcpp::as<MapSparseMat>(G), E, Y, C,
                                              weights, normalize, family);
  } if (mattype_g == 2) {
    Rcpp::S4 G_info(G);
    Rcpp::XPtr<BigMatrix> xptr((SEXP) G_info.slot("address"));
    MapMat Gmap((const double *)xptr->matrix(), xptr->nrow(), xptr->ncol());
    return computeLambdaMaxRcpp<MapMat>(Gmap, E, Y, C, weights, normalize, family);
  } else {
    Rcpp::NumericMatrix G_mat(G);
    MapMat Gmap((const double *) &G_mat[0], G_mat.rows(), G_mat.cols());
    return computeLambdaMaxRcpp<MapMat>(Gmap, E, Y, C, weights, normalize, family);
  }
}
