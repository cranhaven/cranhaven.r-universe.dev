#include <bigmemory/MatrixAccessor.hpp>
#include <Rcpp.h>
#include <RcppEigen.h>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <random>

#include <RcppThread.h>

#include "SolverTypes.h"
#include "Solver.h"
#include "GaussianSolver.h"
#include "BinomialSolver.h"

// [[Rcpp::depends(RcppEigen, RcppThread, BH, bigmemory)]]

template <typename TG>
void fitModelCVRcppSingleFold(const TG& G,
                              const Eigen::Map<Eigen::VectorXd>& E,
                              const Eigen::Map<Eigen::VectorXd>& Y,
                              const Eigen::Map<Eigen::MatrixXd>& C,
                              const VecXd& fold_ids,
                              const Rcpp::LogicalVector& normalize,
                              const Eigen::VectorXd& grid,
                              double alpha,
                              const std::string& family,
                              double tolerance,
                              int max_iterations,
                              int min_working_set_size,
                              int test_fold_id,
                              Eigen::MatrixXd& xbeta,
                              Eigen::MatrixXd& test_loss,
                              Eigen::MatrixXi& beta_g_nonzero,
                              Eigen::MatrixXi& beta_gxe_nonzero,
                              Eigen::MatrixXi& has_converged) {
  const int n = fold_ids.size();
  Eigen::VectorXd weights(n);
  std::vector<int> test_idx;
  for (int i = 0; i < n; ++i) {
    if (fold_ids[i] == test_fold_id) {
      weights[i] = 0;
      test_idx.push_back(i);
    } else {
      weights[i] = 1;
    }
  }
  weights = weights / weights.sum();
  Eigen::Map<Eigen::VectorXd> weights_map(weights.data(), n);
  
  std::unique_ptr<Solver<TG> > solver;
  if (family == "gaussian") {
    solver.reset(
      new GaussianSolver<TG>(G, E, Y, C, weights_map, normalize[0]));
  } 
  else if (family == "binomial") {
    solver.reset(
      new BinomialSolver<TG>(G, E, Y, C, weights_map, normalize[0]));
  }

  int grid_size_squared;
  if (alpha < 0) {
    grid_size_squared = grid.size() * grid.size();
  } else {
    grid_size_squared = grid.size();
  }

  Eigen::VectorXd grid_lambda_1 = grid;
  std::sort(grid_lambda_1.data(), grid_lambda_1.data() + grid_lambda_1.size());
  std::reverse(grid_lambda_1.data(), grid_lambda_1.data() + grid_lambda_1.size());
  Eigen::VectorXd grid_lambda_2 = grid_lambda_1;
  
  int index = 0;
  int curr_solver_iterations;
  double curr_lambda_2;
  for (int i = 0; i < grid.size(); ++i) {
    for (int j = 0; j < grid.size(); ++j) {
      if (alpha < 0) {
        curr_lambda_2 = grid_lambda_2[j];
      } else {
        if (i == j) {
          curr_lambda_2 = grid_lambda_1[i] * alpha;
        } else {
          continue;
        }
      }
      
      curr_solver_iterations = solver->solve(grid_lambda_1[i], curr_lambda_2, tolerance, max_iterations, min_working_set_size);
      
      for (int i = 0; i < test_idx.size(); ++i) {
        xbeta(test_idx[i], index) = solver->get_xbeta(test_idx[i]);
      }
      test_loss(test_fold_id, index) = solver->get_test_loss(test_idx) / test_idx.size();
      beta_g_nonzero(test_fold_id, index) = solver->get_b_g_non_zero();
      beta_gxe_nonzero(test_fold_id, index) = solver->get_b_gxe_non_zero();
      has_converged(test_fold_id, index) = int(curr_solver_iterations < max_iterations);
      index++;
      
      if (index >= grid_size_squared) {
        break;
      }
    }
    std::reverse(grid_lambda_2.data(), grid_lambda_2.data() + grid_lambda_2.size());
    
    if (index >= grid_size_squared) {
      break;
    }
  }
}

template <typename TG>
Rcpp::List fitModelCVRcpp(const TG& G,
                          const Eigen::Map<Eigen::VectorXd>& E,
                          const Eigen::Map<Eigen::VectorXd>& Y,
                          const Eigen::Map<Eigen::MatrixXd>& C,
                          const Rcpp::LogicalVector& normalize,
                          const Eigen::VectorXd& grid,
                          double alpha,
                          const std::string& family,
                          double tolerance,
                          int max_iterations,
                          int min_working_set_size,
                          const Eigen::VectorXd& fold_ids,
                          int seed,
                          int ncores) {
  int grid_size_squared;
  if (alpha < 0) {
    grid_size_squared = grid.size() * grid.size();
  } else {
    grid_size_squared = grid.size();
  }
  const int nfolds = fold_ids.maxCoeff() + 1;
  const int n = fold_ids.size();
  Eigen::MatrixXd xbeta(n, grid_size_squared);
  Eigen::MatrixXd test_loss(nfolds, grid_size_squared);
  Eigen::MatrixXi beta_g_nonzero(nfolds, grid_size_squared);
  Eigen::MatrixXi beta_gxe_nonzero(nfolds, grid_size_squared);
  Eigen::MatrixXi has_converged(nfolds, grid_size_squared);

  if (ncores == 1) {
    for (int test_fold_id = 0; test_fold_id < nfolds; ++test_fold_id)
      fitModelCVRcppSingleFold<TG>(G, E, Y, C, fold_ids, normalize, grid, alpha, family,
                           tolerance, max_iterations, min_working_set_size,
                           test_fold_id, xbeta, test_loss, beta_g_nonzero, beta_gxe_nonzero, has_converged);
  } else {
    Eigen::initParallel();
    RcppThread::ThreadPool pool(ncores);
    for (int test_fold_id = 0; test_fold_id < nfolds; ++test_fold_id)
      pool.push([&, test_fold_id] {
        fitModelCVRcppSingleFold<TG>(G, E, Y, C, fold_ids, normalize, grid, alpha,
                             family, tolerance, max_iterations, min_working_set_size,
                             test_fold_id, xbeta, test_loss, beta_g_nonzero, beta_gxe_nonzero, has_converged);
      });
    pool.join();
  }
  return Rcpp::List::create(
    Rcpp::Named("xbeta") = xbeta,
    Rcpp::Named("test_loss") = test_loss,
    Rcpp::Named("beta_g_nonzero") = beta_g_nonzero,
    Rcpp::Named("beta_gxe_nonzero") = beta_gxe_nonzero,
    Rcpp::Named("has_converged") = has_converged
  );  
}

// [[Rcpp::export]]
Rcpp::List fitModelCV(SEXP G,
                      const Eigen::Map<Eigen::VectorXd>& E,
                      const Eigen::Map<Eigen::VectorXd>& Y,
                      const Eigen::Map<Eigen::MatrixXd>& C,
                      const Rcpp::LogicalVector& normalize,
                      const Eigen::VectorXd& grid,
                      double alpha,
                      const std::string& family,
                      double tolerance,
                      int max_iterations,
                      int min_working_set_size,
                      const Eigen::VectorXd& fold_ids,
                      int seed,
                      int ncores,
                      int mattype_g) {
  if (mattype_g == 1) {
    return fitModelCVRcpp<MapSparseMat>(Rcpp::as<MapSparseMat>(G), E, Y, C,
                                    normalize, grid, alpha,
                                    family, tolerance, max_iterations, min_working_set_size,
                                    fold_ids, seed, ncores);    
  } if (mattype_g == 2) {
    Rcpp::S4 G_info(G);
    Rcpp::XPtr<BigMatrix> xptr((SEXP) G_info.slot("address"));
    MapMat Gmap((const double *)xptr->matrix(), xptr->nrow(), xptr->ncol());
    return fitModelCVRcpp<MapMat>(Gmap, E, Y, C, normalize, grid, alpha,
                                  family, tolerance, max_iterations, min_working_set_size,
                                  fold_ids, seed, ncores);
  } else {
    Rcpp::NumericMatrix G_mat(G);
    MapMat Gmap((const double *) &G_mat[0], G_mat.rows(), G_mat.cols());
    return fitModelCVRcpp<MapMat>(Gmap, E, Y, C, normalize, grid, alpha,
                                  family, tolerance, max_iterations, min_working_set_size,
                                  fold_ids, seed, ncores);
  }
}
