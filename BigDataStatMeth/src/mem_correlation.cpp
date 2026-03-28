/**
 * @file mem_correlation.cpp
 * @brief In-memory correlation computation functions for BigDataStatMeth
 * 
 * This file provides R-callable functions for computing correlations of matrices
 * that fit in memory, optimized for performance with OpenMP parallelization.
 * 
 * @author BigDataStatMeth Development Team
 * @date 2025
 * @version 1.0
 */

#include "BigDataStatMeth.hpp"

using namespace Rcpp;
using namespace RcppEigen;
using namespace BigDataStatMeth;

//' @title Compute correlation matrix for in-memory matrices (unified function)
//' @description Compute Pearson or Spearman correlation matrix for matrices that fit in memory.
//' This function automatically detects whether to compute:
//' \itemize{
//'   \item Single matrix correlation cor(X) - when only matrix X is provided
//'   \item Cross-correlation cor(X,Y) - when both matrices X and Y are provided
//' }
//' 
//' @param X First numeric matrix (observations in rows, variables in columns)
//' @param Y Second numeric matrix (optional, observations in rows, variables in columns)
//' @param trans_x Logical, whether to transpose matrix X (default: FALSE) 
//' @param trans_y Logical, whether to transpose matrix Y (default: FALSE, ignored if Y not provided)
//' @param method Character string indicating correlation method ("pearson" or "spearman", default: "pearson")
//' @param use_complete_obs Logical, whether to use only complete observations (default: TRUE)
//' @param compute_pvalues Logical, whether to compute p-values for correlations (default: TRUE)
//' @param threads Integer, number of threads for parallel computation (optional, default: -1 for auto)
//' 
//' @return A list containing correlation results
//' 
//' @examples
//' \dontrun{
//' # Backward compatible - existing code unchanged
//' set.seed(123)
//' X <- matrix(rnorm(1000), ncol = 10)
//' result_original <- bdCorr_matrix(X)
//' 
//' # Create omics-style data
//' gene_expr <- matrix(rnorm(5000), nrow = 100, ncol = 50)  # 100 samples × 50 genes
//' 
//' # Gene-gene correlations (variables)
//' gene_corr <- bdCorr_matrix(gene_expr, trans_x = FALSE)
//' 
//' # Sample-sample correlations (individuals)  
//' sample_corr <- bdCorr_matrix(gene_expr, trans_x = TRUE)
//' 
//' # Cross-correlation examples
//' methylation <- matrix(rnorm(4000), nrow = 100, ncol = 40)  # 100 samples × 40 CpGs
//' 
//' # Variables vs variables (genes vs CpGs)
//' vars_vs_vars <- bdCorr_matrix(gene_expr, methylation, 
//'                              trans_x = FALSE, trans_y = FALSE)
//' 
//' # Samples vs variables (individuals vs CpGs)
//' samples_vs_vars <- bdCorr_matrix(gene_expr, methylation,
//'                                 trans_x = TRUE, trans_y = FALSE)
//' }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdCorr_matrix(Rcpp::RObject X,
                         Rcpp::Nullable<Rcpp::RObject> Y = R_NilValue,
                         Rcpp::Nullable<bool> trans_x = R_NilValue,
                         Rcpp::Nullable<bool> trans_y = R_NilValue,
                         Rcpp::Nullable<std::string> method = R_NilValue,
                         Rcpp::Nullable<bool> use_complete_obs = R_NilValue,
                         Rcpp::Nullable<bool> compute_pvalues = R_NilValue,
                         Rcpp::Nullable<int> threads = R_NilValue)
{    
         
     try {

         bool btrans_x = trans_x.isNotNull() ? Rcpp::as<bool>(trans_x.get()) : false;
         bool btrans_y = trans_y.isNotNull() ? Rcpp::as<bool>(trans_y.get()) : false;
         bool buse_complete_obs = use_complete_obs.isNotNull() ? Rcpp::as<bool>(use_complete_obs.get()) : true;
         bool bcompute_pvalues = compute_pvalues.isNotNull() ? Rcpp::as<bool>(compute_pvalues.get()) : true;
         std::string strmethod = method.isNotNull() ? Rcpp::as<std::string>(method.get()) : "pearson";
         
         // Validate method parameter
         if (strmethod != "pearson" && strmethod != "spearman") {
             stop("Method must be either 'pearson' or 'spearman'");
         }
         
         // Convert X to Eigen matrix
         Eigen::MatrixXd eigen_X = Rcpp::as<Eigen::MatrixXd>(X);
         
         // Determine correlation type based on Y parameter
         bool is_cross_correlation = (Y != R_NilValue);
         
         // // Convert threads parameter
         // Nullable<int> num_threads = (threads == -1) ? R_NilValue : wrap(threads);
         
         if (is_cross_correlation) {
             // Cross-correlation cor(X,Y)
             // NumericMatrix Y_matrix = Rcpp::as<Rcpp::NumericMatrix>(Y);
             // Convert Y to Eigen matrix
             Eigen::MatrixXd eigen_Y = as<Eigen::MatrixXd>(Y);
             
             
             // Validate dimensions based on transpose configuration
             if (btrans_x && btrans_y) {
                 // cor(t(X), t(Y)) - check X.cols == Y.cols (will become rows after transpose)
                 if (eigen_X.cols() != eigen_Y.cols()) {
                     stop("For cor(t(X), t(Y)): X and Y must have same number of columns (observations)");
                 }
             } else if (btrans_x && !btrans_y) {
                 // cor(t(X), Y) - check X.cols == Y.rows
                 if (eigen_X.cols() != eigen_Y.rows()) {
                     stop("For cor(t(X), Y): X columns must equal Y rows");
                 }
             } else if (!btrans_x && btrans_y) {
                 // cor(X, t(Y)) - check X.rows == Y.cols  
                 if (eigen_X.rows() != eigen_Y.cols()) {
                     stop("For cor(X, t(Y)): X rows must equal Y columns");
                 }
             } else {
                 // cor(X, Y) - check X.rows == Y.rows
                 if (eigen_X.rows() != eigen_Y.rows()) {
                     stop("For cor(X, Y): matrices must have same number of rows (observations)");
                 }
             }
             
             
             // // Rcpp::Rcout << "Computing cross-correlation cor(X,Y) for matrices " 
             // //             << X.nrow() << "x" << X.ncol() << " and " 
             // //             << Y_matrix.nrow() << "x" << Y_matrix.ncol() << std::endl;
             // 
             // // Validate matrix dimensions
             // if ( eigen_X.rows() != eigen_Y.rows()) {
             //     stop("Matrices must have the same number of rows (observations)");
             // }
             
             // Compute cross-correlation
             corr_result result = RcppbdCorr_matrix_cross(eigen_X, eigen_Y, strmethod, buse_complete_obs, bcompute_pvalues, btrans_x, btrans_y, threads);
             
             if (!result.bcomputed) {
                 stop("Cross-correlation computation failed");
             }
             
             // Prepare return list for cross-correlation
             return List::create(
                 Named("correlation") = wrap(result.correlation_matrix),
                 Named("method") = result.method,
                 Named("correlation_type") = "cross",
                 Named("trans_x") = result.trans_x,
                 Named("trans_y") = result.trans_y,
                 Named("n_observations") = result.n_obs,
                 Named("n_variables_x") = result.n_vars_x,
                 Named("n_variables_y") = result.n_vars_y,
                 Named("use_complete_obs") = use_complete_obs,
                 Named("pvalues") = result.has_pvalues && compute_pvalues ? wrap(result.pvalues) : R_NilValue,
                 Named("has_pvalues") = result.has_pvalues && compute_pvalues
             );
             
         } else {
             // // Single matrix correlation cor(X)
             // Rcpp::Rcout << "Computing single matrix correlation cor(X) for matrix " 
             //             << X.nrow() << "x" << X.ncol() << std::endl;
             
             // Compute single matrix correlation
             corr_result result = RcppbdCorr_matrix_single(eigen_X, strmethod, buse_complete_obs, bcompute_pvalues, btrans_x, threads);
             
             if (!result.bcomputed) {
                 stop("Correlation computation failed");
             }
             
             // Prepare return list for single matrix correlation
             return List::create(
                 Named("correlation") = wrap(result.correlation_matrix),
                 Named("method") = result.method,
                 Named("correlation_type") = "single",
                 Named("transposed") = result.trans_x,
                 Named("n_observations") = result.n_obs,
                 Named("n_variables") = result.n_vars_x,
                 Named("use_complete_obs") = use_complete_obs,
                 Named("pvalues") = result.has_pvalues && compute_pvalues ? wrap(result.pvalues) : R_NilValue,
                 Named("has_pvalues") = result.has_pvalues && compute_pvalues
             );
         }
         
     } catch(std::exception &ex) {
         forward_exception_to_r(ex);
     } catch(...) {
         ::Rf_error("C++ exception bdCorr_matrix (unknown reason)");
     }
     
     return List::create(
         Named("correlation") = R_NilValue,
         Named("method") = R_NilValue);
 }


// 
// //' @title Compute correlation matrix for in-memory matrices (single matrix only)
// //' @description Compute Pearson or Spearman correlation matrix for a single matrix that fits in memory.
// //' This is a specialized function for single matrix correlation cor(X).
// //' 
// //' @param X Numeric matrix (observations in rows, variables in columns)
// //' @param method Character string indicating correlation method ("pearson" or "spearman")
// //' @param use_complete_obs Logical, whether to use only complete observations (default: TRUE)
// //' @param compute_pvalues Logical, whether to compute p-values for correlations (default: TRUE)
// //' @param threads Integer, number of threads for parallel computation (optional, default: -1 for auto)
// //' 
// //' @return A list containing correlation results for single matrix
// //' 
// //' @examples
// //' \dontrun{
// //' # Create sample data
// //' set.seed(123)
// //' X <- matrix(rnorm(1000), ncol = 10)
// //' result <- bdCorr_matrix_single(X, method = "pearson")
// //' }
// //' 
// //' @export
//  // [[Rcpp::export]]
//  List bdCorr_matrix_single(NumericMatrix X, std::string method = "pearson",
//                            bool use_complete_obs = true, bool compute_pvalues = true,
//                            int threads = -1) {
//      
//      try {
//          // Validate method parameter
//          if (method != "pearson" && method != "spearman") {
//              stop("Method must be either 'pearson' or 'spearman'");
//          }
//          
//          // Convert to Eigen matrix
//          Eigen::MatrixXd eigen_X = as<Eigen::MatrixXd>(X);
//          
//          // Convert threads parameter
//          Nullable<int> num_threads = (threads == -1) ? R_NilValue : wrap(threads);
//          
//          // Compute correlation with threading support
//          corr_result result = RcppbdCorr_matrix_single(eigen_X, method, use_complete_obs, compute_pvalues, num_threads);
//          
//          if (!result.bcomputed) {
//              stop("Correlation computation failed");
//          }
//          
//          // Prepare return list
//          return List::create(
//              Named("correlation") = wrap(result.correlation_matrix),
//              Named("method") = result.method,
//              Named("correlation_type") = "single",
//              Named("n_observations") = result.n_obs,
//              Named("n_variables") = (int)result.correlation_matrix.rows(),
//              Named("use_complete_obs") = use_complete_obs,
//              Named("pvalues") = result.has_pvalues && compute_pvalues ? wrap(result.pvalues) : R_NilValue,
//              Named("has_pvalues") = result.has_pvalues && compute_pvalues
//          );
//          
//      } catch(std::exception &ex) {
//          forward_exception_to_r(ex);
//      } catch(...) {
//          ::Rf_error("C++ exception bdCorr_matrix_single (unknown reason)");
//      }
//  }
// 
// //' @title Compute cross-correlation matrix for in-memory matrices
// //' @description Compute Pearson or Spearman cross-correlation matrix between two matrices 
// //' that fit in memory. This is a specialized function for cross-correlation cor(X,Y).
// //' 
// //' @param X First numeric matrix (observations in rows, variables in columns)
// //' @param Y Second numeric matrix (observations in rows, variables in columns)
// //' @param method Character string indicating correlation method ("pearson" or "spearman")
// //' @param use_complete_obs Logical, whether to use only complete observations (default: TRUE)
// //' @param compute_pvalues Logical, whether to compute p-values for correlations (default: TRUE)
// //' @param threads Integer, number of threads for parallel computation (optional, default: -1 for auto)
// //' 
// //' @return A list containing cross-correlation results
// //' 
// //' @examples
// //' \dontrun{
// //' # Create sample data with different number of variables
// //' set.seed(123)
// //' X <- matrix(rnorm(1000), nrow = 100, ncol = 10)
// //' Y <- matrix(rnorm(1500), nrow = 100, ncol = 15)
// //' result <- bdCorr_matrix_cross(X, Y, method = "pearson")
// //' }
// //' 
// //' @export
//  // [[Rcpp::export]]
//  List bdCorr_matrix_cross(NumericMatrix X, NumericMatrix Y, std::string method = "pearson",
//                           bool use_complete_obs = true, bool compute_pvalues = true,
//                           int threads = -1) {
//      
//      try {
//          // Validate method parameter
//          if (method != "pearson" && method != "spearman") {
//              stop("Method must be either 'pearson' or 'spearman'");
//          }
//          
//          // Validate matrix dimensions
//          if (X.nrow() != Y.nrow()) {
//              stop("Matrices must have the same number of rows (observations)");
//          }
//          
//          // Convert to Eigen matrices
//          Eigen::MatrixXd eigen_X = as<Eigen::MatrixXd>(X);
//          Eigen::MatrixXd eigen_Y = as<Eigen::MatrixXd>(Y);
//          
//          // Convert threads parameter
//          Nullable<int> num_threads = (threads == -1) ? R_NilValue : wrap(threads);
//          
//          // Compute cross-correlation with threading support
//          corr_result result = RcppbdCorr_matrix_cross(eigen_X, eigen_Y, method, use_complete_obs, compute_pvalues, num_threads);
//          
//          if (!result.bcomputed) {
//              stop("Cross-correlation computation failed");
//          }
//          
//          // Prepare return list
//          return List::create(
//              Named("correlation") = wrap(result.correlation_matrix),
//              Named("method") = result.method,
//              Named("correlation_type") = "cross",
//              Named("n_observations") = result.n_obs,
//              Named("n_variables_x") = result.n_vars_x,
//              Named("n_variables_y") = result.n_vars_y,
//              Named("use_complete_obs") = use_complete_obs,
//              Named("pvalues") = result.has_pvalues && compute_pvalues ? wrap(result.pvalues) : R_NilValue,
//              Named("has_pvalues") = result.has_pvalues && compute_pvalues
//          );
//          
//      } catch(std::exception &ex) {
//          forward_exception_to_r(ex);
//      } catch(...) {
//          ::Rf_error("C++ exception bdCorr_matrix_cross (unknown reason)");
//      }
//  }
