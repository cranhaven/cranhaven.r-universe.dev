/**
 * @file hdf5_matrixCorrelation.cpp
 * @brief Unified R interface for correlation computation in BigDataStatMeth
 * 
 * This file provides a single R-callable function for computing correlations of matrices
 * stored in HDF5 format, automatically detecting whether to compute cor(X) or cor(X,Y)
 * based on the provided parameters.
 * 
 * @author BigDataStatMeth Development Team
 * @date 2025
 * @version 1.0
 */

#include "BigDataStatMeth.hpp"

using namespace Rcpp;
using namespace RcppEigen;
using namespace BigDataStatMeth;

//' @title Compute correlation matrix for matrices stored in HDF5 format
//' @description This function computes Pearson or Spearman correlation matrix 
//' for matrices 
//' stored in HDF5 format. It automatically detects whether to compute:
//' \itemize{
//'   \item Single matrix correlation cor(X) - when only dataset_x is provided
//'   \item Cross-matrix correlation cor(X,Y) - when both dataset_x and 
//'   dataset_y are provided
//' }
//' It automatically selects between direct computation for small matrices and 
//' block-wise processing for large matrices to optimize memory usage and 
//' performance.
//' 
//' Correlation types supported:
//' \itemize{
//'   \item Single matrix: cor(X) when only dataset_x provided
//'   \item Single matrix transposed: cor(t(X)) when trans_x=TRUE
//'   \item Cross-correlation: cor(X,Y) when both datasets provided
//'   \item Cross with transpose: cor(t(X),Y), cor(X,t(Y)), cor(t(X),t(Y))
//' }
//' 
//' For omics data analysis:
//' \itemize{
//'   \item trans_x=FALSE, trans_y=FALSE: Variables vs Variables 
//'   (genes vs genes, CpGs vs CpGs)
//'   \item trans_x=TRUE, trans_y=FALSE: Samples vs Variables 
//'   (individuals vs genes)
//'   \item trans_x=FALSE, trans_y=TRUE: Variables vs Samples 
//'   (genes vs individuals)
//'   \item trans_x=TRUE, trans_y=TRUE: Samples vs Samples 
//'   (individuals vs individuals) - optimized to cor(X,Y)
//' }
//' 
//' @param filename_x Character string with the path to the HDF5 file containing 
//' matrix X
//' @param group_x Character string indicating the group containing matrix X
//' @param dataset_x Character string indicating the dataset name of matrix X
//' @param filename_y Character string with the path to the HDF5 file 
//' containing matrix Y (optional, default: "")
//' @param group_y Character string indicating the group containing matrix Y 
//' (optional, default: "")
//' @param dataset_y Character string indicating the dataset name of matrix Y 
//' (optional, default: "")
//' @param trans_x Logical, whether to transpose matrix X (default: FALSE)
//' @param trans_y Logical, whether to transpose matrix Y (default: FALSE, 
//' ignored for single matrix)
//' @param method Character string indicating correlation method 
//' ("pearson" or "spearman", default: "pearson")
//' @param use_complete_obs Logical, whether to use only complete observations 
//' (default: TRUE)
//' @param compute_pvalues Logical, whether to compute p-values for correlations 
//' (default: TRUE)
//' @param block_size Integer, block size for large matrix processing 
//' (default: 1000)
//' @param overwrite Logical, whether to overwrite existing results 
//' (default: FALSE)
//' @param output_filename Character string, output HDF5 file 
//' (default: same as filename_x)
//' @param output_group Character string, custom output group name 
//' (default: auto-generated)
//' @param output_dataset_corr Character string, custom correlation dataset 
//' name (default: "correlation")
//' @param output_dataset_pval Character string, custom p-values dataset 
//' name (default: "pvalues")
//' @param threads Integer, number of threads for parallel computation 
//' (optional, default: auto)
//' 
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the correlation matrix (group/dataset)}
//' }
//' 
//' @examples
//' \dontrun{
//' # Backward compatible - existing code works unchanged
//' result_original <- bdCorr_hdf5("data.h5", "expression", "genes")
//' 
//' # New transpose functionality
//' # Gene-gene correlations (variables)
//' gene_corr <- bdCorr_hdf5("omics.h5", "expression", "genes", trans_x = FALSE)
//' 
//' # Sample-sample correlations (individuals) 
//' sample_corr <- bdCorr_hdf5("omics.h5", "expression", "genes", trans_x = TRUE)
//' 
//' # Cross-correlation: genes vs methylation sites (variables vs variables)
//' cross_vars <- bdCorr_hdf5("omics.h5", "expression", "genes", 
//'                          "omics.h5", "methylation", "cpg_sites",
//'                          trans_x = FALSE, trans_y = FALSE)
//' 
//' # Cross-correlation: samples vs methylation sites (samples vs variables)
//' samples_vs_cpg <- bdCorr_hdf5("omics.h5", "expression", "genes",
//'                              "omics.h5", "methylation", "cpg_sites", 
//'                              trans_x = TRUE, trans_y = FALSE)
//' }
//' 
//' @export
 // [[Rcpp::export]]
Rcpp::List bdCorr_hdf5(std::string filename_x, std::string group_x, std::string dataset_x,
                  std::string filename_y = "", std::string group_y = "", std::string dataset_y = "",
                  bool trans_x = false, bool trans_y = false,
                  std::string method = "pearson", bool use_complete_obs = true,
                  bool compute_pvalues = true, int block_size = 1000,
                  bool overwrite = false, std::string output_filename = "",
                  std::string output_group = "", std::string output_dataset_corr = "",
                  std::string output_dataset_pval = "", int threads = -1) {
     
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
     try {
         
         H5::Exception::dontPrint();
         
         // Validate method parameter
         if (method != "pearson" && method != "spearman") {
             stop("Method must be either 'pearson' or 'spearman'");
         }
         
         // Validate block_size
         if (block_size <= 0) {
             stop("block_size must be a positive integer");
         }
         
         // Determine correlation type based on provided parameters
         bool is_cross_correlation = !dataset_y.empty();
         
         // Check if second dataset parameters are provided for cross-correlation
         if (is_cross_correlation) {
             if (group_y.empty()) {
                 stop("group_y must be provided when dataset_y is specified for cross-correlation");
             }
             
             // Set default filename_y if not provided
             if (filename_y.empty()) {
                 filename_y = filename_x;
             }
         } else {
             // Ensure Y parameters are not partially provided
             if (!filename_y.empty() || !group_y.empty()) {
                 stop("dataset_y must be provided when filename_y or group_y are specified");
             }
         }
         
         // Set default output filename
         if (output_filename.empty()) {
             output_filename = filename_x;
         }
         
         // Convert string parameters to Nullable types for internal functions
         Nullable<CharacterVector> out_group = output_group.empty() ? R_NilValue :  wrap(output_group);
         Nullable<CharacterVector> out_dataset_corr = output_dataset_corr.empty() ? R_NilValue : wrap(output_dataset_corr);
         Nullable<CharacterVector> out_dataset_pval = output_dataset_pval.empty() ? R_NilValue : wrap(output_dataset_pval);
         Nullable<int> num_threads = (threads == -1) ? R_NilValue : wrap(threads);
         
         // Log correlation type
         if (is_cross_correlation) {
             // Rcpp::Rcout << "Computing cross-correlation cor(X,Y) between datasets '"
             //             << dataset_x << "' and '" << dataset_y << "'" << std::endl;
             
             // Call cross-correlation function
             return RcppbdCorr_hdf5_cross(
                 filename_x, group_x, dataset_x,
                 filename_y, group_y, dataset_y,
                 method, use_complete_obs, compute_pvalues, block_size, overwrite,
                 output_filename, out_group, out_dataset_corr, out_dataset_pval, 
                 !trans_x, !trans_y, num_threads
             );
             // return RcppbdCorr_hdf5_cross(
             //     filename_x, group_x, dataset_x,
             //     filename_y, group_y, dataset_y,
             //     method, use_complete_obs, compute_pvalues, block_size, overwrite,
             //     output_filename, out_group, out_dataset_corr, out_dataset_pval, num_threads
             // );
             
             
             
         } else {
             // Rcpp::Rcout << "Computing single matrix correlation cor(X) for dataset '" 
             //             << dataset_x << "'" << std::endl;
             
             // Call single matrix correlation function
             return RcppbdCorr_hdf5_single(
                 filename_x, group_x, dataset_x,
                 method, use_complete_obs, compute_pvalues, block_size, overwrite,
                 out_group, out_dataset_corr, out_dataset_pval, !trans_x, num_threads
             );
             // return RcppbdCorr_hdf5_single(
             //     filename_x, group_x, dataset_x,
             //     method, use_complete_obs, compute_pvalues, block_size, overwrite,
             //     out_group, out_dataset_corr, out_dataset_pval, num_threads
             // );
             
         }
         
     } catch(std::exception &ex) {
         forward_exception_to_r(ex);
         return(lst_return);
     } catch(...) {
         ::Rf_error("C++ exception bdCorr_hdf5 (unknown reason)");
     }
     return(lst_return);
 }
