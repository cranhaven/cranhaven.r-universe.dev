/**
 * @file hdf5_eigenDecomposition.cpp
 * @brief Implementation of Eigenvalue Decomposition for HDF5-stored matrices using Spectra
 * @details This file contains implementations for computing eigenvalue decomposition
 * of large matrices stored in HDF5 files using the Spectra library for consistent
 * results with RSpectra. Handles both symmetric and non-symmetric matrices.
 */

#include <BigDataStatMeth.hpp>

//' Eigenvalue Decomposition for HDF5-Stored Matrices using Spectra
//'
//' @description
//' Computes the eigenvalue decomposition of a large matrix stored in an HDF5 file using
//' the Spectra library. This provides consistent results with the RSpectra package and
//' can handle both symmetric and non-symmetric matrices.
//' 
//' @details
//' This function uses the Spectra library (same as RSpectra) for eigenvalue computation,
//' ensuring consistent results. Key features include:
//' * Automatic detection of symmetric vs non-symmetric matrices
//' * Support for both real and complex eigenvalues/eigenvectors
//' * Memory-efficient block-based processing for large matrices
//' * Parallel processing support
//' * Various eigenvalue selection criteria
//' * Consistent interface with RSpectra::eigs()
//'
//' The implementation automatically:
//' * Detects matrix symmetry and uses appropriate solver (SymEigsSolver vs GenEigsSolver)
//' * Handles complex eigenvalues for non-symmetric matrices
//' * Saves imaginary parts separately when non-zero
//' * Provides the same results as RSpectra::eigs() function
//'
//' @param filename Character string. Path to the HDF5 file containing the input matrix.
//' @param group Character string. Path to the group containing the input dataset.
//' @param dataset Character string. Name of the input dataset to decompose.
//' @param k Integer. Number of eigenvalues to compute (default = 6, following Spectra convention).
//' @param which Character string. Which eigenvalues to compute (default = "LM"):
//'   * "LM": Largest magnitude
//'   * "SM": Smallest magnitude  
//'   * "LR": Largest real part (non-symmetric matrices)
//'   * "SR": Smallest real part (non-symmetric matrices)
//'   * "LI": Largest imaginary part (non-symmetric matrices)
//'   * "SI": Smallest imaginary part (non-symmetric matrices)
//'   * "LA": Largest algebraic (symmetric matrices)
//'   * "SA": Smallest algebraic (symmetric matrices)
//' @param ncv Integer. Number of Arnoldi vectors (default = 0, auto-selected as max(2*k+1, 20)).
//' @param bcenter Logical. If TRUE, centers the data by subtracting column means (default = FALSE).
//' @param bscale Logical. If TRUE, scales the centered columns by their standard deviations (default = FALSE).
//' @param tolerance Numeric. Convergence tolerance for Spectra algorithms (default = 1e-10).
//' @param max_iter Integer. Maximum number of iterations for Spectra algorithms (default = 1000).
//' @param compute_vectors Logical. If TRUE (default), computes both eigenvalues and eigenvectors.
//' @param overwrite Logical. If TRUE, allows overwriting existing results (default = FALSE).
//' @param threads Integer. Number of threads for parallel computation (default = NULL, uses available cores).
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{values}{Character string with the full dataset path to the eigenvalues (real part) (group/dataset)}
//'   \item{vectors}{Character string with the full dataset path to the eigenvectors (real part) (group/dataset)}
//'   \item{values_imag}{Character string with the full dataset path to the eigenvalues (imaginary part), or NULL if all eigenvalues are real}
//'   \item{vectors_imag}{Character string with the full dataset path to the eigenvectors (imaginary part), or NULL if all eigenvectors are real}
//'   \item{is_symmetric}{Logical indicating whether the matrix was detected as symmetric}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' library(rhdf5)
//' library(RSpectra)
//' 
//' # Create a sample matrix (can be non-symmetric)
//' set.seed(123)
//' A <- matrix(rnorm(2500), 50, 50)
//' 
//' fn <- "test_eigen.hdf5"
//' bdCreate_hdf5_matrix_file(filename = fn, object = A, group = "data", dataset = "matrix")
//'
//' # Compute eigendecomposition with BigDataStatMeth
//' res <- bdEigen_hdf5(fn, "data", "matrix", k = 6, which = "LM")
//'
//' # Compare with RSpectra (should give same results)
//' rspectra_result <- eigs(A, k = 6, which = "LM")
//' 
//' # Extract results from HDF5
//' eigenvals_bd <- h5read(res$fn, res$values)
//' eigenvecs_bd <- h5read(res$fn, res$vectors)
//' 
//' # Compare eigenvalues (should be identical)
//' all.equal(eigenvals_bd, Re(rspectra_result$values), tolerance = 1e-12)
//' 
//' # For non-symmetric matrices, check imaginary parts
//' if (!is.null(res$values_imag)) {
//'   eigenvals_imag <- h5read(res$fn, res$values_imag)
//'   all.equal(eigenvals_imag, Im(rspectra_result$values), tolerance = 1e-12)
//' }
//' 
//' # Remove file
//' if (file.exists(fn)) {
//'   file.remove(fn)
//' }
//' }
//'
//' @references
//' * Qiu, Y., & Mei, J. (2022). RSpectra: Solvers for Large-Scale Eigenvalue and SVD Problems.
//' * Li, R. (2021). Spectra: C++ Library For Large Scale Eigenvalue Problems.
//'
//' @seealso
//' * \code{\link{bdSVD_hdf5}} for Singular Value Decomposition
//' * \code{\link{bdPCA_hdf5}} for Principal Component Analysis
//' * \code{RSpectra::eigs} for the R equivalent function
//'
//' @export
 // [[Rcpp::export]]
 Rcpp::List bdEigen_hdf5(Rcpp::RObject filename, 
                         Rcpp::Nullable<Rcpp::CharacterVector> group = R_NilValue,
                         Rcpp::Nullable<Rcpp::CharacterVector> dataset = R_NilValue,
                         Rcpp::Nullable<int> k = R_NilValue,
                         Rcpp::Nullable<Rcpp::CharacterVector> which = R_NilValue,
                         Rcpp::Nullable<int> ncv = R_NilValue,
                         Rcpp::Nullable<bool> bcenter = R_NilValue, 
                         Rcpp::Nullable<bool> bscale = R_NilValue,
                         Rcpp::Nullable<double> tolerance = R_NilValue,
                         Rcpp::Nullable<int> max_iter = R_NilValue,
                         Rcpp::Nullable<bool> compute_vectors = R_NilValue,
                         Rcpp::Nullable<bool> overwrite = R_NilValue,
                         Rcpp::Nullable<int> threads = R_NilValue) {
     
     std::string str_filename;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("values") = "",
                                                Rcpp::Named("vectors") = "",
                                                Rcpp::Named("values_imag") = R_NilValue,
                                                Rcpp::Named("vectors_imag") = R_NilValue,
                                                Rcpp::Named("is_symmetric") = R_NilValue);
     
     BigDataStatMeth::hdf5File* objFile = nullptr;
     
     try {
         
         H5::Exception::dontPrint();
         
         int ks, max_iters, ncvs;
         bool bcent, bscal, bforce, bcompute_vecs;
         double tol;
         std::string which_str;
         Rcpp::CharacterVector strgroup, strdataset;
         
         // Process parameters with defaults
         if (k.isNull()) ks = 6;  // Spectra default
         else ks = Rcpp::as<int>(k);
         
         if (which.isNull()) which_str = "LM";  // Largest Magnitude default
         else which_str = Rcpp::as<std::string>(which);
         
         if (ncv.isNull()) ncvs = 0;  // Auto-select
         else ncvs = Rcpp::as<int>(ncv);
         
         if (bcenter.isNull()) bcent = false;
         else bcent = Rcpp::as<bool>(bcenter);
         
         if (bscale.isNull()) bscal = false;
         else bscal = Rcpp::as<bool>(bscale);
         
         if (overwrite.isNull()) bforce = false;
         else bforce = Rcpp::as<bool>(overwrite);
         
         if (compute_vectors.isNull()) bcompute_vecs = true;
         else bcompute_vecs = Rcpp::as<bool>(compute_vectors);
         
         if (tolerance.isNull()) tol = 1e-10;
         else tol = Rcpp::as<double>(tolerance);
         
         if (max_iter.isNull()) max_iters = 1000;
         else max_iters = Rcpp::as<int>(max_iter);
         
         if (group.isNull()) strgroup = "";
         else strgroup = Rcpp::as<std::string>(group);
         
         if (dataset.isNull()) strdataset = "";
         else strdataset = Rcpp::as<std::string>(dataset);
         
         // Validate filename
         if (Rcpp::is<Rcpp::CharacterVector>(filename)) {
             str_filename = Rcpp::as<std::string>(filename);
         } else {
             Rcpp::Rcout << "File name must be character string";
             return lst_return;
         }
         
         // Validate parameters
         if (ks < 1) {
             Rcpp::Rcout << "k must be positive";
             return lst_return;
         }
         
         if (tol < 0 || tol > 0.1) {
             Rcpp::Rcout << "Tolerance must be between 0 and 0.1";
             return lst_return;
         }
         
         if (max_iters < 1) {
             Rcpp::Rcout << "Maximum iterations must be positive";
             return lst_return;
         }
         
         // Validate 'which' parameter
         std::vector<std::string> valid_which = {"LM", "SM", "LR", "SR", "LI", "SI", "LA", "SA"};
         if (std::find(valid_which.begin(), valid_which.end(), which_str) == valid_which.end()) {
             Rcpp::Rcout << "Invalid 'which' parameter. Must be one of: LM, SM, LR, SR, LI, SI, LA, SA";
             return lst_return;
         }
         
         // Call the main eigendecomposition function with all parameters
         BigDataStatMeth::RcppbdEigen_hdf5(str_filename, 
                                           Rcpp::as<std::string>(strgroup), 
                                           Rcpp::as<std::string>(strdataset), 
                                           ks, which_str, ncvs, bcent, bscal, 
                                           tol, max_iters, bcompute_vecs, bforce, threads);
         
         // Set return values
         lst_return["fn"] = str_filename;
         lst_return["values"] = "EIGEN/" + Rcpp::as<std::string>(strdataset) + "/values";
         
         if (bcompute_vecs) {
             lst_return["vectors"] = "EIGEN/" + Rcpp::as<std::string>(strdataset) + "/vectors";
         }
         
         // Check if imaginary parts exist (for non-symmetric matrices)
         try {
             
             objFile = new BigDataStatMeth::hdf5File(str_filename, false);
             objFile->openFile("r");
             
             if( objFile->getFileptr()!=nullptr && BigDataStatMeth::exists_HDF5_element( objFile->getFileptr(), "EIGEN/" + Rcpp::as<std::string>(strdataset) + "/values_imag")) {
                 lst_return["values_imag"] = "EIGEN/" + Rcpp::as<std::string>(strdataset) + "/values_imag";
             }
             
             if (bcompute_vecs) {
                 if( objFile->getFileptr()!=nullptr && BigDataStatMeth::exists_HDF5_element( objFile->getFileptr(), "EIGEN/" + Rcpp::as<std::string>(strdataset) + "/vectors_imag")){
                     lst_return["vectors_imag"] = "EIGEN/" + Rcpp::as<std::string>(strdataset) + "/vectors_imag";
                 }
             }
             delete objFile; objFile = nullptr;
             
         } catch (...) {
             
             // imag datasets don't exist, matrix was symmetric
         }
         
         // Determine if matrix was symmetric based on existence of imaginary parts
         lst_return["is_symmetric"] = (lst_return["values_imag"] == R_NilValue);
         
     } catch (H5::FileIException& error) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "\nc++ exception bdEigen_hdf5 (File IException)";
     } catch (H5::GroupIException& error) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "\nc++ exception bdEigen_hdf5 (Group IException)";
     } catch (H5::DataSetIException& error) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "\nc++ exception bdEigen_hdf5 (DataSet IException)";
     } catch (std::exception& ex) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "\nc++ exception bdEigen_hdf5: " << ex.what();
     } catch (...) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "\nC++ exception bdEigen_hdf5 (unknown reason)";
     }
     
     return lst_return;
 }
