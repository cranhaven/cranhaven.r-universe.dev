/**
 * @file hdf5_getMatrixProperties.cpp
 * @brief Implementation to check main matrix properties for HDF5-stored matrices
 * @details This file contains implementations to check whether a matrix stored 
 * in HDF5 is suitable for eigenvalue decomposition by verifying that it is 
 * square and optionally checking for symmetry (since eigendecomposition is 
 * most meaningful for symmetric matrices).
 */

#include <BigDataStatMeth.hpp>


//' Check Matrix Suitability for Eigenvalue Decomposition with Spectra
//'
//' @description
//' Checks whether a matrix stored in HDF5 format is suitable for eigenvalue decomposition
//' using Spectra. The function verifies that the matrix is square and optionally checks
//' for symmetry to recommend the best solver type.
//'
//' @param filename Character string. Path to the HDF5 file containing the matrix.
//' @param group Character string. Path to the group containing the dataset.
//' @param dataset Character string. Name of the dataset to check.
//' @param check_symmetry Logical. Whether to check if the matrix is symmetric (default = TRUE).
//' @param tolerance Numeric. Tolerance for symmetry checking (default = 1e-12).
//' @param sample_size Integer. Number of elements to sample for large matrices (default = 1000).
//'
//' @return A list with matrix properties and suitability assessment.
//'
//' @examples
//' \dontrun{
//' # Check matrix suitability
//' check_result <- bdEigen_check_matrix("data.h5", "matrices", "my_matrix")
//' 
//' if (check_result$suitable_for_eigen) {
//'   # Use appropriate solver based on recommendation
//'   if (check_result$recommended_solver == "symmetric") {
//'     result <- bdEigen_hdf5("data.h5", "matrices", "my_matrix", which = "LA")
//'   } else {
//'     result <- bdEigen_hdf5("data.h5", "matrices", "my_matrix", which = "LM")
//'   }
//' } else {
//'   cat("Matrix is not suitable for eigendecomposition\n")
//' }
//' }
//'
//' @export
 // [[Rcpp::export]]
 Rcpp::List bdCheckMatrix_hdf5(Rcpp::RObject filename,
                                 Rcpp::Nullable<Rcpp::CharacterVector> group = R_NilValue,
                                 Rcpp::Nullable<Rcpp::CharacterVector> dataset = R_NilValue,
                                 Rcpp::Nullable<bool> check_symmetry = R_NilValue,
                                 Rcpp::Nullable<double> tolerance = R_NilValue,
                                 Rcpp::Nullable<int> sample_size = R_NilValue) {
     
     std::string str_filename;
     Rcpp::CharacterVector strgroup, strdataset;
     bool bcheck_sym;
     double tol;
     int samp_size;
     
     try {
         
         H5::Exception::dontPrint();
         
         // Process parameters
         if (Rcpp::is<Rcpp::CharacterVector>(filename)) {
             str_filename = Rcpp::as<std::string>(filename);
         } else {
             Rcpp::stop("Filename must be a character string");
         }
         
         if (group.isNull()) strgroup = "";
         else strgroup = Rcpp::as<std::string>(group);
         
         if (dataset.isNull()) strdataset = "";
         else strdataset = Rcpp::as<std::string>(dataset);
         
         if (check_symmetry.isNull()) bcheck_sym = true;  // Default true
         else bcheck_sym = Rcpp::as<bool>(check_symmetry);
         
         if (tolerance.isNull()) tol = 1e-12;  // Default tolerance
         else tol = Rcpp::as<double>(tolerance);
         
         if (sample_size.isNull()) samp_size = 1000;  // Default sample size
         else samp_size = Rcpp::as<int>(sample_size);
         
         // Open dataset and check properties
         BigDataStatMeth::hdf5Dataset dsA(str_filename, Rcpp::as<std::string>(strgroup), 
                                          Rcpp::as<std::string>(strdataset), false);
         dsA.openDataset();
         
         hsize_t n_rows = dsA.nrows();
         hsize_t n_cols = dsA.ncols();
         
         bool is_square = (n_rows == n_cols);
         bool is_symmetric = true;
         double max_asymmetry = 0.0;
         std::string recommended_solver = "general";
         
         if (is_square && bcheck_sym && n_rows > 1) {
             
             // For large matrices, sample elements for symmetry check
             int check_size = std::min(samp_size, (int)n_rows);
             std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
             
             for (int i = 0; i < check_size && is_symmetric; ++i) {
                 for (int j = i + 1; j < check_size; ++j) {
                     
                     double val_ij, val_ji;
                     std::vector<double> temp_data(1);
                     
                     // Read element (i,j)
                     dsA.readDatasetBlock({(hsize_t)i, (hsize_t)j}, {1, 1}, stride, block, temp_data.data());
                     val_ij = temp_data[0];
                     
                     // Read element (j,i)
                     dsA.readDatasetBlock({(hsize_t)j, (hsize_t)i}, {1, 1}, stride, block, temp_data.data());
                     val_ji = temp_data[0];
                     
                     double asymmetry = std::abs(val_ij - val_ji);
                     max_asymmetry = std::max(max_asymmetry, asymmetry);
                     
                     if (asymmetry > tol) {
                         is_symmetric = false;
                     }
                 }
             }
             
             // Recommend solver based on symmetry
             if (is_symmetric) {
                 recommended_solver = "symmetric";
             }
         }
         
         bool suitable = is_square;  // Only requirement for Spectra is square matrix
         
         return Rcpp::List::create(
             Rcpp::Named("is_square") = is_square,
             Rcpp::Named("is_symmetric") = is_symmetric,
             Rcpp::Named("max_asymmetry") = max_asymmetry,
             Rcpp::Named("matrix_size") = Rcpp::IntegerVector::create(n_rows, n_cols),
             Rcpp::Named("suitable_for_eigen") = suitable,
             Rcpp::Named("symmetry_checked") = bcheck_sym,
             Rcpp::Named("recommended_solver") = recommended_solver,
             Rcpp::Named("recommended_which") = is_symmetric ? "LA" : "LM"
         );
         
     } catch (H5::FileIException& error) {
         Rcpp::stop("HDF5 file error in bdCheckMatrix_hdf5");
     } catch (H5::DataSetIException& error) {
         Rcpp::stop("HDF5 dataset error in bdCheckMatrix_hdf5");
     } catch (std::exception& ex) {
         Rcpp::stop("Error in bdCheckMatrix_hdf5: " + std::string(ex.what()));
     } catch (...) {
         Rcpp::stop("Unknown error in bdCheckMatrix_hdf5");
     }
 }
 
 
