#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixSvd.hpp"

/**
 * @file hdf5_SVD.cpp
 * @brief Implementation of Singular Value Decomposition (SVD) for HDF5-stored matrices
 * @details This file contains implementations for computing SVD of large matrices 
 * stored in HDF5 files using block-based and incremental algorithms. The implementation
 * supports both full and block-based computation methods, with options for data
 * centering, scaling, and parallel processing.
 */

/**
 * @brief Computes SVD of a matrix stored in an HDF5 file
 * 
 * @details Performs SVD of a matrix A into a product A = UDV' where:
 * - U contains the left singular vectors
 * - D contains the singular values
 * - V contains the right singular vectors
 * 
 * The function supports:
 * - Both full and block-based computation methods
 * - Data centering and scaling
 * - Parallel computation
 * - Rank approximation through threshold
 * 
 * @param filename Path to HDF5 file containing input matrix
 * @param group Path to group containing input dataset
 * @param dataset Name of input dataset
 * @param k Number of local SVDs to concatenate at each level
 * @param q Number of levels for incremental computation
 * @param bcenter Whether to center the data
 * @param bscale Whether to scale the data
 * @param rankthreshold Threshold for determining matrix rank
 * @param overwrite Whether to overwrite existing results
 * @param method Computation method ("auto", "blocks", or "full")
 * @param threads Number of threads for parallel computation
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::GroupIException if there are HDF5 group operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */
//' Singular Value Decomposition for HDF5-Stored Matrices
//'
//' @description
//' Computes the Singular Value Decomposition (SVD) of a large matrix stored in an HDF5 file.
//' The SVD decomposes a matrix A into a product A = UDV' where U and V are orthogonal
//' matrices and D is a diagonal matrix containing the singular values.
//' 
//' @details
//' This function implements a block-based SVD algorithm suitable for large matrices
//' that may not fit in memory. Key features include:
//' * Automatic method selection based on matrix size
//' * Block-based computation for large matrices
//' * Data centering and scaling options
//' * Parallel processing support
//' * Rank approximation through threshold
//' * Memory-efficient incremental algorithm
//'
//' The implementation uses an incremental algorithm with two key parameters:
//' * k: number of local SVDs to concatenate at each level
//' * q: number of levels in the computation
//'
//' @param filename Character string. Path to the HDF5 file containing the input matrix.
//' @param group Character string. Path to the group containing the input dataset.
//' @param dataset Character string. Name of the input dataset to decompose.
//' @param k Integer. Number of local SVDs to concatenate at each level (default = 2).
//'   Controls the trade-off between memory usage and computation speed.
//' @param q Integer. Number of levels for SVD computation (default = 1).
//'   Higher values can improve accuracy but increase computation time.
//' @param bcenter Logical. If TRUE (default), centers the data by subtracting column means.
//' @param bscale Logical. If TRUE (default), scales the centered columns by their
//'   standard deviations or root mean square.
//' @param rankthreshold Numeric. Threshold for determining matrix rank (default = 0).
//'   Must be between 0 and 0.1. Used to approximate rank for nearly singular matrices.
//' @param overwrite Logical. If TRUE, allows overwriting existing results.
//' @param method Character string. Computation method:
//'   * "auto": Automatically selects between "full" and "blocks" based on matrix size
//'   * "blocks": Uses block-based computation (recommended for large matrices)
//'   * "full": Performs direct computation without partitioning
//' @param threads Integer. Number of threads for parallel computation.
//'
//' @return A list with the following elements:
//' \describe{
//'   \item{fn}{Path to the HDF5 file}
//'   \item{ds_d}{Path to the dataset containing singular values}
//'   \item{ds_u}{Path to the dataset containing left singular vectors}
//'   \item{ds_v}{Path to the dataset containing right singular vectors}
//' }
//'
//' @examples
//' \dontrun{
//' # Create a sample large matrix in HDF5
//'
//' library(BigDataStatMeth)
//' library(rhdf5)
//' 
//' # Create a sample large matrix in HDF5
//' A <- matrix(rnorm(10000), 1000, 10)
//' 
//' fn <- "test_temp.hdf5"
//' bdCreate_hdf5_matrix(filename = fn, object = A, group = "data", dataset = "matrix")
//'
//' # Compute SVD with default parameters
//' res <- bdSVD_hdf5(fn, "data", "matrix")
//'
//' # Compute SVD with custom parameters
//' res <- bdSVD_hdf5(fn, "data", "matrix",
//'            k = 4, q = 2,
//'            bcenter = TRUE, bscale = TRUE,
//'            method = "blocks",
//'            threads = 4)
//' 
//' # list contents
//' h5ls(res$fn)
//' 
//' # Extract the result from HDF5 (d)
//' result_d_hdf5 <- h5read(res$fn, res$ds_d)
//' result_d_hdf5
//' 
//' # Compute the same SVD in R
//' result_d_r <- svd(A)$d
//' result_d_r
//' 
//' # Compare both results (should be TRUE)
//' all.equal(result_d_hdf5, result_d_r)
//' 
//' # Remove file
//' if (file.exists(fn)) {
//'   file.remove(fn)
//' }
//' 
//' }
//'
//' @references
//' * Halko, N., Martinsson, P. G., & Tropp, J. A. (2011). Finding structure with randomness:
//'   Probabilistic algorithms for constructing approximate matrix decompositions.
//'   SIAM Review, 53(2), 217-288.
//'
//' @seealso
//' * \code{\link{bdPCA_hdf5}} for Principal Component Analysis
//' * \code{\link{bdQR_hdf5}} for QR decomposition
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdSVD_hdf5 ( Rcpp::RObject filename, Rcpp::Nullable<Rcpp::CharacterVector> group = R_NilValue,
                       Rcpp::Nullable<Rcpp::CharacterVector> dataset = R_NilValue,
                       Rcpp::Nullable<int> k=2, Rcpp::Nullable<int> q=1,
                       Rcpp::Nullable<bool> bcenter=true, Rcpp::Nullable<bool> bscale=true,
                       Rcpp::Nullable<double> rankthreshold = 0.0,
                       Rcpp::Nullable<bool> overwrite = R_NilValue,
                       Rcpp::Nullable<Rcpp::CharacterVector> method = R_NilValue,
                       Rcpp::Nullable<int> threads = R_NilValue)
{
 
     std::string str_filename;
     double dthreshold;

     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds_d") = "",
                                                Rcpp::Named("ds_u") = "",
                                                Rcpp::Named("ds_v") = "");
     
     try {
         
         H5::Exception::dontPrint();
         
         int ks, qs, nvs = 0;
         bool bcent, bscal, bforce, bRowMajor = false; //, bbyblocks = true;
         Rcpp::CharacterVector strgroup, strdataset;
         
         if(k.isNull())  ks = 2 ;
         else    ks = Rcpp::as<int>(k);
         
         if(q.isNull())  qs = 1 ;
         else    qs = Rcpp::as<int>(q);
         
         if(bcenter.isNull())  bcent = true ;
         else    bcent = Rcpp::as<bool>(bcenter);
         
         if(bscale.isNull())  bscal = true ;
         else    bscal = Rcpp::as<bool>(bscale);
         
         if(overwrite.isNull())  bforce = false ;
         else    bforce = Rcpp::as<bool>(overwrite);
         
         if(group.isNull())  strgroup = "" ;
         else    strgroup = Rcpp::as<std::string>(group);
         
         if(dataset.isNull())  strdataset = "";
         else    strdataset = Rcpp::as<std::string>(dataset);
         
         if(Rcpp::is<Rcpp::CharacterVector>(filename)) {
             str_filename = Rcpp::as<std::string>(filename);
         } else {
             Rcpp::Rcout<< "File name must be character string";
             // return Rcpp::List::create(Rcpp::Named("file") = "");
             return lst_return;
         }
         
         if(rankthreshold.isNull()) {  
             dthreshold = 0 ;
         } else {
             if( Rcpp::as<double>(rankthreshold) > 0.1 ) {
                 Rcpp::Rcout<< "Threshold to big, please set threshold with value lower than 0.1";
                 return lst_return;
                 // return Rcpp::List::create(Rcpp::Named("file") = str_filename);
             } else if( Rcpp::as<double>(rankthreshold) < 0 ) {
                 Rcpp::Rcout<< "Threshold must be a positive value near zero";
                 return lst_return;
                 // return Rcpp::List::create(Rcpp::Named("file") = str_filename);
             } else {
                 dthreshold = Rcpp::as<double>(rankthreshold);
             }
         }
         
        // retsvd = BigDataStatMeth::RcppbdSVD_hdf5( filename, Rcpp::as<std::string>(strgroup), Rcpp::as<std::string>(strdataset), ks, qs, nvs, bcent, bscal, dthreshold, threads );
        BigDataStatMeth::RcppbdSVD_hdf5( str_filename, Rcpp::as<std::string>(strgroup), Rcpp::as<std::string>(strdataset), ks, qs, nvs, bcent, bscal, dthreshold, bforce, bRowMajor, method, threads );

        lst_return["fn"] = str_filename;
        // lst_return["ds_d"] = Rcpp::as<std::string>(strgroup) + "/" + Rcpp::as<std::string>(strdataset) + "/d";
        // lst_return["ds_u"] = Rcpp::as<std::string>(strgroup) + "/" + Rcpp::as<std::string>(strdataset) + "/u";
        // lst_return["ds_v"] = Rcpp::as<std::string>(strgroup) + "/" + Rcpp::as<std::string>(strdataset) + "/v";
         
         lst_return["ds_d"] = "SVD/" + Rcpp::as<std::string>(strdataset) + "/d";
         lst_return["ds_u"] = "SVD/" + Rcpp::as<std::string>(strdataset) + "/u";
         lst_return["ds_v"] = "SVD/" + Rcpp::as<std::string>(strdataset) + "/v";
         
     } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
         Rcpp::Rcerr<<"\nc++ exception bdSVD_hdf5 (File IException)";
         // return Rcpp::List::create(Rcpp::Named("file") = R_NilValue);
     } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
         Rcpp::Rcerr<<"\nc++ exception bdSVD_hdf5 (Group IException)";
         // return Rcpp::List::create(Rcpp::Named("file") = R_NilValue);
     } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
         Rcpp::Rcerr<<"\nc++ exception bdSVD_hdf5 (DataSet IException)";
         // return Rcpp::List::create(Rcpp::Named("file") = R_NilValue);
     } catch(std::exception& ex) {
         Rcpp::Rcerr<<"\nc++ exception bdSVD_hdf5" << ex.what();
         // return Rcpp::List::create(Rcpp::Named("file") = R_NilValue);
     } catch (...) {
         Rcpp::Rcerr<<"\nC++ exception bdSVD_hdf5 (unknown reason)";
         // return Rcpp::List::create(Rcpp::Named("file") = R_NilValue);
     }
     
     // return Rcpp::List::create(Rcpp::Named("file") = str_filename);
     return lst_return;
 
}

