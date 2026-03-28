#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixInvCholesky.hpp"
// #include "hdf5Algebra/matrixTriangular.hpp"

/**
 * @file hdf5_CholeskyDec.cpp
 * @brief Implementation of Cholesky Decomposition for HDF5-stored matrices
 * @details This file contains implementations for computing the Cholesky decomposition
 * of large symmetric positive-definite matrices stored in HDF5 files. The implementation
 * supports both full matrix and triangular storage formats, with options for block-based
 * computation and parallel processing.
 */

/**
 * @brief Computes Cholesky decomposition of a matrix stored in an HDF5 file
 * 
 * @details Performs Cholesky decomposition of a symmetric positive-definite matrix A
 * into the product A = LL' where:
 * - L is a lower triangular matrix
 * - L' is the transpose of L
 * 
 * The implementation:
 * - Verifies matrix symmetry and positive-definiteness
 * - Supports block-based computation for large matrices
 * - Provides options for storage format (full or triangular)
 * - Utilizes parallel computation when available
 * 
 * @param filename Path to HDF5 file containing input matrix
 * @param group Path to group containing input dataset
 * @param dataset Name of input dataset
 * @param outdataset Name for output dataset
 * @param outgroup Optional group for output (defaults to input group)
 * @param fullMatrix Whether to store full matrix or just triangular part
 * @param overwrite Whether to overwrite existing results
 * @param threads Number of threads for parallel computation
 * @param elementsBlock Block size for block-based computation
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::GroupIException if there are HDF5 group operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */
//' Cholesky Decomposition for HDF5-Stored Matrices
//'
//' @description
//' Computes the Cholesky decomposition of a symmetric positive-definite matrix stored
//' in an HDF5 file. The Cholesky decomposition factors a matrix A into the product
//' A = LL' where L is a lower triangular matrix.
//' 
//' @details 
//' The Cholesky decomposition is a specialized factorization for symmetric 
//' positive-definite matrices that provides several advantages:
//' * More efficient than LU decomposition for symmetric positive-definite matrices
//' * Numerically stable
//' * Useful for solving linear systems and computing matrix inverses
//' * Important in statistical computing (e.g., for sampling from multivariate normal distributions)
//'
//' This implementation features:
//' * Block-based computation for large matrices
//' * Optional storage formats (full or triangular)
//' * Parallel processing support
//' * Memory-efficient block algorithm
//'
//' Mathematical Details:
//' For a symmetric positive-definite matrix A, the decomposition A = LL' has the following properties:
//' * L is lower triangular
//' * L has positive diagonal elements
//' * L is unique
//'
//' The elements of L are computed using:
//' \deqn{l_{ii} = \sqrt{a_{ii} - \sum_{k=1}^{i-1} l_{ik}^2}}
//' \deqn{l_{ji} = \frac{1}{l_{ii}}(a_{ji} - \sum_{k=1}^{i-1} l_{ik}l_{jk})}
//'
//' @param filename Character string. Path to the HDF5 file containing the input matrix.
//' @param group Character string. Path to the group containing the input dataset.
//' @param dataset Character string. Name of the input dataset to decompose.
//' @param outdataset Character string. Name for the output dataset.
//' @param outgroup Character string. Optional output group path. If not provided,
//'   results are stored in the input group.
//' @param fullMatrix Logical. If TRUE, stores the complete matrix. If FALSE (default),
//'   stores only the lower triangular part to save space.
//' @param overwrite Logical. If TRUE, allows overwriting existing results.
//' @param threads Integer. Number of threads for parallel computation.
//' @param elementsBlock Integer. Maximum number of elements to process in each block
//'   (default = 100,000). For matrices larger than 5000x5000, automatically adjusted
//'   to number of rows or columns * 2.
//'
//' @return A list containing the location of the Cholesky decomposition result:
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the result}
//'     \item{ds}{Character string. Full dataset path to the Cholesky decomposition result within the HDF5 file}
//'   }
//'   
//' \describe{
//'   \item{L}{The lower triangular Cholesky factor}
//' }
//' 
//' @examples
//' \dontrun{
//' library(rhdf5)
//' 
//' # Create a symmetric positive-definite matrix
//' set.seed(1234)
//' X <- matrix(rnorm(100), 10, 10)
//' A <- crossprod(X)  # A = X'X is symmetric positive-definite
//'     
//' # Save to HDF5
//' h5createFile("matrix.h5")
//' h5write(A, "matrix.h5", "data/matrix")
//'         
//' # Compute Cholesky decomposition
//' bdCholesky_hdf5("matrix.h5", "data", "matrix",
//'                 outdataset = "chol",
//'                 outgroup = "decompositions",
//'                 fullMatrix = FALSE)
//'        
//' # Verify the decomposition
//' L <- h5read("matrix.h5", "decompositions/chol")
//' max(abs(A - L %*% t(L)))  # Should be very small
//' }
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * Higham, N. J. (2009). Cholesky factorization.
//'   Wiley Interdisciplinary Reviews: Computational Statistics, 1(2), 251-254.
//'        
//' @seealso
//' * \code{\link{bdInvCholesky_hdf5}} for computing inverse using Cholesky decomposition
//' * \code{\link{bdSolve_hdf5}} for solving linear systems
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdCholesky_hdf5(std::string filename, std::string group, std::string dataset,
                     std::string outdataset,
                          Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                          Rcpp::Nullable<bool> fullMatrix = R_NilValue, 
                          Rcpp::Nullable<bool> overwrite = R_NilValue,
                          Rcpp::Nullable<int> threads = R_NilValue,
                          Rcpp::Nullable<long> elementsBlock = 1000000)
{
     
     
     
     BigDataStatMeth::hdf5Dataset* dsA = nullptr;
     BigDataStatMeth::hdf5DatasetInternal* dstmp = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
     try
     {
         H5::Exception::dontPrint();
         
         long dElementsBlock;
         std::string strOutgroup, strIndataset, 
         strOutdataset, strOutdataset_tmp;
         int nrows = 0, ncols = 0;
         
         
         if(outgroup.isNull()) { strOutgroup = group; } 
         else {   strOutgroup = Rcpp::as<std::string>(outgroup); }
         
         if(elementsBlock.isNull()) { dElementsBlock = MAXELEMSINBLOCK; } 
         else { dElementsBlock = Rcpp::as<long>(elementsBlock); }
         
         
         strIndataset = group + "/" + dataset;
         strOutdataset = strOutgroup + "/" + outdataset;
         strOutdataset_tmp = "tmp/tmp_L";
         
         dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
         dsA->openDataset();
         
         if( dsA->getDatasetptr() != nullptr) { 

             nrows = dsA->nrows();
             ncols = dsA->ncols();
             
             if(nrows == ncols) {
                 dstmp = new BigDataStatMeth::hdf5DatasetInternal(filename, strOutdataset, true);
                 dstmp->createDataset(nrows, ncols, "real");
                 
                 int res = 0;
                if( dstmp->getDatasetptr() != nullptr ) {
                    res = Cholesky_decomposition_hdf5(dsA, dstmp, nrows, ncols, dElementsBlock, threads);
                    if(res != 0) {
                        Rcpp::Rcout<<"\n Can't get Cholesky decomposition \n";
                    } else{
                        lst_return["fn"] = filename;
                        lst_return["ds"] = strOutdataset;
                    }
                } else {
                    checkClose_file(dsA, dstmp);
                    Rcpp::Rcerr << "c++ exception bdCholesky_hdf5: " << "Error creating dataset";
                    return(lst_return);
                    // return void();
                }
                
                delete dstmp; dstmp = nullptr;
                 
             } else {
                Rcpp::Rcout<<"\n Can't get Cholesky decomposition not a square matrix\n";
             }    
        } else {
            // delete dsA; dsA = nullptr;
            Rcpp::Rcerr << "c++ exception bdCholesky_hdf5: " << "Error opening dataset";
            // return void();
         }
         
         delete dsA; dsA = nullptr;
         
     } catch( H5::FileIException& error ) { 
         checkClose_file(dsA, dstmp);
         Rcpp::Rcerr<<"c++ exception bdCholesky_hdf5 (File IException)";
     } catch( H5::GroupIException & error ) { 
         checkClose_file(dsA, dstmp);
         Rcpp::Rcerr << "c++ exception bdCholesky_hdf5 (Group IException)";
     } catch( H5::DataSetIException& error ) { 
         checkClose_file(dsA, dstmp);
         Rcpp::Rcerr << "c++ exception bdCholesky_hdf5 (DataSet IException)";
     } catch(std::exception& ex) {
         checkClose_file(dsA, dstmp);
         Rcpp::Rcerr << "c++ exception bdCholesky_hdf5" << ex.what();
     } catch (...) {
         checkClose_file(dsA, dstmp);
         Rcpp::Rcerr<<"\nC++ exception bdCholesky_hdf5 (unknown reason)";
     }
     
     // return void();
     return(lst_return);
 }

