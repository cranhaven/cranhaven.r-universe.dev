#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixInvCholesky.hpp"
// #include "hdf5Algebra/matrixTriangular.hpp"

/**
 * @file hdf5_InvCholesky.cpp
 * @brief Implementation of Matrix Inversion using Cholesky Decomposition for HDF5-stored matrices
 * @details This file contains implementations for computing matrix inverses using
 * the Cholesky decomposition method for symmetric positive-definite matrices stored
 * in HDF5 files. The implementation supports both full and triangular storage formats,
 * with options for block-based computation and parallel processing.
 */

/**
 * @brief Computes matrix inverse using Cholesky decomposition
 * 
 * @details Performs matrix inversion of a symmetric positive-definite matrix A
 * using the following steps:
 * 1. Compute Cholesky decomposition A = LL'
 * 2. Solve LL'X = I for X = A^(-1)
 * 
 * The implementation:
 * - Verifies matrix symmetry and positive-definiteness
 * - Uses block-based computation for large matrices
 * - Provides options for storage format (full or triangular)
 * - Utilizes parallel computation when available
 * - Handles memory efficiently through block processing
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
//' Matrix Inversion using Cholesky Decomposition for HDF5-Stored Matrices
//'
//' @description
//' Computes the inverse of a symmetric positive-definite matrix stored in an HDF5 file
//' using the Cholesky decomposition method. This approach is more efficient and
//' numerically stable than general matrix inversion methods for symmetric
//' positive-definite matrices.
//' 
//' @details
//' This function implements an efficient matrix inversion algorithm that leverages
//' the special properties of symmetric positive-definite matrices. Key features:
//' * Uses Cholesky decomposition for improved numerical stability
//' * Block-based computation for large matrices
//' * Optional storage formats (full or triangular)
//' * Parallel processing support
//' * Memory-efficient block algorithm
//'
//' The algorithm proceeds in two main steps:
//' 1. Compute the Cholesky decomposition A = LL'
//' 2. Solve the system LL'X = I for X = A^(-1)
//'
//' Advantages of this method:
//' * More efficient than general matrix inversion
//' * Better numerical stability
//' * Preserves matrix symmetry
//' * Exploits positive-definiteness for efficiency
//'
//' @param filename Character string. Path to the HDF5 file containing the input matrix.
//' @param group Character string. Path to the group containing the input dataset.
//' @param dataset Character string. Name of the input dataset to invert.
//' @param outdataset Character string. Name for the output dataset.
//' @param outgroup Character string. Optional output group path. If not provided,
//'   results are stored in the input group.
//' @param fullMatrix Logical. If TRUE, stores the complete inverse matrix.
//'   If FALSE (default), stores only the lower triangular part to save space.
//' @param overwrite Logical. If TRUE, allows overwriting existing results.
//' @param threads Integer. Number of threads for parallel computation (default = 2).
//' @param elementsBlock Integer. Maximum number of elements to process in each block
//'   (default = 1,000,000). For matrices larger than 5000x5000, automatically adjusted
//'   to number of rows or columns * 2.
//' 
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the inverse 
//'   Cholesky decomposition A^(-1) result (group/dataset)}
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
//' # Compute inverse using Cholesky decomposition
//' bdInvCholesky_hdf5("matrix.h5", "data", "matrix",
//'                    outdataset = "inverse",
//'                    outgroup = "results",
//'                    fullMatrix = TRUE,
//'                    threads = 4)
//' 
//' # Verify the inverse
//' Ainv <- h5read("matrix.h5", "results/inverse")
//' max(abs(A %*% Ainv - diag(nrow(A))))  # Should be very small
//' }
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * Higham, N. J. (2002). Accuracy and Stability of Numerical Algorithms,
//'   2nd Edition. SIAM.
//'
//' @seealso
//' * \code{\link{bdCholesky_hdf5}} for the underlying Cholesky decomposition
//' * \code{\link{bdSolve_hdf5}} for solving linear systems
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdInvCholesky_hdf5(std::string filename, std::string group, std::string dataset,
                        std::string outdataset,
                        Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                        Rcpp::Nullable<bool> fullMatrix = R_NilValue, 
                        Rcpp::Nullable<bool> overwrite = R_NilValue,
                        Rcpp::Nullable<int> threads = 2,
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
        
        std::string strOutgroup, strOutdataset;
        bool bfull;
        int nrows = 0, ncols = 0;
        
        if(fullMatrix.isNull()) { bfull = false; } 
        else {  bfull = Rcpp::as<bool>(fullMatrix); }
        
        if(outgroup.isNull()) { strOutgroup = group; } 
        else {   strOutgroup = Rcpp::as<std::string>(outgroup); }
        
        if(elementsBlock.isNull()) { dElementsBlock = MAXELEMSINBLOCK; } 
        else { dElementsBlock = Rcpp::as<long>(elementsBlock); }
        
        strOutdataset = strOutgroup + "/" + outdataset;

        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
        dsA->openDataset();
        
        if( dsA->getDatasetptr() != nullptr  )  {
            
            nrows = dsA->nrows();
            ncols = dsA->ncols();
            
            if(nrows == ncols) {
                
                dstmp = new BigDataStatMeth::hdf5DatasetInternal(filename, strOutdataset, true);
                dstmp->createDataset(nrows, ncols, "real");
                
                if( dstmp->getDatasetptr() != nullptr ) {
                    BigDataStatMeth::Rcpp_InvCholesky_hdf5( dsA, dstmp, bfull, dElementsBlock, threads);
                } else {
                    checkClose_file(dsA, dstmp);
                    Rcpp::Rcerr << "c++ exception bdInvCholesky_hdf5: " << "Error creating temporary dataset";
                    return(lst_return);
                }
                
            } else {
                delete dsA; dsA = nullptr;
                Rcpp::Rcout<<"\n Can't get inverse matrix for "<< group + "/" + dataset <<" using Cholesky decomposition \n";
                return(lst_return);
            }    
        } else {
            delete dsA; dsA = nullptr;
            Rcpp::Rcerr << "c++ exception bdInvCholesky_hdf5: " << "Error opening dataset";
            return(lst_return);
        }
        
        lst_return["fn"] = filename;
        lst_return["ds"] = strOutdataset;
        
        delete dsA; dsA = nullptr;
        delete dstmp; dstmp = nullptr;
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsA, dstmp);
        Rcpp::Rcerr<<"\nc++ exception bdInvCholesky_hdf5 (File IException)";
    } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dstmp);
        Rcpp::Rcerr<<"\nc++ exception bdInvCholesky_hdf5 (Group IException)";
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dstmp);
        Rcpp::Rcerr<<"\nc++ exception bdInvCholesky_hdf5 (DataSet IException)";
    } catch(std::exception& ex) {
        checkClose_file(dsA, dstmp);
        Rcpp::Rcerr<<"\nc++ exception bdInvCholesky_hdf5: " << ex.what();
    } catch (...) {
        checkClose_file(dsA, dstmp);
        Rcpp::Rcerr<<"\nC++ exception bdInvCholesky_hdf5 (unknown reason)";
    }
    
    return(lst_return);
}
