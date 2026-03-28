#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixPseudoinverse.hpp"
// #include "Utilities/Utilities.hpp"

/**
 * @file hdf5_pseudoinverse.cpp
 * @brief Implementation of Moore-Penrose pseudoinverse computation for matrices
 * @details This file provides functionality for computing the Moore-Penrose
 * pseudoinverse of matrices, both in-memory and HDF5-stored. The implementation
 * supports:
 * - Singular Value Decomposition (SVD) based computation
 * - Parallel processing capabilities
 * - Both dense and sparse matrices
 * - Memory-efficient processing for large matrices
 * 
 * The pseudoinverse A⁺ of a matrix A satisfies the following properties:
 * 1. AA⁺A = A
 * 2. A⁺AA⁺ = A⁺
 * 3. (AA⁺)* = AA⁺
 * 4. (A⁺A)* = A⁺A
 * 
 * where * denotes the conjugate transpose.
 */

/**
 * @brief Computes the Moore-Penrose pseudoinverse of an in-memory matrix
 * 
 * @details Implements the pseudoinverse computation using SVD decomposition:
 * For a matrix A = UΣV*, the pseudoinverse A⁺ = VΣ⁺U*
 * where Σ⁺ is obtained by reciprocating non-zero singular values.
 * 
 * Implementation features:
 * - Automatic handling of singular values near zero
 * - Support for non-square matrices
 * - Parallel computation options
 * - Numerically stable implementation
 * 
 * @param X Input matrix (m x n)
 * @param threads Number of threads for parallel computation
 * @return Pseudoinverse matrix (n x m)
 * 
 * @throws std::exception if computation fails
 */

//' Compute Matrix Pseudoinverse (In-Memory)
//'
//' @description
//' Computes the Moore-Penrose pseudoinverse of a matrix using SVD decomposition.
//' This implementation handles both square and rectangular matrices, and provides
//' numerically stable results even for singular or near-singular matrices.
//'
//' @details
//' The Moore-Penrose pseudoinverse (denoted A+) of a matrix A is computed using 
//' Singular Value Decomposition (SVD). 
//'
//' For a matrix A = U*Sigma*V^T (where ^T denotes transpose), the pseudoinverse is 
//' computed as:
//'
//' \deqn{A^+ = V \Sigma^+ U^T}
//'
//' where Sigma+ is obtained by taking the reciprocal of non-zero singular values.
//'
//' @section Mathematical Details:
//' \itemize{
//'   \item SVD decomposition: \eqn{A = U \Sigma V^T}
//'   \item Pseudoinverse: \eqn{A^+ = V \Sigma^+ U^T}
//'   \item \eqn{\Sigma^+_{ii} = 1/\Sigma_{ii}} if \eqn{\Sigma_{ii} > \text{tolerance}}
//'   \item \eqn{\Sigma^+_{ii} = 0} otherwise
//' }
//' 
//' Key features:
//' * Robust computation:
//'   - Handles singular and near-singular matrices
//'   - Automatic threshold for small singular values
//'   - Numerically stable implementation
//' 
//' * Implementation details:
//'   - Uses efficient SVD algorithms
//'   - Parallel processing support
//'   - Memory-efficient computation
//'   - Handles both dense and sparse inputs
//'
//' The pseudoinverse satisfies the Moore-Penrose conditions:
//' * \eqn{AA^+A = A}
//' * \eqn{A^+AA^+ = A^+}
//' * \eqn{(AA^+)^* = AA^+}
//' * \eqn{(A^+A)^* = A^+A}
//' 
//' @param X Numeric matrix or vector to be pseudoinverted.
//' @param threads Optional integer. Number of threads for parallel computation.
//'   If NULL, uses maximum available threads.
//'
//' @return The pseudoinverse matrix of X.
//'
//' @examples
//' library(BigDataStatMeth)
//' 
//' # Create a singular matrix
//' X <- matrix(c(1,2,3,2,4,6), 2, 3)  # rank-deficient matrix
//' 
//' # Compute pseudoinverse
//' X_pinv <- bdpseudoinv(X)
//' 
//' # Verify Moore-Penrose conditions
//' # 1. X %*% X_pinv %*% X = X
//' all.equal(X %*% X_pinv %*% X, X)
//' 
//' # 2. X_pinv %*% X %*% X_pinv = X_pinv
//' all.equal(X_pinv %*% X %*% X_pinv, X_pinv)
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * Ben-Israel, A., & Greville, T. N. E. (2003). Generalized Inverses:
//'   Theory and Applications, 2nd Edition. Springer.
//'
//' @seealso
//' * \code{\link{bdpseudoinv_hdf5}} for HDF5-stored matrices
//' * \code{\link{bdSVD_hdf5}} for singular value decomposition
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdpseudoinv( Rcpp::RObject X,
                           Rcpp::Nullable<int> threads = R_NilValue)
{
    
    try {
        
        Eigen::MatrixXd A;
        
        try{
            A = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(X);
        }catch(std::exception &ex) {
            A = Rcpp::as<Eigen::Map<Eigen::VectorXd> >(X);
        }
        
        Eigen::MatrixXd pinv = BigDataStatMeth::RcppPseudoinv(&A, threads);
        
        return(Rcpp::wrap(pinv));
        
    } catch(std::exception &ex) {
        Rcpp::Rcerr << "c++ exception bdpseudoinv" << ex.what();
        return Rcpp::wrap(-1);
    }
    
}

/**
 * @brief Computes the Moore-Penrose pseudoinverse of an HDF5-stored matrix
 * 
 * @details Implements the pseudoinverse computation for matrices stored in HDF5
 * format. Uses SVD-based computation with efficient I/O handling and memory
 * management.
 * 
 * Implementation features:
 * - Memory-efficient HDF5 I/O
 * - Block-based computation for large matrices
 * - Parallel processing support
 * - Flexible output options
 * 
 * @param filename HDF5 file path
 * @param group Group containing input matrix
 * @param dataset Dataset name for input matrix
 * @param outgroup Output group for pseudoinverse
 * @param outdataset Output dataset name
 * @param overwrite Whether to overwrite existing results
 * @param threads Number of threads for parallel computation
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::GroupIException if there are HDF5 group operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Compute Matrix Pseudoinverse (HDF5-Stored)
//'
//' @description
//' Computes the Moore-Penrose pseudoinverse of a matrix stored in HDF5 format.
//' The implementation is designed for large matrices, using block-based processing
//' and efficient I/O operations.
//'
//' @details
//' This function provides an HDF5-based implementation for computing pseudoinverses
//' of large matrices. Key features:
//' 
//' * HDF5 Integration:
//'   - Efficient reading of input matrix
//'   - Block-based processing for large matrices
//'   - Memory-efficient computation
//'   - Direct output to HDF5 format
//' 
//' * Implementation Features:
//'   - SVD-based computation
//'   - Parallel processing support
//'   - Automatic memory management
//'   - Flexible output options
//'
//' The function handles:
//' * Data validation
//' * Memory management
//' * Error handling
//' * HDF5 file operations
//'
//' @param filename String. Path to the HDF5 file.
//' @param group String. Group containing the input matrix.
//' @param dataset String. Dataset name for the input matrix.
//' @param outgroup Optional string. Output group name (defaults to "PseudoInverse").
//' @param outdataset Optional string. Output dataset name (defaults to input dataset name).
//' @param overwrite Logical. Whether to overwrite existing results.
//' @param threads Optional integer. Number of threads for parallel computation.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the pseudoinverse matrix (group/dataset)}
//' }
//'
//' @examples
//' library(BigDataStatMeth)
//' 
//' # Create a singular matrix
//' X <- matrix(c(1,2,3,2,4,6), 2, 3)
//' fn <- "test.hdf5"
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix(filename = fn,
//'                      object = X,
//'                      group = "data",
//'                      dataset = "X",
//'                      overwriteFile = TRUE)
//' 
//' # Compute pseudoinverse
//' bdpseudoinv_hdf5(filename = fn,
//'                  group = "data",
//'                  dataset = "X",
//'                  outgroup = "results",
//'                  outdataset = "X_pinv",
//'                  overwrite = TRUE)
//' 
//' # Cleanup
//' if (file.exists(fn)) {
//'   file.remove(fn)
//' }
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdpseudoinv}} for in-memory computation
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdpseudoinv_hdf5(std::string filename, std::string group, std::string dataset,
                               Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                               Rcpp::Nullable<std::string> outdataset = R_NilValue, 
                               Rcpp::Nullable<bool> overwrite = R_NilValue,
                               Rcpp::Nullable<int> threads = R_NilValue)
{
     
     BigDataStatMeth::hdf5Dataset* dsA = nullptr;
     BigDataStatMeth::hdf5Dataset* dsRes = nullptr;

     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
    try {
        
        H5::Exception::dontPrint();
         
        Eigen::MatrixXd A;
        std::string strOutgroup, strOutdataset;
        bool bforce;
        
        if(outgroup.isNull()) { strOutgroup = "PseudoInverse"; } 
        else {   strOutgroup = Rcpp::as<std::string>(outgroup); }
        
        if(outdataset.isNull()) { strOutdataset = dataset; } 
        else { strOutdataset = Rcpp::as<std::string>(outdataset); }
        
        if(overwrite.isNull()) { bforce = false ; }
        else { bforce = Rcpp::as<bool>(overwrite); }
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
        dsA->openDataset();
        
        dsRes = new BigDataStatMeth::hdf5Dataset(filename, strOutgroup, strOutdataset, bforce);
        
        if( dsA->getDatasetptr() != nullptr ) {
            RcppPseudoinvHdf5(dsA, dsRes, threads);
        } else {
            checkClose_file(dsA, dsRes);
            Rcpp::Rcerr << "c++ exception bdPseudoinv_hdf5: " << "Error opening dataset";
            return(lst_return);
        }

        lst_return["fn"] = filename;
        lst_return["ds"] = strOutgroup + "/" + strOutdataset;

        // lst_return = Rcpp::List::create(Rcpp::Named("fn") = filename,
        //                                 Rcpp::Named("ds") = strOutgroup + "/" + strOutdataset);
        
        delete dsA; dsA = nullptr;
        delete dsRes; dsRes = nullptr;
         
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsA, dsRes);    
        Rcpp::Rcerr << "c++ exception bdCholesky_hdf5 (File IException)";     
        return(lst_return);
    } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsRes);    
        Rcpp::Rcerr << "c++ exception bdCholesky_hdf5 (Group IException)";
        return(lst_return);
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsRes);    
        Rcpp::Rcerr << "c++ exception bdCholesky_hdf5 (DataSet IException)";
        return(lst_return);
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsRes);    
        Rcpp::Rcerr << "c++ exception bdCholesky_hdf5" << ex.what();
        return(lst_return);
    }
    
    // return void();
    return(lst_return);
}
