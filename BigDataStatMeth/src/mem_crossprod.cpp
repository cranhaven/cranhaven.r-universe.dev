#include <BigDataStatMeth.hpp>
// #include "memAlgebra/memOptimizedProducts.hpp"
// #include "memAlgebra/memMultiplication.hpp"

/**
 * @file mem_crossprod.cpp
 * @brief Implementation of efficient cross-product computation for in-memory matrices
 * @details This file provides functionality for computing matrix cross-products
 * (X'X or X'Y) efficiently. The implementation supports:
 * - Single matrix cross-product (X'X)
 * - Two-matrix cross-product (X'Y)
 * - Block-based computation for large matrices
 * - Parallel processing capabilities
 * 
 * Key features:
 * - Memory-efficient block processing
 * - Parallel computation support
 * - Optimized for cache utilization
 * - Automatic block size selection
 */

/**
 * @brief Computes matrix cross-product efficiently
 * 
 * @details Implements efficient cross-product computation using block-based
 * algorithms and optional parallel processing. For a single matrix X, computes
 * X'X. For two matrices X and Y, computes X'Y.
 * 
 * Implementation features:
 * - Block-based computation for large matrices
 * - Parallel processing support
 * - Automatic block size optimization
 * - Memory-efficient implementation
 * 
 * @param A First input matrix
 * @param B Optional second input matrix
 * @param transposed Whether to use transposed input
 * @param block_size Block size for computation
 * @param paral Whether to use parallel processing
 * @param threads Number of threads for parallel processing
 * 
 * @return Result of cross-product operation
 * @throws std::exception if computation fails
 */

//' Efficient Matrix Cross-Product Computation
//'
//' @description
//' Computes matrix cross-products efficiently using block-based algorithms and
//' optional parallel processing. Supports both single-matrix (X'X) and two-matrix
//' (X'Y) cross-products.
//'
//' @details
//' This function implements efficient cross-product computation using block-based
//' algorithms optimized for cache efficiency and memory usage. Key features:
//' 
//' * Operation modes:
//'   - Single matrix: Computes X'X
//'   - Two matrices: Computes X'Y
//' 
//' * Performance optimizations:
//'   - Block-based computation for cache efficiency
//'   - Parallel processing for large matrices
//'   - Automatic block size selection
//'   - Memory-efficient implementation
//'
//' The function automatically selects optimal computation strategies based on
//' input size and available resources. For large matrices, block-based computation
//' is used to improve cache utilization.
//'
//' @param A Numeric matrix. First input matrix.
//' @param B Optional numeric matrix. If provided, computes A'B instead of A'A.
//' @param transposed Logical. If TRUE, uses transposed input matrix.
//' @param block_size Integer. Block size for computation. If NULL, uses optimal
//'   block size based on matrix dimensions and cache size.
//' @param paral Logical. If TRUE, enables parallel computation.
//' @param threads Integer. Number of threads for parallel computation. If NULL,
//'   uses all available threads.
//'
//' @return Numeric matrix containing the cross-product result.
//'
//' @examples
//' library(BigDataStatMeth)
//' 
//' # Single matrix cross-product
//' n <- 100
//' p <- 60
//' X <- matrix(rnorm(n*p), nrow=n, ncol=p)
//' res <- bdCrossprod(X)
//' 
//' # Verify against base R
//' all.equal(crossprod(X), res)
//' 
//' # Two-matrix cross-product
//' n <- 100
//' p <- 100
//' Y <- matrix(rnorm(n*p), nrow=n)
//' res <- bdCrossprod(X, Y)
//' 
//' # Parallel computation
//' res_par <- bdCrossprod(X, Y,
//'                        paral = TRUE,
//'                        threads = 4)
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * Kumar, V. et al. (1994). Introduction to Parallel Computing: Design and
//'   Analysis of Algorithms. Benjamin/Cummings Publishing Company.
//'
//' @seealso
//' * \code{\link{bdtCrossprod}} for transposed cross-product
//' * \code{\link{bdblockMult}} for block-based matrix multiplication
//'
//' @export
// [[Rcpp::export]]
Eigen::MatrixXd bdCrossprod( Rcpp::RObject A, Rcpp::Nullable<Rcpp::RObject> B =  R_NilValue, 
                                     Rcpp::Nullable<bool> transposed = R_NilValue,
                                     Rcpp::Nullable<int> block_size = R_NilValue, 
                                     Rcpp::Nullable<bool> paral = R_NilValue,
                                     Rcpp::Nullable<int> threads = R_NilValue )
{
    
    
    Eigen::MatrixXd C;
    
    try {
        
        Eigen::MatrixXd mA;
        Eigen::MatrixXd mB;
        bool bparal;
        
        if( paral.isNull()) {
            bparal = false;
        } else {
            bparal = Rcpp::as<bool> (paral);
        }
        
        // Read DelayedmArray's A and b
        if ( Rcpp::is<Rcpp::NumericMatrix>(A) || Rcpp::is<Rcpp::IntegerMatrix>(A))    
        {
            try{  
                mA = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(A);
            } catch(std::exception &ex) { }
            
        } else {
            throw("Matrix A is not numeric - Only numeric matrix allowed");
            
        }
        
        if(B.isNull()) {
            C = BigDataStatMeth::bdcrossproduct(mA);
        } else {
            
            
            if(Rcpp::is<Rcpp::NumericMatrix>(B) || Rcpp::is<Rcpp::IntegerMatrix>(B)) {
                try{  
                    mB = Rcpp::as<Eigen::MatrixXd>(B); 
                }
                catch(std::exception &ex) { }
            } else {
                throw("Matrix B is not numeric - Only numeric matrix allowed");
            }
                
            Eigen::Map<Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> > mTrans(mA.data(), mA.cols(), mA.rows());
            
            if(bparal == true) {
                C = BigDataStatMeth::Rcpp_block_matrix_mul_parallel(mTrans, mB, false, false, block_size, threads);
                // C = Bblock_matrix_mul_parallel(mTrans, mB, iblock_size, threads);
                
            } else if (bparal == false)  {
                C = BigDataStatMeth::Rcpp_block_matrix_mul(mTrans, mB, block_size);
                // C = Bblock_matrix_mul(mTrans, mB, iblock_size);
            }
            
        }
        
    } catch(std::exception &ex) {
        Rcpp::Rcerr << "c++ exception bdCrossprod: " << ex.what();
        return(Eigen::MatrixXd(0,0));
        // return(Rcpp::IntegerMatrix(0,0));
    } catch (...) {
        Rcpp::Rcerr << "c++ exception bdCrossprod (unknown reason)";
        return(Eigen::MatrixXd(0,0));
    }
    
    return(C);
    
}
