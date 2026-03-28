#include <BigDataStatMeth.hpp>
// #include "memAlgebra/memMultiplication.hpp"
// #include "memAlgebra/memOptimizedProducts.hpp"

/**
 * @file mem_blockmult.cpp
 * @brief Implementation of block-based matrix multiplication for in-memory matrices
 * @details This file provides functionality for efficient matrix multiplication
 * using block-based algorithms. The implementation supports:
 * - Matrix-matrix multiplication
 * - Matrix-vector multiplication
 * - Vector-vector multiplication
 * - Parallel processing for large matrices
 * - Block-based computation for memory efficiency
 * 
 * Key features:
 * - Automatic selection of optimal block size
 * - Support for parallel computation
 * - Memory-efficient block processing
 * - Multiple data type combinations
 */

/**
 * @brief Performs block-based matrix multiplication
 * 
 * @details Implements efficient matrix multiplication using block-based algorithms
 * for better cache utilization and memory efficiency. The function automatically
 * selects the appropriate multiplication method based on input types:
 * - Matrix-matrix multiplication
 * - Matrix-vector multiplication
 * - Vector-vector multiplication
 * 
 * Implementation features:
 * - Block-based computation for large matrices
 * - Parallel processing support
 * - Automatic block size optimization
 * - Multiple input type combinations
 * 
 * @param A First input matrix/vector
 * @param B Second input matrix/vector
 * @param block_size Size of blocks for computation
 * @param paral Whether to use parallel processing
 * @param byBlocks Whether to force block-based computation
 * @param threads Number of threads for parallel processing
 * 
 * @return Result of multiplication operation
 * @throws std::exception if computation fails
 */

//' Block-Based Matrix Multiplication
//'
//' @description
//' Performs efficient matrix multiplication using block-based algorithms. The function
//' supports various input combinations (matrix-matrix, matrix-vector, vector-vector)
//' and provides options for parallel processing and block-based computation.
//'
//' @details
//' This function implements block-based matrix multiplication algorithms optimized
//' for cache efficiency and memory usage. Key features:
//' 
//' * Input combinations supported:
//'   - Matrix-matrix multiplication
//'   - Matrix-vector multiplication (both left and right)
//'   - Vector-vector multiplication
//' 
//' * Performance optimizations:
//'   - Block-based computation for cache efficiency
//'   - Parallel processing for large matrices
//'   - Automatic block size selection
//'   - Memory-efficient implementation
//'
//' The function automatically selects the appropriate multiplication method based
//' on input types and sizes. For large matrices (>2.25e+08 elements), block-based
//' computation is used by default.
//'
//' @param A Matrix or vector. First input operand.
//' @param B Matrix or vector. Second input operand.
//' @param block_size Integer. Block size for computation. If NULL, uses maximum
//'   allowed block size.
//' @param paral Logical. If TRUE, enables parallel computation. Default is FALSE.
//' @param byBlocks Logical. If TRUE (default), forces block-based computation for
//'   large matrices. Can be set to FALSE to disable blocking.
//' @param threads Integer. Number of threads for parallel computation. If NULL,
//'   uses half of available threads or maximum allowed threads.
//'
//' @return Matrix or vector containing the result of A * B.
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Matrix-matrix multiplication
//' N <- 2500
//' M <- 400
//' nc <- 4
//' 
//' set.seed(555)
//' mat <- matrix(rnorm(N*M, mean=0, sd=10), N, M)
//' 
//' # Parallel block multiplication
//' result <- bdblockMult(mat, mat,
//'                       paral = TRUE,
//'                       threads = nc)
//' 
//' # Matrix-vector multiplication
//' vec <- rnorm(M)
//' result_mv <- bdblockMult(mat, vec,
//'                          paral = TRUE,
//'                          threads = nc)
//' }
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * Kumar, V. et al. (1994). Introduction to Parallel Computing: Design and
//'   Analysis of Algorithms. Benjamin/Cummings Publishing Company.
//'
//' @seealso
//' * \code{\link{bdblockSum}} for block-based matrix addition
//' * \code{\link{bdblockSubstract}} for block-based matrix subtraction
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdblockMult(Rcpp::RObject A, Rcpp::RObject B,
                         Rcpp::Nullable<int> block_size = R_NilValue, 
                         Rcpp::Nullable<bool> paral = R_NilValue,
                         Rcpp::Nullable<bool> byBlocks = true,
                         Rcpp::Nullable<int> threads = R_NilValue)
{
    
    
    Rcpp::NumericMatrix C;

    try{
        bool bparal, bbyBlocks;
        if (paral.isNull()) { bparal = false; }
        else { bparal = Rcpp::as<bool> (paral); }
        
        if (byBlocks.isNull()) { bbyBlocks = false; }
        else { bbyBlocks = Rcpp::as<bool> (byBlocks); }

        //..// if( bparal==false || Rcpp::as<Rcpp::NumericVector>(A).size() < MAXELEMSINBLOCK || bbyBlocks == false) {
        if( bparal==false || bbyBlocks == false) {
            if( Rcpp::is<Rcpp::NumericMatrix>(A) && Rcpp::is<Rcpp::NumericMatrix>(B) ) {
                return( Rcpp::wrap(BigDataStatMeth::Rcpp_block_matrix_mul(Rcpp::as<Eigen::MatrixXd>(A), Rcpp::as<Eigen::MatrixXd>(B), block_size)));
                
            } else if( Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericMatrix>(B)) {
                return( BigDataStatMeth::Rcpp_matrix_vect_mult( B, A) );
                
            } else if( Rcpp::is<Rcpp::NumericVector>(B) && Rcpp::is<Rcpp::NumericMatrix>(A)) {
                return( BigDataStatMeth::Rcpp_matrix_vect_mult( A, B) );
                
            } else if(Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericVector>(B)) {
                return( BigDataStatMeth::Rcpp_vector_mult(A, B));
                
            } else {
                Rcpp::Rcout<<"\nData type not allowed";
            }    
            
            
        } else {
            
            if( Rcpp::is<Rcpp::NumericMatrix>(A) && Rcpp::is<Rcpp::NumericMatrix>(B) ) {
                return( Rcpp::wrap(BigDataStatMeth::Rcpp_block_matrix_mul_parallel(Rcpp::as<Eigen::MatrixXd>(A), Rcpp::as<Eigen::MatrixXd>(B), false, false, block_size, threads)));
                
            } else if( Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericMatrix>(B)) {
                return(BigDataStatMeth::Rcpp_matrix_vector_blockMult(B, A, paral, block_size, threads));
                
            } else if( Rcpp::is<Rcpp::NumericVector>(B) && Rcpp::is<Rcpp::NumericMatrix>(A)) {
                return(BigDataStatMeth::Rcpp_matrix_vector_blockMult(A, B, paral, block_size, threads));
                
            } else if(Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericVector>(B)) {
                return( BigDataStatMeth::Rcpp_vector_mult(A, B));
                
            } else {
                Rcpp::Rcout<<"\nData type not allowed";
            }    
            
        }
        
        
    } catch(std::exception &ex) {
        Rcpp::Rcerr << "c++ exception bdblockMult: " << ex.what();
    } catch (...) {
        Rcpp::Rcerr << "c++ exception bdblockMult (unknown reason)";
        
    }

    return(R_NilValue);
}

