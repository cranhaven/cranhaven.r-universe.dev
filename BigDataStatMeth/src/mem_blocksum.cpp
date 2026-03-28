#include <BigDataStatMeth.hpp>
// #include "memAlgebra/memSum.hpp"

/**
 * @file mem_blocksum.cpp
 * @brief Implementation of block-based matrix addition for in-memory matrices
 * @details This file provides functionality for efficient matrix addition
 * using block-based algorithms. The implementation supports:
 * - Matrix-matrix addition
 * - Matrix-vector addition
 * - Vector-vector addition
 * - Parallel processing for large matrices
 * - Block-based computation for memory efficiency
 * 
 * Key features:
 * - Automatic selection of computation method
 * - Support for parallel computation
 * - Memory-efficient block processing
 * - Multiple data type combinations
 */

/**
 * @brief Performs block-based matrix addition
 * 
 * @details Implements efficient matrix addition using block-based algorithms
 * for better cache utilization and memory efficiency. The function automatically
 * selects the appropriate addition method based on input types:
 * - Matrix-matrix addition
 * - Matrix-vector addition
 * - Vector-vector addition
 * 
 * Implementation features:
 * - Block-based computation for large matrices
 * - Parallel processing support
 * - Multiple input type combinations
 * - Memory-efficient implementation
 * 
 * @param A First input matrix/vector
 * @param B Second input matrix/vector
 * @param block_size Size of blocks for computation
 * @param paral Whether to use parallel processing
 * @param byBlocks Whether to force block-based computation
 * @param threads Number of threads for parallel processing
 * 
 * @return Result of addition operation
 * @throws std::exception if computation fails
 */

/**
 *  // // [[Rcpp::export(.blockSum_hdf5)]]
 */


//' Block-Based Matrix Addition
//'
//' @description
//' Performs efficient matrix addition using block-based algorithms. The function
//' supports various input combinations (matrix-matrix, matrix-vector, vector-vector)
//' and provides options for parallel processing and block-based computation.
//'
//' @details
//' This function implements block-based matrix addition algorithms optimized
//' for cache efficiency and memory usage. Key features:
//' 
//' * Input combinations supported:
//'   - Matrix-matrix addition
//'   - Matrix-vector addition (both left and right)
//'   - Vector-vector addition
//' 
//' * Performance optimizations:
//'   - Block-based computation for cache efficiency
//'   - Parallel processing for large matrices
//'   - Automatic method selection based on input size
//'   - Memory-efficient implementation
//'
//' The function automatically selects the appropriate addition method based
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
//'   uses half of available threads.
//'
//' @return Matrix or vector containing the result of A + B.
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Matrix-matrix addition
//' N <- 2500
//' M <- 400
//' nc <- 4
//' 
//' set.seed(555)
//' mat1 <- matrix(rnorm(N*M, mean=0, sd=10), N, M)
//' mat2 <- matrix(rnorm(N*M, mean=0, sd=10), N, M)
//' 
//' # Parallel block addition
//' result <- bdblockSum(mat1, mat2,
//'                      paral = TRUE,
//'                      threads = nc)
//' 
//' # Matrix-vector addition
//' vec <- rnorm(M)
//' result_mv <- bdblockSum(mat1, vec,
//'                         paral = TRUE,
//'                         threads = nc)
//' }
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * Kumar, V. et al. (1994). Introduction to Parallel Computing: Design and
//'   Analysis of Algorithms. Benjamin/Cummings Publishing Company.
//'
//' @seealso
//' * \code{\link{bdblockSubstract}} for block-based matrix subtraction
//' * \code{\link{bdblockMult}} for block-based matrix multiplication
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdblockSum(Rcpp::RObject A, Rcpp::RObject B,
                         Rcpp::Nullable<int> block_size = R_NilValue, 
                         Rcpp::Nullable<bool> paral = R_NilValue,
                         Rcpp::Nullable<bool> byBlocks = true,
                         Rcpp::Nullable<int> threads = R_NilValue)
{
    
    // hsize_t iblock_size;
    //Rcpp::NumericMatrix C;

    try{
        
        bool bparal, bbyBlocks;
        
        if (paral.isNull()) { bparal = false; }
        else { bparal = Rcpp::as<bool> (paral); }
        
        if (byBlocks.isNull()) { bbyBlocks = false; }
        else { bbyBlocks = Rcpp::as<bool> (byBlocks); }

        // if( bparal==false || Rcpp::as<Rcpp::NumericVector>(A).size() < MAXELEMSINBLOCK || bbyBlocks == false) {
        if( bparal==false || static_cast<hsize_t>(Rcpp::as<Rcpp::NumericVector>(A).size()) < MAXELEMSINBLOCK || bbyBlocks == false) {
            if( Rcpp::is<Rcpp::NumericMatrix>(A) && Rcpp::is<Rcpp::NumericMatrix>(B) ) {
                return( BigDataStatMeth::Rcpp_matrix_sum(A, B) );
                
            } else if( Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericMatrix>(B)) {
                return( BigDataStatMeth::Rcpp_matrix_vect_sum( B, A) );
                
            } else if( Rcpp::is<Rcpp::NumericVector>(B) && Rcpp::is<Rcpp::NumericMatrix>(A)) {
                return( BigDataStatMeth::Rcpp_matrix_vect_sum( A, B) );
                
            } else if(Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericVector>(B)) {
                return( BigDataStatMeth::Rcpp_vector_sum(A, B));
                
            } else {
                Rcpp::Rcout<<"\nData type not allowed";
            }    
        } else {
            
            if( Rcpp::is<Rcpp::NumericMatrix>(A) && Rcpp::is<Rcpp::NumericMatrix>(B) ) {
                return( BigDataStatMeth::Rcpp_matrix_blockSum(A, B, threads) );
                
            } else if( Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericMatrix>(B)) {
                 return(BigDataStatMeth::Rcpp_matrix_vector_blockSum(B, A, paral, threads));
                
            } else if( Rcpp::is<Rcpp::NumericVector>(B) && Rcpp::is<Rcpp::NumericMatrix>(A)) {
                return(BigDataStatMeth::Rcpp_matrix_vector_blockSum(A, B, paral, threads));
                
            } else if(Rcpp::is<Rcpp::NumericVector>(A) && Rcpp::is<Rcpp::NumericVector>(B)) {
                
                // return( BigDataStatMeth::Rcpp_vector_sum(A, B));
                
            } else {
                Rcpp::Rcout<<"\nData type not allowed";
            }    
        }
        
    } catch(std::exception &ex) {
        Rcpp::Rcerr << "c++ exception bdblockSum: " << ex.what();
        Rcpp::Rcout<< ex.what();
        return(R_NilValue);
    }

    return(R_NilValue);
}

