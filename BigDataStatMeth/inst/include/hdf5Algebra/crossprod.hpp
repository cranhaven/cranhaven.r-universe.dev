/**
 * @file crossprod.hpp
 * @brief Implementation of matrix cross-product operations using HDF5
 *
 * This file provides functionality for computing matrix cross-products (A^T * B)
 * for large matrices stored in HDF5 format. The implementation uses block-wise
 * operations to handle matrices that don't fit in memory, with optimizations
 * for performance and memory usage.
 *
 * Key features:
 * - Block-wise matrix cross-product computation
 * - Memory-efficient implementation for large matrices
 * - Optimized block size selection
 * - Support for row-major and column-major storage
 * - Optional parallel processing
 *
 * @note This implementation is particularly suited for large matrices
 * where traditional in-memory operations are not feasible.
 *
 * @see BigDataStatMeth::hdf5Dataset
 */

#ifndef BIGDATASTATMETH_ALGEBRA_CROSSPROD_HPP
#define BIGDATASTATMETH_ALGEBRA_CROSSPROD_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Computes the cross-product of two matrices stored in HDF5 format
     *
     * @param dsA First input matrix dataset (A)
     * @param dsB Second input matrix dataset (B)
     * @param dsC Output matrix dataset for result
     * @param hdf5_block Block size for HDF5 operations
     * @param mem_block_size Memory block size for computations
     * @param bparal Whether to use parallel processing
     * @param browmajor Whether matrices are stored in row-major order
     * @param threads Number of threads for parallel processing (optional)
     * @return BigDataStatMeth::hdf5Dataset* Pointer to the result dataset
     *
     * @details Computes C = A^T * B using block-wise operations:
     * - Divides matrices into blocks for memory-efficient processing
     * - Processes blocks using optimized Eigen operations
     * - Accumulates results in the output dataset
     *
     * Implementation notes:
     * - Block sizes are automatically adjusted for matrix boundaries
     * - Uses Eigen for efficient block matrix operations
     * - Handles row-major and column-major storage formats
     *
     * Performance considerations:
     * - Time complexity: O(N*M*K) for matrices of sizes N×K and M×K
     * - Space complexity: O(block_size²) for block operations
     * - I/O complexity depends on block size and matrix dimensions
     *
     * Optimization parameters:
     * - hdf5_block: Controls HDF5 read/write block size
     * - mem_block_size: Controls in-memory block processing size
     * - Block sizes are adjusted for optimal performance
     *
     * Example usage:
     * @code
     * auto* A = hdf5Dataset(...);  // N×K matrix
     * auto* B = hdf5Dataset(...);  // M×K matrix
     * auto* C = hdf5Dataset(...);  // N×M result matrix
     * crossprod(A, B, C, 1000, 1000, true, true);
     * @endcode
     *
     * @throws std::range_error if matrix dimensions are incompatible
     * @throws std::exception on HDF5 operations errors
     *
     * @see getOptimBlockSize for block size optimization
     */
    inline BigDataStatMeth::hdf5Dataset* crossprod( 
            BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, 
            BigDataStatMeth::hdf5Dataset* dsC, bool isSymmetric, 
            hsize_t hdf5_block, hsize_t mem_block_size, bool bparal, 
            bool browmajor, Rcpp::Nullable<int> threads = R_NilValue) 

    {
        
        try {
            
            hsize_t N = dsA->nrows();
            hsize_t K = dsA->ncols();
            hsize_t M = dsB->nrows();
            hsize_t L = dsB->ncols();
            
            if( K == L)
            {
                // hsize_t isize = hdf5_block + 1,
                //     ksize = hdf5_block + 1,
                //     jsize = hdf5_block + 1;
                
                std::vector<hsize_t> stride = {1, 1};
                std::vector<hsize_t> block = {1, 1};
                
                if (isSymmetric) {
                    if (N != M) {
                        throw std::range_error("Symmetric crossprod requires square result matrix");
                    }
                    if (dsA->getFileName() != dsB->getFileName() || 
                        dsA->getGroup() != dsB->getGroup() || 
                        dsA->getDatasetName() != dsB->getDatasetName()) {
                        Rcpp::warning("isSymmetric=TRUE but different datasets provided. Results may be incorrect.");
                    }
                }
                
                dsC->createDataset( N, M, "real");
/** 2025/11/25                
                // Configure parallel processing
                int num_threads = 1;
                if (bparal) {
                    num_threads = get_number_threads(threads, Rcpp::wrap(bparal));  
#ifdef _OPENMP
                    omp_set_num_threads(num_threads);
#endif
                }
 Fi 2025/11/25 **/

#ifdef _OPENMP // Configure parallel processing
                int num_threads = 1;
                if (bparal) {
                    num_threads = get_number_threads(threads, Rcpp::wrap(bparal));  
                    omp_set_num_threads(num_threads);
                }
#endif                
                                
//                 // HDF5 thread safety: Initialize lock for I/O serialization
// #ifdef _OPENMP
//                 static omp_lock_t hdf5_lock;
//                 static bool hdf5_lock_initialized = false;
//                 
//                 if (bparal && !hdf5_lock_initialized) {
//                     omp_init_lock(&hdf5_lock);
//                     hdf5_lock_initialized = true;
//                 }
// #endif
                
                // Calculate total blocks for parallelization
                hsize_t blocks_i = (N + hdf5_block - 1) / hdf5_block;
                hsize_t blocks_j = (M + hdf5_block - 1) / hdf5_block;
                hsize_t total_blocks;
                
                if (isSymmetric) {
                    // For symmetric case: only upper triangle blocks
                    total_blocks = (blocks_i * (blocks_i + 1)) / 2;
                } else {
                    // For general case: all blocks
                    total_blocks = blocks_i * blocks_j;
                }
                
#ifdef _OPENMP
#pragma omp parallel for if(bparal) schedule(dynamic)
#endif
                for (hsize_t block_idx = 0; block_idx < total_blocks; ++block_idx)
                {
                    // Convert linear index to (ii_idx, jj_idx)
                    hsize_t ii_idx = 0, 
                            jj_idx = 0;
                    
                    if (isSymmetric) {
                        // Convert to upper triangle coordinates
                        hsize_t remaining = block_idx;
                        for (hsize_t i = 0; i < blocks_i; ++i) {
                            hsize_t blocks_in_row = blocks_i - i;
                            if (remaining < blocks_in_row) {
                                ii_idx = i;
                                jj_idx = i + remaining;
                                break;
                            }
                            remaining -= blocks_in_row;
                        }
                    } else {
                        // Convert to regular coordinates
                        ii_idx = block_idx / blocks_j;
                        jj_idx = block_idx % blocks_j;
                    }
                    
                    // Convert block indices to matrix indices
                    hsize_t ii = ii_idx * hdf5_block;
                    hsize_t jj = jj_idx * hdf5_block;
                    
                    // Boundary checks
                    if (ii >= N || jj >= M) continue;
                    
                    // Thread-local variables (each thread needs its own)
                    hsize_t local_isize = hdf5_block + 1;
                    hsize_t local_jsize = hdf5_block + 1;
                    hsize_t local_ksize = hdf5_block + 1;
                    
                    hsize_t iRowsA = getOptimBlockSize( N, hdf5_block, ii, local_isize);
                    if( ii + hdf5_block > N ) local_isize = N - ii;
                    
                    hsize_t iRowsB = getOptimBlockSize( M, hdf5_block, jj, local_jsize);
                    if( jj + hdf5_block > M) local_jsize = M - jj;
                    
                    Eigen::MatrixXd C_accum = Eigen::MatrixXd::Zero(iRowsA, iRowsB); 
                    
                    std::vector<hsize_t> offset = {jj, ii};        
                    std::vector<hsize_t> count = {iRowsB, iRowsA}; 
                    
                    for(hsize_t kk = 0; kk < K; kk += hdf5_block)
                    {
                        if( kk + hdf5_block > K ) local_ksize = K - kk;
                        
                        hsize_t iColsA = getOptimBlockSize( K, hdf5_block, kk, local_ksize),
                            iColsB = iColsA;
                        
                        // Pre-allocate data vectors outside critical section
                        std::vector<double> vdA(iRowsA * iColsA);
                        std::vector<double> vdB(iRowsB * iColsB);
                        
//                         // Thread-safe I/O: Serialize all HDF5 operations
// #ifdef _OPENMP
//                         if (bparal) omp_set_lock(&hdf5_lock);
// #endif
                        
                        // HDF5 read operations (inside critical section)
                        dsA->readDatasetBlock( {ii, kk}, {iRowsA,iColsA}, stride, block, vdA.data() );
                        dsB->readDatasetBlock( {jj, kk}, {iRowsB, iColsB}, stride, block, vdB.data() );
                        
// #ifdef _OPENMP
//                         if (bparal) omp_unset_lock(&hdf5_lock);
// #endif
                        
                        // Parallel computation (outside critical section)
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> A (vdA.data(), iRowsA, iColsA );
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> tmp_B (vdB.data(), iRowsB, iColsB);   
                        Eigen::MatrixXd B = tmp_B.transpose();
                        
                        C_accum.noalias() += A * B;
                        
                        // Readjust counters
                        if( kk + hdf5_block > K ) local_ksize = hdf5_block + 1;
                        if( iColsA > hdf5_block ) {
                            kk = kk - hdf5_block + iColsA; 
                        }
                    }
                    
//                     // Thread-safe I/O: Serialize all HDF5 write operations
// #ifdef _OPENMP
//                     if (bparal) omp_set_lock(&hdf5_lock);
// #endif
                    
                    dsC->writeDatasetBlock(Rcpp::wrap(C_accum), offset, count, stride, block, false);
                    
                    if (isSymmetric && ii != jj) {
                        std::vector<hsize_t> offset_sym = {ii, jj};
                        std::vector<hsize_t> count_sym = {iRowsA, iRowsB};
                        dsC->writeDatasetBlock(Rcpp::wrap(C_accum.transpose()), offset_sym, count_sym, stride, block, false);
                    }
                    
// #ifdef _OPENMP
//                     if (bparal) omp_unset_lock(&hdf5_lock);
// #endif
                }
                
            } else {
                throw std::range_error("non-conformable arguments");
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception crossprod: "<<ex.what()<< " \n";
            return(dsC);
        }
        
        return(dsC);
    }

}

#endif // BIGDATASTATMETH_ALGEBRA_CROSSPROD_HPP
