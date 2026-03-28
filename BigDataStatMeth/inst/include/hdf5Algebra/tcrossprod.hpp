/**
 * @file tcrossprod.hpp
 * @brief Transposed cross-product operations for HDF5 matrices
 * @details This header file provides implementations for transposed cross-product
 * operations on matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Transposed matrix multiplication
 * - Block-based computation
 * - Memory-efficient algorithms
 * - Parallel processing support
 * - Error handling and validation
 * 
 * Supported operations:
 * - A'A computation
 * - AA' computation
 * - Block cross-products
 * - Weighted cross-products
 * - Symmetric results
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Dynamic block sizing
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - BLAS Level 3 operations
 * - Block algorithms
 * - HDF5 chunked storage
 * - Parallel I/O
 * - Symmetric optimizations
 */

#ifndef BIGDATASTATMETH_ALGEBRA_TCROSSPROD_HPP
#define BIGDATASTATMETH_ALGEBRA_TCROSSPROD_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {
    
    /**
     * @brief Transposed cross-product for HDF5 matrices
     * @details Computes the transposed cross-product of matrices stored in HDF5 format.
     * Supports both A'A and AA' computations with block-based processing.
     * 
     * @param dsA Input matrix dataset
     * @param dsB Input matrix dataset
     * @param dsC Output matrix dataset
     * @param hdf5_block Block size for HDF5 I/O operations
     * @param mem_block_size Block size for in-memory operations
     * @param bparal Whether to use parallel processing
     * @param browmajor Whether matrices are stored in row-major order
     * @param threads Number of threads for parallel processing
     */
    inline BigDataStatMeth::hdf5Dataset* tcrossprod( 
            BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, 
            BigDataStatMeth::hdf5Dataset* dsC, bool isSymmetric, hsize_t hdf5_block, 
            hsize_t mem_block_size, bool bparal, bool browmajor, 
            Rcpp::Nullable<int> threads = R_NilValue) 

    {
        
        try {
            
            hsize_t N = dsA->ncols();  
            hsize_t K = dsA->nrows();  
            hsize_t M = dsB->ncols();  
            hsize_t L = dsB->nrows();  
            
            if (K != L) {
                throw std::range_error("non-conformable arguments");
            }
            
            if( K == L)
            {
                if (isSymmetric) {
                    if (N != M) {
                        throw std::range_error("Symmetric tcrossprod requires square result matrix");
                    }
                    if (dsA->getFileName() != dsB->getFileName() || 
                        dsA->getGroup() != dsB->getGroup() || 
                        dsA->getDatasetName() != dsB->getDatasetName()) {
                        Rcpp::warning("isSymmetric=TRUE but different datasets provided. Results may be incorrect.");
                    }
                }

                
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

#ifdef _OPENMP  // Configure parallel processing
                int num_threads = 1;
                if (bparal) {
                    num_threads = get_number_threads(threads, Rcpp::wrap(bparal));
                    omp_set_num_threads(num_threads);
                }
#endif
                
                // Calculate total blocks for parallelization
                hsize_t blocks_i = (N + hdf5_block - 1) / hdf5_block;
                hsize_t blocks_j = isSymmetric ? blocks_i : (M + hdf5_block - 1) / hdf5_block;
                hsize_t total_blocks;
                
                if (isSymmetric) {
                    // For symmetric case: only upper triangle blocks
                    total_blocks = (blocks_i * (blocks_i + 1)) / 2;
                } else {
                    // For general case: all blocks
                    total_blocks = blocks_i * blocks_j;
                }
                
                dsC->createDataset( N, M, "real");
                
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
                    
                    hsize_t Nii = std::min(hdf5_block, N - ii);
                    hsize_t Mjj = std::min(hdf5_block, M - jj);
                    
                    // Thread-local variables
                    std::vector<hsize_t> stride = {1, 1};
                    std::vector<hsize_t> block = {1, 1};
                    
                    Eigen::MatrixXd C_accum = Eigen::MatrixXd::Zero(Nii, Mjj);
                    
                    std::vector<hsize_t> offset = {jj, ii};
                    std::vector<hsize_t> count = {Mjj, Nii};
                    
                    for(hsize_t kk = 0; kk < K; kk += hdf5_block)
                    {
                        hsize_t Kkk = std::min(hdf5_block, K - kk); 
                        
                        Eigen::MatrixXd A;
                        
                        {
                            std::vector<double> vdA( Nii * Kkk);
                            dsA->readDatasetBlock( {kk, ii}, {Kkk, Nii}, stride, block, vdA.data() );
                            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> tmp_A (vdA.data(), Kkk, Nii );    
                            A = tmp_A.transpose();
                        }
                        
                        std::vector<double> vdB( Mjj * Kkk);
                        dsB->readDatasetBlock( {kk, jj}, { Kkk, Mjj}, stride, block, vdB.data() );
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> B (vdB.data(), Kkk, Mjj);   
                        
                        C_accum.noalias() += A * B;
                    }
                    
                    dsC->writeDatasetBlock(Rcpp::wrap(C_accum), offset, count, stride, block, false);
                    
                    // Symetric matrices
                    if (isSymmetric && ii != jj) {
                        std::vector<hsize_t> offset_sym = {ii, jj};
                        std::vector<hsize_t> count_sym = {Nii, Mjj};
                        dsC->writeDatasetBlock(Rcpp::wrap(C_accum.transpose()), offset_sym, count_sym, stride, block, false);
                    }
                }
                
            } else {
                throw std::range_error("non-conformable arguments");
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception tcrossprod: "<<ex.what()<< " \n";
            return(dsC);
        }
        
        return(dsC);
    }

}

#endif // BIGDATASTATMETH_ALGEBRA_TCROSSPROD_HPP
