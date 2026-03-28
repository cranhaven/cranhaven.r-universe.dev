/**
 * @file multiplication.hpp
 * @brief Matrix multiplication operations for HDF5 matrices
 * @details This header file provides implementations for matrix multiplication
 * operations on matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Dense matrix multiplication
 * - Block-based multiplication
 * - Parallel processing support
 * - Memory-efficient algorithms
 * - Error handling and validation
 * 
 * Supported operations:
 * - Matrix-matrix multiplication
 * - Block matrix multiplication
 * - Transposed multiplication
 * - Multi-threaded multiplication
 * - Out-of-core processing
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
 * - Cache blocking
 */

#ifndef BIGDATASTATMETH_HDF5_MULTIPLICATION_HPP
#define BIGDATASTATMETH_HDF5_MULTIPLICATION_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {


/**
 * @brief Main matrix multiplication function for HDF5 matrices
 * @details Performs matrix multiplication C = A * B where A, B, and C are HDF5 datasets.
 * Supports parallel processing and block-based computation for memory efficiency.
 * 
 * @param dsA First input matrix dataset
 * @param dsB Second input matrix dataset
 * @param dsC Output matrix dataset
 * @param transpose_A Whether to transpose matrix A
 * @param transpose_B Whether to transpose matrix B
 * @param bparal Whether to use parallel processing
 * @param hdf5_block Block size for HDF5 I/O operations
 * @param threads Number of threads for parallel processing
 */
// inline void multiplication( BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
//                                    Rcpp::Nullable<bool> bparal, Rcpp::Nullable<int> hdf5_block, Rcpp::Nullable<int> threads);

inline void multiplication( BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
                            bool transpose_A, bool transpose_B, Rcpp::Nullable<bool> bparal, Rcpp::Nullable<int> hdf5_block, Rcpp::Nullable<int> threads);


/**
 * @brief Calculate block positions and sizes for HDF5 matrix operations
 * @details Determines optimal block positions and sizes for block-based matrix
 * operations on HDF5 datasets.
 * 
 * @param maxPosition Maximum position to process
 * @param blockSize Size of each block
 * @param[out] starts Vector to store starting positions of blocks
 * @param[out] sizes Vector to store sizes of blocks
 */
inline void getBlockPositionsSizes_hdf5( hsize_t maxPosition, hsize_t blockSize, std::vector<hsize_t>& starts, std::vector<hsize_t>& sizes ){

        hsize_t isize = blockSize + 1;

        for (hsize_t ii = 0; ii < maxPosition; ii += blockSize)
        {
            if( ii + blockSize > maxPosition ) {
                isize = maxPosition - ii; }

            hsize_t sizetoRead = getOptimBlockSize( maxPosition, blockSize, ii, isize);

            starts.push_back(ii);
            sizes.push_back(sizetoRead);

            // if( ii + blockSize > maxPosition ) {
            //     isize = blockSize + 1; }
            if( sizetoRead > blockSize ) {
                ii = ii - blockSize + sizetoRead; }
        }

    }


    /**
     * @brief Calculate optimal block sizes specifically for multiplication
     * @param N Number of result rows (dsA->ncols)
     * @param M Number of result cols (dsB->nrows)  
     * @param K Inner dimension (dsA->nrows = dsB->ncols)
     */
    inline BlockSizes calculate_multiplication_blocks(hsize_t N, hsize_t M, hsize_t K) {
        const hsize_t MEMORY_BUDGET = (hsize_t)(1.5 * 1024 * 1024 * 1024);  // 1.5GB
        const hsize_t bytes_per_element = sizeof(double);
        const hsize_t min_block = 256;
        
        BlockSizes result;
        result.type = classify_matrix_type(N, M, K);
        
        switch (result.type) {
        case RECTANGULAR_EXTREME: {
            // Para big-omics: K >> N,M - priorizar bloques grandes en K
            hsize_t small_dim = std::min(N, M);
            result.output_block = std::min(small_dim, (hsize_t)1024);
            
            // Maximizar bloque K con memoria restante
            // Memory = A_block + B_block = K×(N+M) aprox
            hsize_t remaining_budget = (hsize_t)(0.8 * MEMORY_BUDGET);
            result.inner_block = remaining_budget / ((N + M) * bytes_per_element);
            result.inner_block = std::min({result.inner_block, K, (hsize_t)32768});
            break;
        }
            
        case SQUARE_SMALL: {
            hsize_t block = sqrt(MEMORY_BUDGET / (3 * bytes_per_element));
            block = std::min({block, N/2, M/2, K/2});
            result.inner_block = result.output_block = block;
            break;
        }
            
        case SQUARE_LARGE: {
            hsize_t target_blocks = 2;  // 2×2 output blocks
            result.output_block = std::max({N / target_blocks, M / target_blocks, min_block});
            
            hsize_t output_memory = result.output_block * result.output_block * bytes_per_element;
            hsize_t remaining_budget = MEMORY_BUDGET - output_memory;
            result.inner_block = remaining_budget / (2 * result.output_block * bytes_per_element);
            result.inner_block = std::min({result.inner_block, K, (hsize_t)16384});
            break;
        }
            
        case SQUARE_EXTREME: {
            hsize_t block = sqrt(MEMORY_BUDGET / (3 * bytes_per_element));
            block = std::min({block, (hsize_t)8192});
            result.inner_block = result.output_block = block;
            break;
        }
        }
        
        // Aplicar límites y redondear
        result.inner_block = std::max(result.inner_block, min_block);
        result.output_block = std::max(result.output_block, min_block);
        
        result.inner_block = ((result.inner_block + 63) / 64) * 64;
        result.output_block = ((result.output_block + 63) / 64) * 64;
        
        return result;
    }

    // In-memory execution - Parallel version
    // 
    //  IMPORTANT : FUNCIÓ MODIFICADA EL 2024/04/06  I NO TESTEJADA !!!!
    // 

    /**
     * @brief Parallel block-based matrix multiplication
     * @details Implements parallel block-based matrix multiplication for in-memory matrices.
     * 
     * @param A First input matrix
     * @param B Second input matrix
     * @param block_size Size of blocks for computation
     * @param threads Number of threads for parallel processing
     * @return Result of matrix multiplication
     */
    inline Eigen::MatrixXd Bblock_matrix_mul_parallel( Eigen::MatrixXd A, Eigen::MatrixXd B, 
                                                             int block_size, Rcpp::Nullable<int> threads  = R_NilValue)
    {
        
        // unsigned int ithreads;
        Eigen::MatrixXd C;
        
        try {

            int M = A.rows();
            int K = A.cols();
            int N = B.cols();
            
            C = Eigen::MatrixXd::Zero(M,N) ;
            
            if( A.rows() == B.cols())
            {
                
                if(block_size > std::min( N, std::min(M,K)) )
                    block_size = std::min( N, std::min(M,K)); 
                
                std::vector<hsize_t> vsizetoRead, vstart,
                                     vsizetoReadM, vstartM,
                                     vsizetoReadK, vstartK;
                
                getBlockPositionsSizes_hdf5( N, block_size, vstart, vsizetoRead );
                getBlockPositionsSizes_hdf5( M, block_size, vstartM, vsizetoReadM );
                getBlockPositionsSizes_hdf5( K, block_size, vstartK, vsizetoReadK );
                
                // int ithreads = get_number_threads(threads, R_NilValue);
                // int chunks = vstart.size()/ithreads;
                
                #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared(A, B, C) //..// , chunk) private(tid ) 
                {
                    
                    #pragma omp for schedule (static) // collapse(3)
                    for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                    {
                        for (hsize_t jj = 0; jj < vstartM.size(); jj++)
                        {
                            for (hsize_t kk = 0; kk < vstartK.size(); kk++)
                            {
                                C.block(vstart[ii], vstartM[jj], vsizetoRead[ii], vsizetoReadM[jj]) = 
                                    C.block(vstart[ii], vstartM[jj], vsizetoRead[ii], vsizetoReadM[jj]) + 
                                    ( A.block(vstart[ii], vstartK[kk], vsizetoRead[ii], vsizetoReadK[kk]) * 
                                      B.block(vstartK[kk], vstartM[jj], vsizetoReadK[kk], vsizetoReadM[jj]) );
                            }
                        }
                    }
                }
                
            } else {
                throw std::range_error("multiplication error: non-conformable arguments");
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcerr<< "c++ exception Bblock_matrix_mul_parallel: "<<ex.what()<< " \n";
        }
        
        return(C);
        
    }
    

    inline void multiplication( BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
                                bool transpose_A, bool transpose_B, Rcpp::Nullable<bool> bparal, Rcpp::Nullable<int> hdf5_block, Rcpp::Nullable<int> threads = R_NilValue) 
    {
        
        try {
        
            // int ihdf5_block;
            // hsize_t K = dsA->nrows();
            // hsize_t N = dsA->ncols();
            // hsize_t L = dsB->ncols();
            // hsize_t M = dsB->nrows();
            // 
             
             hsize_t K, N, L, M;
            
            if (transpose_A) {
                K = dsA->ncols();  // t(A): filas de A se vuelven columnas
                N = dsA->nrows();  // t(A): columnas de A se vuelven filas
            } else {
                K = dsA->nrows();  // A normal
                N = dsA->ncols();
            }
            
            if (transpose_B) {
                L = dsB->nrows();  // t(B): columnas de B se vuelven filas
                M = dsB->ncols();  // t(B): filas de B se vuelven columnas
            } else {
                L = dsB->ncols();  // B normal
                M = dsB->nrows();
            }
            
             // if( hdf5_block.isNotNull()) {
             //     ihdf5_block =  Rcpp::as<int>(hdf5_block);
             // } else {
             //     ihdf5_block =  MAXBLOCKSIZE/3;
             // }

             
             int ihdf5_block_N, ihdf5_block_M, ihdf5_block_K;
             
             if( hdf5_block.isNotNull()) {
                 ihdf5_block_N = ihdf5_block_M = ihdf5_block_K = Rcpp::as<int>(hdf5_block);
             } else {
                 BlockSizes blocks = calculate_multiplication_blocks(N, M, K);
                 ihdf5_block_N = ihdf5_block_M = blocks.output_block;
                 ihdf5_block_K = blocks.inner_block;
             }
             
             
            if( K == L )
            {

                std::vector<hsize_t> stride = {1, 1},
                                     block = {1, 1},
                                     vsizetoRead, vstart,
                                     vsizetoReadM, vstartM,
                                     vsizetoReadK, vstartK;
                
                dsC->createDataset( N, M, "real");
                
                if( dsC->getDatasetptr() != nullptr) {

                    
                    getBlockPositionsSizes_hdf5( N, ihdf5_block_N, vstart, vsizetoRead );
                    getBlockPositionsSizes_hdf5( M, ihdf5_block_M, vstartM, vsizetoReadM );
                    getBlockPositionsSizes_hdf5( K, ihdf5_block_K, vstartK, vsizetoReadK );
                    
                    // getBlockPositionsSizes_hdf5( N, ihdf5_block, vstart, vsizetoRead );
                    // getBlockPositionsSizes_hdf5( M, ihdf5_block, vstartM, vsizetoReadM );
                    // getBlockPositionsSizes_hdf5( K, ihdf5_block, vstartK, vsizetoReadK );
                    
                    
                    // int ithreads = get_number_threads(threads, R_NilValue);
                    // int chunks = vstart.size()/ithreads;
                    
                    #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared(dsA, dsB, dsC, vstart, vsizetoRead) // chunks
                    {
                        
                        #pragma omp for schedule(dynamic) nowait
                        for (hsize_t ii = 0; ii < vstart.size(); ii++)
                        {
                            for (hsize_t jj = 0; jj < vstartM.size(); jj++)
                            {
                                
                                Eigen::MatrixXd C_accumulator = Eigen::MatrixXd::Zero(vsizetoReadM[jj], vsizetoRead[ii]);
                                
                                for (hsize_t kk = 0; kk < vstartK.size(); kk++)
                                {
                                    hsize_t iColsA = vsizetoReadK[kk],
                                            iRowsA = vsizetoRead[ii],
                                            iColsB = vsizetoReadM[jj],
                                            iRowsB = vsizetoReadK[kk];
                                    
                                    std::vector<double> vdA( iRowsA * iColsA );
                                    #pragma omp critical(accessFile)
                                    {
                                        dsA->readDatasetBlock( {vstartK[kk], vstart[ii]}, {vsizetoReadK[kk], vsizetoRead[ii]}, stride, block, vdA.data() );
                                    }
                                    
                                    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> A (vdA.data(), vsizetoReadK[kk], vsizetoRead[ii] );
                                    
                                    std::vector<double> vdB( iRowsB * iColsB );
                                    #pragma omp critical(accessFile)
                                    {
                                        dsB->readDatasetBlock( {vstartM[jj], vstartK[kk]}, {vsizetoReadM[jj], vsizetoReadK[kk]}, stride, block, vdB.data() );
                                    }
                                    
                                    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> B (vdB.data(), vsizetoReadM[jj], vsizetoReadK[kk] );
                                    
                                    // C_accumulator += B * A;
                                    // Operación según flags de transposición
                                    if (!transpose_A && !transpose_B) {
                                        C_accumulator += B * A;                           // A * B
                                    } else if (transpose_A && !transpose_B) {
                                        C_accumulator += B * A.transpose();               // t(A) * B
                                    } else if (!transpose_A && transpose_B) {
                                        C_accumulator += B.transpose() * A;               // A * t(B)
                                    } else {
                                        C_accumulator += B.transpose() * A.transpose();   // t(A) * t(B)
                                    }
                                }
                            
                                std::vector<double> vdC_final(vsizetoReadM[jj] * vsizetoRead[ii]);
                                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> C_final_map(vdC_final.data(), vsizetoReadM[jj], vsizetoRead[ii]);
                                C_final_map = C_accumulator;
                                
                                std::vector<hsize_t> offset = {vstartM[jj], vstart[ii]};
                                std::vector<hsize_t> count = {vsizetoReadM[jj], vsizetoRead[ii]};
                                                                
                                #pragma omp critical(accessFile)
                                {
                                    dsC->writeDatasetBlock(vdC_final, offset, count, stride, block);
                                }
                            }
                        }
                        
                    // #pragma omp for schedule(dynamic) nowait
                    //     for (hsize_t ii = 0; ii < vstart.size(); ii++)
                    //     {
                    //         
                    //         for (hsize_t jj = 0; jj < vstartM.size(); jj++)
                    //         {
                    //             
                    //             for (hsize_t kk = 0; kk < vstartK.size(); kk++)
                    //             {
                    //                 
                    //                 hsize_t iColsA = vsizetoReadK[kk],
                    //                         iRowsA = vsizetoRead[ii],
                    //                         iColsB = vsizetoReadM[jj],
                    //                         iRowsB = vsizetoReadK[kk];
                    //                 
                    //                 std::vector<double> vdA( iRowsA * iColsA );
                    //                 #pragma omp critical(accessFile)
                    //                 {
                    //                     dsA->readDatasetBlock( {vstartK[kk], vstart[ii]}, {vsizetoReadK[kk], vsizetoRead[ii]}, stride, block, vdA.data() );
                    //                 }
                    //                 
                    //                 Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> A (vdA.data(), vsizetoReadK[kk], vsizetoRead[ii] );
                    //                 
                    //                 std::vector<double> vdB( iRowsB * iColsB );
                    //                 #pragma omp critical(accessFile)
                    //                 {
                    //                     dsB->readDatasetBlock( {vstartM[jj], vstartK[kk]}, {vsizetoReadM[jj], vsizetoReadK[kk]}, stride, block, vdB.data() );
                    //                 }
                    //                 
                    //                 Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> B (vdB.data(), vsizetoReadM[jj], vsizetoReadK[kk] );
                    //                 
                    //                 std::vector<double> vdC( vsizetoReadM[jj] * vsizetoRead[ii] );
                    //                 #pragma omp critical(accessFile)
                    //                 {
                    //                     dsC->readDatasetBlock( {vstartM[jj], vstart[ii]}, {vsizetoReadM[jj],  vsizetoRead[ii]}, stride, block, vdC.data() );
                    //                 }
                    // 
                    //                 Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> C (vdC.data(), B.rows(), A.cols() );
                    // 
                    //                 C = C + B * A;
                    // 
                    //                 std::vector<hsize_t> offset = {vstartM[jj], vstart[ii]};
                    //                 std::vector<hsize_t> count = {vsizetoReadM[jj], vsizetoRead[ii] };
                    //                 
                    //                 #pragma omp critical(accessFile)
                    //                 {
                    //                     dsC->writeDatasetBlock(vdC, offset, count, stride, block);
                    //                 }
                    //             }
                    //         }
                    //     }
                    }
                } 
                
            } else {
                throw std::range_error("multiplication error: non-conformable arguments");
            }

        }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ c++ exception multiplication (File IException)\n";
            // return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception multiplication (DataSet IException)\n";
            // return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception multiplication " << ex.what();
            // return void();
        }  catch (...) {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nC++ exception multiplication (unknown reason)";
            // return void();
        }

        return void();
    }
    
}

#endif // BIGDATASTATMETH_ALGEBRA_MULTIPLICATION_HPP
