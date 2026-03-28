/**
 * @file matrixSum.hpp
 * @brief Matrix addition operations for HDF5 matrices
 * @details This header file provides implementations for matrix addition
 * operations on matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Matrix-matrix addition
 * - Matrix-vector addition
 * - Block-based computation
 * - Memory-efficient algorithms
 * - Parallel processing support
 * 
 * Supported operations:
 * - Element-wise addition
 * - Block-based addition
 * - Row/column vector addition
 * - Multi-threaded addition
 * - In-place operations
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Dynamic block sizing
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - Block-based algorithms
 * - HDF5 chunked storage
 * - Parallel processing
 * - Vectorized operations
 * - Memory-efficient I/O
 */

#ifndef BIGDATASTATMETH_ALGEBRA_SUM_HPP
#define BIGDATASTATMETH_ALGEBRA_SUM_HPP


#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {


    inline BigDataStatMeth::hdf5Dataset*  Rcpp_block_matrix_sum_hdf5( 
            BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
            hsize_t hdf5_block, hsize_t mem_block_size, bool bparal, Rcpp::Nullable<int> threads);
    
    inline BigDataStatMeth::hdf5Dataset* Rcpp_block_matrix_vector_sum_hdf5( 
            BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
            hsize_t hdf5_block, bool bparal, Rcpp::Nullable<int> threads);
    
    
    

    /**
     * @brief Block-based matrix addition for HDF5 matrices
     * @details Performs block-based matrix addition C = A + B where A, B, and C
     * are HDF5 datasets. Optimized for large matrices with parallel processing.
     * 
     * @param dsA First input matrix dataset
     * @param dsB Second input matrix dataset
     * @param dsC Output matrix dataset
     * @param hdf5_block Block size for HDF5 I/O operations
     * @param bparal Whether to use parallel processing
     * @param threads Number of threads for parallel processing (optional)
     * @return Pointer to result dataset
     */
    inline BigDataStatMeth::hdf5Dataset*  Rcpp_block_matrix_sum_hdf5( 
            BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
            hsize_t hdf5_block, bool bparal, Rcpp::Nullable<int> threads  = R_NilValue)
    {
        
        try {
            
            hsize_t K = dsA->nrows();
            hsize_t N = dsA->ncols();
            
            if( K == dsB->nrows() && N == dsB->ncols())
            {
                
                // Parallellization and Block variables 
                // unsigned int ithreads;
                std::vector<hsize_t> vstart, vsizetoRead;
                std::vector<hsize_t> stride = {1, 1};
                std::vector<hsize_t> block = {1, 1};
                
                dsC->createDataset( N, K, "real"); 
                
                // ithreads = get_threads(bparal, threads);
                
                if( K<=N ) {
                    
                    getBlockPositionsSizes( N, hdf5_block, vstart, vsizetoRead );
                    // int chunks = vstart.size()/ithreads;
                    
                    #pragma omp parallel num_threads( get_threads(bparal, threads) ) shared(dsA, dsB, dsC) //, chunks)
                    {
                    #pragma omp for schedule (dynamic)
                        for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                        {
                            
                            std::vector<double> vdA( K * vsizetoRead[ii] ); 
                            #pragma omp critical(accessFile)
                            {
                                dsA->readDatasetBlock( {0, vstart[ii]}, { K, vsizetoRead[ii]}, stride, block, vdA.data() );
                            }
                            
                            std::vector<double> vdB( K * vsizetoRead[ii] ); 
                            #pragma omp critical(accessFile)
                            {
                                dsB->readDatasetBlock( {0, vstart[ii]}, {K, vsizetoRead[ii]}, stride, block, vdB.data() );
                            }
                            std::transform (vdA.begin(), vdA.end(),
                                            vdB.begin(), vdA.begin(), std::plus<double>());
                            
                            std::vector<hsize_t> offset = { 0, vstart[ii] };
                            std::vector<hsize_t> count = { K, vsizetoRead[ii] };
                            #pragma omp critical(accessFile) 
                            {
                                dsC->writeDatasetBlock(vdA, offset, count, stride, block);
                            }
                        }
                    }
                    
                } else {
                    
                    getBlockPositionsSizes( K, hdf5_block, vstart, vsizetoRead );
                    // int chunks = vstart.size()/ithreads;
                    
                    #pragma omp parallel num_threads( get_threads(bparal, threads) ) shared(dsA, dsB, dsC) //, chunks)
                    {
                    #pragma omp for schedule (dynamic)
                        for (hsize_t ii = 0; ii < vstart.size(); ii++)
                        {
                            std::vector<double> vdA( vsizetoRead[ii] * N ); 
                            #pragma omp critical(accessFile)
                            {
                                dsA->readDatasetBlock( {vstart[ii], 0}, { vsizetoRead[ii], N}, stride, block, vdA.data() );
                            }
                            
                            std::vector<double> vdB( vsizetoRead[ii] * N); 
                            #pragma omp critical(accessFile)
                            {
                                dsB->readDatasetBlock( {vstart[ii], 0}, {vsizetoRead[ii], N}, stride, block, vdB.data() );
                            }
                            
                            std::transform (vdA.begin(), vdA.end(),
                                            vdB.begin(), vdA.begin(), std::plus<double>());
                            
                            std::vector<hsize_t> offset = { vstart[ii], 0 };
                            std::vector<hsize_t> count = { vsizetoRead[ii], N };
                            #pragma omp critical 
                            {
                                dsC->writeDatasetBlock(vdA, offset, count, stride, block);
                            }
                        }
                    }
                }
            } else {
                Rcpp::Rcout<<"matrix sum error: non-conformable arguments\n";
            }
            
        } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_sum_hdf5 (File IException)";
            return(dsC);
        } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_sum_hdf5 (Group IException)";
            return(dsC);
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_sum_hdf5 (DataSet IException)";
            return(dsC);
        } catch(std::exception& ex) {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_sum_hdf5: " << ex.what();
            return(dsC);
        } catch (...) {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nC++ exception Rcpp_block_matrix_sum_hdf5 (unknown reason)";
            return(dsC);
        }
        
        return(dsC);
    }
    
    
    

/**
 * @brief Block-based matrix-vector addition for HDF5 matrices
 * @details Performs block-based matrix-vector addition where one operand
 * is a vector and the other is a matrix. Supports both row and column vectors.
 * 
 * @param dsA Input vector dataset
 * @param dsB Input matrix dataset
 * @param dsC Output matrix dataset
 * @param hdf5_block Block size for HDF5 I/O operations
 * @param bparal Whether to use parallel processing
 * @param threads Number of threads for parallel processing (optional)
 * @return Pointer to result dataset
 */
    inline BigDataStatMeth::hdf5Dataset* Rcpp_block_matrix_vector_sum_hdf5( 
            BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsC,
            hsize_t hdf5_block, bool bparal, Rcpp::Nullable<int> threads  = R_NilValue)
    {
    
        try {
            
            // Vector
            hsize_t K = dsA->nrows();
            hsize_t N = dsA->ncols();
            
            // Matrix
            hsize_t M = dsB->nrows();
            hsize_t L = dsB->ncols();
            
            std::vector<hsize_t> vstart, vsizetoRead;
            // unsigned int ithreads;
    
            if(hdf5_block == 1) {
                hdf5_block = ceil(MAXBLOCKSIZE/(K*N));
            }
    
            // hsize_t isize = hdf5_block + 1;
            std::vector<hsize_t> stride = {1, 1},
                                 block = {1, 1};
            
            dsC->createDataset( L, M, "real");
            
            std::vector<double> vdA( K * N );
            dsA->readDatasetBlock( {0, 0}, {K, N}, stride, block, vdA.data() );
            
            // ithreads = get_threads(bparal, threads);
            
            if(  K == M )
            { // Sum vector to every col
                
                getBlockPositionsSizes( L, hdf5_block, vstart, vsizetoRead );
                // int chunks = vstart.size()/ithreads;
                
                #pragma omp parallel num_threads( get_threads(bparal, threads) ) shared(dsA, dsB, dsC) //, chunks)
                {
                    #pragma omp for schedule (dynamic) // collapse(2)
                    for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                    {
                        std::vector<double> vdB( K * vsizetoRead[ii] );
                        #pragma omp critical(accessFile)
                        {
                            dsB->readDatasetBlock( {0, vstart[ii]}, {K, vsizetoRead[ii]}, stride, block, vdB.data() );
                        }
                        
                        // Duplicate vector
                        std::size_t const no_of_duplicates = (K * vsizetoRead[ii]) / vdA.size();
                        
                        std::vector<double> v = vdA; 
                        v.reserve(vdA.size() * no_of_duplicates);
                        auto end = std::end(v);
                        
                        for(std::size_t i = 1; i < no_of_duplicates; ++i)
                            v.insert(std::end(v), std::begin(v), end);
                        
                        // Sum vector to matrix by columns / rows
                        std::transform (vdB.begin(), vdB.end(),
                                        v.begin(), vdB.begin(), std::plus<double>());
                        
                        std::vector<hsize_t> offset = { 0, vstart[ii]};
                        std::vector<hsize_t> count = { K, vsizetoRead[ii]};
                        
                        #pragma omp critical(accessFile)
                        {
                            dsC->writeDatasetBlock( vdB, offset, count, stride, block);
                        }
                    }
                }
                
    
            } else if(  K == L ) { // Sum vector to every row
                
                getBlockPositionsSizes( M, hdf5_block, vstart, vsizetoRead );
                // int chunks = vstart.size()/ithreads;
                
                #pragma omp parallel num_threads( get_threads(bparal, threads) ) shared(dsA, dsB, dsC) //, chunks)
                {
                    #pragma omp for schedule (dynamic) // collapse(2)
                    for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                    {
                        std::vector<double> vdB( L * vsizetoRead[ii] );
                        #pragma omp critical (accessFile)
                        {
                            dsB->readDatasetBlock( {vstart[ii], 0}, {vsizetoRead[ii], K}, stride, block, vdB.data() );
                        }
                        Rcpp::NumericMatrix B (vsizetoRead[ii], K, vdB.begin());
                        
                        Rcpp::transpose(B);
                        
                        // Duplicate vector
                        std::size_t const no_of_duplicates = (vsizetoRead[ii] * K) / vdA.size();
                        
                        std::vector<double> v = vdA; 
                        v.reserve(v.size() * no_of_duplicates);
                        auto end = std::end(v);
                        
                        for(std::size_t i = 1; i < no_of_duplicates; ++i)
                            v.insert(std::end(v), std::begin(v), end);
                        
                        // Sum vector to matrix by columns / rows
                        std::transform (B.begin(), B.end(),
                                        v.begin(), B.begin() , std::plus<double>());
                    
                        std::vector<hsize_t> offset = { vstart[ii], 0};
                        std::vector<hsize_t> count = { vsizetoRead[ii], K};
                        
                        #pragma omp critical (accessFile)
                        {
                            dsC->writeDatasetBlock( vdB, offset, count, stride, block);
                        }
                    }
                }
            } else {
                Rcpp::Rcout<< "vector sum error: non-conformable arguments\n";
            }
    
        } catch( H5::FileIException& error ) { 
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_vector_sum_hdf5 (File IException)";
            return(dsC);
        } catch( H5::GroupIException & error ) { 
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_vector_sum_hdf5 (Group IException)";
            return(dsC);
        } catch( H5::DataSetIException& error ) { 
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_vector_sum_hdf5 (DataSet IException)";
            return(dsC);
        } catch(std::exception& ex) {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nc++ exception Rcpp_block_matrix_vector_sum_hdf5: " << ex.what();
            return(dsC);
        } catch (...) {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"\nC++ exception Rcpp_block_matrix_vector_sum_hdf5 (unknown reason)";
            return(dsC);
        }
        
        return(dsC);
    }

}

#endif // BIGDATASTATMETH_ALGEBRA_SUM_HPP
