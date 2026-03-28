/**
 * @file memSum.hpp
 * @brief Matrix addition and summation operations for in-memory computations
 * @details This header file provides comprehensive matrix addition and summation
 * functionality for in-memory computations. The implementation includes:
 * 
 * Key features:
 * - Matrix addition operations
 * - Element-wise summation
 * - Column/row-wise sums
 * - Block-based summation
 * - Parallel processing support
 * 
 * Supported operations:
 * - Matrix-matrix addition
 * - Matrix-scalar addition
 * - Column sums and means
 * - Row sums and means
 * - Weighted sums
 * - Running sums
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Block-based processing
 * - Thread-level parallelism
 * - Vectorized operations
 * - Memory access optimization
 */

#ifndef BIGDATASTATMETH_ALGEBRA_MEM_SUM_HPP
#define BIGDATASTATMETH_ALGEBRA_MEM_SUM_HPP

#include <RcppEigen.h>

namespace BigDataStatMeth {



    template< typename T>  inline Rcpp::RObject Rcpp_matrix_sum ( T  A, T  B);
    template< typename T, typename U>  inline Rcpp::RObject Rcpp_matrix_vect_sum ( T  A, U  B);
    template< typename T>  inline Rcpp::RObject Rcpp_vector_sum ( T  A, T  B);
    
    template< typename T>  inline Rcpp::RObject Rcpp_matrix_blockSum ( T  A, T  B, Rcpp::Nullable<int> threads = R_NilValue);
    template< typename T>  inline Rcpp::RObject Rcpp_matrix_vector_blockSum( T  A, T  B, Rcpp::Nullable<bool> bparal, Rcpp::Nullable<int> threads);

    // /**
    //  * @brief Low-level block-based matrix-vector addition implementation
    //  * @details Core implementation that:
    //  * - Processes matrix in cache-friendly blocks
    //  * - Supports parallel execution through OpenMP
    //  * - Optimizes memory access patterns
    //  * - Handles edge cases for non-uniform block sizes
    //  * 
    //  * @tparam T Matrix/vector type
    //  * @param A Input matrix
    //  * @param B Input vector
    //  * @param block_size Size of processing blocks
    //  * @param bparal Enable/disable parallel processing
    //  * @param threads Number of threads for parallel computation
    //  * @return Eigen::MatrixXd containing the result
    //  */
    // template< typename T>
    // inline Eigen::MatrixXd Rcpp_block_matrix_vector_sum( T  A, T  B, hsize_t block_size, 
    //                                               bool bparal, Rcpp::Nullable<int> threads = R_NilValue);



    // inline void getBlockPositionsSizes( hsize_t maxPosition, hsize_t blockSize, std::vector<hsize_t>& starts, std::vector<hsize_t>& sizes ){
    //     
    //     hsize_t isize = blockSize + 1;
    //     
    //     for (hsize_t ii = 0; ii < maxPosition; ii += blockSize)
    //     {
    //         if( ii + blockSize > maxPosition ) {
    //             isize = maxPosition - ii; }
    //         
    //         hsize_t sizetoRead = getOptimBlockSize( maxPosition, blockSize, ii, isize);
    //         
    //         starts.push_back(ii);
    //         sizes.push_back(sizetoRead);
    //         
    //         if( ii + blockSize > maxPosition ) isize = blockSize + 1;
    //         if( sizetoRead > blockSize ) {
    //             ii = ii - blockSize + sizetoRead; }
    //     }
    //     
    // }


    template< typename T>
    inline Rcpp::RObject Rcpp_matrix_sum ( T  A, T  B)
    {
        
        // static_assert(std::is_same<T, Eigen::MatrixXd >::value ||
        //               std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value ||
        //               std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value ||
        //               std::is_same<T, Rcpp::NumericMatrix >::value,
        //               "Error - type not allowed");
        
        Rcpp::NumericMatrix m = Rcpp::as<Rcpp::NumericMatrix>(A);
        Rcpp::NumericMatrix m2 = Rcpp::as<Rcpp::NumericMatrix>(B);
        
        if( m.rows() == m2.rows() && m.cols() == m2.cols()) {
            Rcpp::NumericVector C = m + m2;
            C.attr("dim") = Rcpp::Dimension( m.rows(), m.cols());
            
            return(C);
            
        } else {
            Rcpp::Rcout<<"Error: non-conformable arguments";
        }
        
        return(R_NilValue);
        
    }
    
    
    // Suma per files o columnes depenent si la mida del vector es igual al nombre
    // de files o igual al nombre de columnes
    template< typename T, typename U>
    inline Rcpp::RObject Rcpp_matrix_vect_sum ( T  A, U  B)
    {
        
        Rcpp::NumericMatrix m = Rcpp::as<Rcpp::NumericMatrix>(A);
        Rcpp::NumericVector v = Rcpp::as<Rcpp::NumericVector>(B);
        
        if( v.length() == m.rows()) {
            
            Rcpp::NumericMatrix C = Rcpp::no_init( m.rows(), m.cols());
            
            for( int i=0; i<m.cols(); i++) {
                C( Rcpp::_, i) = m( Rcpp::_, i) + v;  
            }    
            return(C);
            
        } else if( v.length() == m.cols()) {
            
            Rcpp::NumericMatrix C = Rcpp::no_init( m.rows(), m.cols());
            
            for( int i=0; i<m.rows(); i++) {
                C( i, Rcpp::_) = m( i, Rcpp::_) + v;  
            }    
            return(C);
            
        } else {
            Rcpp::Rcout<<"Error: non-conformable arguments";
        }
        
        return(R_NilValue);
    }


    template< typename T>
    inline Rcpp::RObject Rcpp_vector_sum ( T  A, T  B)
    {
        
        Rcpp::NumericVector v = Rcpp::as<Rcpp::NumericVector>(A);
        Rcpp::NumericVector v2 = Rcpp::as<Rcpp::NumericVector>(B);
        
        if(v.size() == v2.size()) {
            Rcpp::NumericVector C = Rcpp::no_init( v.size());
            
            std::transform (v.begin(), v.end(), v2.begin(), C.begin(), std::plus<double>());
            
            C.attr("dim") = Rcpp::Dimension( C.size(), 1); 
            
            return(C);
        }
        
        return(R_NilValue);
        
    }
    
    
    /**
     * @brief Block-based matrix addition implementation
     * @details Internal implementation of block-based matrix addition that processes
     * matrices in blocks for better cache utilization and memory efficiency.
     * 
     * This function:
     * - Determines optimal block sizes based on matrix dimensions
     * - Processes matrices in blocks to improve cache efficiency
     * - Supports parallel processing through OpenMP
     * - Handles edge cases for non-uniform block sizes
     * 
     * @tparam T Matrix type (typically Eigen::MatrixXd or similar)
     * @param A First input matrix
     * @param B Second input matrix
     * @param threads Optional number of threads for parallel processing
     * @return Rcpp::RObject containing the result matrix
     * @throws Runtime error if matrices are not conformable
     */
    template< typename T>
    inline Rcpp::RObject Rcpp_matrix_blockSum ( T  A, T  B, Rcpp::Nullable<int> threads)
    {
        
        // static_assert(std::is_same<T, Eigen::MatrixXd >::value || 
        //               std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
        //               std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value ,
        //               "Error - type not allowed");
        
        Rcpp::NumericMatrix X = Rcpp::as<Rcpp::NumericMatrix>(A);
        Rcpp::NumericMatrix Y = Rcpp::as<Rcpp::NumericMatrix>(B);
        // unsigned int ithreads;
        
        hsize_t N = X.rows();
        hsize_t M = X.cols();
        
        Rcpp::NumericMatrix C = Rcpp::no_init( N, M);
        
        hsize_t block_size; 
        
        try {
            
            std::vector<hsize_t> vsizetoRead;
            std::vector<hsize_t> vstart;
            
            std::vector<hsize_t> blockSize = getMatrixBlockSize( N, M);
            if(N < M) {
                block_size = blockSize.at(0);    
            } else {
                block_size = blockSize.at(1);
            }
            
            if(block_size > 0 ) {
                
                // if( N == Y.rows() && M == Y.cols())
                if (N == static_cast<hsize_t>(Y.rows()) && M == static_cast<hsize_t>(Y.cols())) 
                {
                    // hsize_t size = block_size + 1;
                    
                    // ithreads = get_number_threads(threads, R_NilValue);
                    
                    getBlockPositionsSizes( N*M, block_size, vstart, vsizetoRead );
                    // int chunks = vstart.size()/ithreads;
                    
                    #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared(A, B, C)
                    {
                    #pragma omp for schedule (dynamic)
                        for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                        {
                            
                            if( vstart[ii] + vsizetoRead[ii] >= N*M ) {
                                std::transform (X.begin() + vstart[ii], X.end(),
                                                Y.begin() + vstart[ii], C.begin() + vstart[ii], std::plus<double>());
                            } else {
                                std::transform (X.begin() + vstart[ii], X.begin() + vstart[ii] + vsizetoRead[ii],
                                                Y.begin() + vstart[ii], C.begin() + vstart[ii], std::plus<double>());   
                            }
                        }
                    }

                } else {
                    Rcpp::Rcout<<"matrix sum error: non-conformable arguments\n";
                    return(R_NilValue);
                }
                
            } else{
                Rcpp::Rcout<<"matrix sum error: Error whent computing block sizes\n";
                return(R_NilValue);
            }
            
        } catch(std::exception &ex) {
            Rcpp::Rcout<< ex.what();
            return(R_NilValue);
        }
        
        C.attr("dim") = Rcpp::Dimension( N, M);
        return(C);
        
    }
    
    
    
    
    /**
     * @brief Block-based matrix-vector addition with parallel processing
     * @details High-level interface for block-based matrix-vector addition that:
     * - Validates input dimensions
     * - Configures parallel processing based on input parameters
     * - Delegates to low-level implementation
     * 
     * @tparam T Matrix/vector type
     * @param A Input matrix
     * @param B Input vector
     * @param bparal Boolean flag to enable/disable parallel processing
     * @param threads Number of threads for parallel computation (if enabled)
     * @return Rcpp::RObject containing the result
     */
    template< typename T>
    inline Rcpp::RObject Rcpp_matrix_vector_blockSum( T  A, T  B,  
                                 Rcpp::Nullable<bool> bparal, Rcpp::Nullable<int> threads)
    {
        
        // NOTA: Per defecte, suma per columnes tal i com raja.... 
        
        // static_assert(std::is_same<T, Eigen::MatrixXd >::value || 
        //               std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
        //               std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
        //               "Error - type not allowed");
        
        bool btransposed = false;
        // unsigned int ithreads;
        hsize_t block_size;
        
        Rcpp::NumericMatrix X = Rcpp::as<Rcpp::NumericMatrix>(A);
        Rcpp::NumericVector Y = Rcpp::as<Rcpp::NumericVector>(B);
        Rcpp::NumericMatrix C;
        
        // Matrix
        hsize_t M = X.rows(),
                N = X.cols();
        
        // Vector
        hsize_t K = Y.length();
        
        try {
            
            if( K==N || K==M) {
                if ( K == N){
                    // Sum vector to every col
                    btransposed = true;
                    
                    X = Rcpp::transpose(X);
                    
                    //.Comentat 2024-10-29.// hsize_t N = X.rows();
                    //.Comentat 2024-10-29.// hsize_t M = X.cols();
                    N = X.rows();
                    M = X.cols();
                } 
                
                std::vector<hsize_t> vsizetoRead;
                std::vector<hsize_t> vstart;
                
                // ithreads = get_number_threads(threads, bparal);
                
                C = Rcpp::no_init( M, N);
                
                block_size = getMatrixBlockSize( N, M).at(0);
                
                // minimum block size: 2 columns
                if(block_size <= 0 ) {
                    block_size = M*2;
                }
                
                // Mínimum block size: 2 columns
                getBlockPositionsSizes( M*N, block_size, vstart, vsizetoRead );
                // int chunks = vstart.size()/ithreads;
                
                #pragma omp parallel num_threads( get_number_threads(threads, bparal) ) shared(A, B, C) //, chunks)
                {
                    #pragma omp for schedule (dynamic) // collapse(2)
                    for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                    {
                        // Duplicate vector
                        std::size_t const no_of_duplicates = vsizetoRead[ii] / Y.length();
                        
                        std::vector<double> v = Rcpp::as<std::vector<double> >(Y); 
                        v.reserve(Y.size() * no_of_duplicates);
                        auto end = std::end(v);
                        
                        for(std::size_t i = 1; i < no_of_duplicates; ++i)
                            v.insert(std::end(v), std::begin(v), end);
                        
                        // Sum vector to matrix by columns / rows
                        if( vstart[ii] + vsizetoRead[ii] >= M*N ) {
                            std::transform (X.begin() + vstart[ii], X.end(),
                                            v.begin(), C.begin() + vstart[ii], std::plus<double>());
                        } else {
                            std::transform (X.begin() + vstart[ii], X.begin() + vstart[ii] + vsizetoRead[ii],
                                            v.begin() , C.begin() + vstart[ii], std::plus<double>());   
                        }
                    }
                }
            
            } else {
                
                Rcpp::Rcout<< "vector sum error: non-conformable arguments\n";
                return(R_NilValue);
            }
            
    
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception Rcpp_matrix_vector_blockSum: "<<ex.what()<< " \n";
            return(R_NilValue);
        }
        
        if(btransposed == true){
            Rcpp::transpose(C);
        } 
        
        C.attr("dim") = Rcpp::Dimension( M, N);
        return(C);
    
    }

}

#endif // BIGDATASTATMETH_ALGEBRA_MSUM_HPP
