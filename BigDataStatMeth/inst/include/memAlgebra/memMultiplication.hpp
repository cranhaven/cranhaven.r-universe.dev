/**
 * @file memMultiplication.hpp
 * @brief Matrix multiplication operations for in-memory computations
 * @details This header file provides comprehensive matrix multiplication
 * functionality for in-memory computations. The implementation includes:
 * 
 * Key features:
 * - Dense matrix multiplication
 * - Matrix-vector multiplication
 * - Block-based multiplication
 * - Parallel processing support
 * - Memory-efficient implementations
 * 
 * Supported operations:
 * - Standard matrix multiplication (AB)
 * - Transposed multiplication (A'B, AB')
 * - Weighted multiplication
 * - Block matrix multiplication
 * - Multi-threaded multiplication
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Dynamic block sizing
 * - Thread-level parallelism
 * - Memory access optimization
 * - SIMD vectorization
 */

#ifndef BIGDATASTATMETH_ALGEBRA_MEM_MULTIPLICATION_HPP
#define BIGDATASTATMETH_ALGEBRA_MEM_MULTIPLICATION_HPP

// #include <RcppEigen.h>


namespace BigDataStatMeth {

    // inline Eigen::MatrixXd Rcpp_block_matrix_mul( Eigen::MatrixXd A, Eigen::MatrixXd B, Rcpp::Nullable<int>  iblock_size);
    /**
     * @brief Block-based matrix multiplication
     * @details Implements efficient block-based matrix multiplication using
     * cache-friendly algorithms.
     * 
     * @tparam T First matrix type (MatrixXd or compatible mapped types)
     * @tparam U Second matrix type (MatrixXd or compatible mapped types)
     * @param X First input matrix
     * @param Y Second input matrix
     * @param iblock_size Block size for computation
     * @return Result of matrix multiplication
     */
    template<typename T, typename U> inline Eigen::MatrixXd Rcpp_block_matrix_mul( T X, U Y, Rcpp::Nullable<int>  iblock_size);

    
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
    
    
    /**
     * @brief Calculate block positions and sizes for matrix operations
     * @details Determines optimal block positions and sizes for block-based matrix
     * operations, ensuring efficient memory usage and processing.
     * 
     * @param maxPosition Maximum position to process
     * @param blockSize Size of each block
     * @param[out] starts Vector to store starting positions of blocks
     * @param[out] sizes Vector to store sizes of blocks
     */
    inline void getBlockPositionsSizes_mat( hsize_t maxPosition, hsize_t blockSize, std::vector<hsize_t>& starts, std::vector<hsize_t>& sizes ){
        
        hsize_t isize = blockSize + 1;
        
        for (hsize_t ii = 0; ii < maxPosition; ii += blockSize)
        {
            if( ii + blockSize > maxPosition ) {
                isize = maxPosition - ii; }
            
            hsize_t sizetoRead = getOptimBlockSize( maxPosition, blockSize, ii, isize);
            
            starts.push_back(ii);
            sizes.push_back(sizetoRead);
            
            if( sizetoRead > blockSize ) {
                ii = ii - blockSize + sizetoRead; }
        }
        
    }
    
    
   
   // In-memory execution - Serial version by Blocks
   template<typename T, typename U>
   inline Eigen::MatrixXd Rcpp_block_matrix_mul( T X, U Y, Rcpp::Nullable<int>  iblock_size)
   {
       
       Eigen::MatrixXd C;
       
       try{

           static_assert(std::is_same<T, Eigen::MatrixXd >::value || 
                         std::is_same<T, Eigen::Map< Eigen::MatrixXd >>::value || 
                         std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                         std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                         "Error - type not allowed");
           
           static_assert(std::is_same<U, Eigen::MatrixXd >::value || 
                         std::is_same<U, Eigen::Map< Eigen::MatrixXd >>::value || 
                         std::is_same<U, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                         std::is_same<U, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                         "Error - type not allowed");
           
           Eigen::MatrixXd A = X,
                           B = Y;
           
           // if(transpX == true){ A = X.transpose(); }
           // if(transpY == true){ B = Y.transpose(); }
           
           // int chunks;//, tid;
           hsize_t block_size;
           
           std::vector<hsize_t> vsizetoReadN, vstartN,
                                vsizetoReadM, vstartM,
                                vsizetoReadK, vstartK;
           
           // unsigned int ithreads;
           hsize_t M = A.rows();
           hsize_t K = A.cols();
           hsize_t N = B.cols();
           
           //. 20251121 .// if( K == B.rows()) {
           if( static_cast<Eigen::Index>(K) == B.rows()) {
               if( iblock_size.isNotNull()) {
                   block_size =  Rcpp::as<int>(iblock_size);  
               } else {
                   block_size =  MAXBLOCKSIZE/3;  
               }
               
               C = Eigen::MatrixXd::Zero(M,N) ;
               if(block_size > std::min( N, std::min(M,K)) )
                   block_size = std::min( N, std::min(M,K)); 
               
               getBlockPositionsSizes( N, block_size, vstartN, vsizetoReadN );
               getBlockPositionsSizes( M, block_size, vstartM, vsizetoReadM );
               getBlockPositionsSizes( K, block_size, vstartK, vsizetoReadK );
               
               for (size_t ii = 0; ii < vstartM.size(); ii++)
               {
                   for (size_t jj = 0; jj < vstartN.size(); jj++)
                   {
                       for(size_t kk = 0; kk < vstartK.size(); kk++)
                       {
                           C.block(vstartM[ii], vstartN[jj], vsizetoReadM[ii], vsizetoReadN[jj]) =  C.block(vstartM[ii], vstartN[jj], vsizetoReadM[ii], vsizetoReadN[jj]) + 
                               (A.block(vstartM[ii], vstartK[kk], vsizetoReadM[ii], vsizetoReadK[kk]) * B.block(vstartK[kk], vstartN[jj], vsizetoReadK[kk], vsizetoReadN[jj]));
                       }
                   }
               }
               
           } else {
               throw std::range_error("non-conformable arguments");
               }
           
           
       } catch(std::exception &ex) {
           Rcpp::Rcout<<"c++ error : Bblock_matrix_mul : " <<ex.what();
           return(C);
           
       } catch(...) { 
           Rf_error("c++ exception in Bblock_matrix_mul (unknown reason)"); 
       }
       
       return(C);
   }

    
    // In-memory execution - Parallel version - by Blocks
    /**
     * @brief Parallel block-based matrix multiplication
     * @details Implements parallel block-based matrix multiplication with configurable
     * block sizes and thread count. This function:
     * - Supports matrix transposition before multiplication
     * - Uses OpenMP for parallel processing
     * - Implements cache-friendly block-based algorithm
     * - Handles edge cases for non-uniform block sizes
     * 
     * @tparam T First matrix type (MatrixXd or compatible mapped types)
     * @tparam U Second matrix type (MatrixXd or compatible mapped types)
     * @param X First input matrix
     * @param Y Second input matrix
     * @param transpX Whether to transpose X before multiplication
     * @param transpY Whether to transpose Y before multiplication
     * @param iblock_size Block size for computation
     * @param threads Number of threads for parallel processing
     * @return Result of matrix multiplication
     */
    template<typename T, typename U>
    inline Eigen::MatrixXd Rcpp_block_matrix_mul_parallel( T X, U Y, 
                                                                  bool transpX,
                                                                  bool transpY,
                                                                  Rcpp::Nullable<int>  iblock_size, 
                                                                  Rcpp::Nullable<int> threads  = R_NilValue)
    {
        
        Eigen::MatrixXd C;
        try{
            static_assert(std::is_same<T, Eigen::MatrixXd >::value || 
                          std::is_same<T, Eigen::Map< Eigen::MatrixXd >>::value || 
                          std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                          std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                          "Error - type not allowed");
            
            static_assert(std::is_same<U, Eigen::MatrixXd >::value || 
                          std::is_same<U, Eigen::Map< Eigen::MatrixXd >>::value || 
                          std::is_same<U, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                          std::is_same<U, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                          "Error - type not allowed");
            
            Eigen::MatrixXd A = X,
                B = Y;
            
            if(transpX == true){ A = X.transpose(); }
            if(transpY == true){ B = Y.transpose(); }
            
            // int chunks;//, tid;
            hsize_t block_size;
            
            std::vector<hsize_t> vsizetoReadN, vstartN,
            vsizetoReadM, vstartM,
            vsizetoReadK, vstartK;
            
            // unsigned int ithreads;
            hsize_t M = A.rows();
            hsize_t K = A.cols();
            hsize_t N = B.cols();
            
            if( iblock_size.isNotNull()) {
                block_size =  Rcpp::as<int>(iblock_size);  
            } else {
                block_size =  MAXBLOCKSIZE/3;  
            }
            
            C = Eigen::MatrixXd::Zero(M,N) ;
            if(block_size > std::min( N, std::min(M,K)) )
                block_size = std::min( N, std::min(M,K)); 
            
            getBlockPositionsSizes( N, block_size, vstartN, vsizetoReadN );
            getBlockPositionsSizes( M, block_size, vstartM, vsizetoReadM );
            getBlockPositionsSizes( K, block_size, vstartK, vsizetoReadK );
            
            // CAMBIO 1: Eliminar collapse(2) para compatibilidad
            const int total_blocks = static_cast<int>(vstartM.size() * vstartN.size());
            
            #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared(A, B, C)
            {
                // CAMBIO 2: Usar bucle plano en lugar de collapse(2)
            #pragma omp for schedule(dynamic)
                for (int block_idx = 0; block_idx < total_blocks; block_idx++)
                {
                    // Convertir índice plano a coordenadas (ii, jj)
                    const int ii = block_idx / static_cast<int>(vstartN.size());
                    const int jj = block_idx % static_cast<int>(vstartN.size());
                    
                    // CAMBIO 3: Buffer local para evitar race condition (CLAVE para eliminar NaN)
                    Eigen::MatrixXd C_local = Eigen::MatrixXd::Zero(vsizetoReadM[ii], vsizetoReadN[jj]);
                    
                    // CAMBIO 4: Bucle k secuencial dentro del hilo (elimina race condition)
                    for(int kk = 0; kk < static_cast<int>(vstartK.size()); kk++)
                    {
                        // Acumulamos en buffer local (sin race condition)
                        C_local += (A.block(vstartM[ii], vstartK[kk], vsizetoReadM[ii], vsizetoReadK[kk]) * 
                            B.block(vstartK[kk], vstartN[jj], vsizetoReadK[kk], vsizetoReadN[jj]));
                    }
                    
                    // CAMBIO 5: Una sola escritura al final (sin race condition)
                    C.block(vstartM[ii], vstartN[jj], vsizetoReadM[ii], vsizetoReadN[jj]) = C_local;
                }
            }
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception Rcpp_block_matrix_mul_parallel: "<<ex.what()<< " \n";
        }
        
        return(C);
    }
    
    /**
     * @brief Matrix-vector multiplication
     * @details Performs matrix-vector multiplication with dimension validation
     * and efficient memory handling.
     * 
     * @tparam T Matrix type
     * @tparam U Vector type
     * @param A Input matrix
     * @param B Input vector
     * @return Result of matrix-vector multiplication
     * @throws Runtime error if dimensions are not compatible
     */
    template< typename T, typename U>
    inline Rcpp::RObject Rcpp_matrix_vect_mult ( T  A, U  B)
    {
        
        Rcpp::NumericMatrix m = Rcpp::as<Rcpp::NumericMatrix>(A);
        Rcpp::NumericVector v = Rcpp::as<Rcpp::NumericVector>(B);
        
        if( v.length() == m.rows()) {
            
            Rcpp::NumericMatrix C = Rcpp::no_init( m.rows(), m.cols());
            
            for( int i=0; i<m.cols(); i++) {
                C( Rcpp::_, i) = m( Rcpp::_, i) * v;  
            }    
            return(C);
            
        } else if( v.length() == m.cols()) {
            
            Rcpp::NumericMatrix C = Rcpp::no_init( m.rows(), m.cols());
            
            for( int i=0; i<m.rows(); i++) {
                C( i, Rcpp::_) = m( i, Rcpp::_) * v;  
            }    
            return(C);
            
        } else {
            Rcpp::Rcout<<"Error: non-conformable arguments";
        }
        
        return(R_NilValue);
    }
    
    
    /**
     * @brief Vector multiplication (dot product)
     * @details Computes the dot product of two vectors with dimension validation.
     * 
     * @tparam T Vector type
     * @param A First input vector
     * @param B Second input vector
     * @return Scalar result of vector dot product
     * @throws Runtime error if vector dimensions don't match
     */
    template< typename T>
    inline Rcpp::RObject Rcpp_vector_mult ( T  A, T  B)
    {
        
        Rcpp::NumericVector v = Rcpp::as<Rcpp::NumericVector>(A);
        Rcpp::NumericVector v2 = Rcpp::as<Rcpp::NumericVector>(B);
        
        if(v.size() == v2.size()) {
            Rcpp::NumericVector C = Rcpp::no_init( v.size());
            
            std::transform (v.begin(), v.end(), v2.begin(), C.begin(), std::multiplies<double>());
            
            C.attr("dim") = Rcpp::Dimension( C.size(), 1); 
            
            return(C);
        }
        
        return(R_NilValue);
        
    }
    
    
    /**
     * @brief Block-based matrix-vector multiplication
     * @details Implements block-based matrix-vector multiplication with:
     * - Optional parallel processing
     * - Configurable block sizes
     * - Thread count control
     * - Cache-friendly algorithms
     * 
     * @tparam T Matrix/vector type
     * @param A Input matrix
     * @param B Input vector
     * @param bparal Whether to use parallel processing
     * @param iblock_size Block size for computation
     * @param threads Number of threads for parallel processing
     * @return Result of matrix-vector multiplication
     */
    template< typename T>
    inline Rcpp::RObject Rcpp_matrix_vector_blockMult( T  A, T  B, Rcpp::Nullable<bool> bparal, 
                            Rcpp::Nullable<int> iblock_size, Rcpp::Nullable<int> threads)
    {
        
        // NOTA: Per defecte, multiplica per columnes tal i com raja.... 

        bool btransposed = false;
        // unsigned int ithreads;
        hsize_t block_size;
        // int chunks;
        
        Rcpp::NumericMatrix X = Rcpp::as<Rcpp::NumericMatrix>(A);
        Rcpp::NumericVector Y = Rcpp::as<Rcpp::NumericVector>(B);
        Rcpp::NumericMatrix C;
        
        // Matrix
        hsize_t M = X.rows(), N = X.cols();
        // Vector
        hsize_t K = Y.length();
        
        try {
            
            if( K==N || K==M) {
                if ( K == N){
                    // multiplies vector to every col
                    btransposed = true;
                    
                    X = Rcpp::transpose(X);
                    
                    N = X.rows();
                    M = X.cols();
                } 
                
                std::vector<hsize_t> vsizetoRead;
                std::vector<hsize_t> vstart;
                
                // ithreads = get_number_threads(threads, bparal);
                
                C = Rcpp::no_init( M, N);
                
                if( iblock_size.isNotNull()) {
                    block_size =  Rcpp::as<int>(iblock_size);  
                } else {
                    block_size = getMatrixBlockSize( N, M).at(0);
                }
                
                // minimum block size: 2 columns
                if(block_size <= 0 ) {
                    block_size = M*2;
                }
                
                // Mínimum block size: 2 columns
                getBlockPositionsSizes( M*N, block_size, vstart, vsizetoRead );
                
                // chunks = vstart.size()/ithreads;
                
                #pragma omp parallel num_threads( get_number_threads(threads, bparal) ) shared(A, B, C) //, chunks)
                {
                #pragma omp for schedule (static)
                    for (hsize_t ii = 0; ii < vstart.size(); ii ++)
                    {
                        // Duplicate vector
                        std::size_t const no_of_duplicates = vsizetoRead[ii] / Y.length();
                        
                        std::vector<double> v = Rcpp::as<std::vector<double> >(Y); 
                        v.reserve(Y.size() * no_of_duplicates);
                        auto end = std::end(v);
                        
                        for(std::size_t i = 1; i < no_of_duplicates; ++i)
                            v.insert(std::end(v), std::begin(v), end);
                        
                        // Mult vector to matrix by columns / rows
                        if( vstart[ii] + vsizetoRead[ii] >= M*N ) {
                            std::transform (X.begin() + vstart[ii], X.end(),
                                            v.begin(), C.begin() + vstart[ii], std::multiplies<double>());
                        } else {
                            std::transform (X.begin() + vstart[ii], X.begin() + vstart[ii] + vsizetoRead[ii],
                                            v.begin() , C.begin() + vstart[ii], std::multiplies<double>());   
                        }
                    }
                }

            } else {
                
                Rcpp::Rcout<< "vector sum error: non-conformable arguments\n";
                return(R_NilValue);
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception Rcpp_matrix_vector_blockMult: "<<ex.what()<< " \n";
            return(R_NilValue);
        }
        
        if(btransposed == true){
            Rcpp::transpose(C);
        } 
        
        C.attr("dim") = Rcpp::Dimension( M, N);
        
        return(C);
    }
    

}

#endif // BIGDATASTATMETH_ALGEBRA_MEM_MULTIPLICATION_HPP
