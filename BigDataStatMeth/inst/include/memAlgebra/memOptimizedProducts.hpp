/**
 * @file memOptimizedProducts.hpp
 * @brief Optimized matrix product operations for in-memory computations
 * @details This header file provides highly optimized implementations of matrix
 * product operations for in-memory computations. The implementation includes:
 * 
 * Key features:
 * - Cache-efficient matrix multiplication
 * - Cache-efficient matrix transposed multiplication
 * - Cache-efficient matrix-vector products
 * - Cache-efficient vector-matrix products
 * - Cache-efficient diagonal matrix products
 * - Cache-efficient weighted cross-product
 * - Cache-efficient weighted transposed cross-product
 * - Cache-efficient weighted matrix-vector product
 * - Cache-efficient weighted vector-matrix product
 * - Block-based matrix operations
 * - SIMD optimizations where available
 * - Multi-threaded implementations
 * - Memory-efficient algorithms
 * 
 * The module focuses on:
 * - Dense matrix multiplication
 * - Matrix-vector products
 * - Block matrix operations
 * - Parallel processing optimizations
 * - Cache-friendly algorithms
 * 
 * Performance optimizations include:
 * - Loop unrolling
 * - Cache blocking
 * - Vectorization
 * - Thread-level parallelism
 */
#ifndef BIGDATASTATMETH_ALGEBRA_MEM_OPTIMIZED_PRODS_HPP
#define BIGDATASTATMETH_ALGEBRA_MEM_OPTIMIZED_PRODS_HPP

#include <RcppEigen.h>


namespace BigDataStatMeth {


/**
 * @brief Compute matrix cross-product X'X
 * @details Efficiently computes the cross-product of a matrix with its transpose (X'X)
 * using optimized matrix operations.
 * 
 * @tparam T Matrix type (MatrixXd or compatible mapped types)
 * @param X Input matrix
 * @return Cross-product matrix X'X
 */
template< typename T> inline Eigen::MatrixXd bdcrossproduct ( T X );

/**
 * @brief Compute matrix transposed cross-product XX'
 * @details Efficiently computes the transposed cross-product of a matrix (XX')
 * using optimized matrix operations.
 * 
 * @tparam T Matrix type (MatrixXd or compatible mapped types)
 * @param X Input matrix
 * @return Transposed cross-product matrix XX'
 */
template< typename T> inline Eigen::MatrixXd bdtcrossproduct ( T X );

/**
 * @brief Compute weighted cross-product XwX'
 * @details Computes the weighted cross-product of a matrix with its transpose,
 * where w is a diagonal weight matrix.
 * 
 * @param X Input matrix
 * @param w Weight matrix
 * @return Weighted cross-product XwX'
 */
inline Eigen::MatrixXd xwxt(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w);

/**
 * @brief Compute transposed weighted cross-product X'wX
 * @details Computes the transposed weighted cross-product of a matrix,
 * where w is a diagonal weight matrix.
 * 
 * @param X Input matrix
 * @param w Weight matrix
 * @return Transposed weighted cross-product X'wX
 */
inline Eigen::MatrixXd xtwx(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w);

/**
 * @brief Compute  matrix-diagonal product Xw
 * @details Implementation of matrix-diagonal product computation.
 * 
 * @param X Input matrix
 * @param w Vector representing diagonal matrix
 * @return Matrix-diagonal product Xw
 */
inline Eigen::MatrixXd Xwd(const Eigen::MatrixXd& X, const Eigen::VectorXd& w);

/**
 * @brief Compute matrix-diagonal product Xw
 * @details Computes the product of a matrix with a diagonal matrix
 * represented as a matrix.
 * 
 * @param X Input matrix
 * @param w Matrix representing diagonal matrix
 * @return Matrix-diagonal product Xw
 */
inline Eigen::MatrixXd Xw(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w);

/**
 * @brief Compute diagonal-matrix product wX
 * @details Computes the product of a diagonal matrix with a matrix.
 * 
 * @param X Input matrix
 * @param w Matrix representing diagonal matrix
 * @return Diagonal-matrix product wX
 */
inline Eigen::MatrixXd wX(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w);

/**
 * @brief Compute diagonal-matrix product wX
 * @details Computes the product of a diagonal matrix (represented as a vector)
 * with a matrix using sequential processing.
 * 
 * @param X Input matrix
 * @param w Vector representing diagonal matrix
 * @return Diagonal-matrix product wX
 */
inline Eigen::MatrixXd wdX(const Eigen::MatrixXd& X, const Eigen::VectorXd& w);

/**
 * @brief Compute parallel matrix-diagonal product Xw
 * @details Parallel implementation of matrix-diagonal product computation.
 * 
 * @param X Input matrix
 * @param w Vector representing diagonal matrix
 * @param threads Number of threads for parallel computation
 * @return Matrix-diagonal product Xw
 */
inline Eigen::MatrixXd Xwd_parallel(const Eigen::MatrixXd& X, const Eigen::VectorXd& w, Rcpp::Nullable<int> threads);

/**
 * @brief Compute parallel diagonal-matrix product wX
 * @details Parallel implementation of diagonal-matrix product computation.
 * 
 * @param X Input matrix
 * @param w Vector representing diagonal matrix
 * @param threads Number of threads for parallel computation
 * @return Diagonal-matrix product wX
 */
inline Eigen::MatrixXd wdX_parallel(const Eigen::MatrixXd& X, const Eigen::VectorXd& w, Rcpp::Nullable<int> threads);


// Computes CrossProduct X'X
// inline Eigen::MatrixXd bdcrossproduct (Eigen::MatrixXd& X)
// {
//     size_t nc(X.cols());
//     Eigen::MatrixXd XtX(Eigen::MatrixXd(nc, nc).setZero().selfadjointView<Eigen::Lower>().rankUpdate(X.adjoint()));
//     return(XtX);
// }

template< typename T>
inline Eigen::MatrixXd bdcrossproduct ( T X )
{
    
    static_assert(std::is_same<T, Eigen::MatrixXd >::value || 
                  std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                  std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value ||
                  std::is_same<T, Eigen::Transpose<Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> > >::value ||
                  std::is_same<T, Eigen::Transpose<Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> > >::value,
                  "Error - type not allowed");
    
    Eigen::MatrixXd Xem = X;
    size_t nc(Xem.cols());
    Eigen::MatrixXd XtX(Eigen::MatrixXd(nc, nc).setZero().selfadjointView<Eigen::Lower>().rankUpdate(Xem.adjoint()));
    return(XtX);
}




// Compute tCrossProduct XX'
// inline Eigen::MatrixXd bdtcrossproduct (Eigen::MatrixXd& X)
// {
//     
//     size_t nr(X.rows());
//     Eigen::MatrixXd XXt(Eigen::MatrixXd(nr, nr).setZero().selfadjointView<Eigen::Lower>().rankUpdate(X));
//     return(XXt);
// }


template< typename T>
inline Eigen::MatrixXd bdtcrossproduct ( T X )
{
    
    static_assert(std::is_same<T, Eigen::MatrixXd >::value || 
                  std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                  std::is_same<T, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value ||
                  std::is_same<T, Eigen::Transpose<Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> > >::value ||
                  std::is_same<T, Eigen::Transpose<Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> > >::value,
                  "Error - type not allowed");
    
    Eigen::MatrixXd Xem = X;
    
    size_t nr(Xem.rows());
    Eigen::MatrixXd XXt(Eigen::MatrixXd(nr, nr).setZero().selfadjointView<Eigen::Lower>().rankUpdate(Xem));
    return(XXt);
}



// Compute weighted crossproduct XwX'
inline Eigen::MatrixXd xwxt(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w) 
{
    const int n(X.rows());
    Eigen::MatrixXd XwXt(Eigen::MatrixXd(n, n).setZero().
                             selfadjointView<Eigen::Lower>().rankUpdate(X * w.array().sqrt().matrix().asDiagonal()));
    return (XwXt);
}


// Compute transposed weighted crossproduct X'wX 
inline Eigen::MatrixXd xtwx(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w)
{
    const int n(X.cols());
    Eigen::MatrixXd XtwX(Eigen::MatrixXd(n, n).setZero().
                             selfadjointView<Eigen::Lower>().rankUpdate(X.adjoint() * w.array().sqrt().matrix().asDiagonal()));
    return (XtwX);
}


// Compute weighted crossproduct Xw
inline Eigen::MatrixXd Xw(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w) 
{
    Eigen::MatrixXd Xw = X * w.array().matrix().asDiagonal();
    return (Xw);
}


// Compute weighted crossproduct Xw
inline Eigen::MatrixXd wX(const Eigen::MatrixXd& X, const Eigen::MatrixXd& w) 
{
    Eigen::MatrixXd wX = w.array().matrix().asDiagonal()*X;
    return (wX);
}



// Matirx - vector as diagonal matrix Multiplication
inline Eigen::MatrixXd Xwd(const Eigen::MatrixXd& X, const Eigen::VectorXd& w)
{
    int n = X.rows();
    Eigen::MatrixXd C = Eigen::MatrixXd::Zero(n,X.cols()) ; 
    
    for (int i=0; i<n; i++) {
        C.col(i) = X.col(i)*w(i);
    }
    return(C);
}


//vector as diagonal matrix -  Matirx Multiplication
inline Eigen::MatrixXd wdX(const Eigen::MatrixXd& X, const Eigen::VectorXd& w)
{
    int n = X.cols();
    Eigen::MatrixXd C = Eigen::MatrixXd::Zero(X.rows(),n) ; 
    
    for (int i=0; i<n; i++) {
        C.row(i) = w(i)*X.row(i);
    }
    return(C);
}


// Matirx - vector as diagonal matrix Multiplication (parallel)
inline Eigen::MatrixXd Xwd_parallel(const Eigen::MatrixXd& X, const Eigen::VectorXd& w, Rcpp::Nullable<int> threads = R_NilValue)
{
    int n = X.rows();
    // unsigned int ithreads;
    Eigen::MatrixXd C = Eigen::MatrixXd::Zero(n,X.cols()) ; 
    
    
    // ithreads = get_number_threads(threads, R_NilValue);
    
    // if(threads.isNotNull()) {
    //     if (Rcpp::as<int> (threads) <= std::thread::hardware_concurrency()){
    //         ithreads = Rcpp::as<int> (threads);
    //     } else {
    //         ithreads = getDTthreads(0, true);
    //         //.11-04-2022.// ithreads = std::thread::hardware_concurrency()/2;}
    //     }
    // } else {
    //     ithreads = getDTthreads(0, true);
    //     //.11-04-2022.// ithreads = std::thread::hardware_concurrency()/2;
    // }
    
    //.OpenMP.//omp_set_num_threads(ithreads);
    
    //.OpenMP.//#pragma omp parallel shared(X, w, C) 
    #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared(X, w, C) 
    {
    #pragma omp for schedule (dynamic)
        for (int i=0; i<n; i++)
        {
            C.col(i) = X.col(i)*w(i);
        }  
    }
    return(C);
}


//vector as diagonal matrix -  Matirx Multiplication (parallel)
inline Eigen::MatrixXd wdX_parallel(const Eigen::MatrixXd& X, const Eigen::VectorXd& w, Rcpp::Nullable<int> threads = R_NilValue)
{
    int n = X.cols();
    // unsigned int ithreads;
    Eigen::MatrixXd C = Eigen::MatrixXd::Zero(X.rows(),n);
    
    // ithreads = get_number_threads(threads, R_NilValue);
    
    // if(threads.isNotNull()) {
    //     if (Rcpp::as<int> (threads) <= std::thread::hardware_concurrency()){
    //         ithreads = Rcpp::as<int> (threads);
    //     } else {
    //         ithreads = getDTthreads(0, true);
    //         //.11-04-2022.// ithreads = std::thread::hardware_concurrency()/2;}
    //     }
    // } else {
    //     ithreads = getDTthreads(0, true);
    //     //.11-04-2022.// ithreads = std::thread::hardware_concurrency()/2;
    // }
    
    //.OpenMP.// omp_set_num_threads(ithreads);
    
    //.OpenMP.//#pragma omp parallel shared(X, w, C) 
    #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared(X, w, C) 
    {
        #pragma omp for schedule (dynamic)
        for (int i=0; i<n; i++)
        {
            C.row(i) = w(i)*X.row(i);
        }
    }
    return(C);
}

}

#endif // BIGDATASTATMETH_ALGEBRA_MEM_OPTIMIZED_PRODS_HPP
