/**
 * @file matrixPseudoinverse.hpp
 * @brief Pseudoinverse computation for HDF5 matrices
 * @details This header file provides implementations for computing the
 * Moore-Penrose pseudoinverse of matrices stored in HDF5 format. The
 * implementation includes:
 * 
 * Key features:
 * - SVD-based pseudoinverse computation
 * - Memory-efficient algorithms
 * - Parallel processing support
 * - Numerical stability checks
 * - LAPACK integration
 * 
 * Supported operations:
 * - Full pseudoinverse computation
 * - Singular value thresholding
 * - In-memory computation
 * - HDF5 storage computation
 * - Multi-threaded processing
 * 
 * Performance features:
 * - LAPACK optimizations
 * - Cache-friendly algorithms
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - LAPACK DGESVD for SVD
 * - BLAS Level 3 operations
 * - HDF5 chunked storage
 * - Parallel processing
 * - Vectorized operations
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXPSEUDOINVERSE_HPP
#define BIGDATASTATMETH_HDF5_MATRIXPSEUDOINVERSE_HPP


#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @brief LAPACK DGEMM matrix multiplication
 * @details External LAPACK function for matrix-matrix multiplication:
 * C := alpha*op(A)*op(B) + beta*C
 */
extern "C" {
    extern void dgemm_( char*, char*, int*, int*, int*, double*, double*, int*, double*, int*, double*, double*, int* );
}

/**
 * @brief LAPACK DGESVD singular value decomposition
 * @details External LAPACK function for computing the singular value decomposition
 * of a real M-by-N matrix A
 */
extern "C" {
    extern void dgesvd_( char*, char*, int*, int*, double*, int*, double*, double*, int*, double*, int*, double*, int*, int*);
}

/**
 * @brief LAPACK DSCAL vector scaling
 * @details External LAPACK function for scaling a vector by a constant
 */
extern "C" {
    extern void dscal_( int*, double*, double*, int*);
}

/**
 * @brief Compute pseudoinverse of in-memory matrix
 * @details Computes the Moore-Penrose pseudoinverse using SVD decomposition
 * with parallel processing support.
 * 
 * @param A Input matrix to compute pseudoinverse of
 * @param threads Number of threads for parallel processing (optional)
 * @return Pseudoinverse matrix
 */
inline Eigen::MatrixXd RcppPseudoinv(Eigen::MatrixXd* A, 
                                            Rcpp::Nullable<int> threads = R_NilValue)
{
    
    char Schar='S';
    char Cchar='C';
    int ione = 1; 
    double done = 1.0;
    double dzero = 0.0;
    
    int m = A->rows();
    int n = A->cols();
    int lda = m;
    int ldu = std::max(1,m);
    int ldvt =  std::max(1, std::min(m, n));
    int k = std::min(m,n);
    int lwork = std::max( 1, 4*std::min(m,n)* std::min(m,n) + 7*std::min(m, n) );
    int info = 0;
    
    Eigen::VectorXd s = Eigen::VectorXd::Zero(k);
    Eigen::VectorXd work = Eigen::VectorXd::Zero(lwork);
    Eigen::MatrixXd u = Eigen::MatrixXd::Zero(ldu,k);
    Eigen::MatrixXd vt = Eigen::MatrixXd::Zero(ldvt,n);
    
    
    // dgesvd_( char JOBU, char JOBVT, int M, int N, double* A, int LDA, double* S, double* U, int LDU, double* VT, int LDVT, double WORK, int LWORK, int INFO  );
    dgesvd_( &Schar, &Schar, &m, &n, A->data(), &lda, s.data(), u.data(), &ldu, vt.data(), &ldvt, work.data(), &lwork, &info);
    Eigen::MatrixXd pinv = Eigen::MatrixXd::Zero(n,m);
    
    //.OpenMP.// omp_set_dynamic(1);
    
#pragma omp parallel for num_threads(get_number_threads(threads, R_NilValue))
    for (int i = 0; i < k; i++){
        double tempS;
        if(s[i] > 1.0e-9)
            tempS = 1.0/s[i];
        else
            tempS = s[i];
        
        // zscal_ (int* N, double* DA, double* DX, int* INCX )
        dscal_( &m, &tempS, &(u(i*ldu)), &ione );
    }
    // dgemm_( char TRANSA, char TRANSB, int M, int N, int K, double ALPHA, double* A, int LDA, double* B, int LDB, double BETA, double* C, int LDC )	
    dgemm_( &Cchar, &Cchar, &n, &m, &k, &done, vt.data(), &k, u.data(), &m, &dzero, pinv.data(), &n );
    
    
    return(pinv);
    
}


/**
 * @brief Compute pseudoinverse of HDF5 matrix
 * @details Computes the Moore-Penrose pseudoinverse of a matrix stored in
 * HDF5 format using SVD decomposition with parallel processing support.
 * 
 * @param dsA Input matrix dataset
 * @param dsR Output pseudoinverse dataset
 * @param threads Number of threads for parallel processing (optional)
 */
inline void RcppPseudoinvHdf5( BigDataStatMeth::hdf5Dataset* dsA, 
                                      BigDataStatMeth::hdf5Dataset* dsR, 
                                      Rcpp::Nullable<int> threads = R_NilValue )
{
    
    char Schar='S';
    char Cchar='C';
    int ione = 1; 
    double done = 1.0;
    double dzero = 0.0;
    
    int n = dsA->nrows();
    int m = dsA->ncols();
    int lda = m;
    int ldu = std::max(1,m);
    int ldvt =  std::max(1, std::min(m, n));
    int k = std::min(m,n);
    int lwork = std::max( 1, 4*std::min(m,n)* std::min(m,n) + 7*std::min(m, n) );
    int info = 0;
    
    std::vector<hsize_t> offset = {0,0},
        count = {dsA->nrows(), dsA->ncols()},
        stride = {1,1},
        block = {1,1};
    
    Eigen::VectorXd s = Eigen::VectorXd::Zero(k);
    Eigen::MatrixXd u = Eigen::MatrixXd::Zero(ldu,k);
    Eigen::MatrixXd vt = Eigen::MatrixXd::Zero(ldvt,n);
    
    {
        Eigen::VectorXd work = Eigen::VectorXd::Zero(lwork);
        std::vector<double> vdA( count[0] * count[1] );
        
        dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> A (vdA.data(), count[0], count[1]);
        
        dgesvd_( &Schar, &Schar, &m, &n, A.transpose().data() , &lda, s.data(), u.data(), &ldu, vt.data(), &ldvt, work.data(), &lwork, &info);
    }
    
    Eigen::MatrixXd pinv = Eigen::MatrixXd::Zero(n,m);
    dsR->createDataset( n, m, "real" );
    
#pragma omp parallel for num_threads(get_number_threads(threads, R_NilValue))
    for (int i = 0; i < k; i++){
        double tempS;
        if(s[i] > 1.0e-9)
            tempS = 1.0/s[i];
        else
            tempS = s[i];
        
        dscal_( &m, &tempS, &(u(i*ldu)), &ione );
    }
    
    dgemm_( &Cchar, &Cchar, &n, &m, &k, &done, vt.data(), &k, u.data(), &m, &dzero, pinv.data(), &n );
    dsR->writeDataset(Rcpp::wrap(pinv));

    return void();
}


}

#endif // BIGDATASTATMETH_HDF5_MATRIXPSEUDOINVERSE_HPP
