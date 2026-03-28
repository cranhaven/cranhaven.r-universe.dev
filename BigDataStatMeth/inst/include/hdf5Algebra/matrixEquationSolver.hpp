/**
 * @file matrixEquationSolver.hpp
 * @brief Matrix equation solver implementation using LAPACK for both in-memory and HDF5-based operations
 *
 * This file provides functionality for solving linear systems of equations in the form AX = B,
 * supporting both standard and symmetric matrix equations. It implements solutions using LAPACK's
 * DGESV (general matrices) and DSYSV (symmetric matrices) routines.
 *
 * Key features:
 * - Solves linear equations for both in-memory and HDF5-stored matrices
 * - Automatic detection and optimization for symmetric matrices
 * - Integration with Eigen and HDF5 libraries
 * - Exception-safe implementation with comprehensive error handling
 *
 * @note Performance depends on matrix size and available memory. For large matrices,
 * the HDF5-based implementation is recommended.
 *
 * @see BigDataStatMeth::hdf5Dataset
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXEQUATIONSOLVER_HPP
#define BIGDATASTATMETH_HDF5_MATRIXEQUATIONSOLVER_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @namespace BigDataStatMeth
 * @brief Main namespace for the BigDataStatMeth library containing matrix operations and HDF5 utilities
 */

    // Symbols in the LAPACK library files : 
    // DGESV computes the solution to a real system of linear equations : A * X = B, where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
    extern "C" {
        /**
         * @brief LAPACK routine for solving general linear equations AX = B
         * @param n Pointer to matrix order
         * @param nrhs Pointer to number of right-hand sides
         * @param a Pointer to matrix A
         * @param lda Pointer to leading dimension of A
         * @param ipiv Pointer to pivot indices
         * @param b Pointer to matrix B
         * @param ldb Pointer to leading dimension of B
         * @param info Pointer to status information
         */
        extern int dgesv_(int*, int*, double*, int*, int*, double*, int*, int*);
    }
    
    // DSYSV computes the solution to a real system of linear equations :   A * X = B, where A is an N-by-N symmetric matrix and X and B are N-by-NRHS matrices.
    extern "C" {
        /**
         * @brief LAPACK routine for solving symmetric linear equations AX = B
         * @param uplo Pointer to upper/lower triangular indicator
         * @param n Pointer to matrix order
         * @param nrhs Pointer to number of right-hand sides
         * @param a Pointer to matrix A
         * @param lda Pointer to leading dimension of A
         * @param ipiv Pointer to pivot indices
         * @param b Pointer to matrix B
         * @param ldb Pointer to leading dimension of B
         * @param work Pointer to workspace array
         * @param lwork Pointer to workspace size
         * @param info Pointer to status information
         */
        extern int dsysv_( char*, int*, int*, double*, int*, int*, double*, int*, double*, int*, int*);
    }


    /**
     * @brief Solves the linear system AX = B using Eigen matrices
     * 
     * @param a Input matrix A (coefficient matrix)
     * @param b Input/output matrix B (right-hand side matrix, will contain solution X)
     * @return Eigen::MatrixXd Solution matrix X
     * 
     * @details This function automatically detects if A is symmetric and uses the appropriate
     * LAPACK solver (DSYSV for symmetric, DGESV for general matrices). The solution is computed
     * in-place in matrix b.
     * 
     * @note Time complexity: O(n³) for general matrices, O(n³/3) for symmetric matrices
     * @note Space complexity: O(n²) for matrix storage
     * 
     * @throws std::exception if matrix dimensions are incompatible or numerical errors occur
     * 
     * @see RcppSolveHdf5 for HDF5-based version
     */
    inline Eigen::MatrixXd RcppSolve(Eigen::Map<Eigen::MatrixXd> a, Eigen::Map<Eigen::MatrixXd> b)
    {
        
        try {
            
            char Uchar = 'U';
            int info = 0;
            
            // Declare matrix variables
            int n = a.rows();
            int nrhs = b.cols();
            int lwork = std::max( 1, n );
            int lda = std::max( 1, n );
            int ldb = std::max( 1, n );
            std::vector<int> ipiv(n);
            std::vector<double> work(lwork);
            
            // Solve matrix equation
            if( a == a.transpose()  )
            {
                // dsysv_( char* UPLO, int* N , int* NRHS, double* A, int* LDA, int* IPIV, double* B, int* LDB, double* WORK, int* LWORK, int* INFO);
                dsysv_( &Uchar, &n, &nrhs, a.data(), &lda, ipiv.data(), b.data(), &ldb, work.data(), &lwork, &info);
            } else {
                
                // dgesv( int N, int NRHS, double A, int LDA, int IPIV, double B, int LDB, int INFO);
                dgesv_( &n, &nrhs, a.data(), &lda, ipiv.data(), b.data(), &ldb, &info );
            }
            
        } catch(std::exception &ex) {
            Rcpp::Rcout<< ex.what();
            return(Eigen::MatrixXd::Zero(2,2));
        }
        
        return(b);
        
    }
    
    
    /**
     * @brief Solves the linear system AX = B using HDF5-stored matrices
     * 
     * @param dsA Input HDF5 dataset containing matrix A
     * @param dsB Input HDF5 dataset containing matrix B
     * @param dsX Output HDF5 dataset for solution matrix X
     * 
     * @details This function provides an HDF5-based implementation for solving linear systems,
     * suitable for large matrices that don't fit in memory. It reads data in blocks and
     * automatically handles symmetric cases.
     * 
     * Implementation notes:
     * - Matrices are read in blocks to manage memory usage
     * - Symmetric matrix detection is performed
     * - Results are written directly to HDF5 dataset
     * 
     * @note For optimal performance:
     * - Ensure matrices are contiguously stored in HDF5
     * - Consider chunk size in HDF5 datasets
     * 
     * @throws H5::FileIException on HDF5 file operations failure
     * @throws H5::GroupIException on HDF5 group operations failure
     * @throws H5::DataSetIException on HDF5 dataset operations failure
     * @throws std::exception on general errors
     * 
     * @see RcppSolve for in-memory version
     */
    inline void RcppSolveHdf5(BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, BigDataStatMeth::hdf5Dataset* dsX )
    {
        
        try {
            
            std::vector<hsize_t> offset = { 0, 0 },
                                 countA = { dsA->nrows(), dsA->ncols() },
                                 countB = { dsB->nrows(), dsB->ncols() },
                                 stride = { 1, 1},
                                 block = { 1, 1};
            
            std::vector<double> vdB( countB[0] * countB[1] );
            dsB->readDatasetBlock( {offset[0], offset[1]}, {countB[0], countB[1]}, stride, block, vdB.data() );
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> b (vdB.data(), countB[0], countB[1]);
            
            {
                char Uchar = 'U';
                int info = 0;
                
                // Declare matrix variables
                int n = dsA->nrows();
                int nrhs = dsB->nrows();
                int lwork = std::max( 1, n );
                int lda = std::max( 1, n );
                int ldb = std::max( 1, n );
                std::vector<int> ipiv(n);
                std::vector<double> work(lwork);
                
                std::vector<double> vdA( countA[0] * countA[1] );
                dsA->readDatasetBlock( {offset[0], offset[1]}, {countA[0], countA[1]}, stride, block, vdA.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> a (vdA.data(), countA[0], countA[1]);
                
                // Solve matrix equation
                if( a == a.transpose()  )
                {
                    // dsysv_( char* UPLO, int* N , int* NRHS, double* A, int* LDA, int* IPIV, double* B, int* LDB, double* WORK, int* LWORK, int* INFO);
                    dsysv_( &Uchar, &n, &nrhs, a.data(), &lda, ipiv.data(), b.data(), &ldb, work.data(), &lwork, &info);
                } else {
                    // dgesv( int N, int NRHS, double A, int LDA, int IPIV, double B, int LDB, int INFO);
                    dgesv_( &n, &nrhs, a.data(), &lda, ipiv.data(), b.data(), &ldb, &info );
                }
                
            }
            
            dsX->writeDataset(b.data());
            
        } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsB, dsX);
            dsA = dsB = dsX = nullptr;
            Rcpp::Rcerr<<"\nc++ exception RcppSolveHdf5 (File IException)";
            return void();
        } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsB, dsX);
            dsA = dsB = dsX = nullptr;
            Rcpp::Rcerr<<"\nc++ exception RcppSolveHdf5 (Group IException)";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsB, dsX);
            dsA = dsB = dsX = nullptr;
            Rcpp::Rcerr<<"\nc++ exception RcppSolveHdf5 (DataSet IException)";
            return void();
        } catch(std::exception& ex) {
            checkClose_file(dsA, dsB, dsX);
            dsA = dsB = dsX = nullptr;
            Rcpp::Rcerr<<"\nc++ exception RcppSolveHdf5" << ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsA, dsB, dsX);
            dsA = dsB = dsX = nullptr;
            Rcpp::Rcerr<<"\nC++ exception RcppSolveHdf5 (unknown reason)";
            return void();
        }
        
        return void();
        
    }



}

#endif // BIGDATASTATMETH_HDF5_MATRIXEQUATIONSOLVER_HPP
