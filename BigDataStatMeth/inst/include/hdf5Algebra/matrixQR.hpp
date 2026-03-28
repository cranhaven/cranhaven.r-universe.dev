/**
 * @file matrixQR.hpp
 * @brief QR decomposition for HDF5 matrices
 * @details This header file provides implementations for computing the QR
 * decomposition of matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Full and thin QR decomposition
 * - Memory-efficient algorithms
 * - Parallel processing support
 * - Rank computation
 * - Householder transformations
 * 
 * Supported operations:
 * - Full QR decomposition
 * - Thin QR decomposition
 * - Rank-revealing QR
 * - In-memory computation
 * - HDF5 storage computation
 * 
 * Performance features:
 * - Eigen optimizations
 * - Cache-friendly algorithms
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - Householder QR algorithm
 * - Block-based computation
 * - HDF5 chunked storage
 * - Parallel processing
 * - Vectorized operations
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXQR_HPP
#define BIGDATASTATMETH_HDF5_MATRIXQR_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

// Function declaration
/**
 * @brief Template function for QR decomposition
 * @details Computes the QR decomposition of a matrix using Householder
 * transformations. Supports both full and thin decomposition.
 * 
 * @tparam M Matrix type (Eigen::MatrixXd or mapped matrix)
 * @param X Input matrix to decompose
 * @param bthin Whether to compute thin QR
 * @return Structure containing Q and R matrices
 */
template< typename M> inline strQR RcppQR ( M X, bool bthin );

/**
 * @brief QR decomposition for HDF5 matrices
 * @details Computes the QR decomposition of a matrix stored in HDF5 format.
 * Supports both full and thin decomposition with parallel processing.
 * 
 * @param dsA Input matrix dataset
 * @param dsQ Output Q matrix dataset
 * @param dsR Output R matrix dataset
 * @param bthin Whether to compute thin QR
 * @param block_size Block size for processing (optional)
 * @param threads Number of threads for parallel processing (optional)
 */
inline void RcppQRHdf5( BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsQ, BigDataStatMeth::hdf5Dataset* dsR, 
                               bool bthin, Rcpp::Nullable<int> block_size, Rcpp::Nullable<int> threads );

// Function definition
template< typename M>
inline strQR RcppQR ( M X, bool bthin )
{
    
    static_assert(std::is_same<M, Eigen::MatrixXd >::value || 
                  std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                  std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                  "Error - type not allowed");
    
    Eigen::MatrixXd A = X;
    int m = A.rows(), n = A.cols();
    int irank;
    strQR vQR;
    
    Eigen::MatrixXd R;
    Eigen::MatrixXd Q;
    
    Eigen::FullPivLU<Eigen::MatrixXd>lu_decomp(A);
    Eigen::HouseholderQR<Eigen::MatrixXd> qr(A);
    
    qr.compute(A);
    irank = lu_decomp.rank();
    
    if (irank == m + 1 || irank == n + 1 )
    {
        vQR.R = qr.matrixQR().template triangularView<Eigen::Upper>();
    } else {
        vQR.R = qr.matrixQR().topLeftCorner(irank, irank).template triangularView<Eigen::Upper>(); 
    }
    
    if (bthin == false)
    {
        vQR.Q =  qr.householderQ();       // Full decomposition
    } else {
        
        vQR.Q = Eigen::MatrixXd::Identity(m,n);
        vQR.Q = qr.householderQ() * vQR.Q;    // Thin decomposition
    }
    
    return(vQR);
}




// extern strQR RcppQR( Eigen::MatrixXd & A, bool bthin)
// {
//     
//     int m = A.rows(), n = A.cols();
//     int irank;
//     strQR vQR;
//     
//     Eigen::MatrixXd R;
//     Eigen::MatrixXd Q;
//     
//     Eigen::FullPivLU<Eigen::MatrixXd>lu_decomp(A);
//     Eigen::HouseholderQR<Eigen::MatrixXd> qr(A);
//     
//     qr.compute(A);
//     irank = lu_decomp.rank();
//     
//     if (irank == m + 1 || irank == n + 1 )
//     {
//         vQR.R = qr.matrixQR().template triangularView<Eigen::Upper>();
//     } else {
//         vQR.R = qr.matrixQR().topLeftCorner(irank, irank).template triangularView<Eigen::Upper>(); 
//     }
//     
//     if (bthin == false)
//     {
//         vQR.Q =  qr.householderQ();       // Full decomposition
//     } else {
//         
//         vQR.Q = Eigen::MatrixXd::Identity(m,n);
//         vQR.Q = qr.householderQ() * vQR.Q;    // Thin decomposition
//     }
//     
//     return(vQR);
//     
// }



inline void RcppQRHdf5( BigDataStatMeth::hdf5Dataset* dsA, 
                        BigDataStatMeth::hdf5Dataset* dsQ, 
                        BigDataStatMeth::hdf5Dataset* dsR, 
                        bool bthin,
                        Rcpp::Nullable<int> block_size = R_NilValue,
                        Rcpp::Nullable<int> threads = R_NilValue )
{
    
    
    try {

        int irank; //,
            // iblockfactor = 1;
        std::vector<hsize_t> offset = {0,0},
            count = {dsA->nrows(), dsA->ncols()},
            stride = {1,1},
            block = {1,1};
        Eigen::MatrixXd Q;
        
        
        std::vector<double> vdA( count[0] * count[1] ); 
        dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
        Eigen::MatrixXd A = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdA.data(), count[0], count[1] );
        A.transposeInPlace();
        
        
        Eigen::FullPivLU<Eigen::MatrixXd>lu_decomp(A);
        Eigen::HouseholderQR<Eigen::MatrixXd> qr(A);
        
        qr.compute(A);
        irank = lu_decomp.rank();
        
        
        //..//if (irank == count[0] + 1 || irank == count[1] + 1 )
        if (static_cast<unsigned long long>(irank) == count[0] + 1ULL ||
            static_cast<unsigned long long>(irank) == count[1] + 1ULL)
        {
            Eigen::MatrixXd R = qr.matrixQR().template triangularView<Eigen::Upper>();
            dsR->createDataset( R.rows(), R.cols(), "real" );
            dsR->writeDataset(Rcpp::wrap(R));
        } else {
            Eigen::MatrixXd R = qr.matrixQR().topLeftCorner(irank, irank).template triangularView<Eigen::Upper>();
            dsR->createDataset( R.rows(), R.cols(), "real" );
            dsR->writeDataset(Rcpp::wrap(R));
        }
        
        
        if (bthin == false)
        {
            Eigen::MatrixXd Q = qr.householderQ();
            dsQ->createDataset( Q.rows(), Q.cols(), "real" );
            dsQ->writeDataset( Rcpp::wrap(Q) );
        } else {
            
            //. 2025/01/15 error with thin calculus.// Eigen::MatrixXd Qthin = qr.householderQ() * Eigen::MatrixXd::Identity(count[0], count[1]);
            //. 2025/01/15 .// dsQ->createDataset( Qthin.rows(), Qthin.cols(), "real" );
            //. 2025/01/15 .// dsQ->writeDataset( Rcpp::wrap(Qthin));
            
            Eigen::MatrixXd Qthin = qr.householderQ() * Eigen::MatrixXd::Identity(count[1], count[0]);
            dsQ->createDataset( Qthin.rows(), Qthin.cols(), "real" );
            dsQ->writeDataset( Rcpp::wrap(Qthin));
            
        }
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        Rcpp::Rcout<<"c++ exception RcppQRHdf5 (File IException)";
        return void();
    } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
        Rcpp::Rcout << "c++ exception RcppQRHdf5 (Group IException)";
        return void();
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        Rcpp::Rcout << "c++ exception RcppQRHdf5 (DataSet IException)";
        return void();
    } catch(std::exception& ex) {
        Rcpp::Rcout << "c++ exception RcppQRHdf5" << ex.what();
        return void();
    }
    
    return void();
    
}



}

#endif // BIGDATASTATMETH_HDF5_MATRIXQR_HPP
