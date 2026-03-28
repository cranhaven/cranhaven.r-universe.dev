/**
 * @file vectormatrix.hpp
 * @brief Vector-matrix operations for HDF5 matrices
 * @details This header file provides implementations for vector-matrix operations
 * on matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Vector-matrix multiplication
 * - Matrix-vector multiplication
 * - Block-based computation
 * - Memory-efficient algorithms
 * - Parallel processing support
 * 
 * Supported operations:
 * - Vector-matrix products
 * - Matrix-vector products
 * - Row/column vector operations
 * - Block vector operations
 * - Transposed operations
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Dynamic block sizing
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - BLAS Level 2 operations
 * - Block algorithms
 * - HDF5 chunked storage
 * - Parallel I/O
 * - Vectorized operations
 */

#ifndef BIGDATASTATMETH_ALGEBRA_VECTORMATRIX_HPP
#define BIGDATASTATMETH_ALGEBRA_VECTORMATRIX_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @brief Matrix-vector multiplication by rows
 * @details Multiplies each row of a matrix by a vector element-wise.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of row-wise multiplication
 */
inline Eigen::MatrixXd Rcpp_matrixVectorMultiplication_byRow(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().colwise() * v.array();
    return(X);
}

/**
 * @brief Matrix-vector addition by rows
 * @details Adds a vector to each row of a matrix.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of row-wise addition
 */
inline Eigen::MatrixXd Rcpp_matrixVectorSum_byRow(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().colwise() + v.array();
    return(X);
}

/**
 * @brief Matrix-vector subtraction by rows
 * @details Subtracts a vector from each row of a matrix.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of row-wise subtraction
 */
inline Eigen::MatrixXd Rcpp_matrixVectorSubstract_byRow(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().colwise() - v.array();
    return(X);
}

/**
 * @brief Matrix-vector division by rows
 * @details Divides each row of a matrix by a vector element-wise.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of row-wise division
 */
inline Eigen::MatrixXd Rcpp_matrixVectorDivision_byRow(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().colwise() / v.array();
    return(X);
}


/**
 * @brief Matrix-vector power by rows
 * @details Divides each row of a matrix by a vector element-wise.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of row-wise power
 */
inline Eigen::MatrixXd Rcpp_matrixVectorPow_byRow(Eigen::MatrixXd X, Eigen::VectorXd v) {
    for (int col = 0; col < X.cols(); ++col) {
        X.col(col) = X.col(col).array().unaryExpr(
            [&v, col](double x) { return std::pow(x, v(col)); }
        );
    }
    return(X);
}


/**
 * @brief Matrix-vector multiplication by columns
 * @details Multiplies each column of a matrix by a vector element-wise.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of column-wise multiplication
 */
inline Eigen::MatrixXd Rcpp_matrixVectorMultiplication_byCol(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().rowwise() * v.transpose().array();    
    return(X);
}

/**
 * @brief Matrix-vector addition by columns
 * @details Adds a vector to each column of a matrix.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of column-wise addition
 */
inline Eigen::MatrixXd Rcpp_matrixVectorSum_byCol(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().rowwise() + v.transpose().array();    
    return(X);
}

/**
 * @brief Matrix-vector subtraction by columns
 * @details Subtracts a vector from each column of a matrix.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of column-wise subtraction
 */
inline Eigen::MatrixXd Rcpp_matrixVectorSubstract_byCol(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().rowwise() - v.transpose().array();    
    return(X);
}

/**
 * @brief Matrix-vector division by columns
 * @details Divides each column of a matrix by a vector element-wise.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of column-wise division
 */
inline Eigen::MatrixXd Rcpp_matrixVectorDivision_byCol(Eigen::MatrixXd X, Eigen::VectorXd v) {
    X = X.array().rowwise() / v.transpose().array();    
    return(X);
}


/**
 * @brief Matrix-vector power by columns
 * @details Power each column of a matrix by a vector element-wise.
 * 
 * @param X Input matrix
 * @param v Input vector
 * @return Result of column-wise power
 */
inline Eigen::MatrixXd Rcpp_matrixVectorPower_byCol(Eigen::MatrixXd X, Eigen::VectorXd v) {
    for (int row = 0; row < X.rows(); ++row) {
        X.row(row) = X.row(row).array().unaryExpr(
            [&v, row](double x) { return std::pow(x, v(row)); }
        );
    }
    return(X);
}


/*
    Annotation: 
        * Function:
        *  1: '+'
        *  2: '-'
        *  3: '*'
        *  4: '/'
        *  5: 'pow'
*/
/**
 * @brief Vector-matrix operations for HDF5 matrices
 * @details Performs vector-matrix operations on HDF5 datasets with support for
 * parallel processing and row/column-wise operations.
 * 
 * @param dsA Input matrix dataset
 * @param dsB Input vector dataset
 * @param dsC Output matrix dataset
 * @param function Operation type (multiplication, addition, subtraction, division, power)
 * @param bbyrows Whether to operate by rows or columns
 * @param bparal Whether to use parallel processing
 * @param threads Number of threads for parallel processing
 */
inline BigDataStatMeth::hdf5Dataset* hdf5_matrixVector_calculus(
        BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, 
        BigDataStatMeth::hdf5Dataset* dsC, int function, bool bbyrows, 
        bool bparal, Rcpp::Nullable<int> threads  = R_NilValue)
{
    
    try{
        
        std::vector<hsize_t> stride = {1, 1};
        std::vector<hsize_t> block = {1, 1};
        
        int blocksize = 0;
        
        // Define blocksize atending number of elements in rows and cols
        if( bbyrows == false) {
            if( dsA->ncols() > MAXELEMSINBLOCK ) {
                blocksize = 1;
            } else {
                hsize_t maxsize = std::max<hsize_t>(  dsA->nrows(),  dsA->ncols());
                blocksize = std::ceil( MAXELEMSINBLOCK / maxsize);
            }
            
        } else {
            if( dsA->nrows() > MAXELEMSINBLOCK) {
                blocksize = 1;
            } else {
                hsize_t maxsize = std::max<hsize_t>( dsA->nrows(), dsA->ncols());
                blocksize = std::ceil( MAXELEMSINBLOCK / maxsize);
            }
        }
        
        
        dsC->createDataset( dsA->nrows(), dsA->ncols(), "real");    
        
        std::vector<double> vdB( dsB->nrows() * dsB->ncols());
        dsB->readDatasetBlock( {0, 0}, { dsB->nrows(), dsB->ncols()}, stride, block, vdB.data() );
        
        
        if( bbyrows == false) {
            
            Eigen::RowVectorXd vWeights = Eigen::Map<Eigen::VectorXd, Eigen::Unaligned>(vdB.data(), vdB.size());
            
            for(hsize_t i=0; (i * blocksize) <= dsA->nrows(); i++)
            {
                hsize_t sizetoread = 0;
                if((i+1)*blocksize<dsA->nrows()) {
                    sizetoread = blocksize;
                } else {
                    sizetoread = dsA->nrows()-(i*blocksize);
                }
                
                std::vector<hsize_t> offset = { i*blocksize, 0};
                std::vector<hsize_t> count = {sizetoread, dsA->ncols()};
                
                // Compute and write data
                std::vector<double> vdA( count[0] * count[1]);
                dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdA.data(), count[0], count[1] );    
                                
                if( function == 0 ) {
                    X = Rcpp_matrixVectorSum_byCol(X, vWeights); 
                } else if( function == 1 ) {
                    X = Rcpp_matrixVectorSubstract_byCol(X, vWeights);
                } else if( function == 2 ) {
                    X = Rcpp_matrixVectorMultiplication_byCol(X, vWeights);
                } else if( function == 3 ) {
                    X = Rcpp_matrixVectorDivision_byCol(X, vWeights);
                } else if( function == 4 ) {
                    Rcpp_matrixVectorPower_byCol(X, vWeights);
                }
                
                dsC->writeDatasetBlock(Rcpp::wrap(X.transpose()), offset, count, stride, block, false);
            }
        } else {
            
            Eigen::VectorXd vWeights = Eigen::Map<Eigen::VectorXd, Eigen::Unaligned>(vdB.data(), vdB.size());
            
            for(hsize_t i=0; i*blocksize <= dsA->ncols() ; i++) {
                hsize_t sizetoread = 0;
                if( (i+1)*blocksize < dsA->ncols() ){
                    sizetoread = blocksize;
                } else {
                    sizetoread = dsA->ncols()-(i*blocksize);
                }
                
                std::vector<hsize_t> offset = {0, i*blocksize};
                std::vector<hsize_t> count = { dsA->nrows(), sizetoread};
                
                // Compute and write data
                std::vector<double> vdA( count[0] * count[1]);
                dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdA.data(), count[0], count[1] );
                
                if( function == 0 ) {
                    X = Rcpp_matrixVectorSum_byRow(X, vWeights); 
                } else if( function == 1 ) {
                    X = Rcpp_matrixVectorSubstract_byRow(X, vWeights);
                } else if( function == 2 ) {
                    X = Rcpp_matrixVectorMultiplication_byRow(X, vWeights);
                } else if( function == 3 ) {
                    X = Rcpp_matrixVectorDivision_byRow(X, vWeights);
                } else if( function == 4 ) {
                    X = Rcpp_matrixVectorPow_byRow(X, vWeights);
                }
                
                offset = {i*blocksize, 0};
                
                dsC->writeDatasetBlock(Rcpp::wrap(X.transpose()), offset, count, stride, block, true);
            }
        }
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        // error.printErrorStack();
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<< "c++ exception hdf5_matrixVector_calculus (File IException)" ;
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        // error.printErrorStack();
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception hdf5_matrixVector_calculus (DataSet IException)";
    } catch(std::exception& error) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcout<< "c++ exception vector-matrix functions: "<<error.what()<< " \n";
        // return(dsC);
    }
    
    return(dsC);
    
}



}

#endif // BIGDATASTATMETH_ALGEBRA_VECTORMATRIX_HPP
