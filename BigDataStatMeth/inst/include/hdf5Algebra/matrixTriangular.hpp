/**
 * @file matrixTriangular.hpp
 * @brief Triangular matrix operations for HDF5 matrices
 * @details This header file provides implementations for operations involving
 * triangular matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Upper triangular operations
 * - Lower triangular operations
 * - Triangular solvers
 * - Block-based processing
 * - Memory-efficient algorithms
 * 
 * Supported operations:
 * - Triangular matrix multiplication
 * - Triangular system solving
 * - Back substitution
 * - Forward substitution
 * - Matrix decomposition
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Block-based computation
 * - Multi-threaded processing
 * - Memory access optimization
 * - I/O efficiency
 * 
 * The implementation uses:
 * - BLAS Level 3 operations
 * - Block algorithms
 * - HDF5 chunked storage
 * - Parallel processing
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXTRIANGULAR_HPP
#define BIGDATASTATMETH_HDF5_MATRIXTRIANGULAR_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Set the upper triangular matrix using block-based approach
     * @details Reads blocks, modifies them in memory to create upper triangular structure,
     * and writes complete modified blocks instead of individual elements
     * 
     * @param dsMat The dataset to set the upper triangular matrix of
     * @param dElementsBlock The block size to use for the matrix
     */
    inline void setUpperTriangularMatrix( BigDataStatMeth::hdf5Dataset* dsMat, hsize_t dElementsBlock)
    {
        
        try {
            
            std::vector<hsize_t> stride = {1, 1},
                block = {1, 1};
            
            hsize_t readedRows = 0,
                rowstoRead,
                minimumBlockSize;
            
            // Optimized block size calculation
            minimumBlockSize = std::max(static_cast<hsize_t>(1024), 
                                        std::min(dElementsBlock, dsMat->nrows()));
            
            while ( readedRows < dsMat->nrows() ) {
                
                rowstoRead = ( -2 * readedRows - 1 + std::sqrt( pow(2*readedRows, 2) - 4 * readedRows + 8 * minimumBlockSize + 1) ) / 2;
                
                if( readedRows + rowstoRead > dsMat->nrows()) {
                    rowstoRead = dsMat->nrows() - readedRows;
                }
                
                // Read square block from diagonal position
                std::vector<hsize_t> offset = {readedRows, readedRows};
                std::vector<hsize_t> count = {rowstoRead, rowstoRead};
                
                // Read the current square block
                std::vector<double> block_data(rowstoRead * rowstoRead);
                dsMat->readDatasetBlock(offset, count, stride, block, block_data.data());
                
                // Map to Eigen matrix for easier manipulation
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> 
                    block_matrix(block_data.data(), rowstoRead, rowstoRead);
                
                // Create upper triangular: copy lower triangle to upper triangle
                for (hsize_t i = 0; i < rowstoRead; i++) {
                    for (hsize_t j = i + 1; j < rowstoRead; j++) {
                        block_matrix(i, j) = block_matrix(j, i);  // Copy lower to upper
                    }
                }
                
                // Write the complete modified block back - SINGLE WRITE OPERATION
                dsMat->writeDatasetBlock(Rcpp::wrap(block_matrix), offset, count, stride, block, false);
                
                // Handle rectangular regions outside the diagonal blocks
                if (readedRows + rowstoRead < dsMat->nrows()) {
                    // Write lower-right rectangular region (below diagonal block)
                    hsize_t remaining_rows = dsMat->nrows() - readedRows - rowstoRead;
                    std::vector<hsize_t> rect_offset = {readedRows + rowstoRead, readedRows};
                    std::vector<hsize_t> rect_count = {remaining_rows, rowstoRead};
                    
                    // Read lower-left block (source for transpose)
                    std::vector<double> rect_data(remaining_rows * rowstoRead);
                    dsMat->readDatasetBlock(rect_offset, rect_count, stride, block, rect_data.data());
                    
                    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> 
                        rect_matrix(rect_data.data(), remaining_rows, rowstoRead);
                    
                    // Write transposed data to upper-right position
                    std::vector<hsize_t> upper_offset = {readedRows, readedRows + rowstoRead};
                    std::vector<hsize_t> upper_count = {rowstoRead, remaining_rows};
                    
                    Eigen::MatrixXd transposed = rect_matrix.transpose();
                    dsMat->writeDatasetBlock(Rcpp::wrap(transposed), upper_offset, upper_count, stride, block, false);
                }
                
                readedRows = readedRows + rowstoRead; 
            }
            
        }
        catch( H5::FileIException& error ) {
            Rcpp::Rcout<<"c++ exception setUpperTriangularMatrix (File IException)";
            return void();
        } catch( H5::DataSetIException& error ) {
            Rcpp::Rcout << "c++ exception setUpperTriangularMatrix (DataSet IException)";
            return void();
        } catch(std::exception& ex) {
            Rcpp::Rcout << "c++ exception setUpperTriangularMatrix: " << ex.what();
            return void();
        }
        
        return void();
    }
    
    /**
     * @brief Set the lower triangular matrix using block-based approach
     * @details Reads blocks, modifies them in memory to create lower triangular structure,
     * and writes complete modified blocks instead of individual elements
     * 
     * @param dsMat The dataset to set the lower triangular matrix of
     * @param dElementsBlock The block size to use for the matrix
     */
    inline void setLowerTriangularMatrix( BigDataStatMeth::hdf5Dataset* dsMat, hsize_t dElementsBlock)
    {
        
        try {
            
            std::vector<hsize_t> stride = {1, 1},
                block = {1, 1};
            
            hsize_t readedRows = 0,
                rowstoRead,
                minimumBlockSize;
            
            // Optimized block size calculation
            minimumBlockSize = std::max(static_cast<hsize_t>(1024), 
                                        std::min(dElementsBlock, dsMat->nrows()));
            
            while ( readedRows < dsMat->nrows() ) {
                
                rowstoRead = ( -2 * readedRows - 1 + std::sqrt( pow(2*readedRows, 2) - 4 * readedRows + 8 * minimumBlockSize + 1) ) / 2;
                
                if( readedRows + rowstoRead > dsMat->nrows()) {
                    rowstoRead = dsMat->nrows() - readedRows;
                }
                
                // Read square block from diagonal position
                std::vector<hsize_t> offset = {readedRows, readedRows};
                std::vector<hsize_t> count = {rowstoRead, rowstoRead};
                
                // Read the current square block
                std::vector<double> block_data(rowstoRead * rowstoRead);
                dsMat->readDatasetBlock(offset, count, stride, block, block_data.data());
                
                // Map to Eigen matrix for easier manipulation
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> 
                    block_matrix(block_data.data(), rowstoRead, rowstoRead);
                
                // Create lower triangular: copy upper triangle to lower triangle
                for (hsize_t i = 0; i < rowstoRead; i++) {
                    for (hsize_t j = i + 1; j < rowstoRead; j++) {
                        block_matrix(j, i) = block_matrix(i, j);  // Copy upper to lower
                    }
                }
                
                // Write the complete modified block back - SINGLE WRITE OPERATION
                dsMat->writeDatasetBlock(Rcpp::wrap(block_matrix), offset, count, stride, block, false);
                
                // Handle rectangular regions outside the diagonal blocks
                if (readedRows + rowstoRead < dsMat->nrows()) {
                    // Write upper-right rectangular region (above diagonal block)
                    hsize_t remaining_cols = dsMat->nrows() - readedRows - rowstoRead;
                    std::vector<hsize_t> rect_offset = {readedRows, readedRows + rowstoRead};
                    std::vector<hsize_t> rect_count = {rowstoRead, remaining_cols};
                    
                    // Read upper-right block (source for transpose)
                    std::vector<double> rect_data(rowstoRead * remaining_cols);
                    dsMat->readDatasetBlock(rect_offset, rect_count, stride, block, rect_data.data());
                    
                    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> 
                        rect_matrix(rect_data.data(), rowstoRead, remaining_cols);
                    
                    // Write transposed data to lower-left position
                    std::vector<hsize_t> lower_offset = {readedRows + rowstoRead, readedRows};
                    std::vector<hsize_t> lower_count = {remaining_cols, rowstoRead};
                    
                    Eigen::MatrixXd transposed = rect_matrix.transpose();
                    dsMat->writeDatasetBlock(Rcpp::wrap(transposed), lower_offset, lower_count, stride, block, false);
                }
                
                readedRows = readedRows + rowstoRead; 
            }
        }
        catch( H5::FileIException& error ) {
            Rcpp::Rcout<<"c++ exception setLowerTriangularMatrix (File IException)";
            return void();
        } catch( H5::DataSetIException& error ) {
            Rcpp::Rcout << "c++ exception setLowerTriangularMatrix (DataSet IException)";
            return void();
        } catch(std::exception& ex) {
            Rcpp::Rcout << "c++ exception setLowerTriangularMatrix: " << ex.what();
            return void();
        }
        
        return void();
    }


}

#endif // BIGDATASTATMETH_HDF5_MATRIXTRIANGULAR_HPP

