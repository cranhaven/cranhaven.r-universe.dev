/**
 * @file matrixDiagonal.hpp
 * @brief Functions for manipulating matrix diagonals in HDF5 datasets
 *
 * This file provides functionality for extracting and setting diagonal elements
 * of matrices stored in HDF5 format. The implementation is optimized for
 * efficient access to diagonal elements without loading the entire matrix
 * into memory.
 *
 * Key features:
 * - Diagonal extraction from HDF5 matrices
 * - Diagonal element setting for HDF5 matrices
 * - Memory-efficient block-wise operations
 * - Exception-safe implementation
 *
 * @note These operations are particularly useful for large matrices where
 * loading the entire matrix into memory is not feasible.
 *
 * @see BigDataStatMeth::hdf5Dataset
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXDIAGONAL_HPP
#define BIGDATASTATMETH_HDF5_MATRIXDIAGONAL_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

namespace BigDataStatMeth {
    
    /**
     * @brief Extracts the diagonal elements from a matrix stored in HDF5 format
     *
     * @param dsMat Input HDF5 dataset containing the matrix
     * @return Rcpp::NumericVector Vector containing the diagonal elements
     *
     * @details Implementation details:
     * - Reads diagonal elements one at a time to minimize memory usage
     * - Uses HDF5 block reading for efficient access
     * - Returns empty vector on error
     *
     * @note This function is optimized for matrices where reading the entire
     * matrix into memory would be impractical.
     *
     * @throws std::exception on HDF5 read errors or memory allocation failures
     */
    inline Rcpp::NumericVector getDiagonalfromMatrix(BigDataStatMeth::hdf5Dataset* dsMat)
    {
        hsize_t matrix_size = dsMat->nrows();
        Rcpp::NumericVector diagonal(matrix_size);
        
        try {
            const hsize_t DIAG_BLOCK_SIZE = 256;
            std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
            
            for (hsize_t block_start = 0; block_start < matrix_size; block_start += DIAG_BLOCK_SIZE) {
                hsize_t current_block_size = std::min(DIAG_BLOCK_SIZE, matrix_size - block_start);
                
                // Read square block starting from diagonal position  
                std::vector<hsize_t> offset = {block_start, block_start};
                std::vector<hsize_t> count = {current_block_size, current_block_size};
                
                std::vector<double> block_data(current_block_size * current_block_size);
                dsMat->readDatasetBlock(offset, count, stride, block, block_data.data());
                
                // Map to Eigen for correct R/HDF5 layout handling
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> 
                    block_matrix(block_data.data(), current_block_size, current_block_size);
                
                // Use Eigen diagonal extraction - NO LOOP NEEDED
                Eigen::Map<Eigen::VectorXd> diagonal_segment(REAL(diagonal) + block_start, current_block_size);
                diagonal_segment = block_matrix.diagonal();
            }
            
        } catch(std::exception& ex) {
            Rcpp::stop ("c++ exception getDiagonalfromMatrix: %s", ex.what());
            // Rcpp::Rcout << "c++ exception getDiagonalfromMatrix: " << ex.what();
        }
        
        return diagonal;
    }
    
    
    
    /**
     * @brief Sets the diagonal elements of a matrix stored in HDF5 format
     *
     * @param dsMat Target HDF5 dataset containing the matrix
     * @param intNewDiagonal Vector of new diagonal values to set
     *
     * @details Implementation approach:
     * - Writes diagonal elements one at a time
     * - Uses HDF5 block writing for efficient access
     * - Preserves existing non-diagonal elements
     *
     * Usage example:
     * @code
     * BigDataStatMeth::hdf5Dataset* matrix = ...;
     * Rcpp::NumericVector newDiag = ...;
     * setDiagonalMatrix(matrix, newDiag);
     * @endcode
     *
     * @throws std::exception on HDF5 write errors or dimension mismatch
     */
    inline void setDiagonalMatrix(BigDataStatMeth::hdf5Dataset* dsMat, Rcpp::NumericVector intNewDiagonal)
    {
        try {
            const hsize_t DIAG_BLOCK_SIZE = 256;
            hsize_t matrix_size = intNewDiagonal.size();
            std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
            
            for (hsize_t block_start = 0; block_start < matrix_size; block_start += DIAG_BLOCK_SIZE) {
                hsize_t current_block_size = std::min(DIAG_BLOCK_SIZE, matrix_size - block_start);
                
                // Read square block starting from diagonal position
                std::vector<hsize_t> offset = {block_start, block_start};
                std::vector<hsize_t> count = {current_block_size, current_block_size};
                
                std::vector<double> block_data(current_block_size * current_block_size);
                dsMat->readDatasetBlock(offset, count, stride, block, block_data.data());
                
                // Map to Eigen for correct R/HDF5 layout handling
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> 
                    block_matrix(block_data.data(), current_block_size, current_block_size);
                
                // Use Eigen diagonal assignment - NO LOOP NEEDED
                Eigen::Map<Eigen::VectorXd> diagonal_segment(REAL(intNewDiagonal) + block_start, current_block_size);
                block_matrix.diagonal() = diagonal_segment;
                
                // Write modified block back
                dsMat->writeDatasetBlock(Rcpp::wrap(block_matrix), offset, count, stride, block, false);
            }
            
        } catch(std::exception& ex) {
            Rcpp::stop("c++ exception setDiagonalMatrix: "+ std::string(ex.what()));
        }
    }

}

#endif // BIGDATASTATMETH_HDF5_MATRIXDIAGONAL_HPP

