/**
 * @file matrixInvCholesky.hpp
 * @brief Implementation of Cholesky decomposition and matrix inversion using HDF5
 *
 * This file provides functionality for computing matrix inverses using the Cholesky
 * decomposition method, specifically designed for large matrices stored in HDF5 format.
 * The implementation includes parallel processing capabilities and memory-efficient
 * block operations.
 *
 * Key features:
 * - Cholesky decomposition for positive definite matrices
 * - Matrix inversion using Cholesky decomposition
 * - Block-wise processing for memory efficiency
 * - Parallel computation support
 * - HDF5 integration for large matrix handling
 *
 * @note This implementation is particularly efficient for large, symmetric,
 * positive-definite matrices that don't fit in memory.
 *
 * @see BigDataStatMeth::hdf5Dataset
 * @see BigDataStatMeth::hdf5DatasetInternal
 */

#include "Utilities/system-utils.hpp"

#ifndef BIGDATASTATMETH_HDF5_INVCHOLESKY_HPP
#define BIGDATASTATMETH_HDF5_INVCHOLESKY_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @brief Computes matrix inverse using Cholesky decomposition with HDF5 storage
 *
 * @param inDataset Input matrix dataset (must be symmetric positive-definite)
 * @param outDataset Output dataset for the inverse matrix
 * @param bfull If true, computes full matrix inverse; if false, only lower triangular part
 * @param dElementsBlock Block size for processing (minimum 2 * matrix dimension)
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details This function performs matrix inversion in three steps:
 * 1. Cholesky decomposition (A = LL^T)
 * 2. Inverse of the Cholesky factor (L^-1)
 * 3. Computation of full inverse (A^-1 = L^-T L^-1)
 *
 * Performance considerations:
 * - Time complexity: O(n³) for n×n matrix
 * - Space complexity: O(b²) where b is the block size
 * - Parallel efficiency depends on matrix and block sizes
 *
 * @throws H5::FileIException on HDF5 file operation errors
 * @throws H5::GroupIException on HDF5 group operation errors
 * @throws H5::DataSetIException on HDF5 dataset operation errors
 * @throws std::exception on general errors
 */
inline void Rcpp_InvCholesky_hdf5 ( BigDataStatMeth::hdf5Dataset* inDataset,  
                                    BigDataStatMeth::hdf5DatasetInternal* outDataset, 
                                    bool bfull, long dElementsBlock, Rcpp::Nullable<int> threads );


/**
 * @brief Performs Cholesky decomposition with automatic algorithm selection
 *
 * @param inDataset Input matrix dataset (must be symmetric positive-definite)
 * @param outDataset Output dataset for the Cholesky factor L
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size for processing
 * @param threads Number of threads for parallel processing (optional)
 * @return int 0 on success, 1 if not positive definite, 2 on errors
 *
 * @details Automatically selects appropriate algorithm based on matrix size:
 * - Matrices < CHOLESKY_OUTOFCORE_THRESHOLD: uses intermediate algorithm
 * - Matrices ≥ CHOLESKY_OUTOFCORE_THRESHOLD: uses out-of-core tiled algorithm
 * 
 * @note Threshold defined in BigDataStatMeth.hpp
 */
inline int Cholesky_decomposition_hdf5(BigDataStatMeth::hdf5Dataset* inDataset,  
                                       BigDataStatMeth::hdf5Dataset* outDataset, 
                                       int idim0, int idim1, long dElementsBlock, 
                                       Rcpp::Nullable<int> threads);

/**
 * @brief Computes inverse of Cholesky factor with automatic algorithm selection
 *
 * @param InOutDataset Dataset containing Cholesky factor L (will be overwritten with L^-1)
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size for processing
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details Automatically selects appropriate algorithm based on matrix size:
 * - Matrices < CHOLESKY_OUTOFCORE_THRESHOLD: uses intermediate algorithm
 * - Matrices ≥ CHOLESKY_OUTOFCORE_THRESHOLD: uses out-of-core tiled algorithm
 */
inline void Inverse_of_Cholesky_decomposition_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                   int idim0, int idim1, long dElementsBlock, 
                                                   Rcpp::Nullable<int> threads);

/**
 * @brief Computes final matrix inverse with automatic algorithm selection
 *
 * @param InOutDataset Dataset containing L^-1 (will be overwritten with A^-1)
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size for processing
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details Computes A^-1 = L^-T L^-1 from inverted Cholesky factor.
 * Automatically selects algorithm based on matrix size.
 */
inline void Inverse_Matrix_Cholesky_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                             int idim0, int idim1, long dElementsBlock, 
                                             Rcpp::Nullable<int> threads);



/**
 * @brief Performs Cholesky decomposition on a matrix stored in HDF5 format
 *
 * @param inDataset Input matrix dataset
 * @param outDataset Output dataset for Cholesky factor L
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size for processing
 * @param threads Number of threads for parallel processing (optional)
 * @return int 0 on success, 1 if matrix is not positive definite, 2 on HDF5 errors
 *
 * @details Implements block-wise Cholesky decomposition algorithm:
 * - Processes matrix in blocks to manage memory usage
 * - Computes lower triangular factor L where A = LL^T
 * - Uses parallel processing for row computations
 *
 * Implementation notes:
 * - Minimum block size is 2 * matrix dimension
 * - Checks for positive definiteness during computation
 * - Optimized for symmetric matrices
 *
 * @throws H5::FileIException on HDF5 file operation errors
 * @throws H5::GroupIException on HDF5 group operation errors
 * @throws H5::DataSetIException on HDF5 dataset operation errors
 */
inline int Cholesky_decomposition_intermediate_hdf5( BigDataStatMeth::hdf5Dataset* inDataset,
                                                     BigDataStatMeth::hdf5Dataset* outDataset, 
                                                     int idim0, int idim1, long dElementsBlock, 
                                                     Rcpp::Nullable<int> threads );

/**
 * @brief Computes inverse of Cholesky factor in-place
 *
 * @param InOutDataset Dataset containing Cholesky factor L (will be overwritten with L^-1)
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size for processing
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details Implements block-wise inversion of lower triangular matrix:
 * - Processes matrix in blocks for memory efficiency
 * - Overwrites input with its inverse
 * - Uses parallel processing for independent computations
 *
 * @note This function assumes input is a lower triangular matrix from Cholesky decomposition
 *
 * @throws H5::FileIException on HDF5 file operation errors
 * @throws H5::GroupIException on HDF5 group operation errors
 * @throws H5::DataSetIException on HDF5 dataset operation errors
 */
inline void Inverse_of_Cholesky_decomposition_intermediate_hdf5(  BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                                  int idim0, int idim1, long dElementsBlock, 
                                                                  Rcpp::Nullable<int> threads);

/**
 * @brief Computes final matrix inverse using inverted Cholesky factors
 *
 * @param InOutDataset Dataset containing L^-1 (will be overwritten with A^-1)
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size for processing
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details Computes A^-1 = L^-T L^-1 where L^-1 is the inverse of Cholesky factor:
 * - Uses block matrix multiplication for memory efficiency
 * - Implements parallel processing for block operations
 * - Result is symmetric but only lower triangular part is computed
 *
 * @note Upper triangular part can be filled using setUpperTriangularMatrix if needed
 *
 * @throws H5::FileIException on HDF5 file operation errors
 * @throws H5::GroupIException on HDF5 group operation errors
 * @throws H5::DataSetIException on HDF5 dataset operation errors
 */
inline void Inverse_Matrix_Cholesky_intermediate_hdf5( BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                           int idim0, int idim1, long dElementsBlock, 
                                                           Rcpp::Nullable<int> threads );


/**
 * @brief Out-of-core Cholesky decomposition for large matrices using tiled algorithm
 *
 * @param inDataset Input matrix dataset
 * @param outDataset Output dataset for Cholesky factor L
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size (not used, tiles are fixed)
 * @param threads Number of threads for parallel processing (optional)
 * @return int 0 on success, 1 if not positive definite, 2 on errors
 *
 * @details Fixed-tile algorithm for constant memory usage:
 * - Processes matrix in 10k×10k tiles
 * - Memory usage: ~800 MB constant regardless of matrix size
 * - Suitable for matrices >250k dimensions
 * - Three-step process: diagonal factorization, column solve, submatrix update
 */
inline int Cholesky_decomposition_outofcore_hdf5(BigDataStatMeth::hdf5Dataset* inDataset,  
                                                 BigDataStatMeth::hdf5Dataset* outDataset, 
                                                 int idim0, int idim1, long dElementsBlock, 
                                                 Rcpp::Nullable<int> threads);

/**
 * @brief Out-of-core inverse of Cholesky factor using tiled back-substitution
 *
 * @param InOutDataset Dataset containing Cholesky factor L (overwritten with L^-1)
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size (not used, tiles are fixed)
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details Inverts lower triangular matrix using fixed tiles:
 * - Processes backward from last tile to first
 * - Each tile inversion updates dependent tiles
 * - Memory usage: ~800 MB constant
 */
inline void Inverse_of_Cholesky_decomposition_outofcore_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                             int idim0, int idim1, long dElementsBlock, 
                                                             Rcpp::Nullable<int> threads);

/**
 * @brief Out-of-core computation of A^-1 = L^-T L^-1 using tiles
 *
 * @param InOutDataset Dataset containing L^-1 (overwritten with A^-1)
 * @param idim0 Number of rows
 * @param idim1 Number of columns
 * @param dElementsBlock Block size (not used, tiles are fixed)
 * @param threads Number of threads for parallel processing (optional)
 *
 * @details Computes final inverse from inverted Cholesky factor:
 * - Accumulates triple-nested tile products
 * - Exploits symmetry (only lower triangle computed)
 * - Memory usage: ~1.6 GB constant
 */
inline void Inverse_Matrix_Cholesky_outofcore_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                   int idim0, int idim1, long dElementsBlock, 
                                                   Rcpp::Nullable<int> threads);




inline void Rcpp_InvCholesky_hdf5 ( BigDataStatMeth::hdf5Dataset* inDataset, 
                           BigDataStatMeth::hdf5DatasetInternal* outDataset, 
                           bool bfull, long dElementsBlock, 
                           Rcpp::Nullable<int> threads = R_NilValue )
{
    
    try{
        
        int nrows = inDataset->nrows();
        int ncols = inDataset->ncols();
        
        // Rcpp::Rcout<<"\nIniciem cholesky";
        int res = Cholesky_decomposition_hdf5(inDataset, outDataset, nrows, ncols, dElementsBlock, threads);
        
        if(res == 0)
        {
            // Rcpp::Rcout<<"\nDins res==0 -> anem Inverse_of_Cholesky_decomposition_hdf5...";
            Inverse_of_Cholesky_decomposition_hdf5( outDataset, nrows, ncols, dElementsBlock, threads);
            // Rcpp::Rcout<<"\nDins res==0 -> anem Inverse_Matrix_Cholesky_parallel...";
            Inverse_Matrix_Cholesky_hdf5( outDataset, nrows, ncols, dElementsBlock, threads);
            
            if( bfull==true ) {
                setUpperTriangularMatrix( outDataset, dElementsBlock);
            }
        }
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr<<"c++ exception Rcpp_InvCholesky_hdf5 (File IException)";
        return void();
    } catch( H5::GroupIException & error ) { 
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Rcpp_InvCholesky_hdf5 (Group IException)";
        return void();
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Rcpp_InvCholesky_hdf5 (DataSet IException)";
        return void();
    } catch(std::exception& ex) {
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Rcpp_InvCholesky_hdf5" << ex.what();
        return void();
    } catch (...) {
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr<<"\nC++ exception Rcpp_InvCholesky_hdf5 (unknown reason)";
        return void();
    }
    
    return void();
    
}




inline int Cholesky_decomposition_hdf5(BigDataStatMeth::hdf5Dataset* inDataset,  
                                       BigDataStatMeth::hdf5Dataset* outDataset, 
                                       int idim0, int idim1, long dElementsBlock, 
                                       Rcpp::Nullable<int> threads = R_NilValue)
{
    // Detect matrix size and select algorithm
    if (idim0 >= CHOLESKY_OUTOFCORE_THRESHOLD) {
        Rcpp::Rcout << "\nUsing out-of-core Cholesky for large matrix (" << idim0 << "x" << idim1 << ")\n";
        return Cholesky_decomposition_outofcore_hdf5(inDataset, outDataset, idim0, idim1, dElementsBlock, threads);
    } else {
        return Cholesky_decomposition_intermediate_hdf5(inDataset, outDataset, idim0, idim1, dElementsBlock, threads);
    }
}


inline int Cholesky_decomposition_intermediate_hdf5( BigDataStatMeth::hdf5Dataset* inDataset,  
                           BigDataStatMeth::hdf5Dataset* outDataset, 
                           int idim0,  int idim1,  long dElementsBlock, 
                           Rcpp::Nullable<int> threads  = R_NilValue)
{
    
    try {
        
        int dimensionSize = idim0,
            readedRows = 0,
            rowstoRead,
            minimumBlockSize;
            // chunk = 1,
            
        bool bcancel = false;
        double sum = 0;
        
        std::vector<hsize_t> offset = {0,0},
                             count = {1,1},
                             stride = {1,1},
                             block = {1,1};
        
        // Set minimum elements in block (mandatory : minimum = 2 * longest line)
        if( dElementsBlock < dimensionSize * 2 ) {
            minimumBlockSize = dimensionSize * 2;
        } else {
            minimumBlockSize = dElementsBlock;
        }
        
        // Rcpp::Rcout<<"\nDebug 2: minimumBlockSize=" << minimumBlockSize;
        // Poso el codi per llegir els blocks aquí i desprès a dins hauria d'anar-hi la j
        if( idim0 == idim1)
        {
            
            // Rcpp::Rcout<<"\nDins Cholesky_decomposition -> idim0 == idim1";
            while ( readedRows < dimensionSize ) {
                
                rowstoRead = ( -2 * readedRows - 1 + std::sqrt( pow(2*readedRows, 2) - 4 * readedRows + 8 * minimumBlockSize + 1) ) / 2;
                
                if (rowstoRead <= 0) {
                    rowstoRead = minimumBlockSize; // Minimum progress to avoid infinite loop
                }

                if( readedRows + rowstoRead > idim0) { // Max size bigger than data to read ?
                    rowstoRead = idim0 - readedRows;
                }
                
                if (readedRows == 0) {
                    // Primer bloque: count[0] = dimensionSize, count[1] = rowstoRead
                    size_t potential_elements = static_cast<size_t>(dimensionSize) * static_cast<size_t>(rowstoRead);
                    size_t optimalSize = getOptimalBlockElements();
                    if (potential_elements > optimalSize) {
                        rowstoRead = static_cast<int>(MAXCHOLBLOCKSIZE / dimensionSize);
                        if (rowstoRead < 1) rowstoRead = 1;
                    }
                } else {
                    // Bloques siguientes: count[0] = dimensionSize - readedRows + 1, count[1] = readedRows + rowstoRead
                    size_t potential_elements = static_cast<size_t>(dimensionSize - readedRows + 1) * static_cast<size_t>(readedRows + rowstoRead);
                    size_t optimalSize = getOptimalBlockElements();
                    if (potential_elements > optimalSize) {
                        int max_cols = static_cast<int>(MAXCHOLBLOCKSIZE / (dimensionSize - readedRows + 1));
                        rowstoRead = max_cols - readedRows;
                        if (rowstoRead < 1) rowstoRead = 1;
                    }
                }
                
                offset[0] = readedRows;
                
                if( readedRows == 0) {
                    count[0] = dimensionSize - readedRows;
                    count[1] = rowstoRead;
                } else {
                    offset[0] = offset[0] - 1; // We need results from previous line to compute next line results
                    count[1] = readedRows + rowstoRead;
                    count[0] = dimensionSize - readedRows + 1;
                }
                
                readedRows = readedRows + rowstoRead; // Ho preparem perquè desprès necessitarem llegir a partir de la línea anterior
                
                Eigen::MatrixXd A, L;
                
                // Rcpp::Rcout<<"\nDebug 7: A L";
                
                // CAMBIO QUIRÚRGICO: Verificar tamaño de bloque para big-matrix
                size_t block_elements = static_cast<size_t>(count[0]) * static_cast<size_t>(count[1]);
                // size_t max_elements = MAXCHOLBLOCKSIZE; // ~400 MB por vector, 800 MB total
                
                if (block_elements > MAXCHOLBLOCKSIZE) {
                    // Recalcular rowstoRead para bloque más pequeño
                    rowstoRead = static_cast<int>(MAXCHOLBLOCKSIZE / count[0]);
                    if (rowstoRead < 1) rowstoRead = 1;
                    
                    // Recalcular count[1]
                    if( readedRows == 0) {
                        count[1] = rowstoRead;
                    } else {
                        count[1] = readedRows + rowstoRead;
                    }
                    
                    // Actualizar readedRows
                    readedRows = (readedRows > rowstoRead) ? (readedRows - rowstoRead + rowstoRead) : readedRows + rowstoRead;
                }
                
                std::vector<double> vdA( count[0] * count[1] ); 
                inDataset->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
                A = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdA.data(), count[0], count[1] );
                
                std::vector<double> vdL( count[0] * count[1] ); 
                outDataset->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdL.data() );
                L = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdL.data(), count[0], count[1] );
                
                if( offset[0] != 0 )
                    rowstoRead = rowstoRead + 1;
                
                if( rowstoRead > static_cast<int>(count[1]) ) {
                    rowstoRead = count[1];
                }
                
                //. 2025/11/21 .// for (int j = 0; j < rowstoRead; j++)  {
                for (int j = 0; j < rowstoRead; j++)  {
                    if( j + offset[0] == 0) {
                        L(j, j) = std::sqrt(A(j,j));
                    } else {
                        L(j, j + offset[0]) = std::sqrt(A(j,j + offset[0]) - (L.row(j).head(j + offset[0]).array().pow(2).sum() ));    
                    }
                    
                    
                    #pragma omp parallel for num_threads( get_number_threads(threads, R_NilValue) ) private(sum) shared (A,L,j) schedule(dynamic) if (j < readedRows - 1)
                    //. 2025/11/21 .// for ( int i = j + 1; i < dimensionSize - offset[0]  ; i++ )
                    for ( int i = j + 1; i < dimensionSize - static_cast<int>(offset[0])  ; i++ )
                    {
                        if(bcancel == false) {
                            if( j + static_cast<int>(offset[0]) > 0) {
                                sum = (L.block(i, 0, 1, j + static_cast<int>(offset[0])).array() * L.block(j, 0, 1, j + static_cast<int>(offset[0])).array()).array().sum();
                                if( sum != sum ) {
                                    Rcpp::Rcout<<"\n Can't get inverse matrix using Cholesky decomposition matrix is not positive definite\n";
                                    bcancel = true;
                                }
                            } else {
                                sum = 0;
                            }
                            if(!bcancel){
                                L(i,j + static_cast<int>(offset[0])) =  (1/L(j,j + static_cast<int>(offset[0]))*(A(i,j + static_cast<int>(offset[0])) - sum));    
                            }    
                        }
                    }
                }
                if(bcancel == true) {
                    return(1);
                }
                
                if( offset[0] != 0) {
                    offset[0] = offset[0] + 1;
                    count[0] = count[0] - 1;
                    
                    outDataset->writeDatasetBlock( Rcpp::wrap(L.block(1, 0, L.rows()-1, L.cols())), offset, count, stride, block, false);
                    
                } else {
                    outDataset->writeDatasetBlock( Rcpp::wrap(L), offset, count, stride, block, false);
                }
                
                // Rcpp::Rcout<<"\nDebug 13";
                
                offset[0] = offset[0] + count[0] - 1;
                
                // Rcpp::Rcout<<"\nDins Cholesky_decomposition -> Despres escriptura ";
            }
            
        } else {
            throw std::range_error("non-conformable arguments");
        }
        
        // Rcpp::Rcout<<"\nDins Cholesky_decomposition -> Bye Bye...";
        
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr<<"c++ exception Cholesky_decomposition_intermediate_hdf5 (File IException)";
        return(2);
    } catch( H5::GroupIException & error ) { 
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Cholesky_decomposition_intermediate_hdf5 (Group IException)";
        return(2);
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Cholesky_decomposition_intermediate_hdf5 (DataSet IException)";
        return(2);
    } catch(std::exception& ex) {
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Cholesky_decomposition_intermediate_hdf5" << ex.what();
        return(2);
    } catch (...) {
        checkClose_file(inDataset, outDataset);
        inDataset = outDataset = nullptr;
        Rcpp::Rcerr<<"\nC++ exception Cholesky_decomposition_intermediate_hdf5 (unknown reason)";
        return(2);
    }
    return(0);
    
}


/**
 * @brief Out-of-core Cholesky decomposition for large matrices using tiled algorithm
 * @details Fixed-tile algorithm for constant memory usage with complete matrix handling
 */
inline int Cholesky_decomposition_outofcore_hdf5(BigDataStatMeth::hdf5Dataset* inDataset,  
                                                 BigDataStatMeth::hdf5Dataset* outDataset, 
                                                 int idim0, int idim1, long dElementsBlock, 
                                                 Rcpp::Nullable<int> threads = R_NilValue)
{
    
    // BigDataStatMeth::hdf5DatasetInternal* tmpA = nullptr;
    
    try {
        int dimensionSize = idim0;
        int tileSize = 10000; // Fixed tile size for predictable memory usage
        int numTiles = (dimensionSize + tileSize - 1) / tileSize;
        
        std::vector<hsize_t> stride = {1,1}, block = {1,1};
        
        // Create temporary working copy of input matrix
        // Block-wise algorithm requires modification of A during computation
        std::string tempAPath = inDataset->getGroup() + "/.tmp_chol_A_" + inDataset->getDatasetName();
        BigDataStatMeth::hdf5DatasetInternal tempA(inDataset->getFileName(), tempAPath, true);
        
        tempA.createDataset(idim0, idim1, "real");

        // Copy complete symmetric matrix to temporary working dataset
        // Need full matrix for proper block-wise updates
        for (int i = 0; i < numTiles; i++) {
            
            hsize_t iStart = i * tileSize;
            hsize_t iSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - iStart));
            
            for (int j = 0; j < numTiles; j++) {  // Complete matrix, not just lower triangle
                hsize_t jStart = j * tileSize;
                hsize_t jSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - jStart));
                
                Eigen::MatrixXd Aij(iSize, jSize);
                std::vector<double> vAij(iSize * jSize);
                
                if (j <= i) {
                    // Read from lower triangle of input matrix
                    inDataset->readDatasetBlock({iStart, jStart}, {iSize, jSize}, stride, block, vAij.data());
                    Aij = Eigen::Map<Eigen::MatrixXd>(vAij.data(), iSize, jSize);
                } else {
                    // Use symmetry: A(i,j) = A(j,i)^T for upper triangle tiles
                    std::vector<double> vAji(jSize * iSize);
                    inDataset->readDatasetBlock({jStart, iStart}, {jSize, iSize}, stride, block, vAji.data());
                    Eigen::MatrixXd Aji = Eigen::Map<Eigen::MatrixXd>(vAji.data(), jSize, iSize);
                    Aij = Aji.transpose();
                }
                
                tempA.writeDatasetBlock(Rcpp::wrap(Aij), {iStart, jStart}, {iSize, jSize}, stride, block, false);
            }
        }
        
        // Block-wise Cholesky algorithm: A = L * L^T where L is lower triangular
        // Process tiles in order: diagonal factorization -> column solve -> submatrix update
        for (int k = 0; k < numTiles; k++) {
            
            hsize_t kStart = k * tileSize;
            hsize_t kSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - kStart));
            
            // Step 1: Diagonal factorization - L(k,k) = cholesky(A(k,k))
            Eigen::MatrixXd Akk(kSize, kSize);
            std::vector<double> vAkk(kSize * kSize);
            tempA.readDatasetBlock({kStart, kStart}, {kSize, kSize}, stride, block, vAkk.data());
            Akk = Eigen::Map<Eigen::MatrixXd>(vAkk.data(), kSize, kSize);
            
            // In-place Cholesky factorization of diagonal tile
            Eigen::LLT<Eigen::MatrixXd> llt(Akk);
            if (llt.info() != Eigen::Success) {
                tempA.remove();
                Rcpp::Rcout << "\nMatrix not positive definite at tile " << k << "\n";
                return 1;
            }
            Eigen::MatrixXd Lkk = llt.matrixL();
            
            // Write diagonal Cholesky factor to output
            outDataset->writeDatasetBlock(Rcpp::wrap(Lkk), {kStart, kStart}, {kSize, kSize}, stride, block, false);
            
            // Step 2: Column solve - L(i,k) = A(i,k) * inv(L(k,k)^T)
            // Columns are independent, can be parallelized
            #pragma omp parallel for num_threads(get_number_threads(threads, R_NilValue)) schedule(dynamic)
            for (int i = k + 1; i < numTiles; i++) {
                
                hsize_t iStart = i * tileSize;
                hsize_t iSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - iStart));
                
                Eigen::MatrixXd Aik(iSize, kSize);
                std::vector<double> vAik(iSize * kSize);
                tempA.readDatasetBlock({iStart, kStart}, {iSize, kSize}, stride, block, vAik.data());
                Aik = Eigen::Map<Eigen::MatrixXd>(vAik.data(), iSize, kSize);
                
                // Solve triangular system: Lkk^T * Lik^T = Aik^T
                Eigen::MatrixXd Lik = Lkk.triangularView<Eigen::Lower>().solve(Aik.transpose()).transpose();
                
                // Write column factor to output
                outDataset->writeDatasetBlock(Rcpp::wrap(Lik), {iStart, kStart}, {iSize, kSize}, stride, block, false);
            }
            
            // Step 3: Submatrix update - A(i,j) -= L(i,k) * L(j,k)^T
            // Outer loop over tiles can be parallelized (tiles are independent)
            #pragma omp parallel for num_threads(get_number_threads(threads, R_NilValue)) schedule(dynamic)
            for (int i = k + 1; i < numTiles; i++) {
                
                hsize_t iStart = i * tileSize;
                hsize_t iSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - iStart));
                
                // Load computed L(i,k) factor
                Eigen::MatrixXd Lik(iSize, kSize);
                std::vector<double> vLik(iSize * kSize);
                outDataset->readDatasetBlock({iStart, kStart}, {iSize, kSize}, stride, block, vLik.data());
                Lik = Eigen::Map<Eigen::MatrixXd>(vLik.data(), iSize, kSize);
                
                // Update tiles in current row (inner loop cannot be parallelized due to dependencies)
                for (int j = k + 1; j <= i; j++) {
                    
                    hsize_t jStart = j * tileSize;
                    hsize_t jSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - jStart));
                    
                    // Load computed L(j,k) factor
                    Eigen::MatrixXd Ljk(jSize, kSize);
                    std::vector<double> vLjk(jSize * kSize);
                    outDataset->readDatasetBlock({jStart, kStart}, {jSize, kSize}, stride, block, vLjk.data());
                    Ljk = Eigen::Map<Eigen::MatrixXd>(vLjk.data(), jSize, kSize);
                    
                    // Load current A(i,j) tile for update
                    Eigen::MatrixXd Aij(iSize, jSize);
                    std::vector<double> vAij(iSize * jSize);
                    tempA.readDatasetBlock({iStart, jStart}, {iSize, jSize}, stride, block, vAij.data());
                    Aij = Eigen::Map<Eigen::MatrixXd>(vAij.data(), iSize, jSize);
                    
                    // Apply rank-k update: A(i,j) -= L(i,k) * L(j,k)^T
                    Aij -= Lik * Ljk.transpose();
                    
                    // Write updated tile back to working matrix
                    tempA.writeDatasetBlock(Rcpp::wrap(Aij), {iStart, jStart}, {iSize, jSize}, stride, block, false);
                }
            }
        }
        
        // Clean up temporary working dataset
        tempA.remove();
        
    } catch(std::exception& ex) {
        Rcpp::Rcerr << "c++ exception Cholesky_decomposition_outofcore_hdf5: " << ex.what();
        return 2;
    }
    
    return 0;
}



inline void Inverse_of_Cholesky_decomposition_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                   int idim0, int idim1, long dElementsBlock, 
                                                   Rcpp::Nullable<int> threads = R_NilValue)
{
    // Detect matrix size and select algorithm
    if (idim0 >= CHOLESKY_OUTOFCORE_THRESHOLD) {
        Rcpp::Rcout << "\nUsing out-of-core inverse Cholesky for large matrix (" << idim0 << "x" << idim1 << ")\n";
        Inverse_of_Cholesky_decomposition_outofcore_hdf5(InOutDataset, idim0, idim1, dElementsBlock, threads);
    } else {
        Inverse_of_Cholesky_decomposition_intermediate_hdf5(InOutDataset, idim0, idim1, dElementsBlock, threads);
    }
}



inline void Inverse_of_Cholesky_decomposition_intermediate_hdf5( BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                    int idim0, int idim1, long dElementsBlock, 
                                    Rcpp::Nullable<int> threads = R_NilValue)
{
    
    try{
        
        int dimensionSize = idim0, 
            readedCols = 0,
            colstoRead,
            minimumBlockSize;
        
        std::vector<hsize_t> offset = {0,0},
                             count = {1,1},
                             stride = {1,1},
                             block = {1,1};

        // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky_decomposition_intermediate_hdf5 -> Inici";
        // Set minimum elements in block (mandatory : minimum = 2 * longest line)
        if( dElementsBlock < dimensionSize * 2 ) {
            // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> if(dElementsBlock < dimensionSize * 2)";
            minimumBlockSize = dimensionSize * 2;
        } else {
            // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> else";
            minimumBlockSize = dElementsBlock;
        }
        
        // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> getDiagonalfromMatrix(InOutDataset)";
        Eigen::VectorXd Diagonal = Rcpp::as<Eigen::VectorXd>(getDiagonalfromMatrix(InOutDataset));
        // Set the new diagonal in the result matrix
        setDiagonalMatrix( InOutDataset, Rcpp::wrap(Diagonal.cwiseInverse()) );
        
        // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> Despres setDiagonal";
        
        if( idim0 == idim1) {
            
            // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> idim0 == idim1";
            
            while ( readedCols < dimensionSize ) {
                
                // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> readedCols < dimensionSize";
                
                colstoRead = ( -2 * readedCols - 1 + std::sqrt( pow(2*readedCols, 2) - 4 * readedCols + 8 * minimumBlockSize + 1) ) / 2;
                
                if (colstoRead <= 0) {
                    colstoRead = minimumBlockSize; // Minimum progress to avoid infinite loop
                }
                
                if( readedCols + colstoRead > idim0) { // Max size bigger than data to read ?
                    colstoRead = idim0 - readedCols;
                }
                
                size_t potential_elements = static_cast<size_t>(dimensionSize - readedCols) * static_cast<size_t>(readedCols + colstoRead);
                size_t optimalSize = getOptimalBlockElements();
                if (potential_elements > optimalSize) {
                    int max_cols = static_cast<int>(MAXCHOLBLOCKSIZE / (dimensionSize - readedCols));
                    colstoRead = max_cols - readedCols;
                    if (colstoRead < 1) colstoRead = 1;
                }
                
                offset[0] = readedCols;
                count[0] =  dimensionSize - offset[0];
                count[1] = readedCols + colstoRead;
                
                Eigen::MatrixXd verticalData;
                
                std::vector<double> vverticalData( count[0] * count[1] ); 
                InOutDataset->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vverticalData.data() );
                verticalData = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vverticalData.data(), count[0], count[1] );
                
                //. 20251121 .// for (int j = 1; j < dimensionSize - offset[0]; j++)
                //!!!!!!!!!!!! for (int j = 1; j < dimensionSize - offset[0] && j < static_cast<int>(count[0]); j++)
                for (int j = 1; j < dimensionSize - static_cast<int>(offset[0]); j++)
                {
                    // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> j < dimensionSize - static_cast<int>(offset[0])";
                    
                    Eigen::VectorXd vR = Eigen::VectorXd::Zero(j+static_cast<int>(offset[0]));
                    Eigen::ArrayXd ar_j;
                    
                    int size_j;
                    if( j < colstoRead) {
                        size_j = j;
                    } else {
                        size_j = colstoRead;
                    }
                    
                    ar_j = verticalData.block( j, static_cast<int>(offset[0]), 1,  size_j).transpose().array();
                    
                    vR = Eigen::VectorXd::Zero(static_cast<int>(offset[0]) + ar_j.size());
                    
#pragma omp parallel for num_threads( get_number_threads(threads, R_NilValue) ) shared (ar_j, j, verticalData, offset, colstoRead, vR) schedule(dynamic) 
                    for (int i = 0; i < static_cast<int>(offset[0]) + ar_j.size() ; i++) {
                        
                        Eigen::ArrayXd ar_i = verticalData.block( 0, i, size_j, 1).array();
                        
                        if( static_cast<int>(offset[0]) > 0 ) {
                            if( j  <= colstoRead ) {
                                if( i < static_cast<int>(offset[0]) ){
                                    vR(i) =  (( verticalData.coeff(j, i) + ((ar_j.transpose() * ar_i.transpose()).sum())) * (-1)) / Diagonal[j+static_cast<int>(offset[0])];
                                } else {
                                    vR(i) =   ((ar_j.transpose() * ar_i.transpose()) * (-1)).sum() / Diagonal[j+static_cast<int>(offset[0])];
                                    
                                }
                            } else {
                                if( i < static_cast<int>(offset[0]) ){
                                    vR(i) =  ( verticalData.coeff(j, i) + ((ar_j.transpose() * ar_i.transpose()).sum()));
                                } else {
                                    vR(i) =   (ar_j.transpose() * ar_i.transpose()).sum();
                                }
                            }
                        } else {
                            if( j <= colstoRead ) {
                                vR(i) =   ((ar_j.transpose() * ar_i.transpose()) * (-1)).sum() / Diagonal[j+static_cast<int>(offset[0])];
                            } else {
                                vR(i) =   (ar_j.transpose() * ar_i.transpose()).sum();
                            }
                        }
                    }
                    
                    verticalData.block(j, 0, 1, vR.size()) = vR.transpose();
                    
                }
                
                InOutDataset->writeDatasetBlock( Rcpp::wrap(verticalData), offset, count, stride, block, false);
                readedCols = readedCols + colstoRead; // Ho preparem perquè desprès necessitarem llegir a partir de la línea anterior
                
            }
        } else {
            throw std::range_error("non-conformable arguments");
        }
        
        // Rcpp::Rcout<<"\nDins Inverse_of_Cholesky -> Bye Bye...";
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr<<"c++ exception Inverse_of_Cholesky_decomposition_intermediate_hdf5 (File IException)";
        return void();
    } catch( H5::GroupIException & error ) { 
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Inverse_of_Cholesky_decomposition_intermediate_hdf5 (Group IException)";
        return void();
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Inverse_of_Cholesky_decomposition_intermediate_hdf5 (DataSet IException)";
        return void();
    } catch(std::exception& ex) {
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Inverse_of_Cholesky_decomposition_intermediate_hdf5" << ex.what();
        return void();
    } catch (...) {
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr<<"\nC++ exception Inverse_of_Cholesky_decomposition_intermediate_hdf5 (unknown reason)";
        return void();
    }
    
    return void();
    
}



/**
 * @brief Out-of-core inverse of Cholesky factor using tiled back-substitution
 * @details Inverts lower triangular matrix L in-place using tiles
 */
inline void Inverse_of_Cholesky_decomposition_outofcore_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                             int idim0, int idim1, long dElementsBlock, 
                                                             Rcpp::Nullable<int> threads = R_NilValue)
{
    
    try {
        int dimensionSize = idim0;
        int tileSize = 10000;
        int numTiles = (dimensionSize + tileSize - 1) / tileSize;
        
        std::vector<hsize_t> stride = {1,1}, block = {1,1};
        
        // Process tiles backward for inverse: last diagonal to first
        for (int k = numTiles - 1; k >= 0; k--) {
            
            hsize_t kStart = k * tileSize;
            hsize_t kSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - kStart));
            
            // 1. Invert diagonal tile L(k,k) in-place
            Eigen::MatrixXd Lkk(kSize, kSize);
            std::vector<double> vLkk(kSize * kSize);
            InOutDataset->readDatasetBlock({kStart, kStart}, {kSize, kSize}, stride, block, vLkk.data());
            Lkk = Eigen::Map<Eigen::MatrixXd>(vLkk.data(), kSize, kSize);
            
            // Invert lower triangular tile
            Lkk = Lkk.triangularView<Eigen::Lower>().solve(Eigen::MatrixXd::Identity(kSize, kSize));
            
            // Write inverted diagonal tile
            InOutDataset->writeDatasetBlock(Rcpp::wrap(Lkk), {kStart, kStart}, {kSize, kSize}, stride, block, false);
            
            // 2. Update column tiles below diagonal: L(i,k) = -L(i,i) * L_old(i,k) * L(k,k)
            #pragma omp parallel for num_threads(get_number_threads(threads, R_NilValue)) schedule(dynamic)
            for (int i = k + 1; i < numTiles; i++) {
                
                hsize_t iStart = i * tileSize;
                hsize_t iSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - iStart));
                
                // Load L(i,k) - old value
                Eigen::MatrixXd Lik_old(iSize, kSize);
                std::vector<double> vLik(iSize * kSize);
                InOutDataset->readDatasetBlock({iStart, kStart}, {iSize, kSize}, stride, block, vLik.data());
                Lik_old = Eigen::Map<Eigen::MatrixXd>(vLik.data(), iSize, kSize);
                
                // Load L(i,i) - already inverted
                Eigen::MatrixXd Lii(iSize, iSize);
                std::vector<double> vLii(iSize * iSize);
                InOutDataset->readDatasetBlock({iStart, iStart}, {iSize, iSize}, stride, block, vLii.data());
                Lii = Eigen::Map<Eigen::MatrixXd>(vLii.data(), iSize, iSize);
                
                // Update: L(i,k) = -L(i,i) * L_old(i,k) * L(k,k)
                Eigen::MatrixXd Lik_new = -Lii * Lik_old * Lkk;
                
                // Write updated column tile
                InOutDataset->writeDatasetBlock(Rcpp::wrap(Lik_new), {iStart, kStart}, {iSize, kSize}, stride, block, false);
            }
        }
        
    } catch(std::exception& ex) {
        Rcpp::Rcerr << "c++ exception Inverse_of_Cholesky_decomposition_outofcore_hdf5: " << ex.what();
        return void();
    }
    
    return void();
}



inline void Inverse_Matrix_Cholesky_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                             int idim0, int idim1, long dElementsBlock, 
                                             Rcpp::Nullable<int> threads = R_NilValue)
{
    // Detect matrix size and select algorithm
    if (idim0 >= CHOLESKY_OUTOFCORE_THRESHOLD) {
        Rcpp::Rcout << "\nUsing out-of-core matrix inverse for large matrix (" << idim0 << "x" << idim1 << ")\n";
        Inverse_Matrix_Cholesky_outofcore_hdf5(InOutDataset, idim0, idim1, dElementsBlock, threads);
    } else {
        Inverse_Matrix_Cholesky_intermediate_hdf5(InOutDataset, idim0, idim1, dElementsBlock, threads);
    }
}


inline void Inverse_Matrix_Cholesky_intermediate_hdf5( BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                     int idim0, int idim1, long dElementsBlock, 
                                     Rcpp::Nullable<int> threads = R_NilValue)
{
    
    try {
        
        int dimensionSize = idim0,
            readedCols = 0,
            colstoRead;
            //.. 20251121 ..// minimumBlockSize;
        
        Eigen::VectorXd newDiag(idim0);
        
        std::vector<hsize_t> offset = {0,0},
                             count = {1,1},
                             stride = {1,1},
                             block = {1,1};
        
        // Rcpp::Rcout<<"\nDins Inverse_Matrix_Cholesky_intermediate_hdf5 -> Hi...";
        // Set minimum elements in block (mandatory : minimum = 2 * longest line)
        //.. 20251121 ..// if( dElementsBlock < dimensionSize * 2 ) {
        //.. 20251121 ..//     minimumBlockSize = dimensionSize * 2;
        //.. 20251121 ..// } else {
        //.. 20251121 ..//     minimumBlockSize = dElementsBlock;
        //.. 20251121 ..// }
        
        if( idim0 == idim1 )
        {
            // Rcpp::Rcout<<"\nDins Inverse_Matrix_Cholesky_parallel -> idim0 == idim1";
            while ( readedCols < dimensionSize ) {
                
                // Rcpp::Rcout<<"\nDins Inverse_Matrix_Cholesky_parallel ->  readedCols < dimensionSize";
                // Set minimum elements in block - prevent overflow and optimize for any matrix size
                //.. 20251121 ..// minimumBlockSize = std::min(static_cast<long>(dElementsBlock), static_cast<long>(std::max(2000L, dimensionSize * 2L)));
                
                // Adaptive block size calculation for any matrix dimensions
                // Target: ~50MB blocks for optimal cache usage and memory efficiency
                // const int targetElements = 6250000; // ~50MB / 8 bytes per double
                const int targetElements = (dimensionSize > 10000) ? 25000000 : 6250000;
                const int minCols = std::max(1, std::min(16, dimensionSize / 100)); // 1-16 or 1% of matrix
                const int maxCols = std::min(dimensionSize, static_cast<int>(std::sqrt(targetElements / dimensionSize))); // Cache-friendly limit
                
                colstoRead = std::max(minCols, std::min(maxCols, (dimensionSize - readedCols + 2) / 3));
                
                // Ensure we don't exceed remaining columns
                colstoRead = std::min(colstoRead, dimensionSize - readedCols);
                
                // Final safety check
                if (colstoRead <= 0) colstoRead = 1;
                
                
                
                // colstoRead = ( -2 * readedCols - 1 + std::sqrt( pow(2*readedCols, 2) - 4 * readedCols + 8 * minimumBlockSize + 1) ) / 2;
                if(colstoRead ==1) {
                    colstoRead = 2;
                }
                
                if( readedCols + colstoRead > idim0) { // Max size bigger than data to read ?
                    colstoRead = idim0 - readedCols;
                }
                
                size_t potential_elements = static_cast<size_t>(dimensionSize - readedCols) * static_cast<size_t>(readedCols + colstoRead);
                size_t optimalSize = getOptimalBlockElements();
                if (potential_elements > optimalSize) {
                    int max_cols = static_cast<int>(MAXCHOLBLOCKSIZE / (dimensionSize - readedCols));
                    colstoRead = max_cols - readedCols;
                    if (colstoRead < 2) colstoRead = 2; // Esta función necesita mínimo 2
                }
                
                offset[0] = readedCols;
                count[0] =  dimensionSize - readedCols;
                count[1] = readedCols + colstoRead;
                
                Eigen::MatrixXd verticalData;
                
                std::vector<double> vverticalData( count[0] * count[1] ); 
                InOutDataset->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vverticalData.data() );
                verticalData = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vverticalData.data(), count[0], count[1] );
                
                // Rcpp::Rcout<<"\nDins Inverse_Matrix_Cholesky_parallel ->  Anem a per el for parallel";
                
                //!!!!! for ( int i = 0; i < colstoRead + offset[0]; i++)   // Columns
// #pragma omp parallel for num_threads(ithreads) shared (verticalData, colstoRead, offset) schedule(dynamic)
                // int max_i = std::min(static_cast<int>(colstoRead + offset[0]), static_cast<int>(count[1]));
                
                if (offset[0] > static_cast<hsize_t>(std::numeric_limits<Eigen::Index>::max())) {
                    Rcpp::stop("offset[0] is too large to convert to Eigen::Index");
                }
                
                #pragma omp parallel for num_threads( get_number_threads(threads, R_NilValue) ) shared (verticalData, colstoRead, offset) schedule(static) ordered
                for ( int i = 0; i < colstoRead + static_cast<int>(offset[0]); i++)   // Columns
                // for ( int i = 0; i < max_i; i++)   // Columns
                {
                    int init;
                    
                    if(static_cast<int>(offset[0]) == 0) {
                        init = i + 1;
                        newDiag(i) = verticalData.block(i, i, idim0-i, 1 ).array().pow(2).sum();
                    } else {
                        if(  i < static_cast<int>(offset[0])) {
                            init = 0;
                        } else {
                            newDiag(i) = verticalData.block( i-static_cast<int>(offset[0]), i, idim0-i, 1 ).array().pow(2).sum();
                            if( i < static_cast<int>(offset[0]) + colstoRead - 1) {
                                init = i - static_cast<int>(offset[0]) + 1;
                            } else {
                                init = colstoRead; // force end
                            }
                        }
                    }
                    
                    #pragma omp ordered
                    {
                        for ( int j = init; j < colstoRead ; j++) { // Rows
                            
                            if( static_cast<int>(offset[0]) + j < verticalData.cols()) {
                                verticalData(j,i) = (verticalData.block( j, i , verticalData.rows() - j, 1).array() * verticalData.block( j, j + static_cast<int>(offset[0]),  verticalData.rows() - j, 1).array()).sum();
                            }
                        }
                    }
                   
                }
                
                // Rcpp::Rcout<<"\nDins Inverse_Matrix_Cholesky_parallel ->  Fi volta, anem a escriure";
                InOutDataset->writeDatasetBlock( Rcpp::wrap(verticalData), offset, count, stride, block, false);
                
                readedCols = readedCols + colstoRead; 
            }
            
            setDiagonalMatrix( InOutDataset, Rcpp::wrap(newDiag) );
            
        } else {
            throw std::range_error("non-conformable arguments");
        }
        
        // Rcpp::Rcout<<"\nDins Inverse_Matrix_Cholesky_parallel -> Bye Bye...";
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr<<"c++ exception Inverse_Matrix_Cholesky_intermediate_hdf5 (File IException)";
        return void();
    } catch( H5::GroupIException & error ) { 
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Inverse_Matrix_Cholesky_intermediate_hdf5 (Group IException)";
        return void();
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Inverse_Matrix_Cholesky_intermediate_hdf5 (DataSet IException)";
        return void();
    } catch(std::exception& ex) {
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr << "c++ exception Inverse_Matrix_Cholesky_intermediate_hdf5: " << ex.what();
        return void();
    } catch (...) {
        checkClose_file(InOutDataset);
        InOutDataset = nullptr;
        Rcpp::Rcerr<<"\nC++ exception Inverse_Matrix_Cholesky_intermediate_hdf5 (unknown reason)";
        return void();
    }
    
    return void();
}


/**
 * @brief Out-of-core computation of A^-1 = L^-T L^-1 using tiles
 * @details Computes final inverse from inverted Cholesky factor
 * Formula: A^-1(i,j) = sum_k L^-1(k,i) * L^-1(k,j)
 * Reads column tiles vertically to compute dot products
 */
inline void Inverse_Matrix_Cholesky_outofcore_hdf5(BigDataStatMeth::hdf5Dataset* InOutDataset, 
                                                   int idim0, int idim1, long dElementsBlock, 
                                                   Rcpp::Nullable<int> threads = R_NilValue)
{
    
    //  BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    
    try {
        int dimensionSize = idim0;
        int tileSize = 10000; // Fixed tile size for memory control
        int numTiles = (dimensionSize + tileSize - 1) / tileSize;
        
        std::vector<hsize_t> stride = {1,1}, block = {1,1};
        
        // Create temporary dataset to avoid overwriting L^-1 during computation
        std::string tempDatasetPath = InOutDataset->getGroup() + "/.tmp_inverse_" + InOutDataset->getDatasetName();
        BigDataStatMeth::hdf5DatasetInternal tempDataset(InOutDataset->getFileName(), tempDatasetPath, true);
        tempDataset.createDataset(idim0, idim1, "real");
        
        // Parallel computation over result tiles (i,j)
        // Result is symmetric, only compute lower triangle
#pragma omp parallel for num_threads(get_number_threads(threads, R_NilValue)) schedule(dynamic)
        for (int i = 0; i < numTiles; i++) {
            
            hsize_t iStart = i * tileSize;
            hsize_t iSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - iStart));
            
            // Only compute j <= i (lower triangle)
            for (int j = 0; j <= i; j++) {
                
                hsize_t jStart = j * tileSize;
                hsize_t jSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - jStart));
                
                // Initialize result tile
                Eigen::MatrixXd Rij = Eigen::MatrixXd::Zero(iSize, jSize);
                
                // Sum over k: A^-1(i,j) = sum_k L^-1(k,i) * L^-1(k,j)
                // L^-1 is lower triangular, so L^-1(k,i) = 0 for k < i
                // Start from max(i,j) to skip zero products
                int k_start = (i > j) ? i : j;
                
                for (int k = k_start; k < numTiles; k++) {
                    
                    hsize_t kStart = k * tileSize;
                    hsize_t kSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - kStart));
                    
                    // Read column tile i: rows k, columns i
                    // This gives L^-1(k,i) which we need for the product
                    Eigen::MatrixXd Lki(kSize, iSize);
                    std::vector<double> vLki(kSize * iSize);
                    InOutDataset->readDatasetBlock({kStart, iStart}, {kSize, iSize}, stride, block, vLki.data());
                    Lki = Eigen::Map<Eigen::MatrixXd>(vLki.data(), kSize, iSize);
                    
                    // Read column tile j: rows k, columns j  
                    // This gives L^-1(k,j)
                    Eigen::MatrixXd Lkj(kSize, jSize);
                    std::vector<double> vLkj(kSize * jSize);
                    InOutDataset->readDatasetBlock({kStart, jStart}, {kSize, jSize}, stride, block, vLkj.data());
                    Lkj = Eigen::Map<Eigen::MatrixXd>(vLkj.data(), kSize, jSize);
                    
                    // Accumulate: R(i,j) += L^-1(k,i)^T * L^-1(k,j)
                    // Lki has rows k, cols i → transpose gives rows i, cols k
                    // Result: (i × k) × (k × j) = (i × j)
                    Rij += Lki.transpose() * Lkj;
                }
                
                // Write result tile to temporary dataset
                tempDataset.writeDatasetBlock(Rcpp::wrap(Rij), {iStart, jStart}, {iSize, jSize}, stride, block, false);
            }
        }
        
        // Copy result from temporary to original dataset
        for (int i = 0; i < numTiles; i++) {
            hsize_t iStart = i * tileSize;
            hsize_t iSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - iStart));
            
            for (int j = 0; j <= i; j++) {
                hsize_t jStart = j * tileSize;
                hsize_t jSize = std::min(static_cast<hsize_t>(tileSize), static_cast<hsize_t>(dimensionSize - jStart));
                
                // Read tile from temporary dataset
                Eigen::MatrixXd Rij(iSize, jSize);
                std::vector<double> vRij(iSize * jSize);
                tempDataset.readDatasetBlock({iStart, jStart}, {iSize, jSize}, stride, block, vRij.data());
                Rij = Eigen::Map<Eigen::MatrixXd>(vRij.data(), iSize, jSize);
                
                // Write to original dataset
                InOutDataset->writeDatasetBlock(Rcpp::wrap(Rij), {iStart, jStart}, {iSize, jSize}, stride, block, false);
            }
        }
        
        // Clean up temporary dataset
        tempDataset.remove();
        
    } catch(std::exception& ex) {
        Rcpp::Rcerr << "c++ exception Inverse_Matrix_Cholesky_outofcore_hdf5: " << ex.what();
        return void();
    }
    
    return void();
}

}

#endif // BIGDATASTATMETH_HDF5_INVCHOLESKY_HPP

