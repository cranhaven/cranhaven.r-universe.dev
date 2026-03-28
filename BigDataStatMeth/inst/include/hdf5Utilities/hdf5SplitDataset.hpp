/**
 * @file hdf5SplitDataset.hpp
 * @brief Implementation of dataset splitting operations for HDF5 files
 *
 * This file provides functionality for splitting large HDF5 datasets into
 * smaller, more manageable chunks. It supports both row-wise and column-wise
 * splitting with flexible block size specification and efficient memory
 * management.
 *
 * Key features:
 * - Row-wise and column-wise splitting
 * - Flexible block size specification
 * - Memory-efficient processing
 * - Support for both R and internal C++ interfaces
 * - Automatic block size optimization
 *
 * @note This implementation is particularly useful for processing large
 * datasets that need to be handled in smaller chunks.
 */

#ifndef BIGDATASTATMETH_UTIL_SPLIT_DATASETS_HPP
#define BIGDATASTATMETH_UTIL_SPLIT_DATASETS_HPP


#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @brief Splits an HDF5 dataset into multiple smaller datasets (R interface)
 *
 * @param dstosplit Input dataset to split
 * @param bycols Whether to split by columns (true) or rows (false)
 * @param stroutgroup Output group path
 * @param stroutdataset Base name for output datasets
 * @param blocksize Size of each block
 * @param irows Number of rows in input dataset
 * @param icols Number of columns in input dataset
 *
 * @details Implementation approach:
 * 1. Calculates number of blocks based on input dimensions
 * 2. Processes dataset in blocks:
 *    - Reads block from input dataset
 *    - Creates new dataset for block
 *    - Writes block to new dataset
 * 3. Handles edge cases for final blocks
 *
 * Memory management:
 * - Processes one block at a time
 * - Efficient memory allocation
 * - Proper resource cleanup
 *
 * Output naming:
 * - Datasets named as: stroutgroup/stroutdataset.N
 * - N is sequential block number (0-based)
 *
 * @throws H5::FileIException on file operation errors
 * @throws H5::DataSetIException on dataset operation errors
 * @throws H5::DataSpaceIException on dataspace operation errors
 *
 * @note This version is designed for R interface use and handles
 * transposition appropriately.
 */
inline void RcppSplit_matrix_hdf5 ( BigDataStatMeth::hdf5Dataset* dstosplit, bool bycols, 
                                std::string stroutgroup, std::string stroutdataset, 
                                int blocksize, int irows, int icols )
{
        
    BigDataStatMeth::hdf5Dataset* dsOut = nullptr;
        
    try {
        
        int blocks;
        hsize_t inrows = irows, 
            incols = icols,
            ii = 0,
            kk = 0;
        
        std::vector<hsize_t> stride = {1, 1},
            block = {1, 1};
        
        std::string newDatasetName = "";
        
        if( bycols == true ) {
            blocks = (icols + blocksize - 1) / blocksize;
            incols = blocksize;
        } else {
            blocks = (irows + blocksize - 1) / blocksize;
            inrows = blocksize;
        }
        
        for ( int i=0; i<blocks; i++)
        {
            newDatasetName = stroutgroup + "/" + stroutdataset + "." + std::to_string(i);
            
            if( bycols == true) { 
                kk = i * blocksize;
                if( kk + static_cast<hsize_t>(blocksize) > static_cast<hsize_t>(icols)) 
                    { incols = static_cast<hsize_t>(icols) - kk; }
            } else  {
                ii = i * blocksize;
                if( ii + static_cast<hsize_t>(blocksize) > static_cast<hsize_t>(irows)) 
                    { inrows = static_cast<hsize_t>(irows) - ii; }
            }
            
            std::vector<double> vdts( inrows * incols );
            dstosplit->readDatasetBlock( {kk, ii}, {incols, inrows}, stride, block, vdts.data() );
                
            dsOut = new BigDataStatMeth::hdf5Dataset(dstosplit->getFileName(), newDatasetName, true);
            dsOut->createDataset( inrows, incols, "real"); 
            
            if( dsOut->getDatasetptr() != nullptr ){
                dsOut->writeDataset(vdts.data());
            }
            
            delete dsOut; dsOut = nullptr;
            
        }
        
    } catch( H5::FileIException& error ) {
        checkClose_file(dstosplit, dsOut);
        Rf_error( "c++ exception RcppSplit_matrix_hdf5(File IException )");
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dstosplit, dsOut);
        Rf_error( "c++ exception RcppSplit_matrix_hdf5 (DataSet IException )");
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dstosplit, dsOut);
        Rf_error( "c++ exception RcppSplit_matrix_hdf5 (DataSpace IException )");
    } catch(std::exception &ex) {
        checkClose_file(dstosplit, dsOut);
        Rf_error( "\nC++ exception RcppSplit_matrix_hdf5 : %s", ex.what());
    } catch (...) {
        checkClose_file(dstosplit, dsOut);
        Rf_error( "\nC++ exception RcppSplit_matrix_hdf5 (unknown reason)");
    }
    
    return void();
    
}
    
    
/**
 * @brief Splits an HDF5 dataset into multiple smaller datasets (internal C++ interface)
 *
 * @param dstosplit Input dataset to split
 * @param stroutgroup Output group path
 * @param stroutdataset Base name for output datasets
 * @param bycols Whether to split by columns (true) or rows (false)
 * @param nblocks Number of blocks to create (if > 0)
 * @param iblocksize Size of each block (if > 0)
 * @param irows Number of rows in input dataset
 * @param icols Number of columns in input dataset
 *
 * @details Implementation features:
 * - Flexible block specification:
 *   - By number of blocks (nblocks > 0)
 *   - By block size (iblocksize > 0)
 * - Automatic block size calculation
 * - Efficient memory usage
 * - Error checking for parameters
 *
 * Block size determination:
 * - If nblocks specified: size = dimension/nblocks (rounded up)
 * - If iblocksize specified: uses provided size
 * - Cannot specify both nblocks and iblocksize
 *
 * Memory considerations:
 * - Block-wise processing for memory efficiency
 * - Dynamic memory allocation for each block
 * - Proper cleanup of resources
 *
 * Error handling:
 * - Parameter validation
 * - HDF5 operation errors
 * - Memory allocation errors
 * - Invalid configuration detection
 *
 * Usage example:
 * @code
 * // Split by 5 blocks
 * RcppSplit_matrix_hdf5_internal(dataset, "/output", "split",
 *                               true, 5, 0, 1000, 1000);
 * // Split by block size of 200
 * RcppSplit_matrix_hdf5_internal(dataset, "/output", "split",
 *                               true, 0, 200, 1000, 1000);
 * @endcode
 *
 * @throws H5::FileIException on file operation errors
 * @throws H5::DataSetIException on dataset operation errors
 * @throws H5::DataSpaceIException on dataspace operation errors
 *
 * @note This version is designed for internal C++ use and maintains
 * native data layout without transposition.
 */
inline void RcppSplit_matrix_hdf5_internal ( BigDataStatMeth::hdf5Dataset* dstosplit,
                                   std::string stroutgroup, std::string stroutdataset,
                                   bool bycols, int nblocks, int iblocksize, 
                                   int irows, int icols )
{
        
    BigDataStatMeth::hdf5Dataset* dsOut = nullptr;
        
    try {
        
        std::string newDatasetName = "";
        hsize_t inrows = irows, 
                incols = icols,
                ii = 0,
                kk = 0;
        int blocks = nblocks, 
            blocksize = iblocksize;;
        std::vector<hsize_t> stride = {1, 1},
                             block = {1, 1};
        
        if( nblocks <= 0 && iblocksize <= 0 ){
            checkClose_file(dstosplit);
            Rf_error( "\n Block size or number of blocks are needed to proceed with matrix split. Please, review parameters");
            return void();
            
        } else if (nblocks > 0 && iblocksize > 0 ) {
            checkClose_file(dstosplit);
            Rf_error( "\nBlock size and number of blocks are defined, please define only one option, split by number of blocks or by block size");
            return void();
            
        } else if( nblocks > 0 ) {
            double module;
            
            if(bycols == true) {
                blocksize = icols / blocks;
                module = icols % blocksize;
            } else {
                blocksize = irows / blocks;
                module = irows % blocksize;
            }
            if (module > 0) { blocksize = blocksize + 1; }
        } 
        
        if(blocksize > 0) {
            
            if( bycols == true ) {
                blocks = (icols + blocksize - 1) / blocksize;
                incols = blocksize;
            } else {
                blocks = (irows + blocksize - 1) / blocksize;
                inrows = blocksize;
            }    
        }
        
        for ( int i=0; i<blocks; i++)
        {
            newDatasetName = stroutgroup + "/" + stroutdataset + "." + std::to_string(i);
            
            if( bycols == true) {
                kk = i * blocksize;
                if( kk + static_cast<hsize_t>(blocksize) > static_cast<hsize_t>(icols)) { 
                    incols = static_cast<hsize_t>(icols) - kk; 
                }
            } else  {
                ii = i * blocksize;
                if( ii + static_cast<hsize_t>(blocksize) > static_cast<hsize_t>(irows)) { 
                    inrows = static_cast<hsize_t>(irows) - ii; 
                }
            }
    
            std::vector<double> vdts( inrows * incols );
            dstosplit->readDatasetBlock( {ii, kk}, {inrows, incols}, stride, block, vdts.data() );
            
            dsOut = new BigDataStatMeth::hdf5Dataset(dstosplit->getFileName(), newDatasetName, true);
            dsOut->createDataset(  incols, inrows, "real"); 
            
            if( dsOut->getDatasetptr() != nullptr ){
                dsOut->writeDataset(vdts.data());
            }
            
            delete dsOut; dsOut = nullptr;
            
        }
        
    } catch( H5::FileIException& error ) {
        checkClose_file(dstosplit, dsOut);
        Rf_error( "c++ exception RcppSplit_matrix_hdf5_internal(File IException )");
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dstosplit, dsOut);
        Rf_error( "c++ exception RcppSplit_matrix_hdf5_internal (DataSet IException )");
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dstosplit, dsOut);
        Rf_error( "c++ exception RcppSplit_matrix_hdf5_internal (DataSpace IException )");
    } catch(std::exception &ex) {
        checkClose_file(dstosplit, dsOut);
        Rf_error( "\nC++ exception RcppSplit_matrix_hdf5_internal : %s",ex.what());
    } catch (...) {
        checkClose_file(dstosplit, dsOut);
        Rf_error( "\nC++ exception RcppSplit_matrix_hdf5_internal (unknown reason)");
    }
    
    return void();
    
}

}

#endif // BIGDATASTATMETH_UTIL_SPLIT_DATASETS_HPP
