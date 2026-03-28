/**
 * @file hdf5RemoveLowData.hpp
 * @brief Quality control utilities for handling low-quality data in HDF5 datasets
 * 
 * This file provides functionality for removing rows or columns from HDF5 datasets
 * that contain a high percentage of missing or low-quality data. It implements
 * efficient block-wise processing for large datasets and provides comprehensive
 * error handling.
 * 
 * Key features:
 * - Row-wise and column-wise filtering
 * - Block-wise processing for memory efficiency
 * - Configurable threshold for data removal
 * - Automatic dataset resizing
 * - Comprehensive error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_UTIL_QC_BASICS_HPP
#define BIGDATASTATMETH_UTIL_QC_BASICS_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Removes rows or columns with high percentage of missing data from HDF5 dataset
     * 
     * This function processes an HDF5 dataset in blocks, removing rows or columns
     * where the percentage of missing data (represented by value 3) exceeds the
     * specified threshold. The filtered data is written to a new dataset.
     * 
     * @param dsIn Pointer to input HDF5 dataset
     * @param dsOut Pointer to output HDF5 dataset where filtered data will be stored
     * @param bycols If true, process by columns; if false, process by rows
     * @param pcent Threshold percentage (0.0-1.0) of missing data to trigger removal
     * 
     * @return int Number of rows/columns removed (negative) or error code
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * @throws H5::DataTypeIException on datatype operation errors
     * 
     * Performance considerations:
     * - Uses block-wise processing with configurable block size (default 1000)
     * - Implements Eigen for efficient matrix operations
     * - Minimizes memory usage through streaming processing
     * 
     * Implementation details:
     * 1. Processes data in blocks to handle large datasets
     * 2. For each block:
     *    - Reads data into memory
     *    - Analyzes rows/columns for missing data percentage
     *    - Removes rows/columns exceeding threshold
     *    - Writes filtered data to output
     * 3. Automatically extends output dataset as needed
     * 
     * @note Missing data is identified by the value 3 in the dataset
     * @warning If all data is removed, a warning is issued suggesting parameter adjustment
     * 
     * Example:
     * @code
     * BigDataStatMeth::hdf5Dataset* input = new hdf5Dataset("data.h5", "/input");
     * BigDataStatMeth::hdf5Dataset* output = new hdf5Dataset("data.h5", "/output");
     * int removed = Rcpp_Remove_Low_Data_hdf5(input, output, true, 0.5);
     * @endcode
     */
    inline int Rcpp_Remove_Low_Data_hdf5( BigDataStatMeth::hdf5Dataset* dsIn, BigDataStatMeth::hdf5Dataset* dsOut, bool bycols, double pcent)
    {
        
        int itotrem = 0;
        
        try{
        
            int ilimit,
                blocksize = 1000;
            
            bool bcreated = false;
            
            std::vector<hsize_t> offset = {0,0},
                                 count = {0,0},
                                 stride = {1,1},
                                 block = {1,1},
                                 newoffset = {0,0};
            
            hsize_t* dims_out = dsIn->dim();
            
            // id bycols == true : read all rows by group of columns ; else : all columns by group of rows
            if (bycols == true) {
                ilimit = dims_out[0];
                count[1] = dims_out[1];
                offset[1] = 0;
            } else {
                ilimit = dims_out[1];
                count[0] = dims_out[0];
                offset[0] = 0;
            };
            
            for( int i=0; i<=(ilimit/blocksize); i++) 
            {
                int iread;
                int iblockrem = 0;
                
                if( (i+1)*blocksize < ilimit) iread = blocksize;
                else iread = ilimit - (i*blocksize);
                
                if(bycols == true) {
                    count[0] = iread; 
                    offset[0] = i*blocksize;
                } else {
                    count[1] = iread; 
                    offset[1] = i*blocksize;
                }
                
                // read block
                std::vector<double> vdCurDataset( count[0] * count[1] ); 
                dsIn->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdCurDataset.data() );
                Eigen::MatrixXd data = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdCurDataset.data(), count[0], count[1] );
                
                
                if(bycols == true) // We have to do it by rows
                {
                    int readedrows = data.rows();
                    
                    for( int row = readedrows-1 ; row>=0; row--)  // COMPLETE EXECUTION
                    {
                        if((data.row(row).array() == 3).count()/(double)count[1]>= pcent )
                        {
                            removeRow(data, row);
                            iblockrem = iblockrem + 1;
                        } 
                    }
                    
                } else {
                    
                    int readedcols = data.cols();
                    
                    for( int col = readedcols-1 ; col>=0; col--)  // COMPLETE EXECUTION
                    { 
                        if((data.col(col).array() == 3).count()/(double)count[0]>=pcent )
                        {
                            removeColumn(data, col);
                            iblockrem = iblockrem + 1;
                        } 
                    }
                }
                
                int extendcols = data.cols();
                int extendrows = data.rows();
                
                if( bcreated == false) {
                    
                    if( extendcols > 0 && extendrows > 0){
                        dsOut->createUnlimitedDataset(extendrows, extendcols, "real");
                        dsOut->openDataset();
                        bcreated = true;
                    } 
                    
                } else {
                    if(bycols == true){
                        dsOut->extendUnlimitedDataset(extendrows, 0);
                    }else{
                        dsOut->extendUnlimitedDataset( 0, extendcols);
                    }
                }

                if( bcreated == true) {
                    std::vector<hsize_t> countblock = {(unsigned long long)extendrows, (unsigned long long)extendcols};
                    dsOut->writeDatasetBlock( Rcpp::wrap(data), newoffset, countblock, stride, block, false);
                    
                    if(bycols == true)
                        newoffset[0] =  newoffset[0] + extendrows;
                    else
                        newoffset[1] =  newoffset[1] + extendcols;
                }
                
                itotrem = itotrem - iblockrem;
                
            }
            
            if( bcreated == false ) {
                Rcpp::warning("All data removed - please adjust pcent parameter or review data");
            }
            
        } catch( H5::FileIException& error) { // catch failure caused by the H5File operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception Rcpp_Remove_Low_Data_hdf5 (File IException)" << std::endl;
            return -1;
        } catch( H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception Rcpp_Remove_Low_Data_hdf5 (DataSet IException)" << std::endl;
            return -1;
        } catch( H5::GroupIException& error) { // catch failure caused by the Group operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception Rcpp_Remove_Low_Data_hdf5 (Group IException)" << std::endl;
            return -1;
        } catch( H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception Rcpp_Remove_Low_Data_hdf5 (DataSpace IException)" << std::endl;
            return -1;
        } catch( H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception Rcpp_Remove_Low_Data_hdf5 (Data TypeIException)" << std::endl;
            return -1;
        } catch(std::exception &ex) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr << "c++ exception Rcpp_Remove_Low_Data_hdf5: " << ex.what();
            return -1;
        } catch (...) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"C++ exception Rcpp_Remove_Low_Data_hdf5 (unknown reason)";
            return -1;
        }
        
        return(itotrem);
    }



}

#endif // BIGDATASTATMETH_UTIL_QC_BASICS_HPP
