/**
 * @file hdf5RemoveMAF.hpp
 * @brief Implementation of Minor Allele Frequency (MAF) filtering for genomic data in HDF5 format
 *
 * This file provides functionality for filtering genomic data based on Minor Allele
 * Frequency (MAF) thresholds. It operates on large-scale genomic datasets stored
 * in HDF5 format, implementing memory-efficient block-wise operations for handling
 * large datasets that don't fit in memory.
 *
 * Key features:
 * - MAF-based filtering of genomic data
 * - Row-wise or column-wise filtering options
 * - Block-wise processing for memory efficiency
 * - Dynamic dataset resizing
 * - Comprehensive error handling
 *
 * @note This implementation is particularly useful in genomic studies where
 * filtering variants based on MAF is a common quality control step.
 *
 * @see BigDataStatMeth::hdf5Dataset
 * @see BigDataStatMeth::calc_freq
 */

#ifndef BIGDATASTATMETH_OMICS_RM_MAF_HPP
#define BIGDATASTATMETH_OMICS_RM_MAF_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

#include "hdf5Omics/hdf5OmicsUtils.hpp"

namespace BigDataStatMeth {

    /**
     * @brief Removes rows or columns from a dataset based on MAF threshold
     *
     * @param dsIn Input HDF5 dataset containing genomic data
     * @param dsOut Output HDF5 dataset for filtered data
     * @param bycols If true, process by columns; if false, process by rows
     * @param pcent MAF threshold percentage (0.0 to 1.0)
     * @param blocksize Number of rows/columns to process in each block
     * @return int Number of removed rows/columns (negative) or error code
     *
     * @details Implementation approach:
     * - Processes data in blocks to manage memory usage
     * - Dynamically creates and extends output dataset
     * - Supports both row-wise and column-wise filtering
     * - Preserves data structure while removing low-MAF variants
     *
     * Algorithm steps:
     * 1. Read data blocks according to specified orientation
     * 2. Calculate MAF for each row/column in block
     * 3. Remove elements below threshold
     * 4. Write filtered block to output dataset
     * 5. Extend output dataset as needed
     *
     * Performance considerations:
     * - Time complexity: O(n*m) where n,m are dataset dimensions
     * - Space complexity: O(blocksize * min(n,m))
     * - I/O operations optimized through block processing
     *
     * Usage example:
     * @code
     * auto* input = hdf5Dataset(...);   // Input genomic data
     * auto* output = hdf5Dataset(...);  // Output filtered data
     * int removed = Rcpp_Remove_MAF_hdf5(input, output, true, 0.05, 1000);
     * @endcode
     *
     * Error codes:
     * - Negative values indicate errors
     * - -1: General error (see error message)
     * - Positive values: Number of removed elements (as negative number)
     *
     * @throws H5::FileIException on HDF5 file operation errors
     * @throws H5::DataSetIException on HDF5 dataset operation errors
     * @throws H5::DataSpaceIException on HDF5 dataspace errors
     * @throws H5::DataTypeIException on HDF5 datatype errors
     * @throws std::exception on general errors
     *
     * @see calc_freq for MAF calculation
     * @see removeRow, removeColumn for filtering operations
     */
    inline int Rcpp_Remove_MAF_hdf5( BigDataStatMeth::hdf5Dataset* dsIn, 
                            BigDataStatMeth::hdf5Dataset* dsOut, 
                            bool bycols, double pcent, int blocksize)
    {
        
        int itotrem = 0;
        
        try{
        
            bool bcreated = false;    
            int ilimit;
            
            std::vector<hsize_t> offset = {0,0},
                                 count = {0,0},
                                 stride = {1,1},
                                 block = {1,1},
                                 newoffset = {0,0};
            
            // Real data set dimension
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
            }
            
            for( int i=0; i <= (ilimit/blocksize); i++) 
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
                    for( int row = readedrows-1 ; row>=0; row--)
                    {
                        if( calc_freq(Rcpp::wrap(data.row(row))) <= pcent ) {
                            removeRow(data, row);
                            iblockrem = iblockrem + 1;
                        } 
                    }
                    
                } else {
                    
                    int readedcols = data.cols();
                    for( int col = readedcols-1 ; col>=0; col--)
                    { 
                        if( calc_freq(Rcpp::wrap(data.col(col))) <= pcent ) {
                            removeColumn(data, col);
                            iblockrem = iblockrem + 1;
                        } 
                    }
                }
                
                int extendcols = data.cols();
                int extendrows = data.rows();
                
                if( extendrows>0 && extendcols>0)
                {
                    if(bcreated == false) {
                        // create_HDF5_unlimited_matrix_dataset_ptr(file, stroutdata, extendrows, extendcols, "numeric");
                        // unlimDataset = new DataSet(file->openDataSet(stroutdata));
                        // bcreated = true;
                        
                        dsOut->createUnlimitedDataset(extendrows, extendcols, "real");
                        dsOut->openDataset();
                        bcreated = true;
                        
                    } else {
                        if(bycols == true){
                            // extend_HDF5_matrix_subset_ptr(file, unlimDataset, extendrows, 0);
                            dsOut->extendUnlimitedDataset(extendrows, 0);
                        }else{
                            // extend_HDF5_matrix_subset_ptr(file, unlimDataset, 0, extendcols);
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
                    
                    // IntegerVector countblock = IntegerVector::create(extendrows, extendcols);
                    // write_HDF5_matrix_subset_v2(file, unlimDataset, newoffset, countblock, stride, block, wrap(data) );
                    
                    // if(bycols == true)
                    //     newoffset[0] =  newoffset[0] + extendrows;
                    // else
                    //     newoffset[1] =  newoffset[1] + extendcols;
                    
                    itotrem = itotrem - iblockrem;
                }
            }
            
            // if (bcreated == true) {
            //     unlimDataset->close();
            // }
            
        } catch( H5::FileIException& error ){
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nc++ c++ exception Rcpp_Remove_MAF_hdf5 (File IException)\n";
            return -1;
        } catch( H5::DataSetIException& error ) { 
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nc++ c++ exception Rcpp_Remove_MAF_hdf5 (DataSet IException)\n";
            return -1;
        } catch( H5::DataSpaceIException& error ) { 
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nc++ c++ exception Rcpp_Remove_MAF_hdf5 (DataSpace IException)\n";
            return -1;
        } catch( H5::DataTypeIException& error ) { 
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nc++ c++ exception Rcpp_Remove_MAF_hdf5 (DataType IException)\n";
            return -1;
        } catch(std::exception &ex) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nc++ c++ exception Rcpp_Remove_MAF_hdf5: "<< ex.what()<<"\n";
            return -1;
        }  catch (...) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nC++ exception Rcpp_Remove_MAF_hdf5 (unknown reason)";
            return -1;
        }
        
        return(itotrem);

    }




} // namespace BigDataStatMeth

#endif // BIGDATASTATMETH_OMICS_RM_MAF_HPP
