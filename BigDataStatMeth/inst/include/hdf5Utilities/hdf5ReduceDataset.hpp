/**
 * @file hdf5ReduceDataset.hpp
 * @brief Implementation of dataset reduction operations for HDF5 files
 *
 * This file provides functionality for reducing multiple HDF5 datasets into
 * a single dataset using specified reduction operations. It supports
 * element-wise operations across datasets while handling dimension
 * mismatches and memory efficiency.
 *
 * Key features:
 * - Dataset reduction using various operations (+, -)
 * - Automatic dimension adjustment
 * - Memory-efficient processing
 * - Support for both internal and R interface usage
 * - Optional dataset cleanup after reduction
 *
 * @note This implementation is particularly useful for combining or
 * reducing large datasets that don't fit in memory simultaneously.
 */

#ifndef BIGDATASTATMETH_UTIL_REDUCE_DATASETS_HPP
#define BIGDATASTATMETH_UTIL_REDUCE_DATASETS_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Reduces multiple HDF5 datasets into a single dataset using specified operation
     *
     * @param filename HDF5 file path
     * @param stringroup Input group containing datasets to reduce
     * @param stroutgroup Output group for reduced dataset
     * @param stroutdataset Name of output dataset
     * @param strreducefunction Reduction operation ("+" or "-")
     * @param boverwrite Whether to overwrite existing output dataset
     * @param bremove Whether to remove input datasets after reduction
     * @param binternal Whether this is an internal call (affects data layout)
     *
     * @details Implementation approach:
     * 1. Opens input HDF5 file and gets dataset list
     * 2. Processes datasets sequentially:
     *    - Reads each dataset into memory
     *    - Adjusts dimensions if necessary
     *    - Applies reduction operation
     * 3. Creates output dataset with reduced result
     *
     * Dimension handling:
     * - Automatically resizes matrices to match dimensions
     * - Preserves data during dimension adjustment
     * - Handles both row and column mismatches
     *
     * Memory management:
     * - Processes one dataset at a time
     * - Uses Eigen for efficient matrix operations
     * - Cleans up resources properly
     *
     * Layout considerations:
     * - Internal calls: Uses RowMajor layout
     * - R interface calls: Uses ColMajor layout and transposes result
     *
     * Supported operations:
     * - Addition ("+")
     * - Subtraction ("-")
     *
     * Error handling:
     * - HDF5 file operations
     * - Dataset operations
     * - Dimension mismatches
     * - Memory allocation
     *
     * Usage example:
     * @code
     * RcppReduce_dataset_hdf5("data.h5", "/input", "/output", "reduced",
     *                         "+", true, false, true);
     * @endcode
     *
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     *
     * @note When binternal is false (R interface), the result is transposed
     * to match R's column-major order expectation.
     */
    inline void RcppReduce_dataset_hdf5(std::string filename,
                                            std::string stringroup,
                                            std::string stroutgroup,
                                            std::string stroutdataset,
                                            std::string strreducefunction,
                                            bool boverwrite,
                                            bool bremove,
                                            bool binternal);


    // NOTE: 
    //      Set 
    //          - binternal = true for internal calls, for example if we are
    //          performing a reduction as a intermediate step using C++ interface,
    //          - binternal = false only for R interface (get transposed results)
    inline void RcppReduce_dataset_hdf5 ( std::string filename, 
                                   std::string stringroup, 
                                   std::string stroutgroup, 
                                   std::string stroutdataset, 
                                   std::string strreducefunction, 
                                   bool boverwrite,
                                   bool bremove,
                                   bool binternal)
    {
        
        BigDataStatMeth::hdf5File* objFile = nullptr;
        BigDataStatMeth::hdf5Dataset* dsIn = nullptr;
        BigDataStatMeth::hdf5Dataset* dsOut = nullptr;
        try {
            
            hsize_t* dims_out;
            std::vector<hsize_t> stride = {1, 1},
                block = {1, 1},
                offset = {0, 0};
            
            Eigen::MatrixXd fullReduced;
            Eigen::MatrixXd newRead;
            int ndatasets;
            
            objFile = new BigDataStatMeth::hdf5File(filename, false);
            objFile->openFile("r");
            
            // Get dataset names without prefix, all datasets inside the group
            Rcpp::StringVector joindata =  objFile->getDatasetNames(stringroup, "", "");
            
            delete objFile; // Close file 
            
            ndatasets = joindata.size();
            
            for ( int i=0; i< ndatasets; i++)
            {
                dsIn = new BigDataStatMeth::hdf5Dataset(filename, stringroup + "/" + joindata[i], false);
                dsIn->openDataset();

                if( dsIn->getDatasetptr() == nullptr) {
                    checkClose_file(dsIn);
                    Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (Dataset IException )" << std::endl;
                    return void();
                }
                
                dims_out =   dsIn->dim();
                
                std::vector<double> vdIn( dims_out[0] * dims_out[1] ); 
                dsIn->readDatasetBlock( {offset[0], offset[1]}, {dims_out[0], dims_out[1]}, stride, block, vdIn.data() );
                
                if( i == 0 ) {
                    
                    if(binternal == true)
                        fullReduced = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdIn.data(), dims_out[0], dims_out[1] );
                    else
                        fullReduced = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdIn.data(), dims_out[0], dims_out[1] );
                    
                } else {
                    
                    if(binternal == true)
                        newRead = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>  (vdIn.data(), dims_out[0], dims_out[1] );
                    else
                        newRead = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>>  (vdIn.data(), dims_out[0], dims_out[1] );
                    
                    if( newRead.rows() != fullReduced.rows()){
                        
                        int difference = std::abs(fullReduced.rows() - newRead.rows());
                        if( newRead.rows() > fullReduced.rows()) {
                            newRead.resize( newRead.rows() + difference, Eigen::NoChange);    
                        } else {
                            fullReduced.resize( fullReduced.rows() + difference, Eigen::NoChange);    
                        }
                    }
                    
                    if( newRead.cols() != fullReduced.cols()){
                        
                        int difference = std::abs(fullReduced.cols() - newRead.cols());
                        if( newRead.cols() > fullReduced.cols()){
                            newRead.resize( Eigen::NoChange, newRead.cols() + difference );
                        } else {
                            fullReduced.resize( Eigen::NoChange, fullReduced.cols() + difference );
                        }
                    }
                    
                    // Reduce matrix
                    if( strreducefunction.compare("+")==0) {
                        fullReduced = fullReduced + newRead;
                    } else if (strreducefunction.compare("-")==0) {
                        fullReduced = fullReduced - newRead;
                    } 
                    
                }
                
                if( bremove == true){
                    dsIn->remove();
                }
                
                delete dsIn; dsIn = nullptr;
            }
            
            dsOut = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, stroutdataset, boverwrite);
            
            if(binternal == true) {
                dsOut->createDataset( fullReduced.rows() , fullReduced.cols(), "real");
                // if( dsOut->getDatasetptr() != nullptr) {
                //     dsOut->writeDataset(Rcpp::wrap(fullReduced));
                // } else {
                //     checkClose_file(dsOut);
                //     Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (Dataset IException )" << std::endl;
                //     return void();
                // }
            } else {
                dsOut->createDataset( fullReduced.cols() , fullReduced.rows(), "real");
                fullReduced.transposeInPlace();
                
                // if( dsOut->getDatasetptr() != nullptr) {
                //     fullReduced.transposeInPlace();
                //     dsOut->writeDataset(Rcpp::wrap(fullReduced));
                // } else {
                //     checkClose_file(dsOut);
                //     Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (Dataset IException )" << std::endl;
                //     return void();
                // }
            }
            
            
            if( dsOut->getDatasetptr() != nullptr) {
                dsOut->writeDataset(Rcpp::wrap(fullReduced));
            } else {
                checkClose_file(dsOut);
                Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (Dataset IException )" << std::endl;
                return void();
            }
            
            delete dsOut; dsOut = nullptr;
            
        }catch( H5::FileIException& error ) {
            checkClose_file(dsIn, dsOut);
            // ::Rf_error( "c++ exception RcppReduce_dataset_hdf5 (File IException )" );
            Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (File IException )" << std::endl;
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the dstosplit operations
            checkClose_file(dsIn, dsOut);
            // ::Rf_error( "c++ exception RcppReduce_dataset_hdf5 (dstosplit IException )" );
            Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (dstosplit IException )" << std::endl;
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsIn, dsOut);
            // ::Rf_error( "c++ exception RcppReduce_dataset_hdf5 (DataSpace IException )" );
            Rcpp::Rcerr<< "c++ exception RcppReduce_dataset_hdf5 (DataSpace IException )" << std::endl;
            return void();
        } 
        return void();
    }




} // namespace BigDataStatMeth

#endif // BIGDATASTATMETH_UTIL_REDUCE_DATASETS_HPP
