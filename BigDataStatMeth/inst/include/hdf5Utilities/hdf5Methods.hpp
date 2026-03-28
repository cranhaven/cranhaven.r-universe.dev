/**
 * @file hdf5Methods.hpp
 * @brief Core HDF5 utility methods for dataset operations
 * 
 * This file provides core functionality for HDF5 dataset operations,
 * particularly focusing on joining multiple datasets. It implements
 * efficient methods for combining datasets while handling memory
 * management and error conditions.
 * 
 * Key features:
 * - Template-based dataset joining
 * - Support for both regular and internal HDF5 datasets
 * - Automatic memory management
 * - Comprehensive error handling
 * - Efficient data transfer using Eigen
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_HDF5_METHODS_HPP
#define BIGDATASTATMETH_HDF5_METHODS_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Joins multiple HDF5 datasets into a single dataset within the same group
     * 
     * @tparam T Dataset type (must be either hdf5Dataset* or hdf5DatasetInternal*)
     * @param dsJoined Pointer to the output dataset where joined data will be stored
     * @param strsubgroup Subgroup path where the datasets are located
     * @param strinput Vector of input dataset names to join
     * @param bremoveJoined Flag to remove original datasets after joining
     * @param byCols Flag indicating whether to join by columns
     * 
     * @return int Returns 0 on success, -1 on failure
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * @throws H5::DataTypeIException on datatype operation errors
     * @throws std::exception on general errors
     * 
     * @note The function uses unlimited datasets to allow for dynamic growth
     * @note Memory is managed efficiently using Eigen's mapping capabilities
     * 
     * Performance considerations:
     * - Uses block-wise reading and writing for memory efficiency
     * - Implements Eigen for fast matrix operations
     * - Automatically extends dataset size as needed
     * 
     * Implementation details:
     * 1. Creates an unlimited dataset for output
     * 2. Processes input datasets sequentially
     * 3. For each dataset:
     *    - Reads data into memory
     *    - Extends output dataset if needed
     *    - Writes data to the extended region
     * 4. Optionally removes original datasets
     * 
     * Example:
     * @code
     * BigDataStatMeth::hdf5Dataset* joined = new hdf5Dataset(...);
     * Rcpp::StringVector inputs = {"dataset1", "dataset2"};
     * join_datasets(joined, "/group", inputs, true, false);
     * @endcode
     */
    template< typename T>
    int join_datasets ( T* dsJoined, std::string strsubgroup, Rcpp::StringVector strinput, bool bremoveJoined, bool byCols )
    {
        static_assert(std::is_same<T*, BigDataStatMeth::hdf5Dataset* >::value || 
                      std::is_same<T*, BigDataStatMeth::hdf5DatasetInternal* >::value,
                      "Error - type not allowed");

        try{
            
            H5::Exception::dontPrint();
            
            std::vector<hsize_t> stride = {1, 1},
                                 block = {1, 1},
                                 offset = {0, 0},
                                 count = {0, 0};
            
            std::string stroutdataset = dsJoined->getDatasetName();
            
            
            
            BigDataStatMeth::hdf5Dataset* dstoJoin = new hdf5Dataset(dsJoined->getFullPath(), strsubgroup, Rcpp::as<std::string>(strinput[0]), false);
            dstoJoin->openDataset();
            
            hsize_t* dims_out = dstoJoin->dim();
            
            // Add rows and needed cols to add the merged data in the new dataset
            dsJoined->createUnlimitedDataset( (unsigned long long)dims_out[0], (unsigned long long)dims_out[1], "real");
            dsJoined->openDataset();

            // Read data to merge
            std::vector<double> vreadeddata( dims_out[0] * dims_out[1] ); 
            dstoJoin->readDatasetBlock( {offset[0], offset[1]}, {dims_out[0], dims_out[1]}, stride, block, vreadeddata.data() );
            {
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> readedData = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vreadeddata.data(),  dims_out[0], dims_out[1] );
                count[0] = dims_out[0]; count[1] = dims_out[1];
                
                // Write data to the new dataset
                dsJoined->writeDatasetBlock( Rcpp::wrap(readedData), offset, count, stride, block, false);
            }
            
            
            delete dstoJoin; // Remove original dataset link
            // Update offset to new position
            offset[1] = offset[1] + dims_out[1];
            
            for( int i=1; i<strinput.size(); i++)
            {
                
                dstoJoin = new hdf5Dataset(dsJoined->getFullPath(), strsubgroup, Rcpp::as<std::string>(strinput[i]), false);
                dstoJoin->openDataset();
                dims_out = dstoJoin->dim();
                
                // Extend dataset before put data
                dsJoined->extendUnlimitedDataset( (unsigned long long)0, (unsigned long long)dims_out[1] );
                
                // Read data to merge
                std::vector<double> vreadeddata( dims_out[0] * dims_out[1] ); 
                dstoJoin->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vreadeddata.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> readedData = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vreadeddata.data(),  dims_out[0], dims_out[1] );
                
                delete dstoJoin;
                
                count[0] = dims_out[0]; count[1] = dims_out[1];
                
                // Write data to the new dataset
                dsJoined->writeDatasetBlock( Rcpp::wrap(readedData), offset, count, stride, block, false);
                
                // Update offset
                offset[1] = offset[1] + dims_out[1];
            }
            
            if(bremoveJoined == true) {
                // Remove joined elements
                BigDataStatMeth::remove_elements(dsJoined->getFileptr(), strsubgroup, strinput);    
            }
            
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            checkClose_file(dsJoined);
            Rcpp::Rcerr<<"c++ exception join_datasets (File IException)" << std::endl;
            return -1;
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            checkClose_file(dsJoined);
            Rcpp::Rcerr<<"c++ exception join_datasets (DataSet IException)" << std::endl;
            return -1;
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            checkClose_file(dsJoined);
            Rcpp::Rcerr<<"c++ exception join_datasets (Group IException)" << std::endl;
            return -1;
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            checkClose_file(dsJoined);
            Rcpp::Rcerr<<"c++ exception join_datasets (DataSpace IException)" << std::endl;
            return -1;
        } catch(H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
            checkClose_file(dsJoined);
            Rcpp::Rcerr<<"c++ exception join_datasets (Data TypeIException)" << std::endl;
            return -1;
        } catch(std::exception &ex) {
            checkClose_file(dsJoined);
            Rcpp::Rcerr << "c++ exception join_datasets: " << ex.what();
            return -1;
        } catch (...) {
            checkClose_file(dsJoined);
            Rcpp::Rcerr<<"C++ exception join_datasets (unknown reason)";
            return -1;
        }
        return(0);
    }


}

#endif // BIGDATASTATMETH_HDF5_METHODS_HPP

