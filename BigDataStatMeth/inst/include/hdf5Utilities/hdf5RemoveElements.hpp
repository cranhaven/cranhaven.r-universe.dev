/**
 * @file hdf5RemoveElements.hpp
 * @brief Utilities for removing elements from HDF5 files
 * 
 * This file provides functionality for safely removing elements (datasets, groups)
 * from HDF5 files. It implements error checking and proper cleanup procedures
 * to maintain file integrity during element removal operations.
 * 
 * Key features:
 * - Safe element removal with existence checking
 * - Comprehensive error handling
 * - Support for multiple element removal
 * - Informative status messages
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_UTIL_REMOVE_ELEMENT_HPP
#define BIGDATASTATMETH_UTIL_REMOVE_ELEMENT_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Removes specified elements from an HDF5 file
     * 
     * This function safely removes multiple elements from an HDF5 file. It checks
     * for the existence of each element before attempting removal and provides
     * appropriate feedback messages.
     * 
     * @param file Pointer to the HDF5 file object
     * @param elements Vector of element paths to be removed
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * 
     * @note The function will continue processing even if some elements fail to remove
     * @note Elements that don't exist will be reported but won't cause function failure
     * 
     * Implementation details:
     * 1. Checks if there are elements to remove
     * 2. For each element:
     *    - Verifies existence
     *    - Attempts removal using H5Ldelete
     *    - Reports any failures
     * 3. Provides feedback through Rcpp messages
     * 
     * Example:
     * @code
     * BigDataStatMeth::hdf5File* file = new hdf5File("data.h5");
     * std::vector<std::string> elements = {"/group1/dataset1", "/group2"};
     * RcppRemove_hdf5_elements(file, elements);
     * @endcode
     */
    inline void RcppRemove_hdf5_elements(BigDataStatMeth::hdf5File* file, std::vector<std::string> elements)
    {
        
        try
        {
            H5::Exception::dontPrint();
            
            
            if(elements.size() == 0) {
                std::string strmessage = "Nothing to be removed removed";
                Rcpp::message(Rcpp::wrap(strmessage));
            } else { // Remove datasets
                // for (int i=0; i<elements.size(); i++) 
                for (size_t i = 0; i < elements.size(); ++i)
                {
                    H5std_string element = "" + elements[i];
                    if(exists_HDF5_element( file->getFileptr(), element))
                    {
                        int result = H5Ldelete( (file->getFileptr())->getId(), element.data(), H5P_DEFAULT);  
                        if(result<0) {
                            Rcpp::Rcout<<"\n Error removing : "<<elements[i]<<"\n";
                        } 
                    } else {
                        Rcpp::Rcout<<"\n Element: "<<elements[i]<<" does not exists \n";
                    }
                }    
            }
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            Rcpp::Rcerr<<"c++ exception RcppRemove_hdf5_elements (File IException)";
            return void();
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            Rcpp::Rcerr<<"c++ exception RcppRemove_hdf5_elements (Group IException)";
            return void();
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            Rcpp::Rcerr<<"c++ exception RcppRemove_hdf5_elements (DataSet IException)";
            return void();
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            Rcpp::Rcerr<<"c++ exception RcppRemove_hdf5_elements (DataSpace IException)";
            return void();
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception RcppRemove_hdf5_elements: " << ex.what();
            return void();
        } catch (...) {
            Rcpp::Rcerr<<"C++ exception RcppRemove_hdf5_elements (unknown reason)";
            return void();
        }
        
        return void();
    }

}

#endif // BIGDATASTATMETH_UTIL_REMOVE_ELEMENT_HPP
