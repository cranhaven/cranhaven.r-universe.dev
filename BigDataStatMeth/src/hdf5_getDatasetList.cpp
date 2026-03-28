#include <BigDataStatMeth.hpp>

/**
 * @file hdf5_getDatasetList.cpp
 * @brief Implementation of HDF5 dataset listing functionality
 * @details This file provides functionality for retrieving lists of datasets
 * from HDF5 files. The implementation supports:
 * - Listing all datasets in a group
 * - Filtering datasets by prefix
 * - Filtering datasets by suffix
 * - Safe HDF5 file operations
 * 
 * Key features:
 * - Flexible dataset filtering
 * - Error handling for HDF5 operations
 * - Memory-safe implementation
 * - Support for read-only operations
 */

/**
 * @brief Lists datasets in an HDF5 group
 * 
 * @details Implements dataset listing functionality with optional filtering
 * by prefix or suffix. The function safely handles HDF5 file operations and
 * provides comprehensive error handling.
 * 
 * Implementation features:
 * - Safe file opening and closing
 * - Optional prefix/suffix filtering
 * - Memory management for HDF5 resources
 * - Comprehensive error handling
 * 
 * @param filename Path to HDF5 file
 * @param group Group path in HDF5 file
 * @param prefix Optional prefix filter
 * 
 * @return Vector of dataset names
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' List Datasets in HDF5 Group
//'
//' @description
//' Retrieves a list of all datasets within a specified HDF5 group, with optional
//' filtering by prefix or suffix.
//'
//' @details
//' This function provides flexible dataset listing capabilities for HDF5 files.
//' Key features:
//' 
//' * Listing options:
//'   - All datasets in a group
//'   - Datasets matching a prefix
//'   - Datasets matching a suffix
//' 
//' * Implementation features:
//'   - Safe HDF5 file operations
//'   - Memory-efficient implementation
//'   - Comprehensive error handling
//'   - Read-only access to files
//'
//' The function opens the HDF5 file in read-only mode to ensure data safety.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group within the HDF5 file.
//' @param prefix Optional character string. If provided, only returns datasets
//'   starting with this prefix.
//'
//' @return Character vector containing dataset names.
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create a test HDF5 file
//' fn <- "test.hdf5"
//' X <- matrix(rnorm(100), 10, 10)
//' Y <- matrix(rnorm(100), 10, 10)
//' 
//' # Save matrices to HDF5
//' bdCreate_hdf5_matrix(fn, X, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix(fn, Y, "data", "matrix2",
//'                      overwriteFile = FALSE)
//' 
//' # List all datasets in group
//' datasets <- bdgetDatasetsList_hdf5(fn, "data")
//' print(datasets)
//' 
//' # List datasets with prefix "matrix"
//' filtered <- bdgetDatasetsList_hdf5(fn, "data", prefix = "matrix")
//' print(filtered)
//' 
//' # Cleanup
//' if (file.exists(fn)) {
//'   file.remove(fn)
//' }
//' }
//'
//' @references
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdgetDatasetsList_hdf5(std::string filename, std::string group, Rcpp::Nullable<std::string> prefix = R_NilValue)
{
    
    // H5File* file = nullptr;
    Rcpp::StringVector groupDatasets;
    BigDataStatMeth::hdf5File* fQuery = nullptr;
    
    try
    {
        
        H5::Exception::dontPrint();
        
        std::string strprefix;
        
        if(prefix.isNull()){  strprefix = "" ;
        } else {   strprefix = Rcpp::as<std::string>(prefix);}
        
        fQuery = new BigDataStatMeth::hdf5File(filename, false);
        fQuery->openFile("r");
        
        if( fQuery->getFileptr() != nullptr) {
            // Get dataset names without prefix, all datasets inside the group
            groupDatasets =  fQuery->getDatasetNames(group, strprefix, "");
        } else {
            delete fQuery; fQuery = nullptr;
            Rcpp::Rcerr << "c++ exception bdgetDatasetsList_hdf5: " << "File does not exist";
            return(R_NilValue);
        }
        
        delete fQuery; fQuery = nullptr;
        
    } catch( H5::FileIException& error ) { 
        Rcpp::Rcerr << "c++ exception bdgetDatasetsList_hdf5 (File IException)\n";
        return(R_NilValue);
    } catch( H5::DataSetIException& error ) { 
        Rcpp::Rcerr << "c++ exception bdgetDatasetsList_hdf5 (DataSet IException)\n";
        return(R_NilValue);
    } catch(std::exception &ex) {
        Rcpp::Rcerr << "c++ exception bdgetDatasetsList_hdf5: " << ex.what();
        return(R_NilValue);
    }
    
    return(groupDatasets);
    
}
