#include <BigDataStatMeth.hpp>
// #include "hdf5Utilities/hdf5RemoveElements.hpp"

/**
 * @file hdf5_removeElement.cpp
 * @brief Implementation of element removal functionality for HDF5 files
 * @details This file provides functionality for removing groups and datasets
 * from HDF5 files. The implementation supports:
 * - Single element removal
 * - Multiple element removal
 * - Groups and datasets removal
 * - Safe file operations
 * 
 * Key features:
 * - Flexible element selection
 * - Safe file operations
 * - Comprehensive error handling
 * - Memory-safe implementation
 * - Support for nested paths
 */

/**
 * @brief Removes elements from an HDF5 file
 * 
 * @details Implements safe removal of groups and datasets from HDF5 files.
 * The function supports removal of multiple elements in a single operation
 * while maintaining file integrity.
 * 
 * Implementation features:
 * - Multiple element removal
 * - Safe file operations
 * - Memory management
 * - Error handling
 * - Path validation
 * 
 * @param filename Path to HDF5 file
 * @param elements Vector of paths to elements to remove
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::GroupIException for HDF5 group operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Remove Elements from HDF5 File
//'
//' @description
//' Removes specified groups or datasets from an HDF5 file.
//'
//' @details
//' This function provides safe element removal capabilities with:
//' 
//' * Removal options:
//'   - Single element removal
//'   - Multiple element removal
//'   - Groups and datasets removal
//' 
//' * Implementation features:
//'   - Safe file operations
//'   - Memory-efficient implementation
//'   - Comprehensive error handling
//'   - Path validation
//'
//' The function validates paths and performs safe removal operations.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param elements Character vector. Full paths to elements to remove
//'   (e.g., "group/dataset" or "group/subgroup").
//'
//' @return No return value, called for side effects (element removal).
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' matA <- matrix(1:15, nrow = 3, byrow = TRUE)
//' matB <- matrix(15:1, nrow = 3, byrow = TRUE)
//' 
//' # Save to HDF5
//' fn <- "test.hdf5"
//' bdCreate_hdf5_matrix(fn, matA, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix(fn, matB, "data", "matrix2",
//'                      overwriteFile = FALSE)
//' 
//' # Remove elements
//' bdRemove_hdf5_element(fn, c("data/matrix1", "data/matrix2"))
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
void bdRemove_hdf5_element(std::string filename, std::vector<std::string> elements)
{
    
    BigDataStatMeth::hdf5File* objFile = nullptr;
    
    try
    {
        
        H5::Exception::dontPrint();
        
        objFile = new BigDataStatMeth::hdf5File(filename, false);
        objFile->openFile("rw");
        
        if(objFile->getFileptr() != nullptr) { 
            BigDataStatMeth::RcppRemove_hdf5_elements(objFile, elements);    
        } else {
            Rcpp::Rcerr << "c++ exception bdRemove_hdf5_element: " << "File does not exist";
            return void();
        }
        
        delete objFile; objFile = nullptr;
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        delete objFile; objFile = nullptr;
        Rcpp::Rcerr << "c++ exception bdRemove_hdf5_element (File IException)";
        return void();
    } catch( H5::GroupIException & error ) { 
        delete objFile; objFile = nullptr;
        Rcpp::Rcerr << "c++ exception bdRemove_hdf5_element (Group IException)";
        return void();
    } catch( H5::DataSetIException& error ) { 
        delete objFile; objFile = nullptr;
        Rcpp::Rcerr << "c++ exception bdRemove_hdf5_element (DataSet IException)";
        return void();
    } catch(std::exception& ex) {
        delete objFile; objFile = nullptr;
        Rcpp::Rcerr << "c++ exception bdRemove_hdf5_element" << ex.what();
        return void();
    } catch (...) {
        delete objFile; objFile = nullptr;
        Rcpp::Rcerr << "c++ exception bdRemove_hdf5_element (unknown reason)";
        return void();
    }
    
    return void();
    
}
