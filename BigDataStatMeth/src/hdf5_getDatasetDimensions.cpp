#include <BigDataStatMeth.hpp>

/**
 * @file hdf5_getDatasetDimensions.cpp
 * @brief Implementation of HDF5 dataset dimension retrieval functionality
 * @details This file provides functionality for retrieving the dimensions of
 * datasets stored in HDF5 files. The implementation supports:
 * - Row and column dimension retrieval
 * - Safe HDF5 file operations
 * - Error handling for missing or invalid datasets
 * - Memory-safe implementation
 * 
 * Key features:
 * - Efficient dimension retrieval
 * - Comprehensive error handling
 * - Memory-safe operations
 * - Support for read-only access
 */

/**
 * @brief Retrieves dimensions of an HDF5 dataset
 * 
 * @details Implements safe and efficient retrieval of dataset dimensions from
 * HDF5 files. The function handles file operations safely and provides
 * comprehensive error handling.
 * 
 * Implementation features:
 * - Safe file opening and closing
 * - Memory management for HDF5 resources
 * - Error handling for invalid datasets
 * - Support for large datasets
 * 
 * @param filename Path to HDF5 file
 * @param dataset Full path to dataset within HDF5 file
 * 
 * @return Vector containing row and column dimensions
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::GroupIException if there are HDF5 group operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Get HDF5 Dataset Dimensions
//'
//' @description
//' Retrieves the dimensions (number of rows and columns) of a dataset stored in
//' an HDF5 file.
//'
//' @details
//' This function provides efficient access to dataset dimensions in HDF5 files.
//' Key features:
//' 
//' * Dimension information:
//'   - Number of rows
//'   - Number of columns
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
//' @param dataset Character string. Full path to the dataset within the HDF5 file
//'   (e.g., "group/subgroup/dataset").
//'
//' @return Integer vector of length 2 containing:
//'   - \[1\] Number of rows
//'   - \[2\] Number of columns
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create a test HDF5 file
//' fn <- "test.hdf5"
//' X <- matrix(rnorm(100), 10, 10)
//' 
//' # Save matrix to HDF5
//' bdCreate_hdf5_matrix(fn, X, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' 
//' # Get dimensions
//' dims <- bdgetDim_hdf5(fn, "data/matrix1")
//' print(paste("Rows:", dims[1]))
//' print(paste("Columns:", dims[2]))
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
//' * \code{\link{bdgetDatasetsList_hdf5}} for listing available datasets
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject  bdgetDim_hdf5( std::string filename, std::string dataset)
{
    
    BigDataStatMeth::hdf5Dataset* ds = nullptr;
    Rcpp::IntegerVector dims(2);
    
    try
    {
        
        H5::Exception::dontPrint();
        
        ds = new BigDataStatMeth::hdf5Dataset(filename, dataset, false);
        ds->openDataset();
        
        if( ds->getDatasetptr() != nullptr ) { 
            dims[0] = ds->nrows_r();  
            dims[1] = ds->ncols_r();
        } else {
            Rcpp::Rcerr << "c++ exception bdgetDim_hdf5: " << "Error opening dataset";
        }
        
        delete ds; ds = nullptr;
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(ds);
        Rcpp::Rcerr << "c++ exception bdgetDim_hdf5 (File IException)";
        return(dims);
    } catch( H5::GroupIException & error ) { 
        checkClose_file(ds);
        Rcpp::Rcerr<<"\nc++ exception bdgetDim_hdf5 (Group IException)";
        return(dims);
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(ds);
        Rcpp::Rcerr << "c++ exception bdgetDim_hdf5 (DataSet IException)";
        return(dims);
    } catch(std::exception& ex) {
        checkClose_file(ds);
        Rcpp::Rcerr << "c++ exception bdgetDim_hdf5" << ex.what();
        return(dims);
    } catch (...) {
        checkClose_file(ds);
        Rcpp::Rcerr << "c++ exception bdgetDim_hdf5 (unknown reason)";
        return(dims);
    }
    
    return(dims);
}
