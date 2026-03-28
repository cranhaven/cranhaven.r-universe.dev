/**
 * @file hdf5_bindDatasets.cpp
 * @brief Dataset binding operations for HDF5 files
 * 
 * This file implements functionality for combining multiple HDF5 datasets through
 * row-wise and column-wise binding operations. It provides an interface similar
 * to R's rbind and cbind functions but optimized for HDF5 datasets.
 * 
 * Key features:
 * - Row-wise binding (rbind equivalent)
 * - Column-wise binding (cbind equivalent)
 * - Index-based row binding
 * - Automatic dimension validation
 * - Memory-efficient operations
 * - Comprehensive error handling
 * 
 * The implementation focuses on:
 * - Efficient handling of large datasets
 * - Proper memory management
 * - Flexible binding options
 * - Data integrity preservation
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#include <BigDataStatMeth.hpp>
#include "hdf5Utilities/hdf5BindDatasets.hpp"

/**
 * @brief Bind HDF5 datasets by rows or columns
 *
 * @details This function merges existing matrices within an HDF5 data file either by
 * combining their rows (stacking vertically) or columns (joining horizontally).
 * It provides functionality similar to R's rbind and cbind operations.
 *
 * Supported binding operations:
 * - bindCols: Merge datasets by columns (horizontal joining)
 * - bindRows: Merge datasets by rows (vertical stacking)
 * - bindRowsbyIndex: Merge datasets by rows using an index
 *
 * @param filename [in] Name of the HDF5 file
 * @param group [in] Input group containing the datasets
 * @param datasets [in] Input datasets to bind
 * @param outgroup [in] Output group for merged dataset
 * @param outdataset [in] Name for the new merged dataset
 * @param func [in] Binding operation to perform
 * @param overwrite [in] Whether to overwrite existing datasets
 *
 * @return void
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::GroupIException if group operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception for other errors
 *
 * @note Memory efficiency is achieved through block-wise processing
 * @see RcppBind_datasets_hdf5()
 */

//' Bind matrices by rows or columns
//'
//' This function merges existing matrices within an HDF5 data file either by
//' combining their rows (stacking vertically) or columns (joining horizontally).
//' It provides functionality similar to R's rbind and cbind operations.
//' 
//' @param filename Character array indicating the name of the file to create
//' @param group Character array indicating the input group containing the datasets
//' @param datasets Character array specifying the input datasets to bind
//' @param outgroup Character array indicating the output group for the merged dataset.
//'        If NULL, output is stored in the same input group
//' @param outdataset Character array specifying the name for the new merged dataset
//' @param func Character array specifying the binding operation:
//'        - "bindRows": Merge datasets by rows (vertical stacking)
//'        - "bindCols": Merge datasets by columns (horizontal joining)
//'        - "bindRowsbyIndex": Merge datasets by rows using an index
//' @param overwrite Boolean indicating whether to overwrite existing datasets.
//'        Defaults to false
//' 
//' @return A list containing the location of the combined dataset:
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the result}
//'     \item{ds}{Character string. Full dataset path to the bound/combined dataset within the HDF5 file}
//'   }
//' 
//' @details
//' The function performs dimension validation before binding:
//' - For row binding: All datasets must have the same number of columns
//' - For column binding: All datasets must have the same number of rows
//' 
//' Memory efficiency is achieved through:
//' - Block-wise reading and writing
//' - Minimal data copying
//' - Proper resource cleanup
//' 
//' @note When binding by rows with an index, the index determines the
//'       order of combination
//' 
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' a <- matrix(1:12, 4, 3)
//' b <- matrix(13:24, 4, 3)
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", a, "data", "A")
//' bdCreate_hdf5_matrix("test.hdf5", b, "data", "B")
//' 
//' # Bind by rows
//' bdBind_hdf5_datasets("test.hdf5", "data", 
//'                      c("A", "B"),
//'                      "results", "combined",
//'                      "bindRows")
//' }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdBind_hdf5_datasets( std::string filename, std::string group, Rcpp::StringVector datasets, 
                  std::string outgroup, std::string outdataset, std::string func,
                  Rcpp::Nullable<bool> overwrite = false )
{
    
    BigDataStatMeth::hdf5Dataset* dsOut  = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try
    {
        H5::Exception::dontPrint();
        
        Rcpp::NumericVector oper = {0, 1, 2};
        oper.names() = Rcpp::CharacterVector({ "bindCols", "bindRows", "bindRowsbyIndex"});
        
        bool boverwrite;
        
        if( overwrite.isNull()) { boverwrite = false; } 
        else {   boverwrite = Rcpp::as<bool>(overwrite); }

        if (func.compare("bindCols") != 0 && func.compare("bindRows") != 0  && func.compare("bindRowsbyIndex") != 0 ) {
            throw std::range_error( "Function to apply must be \"bindRows\", \"bindCols\" or \"bindRowsbyIndex\" other values are not allowed" );
            return(lst_return);
            // return void();
        }
        
        int bindFunction = oper.findName( func );
        
        dsOut = new BigDataStatMeth::hdf5Dataset(filename, outgroup, outdataset, boverwrite);
        
        RcppBind_datasets_hdf5( filename, group, datasets, dsOut, bindFunction, false);
        
        lst_return["fn"] = filename;
        lst_return["ds"] = outgroup + "/" + outdataset;
        
        delete dsOut; dsOut = nullptr;
        
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsOut);
        Rcpp::stop("c++ exception bdBind_hdf5_datasets (File IException)");
        // return void();
    } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsOut);
        Rcpp::stop ("c++ exception bdBind_hdf5_datasets (Group IException)");
        // return void();
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsOut);
        Rcpp::stop ("c++ exception bdBind_hdf5_datasets (DataSet IException)");
        // return void();
    } catch(std::exception& ex) {
        checkClose_file(dsOut);
        Rcpp::stop ("c++ exception bdBind_hdf5_datasets %s", ex.what());
        // return void();
    } catch (...) {
        checkClose_file(dsOut);
        Rcpp::stop("c++ exception bdBind_hdf5_datasets (unknown reason)");
        // return void();
    }
    
    // file->close();
    Rcpp::Rcout<< outdataset <<" dataset has been recomposed from blocks\n";
    return(lst_return);
    // return void();
    
}
