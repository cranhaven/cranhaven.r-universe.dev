#include <BigDataStatMeth.hpp>
// #include "hdf5Utilities/hdf5ReduceDataset.hpp"

/**
 * @file hdf5_reduceDataset.cpp
 * @brief Implementation of dataset reduction operations for HDF5-stored matrices
 * @details This file provides functionality for reducing multiple datasets
 * within an HDF5 group using arithmetic operations. The implementation supports:
 * - Addition and subtraction operations
 * - Flexible output location
 * - Optional source dataset removal
 * - Memory-efficient operations
 * 
 * Key features:
 * - Support for large datasets
 * - Configurable output location
 * - Memory-efficient implementation
 * - Comprehensive error handling
 * - Optional cleanup functionality
 */

/**
 * @brief Reduces multiple datasets using arithmetic operations
 * 
 * @details Implements efficient reduction of multiple datasets within an HDF5
 * group using specified arithmetic operations. The function supports addition
 * and subtraction operations with configurable output handling.
 * 
 * Implementation features:
 * - Flexible operation selection
 * - Configurable output location
 * - Memory-efficient processing
 * - Safe file operations
 * - Optional source cleanup
 * 
 * @param filename Path to HDF5 file
 * @param group Input group containing datasets
 * @param reducefunction Operation to apply ('+' or '-')
 * @param outgroup Output group for result
 * @param outdataset Output dataset name
 * @param overwrite Whether to overwrite existing dataset
 * @param remove Whether to remove source datasets
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::GroupIException for HDF5 group operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Reduce Multiple HDF5 Datasets
//'
//' @description
//' Reduces multiple datasets within an HDF5 group using arithmetic operations
//' (addition or subtraction).
//'
//' @details
//' This function provides efficient dataset reduction capabilities with:
//' 
//' * Operation options:
//'   - Addition of datasets
//'   - Subtraction of datasets
//' 
//' * Output options:
//'   - Custom output location
//'   - Configurable dataset name
//'   - Overwrite protection
//' 
//' * Implementation features:
//'   - Memory-efficient processing
//'   - Safe file operations
//'   - Optional source cleanup
//'   - Comprehensive error handling
//'
//' The function processes datasets efficiently while maintaining data integrity.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing datasets.
//' @param reducefunction Character. Operation to apply, either "+" or "-".
//' @param outgroup Character string (optional). Output group path. If NULL,
//'   uses input group.
//' @param outdataset Character string (optional). Output dataset name. If NULL,
//'   uses input group name.
//' @param overwrite Logical (optional). Whether to overwrite existing dataset.
//'   Default is FALSE.
//' @param remove Logical (optional). Whether to remove source datasets after
//'   reduction. Default is FALSE.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the reduced dataset (group/dataset)}
//'   \item{func}{Character string with the reduction function applied}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' X1 <- matrix(1:100, 10, 10)
//' X2 <- matrix(101:200, 10, 10)
//' X3 <- matrix(201:300, 10, 10)
//' 
//' # Save to HDF5
//' fn <- "test.hdf5"
//' bdCreate_hdf5_matrix(fn, X1, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix(fn, X2, "data", "matrix2",
//'                      overwriteFile = FALSE)
//' bdCreate_hdf5_matrix(fn, X3, "data", "matrix3",
//'                      overwriteFile = FALSE)
//' 
//' # Reduce datasets by addition
//' bdReduce_hdf5_dataset(
//'   filename = fn,
//'   group = "data",
//'   reducefunction = "+",
//'   outgroup = "results",
//'   outdataset = "sum_matrix",
//'   overwrite = TRUE
//' )
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
Rcpp::List bdReduce_hdf5_dataset( std::string filename, std::string group, 
                            std::string reducefunction, 
                            Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                            Rcpp::Nullable<std::string> outdataset = R_NilValue,
                            Rcpp::Nullable<bool> overwrite = false , 
                            Rcpp::Nullable<bool> remove = false )
{
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "",
                                               Rcpp::Named("func") = "");
    
    try
    {
        
        H5::Exception::dontPrint();
       
        std::string strOutgroup,
                    strOutdatset,
                    stroutdata;
        
        bool boverwrite, bremove;
        
        if (reducefunction.compare("+") != 0 && reducefunction.compare("-") != 0 ) {
            throw std::range_error( "Function to apply must be \"+\" or \"-\" other values are not allowed" );
            return(lst_return);
        }
        
        if(outgroup.isNull()) {  strOutgroup = group ;
        } else {   strOutgroup = Rcpp::as<std::string>(outgroup);}
        
        if(remove.isNull()) { bremove = false ;
        } else {   bremove = Rcpp::as<bool>(remove);}
        
        if(overwrite.isNull()) { boverwrite = false; } 
        else {   boverwrite = Rcpp::as<bool>(overwrite); }
        
        if(outdataset.isNull()){  strOutdatset = group ;
        } else {   strOutdatset = Rcpp::as<std::string>(outdataset);}
        
        BigDataStatMeth::RcppReduce_dataset_hdf5( filename, group, strOutgroup, strOutdatset, reducefunction, boverwrite, bremove, false);
        
        lst_return["fn"] = filename;
        lst_return["ds"] = strOutgroup + "/" + strOutdatset;
        lst_return["func"] = reducefunction;
        
    } catch( H5::FileIException& error ) {  
        Rcpp::Rcerr << "c++ exception bdReduce_hdf5_dataset (File IException)";
        return(lst_return);
    } catch( H5::GroupIException & error ) { 
        Rcpp::Rcerr << "c++ exception bdReduce_hdf5_dataset (Group IException)";
        return(lst_return);
    } catch( H5::DataSetIException& error ) { 
        Rcpp::Rcerr << "c++ exception bdReduce_hdf5_dataset (DataSet IException)";
        return(lst_return);
    } catch(std::exception& ex) {
        Rcpp::Rcerr << "c++ exception bdReduce_hdf5_dataset" << ex.what();
        return(lst_return);
    }
    
    return(lst_return);
}
