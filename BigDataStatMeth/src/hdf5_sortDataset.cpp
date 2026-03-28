#include <BigDataStatMeth.hpp>
#include "hdf5Utilities/hdf5SortDataset.hpp"

/**
 * @file hdf5_sortDataset.cpp
 * @brief Implementation of dataset sorting functionality for HDF5 matrices
 * @details This file provides functionality for sorting datasets stored in HDF5
 * format based on predefined sorting orders. The implementation supports:
 * - Row-wise and column-wise sorting
 * - Block-based sorting operations
 * - Flexible output options
 * - Memory-efficient operations
 * 
 * Key features:
 * - Support for large datasets
 * - Block-based processing
 * - Flexible sorting criteria
 * - Memory-efficient implementation
 * - Comprehensive error handling
 */

/**
 * @brief Sorts HDF5 dataset based on predefined order
 * 
 * @details Implements efficient sorting of HDF5 datasets using block-based
 * operations and predefined sorting orders. The function supports both row-wise
 * and column-wise sorting with flexible output options.
 * 
 * Implementation features:
 * - Block-based sorting
 * - Memory-efficient operations
 * - Safe file operations
 * - Flexible output handling
 * - Comprehensive error handling
 * 
 * @param filename Path to HDF5 file
 * @param group Input group containing dataset
 * @param dataset Input dataset name
 * @param outdataset Output dataset name
 * @param blockedSortlist List of sorting blocks
 * @param func Sorting function to apply
 * @param outgroup Output group for results
 * @param overwrite Whether to overwrite existing dataset
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::GroupIException for HDF5 group operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Sort HDF5 Dataset Using Predefined Order
//'
//' @description
//' Sorts a dataset in an HDF5 file based on a predefined ordering specified
//' through a list of sorting blocks.
//'
//' @details
//' This function provides efficient dataset sorting capabilities with:
//' 
//' * Sorting options:
//'   - Row-wise sorting
//'   - Column-wise sorting
//'   - Block-based processing
//' 
//' * Implementation features:
//'   - Memory-efficient processing
//'   - Block-based operations
//'   - Safe file operations
//'   - Progress reporting
//'
//' The sorting order is specified through a list of data frames, where each
//' data frame represents a block of elements to be sorted. Each data frame
//' must contain:
//' - Row names (current identifiers)
//' - chr (new identifiers)
//' - order (current positions)
//' - newOrder (target positions)
//'
//' Example sorting blocks structure:
//' 
//' Block 1 (maintaining order):
//'                       chr order newOrder Diagonal
//' TCGA-OR-A5J1 TCGA-OR-A5J1     1        1        1
//' TCGA-OR-A5J2 TCGA-OR-A5J2     2        2        1
//' TCGA-OR-A5J3 TCGA-OR-A5J3     3        3        1
//' TCGA-OR-A5J4 TCGA-OR-A5J4     4        4        1
//'
//' Block 2 (reordering with new identifiers):
//'                       chr order newOrder Diagonal
//' TCGA-OR-A5J5 TCGA-OR-A5JA    10        5        1
//' TCGA-OR-A5J6 TCGA-OR-A5JB    11        6        1
//' TCGA-OR-A5J7 TCGA-OR-A5JC    12        7        0
//' TCGA-OR-A5J8 TCGA-OR-A5JD    13        8        1
//'
//' Block 3 (reordering with identifier swaps):
//'                       chr order newOrder Diagonal
//' TCGA-OR-A5J9 TCGA-OR-A5J5     5        9        1
//' TCGA-OR-A5JA TCGA-OR-A5J6     6       10        1
//' TCGA-OR-A5JB TCGA-OR-A5J7     7       11        1
//' TCGA-OR-A5JC TCGA-OR-A5J8     8       12        1
//' TCGA-OR-A5JD TCGA-OR-A5J9     9       13        0
//'
//' In this example:
//' - Block 1 maintains the original order
//' - Block 2 assigns new identifiers (A5JA-D) to elements
//' - Block 3 swaps identifiers between elements
//' - The Diagonal column indicates whether the element is on the diagonal (1) or not (0)
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing input dataset.
//' @param dataset Character string. Name of the dataset to sort.
//' @param outdataset Character string. Name for the sorted dataset.
//' @param blockedSortlist List of data frames. Each data frame specifies the
//'   sorting order for a block of elements. See Details for structure.
//' @param func Character string. Function to apply:
//'   - "sortRows" for row-wise sorting
//'   - "sortCols" for column-wise sorting
//' @param outgroup Character string (optional). Output group path. If NULL,
//'   uses input group.
//' @param overwrite Logical (optional). Whether to overwrite existing dataset.
//'   Default is FALSE.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the sorted dataset (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test data
//' data <- matrix(rnorm(100), 10, 10)
//' rownames(data) <- paste0("TCGA-OR-A5J", 1:10)
//' 
//' # Save to HDF5
//' fn <- "test.hdf5"
//' bdCreate_hdf5_matrix(fn, data, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' 
//' # Create sorting blocks
//' block1 <- data.frame(
//'   chr = paste0("TCGA-OR-A5J", c(2,1,3,4)),
//'   order = 1:4,
//'   newOrder = c(2,1,3,4),
//'   row.names = paste0("TCGA-OR-A5J", 1:4)
//' )
//' 
//' block2 <- data.frame(
//'   chr = paste0("TCGA-OR-A5J", c(6,5,8,7)),
//'   order = 5:8,
//'   newOrder = c(6,5,8,7),
//'   row.names = paste0("TCGA-OR-A5J", 5:8)
//' )
//' 
//' # Sort dataset
//' bdSort_hdf5_dataset(
//'   filename = fn,
//'   group = "data",
//'   dataset = "matrix1",
//'   outdataset = "matrix1_sorted",
//'   blockedSortlist = list(block1, block2),
//'   func = "sortRows"
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
Rcpp::List bdSort_hdf5_dataset( std::string filename, std::string group, 
                          std::string dataset, std::string outdataset, 
                          Rcpp::List blockedSortlist, std::string func, 
                          Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                          Rcpp::Nullable<bool> overwrite = false )
{
    
    BigDataStatMeth::hdf5Dataset* dsIn = nullptr;
    BigDataStatMeth::hdf5Dataset* dsOut = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");

    try
    {
        
        H5::Exception::dontPrint();
        
        std::string strOutgroup;
        bool boverwrite;
        hsize_t ncols = 0,
                nrows = 0;
        
        if( blockedSortlist.length()<=0 ) {
            Rcpp::Rcerr<<"\nList is empty, please create a list with the new sort";
            return(lst_return);
        }
        
        if( overwrite.isNull() ) { boverwrite = false; } 
        else { boverwrite = Rcpp::as<bool>(overwrite); }
        
        if( outgroup.isNull() ) {  strOutgroup = group;  } 
        else {  strOutgroup = Rcpp::as<std::string>(outgroup); }
        
        dsIn = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
        dsIn->openDataset();
        
        if( dsIn->getDatasetptr() == nullptr ) {
            checkClose_file(dsIn);
            Rcpp::Rcerr<<"\nError opening dataset";
            return(lst_return);
        }
        
        ncols = dsIn->ncols();

        // Get the nomber of rows in dataframes inside the list
        for(int i = 0; i < blockedSortlist.size(); i++) {     
            Rcpp::DataFrame df(blockedSortlist[i]);
            nrows = nrows + df.nrow();
        } 
        dsOut = new BigDataStatMeth::hdf5Dataset(filename, strOutgroup, outdataset, boverwrite);
        dsOut->createDataset( ncols, nrows, "real");
        
        if( dsOut->getDatasetptr() != nullptr ) {
            RcppSort_dataset_hdf5(dsIn, dsOut, blockedSortlist, func);
        } else {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"\nError creating dataset";
            return(lst_return);
        }
        
        delete dsIn; dsIn = nullptr;
        delete dsOut; dsOut = nullptr;
        
        lst_return["fn"] = filename;
        lst_return["ds"] = strOutgroup + "/" + outdataset;
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ exception bdSort_hdf5_dataset (File IException)";
        return(lst_return);
    } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr << "c++ exception bdSort_hdf5_dataset (Group IException)";
        return(lst_return);
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr << "c++ exception bdSort_hdf5_dataset (DataSet IException)";
        return(lst_return);
    } catch(std::exception& ex) {
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr << "c++ exception bdSort_hdf5_dataset" << ex.what();
        return(lst_return);
    } catch (...) {
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr << "c++ exception bdSort_hdf5_dataset (unknown reason)";
        return(lst_return);
    }
    
    Rcpp::Rcout<<outdataset<<" dataset has been sorted \n";
    return(lst_return);
    
}
