#include <BigDataStatMeth.hpp>
// #include "hdf5Utilities/hdf5SplitDataset.hpp"

/**
 * @file hdf5_splitDataset.cpp
 * @brief Implementation of dataset splitting functionality for HDF5 matrices
 * @details This file provides functionality for splitting large datasets in HDF5
 * format into smaller submatrices. The implementation supports:
 * - Row-wise and column-wise splitting
 * - Block size or block count specification
 * - Flexible output options
 * - Memory-efficient operations
 * 
 * Key features:
 * - Support for large datasets
 * - Configurable splitting strategy
 * - Memory-efficient implementation
 * - Comprehensive error handling
 * - Progress reporting
 */

/**
 * @brief Splits HDF5 dataset into smaller submatrices
 * 
 * @details Implements efficient splitting of large HDF5 datasets into smaller
 * submatrices. The function supports both row-wise and column-wise splitting
 * with configurable block sizes or counts.
 * 
 * Implementation features:
 * - Flexible splitting options
 * - Memory-efficient operations
 * - Safe file operations
 * - Progress tracking
 * - Comprehensive error handling
 * 
 * @param filename Path to HDF5 file
 * @param group Input group containing dataset
 * @param dataset Input dataset name
 * @param outgroup Output group for results
 * @param outdataset Base name for output datasets
 * @param nblocks Number of blocks to split into
 * @param blocksize Size of each block
 * @param bycols Whether to split by columns
 * @param overwrite Whether to overwrite existing datasets
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws H5::DataSpaceIException for HDF5 dataspace errors
 * @throws H5::DataTypeIException for HDF5 datatype errors
 * @throws std::exception for other errors
 */

//' Split HDF5 Dataset into Submatrices
//'
//' @description
//' Splits a large dataset in an HDF5 file into smaller submatrices, with
//' support for both row-wise and column-wise splitting.
//'
//' @details
//' This function provides efficient dataset splitting capabilities with:
//' 
//' * Splitting options:
//'   - Row-wise or column-wise splitting
//'   - Fixed block size splitting
//'   - Fixed block count splitting
//' 
//' * Implementation features:
//'   - Memory-efficient processing
//'   - Block-based operations
//'   - Safe file operations
//'   - Progress reporting
//'
//' The function supports two splitting strategies:
//' 1. By number of blocks: Splits the dataset into a specified number of
//'    roughly equal-sized blocks
//' 2. By block size: Splits the dataset into blocks of a specified size
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing input dataset.
//' @param dataset Character string. Name of the dataset to split.
//' @param outgroup Character string (optional). Output group path. If NULL,
//'   uses input group.
//' @param outdataset Character string (optional). Base name for output datasets.
//'   If NULL, uses input dataset name with block number suffix.
//' @param nblocks Integer (optional). Number of blocks to split into.
//'   Mutually exclusive with blocksize.
//' @param blocksize Integer (optional). Size of each block.
//'   Mutually exclusive with nblocks.
//' @param bycols Logical (optional). Whether to split by columns (TRUE) or
//'   rows (FALSE). Default is TRUE.
//' @param overwrite Logical (optional). Whether to overwrite existing datasets.
//'   Default is FALSE.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the output group path where the split 
//'   datasets are stored. Multiple datasets are created in this location named 
//'   as \<outdataset\>.1, \<outdataset\>.2, etc.}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test data
//' data <- matrix(rnorm(1000), 100, 10)
//' 
//' # Save to HDF5
//' fn <- "test.hdf5"
//' bdCreate_hdf5_matrix(fn, data, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' 
//' # Split by number of blocks
//' bdSplit_matrix_hdf5(
//'   filename = fn,
//'   group = "data",
//'   dataset = "matrix1",
//'   outgroup = "data_split",
//'   outdataset = "block",
//'   nblocks = 4,
//'   bycols = TRUE
//' )
//' 
//' # Split by block size
//' bdSplit_matrix_hdf5(
//'   filename = fn,
//'   group = "data",
//'   dataset = "matrix1",
//'   outgroup = "data_split2",
//'   outdataset = "block",
//'   blocksize = 25,
//'   bycols = TRUE
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
Rcpp::List bdSplit_matrix_hdf5( std::string filename, std::string group, std::string dataset, 
                          Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                          Rcpp::Nullable<std::string> outdataset = R_NilValue, 
                          Rcpp::Nullable<int> nblocks = R_NilValue,  
                          Rcpp::Nullable<int> blocksize = R_NilValue,
                          Rcpp::Nullable<bool> bycols = true, 
                          Rcpp::Nullable<bool> overwrite = false  )
{
    
    BigDataStatMeth::hdf5Dataset* dsIn = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try
    {
        
        H5::Exception::dontPrint();
        
        std::string stroutgroup, stroutdataset, stroutdata;
        std::string strdataset = group + "/" + dataset;
        std::string strdatasetout;
        int iblocksize = 0; //, iwholesize = 0;
        bool bcols; //, bforce;
        
        if(bycols.isNull()) { bcols = true ;
        } else {   bcols = Rcpp::as<bool>(bycols);}
        
        if(outgroup.isNull()) {  stroutgroup = group ;
        } else {   stroutgroup = Rcpp::as<std::string>(outgroup);}
        
        if(outdataset.isNull()){  stroutdataset = dataset ;
        } else {   stroutdataset = Rcpp::as<std::string>(outdataset);}
        
        dsIn = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
        dsIn->openDataset();
        
        if( dsIn->getDatasetptr() != nullptr ) { 
            
            hsize_t nrows = dsIn->nrows(),
            ncols = dsIn->ncols();
            
            if( nblocks.isNull() && blocksize.isNull()){
                checkClose_file(dsIn);
                Rf_error( "c++ exception bdSplit_matrix_hdf5: Block size or number of blocks are needed to proceed with matrix split. Please, review parameters");
                return(lst_return);
                
            } else if (!nblocks.isNull() && !blocksize.isNull()) {
                checkClose_file(dsIn);
                Rf_error( "c++ exception bdSplit_matrix_hdf5: Block size and number of blocks are defined, please define only one option, split by number of blocks or by block size");
                return(lst_return);
                
            } else if(!nblocks.isNull()) {
                
                if ( Rcpp::as<int>(nblocks) == 1) {
                    checkClose_file(dsIn);
                    Rf_error( "c++ exception bdSplit_matrix_hdf5: No data to split: Numbers of blocks = 1, no data to split");
                    return(lst_return);
                    
                } else {
                    
                    double module;
                    
                    if(bcols == true) {
                        iblocksize = nrows / Rcpp::as<int>(nblocks);
                        module = nrows % iblocksize;
                    } else {
                        iblocksize = ncols / Rcpp::as<int>(nblocks);
                        module = ncols % iblocksize;
                    }
                    if (module > 0) { iblocksize = iblocksize + 1; }
                }
                
            } else {
                iblocksize = Rcpp::as<int>(blocksize);
                
                if( bcols == true ) {
                    if( iblocksize == nrows) {  
                        checkClose_file(dsIn);
                        Rf_error( "c++ exception bdSplit_matrix_hdf5: No data to split");
                        return(lst_return);
                    }
                } else {
                    if( iblocksize == ncols) {  
                        checkClose_file(dsIn);
                        Rf_error( "c++ exception bdSplit_matrix_hdf5: No data to split");
                        return(lst_return);
                    }
                }
            }
            
            if( dsIn->getDatasetptr() != nullptr) {
                RcppSplit_matrix_hdf5 ( dsIn, bcols, stroutgroup, stroutdataset, iblocksize, ncols, nrows );    
            } else {
                checkClose_file(dsIn);
                Rf_error( "c++ exception bdSplit_matrix_hdf5: File %s does not exist", filename.c_str());
                return(lst_return);
            }
        }
    
        delete dsIn; dsIn = nullptr;
        
        lst_return["fn"] = filename;
        lst_return["ds"] = stroutgroup + "/" + stroutdataset;
        
        Rcpp::Rcout<<"Dataset has been splitted, results can be found in "<< stroutgroup + "/" + stroutdataset <<"\n";
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(dsIn);
        Rf_error( "c++ exception bdSplit_matrix_hdf5 (File IException)");
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dsIn);
        Rf_error( "c++ exception bdSplit_matrix_hdf5 (DataSet IException)");
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dsIn);
        Rf_error( "c++ exception bdSplit_matrix_hdf5 (DataSpace IException)");
    } catch( H5::DataTypeIException& error ) { 
        checkClose_file(dsIn);
        Rf_error( "c++ exception bdSplit_matrix_hdf5 (DataType IException)");
    } catch(std::exception &ex) {
        checkClose_file(dsIn);
        Rf_error( "c++ exception bdSplit_matrix_hdf5 : %s", ex.what());
    } catch (...) {
        checkClose_file(dsIn);
        Rf_error( "c++ exception bdSplit_matrix_hdf5 (unknown reason)");
    }
    
    return(lst_return);
}

