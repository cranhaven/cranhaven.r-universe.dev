/**
 * @file hdf5_blocksum.cpp
 * @brief Block-wise matrix addition for HDF5 datasets
 * 
 * This file implements efficient block-wise matrix addition operations for
 * large datasets stored in HDF5 format. It supports both matrix-matrix and
 * matrix-vector addition with optimizations for memory usage and parallel
 * processing.
 * 
 * Key features:
 * - Matrix-matrix addition
 * - Matrix-vector addition
 * - Block-wise processing
 * - Parallel computation support
 * - Automatic block size optimization
 * 
 * The implementation focuses on:
 * - Memory-efficient operations
 * - Optimized block processing
 * - Flexible data handling
 * - Resource management
 * - Error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixSum.hpp"
// #include "Utilities/Utilities.hpp"

/**
 * @brief Block-wise matrix addition for HDF5 datasets
 *
 * @details Performs optimized block-wise addition between two datasets stored
 * in HDF5 format. Supports both matrix-matrix and matrix-vector operations with
 * memory-efficient block processing.
 *
 * Operation modes:
 * - Matrix-matrix addition (A + B)
 * - Matrix-vector addition
 * - Vector-matrix addition
 *
 * @param filename [in] HDF5 file path
 * @param group [in] Group containing matrix A
 * @param A [in] Dataset name for matrix A
 * @param B [in] Dataset name for matrix B
 * @param groupB [in] Optional group containing matrix B
 * @param block_size [in] Block size for processing
 * @param paral [in] Whether to use parallel processing
 * @param threads [in] Number of threads for parallel processing
 * @param outgroup [in] Output group name
 * @param outdataset [in] Output dataset name
 * @param overwrite [in] Whether to overwrite existing datasets
 *
 * @return void
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::GroupIException if group operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception for other errors
 *
 * @note Performance depends on chosen block size and parallel processing options
 * @see Rcpp_block_matrix_sum_hdf5(), Rcpp_block_matrix_vector_sum_hdf5()
 */

//' HDF5 dataset addition
//'
//' Performs optimized block-wise addition between two datasets stored in HDF5
//' format. Supports both matrix-matrix and matrix-vector operations with
//' memory-efficient block processing.
//' 
//' @param filename String indicating the HDF5 file path
//' @param group String indicating the group containing matrix A
//' @param A String specifying the dataset name for matrix A
//' @param B String specifying the dataset name for matrix B
//' @param groupB Optional string indicating group containing matrix B.
//'        If NULL, uses same group as A
//' @param block_size Optional integer specifying block size for processing.
//'        If NULL, automatically determined based on matrix dimensions
//' @param paral Optional boolean indicating whether to use parallel processing.
//'        Default is false
//' @param threads Optional integer specifying number of threads for parallel processing.
//'        If NULL, uses maximum available threads
//' @param outgroup Optional string specifying output group.
//'        Default is "OUTPUT"
//' @param outdataset Optional string specifying output dataset name.
//'        Default is "A_+_B"
//' @param overwrite Optional boolean indicating whether to overwrite existing datasets.
//'        Default is false
//' 
//' @return A list containing the location of the addition result:
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the result}
//'     \item{ds}{Character string. Full dataset path to the addition result (A + B) within the HDF5 file}
//'   }
//' 
//' @details
//' The function implements optimized addition through:
//' 
//' Operation modes:
//' - Matrix-matrix addition (A + B)
//' - Matrix-vector addition
//' - Vector-matrix addition
//' 
//' Block processing:
//' - Automatic block size selection
//' - Memory-efficient operations
//' - Parallel computation support
//' 
//' Block size optimization based on:
//' - Matrix dimensions
//' - Available memory
//' - Operation type (matrix/vector)
//' 
//' Error handling:
//' - Dimension validation
//' - Resource management
//' - Exception handling
//' 
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' N <- 1500
//' M <- 1500
//' set.seed(555)
//' a <- matrix(rnorm(N*M), N, M)
//' b <- matrix(rnorm(N*M), N, M)
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", a, "data", "A",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix("test.hdf5", b, "data", "B",
//'                      overwriteFile = FALSE)
//' 
//' # Perform addition
//' bdblockSum_hdf5("test.hdf5", "data", "A", "B",
//'                 outgroup = "results",
//'                 outdataset = "sum",
//'                 block_size = 1024,
//'                 paral = TRUE)
//' }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdblockSum_hdf5(std::string filename, 
                   std::string group, 
                   std::string A, 
                   std::string B,
                   Rcpp::Nullable<std::string> groupB = R_NilValue, 
                   Rcpp::Nullable<int> block_size = R_NilValue, 
                   Rcpp::Nullable<bool> paral = R_NilValue,
                   Rcpp::Nullable<int> threads = R_NilValue,
                   Rcpp::Nullable<std::string> outgroup = R_NilValue,
                   Rcpp::Nullable<std::string> outdataset = R_NilValue,
                   Rcpp::Nullable<bool> overwrite = R_NilValue)
{
    
    
    
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsC = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try{
        
        H5::Exception::dontPrint();  

        int iblock_size,
            bparal, 
            bforce;
    
        std::string strsubgroupOut, 
                    strdatasetOut, 
                    strsubgroupIn,
                    strsubgroupInB,
                    strGroupB;
        
        if( outgroup.isNull()) { strsubgroupOut = "OUTPUT"; } 
        else { strsubgroupOut = Rcpp::as<std::string> (outgroup); }
        
        strsubgroupIn = group + "/";
        
        if(groupB.isNotNull()){
            strsubgroupInB =  Rcpp::as<std::string> (groupB) + "/";
            strGroupB = Rcpp::as<std::string> (groupB);
        } else {
            strsubgroupInB =  group + "/";
            strGroupB = group;
        }
        
        if (paral.isNull()) { bparal = false; } 
        else { bparal = Rcpp::as<bool> (paral); }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool> (overwrite); }
        
        if( outdataset.isNotNull()) { strdatasetOut =  Rcpp::as<std::string> (outdataset); } 
        else { strdatasetOut =  A + "_+_" + B; }
        
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupIn, A, false);
        dsA->openDataset();
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, false);
        dsB->openDataset();
        dsC = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
        
        if( dsA->getDatasetptr() != nullptr &&  dsB->getDatasetptr() != nullptr  ) 
        { 
            int irowsA = dsA->nrows(),
                icolsA = dsA->ncols(),
                irowsB = dsB->nrows(),
                icolsB = dsB->ncols();
            
            if (block_size.isNotNull()) {
                iblock_size = Rcpp::as<int> (block_size);
            } else {
                
                if( irowsA == 1 || icolsA == 1 || irowsB == 1 || icolsB == 1){
                    iblock_size = BigDataStatMeth::getVectorBlockSize( irowsA*icolsA);
                } else{
                    std::vector<hsize_t> blockSize = BigDataStatMeth::getMatrixBlockSize( irowsA, icolsA);
                    if(irowsA < icolsA) {
                        iblock_size = blockSize.at(0);    
                    } else {
                        iblock_size = blockSize.at(1);
                    }
                }
            }
            
            if( irowsA != 1 && icolsA!= 1 && irowsB != 1 && icolsB!= 1) {
                Rcpp_block_matrix_sum_hdf5(dsA, dsB, dsC, iblock_size, bparal, threads);
            } else {
                
                if( irowsA==1 || icolsA==1 ) {
                    Rcpp_block_matrix_vector_sum_hdf5(dsA, dsB, dsC, iblock_size, bparal, threads);
                } else {
                    Rcpp_block_matrix_vector_sum_hdf5(dsB, dsA, dsC, iblock_size, bparal, threads);
                }
            }
        }
        
        lst_return["fn"] = filename;
        lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
        
        delete dsA; dsA = nullptr;
        delete dsB; dsB = nullptr;
        delete dsC; dsC = nullptr;
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockSum_hdf5 (File IException)";
    } catch( H5::GroupIException & error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockSum_hdf5 (Group IException)";
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockSum_hdf5 (DataSet IException)";
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockSum_hdf5: " << ex.what();
    } catch (...) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"C++ exception bdblockSum_hdf5 (unknown reason)";
    }
    
    // //..// return(C);
    // return List::create(Named("filename") = filename,
    //                     Named("dataset") = strsubgroupOut + "/" + strdatasetOut,
    //                     Named("result") = wrap(0));
    
    return(lst_return);
}
