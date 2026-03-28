/**
 * @file hdf5_blockmultSparse.cpp
 * @brief Block-wise sparse matrix multiplication for HDF5 datasets
 * 
 * This file implements efficient block-wise matrix multiplication operations
 * specifically optimized for sparse matrices stored in HDF5 format. It provides
 * memory-efficient processing of large sparse matrices through block operations
 * and parallel computation support.
 * 
 * Key features:
 * - Sparse matrix multiplication
 * - Block-wise processing
 * - Parallel computation support
 * - Memory optimization
 * - Automatic block size selection
 * 
 * The implementation focuses on:
 * - Efficient sparse matrix operations
 * - Memory-efficient block processing
 * - Parallel computation optimization
 * - Resource management
 * - Error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/multiplicationSparse.hpp"

/**
 * @brief Block matrix multiplication for sparse matrices
 *
 * @details Performs optimized block-wise matrix multiplication for sparse matrices
 * stored in HDF5 format. The implementation is specifically designed to handle
 * large sparse matrices efficiently through block operations and parallel processing.
 *
 * Optimization features:
 * - Sparse matrix storage format
 * - Block-wise processing
 * - Parallel computation
 * - Memory-efficient operations
 *
 * @param filename [in] HDF5 file path
 * @param group [in] Group path for matrix A
 * @param A [in] Dataset name for matrix A
 * @param B [in] Dataset name for matrix B
 * @param groupB [in] Optional group path for matrix B
 * @param block_size [in] Block size for processing
 * @param mixblock_size [in] Memory block size for parallel processing
 * @param paral [in] Whether to use parallel processing
 * @param threads [in] Number of threads for parallel processing
 * @param outgroup [in] Output group name
 * @param outdataset [in] Output dataset name
 * @param overwrite [in] Whether to overwrite existing datasets
 *
 * @return void
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception for other errors
 *
 * @note Performance significantly improves with appropriate block sizes and parallel processing
 * @see multiplicationSparse()
 */

//' Block matrix multiplication for sparse matrices
//' 
//' Performs optimized block-wise matrix multiplication for sparse matrices stored
//' in HDF5 format. The implementation is specifically designed to handle large
//' sparse matrices efficiently through block operations and parallel processing.
//' 
//' @param filename String indicating the HDF5 file path
//' @param group String indicating the group path for matrix A
//' @param A String specifying the dataset name for matrix A
//' @param B String specifying the dataset name for matrix B
//' @param groupB Optional string indicating group path for matrix B.
//'        If NULL, uses same group as A
//' @param block_size Optional integer specifying block size for processing.
//'        If NULL, automatically determined based on matrix dimensions
//' @param mixblock_size Optional integer for memory block size in parallel processing
//' @param paral Optional boolean indicating whether to use parallel processing.
//'        Default is false
//' @param threads Optional integer specifying number of threads for parallel processing.
//'        If NULL, uses maximum available threads
//' @param outgroup Optional string specifying output group.
//'        Default is "OUTPUT"
//' @param outdataset Optional string specifying output dataset name.
//'        Default is "A_x_B"
//' @param overwrite Optional boolean indicating whether to overwrite existing datasets.
//'        Default is false
//' 
//' @return Modifies the HDF5 file in place, adding the multiplication result
//' 
//' @details
//' The function implements optimized sparse matrix multiplication through:
//' - Block-wise processing to manage memory usage
//' - Automatic block size optimization
//' - Parallel processing support
//' - Efficient sparse matrix storage
//' 
//' Block size optimization considers:
//' - Available system memory
//' - Matrix dimensions and sparsity
//' - Parallel processing requirements
//' 
//' Memory efficiency is achieved through:
//' - Sparse matrix storage format
//' - Block-wise processing
//' - Minimal temporary storage
//' - Proper resource cleanup
//' 
//' @examples
//' \dontrun{
//' library(Matrix)
//' library(BigDataStatMeth)
//' 
//' # Create sparse test matrices
//' k <- 1e3
//' set.seed(1)
//' x_sparse <- sparseMatrix(
//'     i = sample(x = k, size = k),
//'     j = sample(x = k, size = k),
//'     x = rnorm(n = k)
//' )
//' 
//' set.seed(2)
//' y_sparse <- sparseMatrix(
//'     i = sample(x = k, size = k),
//'     j = sample(x = k, size = k),
//'     x = rnorm(n = k)
//' )
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", as.matrix(x_sparse), "SPARSE", "x_sparse")
//' bdCreate_hdf5_matrix("test.hdf5", as.matrix(y_sparse), "SPARSE", "y_sparse")
//' 
//' # Perform multiplication
//' bdblockmult_sparse_hdf5("test.hdf5", "SPARSE", "x_sparse", "y_sparse",
//'                         block_size = 1024,
//'                         paral = TRUE,
//'                         threads = 4)
//' }
//' 
//' @export
// [[Rcpp::export]]
void bdblockmult_sparse_hdf5( std::string filename, std::string group, 
                          std::string A, std::string B,
                          Rcpp::Nullable<std::string> groupB = R_NilValue, 
                          Rcpp::Nullable<int> block_size = R_NilValue,
                          Rcpp::Nullable<int> mixblock_size = R_NilValue,
                          Rcpp::Nullable<bool> paral = R_NilValue,
                          Rcpp::Nullable<int> threads = R_NilValue,
                          Rcpp::Nullable<std::string> outgroup = R_NilValue,
                          Rcpp::Nullable<std::string> outdataset = R_NilValue,
                          Rcpp::Nullable<bool> overwrite = R_NilValue )
{
     
    
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsC = nullptr;
    
    

    try
    {

     
        H5::Exception::dontPrint();  
        std::string strdataset;
        std::string spMatrix("dgCMatrix");
        
        int iblock_size;
        bool bparal, bforce;
        
        std::string strsubgroupOut,
                    strdatasetOut, 
                    strsubgroupIn,
                    strsubgroupInB;
        
        int iblockfactor = 2;
        
        strsubgroupIn = group;
        
        if( outgroup.isNull()) { strsubgroupOut = "OUTPUT";
        } else { strsubgroupOut = Rcpp::as<std::string> (outgroup); }
        
        if(groupB.isNotNull()){ strsubgroupInB =  Rcpp::as<std::string> (groupB) ; } 
        else { strsubgroupInB =  group; }
        
        if( outdataset.isNotNull()) { strdatasetOut =  Rcpp::as<std::string> (outdataset); } 
        else { strdatasetOut =  A + "_x_" + B; }
        
        if (paral.isNull()) { bparal = false; } 
        else { bparal = Rcpp::as<bool> (paral); }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool> (overwrite); }
        
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupIn, A, false);
        dsA->openDataset();
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, false);
        dsB->openDataset();
        dsC = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
        
        if( dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
            iblock_size = BigDataStatMeth::getMaxBlockSize( dsA->nrows(), dsA->ncols(), dsB->nrows(), dsB->ncols(), iblockfactor, block_size);
        } else {
            checkClose_file(dsA, dsB, dsC);
            Rcpp::Rcerr<<"c++ exception bdblockmult_sparse_hdf5 : error with "<<A<< " or "<<B<< " datset\n";
            return void();
        }
     
     
     // if (block_size.isNotNull()) {
     //     iblock_size = Rcpp::as<int> (block_size);
     // } else {
     //     iblock_size = std::min(  std::min(dsA->nrows(),dsA->ncols()),  std::min(dsB->nrows(), dsB->ncols()));
     //     if (iblock_size>1024)
     //         iblock_size = 1024;
     // }
     
         if(bparal == true) { // parallel
             
             int memory_block; 
             if(mixblock_size.isNotNull()) {
                 memory_block = Rcpp::as<int> (mixblock_size);
             } else {
                 memory_block = iblock_size/2;
             }
             
             dsC = BigDataStatMeth::multiplicationSparse(dsA, dsB, dsC, iblock_size, memory_block, bparal, true, threads);
             
         } else if (bparal == false) { // Not parallel
             dsC = BigDataStatMeth::multiplicationSparse(dsA, dsB, dsC, iblock_size, 0, bparal, true, threads);
         }
     
         delete dsA;   dsA = nullptr;
         delete dsB;   dsB = nullptr;
         delete dsC;   dsC = nullptr;
     
     
    } catch( H5::FileIException& error ) { 
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockmult_sparse_hdf5 (File IException)";
        return void();
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockmult_sparse_hdf5 (DataSet IException)";
        return void();
    } catch(std::exception &ex) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdblockmult_sparse_hdf5";
        return void();
    } catch (...) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"C++ exception bdblockmult_sparse_hdf5 (unknown reason)";
        return void();
    }
    
    return void();
     
     
}



