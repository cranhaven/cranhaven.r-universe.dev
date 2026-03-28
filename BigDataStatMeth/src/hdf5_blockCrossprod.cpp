/**
 * @file hdf5_blockCrossprod.cpp
 * @brief Block-wise cross product operations for HDF5 matrices
 * 
 * This file implements efficient block-wise cross product operations for large
 * matrices stored in HDF5 format. It supports both single-matrix operations
 * (A^t * A) and two-matrix operations (A^t * B) with optimizations for memory
 * usage and parallel processing.
 * 
 * Key features:
 * - Block-wise matrix multiplication
 * - Parallel processing support
 * - Memory-efficient operations
 * - Flexible output options
 * - Automatic block size optimization
 * 
 * The implementation focuses on:
 * - Minimizing memory usage for large matrices
 * - Optimizing performance through block operations
 * - Supporting parallel computation
 * - Providing comprehensive error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/crossprod.hpp"
// #include "Utilities/Utilities.hpp"

/**
 * @brief Compute cross product of HDF5 matrices
 *
 * @details Performs optimized cross product operations on matrices stored in HDF5 format.
 * For a single matrix A, computes A^t * A. For two matrices A and B, computes
 * A^t * B. Uses block-wise processing for memory efficiency.
 *
 * Block-wise processing features:
 * - Automatic block size optimization
 * - Memory-efficient operations
 * - Parallel computation support
 * - Cache-friendly algorithms
 *
 * @param filename [in] HDF5 file path
 * @param group [in] Input group containing matrix A
 * @param A [in] Dataset name for matrix A
 * @param B [in] Optional dataset name for matrix B
 * @param groupB [in] Optional group containing matrix B
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
 * @see crossprod()
 */

//' Crossprod with hdf5 matrix
//' 
//' Performs optimized cross product operations on matrices stored in HDF5 format.
//' For a single matrix A, computes A^t * A. For two matrices A and B, computes
//' A^t * B. Uses block-wise processing for memory efficiency.
//' 
//' @param filename String indicating the HDF5 file path
//' @param group String indicating the input group containing matrix A
//' @param A String specifying the dataset name for matrix A
//' @param B Optional string specifying dataset name for matrix B.
//'        If NULL, performs A^t * A
//' @param groupB Optional string indicating group containing matrix B.
//'        If NULL, uses same group as A
//' @param block_size Optional integer specifying the block size for processing.
//'        Default is automatically determined based on matrix dimensions
//' @param mixblock_size Optional integer for memory block size in parallel processing
//' @param paral Optional boolean indicating whether to use parallel processing.
//'        Default is false
//' @param threads Optional integer specifying number of threads for parallel processing.
//'        If NULL, uses maximum available threads
//' @param outgroup Optional string specifying output group.
//'        Default is "OUTPUT"
//' @param outdataset Optional string specifying output dataset name.
//'        Default is "CrossProd_A_x_B"
//' @param overwrite Optional boolean indicating whether to overwrite existing datasets.
//'        Default is false
//' 
//' @return A list containing the location of the crossproduct result:
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the result}
//'     \item{ds}{Character string. Full dataset path to the crossproduct 
//'     result (t(A) %*% A or t(A) %*% B) within the HDF5 file}
//'   }
//' 
//' @details
//' The function implements block-wise matrix multiplication to handle large matrices
//' efficiently. Block size is automatically optimized based on:
//' - Available memory
//' - Matrix dimensions
//' - Whether parallel processing is enabled
//' 
//' For parallel processing:
//' - Uses OpenMP for thread management
//' - Implements cache-friendly block operations
//' - Provides automatic thread count optimization
//' 
//' Memory efficiency is achieved through:
//' - Block-wise reading and writing
//' - Minimal temporary storage
//' - Proper resource cleanup
//' 
//' @examples
//' \dontrun{
//'   library(BigDataStatMeth)
//'   library(rhdf5)
//'   
//'   # Create test matrix
//'   N = 1000
//'   M = 1000
//'   set.seed(555)
//'   a <- matrix(rnorm(N*M), N, M)
//'   
//'   # Save to HDF5
//'   bdCreate_hdf5_matrix("test.hdf5", a, "INPUT", "A", overwriteFile = TRUE)
//'   
//'   # Compute cross product
//'   bdCrossprod_hdf5("test.hdf5", "INPUT", "A", 
//'                    outgroup = "OUTPUT",
//'                    outdataset = "result",
//'                    block_size = 1024,
//'                    paral = TRUE,
//'                    threads = 4)
//' }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdCrossprod_hdf5( std::string filename, 
                             std::string group, 
                             std::string A, 
                             Rcpp::Nullable<std::string> B = R_NilValue, 
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
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try {
        
        H5::Exception::dontPrint();  

        int iblock_size;
        int iblockfactor = 2;
        bool bparal, bforce, bisSymetric = false;

        std::string strsubgroupOut, 
        strdatasetOut, 
        strsubgroupIn,
        strsubgroupInB;
        std::string matB;
        
        strsubgroupIn = group;
        
        if( outgroup.isNull()) { strsubgroupOut = "OUTPUT";
        } else { strsubgroupOut = Rcpp::as<std::string> (outgroup); }
        
        if(B.isNotNull()){ matB =  Rcpp::as<std::string> (B) ; } 
        else { 
            matB =  A; 
            bisSymetric = true;
        }
        
        if(groupB.isNotNull()){ strsubgroupInB =  Rcpp::as<std::string> (groupB) ; } 
        else { strsubgroupInB =  group; }
        
        if (paral.isNull()) { bparal = false; } 
        else { bparal = Rcpp::as<bool> (paral); }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool> (overwrite); }
        
        if( outdataset.isNotNull()) { strdatasetOut =  Rcpp::as<std::string> (outdataset); } 
        else { strdatasetOut = "CrossProd_" + A + "_x_" + matB; }
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupIn, A, false);
        dsA->openDataset();
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, matB, false);
        dsB->openDataset();
        
        if( dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
            
            dsC = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
            
            iblock_size = BigDataStatMeth::getMaxBlockSize( dsA->nrows(), dsA->ncols(), dsB->nrows(), dsB->ncols(), iblockfactor, block_size);
            
            if(bparal == true) { // parallel
                
                int memory_block; 
                if(mixblock_size.isNotNull()) {
                    memory_block = Rcpp::as<int> (mixblock_size);
                } else {
                    memory_block = iblock_size/2;
                }
                
                dsC = BigDataStatMeth::crossprod(dsA, dsB, dsC, bisSymetric, iblock_size, memory_block, bparal, true, threads);
                
            } else if (bparal == false) { // Not parallel
                
                dsC = BigDataStatMeth::crossprod(dsA, dsB, dsC, bisSymetric, iblock_size, 0, bparal, true, threads);
            }
            
            lst_return["fn"] = filename;
            lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
            
            delete dsC; dsC = nullptr;
        }
        
        delete dsA; dsA = nullptr;
        delete dsB; dsB = nullptr;
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ c++ exception bdCrossprod_hdf5 (File IException)";
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"c++ exception bdCrossprod_hdf5 (DataSet IException)";
    } catch(std::exception &ex) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr << "c++ exception blockmult_hdf5: " << ex.what();
    } catch (...) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"C++ exception bdCrossprod_hdf5 (unknown reason)";
    }
    
    // return List::create(Named("filename") = filename,
    //                     Named("dataset") = strsubgroupOut + "/" + strdatasetOut);
    // return void();
    return(lst_return);
    
}

