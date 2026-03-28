/**
 * @file BigDataStatMeth.hpp
 * @brief Main header file for the BigDataStatMeth package
 * @details This is the main header file for the BigDataStatMeth package, which provides
 * statistical methods and utilities for handling large-scale data using HDF5 and in-memory
 * operations. The package includes:
 * 
 * Key components:
 * - HDF5-based matrix operations
 * - Memory-efficient algorithms
 * - Parallel processing capabilities
 * - Statistical computations
 * - Omics data utilities
 * 
 * The package is organized into several modules:
 * - hdf5Algebra: Matrix operations on HDF5 files
 * - hdf5Utilities: HDF5 file handling utilities
 * - hdf5Omics: Specialized functions for omics data
 * - memAlgebra: In-memory matrix operations
 * - Utilities: General utility functions
 * 
 * @author Dolors Pelegrí
 * @version 0.63.0
 * @date 2025
 */


#ifndef BIGDATASTATMETH_HPP
#define BIGDATASTATMETH_HPP
// #ifndef BigDataStatMeth_BigDataStatMeth_H
// #define BigDataStatMeth_BigDataStatMeth_H



    // =============================================================================
    // SYSTEM INCLUDES 
    // =============================================================================


    #include <RcppEigen.h>
    
    // Common headers
    #include "H5Cpp.h" //1.8.18
    #include <iostream>
    #include <sstream>
    #include <boost/algorithm/string.hpp>
    #include <fstream>
    #include <sys/stat.h>
    #include <string>
    // #include <regex>
    // #include <algorithm>
    #include <vector>
    // #include <cmath>
    // #include <numeric>
    // #include <random>


    
    // Common headers - multi-threading
    #include <thread>
    #include "Utilities/pkg_omp.h" // first for clang-13-omp, #5122
    
    #include <R.h>
    #include <Rversion.h>
    #if !defined(R_VERSION) || R_VERSION < R_Version(3, 5, 0)  // R-exts$6.14
        #  define ALTREP(x) 0     // #2866
        #  define USE_RINTERNALS  // #3301
        #  define DATAPTR_RO(x) ((const void *)DATAPTR(x))
    #endif
    #include <Rinternals.h>
    #define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))  // to avoid overhead of looped STRING_ELT and VECTOR_ELT
        #include <stdint.h>    // for uint64_t rather than unsigned long long
        #include <stdbool.h>
    #ifdef WIN32  // positional specifiers (%n$) used in translations; #4402
        #  define snprintf dt_win_snprintf  // see our snprintf.c; tried and failed to link to _sprintf_p on Windows
    #endif
    #ifdef sprintf
        #undef sprintf
    #endif


    // #include <signal.h> // the debugging machinery + breakpoint aidee
    // raise(SIGINT);
    #define IS_TRUE_OR_FALSE(x) (TYPEOF(x)==LGLSXP && LENGTH(x)==1 && LOGICAL(x)[0]!=NA_LOGICAL)
    
    // for use with bit64::integer64
    #define NA_INTEGER64  INT64_MIN
    #define MAX_INTEGER64 INT64_MAX

    
    // 
    // /**
    //  * @brief Maximum length for names in HDF5 objects
    //  */
    // #define MAX_NAME 1024
    // // #define MAXSVDBLOCK 1500 //. ORIGINAL - CORRECTE.//
    // 
    // const int RANK1 = 1;
    // const int RANK2 = 2;
    // const int RANK3 = 3;
    // const int DIM1 = 1;
    // const int DIM2 = 2;
    // const int DIM3 = 3;
    // 
    // const int MAXSTRING = 20;
    // const hsize_t MAXSTRBLOCK = 1 << 5;
    // 
    // /**
    //  * @brief Maximum number of elements in a block for HDF5 operations
    //  * @details This constant defines the maximum number of elements that can be processed
    //  * in a single block during HDF5 operations. The value is set to (2^29 - 1) to ensure
    //  * efficient memory usage while maintaining performance.
    //  */
    // const hsize_t MAXELEMSINBLOCK = ((2 << 29) - 1);
    // // const hsize_t MAXELEMSINBLOCK = ((2 << 5) - 1);
    // /**
    //  * @brief Maximum block size for matrix operations
    //  * @details Calculated as the square root of MAXELEMSINBLOCK to optimize matrix
    //  * block operations.
    //  */
    // const hsize_t MAXBLOCKSIZE = std::floor(std::sqrt(MAXELEMSINBLOCK));
    // /**
    //  * @brief Maximum block size for multiplication operations
    //  */
    // const hsize_t MAXMULTBLOCKSIZE = 1 << 13;
    // 
    // // const int maxElemBlock = 3000000;
    // //.Only test.// const int maxElemBlock = 30;
    // /**
    //  * @brief Execution status codes
    //  * @{
    //  */
    // const int EXEC_OK = 0;      /**< Successful execution */
    // const int EXEC_ERROR = 1;   /**< Execution error */
    // const int EXEC_WARNING = 2; /**< Execution warning */
    // /** @} */

    // openme-utils.cpp functions
    void initDTthreads();
    int getDTthreads(const int64_t n, const bool throttle);
    static const char *mygetenv(const char *name, const char *unset);
    SEXP getDTthreads_R(SEXP verbose);


// =============================================================================
// CONSTANTS DEFINITION
// =============================================================================
    
    /**
     * @brief Maximum length for names in HDF5 objects
     */
    #define MAX_NAME 1024
    
    const int RANK1 = 1;
    const int RANK2 = 2;
    const int RANK3 = 3;
    const int DIM1 = 1;
    const int DIM2 = 2;
    const int DIM3 = 3;
    
    const int MAXSTRING = 20;
    const hsize_t MAXSTRBLOCK = 1 << 5;
    
    /**
     * @brief Maximum number of elements in a block for HDF5 operations
     * @details This constant defines the maximum number of elements that can be processed
     * in a single block during HDF5 operations. The value is set to (2^29 - 1) to ensure
     * efficient memory usage while maintaining performance.
     */
    const hsize_t MAXELEMSINBLOCK = ((2 << 29) - 1);
    
    /**
     * @brief Maximum block size for matrix operations
     * @details Calculated as the square root of MAXELEMSINBLOCK to optimize matrix
     * block operations.
     */
    const hsize_t MAXBLOCKSIZE = std::floor(std::sqrt(MAXELEMSINBLOCK));
    
    /**
     * @brief Maximum block size for multiplication operations
     */
    const hsize_t MAXMULTBLOCKSIZE = 1 << 13;
    
    /**
     * @brief Block size for efficient diagonal element processing in HDF5
     * @details Processes diagonal elements in chunks of 256x256 to minimize I/O operations
     * while maintaining reasonable memory usage (~500KB per block)
     */
    const hsize_t DIAG_BLOCK_SIZE = 256;
    
    /**
     * @brief Maximum block size for Cholesky  operations
     */
    const hsize_t MAXCHOLBLOCKSIZE = 250000000;
    // const hsize_t MAXCHOLBLOCKSIZE = 50000000;
    
    
    /**
     * @brief Threshold for switching to out-of-core algorithm
     */
    const int CHOLESKY_OUTOFCORE_THRESHOLD = 250;
    // const int CHOLESKY_OUTOFCORE_THRESHOLD = 250000;
    
    /**
     * @brief Execution status codes
     * @{
     */
    const int EXEC_OK = 0;      /**< Successful execution */
    const int EXEC_ERROR = 1;   /**< Execution error */
    const int EXEC_WARNING = 2; /**< Execution warning */
    /** @} */
    
    
// =============================================================================
// LEVEL 0 - MAIN UTILITIES (HDF5 AND ON-MEMORY)
// =============================================================================
    #include "Utilities/openme-utils.hpp"
    #include "Utilities/Utilities.hpp"
    #include "hdf5Utilities/hdf5Utilities.hpp"
    
    // #include "hdf5Omics/hdf5OmicsUtils.hpp"
    

    
// =============================================================================
// LEVEL 0 - CORE UTILITIES - Classes and related to classes
// =============================================================================
    #include "hdf5Utilities/hdf5Files.hpp"
    #include "hdf5Utilities/hdf5Groups.hpp"
    #include "hdf5Utilities/hdf5Datasets.hpp"
    #include "hdf5Utilities/hdf5DatasetsInternal.hpp"
    #include "hdf5Utilities/hdf5CheckClose.hpp"
    #include "hdf5Utilities/hdf5Dims.hpp"
    
    // #include "hdf5Utilities/hdf5DiagonalOperations.hpp"

    // #include "hdf5Utilities/hdf5Diagonal.hpp"
    
    
// =============================================================================
// LEVEL 0 - ON-MEMORY ALBEGRA AND OTHER FUNCTIONS
// =============================================================================
    #include "memAlgebra/memOtherFunctions.hpp"
    #include "memAlgebra/memOptimizedProducts.hpp"
    #include "memAlgebra/memMultiplication.hpp"
    #include "memAlgebra/memSubstract.hpp"
    #include "memAlgebra/memSum.hpp"
    
// =============================================================================
// LEVEL 0 -  HDF5  UTILITIES
// =============================================================================
    #include "hdf5Utilities/hdf5Methods.hpp"
    #include "hdf5Utilities/hdf5ImportFiles.hpp"
    #include "hdf5Utilities/hdf5RemoveElements.hpp"
    
// =============================================================================
// LEVEL 0 -  HDF5 ALBEGRA
// =============================================================================
    #include "hdf5Algebra/matrixTriangular.hpp"
    #include "hdf5Algebra/matrixDiagonal.hpp"

    #include "hdf5Algebra/vectormatrix.hpp"
    #include "hdf5Algebra/vectorOperations.hpp"
    #include "hdf5Algebra/matrixSdMean.hpp"
    #include "hdf5Algebra/matrixNormalization.hpp"

    
    #include "hdf5Algebra/multiplication.hpp"
    
    #include "hdf5Algebra/matrixEquationSolver.hpp"
    
    
    #include "hdf5Algebra/matrixSvdBlock.hpp"
    #include "hdf5Algebra/matrixSvd.hpp"
    #include "hdf5Algebra/matrixPCA.hpp"
    #include "hdf5Algebra/matrixEigenDecomposition.hpp"
    
    
    #include "hdf5Algebra/multiplicationSparse.hpp"
    #include "hdf5Algebra/matrixQR.hpp"
    #include "hdf5Algebra/tcrossprod.hpp"
    #include "hdf5Algebra/matrixInvCholesky.hpp"
    #include "hdf5Algebra/matrixSum.hpp"
    
    
    #include "hdf5Algebra/matrixSubstract.hpp"
    #include "hdf5Algebra/matrixPseudoinverse.hpp"
    
    #include "hdf5Algebra/crossprod.hpp"
    #include "hdf5Algebra/matrixCorrelation.hpp"
    
// =============================================================================
// LEVEL 0 - HDF5 DATASET UTILITIES
// =============================================================================
    #include "hdf5Utilities/hdf5SplitDataset.hpp"
    #include "hdf5Utilities/hdf5BindDatasets.hpp"
    #include "hdf5Utilities/hdf5ReduceDataset.hpp"
    #include "hdf5Utilities/hdf5SortDataset.hpp"
    #include "hdf5Utilities/hdf5ApplytoDatasets.hpp"
    // #include "hdf5Utilities/hdf5MoveDatasets.hpp"
        
// =============================================================================
// LEVEL 1 - CORE UTILITIES - Classes and related to classes
// =============================================================================
    #include "hdf5Utilities/hdf5DiagonalMethods.hpp"
    #include "hdf5Utilities/hdf5Diagonal.hpp"
    

// =============================================================================
// LEVEL 1 -  HDF5 ALBEGRA
// =============================================================================
    


// =============================================================================
// HDF5 OMIC UTILITIES
// =============================================================================
    // #include "hdf5Omics/hdf5RemoveMAF.hpp"
    // #include "hdf5Utilities/hdf5ImputeData.hpp"
    // #include "hdf5Utilities/hdf5RemoveLowData.hpp"





#endif // BIGDATASTATMETH_HPP
