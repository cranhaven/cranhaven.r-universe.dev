/**
 * @file Utilities.hpp
 * @brief Core utility functions and data structures for BigDataStatMeth
 *
 * This file provides essential utility functions and data structures used
 * throughout the BigDataStatMeth package. It includes functionality for
 * file path handling, R object type detection, matrix operations,
 * and block size calculations for efficient memory management.
 *
 * Key features:
 * - Path and filename manipulation
 * - R object type detection and dimension extraction
 * - Matrix block size optimization
 * - SVD and QR decomposition structures
 * - Memory-efficient block operations
 *
 * @note This is a core utility file that many other components depend on.
 */

#ifndef BIGDATASTATMETH_UTILITIES_HPP
#define BIGDATASTATMETH_UTILITIES_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    // Structs
    /**
     * @brief Structure for storing file path components
     */
    struct fullpath {
        std::string path ;
        std::string filename ;
    };


    /**
     * @brief Structure for storing SVD decomposition results
     */
    struct svdeig {
        Eigen::VectorXd d;
        Eigen::MatrixXd u;
        Eigen::MatrixXd v;
        bool bokuv = false;
        bool bokd = false;
        std::string hdf5file = "";
    };

    /**
     * @brief Structure for storing QR decomposition results
     */
    struct strQR {
        Eigen::MatrixXd Q;
        Eigen::MatrixXd R;
    };
    
    /**
     * @brief Matrix type classification for optimization strategies
     * @details Enumeration used to classify matrices based on their dimensional characteristics
     * to apply appropriate optimization algorithms. The classification considers both matrix
     * size and aspect ratios to determine the most efficient computational approach.
     */
    enum MatrixType {
        RECTANGULAR_EXTREME,  // min < 1000 && ratio > 100 (80×860,000)
        SQUARE_SMALL,         // max < 10,000 && ratio < 5  (5,000×5,000)  
        SQUARE_LARGE,         // 10K ≤ max < 100K && ratio < 5 (50,000×50,000)
        SQUARE_EXTREME        // max ≥ 100K && ratio < 5 (500,000×500,000)
    };
    
    
    /**
     * @brief Block size configuration for dual-block algorithms
     * @details Structure containing optimized block sizes for different computational
     * phases of the transposed cross-product operation.
     */
    struct BlockSizes {
        hsize_t inner_block;
        hsize_t output_block;
        MatrixType type;
    };


    // Functions
    
    /**
     * @brief Splits a full file path into directory path and filename
     *
     * @param str Full file path
     * @return fullpath Structure containing separated path and filename
     *
     * @details Handles both forward and backward slashes for cross-platform compatibility
     */
    inline fullpath SplitElementName (std::string str)
    {
        fullpath currentpath;
        std::size_t found = str.find_last_of("/\\");
        
        if( found< str.length() ) {
            currentpath.filename =  str.substr(found+1);
            currentpath.path = str.substr(0,found);
        }else {
            currentpath.filename = str;
            currentpath.path = "";
        }
        return(currentpath);
    }
    
    
    /**
     * @brief Determines the data type of an R object
     *
     * @param obj R object to analyze
     * @return std::string Type identifier string
     *
     * @details Supported types:
     * - "numeric": Numeric vectors/matrices
     * - "int": Integer vectors/matrices
     * - "char": Character vectors/matrices
     * - "logic": Logical vectors/matrices
     * - "dataframe": Data frames
     * - "list": Lists
     * - "S4": S4 objects
     * - "NULL": NULL objects
     * - "unknown": Unrecognized types
     *
     * @throws std::exception on type detection errors
     */
    inline std::string getObjecDataType(Rcpp::RObject obj) 
    {
        
        std::string strtype = "";
        
        try 
        {
            if( Rcpp::is<Rcpp::NumericVector>(obj) ) {
                strtype = "numeric";
            } else if( Rcpp::is<Rcpp::IntegerVector>(obj) ) {
                strtype = "int";
            } else if( Rcpp::is<Rcpp::CharacterVector>(obj) ) {
                strtype = "char";
            } else if( Rcpp::is<Rcpp::LogicalVector>(obj) ) {
                strtype = "logic";
            } else if( Rcpp::is<Rcpp::DataFrame>(obj) ) {
                strtype = "dataframe";
            } else if( Rcpp::is<Rcpp::List>(obj) ) {
                strtype = "list";
            } else if( obj.isS4() ) {
                strtype = "S4";
            } else if( obj.isNULL() ) {
                strtype = "NULL";
            } else {
                strtype = "unknown";
            }
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception getObjecDataType: "<< ex.what() << "\n";
        }
        
        return(strtype);
    }
    
    
    
    /**
     * @brief Gets dimensions of an R object
     *
     * @param obj R object to measure
     * @param strtype Optional pre-determined object type
     * @return Rcpp::IntegerVector Two-element vector with dimensions
     *
     * @details
     * - Returns [1, length] for vectors
     * - Returns [rows, cols] for matrices
     * - Returns [nrow, ncol] for data frames
     * - Returns [0, 0] for unsupported types
     *
     * @throws std::exception on dimension extraction errors
     */
    inline Rcpp::IntegerVector getObjectDims(Rcpp::RObject obj, std::string strtype) 
    {
        
        Rcpp::IntegerVector dims(2);
        
        try 
        {
            if(strtype =="") {
                strtype = getObjecDataType(obj);    
            }
            
            if( strtype == "numeric"  || strtype == "int" || strtype == "factor" ){
                if( Rf_isMatrix(obj)) {
                    dims[0] = Rcpp::as<Rcpp::NumericMatrix>(obj).rows();
                    dims[1] = Rcpp::as<Rcpp::NumericMatrix>(obj).cols();
                } else {
                    dims[0] = 1;
                    dims[1] = Rcpp::as<Rcpp::NumericVector>(obj).length();
                }
            }else if( strtype == "logic" ) {
                if( Rf_isMatrix(obj)) {
                    dims[0] = Rcpp::as<Rcpp::LogicalMatrix>(obj).rows();
                    dims[1] = Rcpp::as<Rcpp::LogicalMatrix>(obj).cols();
                } else {
                    dims[0] = 1;
                    dims[1] = Rcpp::as<Rcpp::LogicalVector>(obj).length();
                }
            } else if( strtype == "char" ){
                if( Rf_isMatrix(obj)) {
                    dims[0] = Rcpp::as<Rcpp::CharacterMatrix>(obj).rows();
                    dims[1] = Rcpp::as<Rcpp::CharacterMatrix>(obj).cols();
                } else {
                    dims[0] = 1;
                    dims[1] = Rcpp::as<Rcpp::CharacterVector>(obj).length();
                }
            } else if(strtype == "dataframe"){
                dims[0] = Rcpp::as<Rcpp::DataFrame>(obj).nrows();
                dims[1] = Rcpp::as<Rcpp::DataFrame>(obj).length();
                
            } else {
                dims[0] = 0;
                dims[1] = 0;
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception getObjecDataType: "<<ex.what()<< " \n";
        }
        
        return(dims);
    }
    
    
    
    /**
     * @brief Calculates optimal block size for matrix operations
     *
     * @param nRowsA Number of rows in matrix A
     * @param nColsA Number of columns in matrix A
     * @param nRowsB Number of rows in matrix B
     * @param nColsB Number of columns in matrix B
     * @param ifactor Block size scaling factor
     * @param block_size Optional user-specified block size
     * @return int Optimal block size
     *
     * @details Block size determination:
     * - Considers matrix dimensions
     * - Respects maximum block size limits
     * - Allows user override with warnings
     * - Optimizes for memory usage
     *
     * @throws std::exception on calculation errors
     */
    inline int getMaxBlockSize ( int nRowsA, int nColsA, int nRowsB, int nColsB, int ifactor, Rcpp::Nullable<int> block_size = R_NilValue) 
    {
        
        int iblock_size;
        
        try
        {
            
            iblock_size = std::min( std::min( nRowsA, nColsA), std::min( nRowsB, nColsB) );

            if (block_size.isNotNull()) {
                // if( Rcpp::as<int> (block_size) < iblock_size ) {
                //     iblock_size = Rcpp::as<int> (block_size); }
                iblock_size = Rcpp::as<int> (block_size);
                if( (unsigned)iblock_size > (MAXBLOCKSIZE / ifactor) ) {
                    Rcpp::warning("Warning: block size %i is bigger than the maximum recomended %i.", iblock_size, (MAXBLOCKSIZE / ifactor));
                }
            } else {
                //..// iblock_size = std::min(  std::min(dsA->nrows(),dsA->ncols()),  std::min(dsB->nrows(), dsB->ncols()));
                if ((unsigned)iblock_size > (MAXBLOCKSIZE / ifactor))
                    iblock_size = MAXBLOCKSIZE / ifactor;
            }
                
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception getObjecDataType: "<<ex.what()<< " \n";
        }
        
        return(iblock_size);
    }
    
    
    /**
     * @brief Optimizes block size for edge cases
     *
     * @param fullSize Total size of dimension
     * @param blockSize Current block size
     * @param iDesp Current displacement
     * @param currentSize Current size being processed
     * @return size_t Optimized block size
     *
     * @details Optimization strategy:
     * - Handles last block to avoid single element operations
     * - Adjusts for matrix boundaries
     * - Prevents overflow
     * - Optimizes for performance
     *
     * @throws std::exception on optimization errors
     */
    inline size_t getOptimBlockSize( size_t fullSize, size_t blockSize, size_t iDesp, size_t currentSize ) 
    {
        
        try
        {
            if( iDesp + blockSize == fullSize - 1) {
                currentSize = blockSize + 1;
            } else if( iDesp + blockSize > fullSize ) { 
                currentSize = fullSize - iDesp; 
            } else {
                currentSize = blockSize;
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception getOptimBlockSize: "<<ex.what()<< " \n";
        }
        
        return(currentSize);
    }
    

    // Get the number of rows to read taking in to account the maximum elements per block
    // util when we have rectangular matrices, especially in omics data where we have
    // few samples and thousands of variables
    
    /**
     * @brief Calculates optimal block size for matrix operations
     *
     * @param nrows Number of rows
     * @param ncols Number of columns
     * @return std::vector<hsize_t> Vector containing optimized dimensions
     *
     * @details Particularly useful for:
     * - Omics data with few samples and many variables
     * - Memory-efficient processing of rectangular matrices
     * - Optimizing I/O operations
     *
     * @note Considers both memory constraints and processing efficiency
     */
    inline std::vector<hsize_t> getMatrixBlockSize( int nrows, int ncols ) 
    {
        size_t  maxRows = nrows,
                maxCols = ncols;
        
        std::vector<hsize_t> blockSize = {0, 0};
        
        try
        {
            // Calculem el mínim de files
            if( nrows < ncols ) {
                if( maxRows < MAXBLOCKSIZE ){
                    maxRows = nrows;
                } else{
                    maxRows = MAXBLOCKSIZE;
                }
                
                maxCols = std::floor( MAXELEMSINBLOCK / maxRows );
                if( maxCols> (unsigned)ncols || maxCols + 1 == (unsigned)ncols) {
                    maxCols = ncols;
                }
            } else {
                if( maxCols < MAXBLOCKSIZE ){
                    maxCols = ncols;
                } else{
                    maxCols = MAXBLOCKSIZE;
                }
                maxRows = std::floor( MAXELEMSINBLOCK / maxCols );
                if( maxRows> (unsigned)nrows || maxRows + 1 == (unsigned)nrows) {
                    maxRows = nrows;
                }
                
            }    
            blockSize[0] = maxRows;
            blockSize[1] = maxCols;
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception getMatrixBlockSize: "<<ex.what()<< " \n";
        }
        
        return(blockSize);
    }
    
    
    // Get the number of rows to read taking in to account the maximum elements per block
    // util when we have rectangular matrices, especially in omics data where we have
    // few samples and thousands of variables
    /**
     * @brief Calculates optimal block size for vector operations
     *
     * @param maxSize Maximum size of the vector
     * @return hsize_t Optimal block size for vector processing
     *
     * @details Block size optimization:
     * - Considers memory constraints
     * - Optimizes for vector operations
     * - Ensures efficient I/O
     *
     * @note Particularly useful for long vectors that don't fit in memory
     */
    inline hsize_t getVectorBlockSize(int maxSize) 
    {
        hsize_t blockSize = 0;
        
        try
        {
            if( (unsigned)maxSize > MAXELEMSINBLOCK) {
                blockSize = MAXELEMSINBLOCK;
            } else {
                blockSize = maxSize;
            }
            
        } catch(std::exception& ex) {
            Rcpp::Rcout<< "c++ exception getVectorBlockSize: "<<ex.what()<< " \n";
        }
        
        return(blockSize);
    }
    
    
    
    
    // inline Rcpp::IntegerVector getInitialPosition(bool transp, int desp )
    // {
    //     Rcpp::IntegerVector voffset(2);
    //     
    //     if(transp == true)
    //     {
    //         voffset[0] = 0;
    //         voffset[1] = desp;
    //     } else {
    //         voffset[0] = desp;
    //         voffset[1] = 0;
    //     }
    //     
    //     return(voffset);
    // }
    
    /**
     * @brief Determines initial position for matrix operations
     *
     * @param transp Whether to use transposed coordinates
     * @param desp Displacement from start
     * @return std::vector<hsize_t> Initial position coordinates
     *
     * @details Position calculation:
     * - Handles both normal and transposed matrices
     * - Accounts for displacement
     * - Returns [row, col] coordinates
     */
    inline std::vector<hsize_t> getInitialPosition(bool transp, int desp)
    {
        std::vector<hsize_t> voffset = {0, 0};
        
        if(transp == true) {
            voffset[1] = desp;
        } else {
            voffset[0] = desp;
        }
        
        return(voffset);
    }
    
    
    
    /**
     * @brief Checks if a file exists at the specified path
     *
     * @param fullPath Complete file path to check
     * @return bool True if file exists, false otherwise
     *
     * @details File check:
     * - Uses system calls to verify existence
     * - Handles both relative and absolute paths
     * - Thread-safe implementation
     */
    inline bool Rcpp_FileExist(std::string fullPath) 
    {
        bool exists = false;
        
        std::fstream fileStream;
        fileStream.open(fullPath);
        
        if (fileStream.good()) {
            exists = true;
        } else {
            exists = false;
        }
        return(exists);
    }
    
    
    /**
     * @brief Calculates dimensions for reading matrix blocks
     *
     * @param transp Whether to transpose dimensions
     * @param count Number of elements to read
     * @param rows Total rows in matrix
     * @param cols Total columns in matrix
     * @return std::vector<hsize_t> Dimensions for reading
     *
     * @details Dimension calculation:
     * - Handles transposed matrices
     * - Respects matrix boundaries
     * - Optimizes for memory usage
     * - Returns [rows_to_read, cols_to_read]
     */
    inline std::vector<hsize_t> getSizetoRead(bool transp, int count, int rows, int cols)
    {
        std::vector<hsize_t> vcount = {0, 0};

        if(transp == true)
        {
            vcount[0] = cols;
            vcount[1] = count;
            
        } else {
            vcount[0] = count;
            vcount[1] = cols;
        }
        
        return(vcount);
    }
    
    
    
    
    // Return the numbers of threads to be used in parallel processes
    /**
     * @brief Determines number of threads for parallel operations
     *
     * @param bparal Whether to use parallel processing
     * @param threads Optional number of threads to use
     * @return unsigned int Number of threads to use
     *
     * @details Thread determination:
     * - Considers hardware concurrency
     * - Respects user-specified thread count
     * - Falls back to single thread if parallel disabled
     * - Optimizes for system resources
     *
     * @note Integrates with OpenMP thread management
     */
    inline unsigned int get_threads(bool bparal, Rcpp::Nullable<int> threads = R_NilValue) 
    {
        unsigned int ithreads = std::thread::hardware_concurrency();
        
        if(bparal == false) {
            ithreads = 1;
        } else {
            if(threads.isNotNull()) {
                
                if ((unsigned)Rcpp::as<int> (threads) <= ithreads){
                    ithreads = Rcpp::as<int> (threads);
                } 
            
            } else {
                ithreads =  getDTthreads(ithreads, false);
            }    
        }
        
        return(ithreads);
    }
    
    /**
     * @brief Classify matrix type based on dimensional characteristics
     * @details Analyzes matrix dimensions to determine the optimal computational strategy.
     * Classification considers both absolute size and aspect ratios to identify
     * rectangular vs. square-like matrices and their relative sizes.
     * 
     * @param N Number of rows in result matrix
     * @param M Number of columns in result matrix  
     * @param K Inner dimension for matrix multiplication
     * @return MatrixType Classification result for optimization strategy selection
     * 
     * @note Classification logic:
     * - RECTANGULAR_EXTREME: min_dim < 1000 && ratio > 100
     * - SQUARE_SMALL: max_dim < 10000 && ratio < 5
     * - SQUARE_LARGE: 10000 ≤ max_dim < 100000 && ratio < 5
     * - SQUARE_EXTREME: max_dim ≥ 100000 && ratio < 5
     * - Default fallback: RECTANGULAR_EXTREME for other cases
     */
    inline MatrixType classify_matrix_type(hsize_t N, hsize_t M, hsize_t K) {
        hsize_t min_dim = std::min({N, M, K});
        hsize_t max_dim = std::max({N, M, K});
        double ratio = (double)max_dim / min_dim;
        
        if (min_dim < 1000 && ratio > 100) {
            return RECTANGULAR_EXTREME;
        } else if (max_dim < 10000 && ratio < 5) {
            return SQUARE_SMALL;
        } else if (max_dim < 100000 && ratio < 5) {
            return SQUARE_LARGE;  
        } else if (ratio < 5) {
            return SQUARE_EXTREME;
        } else {
            return RECTANGULAR_EXTREME;  // Default for other rectangular cases
        }
    }
    
}

#endif // BIGDATASTATMETH_UTILITIES_HPP
