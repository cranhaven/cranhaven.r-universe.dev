/**
 * @file hdf5_diagonalOperations.cpp
 * @brief Diagonal operations for HDF5 datasets
 * 
 * This file implements functionality for performing arithmetic operations on
 * diagonal elements of matrices or vectors stored in HDF5 format. It provides
 * optimized operations that are significantly faster than traditional matrix
 * approaches by focusing only on diagonal elements.
 * 
 * Key features:
 * - Diagonal addition, subtraction, multiplication, and division
 * - Automatic type detection (matrix vs vector inputs)
 * - Flexible target specification (in-place or new dataset creation)
 * - Memory-efficient processing through extract-operate-write strategy
 * - Parallel processing support for large datasets
 * - Comprehensive error handling and validation
 * 
 * Operation modes supported:
 * - Matrix + Matrix: Extract diagonals → vector operation → write result
 * - Matrix + Vector: Extract diagonal → vector operation → write result
 * - Vector + Vector: Direct vector operation (most efficient)
 * 
 * Target options:
 * - "new": Create new dataset for result (default)
 * - "A": Write result to first operand (in-place)
 * - "B": Write result to second operand (in-place)
 * 
 * Performance characteristics:
 * - ~50-250x faster than traditional matrix operations for diagonal computations
 * - Uses optimized vector operations from vectorOperations.hpp
 * - Leverages existing matrix diagonal utilities from matrixDiagonal.hpp
 * - Automatic memory management and cleanup
 * 
 * The implementation focuses on:
 * - Minimal memory footprint through diagonal-only processing
 * - Efficient I/O operations using HDF5 hyperslab selection
 * - Robust dimension validation and type checking
 * - Seamless integration with BigDataStatMeth ecosystem
 * 
 * @note This module is part of the BigDataStatMeth library
 * @see BigDataStatMeth::DiagonalOps for the underlying operation implementations
 * @see matrixDiagonal.hpp for diagonal extraction/writing utilities
 * @see vectorOperations.hpp for core vector arithmetic operations
 */

#include <BigDataStatMeth.hpp>

/**
 * @brief Add diagonal elements from two HDF5 matrices or vectors
 *
 * @details This function performs optimized diagonal addition between two datasets
 * stored in HDF5 format. It automatically detects whether inputs are matrices
 * (extracts diagonals) or vectors (direct operation) and uses the most efficient
 * approach. The operation is approximately 50-250x faster than traditional matrix
 * addition when only diagonal elements are needed.
 *
 * Supported operation modes:
 * - Matrix + Matrix: Extracts diagonals from both matrices, performs vector addition,
 *   and writes result according to target specification
 * - Matrix + Vector: Extracts diagonal from matrix, performs vector addition with
 *   the vector input, and writes result according to target
 * - Vector + Vector: Performs direct vector addition (most efficient mode)
 *
 * Target behavior:
 * - target="new": Creates new dataset with result (default behavior)
 * - target="A": Writes result in-place to first operand A
 * - target="B": Writes result in-place to second operand B
 *
 * Validation checks:
 * - Matrix inputs must be square (N×N) for diagonal extraction
 * - Vector inputs must have compatible dimensions (same length)
 * - Automatic dimension matching between operands
 *
 * @param filename [in] Path to the HDF5 file containing the datasets
 * @param group [in] Group path containing the first dataset (A)
 * @param A [in] Name of the first dataset (matrix or vector)
 * @param B [in] Name of the second dataset (matrix or vector)
 * @param groupB [in] Optional group path containing dataset B (default: same as group)
 * @param target [in] Where to write result: "A", "B", or "new" (default: "new")
 * @param outgroup [in] Output group path, only used if target="new" (default: "OUTPUT")
 * @param outdataset [in] Output dataset name, only used if target="new" (default: "A_+_B.diag")
 * @param paral [in] Whether to use parallel processing (default: false)
 * @param threads [in] Number of threads for parallel processing (default: auto-detect)
 * @param overwrite [in] Whether to overwrite existing datasets (default: false)
 *
 * @return List with components:
 *   - fn: Character string with the HDF5 filename
 *   - ds: Character string with the full dataset path (group/dataset)
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception if dimension mismatch or validation fails
 *
 * @note For maximum efficiency with vector inputs, ensure data is stored in
 *       vector format (1×N or N×1) rather than matrix format
 * @note The .diag suffix is automatically added to output names for identification
 *
 * @see BigDataStatMeth::DiagonalOps::addDiagonals()
 * @see bdDiag_subtract_hdf5(), bdDiag_multiply_hdf5(), bdDiag_divide_hdf5()
 */

/**
 * @brief Subtract diagonal elements from two HDF5 matrices or vectors
 *
 * @details This function performs optimized diagonal subtraction between two datasets
 * stored in HDF5 format (C = A - B). It uses the same optimization strategy as
 * diagonal addition but for subtraction operations. Automatically detects input
 * types and selects the most efficient processing approach.
 *
 * Operation semantics:
 * - Performs element-wise subtraction on diagonal elements
 * - Preserves numerical precision throughout the operation
 * - Handles edge cases like underflow gracefully
 * - Maintains data type consistency with inputs
 *
 * @param filename [in] Path to the HDF5 file containing the datasets
 * @param group [in] Group path containing the first dataset (A, minuend)
 * @param A [in] Name of the first dataset (minuend)
 * @param B [in] Name of the second dataset (subtrahend)
 * @param groupB [in] Optional group path containing dataset B (default: same as group)
 * @param target [in] Where to write result: "A", "B", or "new" (default: "new")
 * @param outgroup [in] Output group path, only used if target="new" (default: "OUTPUT")
 * @param outdataset [in] Output dataset name, only used if target="new" (default: "A_-_B.diag")
 * @param paral [in] Whether to use parallel processing (default: false)
 * @param threads [in] Number of threads for parallel processing (default: auto-detect)
 * @param overwrite [in] Whether to overwrite existing datasets (default: false)
 *
 * @return List with components:
 *   - fn: Character string with the HDF5 filename
 *   - ds: Character string with the full dataset path (group/dataset)
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception if dimension mismatch or validation fails
 *
 * @note Order matters: result = A - B (A is minuend, B is subtrahend)
 *
 * @see BigDataStatMeth::DiagonalOps::subtractDiagonals()
 * @see bdDiag_add_hdf5() for detailed operation mode descriptions
 */

/**
 * @brief Multiply diagonal elements from two HDF5 matrices or vectors
 *
 * @details This function performs optimized element-wise multiplication of diagonal
 * elements between two datasets stored in HDF5 format (C = A * B). Uses the same
 * optimization strategy as other diagonal operations but for multiplication.
 * This is element-wise multiplication, not matrix multiplication.
 *
 * Mathematical operation:
 * - Performs element-wise multiplication: result[i] = A_diagonal[i] * B_diagonal[i]
 * - Handles overflow conditions according to IEEE 754 standards
 * - Preserves sign information correctly
 * - Maintains numerical stability for large values
 *
 * @param filename [in] Path to the HDF5 file containing the datasets
 * @param group [in] Group path containing the first dataset (A)
 * @param A [in] Name of the first dataset
 * @param B [in] Name of the second dataset
 * @param groupB [in] Optional group path containing dataset B (default: same as group)
 * @param target [in] Where to write result: "A", "B", or "new" (default: "new")
 * @param outgroup [in] Output group path, only used if target="new" (default: "OUTPUT")
 * @param outdataset [in] Output dataset name, only used if target="new" (default: "A_*_B.diag")
 * @param paral [in] Whether to use parallel processing (default: false)
 * @param threads [in] Number of threads for parallel processing (default: auto-detect)
 * @param overwrite [in] Whether to overwrite existing datasets (default: false)
 *
 * @return List with components:
 *   - fn: Character string with the HDF5 filename
 *   - ds: Character string with the full dataset path (group/dataset)
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception if dimension mismatch or validation fails
 *
 * @note This is element-wise multiplication, not matrix multiplication
 * @note Multiplication is commutative: A * B = B * A
 *
 * @see BigDataStatMeth::DiagonalOps::multiplyDiagonals()
 * @see bdDiag_add_hdf5() for detailed operation mode descriptions
 */

/**
 * @brief Divide diagonal elements from two HDF5 matrices or vectors
 *
 * @details This function performs optimized element-wise division of diagonal
 * elements between two datasets stored in HDF5 format (C = A / B). Uses the same
 * optimization strategy as other diagonal operations but for division.
 * Handles division by zero according to IEEE 754 standards.
 *
 * Mathematical operation:
 * - Performs element-wise division: result[i] = A_diagonal[i] / B_diagonal[i]
 * - Division by zero results in infinity (IEEE 754 standard behavior)
 * - Handles special cases: ±inf, NaN, and subnormal numbers
 * - Preserves numerical precision and sign information
 *
 * Division behavior:
 * - Positive / Positive = Positive
 * - Positive / Negative = Negative
 * - Any / 0 = ±Infinity (sign preserved)
 * - 0 / 0 = NaN
 *
 * @param filename [in] Path to the HDF5 file containing the datasets
 * @param group [in] Group path containing the first dataset (A, dividend)
 * @param A [in] Name of the first dataset (dividend)
 * @param B [in] Name of the second dataset (divisor)
 * @param groupB [in] Optional group path containing dataset B (default: same as group)
 * @param target [in] Where to write result: "A", "B", or "new" (default: "new")
 * @param outgroup [in] Output group path, only used if target="new" (default: "OUTPUT")
 * @param outdataset [in] Output dataset name, only used if target="new" (default: "A_/_B.diag")
 * @param paral [in] Whether to use parallel processing (default: false)
 * @param threads [in] Number of threads for parallel processing (default: auto-detect)
 * @param overwrite [in] Whether to overwrite existing datasets (default: false)
 *
 * @return List with components:
 *   - fn: Character string with the HDF5 filename
 *   - ds: Character string with the full dataset path (group/dataset)
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception if dimension mismatch or validation fails
 *
 * @note Order matters: result = A / B (A is dividend, B is divisor)
 * @note Division by zero results in infinity (IEEE 754 standard behavior)
 * @note Check for infinite results if division by zero is a concern
 *
 * @see BigDataStatMeth::DiagonalOps::divideDiagonals()
 * @see bdDiag_add_hdf5() for detailed operation mode descriptions
 */


//' Add Diagonal Elements from HDF5 Matrices or Vectors
//'
//' @description
//' Performs optimized diagonal addition between two datasets stored in HDF5 format.
//' Automatically detects whether inputs are matrices (extracts diagonals) or vectors
//' (direct operation) and uses the most efficient approach.
//'
//' @param filename String. Path to the HDF5 file containing the datasets.
//' @param group String. Group path containing the first dataset (A).
//' @param A String. Name of the first dataset (matrix or vector).
//' @param B String. Name of the second dataset (matrix or vector).
//' @param groupB Optional string. Group path containing dataset B.
//' @param target Optional string. Where to write result: "A", "B", or "new" (default: "new").
//' @param outgroup Optional string. Output group path (only used if target="new").
//' @param outdataset Optional string. Output dataset name (only used if target="new").
//' @param paral Optional logical. Whether to use parallel processing.
//' @param threads Optional integer. Number of threads for parallel processing.
//' @param overwrite Optional logical. Whether to overwrite existing datasets.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the diagonal addition result (group/dataset)}
//' }
//'
//' @export
 // [[Rcpp::export]]
 Rcpp::List bdDiag_add_hdf5(std::string filename,
                            std::string group,
                            std::string A,
                            std::string B,
                            Rcpp::Nullable<std::string> groupB = R_NilValue,
                            Rcpp::Nullable<std::string> target = R_NilValue,
                            Rcpp::Nullable<std::string> outgroup = R_NilValue,
                            Rcpp::Nullable<std::string> outdataset = R_NilValue,
                            Rcpp::Nullable<bool> paral = R_NilValue,
                            Rcpp::Nullable<int> threads = R_NilValue,
                            Rcpp::Nullable<bool> overwrite = R_NilValue)
 {
     BigDataStatMeth::hdf5Dataset* dsA = nullptr;
     BigDataStatMeth::hdf5Dataset* dsB = nullptr;
     BigDataStatMeth::hdf5Dataset* dsResult = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
     try {
         H5::Exception::dontPrint();
         
         bool bparal, 
              bforce, 
              bforce_tA = false, 
              bforce_tB = false;
         std::string strsubgroupOut, strdatasetOut, strsubgroupInB, strtarget;
         
         
         // Parse parameters
         if (paral.isNull()) { bparal = false; } 
         else { bparal = Rcpp::as<bool>(paral); }
         
         if (overwrite.isNull()) { bforce = false; } 
         else { bforce = Rcpp::as<bool>(overwrite); }
         
         if (target.isNull()) { strtarget = "new"; }
         else { strtarget = Rcpp::as<std::string>(target); }
         
         // Validate target values
         std::string strtargetChk = strtarget;
         std::transform(strtargetChk.begin(), strtargetChk.end(), strtargetChk.begin(), ::tolower);
         if (strtargetChk != "new" && strtargetChk != "a" && strtargetChk != "b") {
             Rcpp::stop("Invalid target value: '%s'. Must be 'new', 'A', or 'B'", strtarget.c_str());
         }
         
         if (groupB.isNotNull()) { strsubgroupInB = Rcpp::as<std::string>(groupB); } 
         else { strsubgroupInB = group; }
         
         if (outgroup.isNull()) { 
             if( strtarget =="A") {
                 strsubgroupOut = "group";
                 bforce_tA = true;
                 
             } else if( strtarget =="B") {
                 strsubgroupOut = strsubgroupInB;
                 bforce_tB = true;
             } else {
                 strsubgroupOut = "OUTPUT"; 
            }    
         } else { strsubgroupOut = Rcpp::as<std::string>(outgroup); }
         
         if (outdataset.isNotNull()) { strdatasetOut = Rcpp::as<std::string>(outdataset); } 
         else { strdatasetOut = A + "_+_" + B + ".diag"; }
         
         // Open datasets
         dsA = new BigDataStatMeth::hdf5Dataset(filename, group, A, bforce_tA);
         dsA->openDataset();
         
         dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, bforce_tB);
         dsB->openDataset();
         
         if (dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
             
             lst_return["fn"] = filename;
             
             // Only create result dataset if target="new"
             if (strtarget == "new") {
                 dsResult = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
                 lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
             } else if (strtarget == "A") {
                 lst_return["ds"] = group + "/" + A;
             } else if (strtarget == "B") {
                 lst_return["ds"] = strsubgroupInB + "/" + B;
             }
             
             // Perform diagonal addition using namespace function
             BigDataStatMeth::DiagonalOps::addDiagonals(dsA, dsB, dsResult, strtarget, bparal, threads);
         }
         
         delete dsA; dsA = nullptr;
         delete dsB; dsB = nullptr;
         if (dsResult) { delete dsResult; dsResult = nullptr; }
         
     }  catch(H5::FileIException& error) {
         checkClose_file(dsA, dsB, dsResult);
         Rcpp::stop("c++ exception bdDiag_add_hdf5 (File IException)");
     } catch(H5::DataSetIException& error) {
         checkClose_file(dsA, dsB, dsResult);
         Rcpp::stop("c++ exception bdDiag_add_hdf5 (DataSet IException)");
     } catch(std::exception& ex) {
         checkClose_file(dsA, dsB, dsResult);
         Rcpp::stop("c++ exception bdDiag_add_hdf5: %s", ex.what());
     } catch(...) {
         checkClose_file(dsA, dsB, dsResult);
         Rcpp::stop("C++ exception bdDiag_add_hdf5 (unknown reason)");
     }
     
     return lst_return;
 }



//' Subtract Diagonal Elements from HDF5 Matrices or Vectors
//'
//' @description
//' Performs optimized diagonal subtraction between two datasets stored in HDF5 format.
//' Automatically detects whether inputs are matrices (extracts diagonals) or vectors
//' (direct operation) and uses the most efficient approach. This function is ~50-250x
//' faster than traditional matrix operations for diagonal computations.
//'
//' @details
//' This function provides flexible diagonal subtraction with automatic optimization:
//' 
//' * Operation modes:
//'   - Matrix - Matrix: Extract diagonals → vector subtraction → save as vector
//'   - Matrix - Vector: Extract diagonal → vector subtraction → save as vector  
//'   - Vector - Vector: Direct vector subtraction (most efficient)
//' 
//' * Performance features:
//'   - Uses optimized vector operations for maximum efficiency
//'   - Automatic type detection and dimension validation
//'   - Memory-efficient processing for large datasets
//'   - Parallel processing support for improved performance
//'
//' * Validation checks:
//'   - Matrix inputs must be square (N×N)
//'   - Vector inputs must have compatible dimensions
//'   - Automatic dimension matching between operands
//'
//' @param filename String. Path to the HDF5 file containing the datasets.
//' @param group String. Group path containing the first dataset (A, minuend).
//' @param A String. Name of the first dataset (minuend).
//' @param B String. Name of the second dataset (subtrahend).
//' @param groupB Optional string. Group path containing dataset B.
//'   If NULL, uses same group as A.
//' @param target Optional string. Where to write result: "A", "B", or "new" (default: "new").
//' @param outgroup Optional string. Output group path.
//'   Default is "OUTPUT".
//' @param outdataset Optional string. Output dataset name.
//'   Default is "A_-_B" with .diag suffix if appropriate.
//' @param paral Optional logical. Whether to use parallel processing.
//'   Default is FALSE.
//' @param threads Optional integer. Number of threads for parallel processing.
//'   If NULL, uses maximum available threads.
//' @param overwrite Optional logical. Whether to overwrite existing datasets.
//'   Default is FALSE.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the diagonal subtraction result (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' N <- 1000
//' set.seed(123)
//' A <- matrix(rnorm(N*N), N, N)
//' B <- matrix(rnorm(N*N), N, N)
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", A, "data", "matrixA",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix("test.hdf5", B, "data", "matrixB",
//'                      overwriteFile = FALSE)
//' 
//' # Subtract diagonals
//' result <- bdDiag_subtract_hdf5("test.hdf5", "data", "matrixA", "matrixB",
//'                               outgroup = "results",
//'                               outdataset = "diagonal_diff",
//'                               paral = TRUE)
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdDiag_subtract_hdf5(std::string filename,
                                std::string group,
                                std::string A,
                                std::string B,
                                Rcpp::Nullable<std::string> groupB = R_NilValue,
                                Rcpp::Nullable<std::string> target = R_NilValue,
                                Rcpp::Nullable<std::string> outgroup = R_NilValue,
                                Rcpp::Nullable<std::string> outdataset = R_NilValue,
                                Rcpp::Nullable<bool> paral = R_NilValue,
                                Rcpp::Nullable<int> threads = R_NilValue,
                                Rcpp::Nullable<bool> overwrite = R_NilValue)
{
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsResult = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try {
        H5::Exception::dontPrint();
        
        bool bparal, bforce;
        std::string strsubgroupOut, strdatasetOut, strsubgroupInB, strtarget;
        
        // Parse parameters
        if (paral.isNull()) { bparal = false; } 
        else { bparal = Rcpp::as<bool>(paral); }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool>(overwrite); }
        
        if (target.isNull()) { strtarget = "new"; }
        else { strtarget = Rcpp::as<std::string>(target); }
        
        // Validate target values
        std::string strtargetChk = strtarget;
        std::transform(strtargetChk.begin(), strtargetChk.end(), strtargetChk.begin(), ::tolower);
        if (strtargetChk != "new" && strtargetChk != "a" && strtargetChk != "b") {
            Rcpp::stop("Invalid target value: '%s'. Must be 'new', 'A', or 'B'", strtarget.c_str());
        }
        
        if (groupB.isNotNull()) { strsubgroupInB = Rcpp::as<std::string>(groupB); } 
        else { strsubgroupInB = group; }
        
        if (outgroup.isNull()) { 
            if( strtarget =="A") {
                strsubgroupOut = "group";
            } else if( strtarget =="B") {
                strsubgroupOut = strsubgroupInB;
            } else {
                strsubgroupOut = "OUTPUT"; 
            }    
        } else { strsubgroupOut = Rcpp::as<std::string>(outgroup); }
        
        if (outdataset.isNotNull()) { strdatasetOut = Rcpp::as<std::string>(outdataset); } 
        else { strdatasetOut = A + "_-_" + B + ".diag"; }
        
        // Open datasets
        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, A, false);
        dsA->openDataset();
        
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, false);
        dsB->openDataset();
        
        if (dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
            
            lst_return["fn"] = filename;
            
            // Only create result dataset if target="new"
            if (strtarget == "new") {
                dsResult = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
                lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
            } else if (strtarget == "A") {
                lst_return["ds"] = group + "/" + A;
            } else if (strtarget == "B") {
                lst_return["ds"] = strsubgroupInB + "/" + B;
            }
            
            // Perform diagonal subtraction using namespace function
            BigDataStatMeth::DiagonalOps::subtractDiagonals(dsA, dsB, dsResult, strtarget, bparal, threads);
        }
        
        delete dsA; dsA = nullptr;
        delete dsB; dsB = nullptr;
        if (dsResult) { delete dsResult; dsResult = nullptr; }
        
    } catch(H5::FileIException& error) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_subtract_hdf5 (File IException)");
    } catch(H5::DataSetIException& error) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_subtract_hdf5 (DataSet IException)");
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_subtract_hdf5: %s", ex.what());
    } catch(...) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("C++ exception bdDiag_subtract_hdf5 (unknown reason)");
    }
    
    return lst_return;
}




//' Multiply Diagonal Elements from HDF5 Matrices or Vectors
//'
//' @description
//' Performs optimized diagonal multiplication between two datasets stored in HDF5 format.
//' Automatically detects whether inputs are matrices (extracts diagonals) or vectors
//' (direct operation) and uses the most efficient approach. This function performs
//' element-wise multiplication and is ~50-250x faster than traditional matrix operations.
//'
//' @details
//' This function provides flexible diagonal multiplication with automatic optimization:
//' 
//' * Operation modes:
//'   - Matrix * Matrix: Extract diagonals → vector multiplication → save as vector
//'   - Matrix * Vector: Extract diagonal → vector multiplication → save as vector  
//'   - Vector * Vector: Direct vector multiplication (most efficient)
//' 
//' * Performance features:
//'   - Uses optimized vector operations for maximum efficiency
//'   - Automatic type detection and dimension validation
//'   - Memory-efficient processing for large datasets
//'   - Parallel processing support for improved performance
//'
//' * Mathematical properties:
//'   - Element-wise multiplication (not matrix multiplication)
//'   - Commutative operation: A * B = B * A
//'   - Handles overflow according to IEEE 754 standards
//'   - Preserves sign information correctly
//'
//' @param filename String. Path to the HDF5 file containing the datasets.
//' @param group String. Group path containing the first dataset (A).
//' @param A String. Name of the first dataset (matrix or vector).
//' @param B String. Name of the second dataset (matrix or vector).
//' @param groupB Optional string. Group path containing dataset B.
//'   If NULL, uses same group as A.
//' @param target Optional string. Where to write result: "A", "B", or "new" (default: "new").
//' @param outgroup Optional string. Output group path.
//'   Default is "OUTPUT".
//' @param outdataset Optional string. Output dataset name.
//'   Default is "A_*_B" with .diag suffix if appropriate.
//' @param paral Optional logical. Whether to use parallel processing.
//'   Default is FALSE.
//' @param threads Optional integer. Number of threads for parallel processing.
//'   If NULL, uses maximum available threads.
//' @param overwrite Optional logical. Whether to overwrite existing datasets.
//'   Default is FALSE.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the diagonal multiplication result (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' N <- 1000
//' set.seed(123)
//' A <- matrix(rnorm(N*N), N, N)
//' B <- matrix(rnorm(N*N), N, N)
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", A, "data", "matrixA",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix("test.hdf5", B, "data", "matrixB",
//'                      overwriteFile = FALSE)
//' 
//' # Multiply diagonals (element-wise)
//' result <- bdDiag_multiply_hdf5("test.hdf5", "data", "matrixA", "matrixB",
//'                               outgroup = "results",
//'                               outdataset = "diagonal_product",
//'                               paral = TRUE)
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdDiag_multiply_hdf5(std::string filename, std::string group,
                                std::string A, std::string B,
                                Rcpp::Nullable<std::string> groupB = R_NilValue,
                                Rcpp::Nullable<std::string> target = R_NilValue,
                                Rcpp::Nullable<std::string> outgroup = R_NilValue,
                                Rcpp::Nullable<std::string> outdataset = R_NilValue,
                                Rcpp::Nullable<bool> paral = R_NilValue,
                                Rcpp::Nullable<int> threads = R_NilValue,
                                Rcpp::Nullable<bool> overwrite = R_NilValue)
{
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsResult = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try {
        H5::Exception::dontPrint();
        
        bool bparal, bforce;
        std::string strsubgroupOut, strdatasetOut, strsubgroupInB, strtarget;
        
        // Parse parameters
        if (paral.isNull()) { bparal = false; } 
        else { bparal = Rcpp::as<bool>(paral); }
        
        if (target.isNull()) { strtarget = "new"; }
        else { strtarget = Rcpp::as<std::string>(target); }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool>(overwrite); }
        
        // Validate target values
        std::string strtargetChk = strtarget;
        std::transform(strtargetChk.begin(), strtargetChk.end(), strtargetChk.begin(), ::tolower);
        if (strtargetChk != "new" && strtargetChk != "a" && strtargetChk != "b") {
            Rcpp::stop("Invalid target value: '%s'. Must be 'new', 'A', or 'B'", strtarget.c_str());
        }
        
        if (groupB.isNotNull()) { strsubgroupInB = Rcpp::as<std::string>(groupB); } 
        else { strsubgroupInB = group; }
        
        if (outgroup.isNull()) { 
            if( strtarget =="A") {
                strsubgroupOut = "group";
            } else if( strtarget =="B") {
                strsubgroupOut = strsubgroupInB;
            } else {
                strsubgroupOut = "OUTPUT"; 
            }    
        } else { strsubgroupOut = Rcpp::as<std::string>(outgroup); }
        
        if (outdataset.isNotNull()) { strdatasetOut = Rcpp::as<std::string>(outdataset); } 
        else { strdatasetOut = A + "_*_" + B + ".diag"; }
        
        // Open datasets
        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, A, false);
        dsA->openDataset();
        
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, false);
        dsB->openDataset();
        
        if (dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
            
            lst_return["fn"] = filename;
            
            // Only create result dataset if target="new"
            if (strtarget == "new") {
                dsResult = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
                lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
            } else if (strtarget == "A") {
                lst_return["ds"] = group + "/" + A;
            } else if (strtarget == "B") {
                lst_return["ds"] = strsubgroupInB + "/" + B;
            }
            
            // Perform diagonal multiplication using namespace function
            BigDataStatMeth::DiagonalOps::multiplyDiagonals(dsA, dsB, dsResult, strtarget, bparal, threads);
        }
        
        delete dsA; dsA = nullptr;
        delete dsB; dsB = nullptr;
        if (dsResult) { delete dsResult; dsResult = nullptr; }
        
    } catch(H5::FileIException& error) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_multiply_hdf5 (File IException)");
    } catch(H5::DataSetIException& error) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_multiply_hdf5 (DataSet IException)");
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_multiply_hdf5: %s", ex.what());
    } catch(...) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("C++ exception bdDiag_multiply_hdf5 (unknown reason)");
    }
    
    return lst_return;
}



//' Divide Diagonal Elements from HDF5 Matrices or Vectors
//'
//' @description
//' Performs optimized diagonal division between two datasets stored in HDF5 format.
//' Automatically detects whether inputs are matrices (extracts diagonals) or vectors
//' (direct operation) and uses the most efficient approach. This function is ~50-250x
//' faster than traditional matrix operations for diagonal computations.
//'
//' @details
//' This function provides flexible diagonal division with automatic optimization:
//' 
//' * Operation modes:
//'   - Matrix / Matrix: Extract diagonals → vector division → save as vector
//'   - Matrix / Vector: Extract diagonal → vector division → save as vector  
//'   - Vector / Vector: Direct vector division (most efficient)
//' 
//' * Performance features:
//'   - Uses optimized vector operations for maximum efficiency
//'   - Automatic type detection and dimension validation
//'   - Memory-efficient processing for large datasets
//'   - Parallel processing support for improved performance
//'
//' * Mathematical properties:
//'   - Element-wise division: result\[i\] = A\[i\] / B\[i\]
//'   - Division by zero results in infinity (IEEE 754 standard)
//'   - Handles special cases: ±inf, NaN, and subnormal numbers
//'   - Order matters: \eqn{A / B \neq B / A}.
//'
//' @param filename String. Path to the HDF5 file containing the datasets.
//' @param group String. Group path containing the first dataset (A, dividend).
//' @param A String. Name of the first dataset (dividend).
//' @param B String. Name of the second dataset (divisor).
//' @param groupB Optional string. Group path containing dataset B.
//'   If NULL, uses same group as A.
//' @param target Optional string. Where to write result: "A", "B", or "new" (default: "new").
//' @param outgroup Optional string. Output group path.
//'   Default is "OUTPUT".
//' @param outdataset Optional string. Output dataset name.
//'   Default is "A_/_B" with .diag suffix if appropriate.
//' @param paral Optional logical. Whether to use parallel processing.
//'   Default is FALSE.
//' @param threads Optional integer. Number of threads for parallel processing.
//'   If NULL, uses maximum available threads.
//' @param overwrite Optional logical. Whether to overwrite existing datasets.
//'   Default is FALSE.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the diagonal division result (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' N <- 1000
//' set.seed(123)
//' A <- matrix(rnorm(N*N), N, N)
//' B <- matrix(rnorm(N*N, mean=1), N, N)  # Avoid division by zero
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", A, "data", "matrixA",
//'                      overwriteFile = TRUE)
//' bdCreate_hdf5_matrix("test.hdf5", B, "data", "matrixB",
//'                      overwriteFile = FALSE)
//' 
//' # Divide diagonals
//' result <- bdDiag_divide_hdf5("test.hdf5", "data", "matrixA", "matrixB",
//'                             outgroup = "results",
//'                             outdataset = "diagonal_ratio",
//'                             paral = TRUE)
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdDiag_divide_hdf5(std::string filename, std::string group, std::string A,
                              std::string B,
                              Rcpp::Nullable<std::string> groupB = R_NilValue,
                              Rcpp::Nullable<std::string> target = R_NilValue,
                              Rcpp::Nullable<std::string> outgroup = R_NilValue,
                              Rcpp::Nullable<std::string> outdataset = R_NilValue,
                              Rcpp::Nullable<bool> paral = R_NilValue,
                              Rcpp::Nullable<int> threads = R_NilValue,
                              Rcpp::Nullable<bool> overwrite = R_NilValue)
{
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsResult = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try {
        H5::Exception::dontPrint();
        
        bool bparal, bforce;
        std::string strsubgroupOut, strdatasetOut, strsubgroupInB, strtarget;
        
        // Parse parameters
        if (paral.isNull()) { bparal = false; } 
        else { bparal = Rcpp::as<bool>(paral); }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool>(overwrite); }
        
        if (target.isNull()) { strtarget = "new"; }
        else { strtarget = Rcpp::as<std::string>(target); }
        
        // Validate target values
        std::string strtargetChk = strtarget;
        std::transform(strtargetChk.begin(), strtargetChk.end(), strtargetChk.begin(), ::tolower);
        if (strtargetChk != "new" && strtargetChk != "a" && strtargetChk != "b") {
            Rcpp::stop("Invalid target value: '%s'. Must be 'new', 'A', or 'B'", strtarget.c_str());
        }
        
        if (groupB.isNotNull()) { strsubgroupInB = Rcpp::as<std::string>(groupB); } 
        else { strsubgroupInB = group; }
        
        if (outgroup.isNull()) { 
            if( strtarget =="A") {
                strsubgroupOut = "group";
            } else if( strtarget =="B") {
                strsubgroupOut = strsubgroupInB;
            } else {
                strsubgroupOut = "OUTPUT"; 
            }    
        } else { strsubgroupOut = Rcpp::as<std::string>(outgroup); }
        
        if (outdataset.isNotNull()) { strdatasetOut = Rcpp::as<std::string>(outdataset); } 
        else { strdatasetOut = A + "_/_" + B + ".diag"; }
        
        // Open datasets
        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, A, false);
        dsA->openDataset();
        
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, false);
        dsB->openDataset();
        
        if (dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
            
            lst_return["fn"] = filename;
            
            // Only create result dataset if target="new"
            if (strtarget == "new") {
                dsResult = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
                lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
            } else if (strtarget == "A") {
                lst_return["ds"] = group + "/" + A;
            } else if (strtarget == "B") {
                lst_return["ds"] = strsubgroupInB + "/" + B;
            }
            
            // Perform diagonal division using namespace function
            BigDataStatMeth::DiagonalOps::divideDiagonals(dsA, dsB, dsResult, strtarget, bparal, threads);
        }
        
        delete dsA; dsA = nullptr;
        delete dsB; dsB = nullptr;
        if (dsResult) { delete dsResult; dsResult = nullptr; }
        
    } catch(H5::FileIException& error) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_divide_hdf5 (File IException)");
    } catch(H5::DataSetIException& error) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_divide_hdf5 (DataSet IException)");
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("c++ exception bdDiag_divide_hdf5: %s", ex.what());
    } catch(...) {
        checkClose_file(dsA, dsB, dsResult);
        Rcpp::stop("C++ exception bdDiag_divide_hdf5 (unknown reason)");
    }
    
    return lst_return;
}


//' Apply Scalar Operations to Diagonal Elements
//'
//' @description
//' Performs optimized scalar operations on diagonal elements of matrices or vectors
//' stored in HDF5 format. Automatically detects whether input is a matrix (extracts
//' diagonal) or vector (direct operation) and applies the specified scalar operation.
//'
//' @details
//' This function provides flexible scalar operations on diagonals:
//' 
//' * Supported operations:
//'   - "+": diagonal\[i\] + scalar
//'   - "-": diagonal\[i\] - scalar  
//'   - "*": diagonal\[i\] * scalar
//'   - "/": diagonal\[i\] / scalar
//'   - "pow": diagonal\[i\] ^ scalar
//' 
//' * Input types:
//'   - Matrix input: Extracts diagonal automatically
//'   - Vector input: Operates directly (most efficient)
//' 
//' * Target options:
//'   - "input": Modifies original dataset in-place
//'   - "new": Creates new dataset with result
//'
//' @param filename String. Path to the HDF5 file containing the dataset.
//' @param group String. Group path containing the input dataset.
//' @param dataset String. Name of the input dataset (matrix or vector).
//' @param scalar Numeric. Scalar value for the operation.
//' @param operation String. Operation to perform: "add", "subtract", "multiply", "divide".
//' @param target Optional string. Where to write result: "input" or "new" (default: "new").
//' @param paral Optional logical. Whether to use parallel processing (default: FALSE).
//' @param threads Optional integer. Number of threads for parallel processing.
//' @param outgroup Optional string. Output group path (only used if target="new").
//' @param outdataset Optional string. Output dataset name (only used if target="new").
//' @param overwrite Optional logical. Whether to overwrite existing datasets (default: FALSE).
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{gr}{Character string with the HDF5 group}
//'   \item{ds}{Character string with the full dataset path (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrix
//' A <- matrix(rnorm(100), 10, 10)
//' bdCreate_hdf5_matrix("test.h5", A, "data", "matrix_A", overwriteFile = TRUE)
//' 
//' # Add scalar to diagonal (creates new dataset)
//' result <- bdDiag_scalar_hdf5("test.h5", "data", "matrix_A",
//'                             scalar = 5.0, operation = "+",
//'                             target = "new", outdataset = "diag_plus_5")
//' 
//' # Multiply diagonal in-place
//' result2 <- bdDiag_scalar_hdf5("test.h5", "data", "matrix_A", 
//'                              scalar = 2.0, operation = "*",
//'                              target = "input")
//' }
//'
//' @export
 // [[Rcpp::export]]
 Rcpp::List bdDiag_scalar_hdf5(std::string filename,
                               std::string group,
                               std::string dataset,
                               double scalar,
                               std::string operation,
                               Rcpp::Nullable<std::string> target = R_NilValue,
                               Rcpp::Nullable<bool> paral = R_NilValue,
                               Rcpp::Nullable<int> threads = R_NilValue,
                               Rcpp::Nullable<std::string> outgroup = R_NilValue,
                               Rcpp::Nullable<std::string> outdataset = R_NilValue,
                               Rcpp::Nullable<bool> overwrite = R_NilValue)
 {
     BigDataStatMeth::hdf5Dataset* dsInput = nullptr;
     BigDataStatMeth::hdf5Dataset* dsResult = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("gr") = "",
                                                Rcpp::Named("ds") = "");
     
     try {
         H5::Exception::dontPrint();
         
         bool bparal, bforce;
         std::string strsubgroupOut, strdatasetOut, strtarget;
         
         // Parse parameters
         if (paral.isNull()) { bparal = false; } 
         else { bparal = Rcpp::as<bool>(paral); }
         
         if (overwrite.isNull()) { bforce = false; } 
         else { bforce = Rcpp::as<bool>(overwrite); }
         
         if (target.isNull()) { strtarget = "new"; }
         else { strtarget = Rcpp::as<std::string>(target); }
         
         // Validate parameters
         if (strtarget != "new" && strtarget != "input") {
             Rcpp::stop("Invalid target value: '%s'. Must be 'new' or 'input'", strtarget.c_str());
         }
         
         if (operation != "+" && operation != "-" && 
             operation != "*" && operation != "/" && operation != "pow") {
             Rcpp::stop("Invalid operation: '%s'. Must be '+', '-', '*', '/' or 'pow'", operation.c_str());
         }
         
         if (outgroup.isNull()) { strsubgroupOut = "OUTPUT"; }
         else { strsubgroupOut = Rcpp::as<std::string>(outgroup); }
         
         if (outdataset.isNotNull()) { strdatasetOut = Rcpp::as<std::string>(outdataset); } 
         else { strdatasetOut = dataset + "_" + operation + "_" + std::to_string(scalar) + ".diag"; }
         
         // Open input dataset
         dsInput = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
         dsInput->openDataset();
         
         if (dsInput->getDatasetptr() != nullptr) {
             
             lst_return["fn"] = filename;
             lst_return["gr"] = strsubgroupOut;
             
             // Only create result dataset if target="new"
             if (strtarget == "new") {
                 dsResult = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
                 lst_return["ds"] = strdatasetOut;
             } else if (strtarget == "input") {
                 lst_return["ds"] = dataset;
             }
             
             // Perform scalar operation
             
             // Rcpp_diagonal_scalar_hdf5(dsInput, dsResult, scalar, operation, strtarget, bparal, threads);
             
             int op_code;
             if (operation == "+") op_code = 0;
             else if (operation == "-") op_code = 1;
             else if (operation == "*") op_code = 2;
             else if (operation == "/") op_code = 3;
             else if (operation == "pow") op_code = 4;
             else {
                 checkClose_file(dsInput, dsResult);
                 Rcpp::stop("Invalid operation: %s. Must be 'add', 'subtract', 'multiply', or 'divide'", operation.c_str());
             }
             
             BigDataStatMeth::DiagonalOps::scalarOperation(dsInput, dsResult, scalar, op_code, strtarget, bparal, threads);
             
         }
         
         delete dsInput; dsInput = nullptr;
         if (dsResult) { delete dsResult; dsResult = nullptr; }
         
     } catch(H5::FileIException& error) {
         checkClose_file(dsInput, dsResult);
         Rcpp::stop("c++ exception bdDiag_scalar_hdf5 (File IException)");
     } catch(H5::DataSetIException& error) {
         checkClose_file(dsInput, dsResult);
         Rcpp::stop("c++ exception bdDiag_scalar_hdf5 (DataSet IException)");
     } catch(std::exception& ex) {
         checkClose_file(dsInput, dsResult);
         Rcpp::stop("c++ exception bdDiag_scalar_hdf5: %s", ex.what());
     } catch(...) {
         checkClose_file(dsInput, dsResult);
         
         Rcpp::stop("C++ exception bdDiag_scalar_hdf5 (unknown reason)");
     }
     
     return lst_return;
 }
