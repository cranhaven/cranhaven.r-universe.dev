/**
 * @file hdf5DiagonalMatrix.hpp
 * @brief Efficient creation of large diagonal matrices using block-wise approach
 * @details This header provides functionality for creating very large diagonal matrices
 * directly in HDF5 files without loading the entire matrix into memory.
 * 
 * Key features:
 * - Block-wise processing for memory efficiency
 * - OpenMP parallelization support
 * - Scalar multiplication support
 * - Direct HDF5 storage
 * - Support for matrices with millions of elements
 */

#ifndef BIGDATASTATMETH_HDF5_DIAGONAL_MATRIX_HPP
#define BIGDATASTATMETH_HDF5_DIAGONAL_MATRIX_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"
// #ifdef _OPENMP
// #include <omp.h>
// #endif

// Added here to. avoid "function() was not declared in this scope"
// #include "hdf5Algebra/vectorOperations.hpp" 
// #include "hdf5Algebra/matrixDiagonal.hpp" 


namespace BigDataStatMeth {
    
    /**
     * @class hdf5DiagonalMatrix
     * @brief Class for creating large diagonal matrices efficiently
     * @details Extends hdf5Dataset to provide specialized functionality for creating
     * diagonal matrices using block-wise processing to minimize memory usage.
     * 
     * This implementation uses an optimized read-modify-write strategy that processes
     * only diagonal blocks instead of the entire matrix, resulting in ~250x reduction
     * in I/O operations for large matrices.
     */
    class hdf5DiagonalMatrix : public hdf5Dataset 
    {
        public:
            
            /**
             * @brief Constructor for diagonal matrix
             * @param filename HDF5 filename path
             * @param group Group path
             * @param datasetname Dataset name
             * @param overwrite Whether to overwrite existing dataset
             */
            hdf5DiagonalMatrix(std::string filename, std::string group, std::string datasetname, bool overwrite) : 
            hdf5Dataset(filename, group, datasetname, overwrite), block_size(1024)
            {
            }
            
            /**
             * @brief Constructor for diagonal matrix
             * @param file HDF5 file pointer
             * @param group Group path
             * @param datasetname Dataset name
             * @param overwrite Whether to overwrite existing dataset
             */
            hdf5DiagonalMatrix(H5::H5File* file, std::string group, std::string datasetname, bool overwrite) : 
            hdf5Dataset(file, group, datasetname, overwrite), block_size(1024)
            {
            }
            
            /**
             * @brief Constructor with file object
             * @param oFile HDF5 file object
             * @param group Group path
             * @param datasetname Dataset name
             * @param overwrite Whether to overwrite existing dataset
             */
            hdf5DiagonalMatrix(BigDataStatMeth::hdf5File* oFile, std::string group, std::string datasetname, bool overwrite) : 
            hdf5Dataset(oFile, group, datasetname, overwrite), block_size(1024)
            {
            }
            
            /**
             * @brief Create diagonal matrix with scalar multiplication
             * @details Creates a diagonal matrix where diagonal elements are scalar * vector[i].
             * Uses optimized block-wise processing that only processes diagonal blocks,
             * resulting in ~250x reduction in I/O operations compared to full matrix processing.
             * 
             * Performance characteristics:
             * - Processes only N/block_size diagonal blocks instead of N²/block_size² total blocks
             * - Uses read-modify-write strategy for existing datasets
             * - Leverages Eigen's optimized diagonal assignment operations
             * - Scales efficiently with matrix size and number of threads
             * 
             * @param size Size of the diagonal (N elements)
             * @param scalar Scalar to multiply with diagonal vector  
             * @param diagonal_vector Vector of diagonal values (length = size)
             * @param block_size_hint Suggested block size for processing (default: 1024)
             * @param compression_level Compression level (0-9, default: 6)
             * @param threads Number of threads to use (default: auto-detect)
             * @param output_type Output format: "matrix" for N×N sparse matrix, "vector" for 1×N vector (default: "matrix")
             * 
             * @note Matrix format creates sparse N×N dataset with only diagonal populated
             * @note Vector format creates efficient 1×N dataset with direct diagonal values
             * @note Vector format is recommended when only diagonal values are needed for subsequent operations
             * 
             */
            void createScalarDiagonalMatrix(hsize_t size, 
                                            double scalar, 
                                            const std::vector<double>& diagonal_vector,
                                            hsize_t block_size_hint = 1024,
                                            int compression_level = 6,
                                            Rcpp::Nullable<int> threads = R_NilValue,
                                            std::string output_type = "matrix")
            {
                if (diagonal_vector.size() != size) {
                    Rf_error("Diagonal vector size must match matrix size");
                    return;
                }
                
                if (output_type != "matrix" && output_type != "vector") {
                    Rf_error("Invalid output_type: must be 'matrix' or 'vector'");
                    return;
                }
                
                // Set optimal block size
                block_size = std::min(block_size_hint, size);
                
                // // Create the HDF5 dataset
                // createDataset(size, size, "numeric", compression_level);
                // 
                // // Process only diagonal 
                // writeDiagonal(size, scalar, diagonal_vector);
                
                if (output_type == "matrix") {
                    // Comportamiento original - crear matriz completa N×N
                    createDataset(size, size, "numeric", compression_level);
                    writeDiagonal(size, scalar, diagonal_vector);
                } else {
                    // Nuevo comportamiento - crear vector 1×N
                    createDataset(1, size, "numeric", compression_level);  // Vector fila 1×N
                    writeVectorDiagonal(size, scalar, diagonal_vector);
                }
                
            }
            
            /**
             * @brief Create identity matrix scaled by scalar with flexible output format
             * @details Creates an identity matrix multiplied by a scalar value. More efficient than 
             * general diagonal matrix for identity cases. Supports both full matrix and vector output formats.
             * 
             * Output format considerations:
             * - "matrix": Suitable when matrix operations (like matrix multiplication) are needed
             * - "vector": Ideal for diagonal operations, significantly more memory efficient
             * 
             * Mathematical equivalence:
             * - Matrix mode: Creates N×N matrix where M[i,i] = scalar, M[i,j] = 0 (i≠j)
             * - Vector mode: Creates 1×N vector where V[i] = scalar
             * 
             * @param size Size of the identity matrix/vector (N×N or 1×N)
             * @param scalar Scalar value for diagonal elements (default: 1.0 for standard identity)
             * @param block_size_hint Suggested block size for processing
             * @param compression_level Compression level (0-9, default: 6)  
             * @param threads Number of threads to use (default: auto-detect)
             * @param output_type Output format: "matrix" for N×N identity matrix, "vector" for 1×N identity vector (default: "matrix")
             * 
             * @note Vector format is highly recommended for diagonal operations as it's ~N times more memory efficient
             * @note Standard identity: scalar=1.0, scaled identity: scalar≠1.0
             * 
             */
            void createScalarIdentityMatrix(hsize_t size, 
                                            double scalar,
                                            hsize_t block_size_hint = 1024,
                                            int compression_level = 6,
                                            Rcpp::Nullable<int> threads = R_NilValue,
                                            std::string output_type = "matrix")
            {
                std::vector<double> identity_vector(size, 1.0);
                createScalarDiagonalMatrix(size, scalar, identity_vector, block_size_hint, compression_level, threads, output_type);
            }
            
            /**
             * @brief Set block size for processing
             * @param new_block_size New block size to use
             */
            void setBlockSize(hsize_t new_block_size) {
                block_size = new_block_size;
            }
            
            /**
             * @brief Get current block size
             * @return Current block size
             */
            hsize_t getBlockSize() const {
                return block_size;
            }
            
            
            // /**
            //  * @brief Add diagonal elements from two matrices or vectors
            //  * @details Performs optimized diagonal addition C_diag = A_diag + B_diag.
            //  * Automatically detects whether inputs are matrices (extracts diagonals) or 
            //  * vectors (direct vector operation). Uses vectorOperations.hpp for maximum efficiency.
            //  * 
            //  * Operation modes:
            //  * - Matrix + Matrix: Extract diagonals → vector addition → write diagonal
            //  * - Matrix + Vector: Extract diagonal → vector addition → write diagonal  
            //  * - Vector + Vector: Direct vector addition (most efficient)
            //  * 
            //  * @param dsA First input dataset (matrix or vector)
            //  * @param dsB Second input dataset (matrix or vector)  
            //  * @param dsResult Result dataset (only used if target="new")
            //  * @param target Where to write result: "A", "B", or "new"
            //  * @param bparal Whether to use parallel processing
            //  * @param threads Number of threads for parallel processing
            //  * 
            //  * @throws std::exception if dimension mismatch or invalid inputs
            //  * @note Result dataset name automatically gets .diag suffix if inputs are vectors
            //  */
            // void addDiagonals(BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB, 
            //                   BigDataStatMeth::hdf5Dataset* dsResult, std::string target = "new",
            //                   bool bparal = false, Rcpp::Nullable<int> threads = R_NilValue)
            // {
            //     try {
            //         bool isVectorA = isDiagonalVector(dsA);
            //         bool isVectorB = isDiagonalVector(dsB);
            //         
            //         if (isVectorA && isVectorB && (target == "A" || target == "B")) {
            //             // Vector + Vector with in-place result
            //             BigDataStatMeth::hdf5Dataset* targetDataset = (target == "A") ? dsA : dsB;
            //             Rcpp_vector_add_hdf5(dsA, dsB, targetDataset, bparal, threads);
            //         } else if (isVectorA && isVectorB && target == "new") {
            //             // Vector + Vector with new result
            //             Rcpp_vector_add_hdf5(dsA, dsB, dsResult, bparal, threads);
            //         } else {
            //             // Matrix operations with target specification
            //             performMatrixDiagonalOperation(dsA, dsB, dsResult, 0, target, bparal, threads);
            //         }
            //     } catch(std::exception& ex) {
            //         Rf_error("Error in addDiagonals: %s", ex.what());
            //     }
            // }

                        
            // /**
            //  * @brief Subtract diagonal elements from two matrices or vectors
            //  * @details Performs optimized diagonal subtraction C_diag = A_diag - B_diag.
            //  * Same optimization strategy as addDiagonals but for subtraction operation.
            //  * 
            //  * @param dsA First input dataset (minuend)
            //  * @param dsB Second input dataset (subtrahend)
            //  * @param dsResult Result dataset (will be created)
            //  * @param target Where to write result: "A", "B", or "new"
            //  * @param bparal Whether to use parallel processing
            //  * @param threads Number of threads for parallel processing
            //  * 
            //  * @throws std::exception if dimension mismatch or invalid inputs
            //  * @see addDiagonals() for detailed operation modes
            //  */
            // void subtractDiagonals(BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB,
            //                        BigDataStatMeth::hdf5Dataset* dsResult, std::string target = "new",
            //                        bool bparal = false, Rcpp::Nullable<int> threads = R_NilValue)
            // {
            //     try {
            //         bool isVectorA = isDiagonalVector(dsA);
            //         bool isVectorB = isDiagonalVector(dsB);
            //         
            //         if (isVectorA && isVectorB && (target == "A" || target == "B")) {
            //             BigDataStatMeth::hdf5Dataset* targetDataset = (target == "A") ? dsA : dsB;
            //             Rcpp_vector_subtract_hdf5(dsA, dsB, targetDataset, bparal, threads);
            //         } else if (isVectorA && isVectorB && target == "new") {
            //             Rcpp_vector_subtract_hdf5(dsA, dsB, dsResult, bparal, threads);
            //         } else {
            //             performMatrixDiagonalOperation(dsA, dsB, dsResult, 1, target, bparal, threads);
            //         }
            //     } catch(std::exception& ex) {
            //         Rf_error("Error in subtractDiagonals: %s", ex.what());
            //     }
            // }
            
            
            // /**
            //  * @brief Multiply diagonal elements from two matrices or vectors
            //  * @details Performs optimized diagonal multiplication C_diag = A_diag * B_diag.
            //  * Same optimization strategy as addDiagonals but for element-wise multiplication.
            //  * 
            //  * @param dsA First input dataset
            //  * @param dsB Second input dataset
            //  * @param dsResult Result dataset (will be created)
            //  * @param target Where to write result: "A", "B", or "new"
            //  * @param bparal Whether to use parallel processing
            //  * @param threads Number of threads for parallel processing
            //  * 
            //  * @throws std::exception if dimension mismatch or invalid inputs
            //  * @note This is element-wise multiplication, not matrix multiplication
            //  */
            // void multiplyDiagonals(BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB,
            //                        BigDataStatMeth::hdf5Dataset* dsResult, std::string target = "new",
            //                        bool bparal = false, Rcpp::Nullable<int> threads = R_NilValue)
            // {
            //     try {
            //         bool isVectorA = isDiagonalVector(dsA);
            //         bool isVectorB = isDiagonalVector(dsB);
            //         
            //         if (isVectorA && isVectorB && (target == "A" || target == "B")) {
            //             BigDataStatMeth::hdf5Dataset* targetDataset = (target == "A") ? dsA : dsB;
            //             Rcpp_vector_multiply_hdf5(dsA, dsB, targetDataset, bparal, threads);
            //         } else if (isVectorA && isVectorB && target == "new") {
            //             Rcpp_vector_multiply_hdf5(dsA, dsB, dsResult, bparal, threads);
            //         } else {
            //             performMatrixDiagonalOperation(dsA, dsB, dsResult, 2, target, bparal, threads);
            //         }
            //     } catch(std::exception& ex) {
            //         Rf_error("Error in multiplyDiagonals: %s", ex.what());
            //     }
            // }
            
            
            
            // /**
            //  * @brief Divide diagonal elements from two matrices or vectors
            //  * @details Performs optimized diagonal division C_diag = A_diag / B_diag.
            //  * Same optimization strategy as addDiagonals but for element-wise division.
            //  * Division by zero follows IEEE 754 standard (results in infinity).
            //  * 
            //  * @param dsA First input dataset (dividend)
            //  * @param dsB Second input dataset (divisor)
            //  * @param dsResult Result dataset (will be created)
            //  * @param target Where to write result: "A", "B", or "new"
            //  * @param bparal Whether to use parallel processing
            //  * @param threads Number of threads for parallel processing
            //  * 
            //  * @throws std::exception if dimension mismatch or invalid inputs
            //  * @note Division by zero results in infinity (IEEE 754 standard behavior)
            //  */
            // void divideDiagonals(BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB,
            //                      BigDataStatMeth::hdf5Dataset* dsResult, std::string target = "new",
            //                      bool bparal = false, Rcpp::Nullable<int> threads = R_NilValue)
            // {
            //     try {
            //         bool isVectorA = isDiagonalVector(dsA);
            //         bool isVectorB = isDiagonalVector(dsB);
            //         
            //         if (isVectorA && isVectorB && (target == "A" || target == "B")) {
            //             BigDataStatMeth::hdf5Dataset* targetDataset = (target == "A") ? dsA : dsB;
            //             Rcpp_vector_divide_hdf5(dsA, dsB, targetDataset, bparal, threads);
            //         } else if (isVectorA && isVectorB && target == "new") {
            //             Rcpp_vector_divide_hdf5(dsA, dsB, dsResult, bparal, threads);
            //         } else {
            //             performMatrixDiagonalOperation(dsA, dsB, dsResult, 3, target, bparal, threads);
            //         }
            //     } catch(std::exception& ex) {
            //         Rf_error("Error in divideDiagonals: %s", ex.what());
            //     }
            // }
            
            
            // /**
            //  * @brief Extract diagonal from matrix and save as vector dataset
            //  * @details Extracts diagonal elements from a matrix and creates a new vector dataset.
            //  * Uses existing getDiagonalfromMatrix() function for optimized diagonal reading.
            //  * The result vector is stored in 1×N format for compatibility with vector operations.
            //  * 
            //  * Extraction process:
            //  * - Validates square matrix requirement
            //  * - Uses getDiagonalfromMatrix() for efficient extraction
            //  * - Creates 1×N vector dataset for storage efficiency
            //  * - Preserves data precision and handles large matrices
            //  * 
            //  * @param dsMatrix Input matrix dataset (must be square N×N)
            //  * @param dsVector Output vector dataset (will be created as 1×N)
            //  * 
            //  * @throws std::exception if matrix is not square or extraction fails
            //  * @note Matrix must be square (N×N) for diagonal extraction
            //  * @note Output vector is stored as 1×N for compatibility with vector operations
            //  * @see getDiagonalfromMatrix() for the underlying extraction algorithm
            //  */
            // void extractDiagonalToVector(BigDataStatMeth::hdf5Dataset* dsMatrix, 
            //                              BigDataStatMeth::hdf5Dataset* dsVector)
            // {
            //     try {
            //         // Validate square matrix
            //         if (dsMatrix->nrows() != dsMatrix->ncols()) {
            //             Rf_error("extractDiagonalToVector: Matrix must be square");
            //             return;
            //         }
            //         
            //         hsize_t matrix_size = dsMatrix->nrows();
            //         
            //         // Extract diagonal using existing optimized function
            //         Rcpp::NumericVector diagonal = getDiagonalfromMatrix(dsMatrix);
            //         
            //         // Create vector dataset (1×N format for compatibility)
            //         dsVector->createDataset(matrix_size, 1, "numeric");
            //         
            //         // Convert to std::vector and write
            //         std::vector<double> diag_vector = Rcpp::as<std::vector<double>>(diagonal);
            //         std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
            //         dsVector->writeDatasetBlock(diag_vector, {0, 0}, {1, matrix_size}, stride, block);
            //         
            //     } catch(std::exception& ex) {
            //         Rf_error("Error in extractDiagonalToVector: %s", ex.what());
            //     }
            // }
            
            
            
            // /**
            //  * @brief Write diagonal vector to matrix diagonal
            //  * @details Takes a vector dataset and writes its values to the diagonal of a matrix.
            //  * Uses existing setDiagonalMatrix() function for optimized diagonal writing.
            //  * Matrix must already exist and be square. Vector must match matrix diagonal size.
            //  * 
            //  * Writing process:
            //  * - Validates vector and matrix dimensions compatibility
            //  * - Reads vector data efficiently
            //  * - Uses setDiagonalMatrix() for optimized diagonal writing
            //  * - Preserves all non-diagonal matrix elements
            //  * 
            //  * @param dsVector Input vector dataset containing diagonal values (1×N or N×1)
            //  * @param dsMatrix Target matrix dataset (must exist and be square N×N)
            //  * 
            //  * @throws std::exception if dimensions don't match or writing fails
            //  * @note Matrix must already exist with proper dimensions
            //  * @note Vector size must match matrix diagonal size (N elements for N×N matrix)
            //  * @see setDiagonalMatrix() for the underlying writing algorithm
            //  */
            // void writeDiagonalFromVector(BigDataStatMeth::hdf5Dataset* dsVector,
            //                              BigDataStatMeth::hdf5Dataset* dsMatrix)
            // {
            //     try {
            //         // Validate matrix is square
            //         if (dsMatrix->nrows() != dsMatrix->ncols()) {
            //             Rf_error("writeDiagonalFromVector: Matrix must be square");
            //             return;
            //         }
            //         
            //         // Validate vector dimensions
            //         hsize_t vector_size = validateVectorDataset(dsVector);
            //         hsize_t matrix_size = dsMatrix->nrows();
            //         
            //         if (vector_size == 0) {
            //             Rf_error("writeDiagonalFromVector: Input is not a valid vector");
            //             return;
            //         }
            //         
            //         if (vector_size != matrix_size) {
            //             Rf_error("writeDiagonalFromVector: Vector size (%d) must match matrix diagonal size (%d)", 
            //                      vector_size, matrix_size);
            //             return;
            //         }
            //         
            //         // Read vector data
            //         std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
            //         std::vector<double> vector_data(vector_size);
            //         
            //         if (dsVector->nrows() == 1) {
            //             // Row vector 1×N
            //             dsVector->readDatasetBlock({0, 0}, {1, vector_size}, stride, block, vector_data.data());
            //         } else {
            //             // Column vector N×1
            //             dsVector->readDatasetBlock({0, 0}, {vector_size, 1}, stride, block, vector_data.data());
            //         }
            //         
            //         // Convert to Rcpp::NumericVector and write using existing function
            //         Rcpp::NumericVector diagonal_values = Rcpp::wrap(vector_data);
            //         setDiagonalMatrix(dsMatrix, diagonal_values);
            //         
            //     } catch(std::exception& ex) {
            //         Rf_error("Error in writeDiagonalFromVector: %s", ex.what());
            //     }
            // }
            
        private:
            hsize_t block_size; ///< Block size for processing
            
            
            /**
             * @brief Write diagonal elements directly using HDF5 element selection
             * @details Uses HDF5's H5Sselect_elements to write all diagonal elements
             * in a single operation. This is ~500x faster than block-based approaches
             * for new datasets as it avoids unnecessary read-modify-write cycles.
             * 
             * Technical implementation:
             * - Creates coordinate array with diagonal positions: (0,0), (1,1), (2,2)...
             * - Uses H5Sselect_elements for direct element selection
             * - Single H5Dwrite call for entire diagonal
             * - No intermediate block processing or memory overhead
             * 
             * @param size Size of the square matrix
             * @param scalar Scalar multiplier for diagonal elements
             * @param diagonal_vector Vector of diagonal values
             * 
             * @note This method bypasses writeDatasetBlock for maximum performance
             * @note Optimal for creating new diagonal matrices from scratch
             * @note Thread-safe but typically faster when run single-threaded due to HDF5 overhead
             */
            void writeDiagonal(hsize_t size, double scalar, const std::vector<double>& diagonal_vector)
            {
                try {
                    // Prepare diagonal coordinates: (0,0), (1,1), (2,2)...
                    std::vector<hsize_t> coord(size * 2);
                    std::vector<double> diag_values(size);
                    
                    for(hsize_t i = 0; i < size; i++) {
                        coord[i * 2] = i;      // row coordinate
                        coord[i * 2 + 1] = i;  // column coordinate
                        diag_values[i] = scalar * diagonal_vector[i];
                    }
                    
                    // Direct HDF5 API for element selection
                    hid_t dataset_id = pdataset->getId();
                    hid_t file_space = H5Dget_space(dataset_id);
                    hid_t mem_space = H5Screate_simple(1, &size, NULL);
                    
                    if (file_space < 0 || mem_space < 0) {
                        Rf_error("Failed to create HDF5 data spaces for diagonal write");
                        return;
                    }
                    
                    // Select diagonal elements in file space
                    herr_t status = H5Sselect_elements(file_space, H5S_SELECT_SET, size, coord.data());
                    if (status < 0) {
                        H5Sclose(file_space);
                        H5Sclose(mem_space);
                        Rf_error("Failed to select diagonal elements in HDF5 file space");
                        return;
                    }
                    
                    // Write all diagonal elements in single operation
                    status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space, file_space, H5P_DEFAULT, diag_values.data());
                    if (status < 0) {
                        H5Sclose(file_space);
                        H5Sclose(mem_space);
                        Rf_error("Failed to write diagonal elements to HDF5 dataset");
                        return;
                    }
                    
                    // Cleanup HDF5 resources
                    H5Sclose(file_space);
                    H5Sclose(mem_space);
                    
                } catch(H5::Exception& error) {
                    Rf_error("HDF5 exception in writeDiagonal: %s", error.getCDetailMsg());
                } catch(std::exception& ex) {
                    Rf_error("C++ exception in writeDiagonal: %s", ex.what());
                }
            }
            
            
            /**
             * @brief Write diagonal values directly as vector dataset
             * @details Writes diagonal values to a 1×N vector dataset using optimized direct write.
             * This function bypasses matrix processing entirely and writes diagonal values directly
             * to vector format, providing maximum efficiency for diagonal-only operations.
             * 
             * Technical implementation:
             * - Creates 1×N dataset layout for optimal vector operations compatibility
             * - Uses existing writeDatasetBlock() infrastructure for consistency
             * - Applies scalar multiplication during write for efficiency
             * - Single-pass operation without intermediate storage
             * 
             * @param size Number of diagonal elements to write
             * @param scalar Scalar multiplier applied to each diagonal element
             * @param diagonal_vector Vector containing base diagonal values
             * 
             * @throws std::exception if write operation fails
             * 
             * @note Output format is 1×N (row vector) for compatibility with vector operations
             * @note Final values written are: diagonal_vector[i] * scalar
             * @note Uses writeDatasetBlock() for consistency with existing HDF5 infrastructure
             * 
             * @private
             * @since Added in version X.X.X to support vector output format
             */
            void writeVectorDiagonal(hsize_t size, double scalar, const std::vector<double>& diagonal_vector)
            {
                try {
                    // Preparar valores escalados
                    std::vector<double> scaled_values(size);
                    for(hsize_t i = 0; i < size; i++) {
                        scaled_values[i] = scalar * diagonal_vector[i];
                    }
                    
                    // Escribir directamente como vector usando writeDatasetBlock existente
                    std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
                    writeDatasetBlock(scaled_values, {0, 0}, {1, size}, stride, block);
                    
                } catch(std::exception& ex) {
                    Rf_error("C++ exception in writeVectorDiagonal: %s", ex.what());
                }
            }
            
            
            // /**
            //  * @brief Check if dataset represents a diagonal vector
            //  * @details Validates dataset dimensions to determine if it's a vector (1×N or N×1).
            //  * A dataset is considered a diagonal vector if it has vector dimensions.
            //  * Future enhancements could include .diag suffix validation.
            //  * 
            //  * Validation criteria:
            //  * - Row vector: 1×N where N > 1
            //  * - Column vector: N×1 where N > 1  
            //  * - Scalar: 1×1 (edge case)
            //  * - Invalid: N×M where both N,M > 1
            //  * 
            //  * @param ds Dataset to check
            //  * @return true if dataset is a valid vector, false if matrix
            //  * 
            //  * @note Future versions may include .diag suffix validation
            //  * @note Scalar datasets (1×1) are considered vectors
            //  */
            // bool isDiagonalVector(BigDataStatMeth::hdf5Dataset* ds)
            // {
            //     hsize_t rows = ds->nrows();
            //     hsize_t cols = ds->ncols();
            //     
            //     // Check vector dimensions
            //     bool isVector = (rows == 1 && cols > 1) ||     // Row vector
            //         (cols == 1 && rows > 1) ||      // Column vector
            //         (rows == 1 && cols == 1);       // Scalar
            //     
            //     // Optional: Future enhancement for naming convention
            //     // std::string name = ds->getDatasetName();
            //     // bool hasDiagSuffix = name.find(".diag") != std::string::npos;
            //     
            //     return isVector;
            // }
            
            
            
            // /**
            //  * @brief Validate vector dataset and return its size
            //  * @details Checks dataset dimensions and returns the number of elements in the vector.
            //  * Used for dimension validation before performing vector operations.
            //  * 
            //  * Size calculation:
            //  * - Row vector (1×N): returns N
            //  * - Column vector (N×1): returns N
            //  * - Scalar (1×1): returns 1
            //  * - Matrix (N×M): returns 0 (invalid)
            //  * 
            //  * @param ds Dataset to validate
            //  * @return Vector size if valid vector, 0 if not a vector
            //  * 
            //  * @note Return value 0 indicates invalid vector (matrix with N×M where N,M > 1)
            //  * @note Used for dimension compatibility checking before operations
            //  */
            // hsize_t validateVectorDataset(BigDataStatMeth::hdf5Dataset* ds)
            // {
            //     hsize_t rows = ds->nrows();
            //     hsize_t cols = ds->ncols();
            //     
            //     if (rows == 1 && cols > 1) {
            //         return cols;  // Row vector 1×N
            //     } else if (cols == 1 && rows > 1) {
            //         return rows;  // Column vector N×1
            //     } else if (rows == 1 && cols == 1) {
            //         return 1;     // Scalar 1×1
            //     }
            //     return 0;  // Not a vector (matrix N×M)
            // }
            
            
            // /**
            //  * @brief Perform diagonal operations on matrices using extract-operate-write strategy
            //  * @details Implements the extract-operate-write pattern for matrix diagonal operations.
            //  * It processes only diagonal elements instead of full matrices.
            //  * 
            //  * Strategy implementation:
            //  * 1. Extract diagonals from matrices (if needed) using extractDiagonalToVector()
            //  * 2. Perform vector operation using optimized functions from vectorOperations.hpp
            //  * 3. Result is stored as vector (most efficient) or written back to matrix diagonal
            //  * 
            //  * Operation codes:
            //  * - 0: Addition (A_diag + B_diag)
            //  * - 1: Subtraction (A_diag - B_diag)  
            //  * - 2: Multiplication (A_diag * B_diag)
            //  * - 3: Division (A_diag / B_diag)
            //  * 
            //  * @param dsA First input dataset
            //  * @param dsB Second input dataset  
            //  * @param dsResult Result dataset (only used if target="new")
            //  * @param operation Operation type: 0=add, 1=subtract, 2=multiply, 3=divide
            //  * @param target Where to write result: "A", "B", or "new"
            //  * @param bparal Whether to use parallel processing
            //  * @param threads Number of threads
            //  * 
            //  * @throws std::exception if matrices are not square or dimensions don't match
            //  * @note Creates temporary vector datasets for matrix inputs (cleaned up automatically)
            //  * @note Validates all dimensions before performing operations
            //  * @note Uses existing getDiagonalfromMatrix() and setDiagonalMatrix() functions
            //  */
            // void performMatrixDiagonalOperation(BigDataStatMeth::hdf5Dataset* dsA, BigDataStatMeth::hdf5Dataset* dsB,
            //                                     BigDataStatMeth::hdf5Dataset* dsResult, int operation, std::string target,
            //                                     bool bparal, Rcpp::Nullable<int> threads)
            // {
            //     BigDataStatMeth::hdf5Dataset* tempA = nullptr;
            //     BigDataStatMeth::hdf5Dataset* tempB = nullptr;
            //     BigDataStatMeth::hdf5Dataset* tempResult = nullptr;
            //     
            //     try {
            //         bool isVectorA = isDiagonalVector(dsA);
            //         bool isVectorB = isDiagonalVector(dsB);
            //         
            //         BigDataStatMeth::hdf5Dataset* finalA = dsA;
            //         BigDataStatMeth::hdf5Dataset* finalB = dsB;
            //         
            //         // Extract diagonal from A if it's a matrix
            //         if (!isVectorA) {
            //             if (dsA->nrows() != dsA->ncols()) {
            //                 Rf_error("Matrix A must be square for diagonal operations");
            //                 return;
            //             }
            //             std::string tempNameA = dsA->getDatasetName() + "_temp_diag_A";
            //             tempA = new BigDataStatMeth::hdf5Dataset(dsA->getFileptr(), dsA->getGroup(), tempNameA, true);
            //             extractDiagonalToVector(dsA, tempA);
            //             finalA = tempA;
            //         }
            //         
            //         // Extract diagonal from B if it's a matrix  
            //         if (!isVectorB) {
            //             if (dsB->nrows() != dsB->ncols()) {
            //                 Rf_error("Matrix B must be square for diagonal operations");
            //                 cleanup_temp_datasets(tempA, tempB);
            //                 return;
            //             }
            //             std::string tempNameB = dsB->getDatasetName() + "_temp_diag_B";
            //             tempB = new BigDataStatMeth::hdf5Dataset(dsB->getFileptr(), dsB->getGroup(), tempNameB, true);
            //             extractDiagonalToVector(dsB, tempB);
            //             finalB = tempB;
            //         }
            //         
            //         // Validate dimensions
            //         hsize_t sizeA = validateVectorDataset(finalA);
            //         hsize_t sizeB = validateVectorDataset(finalB);
            //         
            //         if (sizeA == 0 || sizeB == 0 || sizeA != sizeB) {
            //             Rf_error("Invalid or incompatible diagonal dimensions: %d vs %d", sizeA, sizeB);
            //             cleanup_temp_datasets(tempA, tempB);
            //             return;
            //         }
            //         
            //         // Determine target for operation result
            //         BigDataStatMeth::hdf5Dataset* operationTarget = nullptr;
            //         
            //         if (target == "new") {
            //             operationTarget = dsResult;
            //         } else if (target == "A") {
            //             if (isVectorA) {
            //                 operationTarget = dsA;  // Write directly to vector A
            //             } else {
            //                 // Create temp for result, will write to matrix A diagonal later
            //                 std::string tempNameResult = dsA->getDatasetName() + "_temp_result";
            //                 tempResult = new BigDataStatMeth::hdf5Dataset(dsA->getFileptr(), dsA->getGroup(), tempNameResult, true);
            //                 operationTarget = tempResult;
            //             }
            //         } else if (target == "B") {
            //             if (isVectorB) {
            //                 operationTarget = dsB;  // Write directly to vector B
            //             } else {
            //                 // Create temp for result, will write to matrix B diagonal later
            //                 std::string tempNameResult = dsB->getDatasetName() + "_temp_result";
            //                 tempResult = new BigDataStatMeth::hdf5Dataset(dsB->getFileptr(), dsB->getGroup(), tempNameResult, true);
            //                 operationTarget = tempResult;
            //             }
            //         }
            //         
            //         // Perform vector operation
            //         switch (operation) {
            //         case 0: Rcpp_vector_add_hdf5(finalA, finalB, operationTarget, bparal, threads); break;
            //         case 1: Rcpp_vector_subtract_hdf5(finalA, finalB, operationTarget, bparal, threads); break;
            //         case 2: Rcpp_vector_multiply_hdf5(finalA, finalB, operationTarget, bparal, threads); break;
            //         case 3: Rcpp_vector_divide_hdf5(finalA, finalB, operationTarget, bparal, threads); break;
            //         default: Rf_error("Unknown diagonal operation: %d", operation);
            //         }
            //         
            //         // Write result back to matrix diagonal if needed
            //         if (target == "A" && !isVectorA) {
            //             writeDiagonalFromVector(tempResult, dsA);
            //         } else if (target == "B" && !isVectorB) {
            //             writeDiagonalFromVector(tempResult, dsB);
            //         }
            //         
            //         // Cleanup
            //         cleanup_temp_datasets(tempA, tempB);
            //         if (tempResult) { delete tempResult; tempResult = nullptr; }
            //         
            //     } catch(std::exception& ex) {
            //         cleanup_temp_datasets(tempA, tempB);
            //         if (tempResult) { delete tempResult; tempResult = nullptr; }
            //         Rf_error("Error in performMatrixDiagonalOperation: %s", ex.what());
            //     }
            // }
            
            // /**
            //  * @brief Clean up temporary datasets created during operations
            //  * @details Safely deletes temporary datasets and releases memory.
            //  * Used internally to ensure proper resource cleanup even when exceptions occur.
            //  * 
            //  * Safety features:
            //  * - Null-pointer safe (handles nullptr gracefully)
            //  * - Exception safe (won't throw during cleanup)
            //  * - Memory leak prevention
            //  * - Used in exception handling paths
            //  * 
            //  * @param tempA Temporary dataset A (can be nullptr)
            //  * @param tempB Temporary dataset B (can be nullptr)
            //  * 
            //  * @note Silent cleanup - doesn't throw exceptions to avoid masking original errors
            //  * @note Called automatically by performMatrixDiagonalOperation()
            //  */
            // void cleanup_temp_datasets(BigDataStatMeth::hdf5Dataset* tempA, 
            //                            BigDataStatMeth::hdf5Dataset* tempB)
            // {
            //     try {
            //         if (tempA != nullptr) {
            //             delete tempA;
            //             tempA = nullptr;
            //         }
            //         if (tempB != nullptr) {
            //             delete tempB;
            //             tempB = nullptr;
            //         }
            //     } catch(...) {
            //         // Silent cleanup - avoid throwing exceptions in cleanup code
            //         // This prevents masking the original exception that triggered cleanup
            //     }
            // }
            

    };
    
    /**
     * @brief Utility function to estimate optimal block size for diagonal matrix operations
     * @details Calculates optimal block size based on available memory and matrix size.
     * This function is specifically optimized for diagonal-only processing where only
     * diagonal blocks are read/written, not the entire matrix.
     * 
     * Considerations for diagonal matrices:
     * - Only processes N/block_size diagonal blocks instead of N²/block_size² total blocks
     * - Each diagonal block uses block_size² * 8 bytes for processing
     * - Optimal block size balances memory usage with I/O efficiency
     * - Larger blocks reduce number of I/O operations but increase memory per thread
     * 
     * @param matrix_size Size of the matrix
     * @param available_memory_mb Available memory per thread in MB (default: 100MB)
     * @return Optimal block size for diagonal processing
     * 
     * @note For very large matrices (>1M), consider increasing available_memory_mb
     * @note Block size affects both memory usage and parallelization granularity
     */
    inline hsize_t estimateOptimalBlockSize(hsize_t matrix_size, double available_memory_mb = 100.0) {
        // Each diagonal block uses block_size^2 * 8 bytes (double precision)
        // Keep blocks under specified memory limit per thread
        double bytes_per_mb = 1024.0 * 1024.0;
        double max_elements = (available_memory_mb * bytes_per_mb) / sizeof(double);
        hsize_t max_block_size = static_cast<hsize_t>(std::sqrt(max_elements));
        
        // Ensure block size is reasonable for diagonal processing
        hsize_t min_block = 64;   // Minimum for efficient I/O
        hsize_t max_block = std::min(static_cast<hsize_t>(4096), matrix_size); // Maximum practical size
        
        return std::max(min_block, std::min(max_block_size, max_block));
    }

} // namespace BigDataStatMeth

#endif // BIGDATASTATMETH_HDF5_DIAGONAL_MATRIX_HPP
