/**
 * @file diagonal_matrix_wrapper.cpp
 * @brief R wrapper functions for creating large diagonal matrices
 */

#include "BigDataStatMeth.hpp"

//' Create Diagonal Matrix or Vector in HDF5 File
//'
//' @description
//' Creates a diagonal matrix or vector directly in an HDF5 file using
//' block-wise processing to minimize memory usage. This unified function
//' replaces separate diagonal and identity matrix creation functions,
//' providing flexible diagonal creation with automatic parameter detection.
//'
//' @details
//' This function provides flexible diagonal creation with two main modes:
//' 
//' * Vector mode: Provide custom diagonal values
//'   - Size is automatically detected from vector length
//'   - Scalar acts as additional multiplier
//'   - Ideal for custom diagonal patterns
//' 
//' * Scalar mode: Provide size and scalar value  
//'   - Creates uniform diagonal with specified scalar
//'   - scalar=1.0 creates identity matrix/vector
//'   - Ideal for identity or uniform diagonal matrices
//' 
//' * Output formats:
//'   - "matrix": Creates full N×N matrix (sparse, only diagonal populated)
//'   - "vector": Creates efficient 1×N vector with diagonal values only
//' 
//' * Performance features:
//'   - Block-wise processing for memory efficiency
//'   - Optional compression with configurable levels
//'   - Parallel processing support for large datasets
//'   - Automatic block size optimization
//'
//' @param filename Character. Path to HDF5 file
//' @param group Character. Group path in HDF5 file (default: "/")
//' @param dataset Character. Name of dataset to create
//' @param size Integer. Size of diagonal (auto-detected if diagonal_values provided)
//' @param scalar Numeric. Scalar multiplier for diagonal elements (default: 1.0)
//' @param diagonal_values Numeric vector. Custom diagonal values (optional)
//' @param output_type Character. Output format: "matrix" or "vector" (default: "matrix")
//' @param block_size Integer. Block size for processing (default: auto-estimate)
//' @param compression Integer. Compression level 0-9 (default: 6)
//' @param overwriteFile Logical. Overwrite file if exists (default: FALSE)
//' @param overwriteDataset Logical. Overwrite dataset if exists (default: FALSE)  
//' @param threads Integer. Number of threads to use (default: auto-detect)
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the diagonal matrix (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create identity matrix (1M x 1M)
//' bdCreate_diagonal_hdf5("identity.h5", "/", "I_matrix", 
//'                       size = 1000000, scalar = 1.0)
//' 
//' # Create scaled identity vector (more efficient)
//' bdCreate_diagonal_hdf5("scaled_id.h5", "/", "scaled_I", 
//'                       size = 500000, scalar = 3.14, 
//'                       output_type = "vector")
//' 
//' # Create custom diagonal matrix
//' custom_diag <- runif(10000)
//' bdCreate_diagonal_hdf5("custom.h5", "/", "my_diag",
//'                       diagonal_values = custom_diag,
//'                       scalar = 2.0, output_type = "matrix")
//' 
//' # Create custom diagonal vector (most efficient)
//' bdCreate_diagonal_hdf5("custom_vec.h5", "/", "my_diag_vec",
//'                       diagonal_values = custom_diag,
//'                       output_type = "vector")
//' }
//'
//' @export
 // [[Rcpp::export]]
 Rcpp::List bdCreate_diagonal_hdf5(std::string filename, 
                                   std::string group,
                                   std::string dataset,
                                   Rcpp::Nullable<int> size = R_NilValue,
                                   double scalar = 1.0,
                                   Rcpp::NumericVector diagonal_values = R_NilValue,
                                   std::string output_type = "matrix",
                                   int block_size = 0,
                                   int compression = 6,
                                   Rcpp::Nullable<bool> overwriteFile = R_NilValue,
                                   Rcpp::Nullable<bool> overwriteDataset = R_NilValue,
                                   Rcpp::Nullable<int> threads = R_NilValue) {
     
     BigDataStatMeth::hdf5File* objFile = nullptr;
     BigDataStatMeth::hdf5DiagonalMatrix* dsDiag = nullptr;
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
     try {
         
         H5::Exception::dontPrint();
         
         bool bforceFile, bforceDataset;
         int iRes;
         hsize_t final_size;
         std::vector<double> diag_vec;
         
         // Validate output_type
         if (output_type != "matrix" && output_type != "vector") {
             Rcpp::stop("Invalid output_type: '%s'. Must be 'matrix' or 'vector'", output_type.c_str());
         }
         
         // Auto-detect parameters and validate
         if (diagonal_values.size() > 0) {
             // Vector mode: use provided diagonal values
             final_size = diagonal_values.size();
             diag_vec = Rcpp::as<std::vector<double>>(diagonal_values);
             
             // Check size consistency if both provided
             if (size.isNotNull()) {
                 int provided_size = Rcpp::as<int>(size);
                 if (provided_size != final_size) {
                     Rcpp::stop("Size mismatch: provided size (%d) != diagonal_values length (%d)", 
                                provided_size, final_size);
                 }
             }
         } else if (size.isNotNull()) {
             // Scalar mode: create uniform diagonal
             final_size = Rcpp::as<int>(size);
             if (final_size <= 0) {
                 Rcpp::stop("Size must be positive");
             }
             diag_vec.assign(final_size, 1.0);  // Create identity base
         } else {
             Rcpp::stop("Must provide either 'diagonal_values' or 'size'");
         }
         
         // Parse file parameters
         if(overwriteFile.isNull())  bforceFile = false ;
         else    bforceFile = Rcpp::as<bool>(overwriteFile);
         
         if(overwriteDataset.isNull())  bforceDataset = false ;
         else    bforceDataset = Rcpp::as<bool>(overwriteDataset);
         
         // Estimate optimal block size if not provided
         hsize_t iblock_size = (block_size > 0) ? 
         static_cast<hsize_t>(block_size) : 
             BigDataStatMeth::estimateOptimalBlockSize(final_size);
         
         // Create file
         objFile = new BigDataStatMeth::hdf5File(filename, bforceFile);
         iRes = objFile->createFile();
         
         if( (iRes == EXEC_OK) | (iRes == EXEC_WARNING)) {
             
             if(iRes == EXEC_WARNING) {
                 objFile->openFile("rw");
             }
             
             dsDiag = new BigDataStatMeth::hdf5DiagonalMatrix(objFile->getFileptr(), group, dataset, bforceDataset);
             
             // Create diagonal matrix/vector with unified function
             dsDiag->createScalarDiagonalMatrix(final_size, scalar, diag_vec, 
                                                iblock_size, compression, threads, output_type);
             
             delete dsDiag; dsDiag = nullptr;
             delete objFile; objFile = nullptr;
         }
         
         lst_return["fn"] = filename;
         lst_return["ds"] = group + "/" + dataset;
         
     }  catch (H5::FileIException&) {
         if(objFile != nullptr) delete objFile;
         checkClose_file(dsDiag);
         Rcpp::stop("c++ exception bdCreate_diagonal_hdf5 (File IException)");
         return(lst_return);
         
     } catch (H5::DataSetIException&) {
         if(objFile != nullptr) delete objFile;
         checkClose_file(dsDiag);
         Rcpp::stop("c++ exception bdCreate_diagonal_hdf5 (DataSet IException)");
         return(lst_return);
         
     } catch (std::exception& ex) {
         if(objFile != nullptr) delete objFile;
         checkClose_file(dsDiag);
         Rcpp::stop("c++ exception bdCreate_diagonal_hdf5 %s", ex.what());
         return(lst_return);
     }
     
     return(lst_return);
 }
