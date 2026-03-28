#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/vectormatrix.hpp"

/**
 * @file hdf5_vectormatrix.cpp
 * @brief Implementation of vector-matrix operations for HDF5-stored data
 * @details This file provides functionality for performing element-wise operations
 * between vectors and matrices stored in HDF5 format. It supports:
 * - Element-wise addition, subtraction, multiplication, and division
 * - Operations by rows or columns
 * - Parallel computation capabilities
 * - Memory-efficient processing
 * 
 * The implementation is designed for large-scale data processing with:
 * - Flexible operation modes (row-wise or column-wise)
 * - Error checking and validation
 * - Parallel processing options
 * - Memory-efficient HDF5 I/O
 */

/**
 * @brief Performs element-wise operations between a matrix and a vector
 * 
 * @details This function implements element-wise operations between a matrix and
 * a vector stored in HDF5 format. The operations can be performed row-wise or
 * column-wise, with support for parallel processing.
 * 
 * Supported operations:
 * - Addition (+)
 * - Subtraction (-)
 * - Multiplication (*)
 * - Division (/)
 * 
 * Key features:
 * - Validates input dimensions
 * - Supports parallel processing
 * - Memory-efficient implementation
 * - Flexible output options
 * 
 * @param filename HDF5 file path
 * @param group Group containing the matrix dataset
 * @param dataset Matrix dataset name
 * @param vectorgroup Group containing the vector dataset
 * @param vectordataset Vector dataset name
 * @param outdataset Output dataset name
 * @param func Operation to perform (+, -, *, /)
 * @param outgroup Optional output group
 * @param byrows Whether to apply operation by rows
 * @param paral Whether to use parallel processing
 * @param threads Number of threads for parallel processing
 * @param overwrite Whether to overwrite existing datasets
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Apply Vector Operations to HDF5 Matrix
//'
//' @description
//' Performs element-wise operations between a matrix and a vector stored in HDF5
//' format. The function supports addition, subtraction, multiplication, division and power
//' operations, with options for row-wise or column-wise application and parallel
//' processing.
//'
//' @details
//' This function provides a flexible interface for performing element-wise operations
//' between matrices and vectors stored in HDF5 format. It supports:
//' 
//' * Four basic operations:
//'   - Addition (+): Adds vector elements to matrix rows/columns
//'   - Subtraction (-): Subtracts vector elements from matrix rows/columns
//'   - Multiplication (*): Multiplies matrix rows/columns by vector elements
//'   - Division (/): Divides matrix rows/columns by vector elements
//'   - Power (pow): power matrix rows/columns by vector elements
//' 
//' * Processing options:
//'   - Row-wise or column-wise operations
//'   - Parallel processing for improved performance
//'   - Configurable thread count for parallel execution
//'   - Memory-efficient processing for large datasets
//'
//' The function performs extensive validation:
//' * Checks matrix and vector dimensions for compatibility
//' * Validates operation type
//' * Verifies HDF5 file and dataset accessibility
//' * Ensures proper data structures (matrix vs. vector)
//'
//' @param filename String. Path to the HDF5 file containing the datasets.
//' @param group String. Path to the group containing the matrix dataset.
//' @param dataset String. Name of the matrix dataset.
//' @param vectorgroup String. Path to the group containing the vector dataset.
//' @param vectordataset String. Name of the vector dataset.
//' @param outdataset String. Name for the output dataset.
//' @param func String. Operation to perform: "+", "-", "*", "/", or "pow".
//' @param outgroup Optional string. Output group path. If not provided,
//'   results are stored in the same group as the input matrix.
//' @param byrows Logical. If TRUE, applies operation by rows. If FALSE (default),
//'   applies operation by columns.
//' @param paral Logical. If TRUE, enables parallel processing.
//' @param threads Integer. Number of threads for parallel processing.
//'   Ignored if paral is FALSE.
//' @param overwrite Logical. If TRUE, allows overwriting existing datasets.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{gr}{Character string with the HDF5 group}
//'   \item{ds}{Character string with the full dataset path (group/dataset)}
//' }
//'
//' @examples
//' library(BigDataStatMeth)
//'     
//' # Create test data
//' set.seed(123)
//' Y <- matrix(rnorm(100), 10, 10)
//' X <- matrix(rnorm(10), 10, 1)
//'         
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", Y, "data", "Y",
//'                      overwriteFile = TRUE,
//'                      overwriteDataset = FALSE,
//'                      unlimited = FALSE)
//' bdCreate_hdf5_matrix("test.hdf5", X, "data", "X",
//'                      overwriteFile = FALSE,
//'                      overwriteDataset = FALSE,
//'                      unlimited = FALSE)
//'             
//' # Multiply matrix rows by vector
//' bdcomputeMatrixVector_hdf5("test.hdf5",
//'                            group = "data",
//'                            dataset = "Y",
//'                            vectorgroup = "data",
//'                            vectordataset = "X",
//'                            outdataset = "ProdComputed",
//'                            func = "*",
//'                            byrows = TRUE,
//'                            overwrite = TRUE)
//'     
//' # Subtract vector from matrix rows
//' bdcomputeMatrixVector_hdf5("test.hdf5",
//'                            group = "data",
//'                            dataset = "Y",
//'                            vectorgroup = "data",
//'                            vectordataset = "X",
//'                            outdataset = "SubsComputed",
//'                            func = "-",
//'                            byrows = TRUE,
//'                            overwrite = TRUE)
//'     
//' # Subtract vector from matrix columns
//' bdcomputeMatrixVector_hdf5("test.hdf5",
//'                            group = "data",
//'                            dataset = "Y",
//'                            vectorgroup = "data",
//'                            vectordataset = "X",
//'                            outdataset = "SubsComputed",
//'                            func = "-",
//'                            byrows = FALSE,
//'                            overwrite = TRUE)
//'                            
//' # Cleanup
//' if (file.exists("test.hdf5")) {
//'   file.remove("test.hdf5")
//' }
//'
//' @references
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//' * Eddelbuettel, D., & François, R. (2011). Rcpp: Seamless R and C++
//'   Integration. Journal of Statistical Software, 40(8), 1-18.
//'
//' @seealso
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List  bdcomputeMatrixVector_hdf5( std::string filename, std::string group, 
                                  std::string dataset,
                                  std::string vectorgroup, std::string vectordataset,
                                  std::string outdataset, 
                                  std::string func,
                                  Rcpp::Nullable<std::string> outgroup = R_NilValue,
                                  Rcpp::Nullable<bool> byrows = R_NilValue,
                                  Rcpp::Nullable<bool> paral = R_NilValue,
                                  Rcpp::Nullable<int> threads = R_NilValue,
                                  Rcpp::Nullable<int> overwrite  = false)
 {
     
     
     BigDataStatMeth::hdf5Dataset* dsA = nullptr;
     BigDataStatMeth::hdf5Dataset* dsB = nullptr;
     BigDataStatMeth::hdf5Dataset* dsC = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("gr") = "",
                                                Rcpp::Named("ds") = "");
     
     try{
         
         H5::Exception::dontPrint();
        
        bool bbyrows, bparal, bforce;
        std::string strgroupout;
        bool bError=false;
        
        Rcpp::NumericVector oper = {0, 1, 2, 3, 4};
        oper.names() = Rcpp::CharacterVector({"+", "-", "*", "/", "pow"});
         
         if( byrows.isNull()) { bbyrows = false; } 
         else { bbyrows = Rcpp::as<bool> (byrows); }
         
         if( overwrite.isNull()) { bforce = true; } 
         else { bforce = Rcpp::as<bool> (overwrite); }
         
         if( outgroup.isNull()) { strgroupout = group; } 
         else { strgroupout = Rcpp::as<std::string> (outgroup); }
         
         if (paral.isNull()) { bparal = false; } 
         else { bparal = Rcpp::as<bool> (paral); }
         
         // Function exists?
         if( oper(oper.findName(func)) != 0 && oper(oper.findName(func)) != 1 &&
             oper(oper.findName(func)) != 2 && oper(oper.findName(func)) != 3)  {
             Rcpp::Rcout<<"Function does not exists, please use one of the following : '+', '-', '*', '/', 'pow' ";
             return lst_return;
         } 

         dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
         dsA->openDataset();
         
         dsB = new BigDataStatMeth::hdf5Dataset(filename, vectorgroup, vectordataset, false);
         dsB->openDataset();
         
         if( dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr)  {
             
             // Check Vector dataset and matrix dataset A should be matrix and B should be the vector
             if( dsA->nrows()==1 || dsA->ncols()==1) {
                 Rcpp::Rcout<<"\ndataset is not a matrix";
                 bError = true;
             }
             
             if( dsB->nrows()!=1 && dsB->ncols()!=1) {
                 Rcpp::Rcout<<"\nvectordataset "<<vectordataset<<" is not a vector";
                 bError = true;
             }
             
             if( bError == false ) {
                 dsC = new BigDataStatMeth::hdf5Dataset(filename, strgroupout, outdataset, bforce);
                 
                 if( (bbyrows == false && dsB->ncols() != dsA->ncols()) || dsB->nrows() != 1 ) {
                     Rcpp::Rcout<<"\nNon-conformable dimensions or vector dataset is not a vector";
                     Rcpp::Rcout<<"\nCalculus has not been computed\n";
                 } else if( bbyrows == true && dsB->ncols() != dsA->nrows()) {
                     Rcpp::Rcout<<"\nNon-conformable dimensions - byrows = true";
                     Rcpp::Rcout<<"\nCalculus has not been computed\n";
                 } else {
                     
                     dsC = hdf5_matrixVector_calculus( dsA, dsB, dsC, oper(oper.findName(func)), bbyrows, bparal, threads);
                 }
                 
                 lst_return["fn"] = filename;
                 lst_return["gr"] = strgroupout;
                 lst_return["ds"] = outdataset;
                 
                 delete dsC; dsC = nullptr;
             }    
             
         } else {
             checkClose_file(dsA, dsB);
             Rf_error("c++ exception bdcomputeMatrixVector_hdf5 (DataSet IException)");
         }
         
         delete dsA; dsA = nullptr;
         delete dsB; dsB = nullptr;
         
     } catch( H5::FileIException& error ) {     
        checkClose_file(dsA, dsB, dsC);
        Rcpp::stop ( "c++ exception bdcomputeMatrixVector_hdf5 (File IException)");
     } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
         checkClose_file(dsA, dsB, dsC);
         Rcpp::stop ("c++ exception bdcomputeMatrixVector_hdf5 (DataSet IException)");
     } catch(std::exception &ex) {
         checkClose_file(dsA, dsB, dsC);
         Rcpp::stop ("c++ exception bdcomputeMatrixVector_hdf5 (std::exception): %s",  ex.what());
     }
     
     return lst_return;
 }
