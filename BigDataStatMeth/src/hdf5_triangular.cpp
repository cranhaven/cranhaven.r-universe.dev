#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixTriangular.hpp"
// #include "hdf5Utilities/hdf5Utilities.hpp"

/**
 * @file hdf5_triangular.cpp
 * @brief Implementation of triangular matrix operations for HDF5-stored matrices
 * @details This file provides functionality for working with triangular matrices
 * stored in HDF5 format. It includes operations for:
 * - Mirroring upper triangular to lower triangular matrices and vice versa
 * - Efficient block-based processing for large matrices
 * - In-place matrix modifications
 * 
 * The implementation supports:
 * - Memory-efficient block processing
 * - Error handling and validation
 * - Square matrix requirements
 * - Flexible block size configuration
 */

/**
 * @brief Writes the opposite triangular part of a matrix by mirroring existing values
 * 
 * @details This function takes a matrix with values in either its upper or lower
 * triangular part and mirrors those values to create a symmetric matrix. The operation
 * is performed in-place on the HDF5 dataset.
 * 
 * Key features:
 * - Supports both upper-to-lower and lower-to-upper mirroring
 * - Block-based processing for memory efficiency
 * - Validates matrix dimensions
 * - Preserves original values in source triangular part
 * 
 * Implementation details:
 * - Uses block processing to handle large matrices
 * - Performs in-place modifications
 * - Validates square matrix requirement
 * - Handles HDF5 file operations safely
 * 
 * @param filename Path to HDF5 file containing the matrix
 * @param group Group path containing the dataset
 * @param dataset Name of the dataset to modify
 * @param copytolower Whether to copy upper triangular to lower (true) or vice versa (false)
 * @param elementsBlock Block size for processing
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Write Upper/Lower Triangular Matrix
//'
//' @description
//' Creates a symmetric matrix by mirroring values from one triangular part to the other
//' in an HDF5-stored matrix. This function modifies the matrix in-place, either copying
//' the upper triangular values to the lower triangular part or vice versa.
//'
//' @details
//' This function provides an efficient way to create symmetric matrices from triangular
//' data. It operates directly on HDF5 datasets using block processing for memory
//' efficiency. The function:
//' 
//' * Validates that the input matrix is square
//' * Processes the matrix in blocks for memory efficiency
//' * Performs in-place modification of the dataset
//' * Preserves the original values in the source triangular part
//' * Supports both upper-to-lower and lower-to-upper mirroring
//'
//' The implementation uses block processing to handle large matrices efficiently,
//' making it suitable for big data applications. The block size can be adjusted
//' based on available memory and performance requirements.
//'
//' @param filename Character string specifying the path to an existing HDF5 file
//' @param group Character string indicating the input group containing the dataset
//' @param dataset Character string specifying the dataset to be modified
//' @param copytolower Logical. If TRUE, copies upper triangular to lower triangular.
//'   If FALSE (default), copies lower triangular to upper triangular.
//' @param elementsBlock Integer defining the maximum number of elements to process
//'   in each block. Default is 1,000,000. For matrices larger than 5000x5000,
//'   automatically adjusted to number of rows or columns * 2.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the modified matrix. The opposite triangular part is written to the same input dataset, completing the symmetric matrix (group/dataset)}
//' }
//'
//' @examples
//' library(BigDataStatMeth)
//' 
//' # Create a matrix with upper triangular values
//' X <- matrix(rnorm(100), 10, 10)
//' X.1 <- X
//' X[lower.tri(X)] <- 0
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test_file.hdf5", X, "data", "X", 
//'                      overwriteFile = TRUE, 
//'                      overwriteDataset = FALSE, 
//'                      unlimited = FALSE)
//'                      
//' # Mirror upper triangular to lower
//' bdWriteOppsiteTriangularMatrix_hdf5(
//'   filename = "test_file.hdf5", 
//'   group = "data",
//'   dataset = "X",
//'   copytolower = TRUE,
//'   elementsBlock = 10
//' )
//'
//' # Create a matrix with lower triangular values
//' X <- X.1
//' X[upper.tri(X)] <- 0
//' 
//' # Add to HDF5 file
//' bdCreate_hdf5_matrix("test_file.hdf5", X, "data", "Y", 
//'                      overwriteFile = FALSE, 
//'                      overwriteDataset = FALSE, 
//'                      unlimited = FALSE)
//'                      
//' # Mirror lower triangular to upper
//' bdWriteOppsiteTriangularMatrix_hdf5(
//'   filename = "test_file.hdf5", 
//'   group = "data",
//'   dataset = "Y",
//'   copytolower = FALSE,
//'   elementsBlock = 10
//' )
//'
//' # Cleanup
//' if (file.exists("test_file.hdf5")) {
//'   file.remove("test_file.hdf5")
//' }
//'
//' @references
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdWriteOppsiteTriangularMatrix_hdf5(std::string filename, 
                                      std::string group, std::string dataset, 
                                      Rcpp::Nullable<bool> copytolower = R_NilValue,
                                      Rcpp::Nullable<long> elementsBlock = 1000000)
{
     
     
     
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
     
     try
     {
         
         H5::Exception::dontPrint();
         
        Rcpp::NumericVector intNewDiagonal;
        bool blower;
        long dElementsBlock;
         
         // Get default values for Nullable variables
         if(copytolower.isNull()) { blower = false; } 
         else {  blower = Rcpp::as<bool>(copytolower); }
         
         if(elementsBlock.isNull()) { dElementsBlock = MAXELEMSINBLOCK; } 
         else { dElementsBlock = Rcpp::as<long>(elementsBlock); }
         
         std::string strDataset = group + "/" + dataset;
         
         dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
         dsA->openDataset();
         
         if( dsA->getDatasetptr() != nullptr)  {
             
             if(dsA->nrows() != dsA->ncols()) {
                 Rcpp::Rcout<<"\nCan not write opposite triangular matrix - Non squuare matrix";
                 delete dsA; dsA = nullptr;
                 return(lst_return);   
             }
             
             if( blower == false ) {
                 setLowerTriangularMatrix( dsA, dElementsBlock);
             } else {
                 setUpperTriangularMatrix( dsA, dElementsBlock);
             }    
         }
         
         delete dsA; dsA = nullptr;
         
         lst_return["fn"] = filename;
         lst_return["ds"] = group + "/" + dataset;
         
     } catch( H5::FileIException& error ) { 
         checkClose_file(dsA);
         Rcpp::Rcerr<<"c++ exception bdWriteOppsiteTriangularMatrix_hdf5 (File IException)";
         return(lst_return);
     } catch( H5::DataSetIException& error ) { 
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdWriteOppsiteTriangularMatrix_hdf5 (DataSet IException)";
         return(lst_return);
     } catch(std::exception& ex) {
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdWriteOppsiteTriangularMatrix_hdf5" << ex.what();
         return(lst_return);
     } catch (...) {
         checkClose_file(dsA);
         Rcpp::Rcerr<<"\nC++ exception bdWriteOppsiteTriangularMatrix_hdf5 (unknown reason)";
         return(lst_return);
     }
     
     Rcpp::Rcout<<dataset<<" Triangular Matrix has been mirrored\n";
     return(lst_return);
 }
