#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixDiagonal.hpp"

/**
 * @file hdf5_matrixDiagonals.cpp
 * @brief Implementation of matrix diagonal operations for HDF5-stored matrices
 * @details This file provides functionality for working with matrix diagonals
 * in HDF5 format. The implementation supports:
 * - Reading diagonal elements
 * - Writing diagonal elements
 * - Memory-efficient operations
 * - Error handling and validation
 * 
 * Key features:
 * - Efficient diagonal access
 * - Support for large matrices
 * - Type checking and conversion
 * - Memory-safe operations
 * - Comprehensive error handling
 */

/**
 * @brief Gets diagonal elements from an HDF5 matrix
 * 
 * @details Implements efficient retrieval of diagonal elements from a matrix
 * stored in HDF5 format. The function handles file operations safely and
 * provides comprehensive error handling.
 * 
 * Implementation features:
 * - Memory-efficient diagonal extraction
 * - Safe file operations
 * - Error handling for invalid matrices
 * - Support for large matrices
 * 
 * @param filename Path to HDF5 file
 * @param group Group containing dataset
 * @param dataset Dataset name
 * 
 * @return Vector of diagonal elements
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Get Matrix Diagonal from HDF5
//'
//' @description
//' Retrieves the diagonal elements from a matrix stored in an HDF5 file.
//'
//' @details
//' This function provides efficient access to matrix diagonal elements with:
//' 
//' * Access features:
//'   - Direct diagonal access
//'   - Memory-efficient retrieval
//'   - Support for large matrices
//' 
//' * Implementation features:
//'   - Safe HDF5 file operations
//'   - Memory-efficient implementation
//'   - Comprehensive error handling
//'   - Read-only access to files
//'
//' The function opens the HDF5 file in read-only mode to ensure data safety.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing the dataset.
//' @param dataset Character string. Name of the dataset.
//'
//' @return Numeric vector containing diagonal elements.
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrix
//' X <- matrix(rnorm(100), 10, 10)
//' diag(X) <- 0.5
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", X, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' 
//' # Get diagonal
//' diag_elements <- bdgetDiagonal_hdf5("test.hdf5", "data", "matrix1")
//' print(diag_elements)
//' 
//' # Cleanup
//' if (file.exists("test.hdf5")) {
//'   file.remove("test.hdf5")
//' }
//' }
//'
//' @references
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdWriteDiagonal_hdf5}} for writing diagonal elements
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdgetDiagonal_hdf5( std::string filename, std::string group, std::string dataset)
{
    
    Rcpp::NumericVector intNewDiagonal;
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    try
        {
         dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
         dsA->openDataset();
         
         if( dsA->getDatasetptr() != nullptr) {
             intNewDiagonal = getDiagonalfromMatrix(dsA);    
         } else{
             Rcpp::Rcerr<<"c++ exception error obtaining diagonal from: "<<dataset<<"\n";
         }
         
         delete dsA; dsA = nullptr;
         
     } catch( H5::FileIException& error ) { 
         checkClose_file(dsA);
         Rcpp::Rcerr<<"c++ exception bdgetDiagonal_hdf5 (File IException)";
         return(Rcpp::wrap(0));
     } catch( H5::DataSetIException& error ) { 
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdgetDiagonal_hdf5 (DataSet IException)";
         return(Rcpp::wrap(0));
     } catch(std::exception& ex) {
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdgetDiagonal_hdf5" << ex.what();
         return(Rcpp::wrap(0));
     } catch (...) {
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdgetDiagonal_hdf5 (unknown reason)";
         return(Rcpp::wrap(0));
     }
     
     return(intNewDiagonal);
}




/**
 * @brief Sets diagonal elements in an HDF5 matrix
 * 
 * @details Implements efficient writing of diagonal elements to a matrix
 * stored in HDF5 format. The function handles file operations safely and
 * provides comprehensive error handling.
 * 
 * Implementation features:
 * - Memory-efficient diagonal writing
 * - Safe file operations
 * - Type checking and validation
 * - Support for large matrices
 * 
 * @param diagonal Vector of new diagonal elements
 * @param filename Path to HDF5 file
 * @param group Group containing dataset
 * @param dataset Dataset name
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Write Matrix Diagonal to HDF5
//'
//' @description
//' Updates the diagonal elements of a matrix stored in an HDF5 file.
//'
//' @details
//' This function provides efficient diagonal modification capabilities with:
//' 
//' * Write features:
//'   - Direct diagonal access
//'   - Type checking and validation
//'   - Support for large matrices
//' 
//' * Implementation features:
//'   - Safe HDF5 file operations
//'   - Memory-efficient implementation
//'   - Comprehensive error handling
//'   - Type conversion support
//'
//' The function validates input types and dimensions before modification.
//'
//' @param diagonal Numeric vector. New diagonal elements to write.
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing the dataset.
//' @param dataset Character string. Name of the dataset to modify.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the diagonal elements written (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test matrix
//' X <- matrix(rnorm(100), 10, 10)
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", X, "data", "matrix1",
//'                      overwriteFile = TRUE)
//' 
//' # Create new diagonal
//' new_diag <- seq(1, 10)
//' 
//' # Update diagonal
//' bdWriteDiagonal_hdf5(new_diag, "test.hdf5", "data", "matrix1")
//' 
//' # Verify
//' diag_elements <- bdgetDiagonal_hdf5("test.hdf5", "data", "matrix1")
//' print(diag_elements)
//' 
//' # Cleanup
//' if (file.exists("test.hdf5")) {
//'   file.remove("test.hdf5")
//' }
//' }
//'
//' @references
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdgetDiagonal_hdf5}} for reading diagonal elements
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdWriteDiagonal_hdf5( Rcpp::RObject diagonal, std::string filename, std::string group, std::string dataset)
{
     
     
     BigDataStatMeth::hdf5Dataset* dsA = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
     try
     {
         
         H5::Exception::dontPrint();
         
        Rcpp::NumericVector intNewDiagonal;    
        std::string strDataset = group + "/" + dataset;
         
        if( !Rcpp::is<Rcpp::IntegerVector>(diagonal) || !Rcpp::is<Rcpp::NumericVector>(diagonal) ) {
            intNewDiagonal = Rcpp::as<Rcpp::NumericVector>(diagonal);
        } else {
            Rcpp::Rcout<<"\n Diagonal vector isn't a Numeric vector";
            return(lst_return);
        }
         
        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
        dsA->openDataset();
         
        if( dsA->getDatasetptr() != nullptr) {
             setDiagonalMatrix( dsA, intNewDiagonal);
        } else{
            Rcpp::Rcerr<<"c++ exception error writing diagonal to: "<<dataset<<"\n";
        }
         
        delete dsA; dsA = nullptr;
        
        lst_return["fn"] = filename;
        lst_return["ds"] = strDataset;
         
     }  catch( H5::FileIException& error ) { 
         checkClose_file(dsA);
         Rcpp::Rcerr<<"c++ exception bdWriteDiagonal_hdf5 (File IException)";
         return(lst_return);
     } catch( H5::DataSetIException& error ) { 
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdWriteDiagonal_hdf5 (DataSet IException)";
         return(lst_return);
     } catch(std::exception& ex) {
         checkClose_file(dsA);
         Rcpp::Rcerr << "c++ exception bdWriteDiagonal_hdf5" << ex.what();
         return(lst_return);
     } catch (...) {
         checkClose_file(dsA);
         Rcpp::Rcerr<<"\nC++ exception bdWriteDiagonal_hdf5 (unknown reason)";
         return(lst_return);
     }
     
     Rcpp::Rcout<<dataset<<" diagonal has been overwritten\n";
     return(lst_return);
 }
