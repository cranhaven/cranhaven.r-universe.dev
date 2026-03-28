#include <BigDataStatMeth.hpp>
// #include "Utilities/Utilities.hpp"
// #include "hdf5Algebra/matrixQR.hpp"

/**
 * @file hdf5_QRDecomposition.cpp
 * @brief Implementation of QR decomposition for both in-memory and HDF5-stored matrices
 * @details This file contains implementations for computing QR decomposition of matrices,
 * both for in-memory matrices using Eigen and for matrices stored in HDF5 files.
 */

/**
 * @brief Computes QR decomposition of an in-memory matrix
 * 
 * @details Performs QR decomposition of a matrix A into a product A = QR where:
 * - Q is an orthogonal matrix (Q'Q = I)
 * - R is an upper triangular matrix
 * 
 * The function supports both thin and full QR decomposition.
 * 
 * @param X Input matrix to be decomposed (Rcpp::RObject that can be converted to Eigen::MatrixXd)
 * @param thin Optional boolean, if true returns reduced (thin) Q matrix, if false returns full Q matrix
 * @param block_size Optional integer specifying block size for blocked computation
 * @param threads Optional integer specifying number of threads for parallel computation
 * 
 * @return Rcpp::List containing:
 *   - Q: The orthogonal matrix
 *   - R: The upper triangular matrix
 * 
 * @throws std::exception if matrix conversion fails
 */
//' QR Decomposition for In-Memory Matrices
//' 
//' @description
//' Computes the QR decomposition (also called QR factorization) of a matrix A into 
//' a product A = QR where Q is an orthogonal matrix and R is an upper triangular matrix.
//' This function operates on in-memory matrices.
//' 
//' @details
//' The QR decomposition is a fundamental matrix factorization that decomposes a matrix 
//' into an orthogonal matrix Q and an upper triangular matrix R. This implementation:
//' * Supports both thin and full QR decomposition
//' * Can utilize parallel computation for better performance
//' * Handles both matrix and vector inputs
//' 
//' @param X A real matrix or vector to be decomposed
//' @param thin Logical. If TRUE, returns the reduced (thin) Q matrix. If FALSE (default),
//'   returns the full Q matrix. The thin decomposition is more memory efficient.
//' @param block_size Integer. Optional block size for blocked computation. Larger blocks
//'   may improve performance but require more memory.
//' @param threads Integer. Optional number of threads for parallel computation. If NULL,
//'   uses all available threads.
//'
//' @return A list containing:
//'   * Q: The orthogonal matrix Q
//'   * R: The upper triangular matrix R
//'
//' @examples
//' \dontrun{
//' # Create a random 100x50 matrix
//' X <- matrix(rnorm(5000), 100, 50)
//' 
//' # Compute thin QR decomposition
//' result <- bdQR(X, thin = TRUE)
//' 
//' # Verify the decomposition
//' # Should be approximately zero
//' max(abs(X - result$Q %*% result$R))
//' }
//'
//' @seealso
//' \code{\link{bdQR_hdf5}} for QR decomposition of HDF5-stored matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdQR( const Rcpp::RObject & X, 
                    Rcpp::Nullable<bool> thin = R_NilValue, 
                    Rcpp::Nullable<int> block_size = R_NilValue,
                    Rcpp::Nullable<int> threads = R_NilValue)
{
    
    
    Eigen::MatrixXd A;
    bool bthin;
    BigDataStatMeth::strQR decQR;
    
    try{
        A = Rcpp::as<Eigen::MatrixXd >(X);
    }catch(std::exception &ex) {
        A = Rcpp::as<Eigen::VectorXd >(X);
    }
    
    
    if( thin.isNull()) {
        bthin = false;
    } else {
        bthin = Rcpp::as<bool> (thin);
    }
    
    decQR = BigDataStatMeth::RcppQR(A, bthin);
    
    return Rcpp::List::create(Rcpp::Named("Q") = decQR.Q,
                              Rcpp::Named("R") = decQR.R
    );
}

/**
 * @brief Computes QR decomposition of a matrix stored in an HDF5 file
 * 
 * @details Performs QR decomposition of a matrix A stored in an HDF5 file into a 
 * product A = QR. The results (Q and R matrices) are stored back in the HDF5 file.
 * 
 * The function supports:
 * - Both thin and full QR decomposition
 * - Blocked computation for large matrices
 * - Parallel computation
 * - Flexible output location in HDF5 file
 * 
 * @param filename String path to HDF5 file containing input matrix
 * @param group String path to group containing input dataset
 * @param dataset String name of input dataset
 * @param outgroup Optional string specifying output group path (defaults to input_group/QRDec)
 * @param outdataset Optional string specifying base name for output datasets
 * @param thin Optional boolean controlling thin vs full decomposition
 * @param block_size Optional integer specifying block size for blocked computation
 * @param overwrite Optional boolean, if true allows overwriting existing datasets
 * @param threads Optional integer specifying number of threads for parallel computation
 * 
 * @note Output datasets are named:
 * - Q.[outdataset]: The orthogonal matrix
 * - R.[outdataset]: The upper triangular matrix
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::GroupIException if there are HDF5 group operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */
//' QR Decomposition for HDF5-Stored Matrices
//' 
//' @description
//' Computes the QR decomposition of a matrix stored in an HDF5 file, factoring it into 
//' a product A = QR where Q is an orthogonal matrix and R is an upper triangular matrix.
//' Results are stored back in the HDF5 file.
//' 
//' @details
//' This function performs QR decomposition on large matrices stored in HDF5 format,
//' which is particularly useful for matrices too large to fit in memory. Features include:
//' * Support for both thin and full QR decomposition
//' * Blocked computation for improved performance
//' * Parallel processing capabilities
//' * Flexible output location specification
//' * Optional overwriting of existing datasets
//'
//' @param filename Character string. Path to the HDF5 file containing the input matrix.
//' @param group Character string. Path to the group containing the input dataset.
//' @param dataset Character string. Name of the input dataset to decompose.
//' @param outgroup Character string. Optional output group path where results will be stored.
//'   If not provided, results are stored in `<input_group>/QRDec`.
//' @param outdataset Character string. Optional base name for output datasets. Results
//'   will be stored as `Q.'outdataset'` and `R.'outdataset'`.
//' @param thin Logical. If TRUE, computes the reduced (thin) QR decomposition.
//'   If FALSE (default), computes the full decomposition.
//' @param block_size Integer. Optional block size for blocked computation.
//' @param overwrite Logical. If TRUE, allows overwriting existing datasets.
//'   Default is FALSE.
//' @param threads Integer. Optional number of threads for parallel computation.
//'   If NULL, uses all available threads.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds_Q}{Character string with the full dataset path to the Q matrix 
//'   (orthogonal matrix). Results are written to the HDF5 file as 
//'   "Q.'outdataset'" within the specified group}
//'   \item{ds_R}{Character string with the full dataset path to the R matrix 
//'   (upper triangular matrix). Results are written to the HDF5 file as 
//'   "R.'outdataset'" within the specified group}
//' }
//'
//' @examples
//' \dontrun{
//' # Create a sample HDF5 file with a matrix
//' library(rhdf5)
//' A <- matrix(rnorm(1000), 100, 10)
//' h5createFile("example.h5")
//' h5write(A, "example.h5", "mygroup/mymatrix")
//'
//' # Compute QR decomposition
//' bdQR_hdf5("example.h5", "mygroup", "mymatrix",
//'           outgroup = "mygroup/results",
//'           outdataset = "qr_result",
//'           thin = TRUE)
//' }
//'
//' @seealso
//' \code{\link{bdQR}} for QR decomposition of in-memory matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List  bdQR_hdf5( std::string filename, std::string group, std::string dataset,
                Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                Rcpp::Nullable<std::string> outdataset = R_NilValue,
                Rcpp::Nullable<bool> thin = R_NilValue, 
                Rcpp::Nullable<int> block_size = R_NilValue,
                Rcpp::Nullable<bool> overwrite = R_NilValue,
                Rcpp::Nullable<int> threads = R_NilValue)
{
    
    
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsQ = nullptr;
    BigDataStatMeth::hdf5Dataset* dsR = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds_Q") = "",
                                               Rcpp::Named("ds_R") = "");
    
    try {
        
        H5::Exception::dontPrint();
        
        bool bthin, bforce;
        std::string strOutgroup, strOutdataset_Q, strOutdataset_R;
        // int iblock_size;
        
        if(outgroup.isNull()) { strOutgroup = group + "/QRDec"; } 
        else {   strOutgroup = Rcpp::as<std::string>(outgroup); }
        
        if(outdataset.isNull()) { 
            strOutdataset_Q = "Q." + dataset;
            strOutdataset_R = "R." + dataset; 
        } 
        else {   
            strOutdataset_Q = "Q." + Rcpp::as<std::string>(outdataset);
            strOutdataset_R = "R." + Rcpp::as<std::string>(outdataset);
        }
        
        if( thin.isNull()) {
            bthin = false;
        } else {
            bthin = Rcpp::as<bool> (thin);
        }
        
        if(overwrite.isNull())  bforce = false ;
        else    bforce = Rcpp::as<bool>(overwrite);
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, bforce);
        dsA->openDataset();
        
        
        dsQ = new BigDataStatMeth::hdf5Dataset(filename, strOutgroup, strOutdataset_Q, bforce);
        // dsQ->openDataset();
        
        dsR = new BigDataStatMeth::hdf5Dataset(filename, strOutgroup, strOutdataset_R, bforce);
        // dsR->openDataset();
        
        if( dsA->getDatasetptr() != nullptr ) {
            RcppQRHdf5(dsA, dsQ, dsR, bthin, block_size, threads);


        lst_return["fn"] = filename;
        lst_return["ds_Q"] = strOutgroup + "/" + strOutdataset_Q;
        lst_return["ds_R"] = strOutgroup + "/" + strOutdataset_R;
            
            // lst_return = Rcpp::List::create(Rcpp::Named("fn") = filename,
            //                                 Rcpp::Named("ds_Q") = strOutgroup + "/" + strOutdataset_Q,
            //                                 Rcpp::Named("ds_R") = strOutgroup + "/" + strOutdataset_R);
            
        } else {
            checkClose_file(dsA, dsQ, dsR);
            Rcpp::Rcerr << "c++ exception bdQR_hdf5: " << "Error creating dataset";
            return(lst_return);
        }
        
        delete dsA; dsA = nullptr;
        delete dsQ; dsQ = nullptr;
        delete dsR; dsR = nullptr;
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(dsA, dsQ, dsR);
        Rcpp::Rcerr << "c++ exception bdQR_hdf5 (File IException)";
        // return void();
    } catch( H5::GroupIException & error ) { 
        checkClose_file(dsA, dsQ, dsR);
        Rcpp::Rcerr << "c++ exception bdQR_hdf5 (Group IException)";
        // return void();
    } catch( H5::DataSetIException& error ) {
        checkClose_file(dsA, dsQ, dsR);
        Rcpp::Rcerr << "c++ exception bdQR_hdf5 (DataSet IException)";
        // return void();
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsQ, dsR);
        Rcpp::Rcerr << "c++ exception bdQR_hdf5" << ex.what();
        // return void();
    }
    
    // return void();
    return(lst_return);
    
    
}
