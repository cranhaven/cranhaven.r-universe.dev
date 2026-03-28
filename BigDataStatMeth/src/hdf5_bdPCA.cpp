#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixPCA.hpp"
// #include "Utilities/Utilities.hpp"

/**
 * @file hdf5_bdPCA.cpp
 * @brief Implementation of Principal Component Analysis (PCA) for HDF5-stored matrices
 * @details This file contains implementations for computing PCA of large matrices 
 * stored in HDF5 files. The implementation uses SVD internally and supports both
 * full and block-based computation methods, with options for data preprocessing
 * and parallel computation.
 */

/**
 * @brief Computes PCA of a matrix stored in an HDF5 file
 * 
 * @details Performs Principal Component Analysis by computing the SVD of a centered
 * and/or scaled matrix. The implementation:
 * - Uses SVD internally (A = UDV')
 * - Projects data onto principal components (PC = AD = UDD)
 * - Supports incremental computation for large matrices
 * - Provides options for data preprocessing (centering and scaling)
 * - Allows specification of number of components to compute
 * 
 * @param filename Path to HDF5 file containing input matrix
 * @param group Path to group containing input dataset
 * @param dataset Name of input dataset
 * @param ncomponents Number of principal components to compute (0 for all)
 * @param bcenter Whether to center the data
 * @param bscale Whether to scale the data
 * @param k Number of local SVDs to concatenate at each level
 * @param q Number of levels for incremental computation
 * @param rankthreshold Threshold for determining matrix rank
 * @param SVDgroup Group name for storing/loading SVD results
 * @param overwrite Whether to overwrite existing results
 * @param method Computation method ("auto", "blocks", or "full")
 * @param threads Number of threads for parallel computation
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws H5::DataSpaceIException if there are HDF5 dataspace errors
 * @throws H5::DataTypeIException if there are HDF5 datatype errors
 * @throws std::exception for other errors
 */
//' Principal Component Analysis for HDF5-Stored Matrices
//'
//' @description
//' Performs Principal Component Analysis (PCA) on a large matrix stored in an HDF5 file.
//' PCA reduces the dimensionality of the data while preserving as much variance as
//' possible. The implementation uses SVD internally for efficient and numerically
//' stable computation.
//' 
//' @details
//' This function implements a scalable PCA algorithm suitable for large matrices
//' that may not fit in memory. Key features include:
//' * Automatic method selection based on matrix size
//' * Block-based computation for large matrices
//' * Optional data preprocessing (centering and scaling)
//' * Parallel processing support
//' * Memory-efficient incremental algorithm
//' * Reuse of existing SVD results
//'
//' The implementation uses SVD internally and supports two computation methods:
//' * Full decomposition: Suitable for matrices that fit in memory
//' * Block-based decomposition: For large matrices, uses an incremental algorithm
//'
//' @param filename Character string. Path to the HDF5 file containing the input matrix.
//' @param group Character string. Path to the group containing the input dataset.
//' @param dataset Character string. Name of the input dataset to analyze.
//' @param ncomponents Integer. Number of principal components to compute (default = 0,
//'   which computes all components).
//' @param bcenter Logical. If TRUE, centers the data by subtracting column means.
//'   Default is FALSE.
//' @param bscale Logical. If TRUE, scales the centered columns by their standard
//'   deviations (if centered) or root mean square. Default is FALSE.
//' @param k Integer. Number of local SVDs to concatenate at each level (default = 2).
//'   Controls memory usage in block computation.
//' @param q Integer. Number of levels for SVD computation (default = 1).
//'   Higher values can improve accuracy but increase computation time.
//' @param rankthreshold Numeric. Threshold for determining matrix rank (default = 0).
//'   Must be between 0 and 0.1.
//' @param SVDgroup Character string. Group name where intermediate SVD results are
//'   stored. If SVD was previously computed, results will be reused from this group.
//' @param overwrite Logical. If TRUE, forces recomputation of SVD even if results exist.
//' @param method Character string. Computation method:
//'   * "auto": Automatically selects method based on matrix size
//'   * "blocks": Uses block-based computation (for large matrices)
//'   * "full": Performs direct computation (for smaller matrices)
//' @param threads Integer. Number of threads for parallel computation.
//'
//' @return A list containing the paths to the PCA results stored in the HDF5 file:
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the results}
//'     \item{lambda}{Character string. Dataset path to eigenvalues \eqn{\lambda}}
//'     \item{variance}{Character string. Dataset path to variance explained by each PC}
//'     \item{cumvar}{Character string. Dataset path to cumulative variance explained}
//'     \item{var.coord}{Character string. Dataset path to variable coordinates on the PCs}
//'     \item{var.cos2}{Character string. Dataset path to squared cosines (quality of representation) for variables}
//'     \item{ind.dist}{Character string. Dataset path to distances of individuals from the origin}
//'     \item{components}{Character string. Dataset path to principal components (rotated data)}
//'     \item{ind.coord}{Character string. Dataset path to individual coordinates on the PCs}
//'     \item{ind.cos2}{Character string. Dataset path to squared cosines (quality of representation) for individuals}
//'     \item{ind.contrib}{Character string. Dataset path to contributions of individuals to each PC}
//'   }
//'   All results are written to the HDF5 file in the group 'PCA/`dataset`'.
//'
//' @examples
//' \dontrun{
//' # Create a sample large matrix in HDF5
//' library(rhdf5)
//' X <- matrix(rnorm(10000), 1000, 10)
//' h5createFile("data.h5")
//' h5write(X, "data.h5", "data/matrix")
//'
//' # Basic PCA with default parameters
//' bdPCA_hdf5("data.h5", "data", "matrix")
//'
//' # PCA with preprocessing and specific number of components
//' bdPCA_hdf5("data.h5", "data", "matrix",
//'            ncomponents = 3,
//'            bcenter = TRUE, bscale = TRUE,
//'            method = "blocks",
//'            threads = 4)
//' }
//'
//' @references
//' * Halko, N., Martinsson, P. G., & Tropp, J. A. (2011). Finding structure with randomness:
//'   Probabilistic algorithms for constructing approximate matrix decompositions.
//'   SIAM Review, 53(2), 217-288.
//' * Jolliffe, I. T. (2002). Principal Component Analysis, Second Edition.
//'   Springer Series in Statistics.
//'
//' @seealso
//' * \code{\link{bdSVD_hdf5}} for the underlying SVD computation
//' * \code{\link{bdNormalize_hdf5}} for data preprocessing options
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdPCA_hdf5(std::string filename, std::string group, std::string dataset,
                Rcpp::Nullable<int> ncomponents = 0,
                Rcpp::Nullable<bool> bcenter = false, Rcpp::Nullable<bool> bscale = false, 
                Rcpp::Nullable<int> k = 2, Rcpp::Nullable<int> q = 1,
                Rcpp::Nullable<double> rankthreshold = 0.0,
                Rcpp::Nullable<std::string> SVDgroup = R_NilValue,
                Rcpp::Nullable<bool> overwrite = false, 
                Rcpp::Nullable<Rcpp::CharacterVector> method = R_NilValue,
                Rcpp::Nullable<int> threads = R_NilValue)
{
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("lambda") = "",
                                               Rcpp::Named("variance") = "",
                                               Rcpp::Named("cumvar") = "",
                                               Rcpp::Named("var.coord") = "",
                                               Rcpp::Named("var.cos2") = "",
                                               Rcpp::Named("ind.dist") = "",
                                               Rcpp::Named("components") = "",
                                               Rcpp::Named("ind.coord") = "",
                                               Rcpp::Named("ind.cos2") = "",
                                               Rcpp::Named("ind.contrib") = "");
    
    try
    {
        H5::Exception::dontPrint();
        
        bool bcent, bscal, bforce;
        int ks, qs = 1, incomponents = 0;
        double dthreshold;
        std::string strSVDgroup, 
                    strPCAgroup = "PCA/" + dataset;
        
        if(ncomponents.isNull())  incomponents = 0 ;
        else    incomponents = Rcpp::as<int>(ncomponents);
        
        if(q.isNull())  qs = 1 ;
        else    qs = Rcpp::as<int>(q);
        
        if(k.isNull())  ks = 2 ;
        else    ks = Rcpp::as<int>(k);
        
        if(bcenter.isNull())  bcent = false ;
        else    bcent = Rcpp::as<bool>(bcenter);
        
        if(bscale.isNull())  bscal = false ;
        else    bscal = Rcpp::as<bool>(bscale);
        
        if(overwrite.isNull())  bforce = false ;
        else    bforce = Rcpp::as<bool>(overwrite);
        
        if(rankthreshold.isNull()) {  
            dthreshold = 0 ;
        } else {
            if( Rcpp::as<double>(rankthreshold) > 0.1 ) {
                Rcpp::Rcout<< "Threshold to big, please set threshold with value lower than 0.1";
                return(lst_return);
                // return void();
            } else if( Rcpp::as<double>(rankthreshold) < 0 ) {
                Rcpp::Rcout<< "Threshold must be a positive value near zero";
                return(lst_return);
                // return void();
            } else {
                dthreshold = Rcpp::as<double>(rankthreshold);
            }
        }
        
        if(SVDgroup.isNull()) { strSVDgroup = "SVD"; } 
        else {   strSVDgroup = Rcpp::as<std::string>(SVDgroup); }
        
        if( strSVDgroup.substr(strSVDgroup.length(), strSVDgroup.length()) != "/" ){
            strSVDgroup = strSVDgroup + "/";
        }
        
        BigDataStatMeth::RcppPCAHdf5(filename, group, dataset, strSVDgroup, ks, qs, incomponents, bcent, bscal, dthreshold, bforce, false, method, threads);
        
        lst_return["fn"] = filename;
        lst_return["lambda"] = strPCAgroup + "/" +  "lambda";
        lst_return["variance"] = strPCAgroup + "/" +  "variance";
        lst_return["cumvar"] = strPCAgroup + "/" +  "cumvar";
        lst_return["var.coord"] = strPCAgroup + "/" +  "var.coord";
        lst_return["var.cos2"] = strPCAgroup + "/" +  "var.cos2";
        lst_return["ind.dist"] = strPCAgroup + "/" +  "ind.dist";
        lst_return["components"] = strPCAgroup + "/" +  "components";
        lst_return["ind.coord"] = strPCAgroup + "/" +  "ind.coord";
        lst_return["ind.cos2"] = strPCAgroup + "/" +  "ind.cos2";
        lst_return["ind.contrib"] = strPCAgroup + "/" +  "ind.contrib";
        
    } catch( H5::FileIException& error ) {
        Rcpp::Rcerr<<"\nc++ exception bdPCA_hdf5 (File IException)";
        // return void();
    } catch( H5::DataSetIException& error ) { 
        Rcpp::Rcerr<<"\nc++ exception bdPCA_hdf5 (DataSet IException)";
        // return void();
    } catch( H5::DataSpaceIException& error ) { 
        Rcpp::Rcerr<<"\nc++ exception bdPCA_hdf5 (DataSpace IException)";
        // return void();
    } catch( H5::DataTypeIException& error ) {
        Rcpp::Rcerr<<"\nc++ exception bdPCA_hdf5 (DataType IException)";
        // return void();
    } catch(std::exception &ex) {
        Rcpp::Rcerr<<"\nC++ exception bdPCA_hdf5 : "<< ex.what();
        // return void();
    } catch (...) {
        Rcpp::Rcerr<<"\nC++ exception bdPCA_hdf5 (unknown reason)";
        // return void();
    }
    
    // return void();
    return(lst_return);
    
}
