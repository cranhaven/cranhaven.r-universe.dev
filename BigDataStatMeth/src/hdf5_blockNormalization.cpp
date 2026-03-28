/**
 * @file hdf5_blockNormalization.cpp
 * @brief Block-wise data normalization for HDF5 datasets
 * 
 * This file implements efficient block-wise normalization operations for large
 * datasets stored in HDF5 format. It provides functionality for centering and
 * scaling data, with support for both row-wise and column-wise operations.
 * 
 * Key features:
 * - Data centering (mean subtraction)
 * - Data scaling (standard deviation division)
 * - Row-wise and column-wise operations
 * - Block-wise processing
 * - Memory-efficient implementation
 * 
 * The implementation focuses on:
 * - Efficient statistical computations
 * - Memory-efficient block processing
 * - Flexible normalization options
 * - Resource management
 * - Error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixSdMean.hpp"
// #include "hdf5Algebra/matrixNormalization.hpp"

/**
 * @brief Normalize HDF5 datasets through centering and scaling
 *
 * @details Performs block-wise normalization of datasets stored in HDF5 format
 * through centering and/or scaling operations. Supports both row-wise and
 * column-wise normalization with memory-efficient block processing.
 *
 * Normalization options:
 * - Centering: Subtract mean from each column/row
 * - Scaling: Divide by standard deviation
 * - Row-wise or column-wise processing
 *
 * @param filename [in] HDF5 file path
 * @param group [in] Group containing the dataset
 * @param dataset [in] Dataset name to normalize
 * @param bcenter [in] Whether to center the data
 * @param bscale [in] Whether to scale the data
 * @param byrows [in] Whether to operate by rows
 * @param wsize [in] Block size for processing
 * @param overwrite [in] Whether to overwrite existing datasets
 *
 * @return void
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws H5::DataSpaceIException if dataspace operations fail
 * @throws H5::DataTypeIException if datatype operations fail
 * @throws std::exception for other errors
 *
 * @note Block size affects performance and memory usage
 * @see RcppNormalizeHdf5()
 */

//' Normalize dataset in HDF5 file
//' 
//' Performs block-wise normalization of datasets stored in HDF5 format through
//' centering and/or scaling operations. Supports both row-wise and column-wise
//' normalization with memory-efficient block processing.
//' 
//' @param filename String indicating the HDF5 file path
//' @param group String specifying the group containing the dataset
//' @param dataset String specifying the dataset name to normalize
//' @param bcenter Optional boolean indicating whether to center the data.
//'        If TRUE (default), subtracts mean from each column/row
//' @param bscale Optional boolean indicating whether to scale the data.
//'        If TRUE (default), divides by standard deviation
//' @param byrows Optional boolean indicating whether to operate by rows.
//'        If TRUE, processes row-wise; if FALSE (default), column-wise
//' @param wsize Optional integer specifying the block size for processing.
//'        Default is 1000
//' @param overwrite Optional boolean indicating whether to overwrite existing datasets.
//'        Default is false
//' 
//' @return List with components. If an error occurs, all string values are 
//' returned as empty strings (""):
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the results}
//'     \item{ds}{Character string. Full dataset path to the normalized data, stored under "NORMALIZED/\\[group\\]/\\[dataset\\]"}
//'     \item{mean}{Character string. Dataset path to the column means used for centering, stored under "NORMALIZED/\\[group\\]/mean.\\[dataset\\]"}
//'     \item{sd}{Character string. Dataset path to the standard deviations used for scaling, stored under "NORMALIZED/\\[group\\]/sd.\\[dataset\\]"}
//'   }
//' 
//' @details
//' The function implements block-wise normalization through:
//' 
//' Statistical computations:
//' - Mean calculation (for centering)
//' - Standard deviation calculation (for scaling)
//' - Efficient block-wise updates
//' 
//' Memory efficiency:
//' - Block-wise data processing
//' - Minimal temporary storage
//' - Proper resource cleanup
//' 
//' Processing options:
//' - Row-wise or column-wise operations
//' - Flexible block size selection
//' - Optional centering and scaling
//' 
//' Error handling:
//' - Input validation
//' - Resource management
//' - Exception handling
//' 
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test data
//' data <- matrix(rnorm(1000*100), 1000, 100)
//' 
//' # Save to HDF5
//' bdCreate_hdf5_matrix("test.hdf5", data, "data", "matrix",
//'                      overwriteFile = TRUE)
//' 
//' # Normalize data
//' bdNormalize_hdf5("test.hdf5", "data", "matrix",
//'                  bcenter = TRUE,
//'                  bscale = TRUE,
//'                  wsize = 1000)
//' }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdNormalize_hdf5( std::string filename, std::string group, std::string dataset,
                    Rcpp::Nullable<bool> bcenter = R_NilValue, Rcpp::Nullable<bool> bscale  = R_NilValue,
                    Rcpp::Nullable<bool> byrows = R_NilValue,
                    Rcpp::Nullable<int> wsize  = R_NilValue, Rcpp::Nullable<bool> overwrite  = false)
{
 
     BigDataStatMeth::hdf5Dataset* dsA = nullptr;
     BigDataStatMeth::hdf5Dataset* dsmean = nullptr;
     BigDataStatMeth::hdf5Dataset* dssd = nullptr;
     BigDataStatMeth::hdf5Dataset* dsNormal = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "",
                                                Rcpp::Named("mean") = "",
                                                Rcpp::Named("sd") = "");
     
     try{
         
         H5::Exception::dontPrint();
         
         bool bc, bs, bforce, bbyrows, corrected = false;
         hsize_t nrows, ncols;
         std::string strgroupout;
         std::vector<hsize_t> stride = {1, 1},
                              block = {1, 1};
         
         Eigen::MatrixXd datanormal;
         
         if( bcenter.isNull()) {  bc = true;   }
         else {   bc = Rcpp::as<bool> (bcenter);  }
         
         if( bscale.isNull()) {   bs = true;   }
         else {   bs = Rcpp::as<bool> (bscale);   }
         
         if( byrows.isNull()) {  bbyrows = false;   }
         else {   bbyrows = Rcpp::as<bool> (byrows);   }
         
         if( overwrite.isNull()) {   bforce = false;   }
         else {   bforce = Rcpp::as<bool> (overwrite);   }
         
         dsA = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
         dsA->openDataset();
         
         nrows = dsA->nrows();
         ncols = dsA->ncols();
         
         // Define blocksize atending number of elements in rows and cols
         if( dsA->getDatasetptr() != nullptr) {
             if( bbyrows == false) {
                 datanormal = Eigen::MatrixXd::Zero(2,nrows);
                 get_HDF5_mean_sd_by_column( dsA, datanormal, true, true, wsize);
             } else {
                 datanormal = Eigen::MatrixXd::Zero(2,ncols);
                 get_HDF5_mean_sd_by_row( dsA, datanormal, true, true, wsize);
             }    
         } else {
             checkClose_file(dsA, dsmean, dssd, dsNormal);
             Rcpp::Rcerr<<"\nC++ exception bdNormalize_hdf5 : error with "<<dataset<< " datset\n";
             return(lst_return);
             // return void();
         }
         
         strgroupout = "NORMALIZED/" + group;
         std::string strdatasetmean = "mean." + dataset;
         std::string strdatasetsd = "sd." + dataset;
         
         dsmean = new BigDataStatMeth::hdf5Dataset(filename, strgroupout, strdatasetmean, bforce);
         dsmean->createDataset( datanormal.cols(), 1, "real");
         if( dsmean->getDatasetptr() != nullptr) {
            dsmean->writeDataset( Rcpp::wrap(datanormal.row(0)) );
         } else {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception bdNormalize_hdf5 : error with "<<strdatasetmean<< " datset\n";
            return(lst_return);
            // return void();
         }
         
         dssd = new BigDataStatMeth::hdf5Dataset(filename, strgroupout, strdatasetsd, bforce);
         dssd->createDataset( datanormal.cols(), 1, "real");
         if( dssd->getDatasetptr() != nullptr) {
            dssd->writeDataset( Rcpp::wrap(datanormal.row(1)) );
         } else {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception bdNormalize_hdf5 : error with "<<strdatasetsd<< " datset\n";
            return(lst_return);
            // return void();
         }
         
         delete dssd; dssd = nullptr;
         delete dsmean; dsmean = nullptr;

         dsNormal = new BigDataStatMeth::hdf5Dataset(filename, strgroupout, dataset, bforce);
         dsNormal->createDataset( dsA, "real");
         
         if( dsA->getDatasetptr() != nullptr && dsNormal->getDatasetptr() != nullptr){
             BigDataStatMeth::RcppNormalizeHdf5( dsA, dsNormal, datanormal, wsize, bc, bs, bbyrows, corrected);
         }

         delete dsNormal; dsNormal = nullptr;
         delete dsA; dsA = nullptr;
         
         lst_return["fn"] = filename;
         lst_return["ds"] = strgroupout + "/" + dataset;
         lst_return["mean"] = strgroupout + "/" + strdatasetmean;
         lst_return["sd"] = strgroupout + "/" + strdatasetsd;
         
     } catch( H5::FileIException& error ) {
         checkClose_file(dsA, dsmean, dssd, dsNormal);
         Rcpp::Rcerr<<"c++ exception bdNormalize_hdf5 (File IException)";
     } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
         checkClose_file(dsA, dsmean, dssd, dsNormal);
         Rcpp::Rcerr<<"c++ exception bdNormalize_hdf5 (DataSet IException)";
     } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
         checkClose_file(dsA, dsmean, dssd, dsNormal);
         Rcpp::Rcerr<<"c++ exception bdNormalize_hdf5 (DataSpace IException)";
     } catch( H5::DataTypeIException& error ) { // catch failure caused by the DataSpace operations
         checkClose_file(dsA, dsmean, dssd, dsNormal);
         Rcpp::Rcerr<<"c++ exception bdNormalize_hdf5 (DataType IException)";
     } catch(std::exception &ex) {
         checkClose_file(dsA, dsmean, dssd, dsNormal);
         Rcpp::Rcerr<<"c++ exception bdNormalize_hdf5 : "<< ex.what();
     } catch (...) {
         checkClose_file(dsA, dsmean, dssd, dsNormal);
         Rcpp::Rcerr<<"C++ exception bdNormalize_hdf5 (unknown reason)";
     }

     return(lst_return);
     // return void();
}
