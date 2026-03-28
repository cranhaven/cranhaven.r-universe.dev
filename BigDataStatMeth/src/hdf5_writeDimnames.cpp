/**
 * @brief Write dimension names for an existing HDF5 dataset.
 *
 * @details Writes row and/or column names metadata associated with an
 * existing dataset identified by @p filename, @p group, and @p dataset.
 * If @p rownames or @p colnames are empty vectors, the corresponding
 * dimnames are not written. When provided, lengths must match the dataset
 * dimensions (nrow and ncol respectively).
 *
 * @param filename Path to the HDF5 file.
 * @param group    Group path where the dataset lives.
 * @param dataset  Dataset name inside @p group.
 * @param rownames Character vector of row names. Use a zero-length vector
 *                 to skip writing row names.
 * @param colnames Character vector of column names. Use a zero-length
 *                 vector to skip writing column names.
 *
 * @return void. Called for side effects (metadata write).
 *
 * @pre The dataset @c group/dataset must already exist.
 * @pre If provided, @p rownames length equals nrow; @p colnames length
 *      equals ncol.
 *
 * @throws Rcpp::exception if the file/dataset cannot be opened or if the
 *         provided lengths do not match dataset dimensions.
 *
 * @since 0.99.0
 */

#include "BigDataStatMeth.hpp"



//' Write dimnames to an HDF5 dataset
//'
//' @description
//' Write row and/or column names metadata for an existing dataset in an
//' HDF5 file. Empty vectors skip the corresponding dimnames.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Group containing the dataset.
//' @param dataset Character string. Dataset name inside \code{group}.
//' @param rownames Character vector of row names. Use \code{character(0)}
//'   to skip writing row names. If provided, length must equal nrow.
//' @param colnames Character vector of column names. Use
//'   \code{character(0)} to skip writing column names. If provided,
//'   length must equal ncol.
//'
//' @details
//' The dataset \code{group/dataset} must already exist. When non-empty,
//' \code{rownames} and \code{colnames} lengths are validated against the
//' dataset dimensions.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{dsrows}{Character string with the full dataset path to the row names,
//'    stored as ".\code{dataset}_dimnames/1" within the specified group}
//'   \item{dscols}{Character string with the full dataset path to the column 
//'   names, stored as ".\code{dataset}_dimnames/2" within the specified group}
//' }
//'
//' @examples
//' \dontrun{
//' bdWrite_hdf5_dimnames(
//'   filename = "test.h5",
//'   group = "MGCCA_IN",
//'   dataset = "X",
//'   rownames = paste0("r", seq_len(100)),
//'   colnames = paste0("c", seq_len(50))
//' )
//'
//' # Skip column names:
//' bdWrite_hdf5_dimnames("test.h5", "MGCCA_IN", "X",
//'                       rownames = paste0("r", 1:100),
//'                       colnames = character(0))
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdWrite_hdf5_dimnames( std::string filename, 
                            std::string group, std::string dataset, 
                            Rcpp::StringVector rownames, 
                            Rcpp::StringVector colnames)
 { 
     
     BigDataStatMeth::hdf5Dataset* objDataset = nullptr;
     BigDataStatMeth::hdf5Dims* dsdims = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("dsrows") = "",
                                                Rcpp::Named("dscols") = "");

     try
     {
         
         H5::Exception::dontPrint();
         
         objDataset = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false );
         objDataset->openDataset();
         
         hsize_t* dims = objDataset->dim(); 
         
         dsdims = new BigDataStatMeth::hdf5Dims(objDataset);
         
         if( rownames.size() < dims[1]){
             Rcpp::CharacterVector svrownames(1);
             dsdims->writeDimnames( Rcpp::wrap(colnames), Rcpp::wrap(svrownames) );
         } else if(colnames.size() < dims[0]){
             Rcpp::CharacterVector svrcolnames(1);
             dsdims->writeDimnames( svrcolnames, rownames );
         } else {
             dsdims->writeDimnames( colnames, rownames);
         }
         
         delete dsdims; dsdims = nullptr;
         delete objDataset; objDataset = nullptr;
         
         lst_return["fn"] = filename;
         lst_return["dsrows"] = group + "/." + dataset + "/1";
         lst_return["dscols"] = group + "/." + dataset + "/2";
         
     } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
         if(dsdims != nullptr) delete dsdims;
         checkClose_file(objDataset);
         Rf_error("c++ c++ exception bdWrite_hdf5_dimnames (File IException)");
         return(lst_return);
     } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
         if(dsdims != nullptr) delete dsdims;
         checkClose_file(objDataset);
         Rf_error( "c++ exception bdWrite_hdf5_dimnames (DataSet IException)");
         return(lst_return);
     } catch(std::exception &ex) {
         if(dsdims != nullptr) delete dsdims;
         checkClose_file(objDataset);
         Rf_error( "c++ exception bdWrite_hdf5_dimnames %s", ex.what());
         return(lst_return);
     } 
     
     return(lst_return);
     
 }
