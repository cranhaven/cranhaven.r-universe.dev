#include "BigDataStatMeth.hpp"

//' Move HDF5 Dataset
//'
//' @description
//' Moves an HDF5 dataset from one location to another within the same HDF5 file.
//' This function automatically handles moving associated rownames and colnames 
//' datasets, creates parent groups if needed, and updates all internal references.
//'
//' @param filename Character string. Path to the HDF5 file
//' @param source_path Character string. Current path to the dataset (e.g., "/group1/dataset1")
//' @param dest_path Character string. New path for the dataset (e.g., "/group2/new_name")
//' @param overwrite Logical. Whether to overwrite destination if it exists (default: FALSE)
//'
//' @return List with components. If an error occurs, all string values are 
//' returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the moved dataset 
//'   in its new location (group/dataset)}
//' }
//'
//' @details
//' This function provides a high-level interface for moving datasets within HDF5 files.
//' The operation is efficient as it uses HDF5's native linking mechanism without 
//' copying actual data.
//'
//' Key features:
//' \itemize{
//'   \item Moves main dataset and associated rownames/colnames datasets
//'   \item Creates parent directory structure automatically
//'   \item Preserves all dataset attributes and properties
//'   \item Updates internal dataset references
//'   \item Efficient metadata-only operation
//'   \item Comprehensive error handling
//' }
//'
//' @section Behavior:
//' \itemize{
//'   \item If the destination parent groups don't exist, they will be created automatically
//'   \item Associated rownames and colnames datasets are moved to the same new group
//'   \item All dataset attributes and properties are preserved during the move
//'   \item The operation is atomic - either all elements move successfully or none do
//' }
//'
//' @section Requirements:
//' \itemize{
//'   \item The HDF5 file must exist and be accessible
//'   \item The source dataset must exist
//'   \item The file must not be locked by another process
//'   \item User must have read-write permissions on the file
//' }
//'
//' @examples
//' \dontrun{
//' # Move dataset to a different group
//' success <- bdmove_hdf5_dataset("data.h5", 
//'                          source_path = "/old_group/my_dataset",
//'                          dest_path = "/new_group/my_dataset")
//'
//' # Rename dataset within the same group
//' success <- bdmove_hdf5_dataset("data.h5",
//'                          source_path = "/data/old_name", 
//'                          dest_path = "/data/new_name",
//'                          overwrite = TRUE)
//'
//' # Move dataset to root level
//' success <- bdmove_hdf5_dataset("data.h5",
//'                          source_path = "/deep/nested/dataset",
//'                          dest_path = "/dataset")
//'
//' # Move with automatic group creation
//' success <- bdmove_hdf5_dataset("data.h5",
//'                          source_path = "/old_location/dataset",
//'                          dest_path = "/new/deep/structure/dataset")
//' }
//'
//' @family BigDataStatMeth HDF5 utilities
//' @author BigDataStatMeth package authors
//' @export
 // [[Rcpp::export]]
Rcpp::List bdmove_hdf5_dataset(std::string filename,
                     std::string source_path,
                     std::string dest_path, 
                     bool overwrite = false)
{
    
    BigDataStatMeth::hdf5Dataset* dataset = nullptr;
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
     
     try {
         
         H5::Exception::dontPrint();
         
         // Input validation
         if (filename.empty()) {
             Rcpp::Rcerr << "Error: filename cannot be empty" << std::endl;
             return(lst_return);
         }
         
         if (source_path.empty()) {
             Rcpp::Rcerr << "Error: source_path cannot be empty" << std::endl;
             return(lst_return);
         }
         
         if (dest_path.empty()) {
             Rcpp::Rcerr << "Error: dest_path cannot be empty" << std::endl;
             return(lst_return);
         }
         
         if (source_path == dest_path) {
             Rcpp::Rcerr << "Error: source_path and dest_path cannot be the same" << std::endl;
             return(lst_return);
         }
         
         // Create hdf5Dataset object with current path
         dataset = new BigDataStatMeth::hdf5Dataset(filename, source_path, false);
         
         // Open the existing dataset
         dataset->openDataset();
         
         // Perform the move operation
         dataset->moveDataset(dest_path, overwrite);
         
         // Clean up
         delete dataset; dataset = nullptr;
         
         lst_return["fn"] = filename;
         lst_return["ds"] = dest_path;
         
         return(lst_return);
         
     } catch (const std::exception& e) {
         if (dataset) {
             delete dataset; dataset = nullptr;
         }
         Rcpp::Rcerr << "Exception in bdmove_hdf5_dataset: " << e.what() << std::endl;
         return(lst_return);
     } catch (...) {
         if (dataset) {
             delete dataset; dataset = nullptr;
         }
         Rcpp::Rcerr << "Unknown exception in bdmove_hdf5_dataset" << std::endl;
         return(lst_return);
     }
 }
