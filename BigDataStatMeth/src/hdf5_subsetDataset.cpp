#include "BigDataStatMeth.hpp"


//' Create Subset of HDF5 Dataset
//'
//' @description
//' Creates a new HDF5 dataset containing only the specified rows or columns
//' from an existing dataset. This operation is memory efficient as it uses
//' HDF5's hyperslab selection for direct disk-to-disk copying without loading
//' the entire dataset into memory.
//'
//' @param filename Character string. Path to the HDF5 file
//' @param dataset_path Character string. Path to the source dataset (e.g., "/group1/dataset1")
//' @param indices Integer vector. Row or column indices to include (1-based, as per R convention)
//' @param select_rows Logical. If TRUE, selects rows; if FALSE, selects columns (default: TRUE)
//' @param new_group Character string. Target group for the new dataset (default: same as source)
//' @param new_name Character string. Name for the new dataset (default: original_name + "_subset")
//' @param overwrite Logical. Whether to overwrite destination if it exists (default: FALSE)
//'
//' @return Logical. TRUE on success, FALSE on failure
//'
//' @details
//' This function provides an efficient way to create subsets of large HDF5 datasets
//' without loading all data into memory. It uses HDF5's native hyperslab selection
//' mechanism for optimal performance with big data.
//'
//' Key features:
//' \itemize{
//'   \item Memory efficient - processes one row/column at a time
//'   \item Direct disk-to-disk copying using HDF5 hyperslab selection
//'   \item Preserves all dataset attributes and properties
//'   \item Works with datasets of any size
//'   \item Automatic creation of parent groups if needed
//'   \item Support for both row and column selection
//' }
//'
//' @section Index Convention:
//' Indices follow R's 1-based convention (first element is index 1), but are
//' automatically converted to HDF5's 0-based indexing internally.
//'
//' @section Performance:
//' This function is designed for big data scenarios. Memory usage is minimal
//' regardless of source dataset size, making it suitable for datasets that
//' don't fit in memory.
//'
//' @section Requirements:
//' \itemize{
//'   \item The HDF5 file must exist and be accessible
//'   \item The source dataset must exist and contain numeric data
//'   \item Indices must be valid (within dataset dimensions)
//'   \item User must have read-write permissions on the file
//' }
//'
//' @examples
//' \dontrun{
//' # Select specific rows (e.g., rows 1, 3, 5, 10-15)
//' success <- bdsubset_dataset("data.h5", 
//'                            dataset_path = "/matrix/data",
//'                            indices = c(1, 3, 5, 10:15),
//'                            select_rows = TRUE,
//'                            new_name = "selected_rows")
//'
//' # Select specific columns
//' success <- bdsubset_dataset("data.h5",
//'                            dataset_path = "/matrix/data", 
//'                            indices = c(2, 4, 6:10),
//'                            select_rows = FALSE,
//'                            new_group = "/filtered",
//'                            new_name = "selected_cols")
//'
//' # Create subset in different group
//' success <- bdsubset_dataset("data.h5",
//'                            dataset_path = "/raw_data/matrix",
//'                            indices = 1:100,  # First 100 rows
//'                            select_rows = TRUE,
//'                            new_group = "/processed",
//'                            new_name = "top_100_rows")
//'
//' # Extract specific samples for analysis
//' interesting_samples <- c(15, 23, 45, 67, 89, 123)
//' success <- bdsubset_dataset("data.h5",
//'                            dataset_path = "/experiments/results",
//'                            indices = interesting_samples,
//'                            select_rows = TRUE,
//'                            new_name = "analysis_subset")
//' }
//'
//' @family BigDataStatMeth HDF5 utilities
//' @author BigDataStatMeth package authors
//' @export
 // [[Rcpp::export]]
 bool bdsubset_hdf5_dataset(std::string filename,
                       std::string dataset_path,
                       Rcpp::IntegerVector indices,
                       bool select_rows = true,
                       std::string new_group = "",
                       std::string new_name = "",
                       bool overwrite = false)
 {
     BigDataStatMeth::hdf5Dataset* dataset = nullptr;
     
     try {
         
         H5::Exception::dontPrint();
         
         // Input validation
         if (filename.empty()) {
             Rcpp::Rcerr << "Error: filename cannot be empty" << std::endl;
             return false;
         }
         
         if (dataset_path.empty()) {
             Rcpp::Rcerr << "Error: dataset_path cannot be empty" << std::endl;
             return false;
         }
         
         if (indices.size() == 0) {
             Rcpp::Rcerr << "Error: indices vector cannot be empty" << std::endl;
             return false;
         }
         
         // Convert R indices (1-based) to C++ indices (0-based)
         // Este bucle no se puede vectorizar fácilmente en C++ sin perder legibilidad
         std::vector<int> cpp_indices;
         cpp_indices.reserve(indices.size());
         
         for (int i = 0; i < indices.size(); ++i) {
             if (indices[i] < 1) {
                 Rcpp::Rcerr << "Error: indices must be >= 1 (R convention)" << std::endl;
                 return false;
             }
             cpp_indices.push_back(indices[i] - 1);  // Convert to 0-based
         }
         
         // Create hdf5Dataset object
         dataset = new BigDataStatMeth::hdf5Dataset(filename, dataset_path, false);
         
         // Open the existing dataset
         dataset->openDataset();
         
         // Validate indices against dataset dimensions from R perspective
         hsize_t max_rows_r = dataset->nrows_r();  // Filas como las ve R
         hsize_t max_cols_r = dataset->ncols_r();  // Columnas como las ve R
         hsize_t max_index = select_rows ? max_rows_r : max_cols_r;
         
         for (int idx : cpp_indices) {
             if (idx >= max_index) {
                 delete dataset;
                 Rcpp::Rcerr << "Error: index " << (idx + 1) << " exceeds dataset dimensions (R perspective: " 
                             << max_rows_r << "x" << max_cols_r << ")" << std::endl;
                 return false;
             }
         }
         
         // Determine destination group and name
         std::string target_group = new_group.empty() ? dataset->getGroup() : new_group;
         std::string target_name = new_name.empty() ? dataset->getDatasetName() + "_subset" : new_name;
         std::string target_path = target_group + "/" + target_name;
         
         // Check if destination exists and handle overwrite
         BigDataStatMeth::hdf5File temp_file(filename, false);
         H5::H5File* file = temp_file.openFile("rw");
         
         if (BigDataStatMeth::exists_HDF5_element(file, target_path)) {
             if (!overwrite) {
                 delete dataset;
                 Rcpp::Rcerr << "Error: destination dataset already exists: " << target_path 
                             << ". Use overwrite=TRUE to replace it." << std::endl;
                 return false;
             } else {
                 // Remove existing destination
                 RcppRemove_hdf5_elements(&temp_file, {target_path});
             }
         }
         
         // Perform the subset operation
         dataset->createSubsetDataset(cpp_indices, select_rows, new_group, new_name);
         
         // Clean up
         delete dataset;
         
         return true;
         
     } catch (const std::exception& e) {
         BigDataStatMeth::checkClose_file(dataset);
         Rcpp::Rcerr << "Exception in bdsubset_hdf5_dataset: " << e.what() << std::endl;
         return false;
     } catch (...) {
         BigDataStatMeth::checkClose_file(dataset);
         Rcpp::Rcerr << "Unknown exception in bdsubset_hdf5_dataset" << std::endl;
         return false;
     }
 }
