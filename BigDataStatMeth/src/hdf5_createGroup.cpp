#include <BigDataStatMeth.hpp>


/**
 * @file hdf5_createGroup.cpp
 * @brief Implementation of group creation for HDF5 files
 * @details Provides utilities to create (nested) groups inside HDF5 files.
 * Supported operations:
 * - Single group creation
 * - Nested group creation (creates intermediate parents)
 * - Idempotent behavior (no error if the group already exists)
 *
 * Key features:
 * - Safe file operations
 * - Path validation
 * - Comprehensive error handling
 * - Memory-safe implementation
 */

/**
 * @brief Create (nested) group in an HDF5 file
 *
 * @details Ensures that the group @p group exists inside @p filename.
 * If intermediate path segments are missing (e.g., "A/B/C"), they are
 * created. If the final group already exists, the function returns
 * without error (idempotent).
 *
 * @param filename Path to the HDF5 file (must exist).
 * @param group Group path to create (e.g., "MGCCA_OUT/scores").
 *
 * @throws H5::FileIException   for HDF5 file operation errors
 * @throws H5::GroupIException  for HDF5 group operation errors
 * @throws std::exception       for other errors
 *
 * @note Create the file beforehand (e.g., via a matrix writer) if it
 *       does not exist.
 *
 * @code
 * // Create nested group: MGCCA_OUT/scores
 * bdCreate_hdf5_group("test.hdf5", "MGCCA_OUT/scores");
 * @endcode
 */


//' Create Group in an HDF5 File
//'
//' @description
//' Create a (nested) group inside an HDF5 file. The operation is
//' idempotent: if the group already exists, no error is raised.
//'
//' @details
//' Intermediate groups are created when needed. The HDF5 file must
//' exist prior to the call (create it with a writer function).
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Group path to create
//'   (e.g., `"MGCCA_OUT/scores"`).
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{gr}{Character string with the full group path created within the 
//'   HDF5 file}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' fn <- "test.hdf5"
//'
//' # Ensure file exists (e.g., by creating an empty dataset or via a helper)
//' mat <- matrix(0, nrow = 1, ncol = 1)
//' bdCreate_hdf5_matrix(fn, mat, group = "tmp", dataset = "seed",
//'                      overwriteFile = TRUE)
//'
//' # Create nested group
//' bdCreate_hdf5_group(fn, "MGCCA_OUT/scores")
//' }
//'
//' @references
//' The HDF Group. HDF5 User's Guide.
//'
//' @seealso
//' \code{\link{bdCreate_hdf5_matrix}}, \code{\link{bdRemove_hdf5_element}}
//'
//' @export
// [[Rcpp::export]]

Rcpp::List bdCreate_hdf5_group(std::string filename, std::string group)
 {
     
     BigDataStatMeth::hdf5File* objFile = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("gr") = "");
     
     try
     {
         
         H5::Exception::dontPrint();
         
         objFile = new BigDataStatMeth::hdf5File(filename, false);
         objFile->openFile("rw");
         
         if( BigDataStatMeth::exists_HDF5_element(objFile->getFileptr(),  group))  {
             Rcpp::Rcout<<"Element exists, nothing to do";
         } else {
             // Normalize: drop leading '/' so we build relative path segments
             if (!group.empty() && group.front() == '/') group.erase(0, 1);
             
             std::string acc;                     // accumulator: "A", then "A/B", ...
             size_t i = 0;
             while (i <= group.size()) {
                 size_t j = group.find('/', i);
                 std::string seg = group.substr(i, (j == std::string::npos) ? std::string::npos : j - i);
                 if (!seg.empty()) {
                     acc = acc.empty() ? seg : (acc + "/" + seg);
                     
                     try { objFile->getFileptr()->openGroup(acc); }
                     catch (const H5::Exception&) { objFile->getFileptr()->createGroup(acc); }
                 }
                 if (j == std::string::npos) break;
                 i = j + 1;
             }
         }
         
         delete objFile; objFile = nullptr;
         
         lst_return["fn"] = filename;
         lst_return["gr"] = group;
         
     } catch( H5::FileIException& error ) { 
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "c++ exception bdCreate_hdf5_group (File IException)";
         return(lst_return);
     } catch( H5::GroupIException & error ) { 
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "c++ exception bdCreate_hdf5_group (Group IException)";
         return(lst_return);
     } catch( H5::DataSetIException& error ) { 
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "c++ exception bdCreate_hdf5_group (DataSet IException)";
         return(lst_return);
     } catch(std::exception& ex) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "c++ exception bdCreate_hdf5_group" << ex.what();
         return(lst_return);
     } catch (...) {
         delete objFile; objFile = nullptr;
         Rcpp::Rcerr << "c++ exception bdCreate_hdf5_group (unknown reason)";
         return(lst_return);
     }
     
     return(lst_return);
     
 }
