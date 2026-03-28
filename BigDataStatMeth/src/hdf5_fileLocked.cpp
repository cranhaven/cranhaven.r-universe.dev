#include "BigDataStatMeth.hpp"

// ----------------------------------------------------------------------
// Doxygen
// ----------------------------------------------------------------------
/**
 * @brief Check if an HDF5 file appears locked by another process.
 *
 * @details Enables HDF5 file locking and attempts to open @p filename
 * in read/write mode with a file-access property list that requests
 * locking (when available, HDF5 >= 1.12). If the open fails, the file
 * is considered "locked/busy". If the file does not exist, returns false.
 *
 * @param filename Path to the HDF5 file.
 * @return true if the file seems locked (cannot be opened RW with locking),
 *         false otherwise.
 *
 * @note This detects locks enforced by HDF5. On very old HDF5 versions
 * (without file locking), the result may be conservative.
 */
// ----------------------------------------------------------------------
// Roxygen
// ----------------------------------------------------------------------
//' Test whether an HDF5 file is locked (in use)
//'
//' @description
//' Uses HDF5 file locking to check if \code{filename} can be opened in
//' read/write mode. If opening fails under locking, the file is treated
//' as "in use" and \code{TRUE} is returned. Non-existent files return
//' \code{FALSE}.
//'
//' @param filename Character. Path to the HDF5 file.
//' @return Logical scalar: \code{TRUE} if locked/in use, \code{FALSE} otherwise.
//' @details Requires HDF5 file locking (HDF5 >= 1.12 recommended). The
//'   function sets \code{HDF5_USE_FILE_LOCKING=TRUE} for the process.
//' @examples
//' \dontrun{
//' if (bdIsFileLocked("data.h5")) stop("File in use")
//' }
//' @export
 // [[Rcpp::export]]
 bool bdIsLocked_hdf5(std::string filename) {
     
     BigDataStatMeth::hdf5File* objFile = nullptr;   
     bool locked = true;
     
     try {
         
         H5::Exception::dontPrint();
         
         objFile = new BigDataStatMeth::hdf5File(filename, false);
         locked = objFile->isLocked(filename);  
         
     } catch( H5::FileIException& error ) { 
         if(objFile != nullptr) delete objFile;
         Rf_error("c++ c++ exception bdIsLocked_hdf5 (File IException)");
         return locked;
     } catch( H5::DataSetIException& error ) { 
         if(objFile != nullptr) delete objFile;
         Rf_error( "c++ exception bdIsLocked_hdf5 (DataSet IException)");
         return locked;
     } catch(std::exception &ex) {
         if(objFile != nullptr) delete objFile;
         Rf_error( "c++ exception bdIsLocked_hdf5 %s", ex.what());
         return locked;
     } 
     
     return locked;
     
 }
