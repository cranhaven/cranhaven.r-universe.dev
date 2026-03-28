#include <BigDataStatMeth.hpp>

// ----------------------------------------------------------------------
// Doxygen
// ----------------------------------------------------------------------
/**
 * @brief Create an empty HDF5 dataset using the header-only wrappers.
 *
 * @details Opens/creates the file (respecting @p overwriteFile),
 * ensures access in RW mode when needed, and creates an empty dataset
 * of size @p nrows × @p ncols inside @p group with name @p dataset.
 * No data are written. If the dataset exists and @p overwriteDataset
 * is FALSE/NULL, the function aborts. When TRUE, the dataset is
 * replaced. Optionally creates an unlimited dataset.
 *
 * @param filename Path to the HDF5 file.
 * @param group    Group path.
 * @param dataset  Dataset name.
 * @param nrows    Number of rows (>=1).
 * @param ncols    Number of columns (>=1).
 * @param overwriteFile     R logical (FALSE/TRUE). If TRUE, allow
 *                          recreating/truncating the file in constructor.
 * @param overwriteDataset  R logical (FALSE/TRUE). If TRUE, allow
 *                          replacing an existing dataset.
 * @param unlimited         R logical (FALSE/TRUE). If TRUE, create
 *                          an unlimited dataset.
 * @param datatype          Optional element type as string (e.g. "real",
 *                          "integer", "logical"); defaults to "real".
 *
 * @throws Rcpp::exception via Rf_error on error paths.
 *
 * @note Mirrors the control flow of bdCreate_hdf5_matrix(), but skips
 *       writeDataset() and dimnames writing.
 */
// ----------------------------------------------------------------------
// Roxygen
// ----------------------------------------------------------------------
//' Create an empty HDF5 dataset (no data written)
//'
//' @description
//' Creates an HDF5 dataset of size \code{nrows × ncols} inside \code{group}
//' with name \code{dataset}, without writing data (allocation only).
//' Honors file/dataset overwrite flags and supports unlimited datasets.
//'
//' @param filename Character. Path to the HDF5 file.
//' @param group Character. Group path.
//' @param dataset Character. Dataset name.
//' @param nrows Integer (>= 1). Number of rows.
//' @param ncols Integer (>= 1). Number of columns.
//' @param overwriteFile Logical. If \code{TRUE}, allow file recreate 
//' default value \code{FALSE}.
//' @param overwriteDataset Logical. If \code{TRUE}, replace dataset 
//' default value \code{FALSE}.
//' @param unlimited Logical. If \code{TRUE}, create unlimited dataset 
//' default value \code{FALSE}.
//' @param datatype Character. Element type (e.g., "real").
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the empty 
//'   dataset (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' bdCreate_hdf5_emptyDataset("test.h5", "MGCCA_IN", "X", 1000, 500,
//'                           overwriteFile = FALSE,
//'                           overwriteDataset = TRUE,
//'                           unlimited = FALSE,
//'                           datatype = "real")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdCreate_hdf5_emptyDataset(std::string filename, std::string group,
                                std::string dataset, int nrows = 0, int ncols = 0,
                                Rcpp::Nullable<bool> overwriteFile        = R_NilValue,
                                Rcpp::Nullable<bool> overwriteDataset     = R_NilValue,
                                Rcpp::Nullable<bool> unlimited            = R_NilValue,
                                Rcpp::Nullable<std::string> datatype      = R_NilValue)
 {
     using namespace BigDataStatMeth;
     
     hdf5Dataset* objDataset = nullptr;
     hdf5File*    objFile    = nullptr;
     
     Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                                Rcpp::Named("ds") = "");
     
     try {
         
         H5::Exception::dontPrint();
         
         if (nrows <= 0 || ncols <= 0)
             Rf_error("c++ exception bdCreate_hdf5_emptyDataset - "
                          "nrows and ncols must be positive");
         
         bool bforceFile        = overwriteFile.isNull()    ? false : Rcpp::as<bool>(overwriteFile);
         bool bforceDataset     = overwriteDataset.isNull() ? false : Rcpp::as<bool>(overwriteDataset);
         bool bunlimited        = unlimited.isNull()        ? false : Rcpp::as<bool>(unlimited);
         std::string strdatatype= datatype.isNull()         ? "real": Rcpp::as<std::string>(datatype);
         
         objFile = new hdf5File(filename, bforceFile);
         int iRes = objFile->createFile(); 
         
         if (iRes == EXEC_OK || iRes == EXEC_WARNING) {
             if (iRes == EXEC_WARNING) objFile->openFile("rw");
             
             objDataset = new hdf5Dataset(objFile, group, dataset, bforceDataset);
             
             if (!bunlimited)
                 objDataset->createDataset(nrows, ncols, strdatatype);
             else
                 objDataset->createUnlimitedDataset(nrows, ncols, strdatatype);
             
             // cleanup
             checkClose_file(objDataset);
             delete objDataset; objDataset = nullptr;
             delete objFile; objFile = nullptr;
             
             lst_return["fn"] = filename;
             lst_return["ds"] = group + "/" + dataset;
         }
         
     } catch (H5::FileIException&) {
         if (objFile)    { delete objFile; objFile = nullptr; }
         checkClose_file(objDataset);    // null-safe
         Rcpp::stop("c++ exception bdCreate_hdf5_emptyDataset (File IException)");
         return(lst_return);
         
     } catch (H5::DataSetIException&) {
         if (objFile)    { delete objFile; objFile = nullptr; }
         checkClose_file(objDataset);
         Rcpp::stop("c++ exception bdCreate_hdf5_emptyDataset (DataSet IException)");
         return(lst_return);
         
     } catch (std::exception& ex) {
         if (objFile)    { delete objFile; objFile = nullptr; }
         checkClose_file(objDataset);
         Rcpp::stop("c++ exception bdCreate_hdf5_emptyDataset %s", ex.what());
         return(lst_return);
     }
     
     return(lst_return);
 }
 
