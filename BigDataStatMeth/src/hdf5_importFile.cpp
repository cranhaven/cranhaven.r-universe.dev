#include <BigDataStatMeth.hpp>
// #include "hdf5Utilities/hdf5ImportFiles.hpp"

/**
 * @file hdf5_importFile.cpp
 * @brief Implementation of text file to HDF5 conversion functionality
 * @details This file provides functionality for importing text files into HDF5
 * format. The implementation supports:
 * - Flexible text file parsing
 * - Customizable separators
 * - Header and row name handling
 * - Parallel processing capabilities
 * - Memory-efficient import
 * 
 * Key features:
 * - Support for large text files
 * - Configurable import options
 * - Error handling and validation
 * - Parallel processing support
 * - Memory-efficient implementation
 */

/**
 * @brief Imports text file to HDF5 format
 * 
 * @details Implements efficient text file import functionality with support for
 * various text formats and import configurations. The function handles file
 * operations safely and provides comprehensive error handling.
 * 
 * Implementation features:
 * - Flexible text parsing options
 * - Support for headers and row names
 * - Parallel processing capabilities
 * - Memory-efficient import process
 * - Safe file operations
 * 
 * @param filename Path to input text file
 * @param outputfile Path to output HDF5 file
 * @param outGroup Target group in HDF5 file
 * @param outDataset Target dataset name
 * @param sep Field separator string
 * @param header Whether to process header row
 * @param rownames Whether to process row names
 * @param overwrite Whether to overwrite existing dataset
 * @param paral Whether to use parallel processing
 * @param threads Number of threads for parallel processing
 * @param overwriteFile Whether to overwrite existing HDF5 file
 * 
 * @throws std::runtime_error for runtime errors during import
 * @throws std::exception for other errors
 */

//' Import Text File to HDF5
//'
//' @description
//' Converts a text file (e.g., CSV, TSV) to HDF5 format, providing efficient
//' storage and access capabilities.
//'
//' @details
//' This function provides flexible text file import capabilities with support for:
//' 
//' * Input format options:
//'   - Custom field separators
//'   - Header row handling
//'   - Row names handling
//' 
//' * Processing options:
//'   - Parallel processing
//'   - Memory-efficient import
//'   - Configurable thread count
//' 
//' * File handling:
//'   - Safe file operations
//'   - Overwrite protection
//'   - Comprehensive error handling
//'
//' The function supports parallel processing for large files and provides
//' memory-efficient import capabilities.
//'
//' @param filename Character string. Path to the input text file.
//' @param outputfile Character string. Path to the output HDF5 file.
//' @param outGroup Character string. Name of the group to create in HDF5 file.
//' @param outDataset Character string. Name of the dataset to create.
//' @param sep Character string (optional). Field separator, default is "\\t".
//' @param header Logical (optional). Whether first row contains column names.
//' @param rownames Logical (optional). Whether first column contains row names.
//' @param overwrite Logical (optional). Whether to overwrite existing dataset.
//' @param paral Logical (optional). Whether to use parallel processing.
//' @param threads Integer (optional). Number of threads for parallel processing.
//' @param overwriteFile Logical (optional). Whether to overwrite existing HDF5 file.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the imported data (group/dataset)}
//'   \item{ds_rows}{Character string with the full dataset path to the row names}
//'   \item{ds_cols}{Character string with the full dataset path to the column names}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create a test CSV file
//' data <- matrix(rnorm(100), 10, 10)
//' write.csv(data, "test.csv", row.names = FALSE)
//' 
//' # Import to HDF5
//' bdImportTextFile_hdf5(
//'   filename = "test.csv",
//'   outputfile = "output.hdf5",
//'   outGroup = "data",
//'   outDataset = "matrix1",
//'   sep = ",",
//'   header = TRUE,
//'   overwriteFile = TRUE
//' )
//' 
//' # Cleanup
//' unlink(c("test.csv", "output.hdf5"))
//' }
//'
//' @references
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices directly
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdImportTextFile_hdf5( std::string filename,
                                  std::string outputfile, 
                                  std::string outGroup, std::string outDataset,
                                  Rcpp::Nullable<std::string> sep = R_NilValue,
                                  Rcpp::Nullable<bool> header = false,
                                  Rcpp::Nullable<bool> rownames = false,
                                  Rcpp::Nullable<bool> overwrite = false,
                                  Rcpp::Nullable<bool> paral = R_NilValue,
                                  Rcpp::Nullable<int> threads = R_NilValue,
                                  Rcpp::Nullable<bool> overwriteFile = R_NilValue)
{

    BigDataStatMeth::hdf5File* objFile = nullptr;
    BigDataStatMeth::hdf5Dataset* datasetOut = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "",
                                               Rcpp::Named("ds_rows") = "",
                                               Rcpp::Named("ds_cols") = "");

    try{
        
        H5::Exception::dontPrint();
        
        bool boverwrite, bforceFile;
        
        if( overwrite.isNull()) { boverwrite = false; 
        } else { boverwrite = Rcpp::as<bool> (overwrite); }
        
        if(overwriteFile.isNull())  bforceFile = false ;
        else    bforceFile = Rcpp::as<bool>(overwriteFile);
        
        // Check if exists file to import
        if( BigDataStatMeth::Rcpp_FileExist(filename) ) {

            // Create file if does not exists
            objFile = new BigDataStatMeth::hdf5File(outputfile, bforceFile);
            int iRes = objFile->createFile();

            if(iRes == EXEC_WARNING) {
                objFile->openFile("rw");
            } 
            
            if( objFile->getFileptr() != nullptr ) {
                // Create dataset
                datasetOut = new BigDataStatMeth::hdf5Dataset(objFile, outGroup, outDataset, boverwrite);
                // datasetOut->openDataset();
                
                Rcpp_Import_File_to_hdf5( filename, datasetOut, sep, header, rownames, paral, threads) ;
                
                
            } else {
                Rcpp::Rcerr << "c++ exception bdImportTextFile_hdf5: " << "Error opening file";
                delete objFile; objFile = nullptr;
                return(lst_return);
            }

        } else {
            Rcpp::Rcerr << "c++ exception bdImportTextFile_hdf5: " << "File "<<filename<<" doesn't exists, please, review location";
            delete objFile; objFile = nullptr;
            return(lst_return);
        }
        
        lst_return["fn"] = outputfile;
        lst_return["ds"] = outGroup + "/" + outDataset;
        
        if(Rcpp::as<bool>(header) == true) {
            lst_return["ds_cols"] = outGroup + "/." + outDataset + "_dimnames/2";
            
        }
        if(Rcpp::as<bool>(rownames) == true) {
            lst_return["ds_rows"] = outGroup + "/." + outDataset + "_dimnames/1";
        }

        delete datasetOut; datasetOut = nullptr;
        delete objFile; objFile = nullptr;

    } catch(const std::runtime_error& re) {
        checkClose_file(datasetOut);
        if(objFile != nullptr) { delete objFile; objFile = nullptr; }
        Rcpp::Rcerr << "c++ exception bdImportTextFile_hdf5 - Runtime error: " << re.what() << std::endl;
        return(lst_return);
    } catch(const std::exception& ex) {
        checkClose_file(datasetOut);
        if(objFile != nullptr) { delete objFile; objFile = nullptr; }
        Rcpp::Rcerr << "c++ exception bdImportTextFile_hdf5 - Error occurred: " << ex.what() << std::endl;
        return(lst_return);
    } catch(...) {
        // catch any other errors (that we have no information about)
        checkClose_file(datasetOut);
        if(objFile != nullptr) { delete objFile; objFile = nullptr; }
        Rcpp::Rcerr << "c++ exception bdImportTextFile_hdf5 - Unknown failure occurred. Possible memory corruption" << std::endl;
        return(lst_return);
    }
    
    Rcpp::message(Rcpp::wrap("The file has been imported"));
    return(lst_return);
    // return void();

}
