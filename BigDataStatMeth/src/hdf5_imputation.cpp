#include <BigDataStatMeth.hpp>
#include "hdf5Utilities/hdf5ImputeData.hpp"

/**
 * @file hdf5_imputation.cpp
 * @brief Implementation of SNP imputation for HDF5-stored genomic data
 * @details This file provides functionality for imputing missing values in
 * SNP (Single Nucleotide Polymorphism) data stored in HDF5 format. The
 * implementation supports:
 * - Row-wise and column-wise imputation
 * - Parallel processing capabilities
 * - Memory-efficient operations
 * - Flexible output options
 * 
 * Key features:
 * - Support for large genomic datasets
 * - Configurable imputation direction
 * - Error handling and validation
 * - Parallel processing support
 * - Memory-efficient implementation
 */

/**
 * @brief Imputes missing SNP values in HDF5 datasets
 * 
 * @details Implements efficient SNP imputation for genomic data stored in
 * HDF5 format. The function supports both row-wise and column-wise imputation
 * with parallel processing capabilities.
 * 
 * Implementation features:
 * - Flexible imputation direction (row/column)
 * - Support for parallel processing
 * - Memory-efficient operations
 * - Safe file operations
 * - Comprehensive error handling
 * 
 * @param filename Path to HDF5 file
 * @param group Input group containing dataset
 * @param dataset Input dataset name
 * @param outgroup Output group for results
 * @param outdataset Output dataset name
 * @param bycols Whether to impute by columns
 * @param paral Whether to use parallel processing
 * @param threads Number of threads for parallel processing
 * @param overwrite Whether to overwrite existing dataset
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws H5::DataSpaceIException for HDF5 dataspace errors
 * @throws H5::DataTypeIException for HDF5 datatype errors
 * @throws std::exception for other errors
 */

//' Impute Missing SNP Values in HDF5 Dataset
//'
//' @description
//' Performs imputation of missing values in SNP (Single Nucleotide Polymorphism)
//' data stored in HDF5 format.
//'
//' @details
//' This function provides efficient imputation capabilities for genomic data with
//' support for:
//' 
//' * Imputation options:
//'   - Row-wise or column-wise imputation
//'   - Parallel processing
//'   - Configurable thread count
//' 
//' * Output options:
//'   - Custom output location
//'   - In-place modification
//'   - Overwrite protection
//' 
//' * Implementation features:
//'   - Memory-efficient processing
//'   - Safe file operations
//'   - Error handling
//'
//' The function supports both in-place modification and creation of new datasets.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing input dataset.
//' @param dataset Character string. Name of the dataset to impute.
//' @param outgroup Character string (optional). Output group path. If NULL,
//'   uses input group.
//' @param outdataset Character string (optional). Output dataset name. If NULL,
//'   overwrites input dataset.
//' @param bycols Logical (optional). Whether to impute by columns (TRUE) or
//'   rows (FALSE). Default is TRUE.
//' @param paral Logical (optional). Whether to use parallel processing.
//' @param threads Integer (optional). Number of threads for parallel processing.
//' @param overwrite Logical (optional). Whether to overwrite existing dataset.
//'
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the imputed data (group/dataset)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test data with missing values
//' data <- matrix(sample(c(0, 1, 2, NA), 100, replace = TRUE), 10, 10)
//' 
//' # Save to HDF5
//' fn <- "snp_data.hdf5"
//' bdCreate_hdf5_matrix(fn, data, "genotype", "snps",
//'                      overwriteFile = TRUE)
//' 
//' # Impute missing values
//' bdImputeSNPs_hdf5(
//'   filename = fn,
//'   group = "genotype",
//'   dataset = "snps",
//'   outgroup = "genotype_imputed",
//'   outdataset = "snps_complete",
//'   bycols = TRUE,
//'   paral = TRUE
//' )
//' 
//' # Cleanup
//' if (file.exists(fn)) {
//'   file.remove(fn)
//' }
//' }
//'
//' @references
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//' * Li, Y., et al. (2009). Genotype Imputation. Annual Review of Genomics
//'   and Human Genetics, 10, 387-406.
//'
//' @seealso
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdImputeSNPs_hdf5(std::string filename, std::string group, std::string dataset, 
                        Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                        Rcpp::Nullable<std::string> outdataset = R_NilValue, 
                        Rcpp::Nullable<bool> bycols = true, 
                        Rcpp::Nullable<bool> paral = R_NilValue,
                        Rcpp::Nullable<int> threads = R_NilValue, 
                        Rcpp::Nullable<bool> overwrite = R_NilValue )
{
    
    BigDataStatMeth::hdf5Dataset* dsIn = nullptr;
    BigDataStatMeth::hdf5DatasetInternal* dsOut = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try
    {
        
        H5::Exception::dontPrint();
        
        std::string strdataset = group +"/" + dataset;
        std::string stroutgroup, stroutdataset, stroutdata;
        
        bool bcols, bforce;
        
        if(bycols.isNull()){  bcols = true ; }
        else{  bcols = Rcpp::as<bool>(bycols); }
        
        if(outgroup.isNull())  stroutgroup = group ;
        else    stroutgroup = Rcpp::as<std::string>(outgroup);
        
        if(outdataset.isNull())  stroutdataset = dataset ;
        else    stroutdataset = Rcpp::as<std::string>(outdataset);
        
        if(overwrite.isNull()) { bforce = false ; }
        else { bforce = Rcpp::as<bool>(overwrite); }
        
        stroutdata = stroutgroup + "/" + stroutdataset;
        
        dsIn = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
        dsIn->openDataset();
        
        if( dsIn->getDatasetptr() != nullptr ) {
            dsOut = new BigDataStatMeth::hdf5DatasetInternal(filename, stroutgroup, stroutdataset, bforce);
            Rcpp_Impute_snps_hdf5( dsIn, dsOut, bcols, stroutdataset, threads);
            delete dsOut; dsOut = nullptr;
        } else {
            delete dsIn; dsIn = nullptr;
            Rf_error("c++ exception bdImputeSNPs_hdf5: Error opening %s dataset ",dataset.c_str());
            return(lst_return);
            // return void();
        }
        
        delete dsIn; dsIn = nullptr;
        
        lst_return["fn"] = filename;
        lst_return["ds"] = group + "/" + dataset;
        
    } catch( H5::FileIException& error ){
        checkClose_file(dsIn, dsOut);
        Rf_error("c++ exception bdImputeSNPs_hdf5 (File IException)");
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dsIn, dsOut);
        Rf_error("c++ exception bdImputeSNPs_hdf5 (DataSet IException)");
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dsIn, dsOut);
        Rf_error("c++ exception bdImputeSNPs_hdf5 (DataSpace IException)");
    } catch( H5::DataTypeIException&    error ) { 
        checkClose_file(dsIn, dsOut);
        Rf_error("c++ c++ exception bdImputeSNPs_hdf5 (DataType IException)");
    } catch(std::exception &ex) {   
        checkClose_file(dsIn, dsOut);
        Rf_error( "c++ exception bdImputeSNPs_hdf5 : %s", ex.what());
    }  catch (...) {
        checkClose_file(dsIn, dsOut);
        Rf_error("c++ exception bdImputeSNPs_hdf5 (unknown reason)");
    }
    
    // Rcpp::Rcout<<"SNPs with missing values has been imputed\n";
    // return void();
    return(lst_return);
}

