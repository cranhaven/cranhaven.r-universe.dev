#include <BigDataStatMeth.hpp>
#include "hdf5Omics/hdf5RemoveMAF.hpp"

/**
 * @file hdf5_removeMAF.cpp
 * @brief Implementation of MAF-based SNP filtering for HDF5-stored genomic data
 * @details This file provides functionality for filtering SNPs (Single Nucleotide
 * Polymorphisms) based on Minor Allele Frequency (MAF) in genomic data stored
 * in HDF5 format. The implementation supports:
 * - MAF-based filtering
 * - Row-wise and column-wise processing
 * - Block-based processing
 * - Memory-efficient operations
 * 
 * Key features:
 * - Support for large genomic datasets
 * - Configurable MAF threshold
 * - Block-based processing
 * - Memory-efficient implementation
 * - Comprehensive error handling
 */

/**
 * @brief Removes SNPs based on Minor Allele Frequency
 * 
 * @details Implements efficient filtering of SNPs based on MAF in genomic data
 * stored in HDF5 format. The function supports both row-wise and column-wise
 * processing with block-based operations for memory efficiency.
 * 
 * Implementation features:
 * - MAF threshold-based filtering
 * - Block-based processing
 * - Memory-efficient operations
 * - Safe file operations
 * - Comprehensive error handling
 * 
 * @param filename Path to HDF5 file
 * @param group Input group containing dataset
 * @param dataset Input dataset name
 * @param outgroup Output group for results
 * @param outdataset Output dataset name
 * @param maf MAF threshold for filtering
 * @param bycols Whether to process by columns
 * @param blocksize Block size for processing
 * @param overwrite Whether to overwrite existing dataset
 * 
 * @throws H5::FileIException for HDF5 file operation errors
 * @throws H5::DataSetIException for HDF5 dataset operation errors
 * @throws H5::DataSpaceIException for HDF5 dataspace errors
 * @throws H5::DataTypeIException for HDF5 datatype errors
 * @throws std::exception for other errors
 */

//' Remove SNPs Based on Minor Allele Frequency
//'
//' @description
//' Filters SNPs (Single Nucleotide Polymorphisms) based on Minor Allele
//' Frequency (MAF) in genomic data stored in HDF5 format.
//'
//' @details
//' This function provides efficient MAF-based filtering capabilities with:
//' 
//' * Filtering options:
//'   - MAF threshold-based filtering
//'   - Row-wise or column-wise processing
//'   - Block-based processing
//' 
//' * Implementation features:
//'   - Memory-efficient processing
//'   - Block-based operations
//'   - Safe file operations
//'   - Progress reporting
//'
//' The function supports both in-place modification and creation of new datasets.
//'
//' @param filename Character string. Path to the HDF5 file.
//' @param group Character string. Path to the group containing input dataset.
//' @param dataset Character string. Name of the dataset to filter.
//' @param outgroup Character string. Output group path for filtered data.
//' @param outdataset Character string. Output dataset name for filtered data.
//' @param maf Numeric (optional). MAF threshold for filtering (0-1).
//'   Default is 0.05. SNPs with MAF above this threshold are removed.
//' @param bycols Logical (optional). Whether to process by columns (TRUE) or
//'   rows (FALSE). Default is FALSE.
//' @param blocksize Integer (optional). Block size for processing. Default is 100.
//'   Larger values use more memory but may be faster.
//' @param overwrite Logical (optional). Whether to overwrite existing dataset.
//'   Default is FALSE.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the filtered dataset (group/dataset)}
//'   \item{nremoved}{Integer with the number of SNPs removed due to low Minor Allele Frequency (MAF)}
//' }
//'
//' @examples
//' \dontrun{
//' library(BigDataStatMeth)
//' 
//' # Create test SNP data
//' snps <- matrix(sample(c(0, 1, 2), 1000, replace = TRUE,
//'                      prob = c(0.7, 0.2, 0.1)), 100, 10)
//' 
//' # Save to HDF5
//' fn <- "snp_data.hdf5"
//' bdCreate_hdf5_matrix(fn, snps, "genotype", "raw_snps",
//'                      overwriteFile = TRUE)
//' 
//' # Remove SNPs with high MAF
//' bdRemoveMAF_hdf5(
//'   filename = fn,
//'   group = "genotype",
//'   dataset = "raw_snps",
//'   outgroup = "genotype_filtered",
//'   outdataset = "filtered_snps",
//'   maf = 0.1,
//'   bycols = TRUE,
//'   blocksize = 50
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
//' * Marees, A. T., et al. (2018). A tutorial on conducting genome‐wide
//'   association studies: Quality control and statistical analysis. International
//'   Journal of Methods in Psychiatric Research, 27(2), e1608.
//'
//' @seealso
//' * \code{\link{bdRemovelowdata_hdf5}} for removing low-representation SNPs
//' * \code{\link{bdImputeSNPs_hdf5}} for imputing missing SNP values
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdRemoveMAF_hdf5( std::string filename, std::string group, std::string dataset, 
                       std::string outgroup, std::string outdataset, 
                       Rcpp::Nullable<double> maf, Rcpp::Nullable<bool> bycols, 
                       Rcpp::Nullable<int> blocksize, Rcpp::Nullable<bool> overwrite = R_NilValue )
{
    
    
    BigDataStatMeth::hdf5Dataset* dsIn = nullptr;
    BigDataStatMeth::hdf5DatasetInternal* dsOut = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "",
                                               Rcpp::Named("nremoved") = "");
    
    try
    {
        
        H5::Exception::dontPrint();
        
        int iremoved = 0;    
        bool bcols, bforce;
        double dpcent;
        int iblocksize = 100;
        
        std::string stroutdata = outgroup +"/" + outdataset;
        std::string strdataset = group +"/" + dataset;
        
        if(bycols.isNull()){  
            bcols = false ;
        }else{    
            bcols = Rcpp::as<bool>(bycols);
        }
        
        if(maf.isNull()){ dpcent = 0.05 ; } 
        else { dpcent = Rcpp::as<double>(maf); }
        
        if(!blocksize.isNull()){  
            iblocksize = Rcpp::as<int>(blocksize);
        }
        
        if(overwrite.isNull()) { bforce = false ; }
        else { bforce = Rcpp::as<bool>(overwrite); }
        
            
        if( strdataset.compare(stroutdata)!= 0)
        {
            dsIn = new BigDataStatMeth::hdf5Dataset(filename, group, dataset, false);
            dsIn->openDataset();
            
            dsOut = new BigDataStatMeth::hdf5DatasetInternal(filename, outgroup, outdataset, bforce);
            
        } else {
            throw std::range_error("Input and output dataset must be different");  
            return(lst_return);
        }
        
        if( dsIn->getDatasetptr() != nullptr) {
            iremoved = Rcpp_Remove_MAF_hdf5( dsIn, dsOut, bcols, dpcent, iblocksize);
        } else {
            checkClose_file(dsIn, dsOut);
            throw std::range_error("File does not exist");
            return(lst_return);
        }
        
        delete dsIn; dsIn = nullptr;
        delete dsOut; dsOut = nullptr;
        
        lst_return["fn"] = filename;
        lst_return["ds"] = outgroup + "/" + outdataset;
        lst_return["nremoved"] = iremoved;
        
        Rcpp::Function warning("warning");
        if (!bcols )
            warning( std::to_string(iremoved) + " Rows have been removed");
        else
            warning( std::to_string(iremoved) + " Columns have been removed");
        
    } catch( H5::FileIException& error ){
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ c++ exception bdRemoveMAF_hdf5 (File IException)";
        return(lst_return);
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ c++ exception bdRemoveMAF_hdf5 (DataSet IException)";
        return(lst_return);
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ c++ exception bdRemoveMAF_hdf5 (DataSpace IException)";
        return(lst_return);
    } catch( H5::DataTypeIException& error ) { 
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ c++ exception bdRemoveMAF_hdf5 (DataType IException)";
        return(lst_return);
    } catch(std::exception &ex) {
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ c++ exception bdRemoveMAF_hdf5: "<< ex.what();
        return(lst_return);
    }  catch (...) {
        checkClose_file(dsIn, dsOut);
        Rcpp::Rcerr<<"c++ exception bdRemoveMAF_hdf5 (unknown reason)";
        return(lst_return);
    }
    
    return(lst_return);
    
}

