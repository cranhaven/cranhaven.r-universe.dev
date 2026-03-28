#include <BigDataStatMeth.hpp>
// #include "hdf5Utilities/hdf5Utilities.hpp"


//' Create hdf5 data file and write data to it
//'
//' Creates a hdf5 file with numerical data matrix,
//' 
//' @param filename, character array indicating the name of the file to create
//' @param object numerical data matrix
//' @param group, character array indicating folder name to put the matrix in hdf5 file
//' @param dataset, character array indicating the dataset name to store the matrix data
//' @param transp boolean, if trans=true matrix is stored transposed in hdf5 file
//' @param overwriteFile, optional boolean by default overwriteFile = false, if 
//' true and file exists, removes old file and creates a new file with de dataset 
//' data.
//' @param overwriteDataset, optional boolean by default overwriteDataset = false,  
//' if true and dataset exists, removes old dataset and creates a new dataset.
//' @param unlimited, optional boolean by default unlimited = false, if true 
//' creates a dataset that can growth.
//' @return List with components:
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the created matrix (group/dataset)}
//' }
//' 
//' @examples
//' 
//' matA <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), nrow = 3, byrow = TRUE)
//' bdCreate_hdf5_matrix(filename = "test_temp.hdf5", 
//'                     object = matA, group = "datasets", 
//'                     dataset = "datasetA", transp = FALSE, 
//'                     overwriteFile = TRUE, 
//'                     overwriteDataset = TRUE,
//'                     unlimited = FALSE)
//' 
//' # Remove file (used as example)
//'   if (file.exists("test_temp.hdf5")) {
//'     # Delete file if it exist
//'     file.remove("test_temp.hdf5")
//'   }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdCreate_hdf5_matrix(std::string filename, 
                          Rcpp::RObject object, 
                          Rcpp::Nullable<std::string> group = R_NilValue, 
                          Rcpp::Nullable<std::string> dataset = R_NilValue,
                          Rcpp::Nullable<bool> transp = R_NilValue, 
                          Rcpp::Nullable<bool> overwriteFile = R_NilValue,
                          Rcpp::Nullable<bool> overwriteDataset = R_NilValue,
                          Rcpp::Nullable<bool> unlimited = R_NilValue)
{
    
    
    BigDataStatMeth::hdf5Dataset* objDataset = nullptr;
    BigDataStatMeth::hdf5File* objFile = nullptr;
    BigDataStatMeth::hdf5Dims* dsdims = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try
    {
        
        H5::Exception::dontPrint();
        
        
        Rcpp::IntegerVector dims(2);
        Rcpp::CharacterVector svrows, svrcols;
        
        std::string strsubgroup, 
                    strdataset,
                    strdatatype;
        
        bool bforceFile, 
             bforceDataset, 
             bunlimited;
        
        int iRes;
        
        if(group.isNull())  strsubgroup = "INPUT" ;
        else    strsubgroup = Rcpp::as<std::string>(group);
        
        if(dataset.isNull())  strdataset = "A" ;
        else    strdataset = Rcpp::as<std::string>(dataset);
        
        if(unlimited.isNull())  bunlimited = false ;
        else    bunlimited = Rcpp::as<bool>(unlimited);
        
        if(overwriteDataset.isNull())  bforceDataset = false ;
        else    bforceDataset = Rcpp::as<bool>(overwriteDataset);
        
        if(overwriteFile.isNull())  bforceFile = false ;
        else    bforceFile = Rcpp::as<bool>(overwriteFile);
        
        
        strdatatype = BigDataStatMeth::getObjecDataType(object);
        
        if ( object.sexp_type()==0 ) {
            // throw std::range_error("Unknown data type");
            Rf_error("c++ exception bdCreate_hdf5_matrix - Unknown data type");
            return(lst_return);
        }
        
        if ( object.sexp_type()==0  ) {
            // throw std::range_error("Data matrix must exsits and mustn't be null");
            Rf_error("c++ exception bdCreate_hdf5_matrix - Data matrix must exsits and mustn't be null");
            return(lst_return);
        }
        
        dims = BigDataStatMeth::getObjectDims(object, strdatatype);
        
        objFile = new BigDataStatMeth::hdf5File(filename, bforceFile);
        iRes = objFile->createFile();
        
        if( (iRes == EXEC_OK) | (iRes == EXEC_WARNING)) {
            
            if(iRes == EXEC_WARNING) {
                objFile->openFile("rw");
            }
            
            objDataset = new BigDataStatMeth::hdf5Dataset(objFile, strsubgroup, strdataset, bforceDataset );
            if( bunlimited == false){
                objDataset->createDataset(dims[0], dims[1], strdatatype);
            } else{
                objDataset->createUnlimitedDataset(dims[0], dims[1], strdatatype);
            }
            
            if(Rf_inherits(object, "data.frame")){
                SEXP mat = Rcpp::Language("as.matrix", object).eval();
                if ( Rf_isMatrix(mat) ){
                    objDataset->writeDataset(Rcpp::as<Rcpp::NumericMatrix>(mat));
                } else{
                    Rf_error("c++ exception bdCreate_hdf5_matrix - Unknown data type");
                }
            } else{
                objDataset->writeDataset(object); 
            }
            
            Rcpp::List dimnames = object.attr( "dimnames" );
            
            if(dimnames.size()>0 ) {
                
                dsdims = new BigDataStatMeth::hdf5Dims(objDataset);
                
                if(!Rf_isNull(dimnames[0])) {
                    svrows = rownames(object);
                }
                
                if(!Rf_isNull(dimnames[1])) {
                    svrcols = colnames(object);
                }
                
                if( svrows.size() < dims[0]){
                    Rcpp::CharacterVector svrownames(1);
                    dsdims->writeDimnames( Rcpp::wrap(svrownames), Rcpp::wrap(svrcols));
                } else if(svrcols.size() < dims[1]){
                    Rcpp::CharacterVector svrcolnames(1);
                    dsdims->writeDimnames( svrows, svrcolnames);
                } else {
                    dsdims->writeDimnames( svrows, svrcols);
                }
            }
            
            delete dsdims; dsdims = nullptr;
            delete objDataset; objDataset = nullptr;
            delete objFile; objFile = nullptr;
            
            lst_return["fn"] = filename;
            lst_return["ds"] = strsubgroup + "/" + strdataset;
        } 
        
    }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        if(objFile != nullptr) delete objFile;
        if(dsdims != nullptr) delete dsdims;
        checkClose_file(objDataset);
        Rf_error("c++ c++ exception bdCreate_hdf5_matrix (File IException)");
        return(lst_return);
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        if(objFile != nullptr) delete objFile;
        if(dsdims != nullptr) delete dsdims;
        checkClose_file(objDataset);
        Rf_error( "c++ exception bdCreate_hdf5_matrix (DataSet IException)");
        return(lst_return);
    } catch(std::exception &ex) {
        if(objFile != nullptr) delete objFile;
        if(dsdims != nullptr) delete dsdims;
        checkClose_file(objDataset);
        Rf_error( "c++ exception bdCreate_hdf5_matrix %s", ex.what());
        return(lst_return);
    } 

    return(lst_return);
    
}

