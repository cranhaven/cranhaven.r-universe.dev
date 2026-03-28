#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/multiplication.hpp"

//' Hdf5 datasets multiplication
//'
//' The bdblockmult_hdf5 function performs block-wise matrix multiplication 
//' between two matrices stored in an HDF5 file. This approach is also efficient 
//' for large matrices that cannot be fully loaded into memory.
//' 
//' @param filename string specifying the path to the HDF5 file
//' @param group string specifying the group within the HDF5 file containing 
//' matrix A.
//' @param A string specifying the dataset name for matrix A.
//' the data matrix to be used in calculus
//' @param B string specifying the dataset name for matrix B.
//' @param groupB string, (optional), An optional string specifying the group 
//' for matrix B. Defaults to the value of `group` if not provided.
//' @param transpose_A Whether to transpose matrix A
//' @param transpose_B Whether to transpose matrix B
//' @param block_size integer (optional), an optional parameter specifying the 
//' block size for processing the matrices. If not provided, a default block 
//' size is used. The block size should be chosen based on the available memory 
//' and the size of the matrices
//' @param paral boolean (optional), an optional parameter to enable parallel 
//' computation. Defaults to FALSE. Set `paral = true` to force parallel execution
//' @param threads integer (optional), an optional parameter specifying the 
//' number of threads to use if paral = TRUE. Ignored if paral = FALSE.
//' @param outgroup string (optional), An optional parameter specifying the group 
//' where the output matrix will be stored. If NULL, the output will be stored 
//' in the default group "OUTPUT".
//' @param outdataset string (optional), An optional parameter specifying the 
//' dataset name for the output matrix. If NULL, the default name will be 
//' constructed as the name of dataset A concatenated with _x_ and the 
//' name of dataset B.
//' @param overwrite logical (optional), An optional parameter to indicate whether 
//' existing results in the HDF5 file should be overwritten. Defaults to FALSE. 
//' If FALSE and the dataset already exists, an error will be displayed, and 
//' no calculations will be performed. If TRUE and a dataset with the same 
//' name as specified in outdataset already exists, it will be overwritten.
//' @details
//' * The function `bdblockmult_hdf5()` is efficient for both matrices that cannot 
//' fit into memory (by processing in blocks) and matrices that can be fully 
//' loaded into memory, as it optimizes computations based on available resources.
//' * Ensure that the dimensions of `A` and `B` matrices are compatible for 
//' matrix multiplication.
//' * The `block size` should be chosen based on the available memory and 
//' the size of the matrices.
//' * If `bparal = true`, number of concurrent threads in parallelization. If 
//' `paral = TRUE` and `threads = NULL` then `threads` is set to a half of a 
//' maximum number of available threads 
//' @return A list containing the location of the matrix multiplication result:
//'   \describe{
//'     \item{fn}{Character string. Path to the HDF5 file containing the result}
//'     \item{ds}{Character string. Full dataset path to the A*B multiplication result within the HDF5 file}
//'   }
//'   
//' @examples
//' library("BigDataStatMeth")
//' library("rhdf5")
//' 
//' N = 1000; M = 1000
//' 
//' set.seed(555)
//' a <- matrix( rnorm( N*M, mean=0, sd=1), N, M) 
//' b <- matrix( rnorm( N*M, mean=0, sd=1), M, N) 
//' 
//' fn <- "test_temp.hdf5"
//' bdCreate_hdf5_matrix(filename = fn, 
//'                      object = a, group = "groupA", 
//'                      dataset = "datasetA",
//'                      transp = FALSE,
//'                      overwriteFile = TRUE, 
//'                      overwriteDataset = FALSE, 
//'                      unlimited = FALSE)
//'                      
//' bdCreate_hdf5_matrix(filename = fn, 
//'                      object = t(b), 
//'                      group = "groupA", 
//'                      dataset = "datasetB",
//'                      transp = FALSE,
//'                      overwriteFile = FALSE, 
//'                      overwriteDataset = TRUE, 
//'                      unlimited = FALSE)
//'                      
//' # Multiply two matrix
//' res <- bdblockmult_hdf5(filename = fn, group = "groupA", 
//'     A = "datasetA", B = "datasetB", outgroup = "results", 
//'     outdataset = "res", overwrite = TRUE ) 
//'  
//' # list contents
//' h5ls(fn)
//' 
//' # Extract the result from HDF5
//' result_hdf5 <- h5read(res$fn, res$ds)[1:3, 1:5]
//' result_hdf5
//' 
//' # Compute the same multiplication in R
//' result_r <- (a %*% b)[1:3, 1:5]
//' result_r
//' 
//' # Compare both results (should be TRUE)
//' all.equal(result_hdf5, result_r)
//' 
//' # Remove file
//' if (file.exists(fn)) {
//'   file.remove(fn)
//' }
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bdblockmult_hdf5(std::string filename, 
                    std::string group, 
                    std::string A, 
                    std::string B,
                    Rcpp::Nullable<std::string> groupB = R_NilValue, 
                    Rcpp::Nullable<bool> transpose_A = R_NilValue,
                    Rcpp::Nullable<bool> transpose_B = R_NilValue,
                    Rcpp::Nullable<int> block_size = R_NilValue,
                    Rcpp::Nullable<bool> paral = R_NilValue,
                    Rcpp::Nullable<int> threads = R_NilValue,
                    Rcpp::Nullable<std::string> outgroup = R_NilValue,
                    Rcpp::Nullable<std::string> outdataset = R_NilValue,
                    Rcpp::Nullable<bool> overwrite = R_NilValue)
{
    
    
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsC = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try {
        
        H5::Exception::dontPrint();  
        
        bool bforce, btransA, btransB;
        
        std::string strsubgroupOut, 
        strdatasetOut, 
        strsubgroupIn,
        strsubgroupInB;
        
        strsubgroupIn = group;
        
        if (transpose_A.isNull()) { btransA = false; } 
        else { btransA = Rcpp::as<bool> (transpose_A); }
        
        if (transpose_B.isNull()) { btransB = false; } 
        else { btransB = Rcpp::as<bool> (transpose_B); }
        
        if( outgroup.isNull()) { strsubgroupOut = "OUTPUT";
        } else { strsubgroupOut = Rcpp::as<std::string> (outgroup); }
        
        if(groupB.isNotNull()){ strsubgroupInB =  Rcpp::as<std::string> (groupB) ; } 
        else { strsubgroupInB =  group; }
        
        if( outdataset.isNotNull()) { strdatasetOut =  Rcpp::as<std::string> (outdataset); } 
        else { 
            strdatasetOut = "";
            if(btransA == true) { strdatasetOut = strdatasetOut + "t_"; }
            strdatasetOut = strdatasetOut + A + "_x_";
            if(btransB == true) { strdatasetOut = strdatasetOut + "t_"; }
            strdatasetOut =  strdatasetOut + B; 
        }
        
        if (overwrite.isNull()) { bforce = false; } 
        else { bforce = Rcpp::as<bool> (overwrite); }
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupIn, A, false);
        dsA->openDataset();
        
        dsB = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupInB, B, false);
        dsB->openDataset();
        
        if( dsA->getDatasetptr() != nullptr && dsB->getDatasetptr() != nullptr) {
            dsC = new BigDataStatMeth::hdf5Dataset(filename, strsubgroupOut, strdatasetOut, bforce);
            BigDataStatMeth::multiplication(dsA, dsB, dsC, btransA, btransB, paral, block_size, threads); 
            
            lst_return["fn"] = filename;
            lst_return["ds"] = strsubgroupOut + "/" + strdatasetOut;
            
            delete dsC; dsC = nullptr;
        }
        
        delete dsA; dsA = nullptr;
        delete dsB; dsB = nullptr;
        
    }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"\nc++ c++ exception blockmult_hdf5 (File IException)\n";
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"\nc++ exception blockmult_hdf5 (DataSet IException)\n";
    } catch(std::exception &ex) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr << "c++ exception blockmult_hdf5: " << ex.what();
    }  catch (...) {
        checkClose_file(dsA, dsB, dsC);
        Rcpp::Rcerr<<"C++ exception blockmult_hdf5 (unknown reason)";
    }
    
    return(lst_return);
    // return Rcpp::List::create(Named("filename") = filename,
    //                     Named("dataset") = strsubgroupOut + "/" + strdatasetOut);
    // return void();
}

