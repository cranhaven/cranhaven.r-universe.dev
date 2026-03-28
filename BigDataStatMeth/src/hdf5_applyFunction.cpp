/**
 * @file hdf5_applyFunction.cpp
 * @brief Advanced mathematical operations on HDF5 datasets
 * 
 * This file implements high-level mathematical operations that can be applied to
 * HDF5 datasets. It provides a unified interface for various matrix operations
 * including decompositions, products, and statistical computations.
 * 
 * Key features:
 * - QR decomposition
 * - Cross products and transposed cross products
 * - Matrix multiplication
 * - Cholesky decomposition and inverse
 * - Matrix equation solving
 * - Statistical measures (mean, standard deviation)
 * 
 * The implementation focuses on:
 * - Memory efficiency through block-wise processing
 * - Parallel computation support
 * - Flexible dataset handling
 * - Comprehensive error management
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#include <BigDataStatMeth.hpp>

/**
 * @brief Apply mathematical functions to HDF5 datasets
 *
 * @details This function provides a unified interface for applying various mathematical
 * operations to HDF5 datasets. It supports both single-dataset operations and
 * operations between multiple datasets.
 *
 * The function supports the following operations:
 * - QR decomposition via bdQR()
 * - Cross product via bdCrossprod()
 * - Transposed cross product via bdtCrossprod()
 * - Inverse via Cholesky decomposition
 * - Matrix multiplication
 * - Cross product with two matrices
 * - Transposed cross product with two matrices
 * - Matrix equation solving
 * - Standard deviation and mean computation
 *
 * @param filename [in] Name of the HDF5 file
 * @param group [in] Input group containing the datasets
 * @param datasets [in] Input datasets to process
 * @param outgroup [in] Output group for results storage
 * @param func [in] Function to apply (QR, CrossProd, etc.)
 * @param b_group [in] Optional group for secondary datasets
 * @param b_datasets [in] Optional secondary datasets
 * @param overwrite [in] Whether to overwrite existing results
 * @param transp_dataset [in] Whether to transpose first dataset
 * @param transp_bdataset [in] Whether to transpose second dataset
 * @param fullMatrix [in] Whether to store complete matrix in Cholesky operations
 * @param byrows [in] Whether to compute by rows in statistical operations
 * @param threads [in] Number of threads for parallel processing
 *
 * @return void
 *
 * @throws H5::FileIException if file operations fail
 * @throws H5::DataSetIException if dataset operations fail
 * @throws std::exception for other errors
 *
 * @note Performance is optimized through block-wise processing and parallel computation
 * @see bdQR(), bdCrossprod(), bdtCrossprod()
 */

//' Apply function to different datasets inside a group
//'
//' This function provides a unified interface for applying various mathematical
//' operations to HDF5 datasets. It supports both single-dataset operations and
//' operations between multiple datasets.
//' 
//' @param filename Character array, indicating the name of the file to create
//' @param group Character array, indicating the input group where the data set
//'        to be imputed is
//' @param datasets Character array, indicating the input datasets to be used
//' @param outgroup Character array, indicating group where the data set will 
//'        be saved after imputation. If NULL, output dataset is stored 
//'        in the same input group
//' @param func Character array, function to be applied:
//'        - "QR": QR decomposition via bdQR()
//'        - "CrossProd": Cross product via bdCrossprod()
//'        - "tCrossProd": Transposed cross product via bdtCrossprod()
//'        - "invChol": Inverse via Cholesky decomposition
//'        - "blockmult": Matrix multiplication 
//'        - "CrossProd_double": Cross product with two matrices
//'        - "tCrossProd_double": Transposed cross product with two matrices
//'        - "solve": Matrix equation solving
//'        - "sdmean": Standard deviation and mean computation
//' @param b_group Optional character array indicating the input group for
//'        secondary datasets (used in two-matrix operations)
//' @param b_datasets Optional character array indicating the secondary datasets
//'        for two-matrix operations
//' @param overwrite Optional boolean. If true, overwrites existing results
//' @param transp_dataset Optional boolean. If true, transposes first dataset
//' @param transp_bdataset Optional boolean. If true, transposes second dataset
//' @param fullMatrix Optional boolean for Cholesky operations. If true, stores
//'        complete matrix; if false, stores only lower triangular
//' @param byrows Optional boolean for statistical operations. If true, computes
//'        by rows; if false, by columns
//' @param threads Optional integer specifying number of threads for parallel processing
//' 
//' @return Modifies the HDF5 file in place, adding computed results
//' 
//' @details
//' //' For matrix multiplication operations (`blockmult`, `CrossProd_double`, `tCrossProd_double`),
//' the `datasets` and `b_datasets` vectors must have the same length. Each operation is performed
//' element-wise between the corresponding pairs of datasets. Specifically, the `b_datasets` vector
//' defines the second operand for each matrix multiplication. For example, if
//' `datasets = {"A1", "A2", "A3"}` and `b_datasets = {"B1", "B2", "B3"}`, the operations
//' executed are: `A1 %*% B1`, `A2 %*% B2`, and `A3 %*% B3`.
//' 
//' Example: If `datasets = {"A1", "A2", "A3"}` and `b_datasets = {"B1", "B2", "B3"}`,
//' the function computes: `A1 %*% B1`, `A2 %*% B2`, and `A3 %*% B3`
//' 
//' @examples
//' \dontrun{
//' # Create a sample large matrix in HDF5
//' # Create hdf5 datasets
//' bdCreate_hdf5_matrix(filename = "test_temp.hdf5", 
//'                     object = Y, group = "data", dataset = "Y",
//'                     transp = FALSE,
//'                     overwriteFile = TRUE, overwriteDataset = TRUE, 
//'                     unlimited = FALSE)
//' 
//' bdCreate_hdf5_matrix(filename = "test_temp.hdf5", 
//'                     object = X,  group = "data",  dataset = "X",
//'                     transp = FALSE,
//'                     overwriteFile = FALSE, overwriteDataset = TRUE, 
//'                     unlimited = FALSE)
//' 
//' bdCreate_hdf5_matrix(filename = "test_temp.hdf5",
//'                     object = Z,  group = "data",  dataset = "Z",
//'                     transp = FALSE,
//'                     overwriteFile = FALSE, overwriteDataset = TRUE,
//'                     unlimited = FALSE)
//' 
//' dsets <- bdgetDatasetsList_hdf5("test_temp.hdf5", group = "data")
//' dsets
//' 
//' # Apply function :  QR Decomposition
//' bdapply_Function_hdf5(filename = "test_temp.hdf5",
//'                      group = "data",datasets = dsets,
//'                      outgroup = "QR",func = "QR",
//'                      overwrite = TRUE)
//' }
//' 
//' @note Performance is optimized through:
//'       - Block-wise processing for large datasets
//'       - Parallel computation where applicable
//'       - Memory-efficient matrix operations
//' 
//' @export
// [[Rcpp::export]]
void bdapply_Function_hdf5( std::string filename, 
                            std::string group, 
                            Rcpp::StringVector datasets, 
                            std::string outgroup, 
                            std::string func, 
                            Rcpp::Nullable<std::string> b_group = R_NilValue, 
                            Rcpp::Nullable<Rcpp::StringVector> b_datasets = R_NilValue,
                            Rcpp::Nullable<bool> overwrite = false,
                            Rcpp::Nullable<bool> transp_dataset = false,
                            Rcpp::Nullable<bool> transp_bdataset = false,
                            Rcpp::Nullable<bool> fullMatrix = false,
                            Rcpp::Nullable<bool> byrows = false,
                            Rcpp::Nullable<int> threads = 2 )
{
    
    Rcpp::StringVector str_bdatasets;
    std::string str_bgroup, outputdataset;
    bool btransdataA, btransdataB, bfullMatrix, bbyrows;
    long dElementsBlock = MAXELEMSINBLOCK; 
    Rcpp::NumericVector oper = {0, 1, 2, 3, 4, 11, 22, 5, 6, 7, 8};
    oper.names() = Rcpp::CharacterVector({"QR", "CrossProd", "tCrossProd",
               "invChol", "blockmult", "CrossProd_double", "tCrossProd_double",
               "solve", "normalize", "sdmean", "descChol"});
    
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsQ = nullptr;
    BigDataStatMeth::hdf5Dataset* dsR = nullptr;
    BigDataStatMeth::hdf5Dataset* dsOut = nullptr;
    BigDataStatMeth::hdf5Dataset* dsmean = nullptr;
    BigDataStatMeth::hdf5Dataset* dssd = nullptr;
    
    try
    {
        H5::Exception::dontPrint();
        
        bool bforce;
        
        std::vector<hsize_t> stride = {1, 1},
                             block = {1, 1};
        
        if(overwrite.isNull()) { bforce = false; } 
        else {   bforce = Rcpp::as<bool>(overwrite); }
        
        if(transp_dataset.isNull()) { btransdataA = false; } 
        else {   btransdataA = Rcpp::as<bool>(transp_dataset); }
        
        if(transp_bdataset.isNull()) { btransdataB = false; } 
        else {   btransdataB = Rcpp::as<bool>(transp_bdataset); }
        
        if(fullMatrix.isNull()) { bfullMatrix = false; } 
        else {   bfullMatrix = Rcpp::as<bool>(fullMatrix); }
        
        if(byrows.isNull()) { bbyrows = false; } 
        else {   bbyrows = Rcpp::as<bool>(byrows); }
        
        if(b_group.isNull()) { str_bgroup = group; } 
        else {   str_bgroup = Rcpp::as<std::string>(b_group); }
        
        if( b_datasets.isNotNull() &&  ( oper(oper.findName(func)) == 1 ||  
            oper(oper.findName(func)) == 2 || oper(oper.findName(func)) == 4 || 
            oper(oper.findName(func)) == 5 || oper(oper.findName(func)) == 6) ) {
            
            str_bdatasets = Rcpp::as<Rcpp::StringVector>(b_datasets);
            
            if( oper.findName( func ) == 1) {
                func = "CrossProd_double";
            } else if( oper.findName( func ) == 2) {
                func = "tCrossProd_double";
            }
        }
        
        if( b_group.isNull()) { 
            str_bgroup = group; 
        } else {   
            str_bgroup = Rcpp::as<std::string>(b_group); 
        }
        
        // Seek all datasets to perform calculus
        for( int i=0; i < datasets.size(); i++ ) 
        {
            
            dsA = new BigDataStatMeth::hdf5Dataset(filename, group, Rcpp::as<std::string>(datasets(i)), false);
            dsA->openDataset();

            if( dsA->getDatasetptr() != nullptr ) { 
                
                if( oper(oper.findName( func )) == 0) {
                    // ==> QR Decomposition 
                    
                    BigDataStatMeth::hdf5Dataset* dsQ = new BigDataStatMeth::hdf5Dataset(filename, outgroup, Rcpp::as<std::string>(datasets(i)) + ".Q", bforce);
                    BigDataStatMeth::hdf5Dataset* dsR = new BigDataStatMeth::hdf5Dataset(filename, outgroup, Rcpp::as<std::string>(datasets(i)) + ".R", bforce);
                    
                    RcppQRHdf5(dsA, dsQ, dsR, true, R_NilValue, threads);
                    
                    delete dsA; dsA = nullptr;
                    delete dsQ; dsQ = nullptr;
                    delete dsR; dsR = nullptr;
                    
                } else if( oper(oper.findName( func )) == 1 || oper(oper.findName( func )) == 2) {
                    // ==> CrossProd and transposed CrossProd
                    
                    hsize_t* dims_out = dsA->dim();
                    
                    
                    if((dims_out[0] * dims_out[1]) > (MAXELEMSINBLOCK / 1024)) {
                        dsOut = new BigDataStatMeth::hdf5Dataset(filename, outgroup, Rcpp::as<std::string>(datasets(i)) , bforce);
                        
                        // int iblock_size = BigDataStatMeth::getMaxBlockSize( dsA->nrows(), dsA->ncols(), dsA->nrows(), dsA->ncols(), 4, R_NilValue);
                        // int memory_block = iblock_size/2;
                        int iblock_size = 0,
                            memory_block = 0;
                        
                        bool bparal = true,
                             isSymetric = true;
                        
                        if(  oper(oper.findName( func )) == 1 ) {
                            // results = BigDataStatMeth::bdcrossproduct(original);
                            // dsOut = BigDataStatMeth::crossprod(dsA, dsA, dsOut, iblock_size, memory_block, bparal, true, isSymetric, threads);
                            dsOut = BigDataStatMeth::crossprod(dsA, dsA, dsOut, isSymetric, iblock_size, memory_block, bparal, true, threads);
                        } else {
                            // results = BigDataStatMeth::bdtcrossproduct(original);
                            dsOut = BigDataStatMeth::tcrossprod(dsA, dsA, dsOut, isSymetric, iblock_size, memory_block, bparal, true, threads);
                        }
                        
                        
                        // delete dsOut; dsOut = nullptr;
                        
                    } else {
                        
                        std::vector<double> vdA( dims_out[0] * dims_out[1] ); 
                        dsA->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vdA.data() );
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> original (vdA.data(), dims_out[1], dims_out[0] );
                        
                        // delete dsA; dsA = nullptr;
                        
                        Eigen::MatrixXd results;
                        dsOut = new BigDataStatMeth::hdf5Dataset(filename, outgroup, Rcpp::as<std::string>(datasets(i)) , bforce);
                        
                        if(  oper(oper.findName( func )) == 1 ) {
                            results = BigDataStatMeth::bdcrossproduct(original);
                        } else {
                            results = BigDataStatMeth::bdtcrossproduct(original);
                        }
                        
                        dsOut->createDataset(results.rows(), results.cols(), "numeric"); 
                        dsOut->openDataset(); 
                        if( dsOut->getDatasetptr() != nullptr )  {
                            dsOut->writeDataset(Rcpp::wrap(results));    
                        }
                        
                    }
                    
                    delete dsA; dsA = nullptr;
                    delete dsOut; dsOut = nullptr;
                    
                    
                    
                } else if( oper(oper.findName( func )) == 3 || oper(oper.findName( func )) == 8) {
                    // ==> Inverse Cholesky and Cholesky decomposition
                    
                    int nrows = dsA->nrows();
                    int ncols = dsA->ncols();
                    
                    Rcpp::Rcout<<"\n Estem processant el dataset: "<<datasets[i];
                    
                    if(nrows == ncols) {
                        
                        std::string strOutdataset = outgroup + "/" +  Rcpp::as<std::string>(datasets(i));
                        //..// BigDataStatMeth::hdf5DatasetInternal* dsOut = new BigDataStatMeth::hdf5DatasetInternal(filename, outgroup, Rcpp::as<std::string>(datasets(i)) , bforce);
                        BigDataStatMeth::hdf5DatasetInternal* dsOut = new BigDataStatMeth::hdf5DatasetInternal(filename, strOutdataset, bforce);
                        dsOut->createDataset(nrows, ncols, "real");
                        
                        if(oper(oper.findName( func )) == 3 ) {
                            if( dsOut->getDatasetptr() != nullptr ) {
                                BigDataStatMeth::Rcpp_InvCholesky_hdf5( dsA, dsOut, bfullMatrix, dElementsBlock, threads);    
                            }
                            
                        } else {
                            [[maybe_unused]] int res = BigDataStatMeth::Cholesky_decomposition_hdf5( dsA, dsOut,  nrows, ncols, dElementsBlock, threads);
                            //..// int res [[maybe_unused]] = BigDataStatMeth::Cholesky_decomposition_hdf5( dsA, dsOut,  nrows, ncols, dElementsBlock, threads);
                        }
                        
                        delete dsA; dsA = nullptr;
                        delete dsOut; dsOut = nullptr;
                        
                    } else {
                        delete dsA; dsA = nullptr;
                        Rcpp::Rcout<<"\n Can't get inverse matrix for "<<Rcpp::as<std::string>(datasets(i))<<" dataset using Cholesky decomposition - not an square matrix\n";
                        return void();
                    }
                    
                    
                } else if( oper(oper.findName( func )) == 4 ||  oper(oper.findName( func )) == 11 ||  oper(oper.findName( func )) == 22) {
                    // ==> blockmult, CrossProd Double, tCrossProd Double"
                    
                    dsB = new BigDataStatMeth::hdf5Dataset(filename, str_bgroup, Rcpp::as<std::string>(str_bdatasets(i)), false);
                    dsB->openDataset();
                    
                    if( dsB->getDatasetptr() != nullptr )  {
                        
                        // Real data set dimension                        
                        hsize_t* dims_out = dsA->dim();
                        hsize_t* dims_outB = dsB->dim();
                        
                        if( oper(oper.findName( func )) == 4 ) {
                            outputdataset = Rcpp::as<std::string>(datasets(i)) + "_" + str_bdatasets(i);
                        } else if  (oper(oper.findName( func )) == 11) {
                            outputdataset = "Cross_" + datasets(i) + "_" + str_bdatasets(i);
                        } else if ( oper(oper.findName( func )) == 22) {
                            outputdataset =  "tCross_" + datasets(i) + "_" + str_bdatasets(i);
                        }

                                                
                        if( ((dims_out[0] * dims_out[1]) > (MAXELEMSINBLOCK / 1024) || (dims_outB[0] * dims_outB[1]) > (MAXELEMSINBLOCK / 1024)) && oper(oper.findName( func )) == 4 ) {
                            
                            dsOut = new BigDataStatMeth::hdf5Dataset(filename, outgroup, outputdataset , bforce);
                            BigDataStatMeth::multiplication(dsA, dsB, dsOut, btransdataA, btransdataB, R_NilValue, R_NilValue, threads); 
                            
                            delete dsA; dsA = nullptr;
                            delete dsB; dsB = nullptr;
                            delete dsOut; dsOut = nullptr;
                            
                        } else {
                            
                            // hsize_t* dims_outB = dsB->dim();
                            // hsize_t* dims_out = dsA->dim();
                            
                            std::vector<double> vdA( dims_out[0] * dims_out[1] ); 
                            dsA->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vdA.data() );
                            
                            std::vector<double> vdB( dims_outB[0] * dims_outB[1] ); 
                            dsB->readDatasetBlock( {0, 0}, {dims_outB[0], dims_outB[1]}, stride, block, vdB.data() );
                            
                            Eigen::MatrixXd original;
                            Eigen::MatrixXd originalB;
                            
                            if( oper(oper.findName( func )) == 4 ) {
                                original = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdA.data(), dims_out[1], dims_out[0] );
                                originalB = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdB.data(), dims_outB[1], dims_outB[0] );
                                // outputdataset = Rcpp::as<std::string>(datasets(i)) + "_" + str_bdatasets(i);
                            } else if  (oper(oper.findName( func )) == 11) {
                                original = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdA.data(), dims_out[0], dims_out[1] );
                                originalB = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdB.data(), dims_outB[1], dims_outB[0] );
                                // outputdataset = "Cross_" + datasets(i) + "_" + str_bdatasets(i);
                            } else if ( oper(oper.findName( func )) == 22) {
                                original = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdA.data(), dims_out[1], dims_out[0] );
                                originalB = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdB.data(), dims_outB[0], dims_outB[1] );
                                // outputdataset =  "tCross_" + datasets(i) + "_" + str_bdatasets(i);
                            }
                            
                            Eigen::MatrixXd  results = BigDataStatMeth::Rcpp_block_matrix_mul_parallel(original, originalB, btransdataA, btransdataB, R_NilValue, R_NilValue);
                            
                            if( results != Eigen::MatrixXd::Zero(original.rows(),originalB.cols()) ) {
                                
                                dsOut = new BigDataStatMeth::hdf5Dataset(filename, outgroup, outputdataset , bforce);
                                dsOut->createDataset(results.rows(), results.cols(), "real");
                                dsOut->writeDataset(Rcpp::wrap(results));
                                
                                delete dsA; dsA = nullptr;
                                delete dsB; dsB = nullptr;
                                delete dsOut; dsOut = nullptr;
                                
                            } else {
                                Rcpp::Rcout<<"Multiplication: "<< group<<"/"<< Rcpp::as<std::string>(datasets(i))<< " x "<< str_bgroup<<"/"<< Rcpp::as<std::string>(str_bdatasets(i)) <<" can not be computed \n";
                            }
                        }
                    }
                    
                } else if( oper(oper.findName( func )) == 5) {
                    // ==> Solve matrix equation Ax = B
                    
                    dsB = new BigDataStatMeth::hdf5Dataset(filename, str_bgroup, Rcpp::as<std::string>(str_bdatasets(i)), false);
                    dsB->openDataset();
                    
                    dsOut = new BigDataStatMeth::hdf5DatasetInternal(filename, outgroup, Rcpp::as<std::string>(datasets(i)) + "_eq_" + Rcpp::as<std::string>(str_bdatasets(i)) , bforce);
                    dsOut->createDataset( dsB->nrows(), dsB->ncols(), "real" );
                    
                    if( dsB->getDatasetptr() != nullptr )  {
                        RcppSolveHdf5(dsA, dsB, dsOut );
                    }
                    
                    delete dsOut; dsOut = nullptr;
                    delete dsB; dsB = nullptr;
                    delete dsA; dsA = nullptr;
                    
                } else if( oper(oper.findName( func )) == 7) {
                    // ==> Compute sd and mean by rows or columns
                    
                    Eigen::MatrixXd datanormal;
                    hsize_t* dims_out = dsA->dim();
                    
                    if( bbyrows == false) {
                        datanormal = Eigen::MatrixXd::Zero( 2, (int)dims_out[0]);
                        get_HDF5_mean_sd_by_column( dsA, datanormal, true, true, R_NilValue );
                        
                    } else {
                        datanormal = Eigen::MatrixXd::Zero( 2, (int)dims_out[1]);
                        get_HDF5_mean_sd_by_row( dsA, datanormal, true, true, R_NilValue );
                        
                    }
                    
                    BigDataStatMeth::hdf5Dataset* dsmean = new BigDataStatMeth::hdf5Dataset(filename, outgroup, "mean." + datasets(i) , bforce);
                    dsmean->createDataset(1, datanormal.cols(), "real");
                    dsmean->writeDataset(Rcpp::wrap(datanormal.row(0)));
                    
                    BigDataStatMeth::hdf5Dataset* dssd = new BigDataStatMeth::hdf5Dataset(filename, outgroup, "sd." + datasets(i) , bforce);
                    dssd->createDataset(1, datanormal.cols(), "real");
                    dssd->writeDataset(Rcpp::wrap(datanormal.row(1)));
                    
                    delete dssd; dssd = nullptr;
                    delete dsmean; dsmean = nullptr;
                    delete dsA; dsA = nullptr;
                    
                } else {
                    delete dsA; dsA = nullptr;
                    Rcpp::Rcout<< "Function does not exists, please use one of the following : \"QR\", \"CrossProd\","<<
                        " \"tCrossProd\", \"invChol\", \"blockmult\", \"CrossProd_double\", \"tCrossProd_double\","<<
                            " \"solve\", \"normalize\", \"sdmean\", \"descChol\" ";
                    return void();
                }    
                
            } else {
                checkClose_file(dsA);        
                Rcpp::Rcerr<<"\nc++ exception bdapply_Function_hdf5 error with "<<Rcpp::as<std::string>(datasets(i))<<" dataset\n";
                return void();
            }
            
        }
        
    }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
        Rcpp::Rcerr<<"c++ exception bdapply_Function_hdf5 (File IException)";
        return void();
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
        Rcpp::Rcerr<<"c++ exception bdapply_Function_hdf5 (DataSet IException)";
        return void();
    } catch(std::exception &ex) {
        checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
        Rcpp::Rcerr<<"c++ exception bdapply_Function_hdf5";
        return void();
    } catch (...) {
        checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
        Rcpp::Rcerr<<"C++ exception bdapply_Function_hdf5 (unknown reason)";
        return void();
    }
    
    // Rcpp::Rcout<< func <<" function has been computed in all blocks\n";  
    return void();
}

