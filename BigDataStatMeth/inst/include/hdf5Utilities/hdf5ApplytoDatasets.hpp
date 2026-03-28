/**
 * @file hdf5ApplytoDatasets.hpp
 * @brief Advanced mathematical operations on HDF5 datasets
 * 
 * This file provides a comprehensive set of mathematical and statistical operations
 * that can be applied to HDF5 datasets. It implements various matrix operations,
 * decompositions, and statistical computations with support for parallel processing
 * and memory-efficient block operations.
 * 
 * Key features:
 * - Matrix operations (multiplication, cross products)
 * - Matrix decompositions (QR, Cholesky)
 * - Statistical computations (mean, standard deviation)
 * - Support for parallel processing
 * - Memory-efficient block operations
 * - Flexible dataset transformations
 * 
 * Supported operations:
 * - QR Decomposition
 * - Cross Product and Transposed Cross Product
 * - Inverse Cholesky
 * - Block Matrix Multiplication
 * - Matrix Equation Solving
 * - Normalization
 * - Statistical Measures
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_HDF5_APPLY_HPP
#define BIGDATASTATMETH_HDF5_APPLY_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Applies mathematical functions to HDF5 datasets
     * 
     * This function serves as a high-level interface for applying various mathematical
     * operations to HDF5 datasets. It supports both single-dataset operations and
     * operations between multiple datasets.
     * 
     * @param filename Path to the HDF5 file
     * @param group Group containing input datasets
     * @param datasets Vector of input dataset names
     * @param outgroup Output group path
     * @param func Operation to apply:
     *        - "QR": QR decomposition
     *        - "CrossProd": Cross product
     *        - "tCrossProd": Transposed cross product
     *        - "invChol": Inverse using Cholesky
     *        - "blockmult": Block matrix multiplication
     *        - "solve": Solve matrix equation
     *        - "normalize": Normalize data
     *        - "sdmean": Compute SD and mean
     *        - "descChol": Cholesky decomposition
     * @param b_group Optional group for second dataset in two-dataset operations
     * @param b_datasets Optional second dataset names for two-dataset operations
     * @param overwrite Optional flag to overwrite existing datasets
     * @param transp_dataset Optional flag to transpose first dataset
     * @param transp_bdataset Optional flag to transpose second dataset
     * @param fullMatrix Optional flag for full matrix output
     * @param byrows Optional flag for row-wise operations
     * @param threads Optional number of threads for parallel processing
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * 
     * Performance considerations:
     * - Uses block-wise processing for large datasets
     * - Implements parallel processing where applicable
     * - Optimizes memory usage through Eigen
     * - Handles large matrices efficiently
     * 
     * Implementation details:
     * 1. Validates input parameters and operation type
     * 2. Sets up appropriate dataset handlers
     * 3. Performs requested operation with error handling
     * 4. Manages memory cleanup
     * 
     * Example:
     * @code
     * // Perform QR decomposition
     * RcppApplyFunctionHdf5("data.h5", "/input", {"matrix1"}, "/output", "QR");
     * 
     * // Perform matrix multiplication
     * RcppApplyFunctionHdf5("data.h5", "/input", {"A"}, "/output", "blockmult",
     *                       "/input", {"B"}, false, false, false);
     * @endcode
     * 
     * @note Maximum block size is controlled by MAXELEMSINBLOCK
     * @warning Some operations require specific matrix dimensions or properties
     */
    inline void RcppApplyFunctionHdf5( std::string filename, 
                                std::string group, 
                                Rcpp::StringVector datasets, 
                                std::string outgroup, 
                                std::string func, 
                                Rcpp::Nullable<Rcpp::CharacterVector> b_group = R_NilValue, 
                                Rcpp::Nullable<Rcpp::StringVector> b_datasets = R_NilValue,
                                Rcpp::Nullable<bool> overwrite = false,
                                Rcpp::Nullable<bool> transp_dataset = false,
                                Rcpp::Nullable<bool> transp_bdataset = false,
                                Rcpp::Nullable<bool> fullMatrix = false,
                                Rcpp::Nullable<bool> byrows = false,
                                Rcpp::Nullable<int> threads = R_NilValue)
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
            
            // if( b_group == "") { str_bgroup = group; } 
            // else {   str_bgroup = b_group; }
            
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
                        
                        std::vector<double> vdA( dims_out[0] * dims_out[1] ); 
                        dsA->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vdA.data() );
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> original (vdA.data(), dims_out[1], dims_out[0] );
                        
                        delete dsA; dsA = nullptr;
                        
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
                        delete dsOut; dsOut = nullptr;
                        
                    } else if( oper(oper.findName( func )) == 3 || oper(oper.findName( func )) == 8) {
                        // ==> Inverse Cholesky and Cholesky decomposition
                        
                        int nrows = dsA->nrows();
                        int ncols = dsA->ncols();
                        
                        if(nrows == ncols) {
                            
                            BigDataStatMeth::hdf5DatasetInternal* dsOut = new BigDataStatMeth::hdf5DatasetInternal(filename, outgroup, Rcpp::as<std::string>(datasets(i)) , bforce);
                            dsOut->createDataset(nrows, ncols, "real");
                            
                            if(oper(oper.findName( func )) == 3 ) {
                                BigDataStatMeth::Rcpp_InvCholesky_hdf5( dsA, dsOut, bfullMatrix, dElementsBlock, threads);    
                            } else {
                                int res [[maybe_unused]] = BigDataStatMeth::Cholesky_decomposition_hdf5( dsA, dsOut,  nrows, ncols, dElementsBlock, threads);
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
                            hsize_t* dims_outB = dsB->dim();
                            hsize_t* dims_out = dsA->dim();
                            
                            std::vector<double> vdA( dims_out[0] * dims_out[1] ); 
                            dsA->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vdA.data() );
                            
                            std::vector<double> vdB( dims_outB[0] * dims_outB[1] ); 
                            dsB->readDatasetBlock( {0, 0}, {dims_outB[0], dims_outB[1]}, stride, block, vdB.data() );
                            
                            Eigen::MatrixXd original;
                            Eigen::MatrixXd originalB;
                            
                            if( oper(oper.findName( func )) == 4 ) {
                                original = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdA.data(), dims_out[1], dims_out[0] );
                                originalB = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdB.data(), dims_outB[1], dims_outB[0] );
                                outputdataset = Rcpp::as<std::string>(datasets(i)) + "_" + str_bdatasets(i);
                            } else if  (oper(oper.findName( func )) == 11) {
                                original = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdA.data(), dims_out[0], dims_out[1] );
                                originalB = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdB.data(), dims_outB[1], dims_outB[0] );
                                outputdataset = "Cross_" + datasets(i) + "_" + str_bdatasets(i);
                            } else if ( oper(oper.findName( func )) == 22) {
                                original = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdA.data(), dims_out[1], dims_out[0] );
                                originalB = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdB.data(), dims_outB[0], dims_outB[1] );
                                outputdataset =  "tCross_" + datasets(i) + "_" + str_bdatasets(i);
                            }
                            
                            Eigen::MatrixXd  results = BigDataStatMeth::Rcpp_block_matrix_mul_parallel(original, originalB, btransdataA, btransdataB, R_NilValue, R_NilValue);
                            
                            if( results != Eigen::MatrixXd::Zero(original.rows(),originalB.cols()) ) {
                                
                                dsOut = new BigDataStatMeth::hdf5Dataset(filename, outgroup, outputdataset , bforce);
                                dsOut->createDataset(results.rows(), results.cols(), "real");
                                dsOut->writeDataset(Rcpp::wrap(results));
                                
                                delete dsOut; dsOut = nullptr;
                                
                            } else {
                                Rcpp::Rcout<<"Multiplication: "<< group<<"/"<< Rcpp::as<std::string>(datasets(i))<< " x "<< str_bgroup<<"/"<< Rcpp::as<std::string>(str_bdatasets(i)) <<" can not be computed \n";
                            }
                            
                            delete dsA; dsA = nullptr;
                            delete dsB; dsB = nullptr;
                            
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
                    Rcpp::Rcerr<<"\nc++ exception RcppApplyFunctionHdf5 error with "<<Rcpp::as<std::string>(datasets(i))<<" dataset\n";
                    return void();
                }
                
            }
            
        }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
            Rcpp::Rcerr<<"c++ exception RcppApplyFunctionHdf5 (File IException)\n";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
            Rcpp::Rcerr<<"c++ exception RcppApplyFunctionHdf5 (DataSet IException)\n";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
            Rcpp::Rcerr << "c++ exception blockmult_hdf5: " << ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsA, dsB, dsQ, dsR, dsOut, dsmean, dssd);
            Rcpp::Rcerr<<"C++ exception RcppApplyFunctionHdf5 (unknown reason)";
            return void();
        }
        
        // Rcpp::Rcout<< func <<" function has been computed in all blocks\n";  
        return void();
    }


}

#endif // BIGDATASTATMETH_HDF5APPLY_HPP
