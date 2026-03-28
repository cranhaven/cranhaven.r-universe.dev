/**
 * @file matrixPCA.hpp
 * @brief Principal Component Analysis (PCA) for HDF5 matrices
 * @details This header file provides implementations for performing Principal
 * Component Analysis on large matrices stored in HDF5 format. The implementation
 * includes:
 * 
 * Key features:
 * - Full and truncated PCA computation
 * - Variable contributions analysis
 * - Individual contributions analysis
 * - Memory-efficient algorithms
 * - Parallel processing support
 * 
 * Supported operations:
 * - PCA decomposition
 * - Variable coordinates
 * - Individual coordinates
 * - Variance contributions
 * - Cos² quality metrics
 * - Component analysis
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Block-based computation
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - SVD decomposition
 * - Efficient statistical algorithms
 * - HDF5 chunked storage
 * - Parallel I/O
 * - Vectorized operations
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXPCA_HPP
#define BIGDATASTATMETH_HDF5_MATRIXPCA_HPP


#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {


    // Get's variance and cumulative variance from svd decomposition
    // var.contr, C, var.coord and var.cos^2 and write results to hdf5 file
    
    // void get_HDF5_PCA_variables_ptr(  H5File* file, std::string strdataset)
    /**
     * @brief Calculate PCA variables statistics
     * @details Computes and stores various PCA statistics for variables including:
     * - Eigenvalues (lambda)
     * - Variance contributions
     * - Cumulative variance
     * - Variable coordinates
     * - Cos² quality metrics
     * 
     * @param strPCAgroup HDF5 group name for PCA results
     * @param dsd Singular values dataset
     * @param dsv Right singular vectors dataset
     * @param overwrite Whether to overwrite existing results
     */
    inline void RcppGetPCAVariablesHdf5( std::string strPCAgroup, 
                                  BigDataStatMeth::hdf5Dataset* dsd, 
                                  BigDataStatMeth::hdf5Dataset* dsv, 
                                  bool overwrite )
    {
        
        BigDataStatMeth::hdf5Dataset* dslambda = nullptr;
        BigDataStatMeth::hdf5Dataset* dsvar = nullptr;
        BigDataStatMeth::hdf5Dataset* dscumvar = nullptr;
        BigDataStatMeth::hdf5Dataset* dscoord = nullptr;
        BigDataStatMeth::hdf5Dataset* dscos2 = nullptr;
        
        try
        {
            H5::Exception::dontPrint();
            
            // int ielements = 0;
            std::vector<hsize_t> stride = {1, 1},
                                 block = {1, 1},
                                 offset_d = {0, 0},
                                 count_d = {dsd->nrows(), dsd->ncols()},
                                 offset_v = {0, 0},
                                 count_v = {dsv->nrows(), dsv->ncols()};
            
            std::vector<double> vdd( count_d[0] * count_d[1] );
            dsd->readDatasetBlock( {offset_d[0], offset_d[1]}, {count_d[0], count_d[1]}, stride, block, vdd.data() );
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> d (vdd.data(), count_d[0], count_d[1]);
            
            {    
                // lambda
                Eigen::VectorXd vvar = d.array().pow(2);
                dslambda = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "lambda" ,overwrite );
                dslambda->createDataset( d.rows(), d.cols(), "real");
                dslambda->writeDataset( vvar.data() );
                delete dslambda; dslambda = nullptr;
                
                // Variance
                vvar = (vvar/vvar.array().sum()) * 100;
                dsvar = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "variance" ,overwrite );
                dsvar->createDataset( d.rows(), d.cols(), "real");
                dsvar->writeDataset( vvar.data() );
                delete dsvar; dsvar = nullptr;
                
                // cumulative variance (max 1000 elements (firsts))
                // if(vvar.size()>1000){  ielements = 1000;    }
                // else{ ielements = vvar.size();    }
                
                dscumvar = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "cumvar" ,overwrite );
                dscumvar->createDataset( d.rows(), d.cols(), "real");
                dscumvar->writeDataset( (cumsum(vvar)).data());
                delete dscumvar; dscumvar = nullptr;
            }
            
            {
                Eigen::MatrixXd var_coord;
                {
                    // Coord vars
                    std::vector<double> vdv( count_v[0] * count_v[1] );
                    dsv->readDatasetBlock( {offset_v[0], offset_v[1]}, {count_v[0], count_v[1]}, stride, block, vdv.data() );
                    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> v (vdv.data(), count_v[0], count_v[1]);
                    
                    var_coord = Rcpp_matrixVectorMultiplication_byRow(v, d);
                }
                
                dscoord = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "var.coord" ,overwrite );
                dscoord->createDataset( var_coord.rows(), var_coord.cols(), "real");
                dscoord->writeDataset( var_coord.transpose().data() );
                delete dscoord; dscoord = nullptr;
    
                // Cos2
                Eigen::MatrixXd var_cos2 = var_coord.unaryExpr([](double d) {return std::pow(d, 2);});
                dscos2 = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "var.cos2" ,overwrite );
                dscos2->createDataset( var_cos2.rows(), var_cos2.cols(), "real");
                dscos2->writeDataset( var_cos2.transpose().data() );
                delete dscos2; dscos2 = nullptr;
                
            }
            
        } catch( H5::FileIException& error ) { 
            checkClose_file(dsd, dsv, dslambda, dsvar, dscumvar, dscoord, dscos2);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAVariablesHdf5 (File IException)\n";
            return void();
        } catch( H5::DataSetIException& error ) { 
            checkClose_file(dsd, dsv, dslambda, dsvar, dscumvar, dscoord, dscos2);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAVariablesHdf5 (DataSet IException)\n";
            return void();
        } catch( H5::DataSpaceIException& error ) { 
            checkClose_file(dsd, dsv, dslambda, dsvar, dscumvar, dscoord, dscos2);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAVariablesHdf5 (DataSpace IException)\n";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsd, dsv, dslambda, dsvar, dscumvar, dscoord, dscos2);
            Rcpp::Rcout<<"c++ exception RcppGetPCAVariablesHdf5 \n"<< ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsd, dsv, dslambda, dsvar, dscumvar, dscoord, dscos2);
            Rcpp::Rcout<<"\nC++ exception RcppGetPCAVariablesHdf5 (unknown reason)";
            return void();
        }
        
        
        return void();
    }
    
    
    
    
    /**
     * @brief Calculate PCA individuals statistics
     * @details Computes and stores various PCA statistics for individuals including:
     * - Distances
     * - Component coordinates
     * - Individual coordinates
     * - Cos² quality metrics
     * - Contributions
     * 
     * @param strPCAgroup HDF5 group name for PCA results
     * @param dsX Input data matrix dataset
     * @param dsd Singular values dataset
     * @param dsu Left singular vectors dataset
     * @param overwrite Whether to overwrite existing results
     */
    inline void RcppGetPCAIndividualsHdf5( std::string strPCAgroup, 
                                    BigDataStatMeth::hdf5Dataset* dsX,
                                    BigDataStatMeth::hdf5Dataset* dsd, 
                                    BigDataStatMeth::hdf5Dataset* dsu, 
                                    bool overwrite )
    {
        
        
        BigDataStatMeth::hdf5Dataset* dsdist2 = nullptr;
        BigDataStatMeth::hdf5Dataset* dsComp = nullptr;
        BigDataStatMeth::hdf5Dataset* dscoord = nullptr;
        BigDataStatMeth::hdf5Dataset* dscos2 = nullptr;
        BigDataStatMeth::hdf5Dataset* dscontrib = nullptr;
        
        
        try
        {
            H5::Exception::dontPrint();
            
            // int ielements = 0;
            std::vector<hsize_t> stride = {1, 1},
                block = {1, 1},
                offset = {0, 0},
                count_x = {dsX->nrows(), dsX->ncols()},
                count_d = {dsd->nrows(), dsd->ncols()},
                count_u = {dsu->nrows(), dsu->ncols()};
            
            Eigen::VectorXd dist2;
            Eigen::MatrixXd adjust;
            double weights = 1/(double)dsX->ncols();
            {
                
                std::vector<double> vdX( count_x[0] * count_x[1] );
                dsX->readDatasetBlock( {0,0}, count_x, stride, block, vdX.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdX.data(), count_x[0], count_x[1]);
    
                X = RcppNormalizeRowwise(X, true, false);
                adjust = Eigen::MatrixXd::Constant(1, X.cols(), weights);
                
                Eigen::RowVectorXd ecart =  adjust / adjust.sum() ;
                ecart = ecart * (X.unaryExpr([](double d) {return std::pow(d, 2);})).transpose();
                ecart = ecart.unaryExpr([](double d) {return std::sqrt(d);});
                
                X = X.array().colwise() / ecart.transpose().array();
    
                dist2 =  (X.unaryExpr([](double d) {return std::pow(d, 2);})).colwise().sum();
                Eigen::VectorXd dist = dist2.array().sqrt();
                
                dsdist2 = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "ind.dist" ,overwrite );
                dsdist2->createDataset( dist2.rows(), dist2.cols(), "real");
                dsdist2->writeDataset( dist.data() );
                delete dsdist2; dsdist2 = nullptr;
            }
    
            // Load d
            std::vector<double> vdd( count_d[0] * count_d[1] );
            dsd->readDatasetBlock( offset, count_d, stride, block, vdd.data() );
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> d (vdd.data(), count_d[0], count_d[1]); 
            
            Eigen::MatrixXd var_coord;
            {
                // Load u
                std::vector<double> vdu( count_u[0] * count_u[1] );
                dsu->readDatasetBlock( {0, 0}, count_u, stride, block, vdu.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> u (vdu.data(), count_u[0], count_u[1]);
                
                u = u * sqrt(1/weights); 
            
                // Components
                dsComp = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "components" ,overwrite );
                dsComp->createDataset( u.cols(), u.rows(), "real");
                dsComp->writeDataset( u.data() );
                delete dsComp; dsComp = nullptr;
                
                var_coord = Rcpp_matrixVectorMultiplication_byRow(u, d);
            }
            
            // Coord inds
            dscoord = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "ind.coord" ,overwrite );
            dscoord->createDataset( var_coord.cols(), var_coord.rows(), "real");
            dscoord->writeDataset( Rcpp::wrap(var_coord.transpose()));
            delete dscoord; dscoord = nullptr;
            
            // Cos2 inds
            Eigen::MatrixXd coord2 = var_coord.unaryExpr([](double d) {return std::pow(d, 2);});
            
            {
                Eigen::MatrixXd ind_cos2 = coord2.array().rowwise() / dist2.transpose().array();
                
                dscos2 = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "ind.cos2" ,overwrite );
                dscos2->createDataset( ind_cos2.cols(), ind_cos2.rows(), "real");
                dscos2->writeDataset( Rcpp::wrap(ind_cos2.transpose()) );
                delete dscos2; dscos2 = nullptr;
            }
            
            Eigen::MatrixXd ind_contrib = coord2.array().rowwise() * adjust.row(0).array();
            
            d = d.unaryExpr([](double xd) {return std::pow(xd, 2);});
            
            ind_contrib = ind_contrib.array().colwise() / d.col(0).array();
            ind_contrib = ind_contrib.unaryExpr([](double d) {return d*100;});
            
            dscontrib = new BigDataStatMeth::hdf5Dataset(dsd->getFileName(), strPCAgroup, "ind.contrib" ,overwrite );
            dscontrib->createDataset( ind_contrib.cols(), ind_contrib.rows(), "real");
            dscontrib->writeDataset( Rcpp::wrap(ind_contrib.transpose()) );
            delete dscontrib; dscontrib = nullptr;
                
            // Components
            // createHardLink(dsu->getFileptr(), dsu->getGroupName() + "/" + dsu->getDatasetName(), strPCAgroup + "/components");
            
        } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file( dsX, dsd, dsu, dsdist2, dsComp, dscoord, dscos2, dscontrib);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAIndividualsHdf5 (File IException)\n";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file( dsX, dsd, dsu, dsdist2, dsComp, dscoord, dscos2, dscontrib);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAIndividualsHdf5 (DataSet IException)\n";
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file( dsX, dsd, dsu, dsdist2, dsComp, dscoord, dscos2, dscontrib);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAIndividualsHdf5 (DataSpace IException)\n";
            return void();
        } catch(std::exception &ex) {
            checkClose_file( dsX, dsd, dsu, dsdist2, dsComp, dscoord, dscos2, dscontrib);
            Rcpp::Rcerr<<"\nc++ exception RcppGetPCAIndividualsHdf5 \n"<< ex.what();
            return void();
        } catch (...) {
            checkClose_file( dsX, dsd, dsu, dsdist2, dsComp, dscoord, dscos2, dscontrib);
            Rcpp::Rcerr<<"\nC++ exception RcppGetPCAIndividualsHdf5 (unknown reason)";
            return void();
        }
        
        return void();
    }
    
    
    /**
     * @brief Perform Principal Component Analysis
     * @details Performs PCA on an HDF5 dataset with options for:
     * - Full or truncated analysis
     * - Data preprocessing (centering/scaling)
     * - Method selection
     * - Parallel processing
     * 
     * @param filename HDF5 file name
     * @param strgroup Group name for results
     * @param strdataset Dataset name
     * @param strSVDgroup SVD group name
     * @param k Number of components to compute
     * @param q Block size for processing
     * @param nev Number of eigenvalues
     * @param bcenter Whether to center the data
     * @param bscale Whether to scale the data
     * @param dthreshold Convergence threshold
     * @param bforce Whether to force computation
     * @param asRowMajor Whether data is in row-major order
     * @param method Method selection (optional)
     * @param ithreads Number of threads (optional)
     */
    inline void RcppPCAHdf5( std::string filename, std::string strgroup, std::string strdataset,  
                             std::string strSVDgroup, int k, int q, int nev, 
                             bool bcenter, bool bscale, double dthreshold, 
                             bool bforce, bool asRowMajor, 
                             Rcpp::Nullable<Rcpp::CharacterVector> method = R_NilValue,
                             Rcpp::Nullable<int> ithreads = R_NilValue)
    {
        
        BigDataStatMeth::hdf5Dataset* dsA = nullptr;
        BigDataStatMeth::hdf5Dataset* dsd = nullptr;
        BigDataStatMeth::hdf5Dataset* dsu = nullptr;
        BigDataStatMeth::hdf5Dataset* dsv = nullptr;
        BigDataStatMeth::hdf5Dataset* dsX = nullptr;
        
        try{
            
    
            std::string strPCAgroup = "PCA/" + strdataset;
                        
            // Check for svd decomposition (u, v and d matrices) in hdf5 file or if we 
            // need to compute again the SVD ( foce = true )
            BigDataStatMeth::hdf5File* file = new BigDataStatMeth::hdf5File(filename, false);
            file->openFile("r");
            
            bool bexistsSVD = exists_HDF5_element(file->getFileptr(), strSVDgroup);
            bool bexistsPCA = exists_HDF5_element(file->getFileptr(), strPCAgroup);
            
            delete file; file = nullptr;
            
            if( bexistsSVD == 0 ||  bforce == true ) {
                
                dsA = new BigDataStatMeth::hdf5Dataset(filename, strgroup, strdataset, false);
                dsA->openDataset();
                if( dsA->getDatasetptr() != nullptr ) {
                    RcppTypifyNormalizeHdf5( dsA, bcenter, bscale, false); // Normalize and tipify data ( ((x-mu)/(sd)) * 1/sqrt(n-1) )
                } else {
                    checkClose_file(dsA, dsd, dsu, dsv, dsX);
                    return void();
                }
                
                delete dsA; dsA = nullptr;
                
                BigDataStatMeth::RcppbdSVD_hdf5( filename, "NORMALIZED_T/" + strgroup, strdataset, k, q, nev, false, false, dthreshold, bforce, asRowMajor, method, ithreads );
                
                strSVDgroup = "SVD/" +  strdataset;
                
            }
            
            // Check if PCA decomposition exists
            if( bexistsPCA != 0  && bforce == false) {
                Rcpp::Rcout<<"PCA decomposition exits, please set overwrite = true to overwrite the existing results";
                return void();
            }
            
            // ------------ Variables ----------------
            
            dsd = new BigDataStatMeth::hdf5Dataset(filename, strSVDgroup, "d", false );
            dsd->openDataset();
            
            dsv = new BigDataStatMeth::hdf5Dataset(filename, strSVDgroup, "v", false );
            dsv->openDataset();
            
            if( dsd->getDatasetptr() != nullptr && dsv->getDatasetptr() != nullptr) {
                RcppGetPCAVariablesHdf5( strPCAgroup, dsd, dsv, bforce );
            }
            
            delete dsv; dsv = nullptr;
            // delete dsA;
            
            // ------------ Individuals ----------------
            
            dsX = new BigDataStatMeth::hdf5Dataset(filename, strgroup, strdataset, false);
            dsX->openDataset();
            
            dsu = new BigDataStatMeth::hdf5Dataset(filename, strSVDgroup, "u", false );
            dsu->openDataset();
            
            if( dsX->getDatasetptr() != nullptr && dsd->getDatasetptr() != nullptr && dsu->getDatasetptr() != nullptr) {
                RcppGetPCAIndividualsHdf5( strPCAgroup, dsX, dsd, dsu, bforce );
            }
            
            delete dsd; dsd = nullptr;
            delete dsu; dsu = nullptr;
            delete dsX; dsX = nullptr;
            // delete dsA;
            
        } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsd, dsu, dsv, dsX);
            Rcpp::Rcerr<<"\nc++ exception RcppPCAHdf5 (File IException)\n";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsd, dsu, dsv, dsX);
            Rcpp::Rcerr<<"\nc++ exception RcppPCAHdf5 (DataSet IException)\n";
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsd, dsu, dsv, dsX);
            Rcpp::Rcerr<<"\nc++ exception RcppPCAHdf5 (DataSpace IException)\n";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsd, dsu, dsv, dsX);
            Rcpp::Rcerr<<"\nc++ exception RcppPCAHdf5 \n"<< ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsA, dsd, dsu, dsv, dsX);
            Rcpp::Rcerr<<"\nC++ exception RcppPCAHdf5 (unknown reason)";
            return void();
        }
        
        return void();
        
    }
    



}

#endif // BIGDATASTATMETH_HDF5_MATRIXPCA_HPP
