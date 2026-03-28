/**
 * @file matrixSvd.hpp
 * @brief Singular Value Decomposition (SVD) for HDF5 matrices
 * @details This header file provides implementations for computing the Singular
 * Value Decomposition of large matrices stored in HDF5 format. The implementation
 * includes:
 * 
 * Key features:
 * - Full and truncated SVD computation
 * - Memory-efficient implementation
 * - Block-based processing
 * - Parallel computation support
 * - Error handling and validation
 * 
 * Supported operations:
 * - Full SVD (U, S, V)
 * - Truncated SVD (k components)
 * - Left singular vectors (U)
 * - Right singular vectors (V)
 * - Singular values (S)
 * 
 * Performance features:
 * - Block-based computation
 * - Memory-efficient algorithms
 * - Multi-threaded processing
 * - HDF5 chunked storage
 * - Optimized I/O operations
 * 
 * The implementation uses:
 * - Eigen's SVD solver
 * - Block Krylov methods
 * - Randomized algorithms
 * - HDF5's parallel I/O
 * 
 * @author BigDataStatMeth Development Team
 * @version 1.0
 * @date 2025
 * @note Updated for Spectra 1.0.1 compatibility
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXSVD_HPP
#define BIGDATASTATMETH_HDF5_MATRIXSVD_HPP

#include "Spectra/SymEigsSolver.h"

namespace BigDataStatMeth {
    
    /**
     * @brief Compute SVD decomposition using Spectra eigenvalue solver
     * @details Performs Singular Value Decomposition of a matrix using the 
     * Spectra library for eigenvalue computation. This function computes SVD
     * by solving the eigenvalue problems X^T*X and X*X^T.
     * 
     * @param X Input matrix for SVD computation
     * @param k Number of singular values/vectors to compute (0 = auto-select)
     * @param ncv Number of Arnoldi vectors to use (0 = auto-select)
     * @param bcenter Whether to center the data before computation
     * @param bscale Whether to scale the data before computation
     * 
     * @return svdeig Structure containing U, S, V matrices and computation status
     * 
     * @note Updated for Spectra 1.0.1 API compatibility
     * @warning Matrix dimensions should be reasonable for memory usage
     * 
     * @see BigDataStatMeth::svdeig
     * @see Spectra::SymEigsSolver
     */
    inline svdeig RcppbdSVD( Eigen::MatrixXd& X, int k, int ncv, bool bcenter, bool bscale )
    {
        
        svdeig retsvd;
        Eigen::MatrixXd nX;
        int nconv [[maybe_unused]];
        
        if( k==0 )    k = (std::min(X.rows(), X.cols()))-1;
        else if (k > (std::min(X.rows(), X.cols()))-1 ) k = (std::min(X.rows(), X.cols()))-1;
        
        if(ncv == 0)  ncv = k + 1 ;
        if(ncv<k) ncv = k + 1;
        
        {
            Eigen::MatrixXd Xtcp;
            if(bcenter ==true || bscale == true)  {
                nX = RcppNormalize_Data(X, bcenter, bscale, false);
                Xtcp =  bdtcrossproduct(nX);
            } else {
                Xtcp =  bdtcrossproduct(X);
            }
            
            Spectra::DenseSymMatProd<double> op(Xtcp);
            // Updated for Spectra 1.0.1: removed template parameters, pass object by reference
            Spectra::SymEigsSolver<Spectra::DenseSymMatProd<double>> eigs(op, k, ncv);
            
            // Initialize and compute
            eigs.init();
            // Updated for Spectra 1.0.1: SortRule as runtime parameter
            nconv = eigs.compute(Spectra::SortRule::LargestAlge);
            
            // Updated for Spectra 1.0.1: enum class for status check
            if(eigs.info() == Spectra::CompInfo::Successful) {
                retsvd.d = eigs.eigenvalues().cwiseSqrt();
                retsvd.u = eigs.eigenvectors();
                retsvd.bokuv = true;
            } else {
                retsvd.bokuv = false;
            }
        }
        if(retsvd.bokuv == true)
        {
            Eigen::MatrixXd Xcp;
            if(bcenter ==true || bscale==true ) {
                Xcp =  bdcrossproduct(nX);  
            } else {
                Xcp =  bdcrossproduct(X);
            }  
            
            Spectra::DenseSymMatProd<double> opv(Xcp);
            // Updated for Spectra 1.0.1: removed template parameters, pass object by reference
            Spectra::SymEigsSolver<Spectra::DenseSymMatProd<double>> eigsv(opv, k, ncv);
            
            // Initialize and compute
            eigsv.init();
            // Updated for Spectra 1.0.1: SortRule as runtime parameter
            nconv = eigsv.compute(Spectra::SortRule::LargestAlge);
            
            // Retrieve results
            // Updated for Spectra 1.0.1: enum class for status check
            if(eigsv.info() == Spectra::CompInfo::Successful) {
                retsvd.v = eigsv.eigenvectors();
            } else {
                retsvd.bokd = false;
            }
        }
        
        return retsvd;
    }
    
    
    
    /**
     * @brief Block-wise SVD decomposition for large HDF5 matrices
     * @details Performs SVD computation on large matrices using block-wise
     * decomposition strategy. This function handles matrices too large to
     * fit in memory by processing them in blocks and combining results.
     * 
     * @param dsA Input HDF5 dataset containing the matrix
     * @param dsu Output HDF5 dataset for left singular vectors (U)
     * @param dsv Output HDF5 dataset for right singular vectors (V)
     * @param dsd Output HDF5 dataset for singular values (S)
     * @param k Number of local SVDs to concatenate at each level
     * @param q Number of decomposition levels
     * @param nev Number of eigenvalues per block
     * @param bcenter Whether to center the data
     * @param bscale Whether to scale the data
     * @param irows Number of rows in input matrix
     * @param icols Number of columns in input matrix
     * @param dthreshold Threshold for numerical computations
     * @param threads Number of parallel threads (optional)
     * 
     * @throws H5::FileIException for HDF5 file operation errors
     * @throws H5::DataSetIException for HDF5 dataset operation errors
     * @throws std::exception for computation errors
     * 
     * @note This function implements a hierarchical SVD algorithm for large matrices
     * @warning Requires sufficient disk space for temporary computations
     * 
     * @see BigDataStatMeth::hdf5Dataset
     * @see First_level_SvdBlock_decomposition_hdf5
     * @see Next_level_SvdBlock_decomposition_hdf5
     */
    inline void RcppbdSVD_hdf5_Block( BigDataStatMeth::hdf5Dataset* dsA, 
                                      BigDataStatMeth::hdf5Dataset* dsu, BigDataStatMeth::hdf5Dataset* dsv, 
                                      BigDataStatMeth::hdf5Dataset* dsd, int k, int q, int nev, bool bcenter, bool bscale, 
                                      int irows, int icols, double dthreshold, Rcpp::Nullable<int> threads = R_NilValue )
    {
        
        BigDataStatMeth::hdf5Dataset* dsnormalizedData = nullptr;
        BigDataStatMeth::hdf5Dataset* dsJoined = nullptr;
        BigDataStatMeth::hdf5DatasetInternal* dsnormalizedData_i = nullptr;
        
        try{
            
            std::vector<hsize_t> stride = {1, 1},
                block = {1, 1};
            
            svdeig retsvd;
            // Eigen::MatrixXd nX;
            Eigen::MatrixXd matlast;
            Eigen::MatrixXd v;    
            bool transp = false;
            std::string strGroupName  = "tmpgroup",
                strPrefix; // Outpu group name for temporal data
            
            Rcpp::CharacterVector strvmatnames = {"A","B","C","D","E","F","G","H","I","J","K",
                                                  "L","M","N","O","P","Q","R","S","T","U","V",
                                                  "W","X","Y","Z"};
            strPrefix = strvmatnames[q-1];
            
            if(irows >= icols) {
                transp = true;
            }
            
            First_level_SvdBlock_decomposition_hdf5( dsA, strGroupName, k, q, nev, bcenter, bscale, dthreshold, threads);
            
            for(int j = 1; j < q; j++) { // For each decomposition level :
                Next_level_SvdBlock_decomposition_hdf5( dsA, strGroupName, k, j, dthreshold, threads);
            }
            
            // Get dataset names
            Rcpp::StringVector joindata =  dsA->getDatasetNames(strGroupName, strPrefix, "");
            
            // 1.- Join matrix and remove parts from file
            std::string strnewdataset = std::string((joindata[0])).substr(0,1);
            
            dsJoined = new hdf5DatasetInternal(dsA->getFullPath(), strGroupName, strnewdataset, true);
            
            join_datasets( dsJoined, strGroupName, joindata, false, true );
            
            // 2.- Get SVD from Blocks full mattrix
            hsize_t* dims_out = dsJoined->dim();
            
            std::vector<double> vdreaded( dims_out[0] * dims_out[1] );
            dsJoined->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, {1, 1}, {1, 1}, vdreaded.data() );
            
            matlast = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdreaded.data(), dims_out[0], dims_out[1] );
            delete dsJoined; dsJoined = nullptr;
            
            retsvd = RcppbdSVD_lapack(matlast, false, false, false);
            
            
            // Write results to hdf5 file : in folder "SVD" and dataset "SVD".<name input dataset>
            dsd->createDataset( 1, retsvd.d.size(), "real");
            dsd->writeDataset( Rcpp::wrap(retsvd.d) );
            
            // 3.- crossprod initial matrix and svdA$u
            
            if( bcenter == true || bscale == true || (dsA->getGroupName().find("NORMALIZED_T") != std::string::npos) ) {
                
                if(bcenter == true || bscale == true) {
                    
                    dsnormalizedData = new BigDataStatMeth::hdf5Dataset( dsA->getFullPath(), strGroupName, "normalmatrix", false);
                    dsnormalizedData->openDataset();
                    
                    dims_out = dsnormalizedData->dim();
                    
                    Eigen::MatrixXd A = Eigen::MatrixXd::Zero(dims_out[1], dims_out[0]);
                    dsnormalizedData->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, A.data() );
                    
                    delete dsnormalizedData; dsnormalizedData = nullptr;
                    
                    if(transp == false) {
                        v = A * retsvd.u;
                    } else {
                        v = A.transpose() * retsvd.u;
                        // v = Rcpp_block_matrix_mul_parallel(A,v retsvd.u, false, false, R_NilValue, threads); // multiplication
                    }
                    
                    // v = Rcpp_block_matrix_mul_parallel(A, retsvd.u, false, false, R_NilValue, threads);
                    
                } else {
                    
                    dsnormalizedData_i = new BigDataStatMeth::hdf5DatasetInternal( dsA->getFullPath(), dsA->getGroupName(), dsA->getDatasetName(), false);
                    dsnormalizedData_i->openDataset();
                    
                    dims_out = dsnormalizedData_i->dim();
                    
                    std::vector<double> vdA( dims_out[0] * dims_out[1] );
                    dsnormalizedData_i->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vdA.data() );
                    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> A (vdA.data(), dims_out[0], dims_out[1]);
                    delete dsnormalizedData_i; dsnormalizedData_i= nullptr;
                    
                    v = Rcpp_block_matrix_mul_parallel(A, retsvd.u, false, false, R_NilValue, threads);
                    
                }
                
            } else {
                
                dims_out = dsA->dim();
                Eigen::MatrixXd A;
                std::vector<double> vdA( dims_out[0] * dims_out[1] );
                dsA->readDatasetBlock( {0,0}, {dims_out[0], dims_out[1] }, stride, block, vdA.data() );
                
                A = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdA.data(), dims_out[1], dims_out[0] );
                
                if(transp == false) {
                    v = Rcpp_block_matrix_mul_parallel(A, retsvd.u, true, false, R_NilValue, threads); // crossprod
                } else {
                    v = Rcpp_block_matrix_mul_parallel(A, retsvd.u, false, false, R_NilValue, threads); // multiplication
                }
            }
            
            // 4.- resuls / svdA$d
            // v = v.array().rowwise()/(retsvd.d).transpose().array();
            v = v.array().rowwise() / Eigen::Map<Eigen::RowVectorXd>(retsvd.d.data(), (retsvd.d).size()).array();
            
            if (transp == true)  {
                dsu->createDataset( v.rows(), v.cols(), "real");
                dsv->createDataset( retsvd.u.rows(), retsvd.u.cols(), "real");
                
                dsu->writeDataset(Rcpp::wrap(v));
                dsv->writeDataset(Rcpp::wrap(retsvd.u));
            } else {
                dsu->createDataset( retsvd.u.rows(), retsvd.u.cols(), "real");
                dsv->createDataset( v.rows(), v.cols(), "real");
                
                dsu->writeDataset(Rcpp::wrap(retsvd.u));
                dsv->writeDataset(Rcpp::wrap(v));
            }
            
            remove_elements(dsA->getFileptr(), strGroupName);
            
            
        }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsd, dsu, dsv, dsnormalizedData, dsJoined, dsnormalizedData_i);
            Rcpp::Rcerr<<"\nc++ exception RcppbdSVD_hdf5_Block (File IException)\n";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsd, dsu, dsv, dsnormalizedData, dsJoined, dsnormalizedData_i);
            Rcpp::Rcerr<<"\nc++ exception RcppbdSVD_hdf5_Block (DataSet IException)\n";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsd, dsu, dsv, dsnormalizedData, dsJoined, dsnormalizedData_i);
            Rcpp::Rcerr<< "C++ exception RcppbdSVD_hdf5_Block : "<< ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsA, dsd, dsu, dsv, dsnormalizedData, dsJoined, dsnormalizedData_i);
            Rcpp::Rcerr<<"\nC++ exception RcppbdSVD_hdf5_Block (unknown reason)";
            return void();
        }
        
        return void();
    }
    
    
    /**
     * @brief Main SVD computation function for HDF5 matrices
     * @details This is the main interface for computing SVD of matrices stored
     * in HDF5 format. It automatically selects between direct LAPACK computation
     * for small matrices and block-wise decomposition for large matrices.
     * 
     * @param filename Path to the HDF5 file containing input matrix
     * @param strsubgroup Group path within the HDF5 file
     * @param strdataset Dataset name containing the matrix
     * @param k Number of local SVDs to concatenate at each level
     * @param q Number of decomposition levels
     * @param nev Number of eigenvalues per block
     * @param bcenter Whether to center the data before SVD
     * @param bscale Whether to scale the data before SVD
     * @param dthreshold Numerical threshold for computations
     * @param bforce Whether to overwrite existing results
     * @param asRowMajor Whether to interpret matrix as row-major
     * @param method Computation method ("auto", "blocks", "full")
     * @param ithreads Number of parallel threads (optional)
     * 
     * @throws H5::FileIException for HDF5 file operation errors
     * @throws H5::DataSetIException for HDF5 dataset operation errors
     * @throws std::exception for computation errors
     * 
     * @note Results are written to HDF5 datasets under "SVD/<dataset_name>/"
     *       - Singular values: "SVD/<dataset_name>/d"
     *       - Left singular vectors: "SVD/<dataset_name>/u"
     *       - Right singular vectors: "SVD/<dataset_name>/v"
     * 
     * @warning Large matrices require significant computational resources
     * 
     * @see BigDataStatMeth::hdf5Dataset
     * @see RcppbdSVD_hdf5_Block
     * @see RcppbdSVD_lapack
     */
    inline void RcppbdSVD_hdf5( std::string filename, std::string strsubgroup, std::string strdataset,  
                                int k, int q, int nev, bool bcenter, bool bscale, double dthreshold, 
                                bool bforce, bool asRowMajor, 
                                Rcpp::Nullable<Rcpp::CharacterVector> method = R_NilValue,
                                Rcpp::Nullable<int> ithreads = R_NilValue)
    {
        
        BigDataStatMeth::hdf5Dataset* dsA = nullptr;
        BigDataStatMeth::hdf5Dataset* dsu = nullptr;
        BigDataStatMeth::hdf5Dataset* dsv = nullptr;
        BigDataStatMeth::hdf5Dataset* dsd = nullptr;
        
        try {
            
            std::string strMethod;
            
            std::vector<hsize_t> stride = {1, 1},
                block = {1, 1},
                offset = {0, 0},
                count = {0, 0};
            
            std::vector<std::string> strMethods = {"auto", "blocks", "full"};
            
            if(method.isNull())  strMethod = "auto" ;
            else    strMethod = Rcpp::as<std::string>(method);
            
            dsA = new BigDataStatMeth::hdf5Dataset(filename, strsubgroup, strdataset, false);
            dsA->openDataset();
            
            if( dsA->getDatasetptr() != nullptr ) { 
                
                // Create results folder
                std::string stroutgroup = "SVD/"+ strdataset;
                
                std::vector<hsize_t> dims_out = {dsA->nrows(), dsA->ncols()};;
                count = { dims_out[0], dims_out[1]};
                
                // Small matrices ==> Direct SVD (lapack)
                if( (dims_out[0] * dims_out[1] < (MAXELEMSINBLOCK / 20) && strMethod == "auto") || strMethod == "full" ) {
                    
                    // Rcpp::Rcout<<"\nEste, aquÃ­ - 1";
                    Eigen::MatrixXd X;
                    svdeig retsvd;
                    
                    std::vector<double> vdA( count[0] * count[1] ); 
                    dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
                    
                    if(asRowMajor == true) {
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdA.data(), count[0], count[1] );    
                        retsvd = RcppbdSVD_lapack(X, bcenter, bscale, false);
                    } else {
                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> X (vdA.data(), count[1], count[0] );
                        retsvd = RcppbdSVD_lapack(X, bcenter, bscale, false);
                    }
                    
                    dsu = new hdf5Dataset(filename, stroutgroup, "u", bforce);
                    dsu->createDataset( retsvd.u.rows(), retsvd.u.cols(), "real");
                    dsu->writeDataset( Rcpp::wrap(retsvd.u) );
                    
                    dsv = new hdf5Dataset(filename, stroutgroup, "v", bforce);
                    dsv->createDataset( retsvd.v.rows(), retsvd.v.cols(), "real");
                    dsv->writeDataset( Rcpp::wrap(retsvd.v) );
                    
                    dsd = new hdf5Dataset(filename, stroutgroup, "d", bforce);
                    //.. COMENTAT 2025-02-06 ..// dsd->createDataset( retsvd.d.size(), 1, "real");
                    dsd->createDataset( 1, retsvd.d.size(), "real");
                    dsd->writeDataset( Rcpp::wrap(retsvd.d) );
                    
                } else {
                    // Rcpp::Rcout<<"\nEste, aquÃ­ - 2";
                    dsu = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "u", true);
                    dsv = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "v", true);
                    dsd = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "d", true);
                    
                    if( dsA->getDatasetptr() != nullptr) {
                        RcppbdSVD_hdf5_Block( dsA, dsu, dsv, dsd, k, q, nev, bcenter, bscale, count[1], count[0], dthreshold, ithreads );
                    }
                    
                } 
            } 
            
            delete dsu; dsu = nullptr;
            delete dsv; dsv = nullptr;
            delete dsd; dsd = nullptr;
            delete dsA; dsA = nullptr;
            
        }  catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
            checkClose_file(dsA, dsd, dsu, dsv);
            Rcpp::Rcerr<<"\nc++ exception RcppbdSVD_hdf5 (File IException)\n";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsd, dsu, dsv);
            Rcpp::Rcerr<<"\nc++ exception RcppbdSVD_hdf5 (DataSet IException)\n";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsd, dsu, dsv);
            Rcpp::Rcerr<<"c++ exception RcppbdSVD_hdf5 \n"<< ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsA, dsd, dsu, dsv);
            Rcpp::Rcerr<<"\nC++ exception RcppbdSVD_hdf5 (unknown reason)";
            return void();
        }
        
        return void();
    }


}

#endif // BIGDATASTATMETH_HDF5_MATRIXSVD_HPP
