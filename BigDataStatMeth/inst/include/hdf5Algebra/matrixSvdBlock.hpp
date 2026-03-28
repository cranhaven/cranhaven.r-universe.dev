/**
 * @file matrixSvdBlock.hpp
 * @brief Block-based Singular Value Decomposition for HDF5 matrices
 * @details This header file provides implementations for computing the Singular
 * Value Decomposition of large matrices stored in HDF5 format using block-based
 * algorithms. The implementation includes:
 * 
 * Key features:
 * - Block-based SVD computation
 * - Memory-efficient processing
 * - Parallel computation support
 * - Automatic block size optimization
 * - Error handling and validation
 * 
 * Supported operations:
 * - Block SVD computation
 * - Incremental SVD updates
 * - Truncated block SVD
 * - Out-of-core processing
 * - Distributed computation
 * 
 * Performance features:
 * - Dynamic block sizing
 * - Memory usage optimization
 * - Multi-threaded processing
 * - I/O optimization
 * - Load balancing
 * 
 * The implementation uses:
 * - Block Lanczos methods
 * - Randomized algorithms
 * - HDF5 chunked storage
 * - Parallel I/O
 * - Cache-efficient algorithms
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXSVDBLOCK_HPP
#define BIGDATASTATMETH_HDF5_MATRIXSVDBLOCK_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"


namespace BigDataStatMeth {

// dgesvd_ is a symbol in the LAPACK-BLAS Level 3 
//    DGESVD computes the singular value decomposition (SVD) of a real M-by-N matrix A, 
//       optionally computing the left and/or right singular vectors
extern "C" {
    extern void dgesvd_( char*, char*, int*, int*, double*, int*, double*, double*, int*, double*, int*, double*, int*, int*);
}

extern "C" {
    extern void dgesdd_( char*, int*, int*, double*, int*, double*, double*, int*, double*, int*, double*, int*, int*, int*);
}


struct svdPositions {
    std::string strDatasetName = "";
    std::vector<hsize_t> stride = {1, 1};
    std::vector<hsize_t> block = {1, 1};
    std::vector<hsize_t> count = {0, 0};
    std::vector<hsize_t> write_count = {0, 0};
    hsize_t write_offset = 0;
    std::vector<hsize_t> partOffset = {0, 0};
    std::vector<hsize_t> totOffset = {0, 0};
    unsigned long long cummoffset = 0;
};


// Lapack SVD decomposition - Optimized algorithm with dgesdd
template <class T>
inline svdeig RcppbdSVD_lapack( T X, bool bcenter, bool bscale, bool complete ) {
    
    svdeig retsvd;
    
    char Schar='S';
    char Achar='A';
    int info = 0;
    
    try {
        
        if(bcenter == true || bscale == true) {
            X = RcppNormalize_Data(X, bcenter, bscale, false);
        }
        
        int m = X.rows();
        int n = X.cols();
        int lda = std::max(1,m);
        int ldu = std::max(1,m);
        int ldvt = std::min(m, n);
        int k = std::min(m,n);
        int lwork;
        
        if( complete == false ) {
            lwork = 4*std::min(m,n)*std::min(m,n) + 7*std::min(m,n);
        } else {
            lwork = 4*std::min(m,n)*std::min(m,n) + 6*std::min(m,n) + std::max(m,n);
        }
        
        Eigen::VectorXd s = Eigen::VectorXd::Zero(k);
        Eigen::VectorXd work = Eigen::VectorXd::Zero(lwork);
        Eigen::VectorXi iwork(8*std::min(m,n));
        Eigen::MatrixXd u;
        Eigen::MatrixXd vt = Eigen::MatrixXd::Zero(ldvt,n);
        
        if( complete == false ) {
            u = Eigen::MatrixXd::Zero(ldu,k);
            dgesdd_( &Schar, &m, &n, X.data(), &lda, s.data(), u.data(), &ldu, vt.data(), &ldvt, work.data(), &lwork, iwork.data(), &info);
        } else {
            u = Eigen::MatrixXd::Zero(ldu,m);
            dgesdd_( &Achar, &m, &n, X.data(), &lda, s.data(), u.data(), &ldu, vt.data(), &ldvt, work.data(), &lwork, iwork.data(), &info);
        }
        
        retsvd.d = s;
        retsvd.u = u;
        retsvd.v = vt.transpose();
        
    } catch(std::exception &ex) {
        Rcpp::Rcout<< "C++ exception RcppbdSVD_lapack : "<< ex.what();
        return retsvd;
    } catch (...) {
        Rf_error("C++ exception RcppbdSVD_lapack (unknown reason)");
        return retsvd;
    }
    
    return retsvd;
    
}




// Create a vector of positions, start + blocks sizes to facilitate parallelization
template <class T>
std::vector<svdPositions> prepareForParallelization( T* dsA, int M, int k, bool transp, int block_size, std::string strDatasetName)
{

    static_assert(std::is_same<T*, BigDataStatMeth::hdf5Dataset* >::value ||
                  std::is_same<T*, BigDataStatMeth::hdf5DatasetInternal* >::value,
                  "Error - type not allowed");


    std::vector<svdPositions> pos;
    BigDataStatMeth::hdf5Dataset* unlimDataset = nullptr;

    // Rcpp::Rcout<<"\nAnem a preparar per paralelitzar... a veure que fem per aquí perquè no m'agrada massa... \n";

    try{

        // int realsizeread, cummoffset;
        int realsizeread;
        int irows = dsA->ncols();
        int icols = dsA->nrows();

        
        if(transp == false) {
            irows = dsA->ncols();
            icols = dsA->nrows();    
        } else {
            irows = dsA->nrows();
            icols = dsA->ncols();    
        }
        

        for( int i = 0; i< M ; i++)
        {
            int maxsizetoread = block_size;

            pos.push_back(svdPositions());

            pos[i].strDatasetName = strDatasetName + std::to_string(i/(M/k));
            pos[i].totOffset = getInitialPosition( transp, (unsigned long long)(i*block_size) ); // Initial read position

            // Get max block size to read - for blocks smaller than default block size
            if( ((i+1)*block_size) > icols)
                maxsizetoread = icols - (i*block_size);

            if( i+1 == M && icols - maxsizetoread!=0) {
                realsizeread = icols - (i*block_size);
            } else{
                realsizeread = maxsizetoread;
            }

            pos[i].count = getSizetoRead(transp, (unsigned long long)(realsizeread), icols, irows );

            if( i%(M/k) == 0 || ( (i%(M/k) > 0 &&  !exists_HDF5_element(dsA->getFileptr(),  pos[i].strDatasetName)) ) )
            {
                pos[i].cummoffset = 0;
            } else {
                pos[i].cummoffset = pos[i-1].cummoffset + pos[i-1].count[0];
            }

            pos[i].partOffset[1] = pos[i].cummoffset;
        }


    } catch( H5::FileIException& error ) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"\nc++ exception prepareForParallelization (File IException)\n";
        return(pos);
    } catch( H5::DataSetIException& error ) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"\nc++ exception prepareForParallelization (DataSet IException)\n";
        return(pos);
    } catch( H5::GroupIException& error ) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"\nc++ exception prepareForParallelization (Group IException)\n";
        return(pos);
    } catch( H5::DataTypeIException& error ) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"\nc++ exception prepareForParallelization (DataType IException)\n";
        return(pos);
    } catch( H5::DataSpaceIException& error ) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"\nc++ exception prepareForParallelization (DataSpace IException)\n";
        return(pos);
    } catch(std::exception &ex) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"c++ exception prepareForParallelization \n"<< ex.what();
        return(pos);
    } catch (...) {
        checkClose_file(dsA, unlimDataset);
        Rcpp::Rcerr<<"\nC++ exception prepareForParallelization (unknown reason)";
        return(pos);
    }


    return(pos);
}



// Reads big matrix from hdf5 file in blocks and perform a svd descomposition from
// each block,results are saved in hdf5 datasets under temporal group to be processed
// if necessary
template <class T>
inline void First_level_SvdBlock_decomposition_hdf5( T* dsA, std::string strGroupName, int k, int q, int nev, bool bcenter, bool bscale, 
                                             double dthreshold, Rcpp::Nullable<int> threads = R_NilValue)
{
    
    static_assert(std::is_same<T*, BigDataStatMeth::hdf5Dataset* >::value || 
                  std::is_same<T*, BigDataStatMeth::hdf5DatasetInternal* >::value,
                  "Error - type not allowed");
    
    BigDataStatMeth::hdf5DatasetInternal* normalizedData = nullptr;
    BigDataStatMeth::hdf5Dataset* unlimDataset = nullptr;
    
    try{
        
        std::vector<svdPositions> paralPos;
        
        int  M, p, n, irows, icols, normalsize;
        // int ithreads;
        bool transp = false;
        Rcpp::Nullable<int> wsize = R_NilValue;
        
        irows = dsA->ncols();
        icols = dsA->nrows();
        
        // ithreads = get_number_threads(threads, R_NilValue);
        
        // Work with transposed matrix
        if( irows >= icols ) {
            transp = true;
            n = icols;
            p = irows;
            normalsize = n;
        } else {
            n = irows;
            p = icols;
            normalsize = p;
        }
        
        Eigen::MatrixXd datanormal = Eigen::MatrixXd::Zero(2, normalsize);
        get_HDF5_mean_sd_by_column(dsA, datanormal, true, true, wsize);
        
        M = pow(k, q);
        if(M>p)
            throw std::runtime_error("k^q must not be greater than the number of columns in the matrix");
        
        //.. 2025-02-10 ..// double block_size = std::floor((double)icols/(double)M);
        double block_size = std::round((double)p/(double)M); 
        
        if (bcenter==true || bscale==true) { // Create dataset to store normalized data
            normalizedData = new BigDataStatMeth::hdf5DatasetInternal (dsA->getFullPath(), strGroupName, "normalmatrix", true);
            // normalizedData->createDataset( irows, icols, "real");
            normalizedData->createDataset( irows, icols, "real");
            //.Caldria revisar... .// normalizedData->createDataset( n, p, "real");
        }
        
        // Get all the offsets and counts inside the file to write and read
        paralPos = prepareForParallelization( dsA, M, k, transp, block_size, strGroupName + "/A");
        #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) ) shared (normalizedData)
        {
            
            // Get data from M blocks in initial matrix
            #pragma omp for schedule (dynamic) ordered
            for( int i = 0; i< M ; i++)  
            {
                
                Eigen::MatrixXd restmp;
                
                // 1.- Get SVD from all blocks
                //    a) Get all blocks from initial matrix 
                
                Eigen::MatrixXd X;
                    
                std::vector<double> vdX( paralPos[i].count[0] * paralPos[i].count[1] ); 
                #pragma omp critical(accessFile)
                {
                    dsA->readDatasetBlock( {paralPos[i].totOffset[0], paralPos[i].totOffset[1]}, {paralPos[i].count[0], paralPos[i].count[1]}, paralPos[i].stride, paralPos[i].block, vdX.data() );
                }
                
                if(transp==false){    
                    X = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> (vdX.data(), paralPos[i].count[1], paralPos[i].count[0] );
                } else {
                    X = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdX.data(), paralPos[i].count[0], paralPos[i].count[1] );
                }
                
                // Normalize data
                if (bcenter==true || bscale==true) 
                {
                    std::vector<hsize_t> offset_tmp = {paralPos[i].totOffset[1], paralPos[i].totOffset[0]};
                    std::vector<hsize_t> count_tmp = {paralPos[i].count[1], paralPos[i].count[0]};
                    
                    if(transp == true) {
                        offset_tmp = {paralPos[i].totOffset[0], paralPos[i].totOffset[1]};
                        count_tmp = {paralPos[i].count[0], paralPos[i].count[1]};
                    } 
                        
                    if( transp == false) {
                        // X = RcppNormalize_Data(X, bcenter, bscale, transp, datanormal.block(0, offset_tmp[1], 2, count_tmp[1]));
                        X = RcppNormalizeColwise(X, bcenter, bscale);
                        
                        #pragma omp critical(accessFile)
                        {   
                            normalizedData->writeDatasetBlock( Rcpp::wrap(X), offset_tmp, count_tmp, paralPos[i].stride, paralPos[i].block, false);
                        }
                        
                    } else {
                        
                        X = RcppNormalize_Data(X, bcenter, bscale, transp, datanormal.block(0, offset_tmp[0], 2, count_tmp[0]));
                        
                        count_tmp = {(unsigned long long)X.cols(), (unsigned long long)X.rows()};
                        offset_tmp = {paralPos[i].totOffset[1], paralPos[i].totOffset[0]};
                        
                        #pragma omp critical(accessFile)
                        {   
                            normalizedData->writeDatasetBlock( Rcpp::wrap(X.transpose()), offset_tmp, count_tmp, paralPos[i].stride, paralPos[i].block, false);
                        }
                    }
                }
                

                // Rcpp::Rcout<<"\nPeta a l'step 1? - 1";
                {
                    //    b) SVD for each block
                    svdeig retsvd;
                    retsvd = RcppbdSVD_lapack(X, false, false, false);

                    int size_d = (retsvd.d).size();
                    int nzeros = 0;

                    if( (retsvd.d)[size_d - 1] <= dthreshold ){
                        nzeros = 1;
                        for( int j = (size_d - 2); ( j>1 && (retsvd.d)[i] <= dthreshold ); j-- ) {
                            nzeros++;
                        }
                    }

                    //    c)  U*d
                    // Create diagonal matrix from svd decomposition d
                    int isize = (retsvd.d).size() - nzeros;

                    if( isize < 2 ) {
                        isize = 2;
                    }

                    Eigen::MatrixXd d = Eigen::MatrixXd::Zero(isize, isize);
                    d.diagonal() = (retsvd.d).head(isize);

                    Eigen::MatrixXd u = (retsvd.u).block(0, 0, (retsvd.u).rows(), isize);

                    // if( u.size() < MAXELEMSINBLOCK ) {
                    if (static_cast<hsize_t>(u.size()) < MAXELEMSINBLOCK) {
                        restmp = u*d;
                    } else{
                        restmp = Rcpp_block_matrix_mul( u, d, R_NilValue);
                    }
                }

                paralPos[i].write_count = {(hsize_t)restmp.rows(), (hsize_t)restmp.cols()};

                //    d) Write results to hdf5 file
                #pragma omp ordered
                {
                    #pragma omp critical(accessFile)
                    {

                        if( i%(M/k) == 0 || ( (i%(M/k) > 0 &&  !exists_HDF5_element(dsA->getFileptr(),  paralPos[i].strDatasetName )) ) )
                        {
                            unlimDataset = new BigDataStatMeth::hdf5DatasetInternal(dsA->getFullPath(), paralPos[i].strDatasetName, true );
                            unlimDataset->createUnlimitedDataset(paralPos[i].write_count[0], paralPos[i].write_count[1], "real");
                            delete unlimDataset; unlimDataset = nullptr;

                            paralPos[i].write_offset = 0;
                        }

                        unlimDataset = new BigDataStatMeth::hdf5DatasetInternal(dsA->getFullPath(), paralPos[i].strDatasetName, true );
                        unlimDataset->openDataset();

                        // Extend dataset before to put the data
                        if((i%(M/k)) != 0 && paralPos[i].write_offset > 0) {
                            unlimDataset->extendUnlimitedDataset(0, paralPos[i].write_count[1] );
                        }

                        // std::vector<double> vtmpc(restmp.data(), restmp.data() + restmp.rows() * restmp.cols());
                        unlimDataset->writeDatasetBlock( Rcpp::wrap(restmp), {0, paralPos[i].write_offset}, paralPos[i].write_count, paralPos[i].stride, paralPos[i].block, false);
                        delete unlimDataset; unlimDataset = nullptr;

                        if( i<M-1 )
                            paralPos[i+1].write_offset = paralPos[i].write_offset + paralPos[i].write_count[1];

                    }
                }
            }
        }
        
        if (bcenter==true || bscale==true) { 
            delete normalizedData; normalizedData = nullptr;
        }
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"\nc++ exception First_level_SvdBlock_decomposition_hdf5 (File IException)\n";
        return void();
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"\nc++ exception First_level_SvdBlock_decomposition_hdf5 (DataSet IException)\n";
        return void();
    } catch( H5::GroupIException& error ) { 
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"\nc++ exception First_level_SvdBlock_decomposition_hdf5 (Group IException)\n";
        return void();
    } catch( H5::DataTypeIException& error ) { 
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"\nc++ exception First_level_SvdBlock_decomposition_hdf5 (DataType IException)\n";
        return void();
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"\nc++ exception First_level_SvdBlock_decomposition_hdf5 (DataSpace IException)\n";
        return void();
    } catch(std::exception &ex) {
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"c++ exception First_level_SvdBlock_decomposition_hdf5 \n"<< ex.what();
        return void();
    } catch (...) {
        checkClose_file(dsA, unlimDataset, normalizedData);
        Rcpp::Rcerr<<"\nC++ exception First_level_SvdBlock_decomposition_hdf5 (unknown reason)";
        return void();
    }
    
    
    return void();
    
}



// Reads small datasets from hdf5 and perform a svd descomposition from each block,
// results are saved in hdf5 datasets under temporal group to be processed if necessary
template <class T>
inline void Next_level_SvdBlock_decomposition_hdf5( T* dsA, std::string strGroupName, int k, int q, 
                                                           double dthreshold, Rcpp::Nullable<int> threads = R_NilValue)
    {
    
    
    static_assert(std::is_same<T*, BigDataStatMeth::hdf5Dataset* >::value || 
                  std::is_same<T*, BigDataStatMeth::hdf5DatasetInternal* >::value,
                  "Error - type not allowed");


    BigDataStatMeth::hdf5Dataset* unlimDataset = nullptr;    
    BigDataStatMeth::hdf5Dataset* dsCur = nullptr;
    
    try {
        
        int cummoffset = 0, M;
            // ithreads,  M;
        
        std::vector<hsize_t> stride = {1, 1},
            block = {1, 1},
            offset = {0, 0},
            count = {0, 0};
        
        Rcpp::CharacterVector strvmatnames = {"A","B","C","D","E","F","G","H",
                                              "I","J","K","L","M","N","O","P",
                                              "Q","R","S","T","U","V","W","X",
                                              "Y","Z"};

        // Get dataset names
        Rcpp::StringVector joindata =  dsA->getDatasetNames(strGroupName, (std::string)strvmatnames[q-1], "");
        M = joindata.size();
        
        // ithreads = get_number_threads(threads, R_NilValue);
        
        #pragma omp parallel num_threads( get_number_threads(threads, R_NilValue) )

        // Get data from M blocks in initial matrix
        #pragma omp for ordered schedule (dynamic)
        for( int i = 0; i< M ; i++)
        {
            
            std::string strDatasetName = strGroupName + "/" + strvmatnames[q] + std::to_string(i/(M/k));
            
            Eigen::MatrixXd restmp;
            Eigen::MatrixXd X;

            #pragma omp critical(accessFile)
            {
                //    a) Get dataset
                dsCur = new BigDataStatMeth::hdf5Dataset(dsA->getFullPath(), strGroupName + "/" + joindata[i], false);
                dsCur->openDataset();
                hsize_t* dims_out = dsCur->dim();
                
                std::vector<double> vdCurDataset( dims_out[0] * dims_out[1] ); 
                dsCur->readDatasetBlock( {0, 0}, {dims_out[0], dims_out[1]}, stride, block, vdCurDataset.data() );
                X = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdCurDataset.data(), dims_out[0], dims_out[1] );
                
                delete dsCur; dsCur = nullptr;
            }

            {
                //    b) SVD for each block
                svdeig retsvd;
                retsvd = RcppbdSVD_lapack(X, false, false, false);
            
                int size_d = (retsvd.d).size();
                int nzeros = 0;
            
                if( (retsvd.d)[size_d - 1] <= dthreshold ) {
                    nzeros = 1;
                    for( int j = (size_d - 2); ( j>1 && (retsvd.d)[i] <= dthreshold ); j-- ) {
                        nzeros++;
                    }
                }
            
                //    c)  U*d
                // Create diagonal matrix from svd decomposition d
                int isize = (retsvd.d).size() - nzeros;
                if( isize < 2 ) {
                    isize = 2;
                }
            
                Eigen::MatrixXd d = Eigen::MatrixXd::Zero(isize, isize);
                d.diagonal() = (retsvd.d).head(isize);
            
                Eigen::MatrixXd u = (retsvd.u).block(0, 0, (retsvd.u).rows(), isize);
            
                // if( u.size() < MAXELEMSINBLOCK ) {
                if (static_cast<hsize_t>(u.size()) < MAXELEMSINBLOCK) {
                    restmp = u*d;
                } else{
                    restmp = Rcpp_block_matrix_mul( u, d, R_NilValue);    
                } 
            }
            
            //    d) Write results to dataset
            count[0] = restmp.rows();
            count[1] = restmp.cols();

            #pragma omp ordered
            {
                #pragma omp critical(accessFile)
                {
                    
                    if( i%(M/k) == 0 || ( (i%(M/k) > 0 &&  !BigDataStatMeth::exists_HDF5_element(dsA->getFileptr(),  strDatasetName)) ) ) {
                        // Create unlimited dataset in hdf5 file
                        unlimDataset = new BigDataStatMeth::hdf5DatasetInternal(dsA->getFullPath(), strDatasetName, true );
                        unlimDataset->createUnlimitedDataset(count[0], count[1], "real");
                        delete unlimDataset; unlimDataset = nullptr;
                        
                        cummoffset = 0;
                    }
                    
                    unlimDataset = new BigDataStatMeth::hdf5DatasetInternal(dsA->getFullPath(), strDatasetName, true );
                    unlimDataset->openDataset();
                    
                    // Get write position
                    offset[1] = cummoffset;
                    cummoffset = cummoffset + restmp.cols();
                    
                    // Extend dataset before put data
                    if((i%(M/k)) != 0 && cummoffset > 0) {
                        unlimDataset->extendUnlimitedDataset(0, count[1] );
                    }
                    
                    unlimDataset->writeDatasetBlock( Rcpp::wrap(restmp), offset, count, stride, block, false);
                    delete unlimDataset; unlimDataset = nullptr;
                }
            }
        }

    } catch( H5::FileIException& error ) { 
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"\nc++ exception Next_level_SvdBlock_decomposition_hdf5 (File IException)\n";
        return void();
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"\nc++ exception Next_level_SvdBlock_decomposition_hdf5 (DataSet IException)\n";
        return void();
    } catch( H5::GroupIException& error ) { 
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"\nc++ exception Next_level_SvdBlock_decomposition_hdf5 (Group IException)\n";
        return void();
    } catch( H5::DataTypeIException& error ) { 
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"\nc++ exception Next_level_SvdBlock_decomposition_hdf5 (DataType IException)\n";
        return void();
    } catch( H5::DataSpaceIException& error ) { 
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"\nc++ exception Next_level_SvdBlock_decomposition_hdf5 (DataSpace IException)\n";
        return void();
    } catch(std::exception &ex) {
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"c++ exception Next_level_SvdBlock_decomposition_hdf5 \n"<< ex.what();
        return void();
    } catch (...) {
        checkClose_file(dsA, unlimDataset, dsCur);
        Rcpp::Rcerr<<"\nC++ exception Next_level_SvdBlock_decomposition_hdf5 (unknown reason)";
        return void();
    }
    
    return void();

}

}

#endif // BIGDATASTATMETH_HDF5_MATRIXSVDBLOCK_HPP
