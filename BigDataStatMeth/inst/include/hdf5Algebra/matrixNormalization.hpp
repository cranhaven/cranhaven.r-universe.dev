/**
 * @file matrixNormalization.hpp
 * @brief Matrix normalization operations for HDF5 matrices
 * @details This header file provides implementations for matrix normalization
 * operations on matrices stored in HDF5 format. The implementation includes:
 * 
 * Key features:
 * - Column-wise normalization
 * - Row-wise normalization
 * - Mean centering
 * - Standard deviation scaling
 * - Block-based processing
 * 
 * Supported operations:
 * - Z-score normalization
 * - Mean centering only
 * - Standard deviation scaling only
 * - Custom normalization parameters
 * - Parallel processing support
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Block-based computation
 * - Multi-threaded processing
 * - Memory-efficient algorithms
 * - I/O optimization
 * 
 * The implementation uses:
 * - Efficient statistical computations
 * - Block algorithms
 * - HDF5 chunked storage
 * - Parallel I/O
 * - Vectorized operations
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXSNORMALIZATION_HPP
#define BIGDATASTATMETH_HDF5_MATRIXSNORMALIZATION_HPP


namespace BigDataStatMeth {


    // Function declaration
    template< typename M> inline Eigen::MatrixXd RcppNormalizeColwise ( M  X, bool bc, bool bs, Eigen::MatrixXd normdata );
    template< typename M> inline Eigen::MatrixXd RcppNormalizeColwise ( M  X, bool bc, bool bs);
    
    template< typename M> inline Eigen::MatrixXd RcppNormalizeRowwise ( M  X, bool bc, bool bs, Eigen::MatrixXd normdata );
    template< typename M> inline Eigen::MatrixXd RcppNormalizeRowwise ( M  X, bool bc, bool bs );
    
    template< typename M> inline M RcppNormalize_Data ( M  X, bool bc, bool bs, bool btransp, Eigen::MatrixXd normdata );
    template< typename M> inline M RcppNormalize_Data ( M  X, bool bc, bool bs, bool bRowMajor );
    
    
    
    // Internal call - 
    //   To be used when we have SD and Mean computed for each row/column and we 
    //   use this data to compute normalized matrix
    
    //.. ORIGINAL name ..// Eigen::MatrixXd RcppNormalize_Data_hdf5 ( Eigen::MatrixXd  X, bool bc, bool bs, bool btransp, Eigen::MatrixXd normdata )
    /**
     * @brief Template function for normalizing data with pre-computed statistics
     * @details Normalizes matrix data using pre-computed mean and standard deviation.
     * Supports both row-wise and column-wise normalization.
     * 
     * @tparam M Matrix type (Eigen::MatrixXd or mapped matrix)
     * @param X Input matrix to normalize
     * @param bc Whether to center the data
     * @param bs Whether to scale the data
     * @param btransp Whether to transpose before normalization
     * @param normdata Pre-computed normalization parameters (mean and std)
     * @return Normalized matrix
     */
    template< typename M>
    inline M RcppNormalize_Data ( M  X, bool bc, bool bs, bool btransp, Eigen::MatrixXd normdata )
    {
        
        static_assert(std::is_same<M, Eigen::MatrixXd >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value ,
                      "Error - type not allowed");
        
        Eigen::MatrixXd rX = X;

        if( btransp == true)
        {
            rX = RcppNormalizeRowwise(X, bc, bs, normdata );

        } else {
            rX = RcppNormalizeColwise(X, bc, bs, normdata );
        }

        return(rX);
    }
    

    /**
     * @brief Column-wise normalization with pre-computed statistics
     * @details Normalizes matrix columns using pre-computed mean and standard deviation.
     * 
     * @tparam M Matrix type (Eigen::MatrixXd or mapped matrix)
     * @param X Input matrix to normalize
     * @param bc Whether to center the data
     * @param bs Whether to scale the data
     * @param normdata Pre-computed normalization parameters (mean and std)
     * @return Column-wise normalized matrix
     */
    template< typename M>
    inline Eigen::MatrixXd RcppNormalizeColwise ( M  X, bool bc, bool bs, Eigen::MatrixXd normdata )
    {
        
        static_assert(std::is_same<M, Eigen::MatrixXd >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                      "Error - type not allowed");
        
        Eigen::MatrixXd rX = X;
        
        Eigen::RowVectorXd std = normdata.row(1);
        Eigen::RowVectorXd mean = normdata.row(0);
        
        if( bc==true && bs==true )  {
            
            rX = (X.rowwise() - mean).array().rowwise() / std.array();
            
        }   else if (bc == true  && bs==false)   {
            
            rX = (X.rowwise() - mean);
            
        }  else if ( bc == false && bs == true)   {
            
            rX = X.array().rowwise() / std.array();
        }
        
        return(rX);
    }
    
    
    
    template< typename M>
    inline Eigen::MatrixXd RcppNormalizeColwise ( M  X, bool bc, bool bs )
    {
        
        static_assert(std::is_same<M, Eigen::MatrixXd >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                      "Error - type not allowed");
        
        Eigen::MatrixXd rX = X;
        
        if( bc==true && bs==true )  {
            
            Eigen::RowVectorXd mean = X.colwise().mean();
            Eigen::RowVectorXd std = ((X.rowwise() - mean).array().square().colwise().sum() / (X.rows() - 1)).sqrt();
            rX = (X.rowwise() - mean).array().rowwise() / std.array();
            
        }   else if (bc == true  && bs==false)   {
            
            Eigen::RowVectorXd mean = X.colwise().mean();
            rX = (X.rowwise() - mean);
            
        }  else if ( bc == false && bs == true)   {
            
            Eigen::RowVectorXd mean = X.colwise().mean();
            Eigen::RowVectorXd std = (X.array().square().colwise().sum() / (X.rows() - 1)).sqrt();
            rX = X.array().rowwise() / std.array();
        }
        
        return(rX);
    }
    
    
    template< typename M>
    inline Eigen::MatrixXd RcppNormalizeRowwise ( M  X, bool bc, bool bs )
    {
        
        static_assert(std::is_same<M, Eigen::MatrixXd >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                      "Error - type not allowed");
        
        Eigen::MatrixXd rX = X;
        
        Eigen::VectorXd mean = X.rowwise().mean();
        if( bc==true && bs==true )  {

            Eigen::VectorXd std = ((X.colwise() - mean).array().square().rowwise().sum() / (X.cols() - 1)).sqrt();
            rX = (X.colwise() - mean).array().colwise() / std.array();
            
        }   else if (bc == true  && bs==false)   {
            rX = (X.colwise() - mean);
            
        }  else if ( bc == false && bs == true)   {
            
            Eigen::VectorXd std = ((X.colwise() - mean).array().square().rowwise().sum() / (X.cols() - 1)).sqrt();
            rX = X.array().colwise() / std.array();
        }
        
        return(rX);
    }
    
    
    template< typename M>
    inline Eigen::MatrixXd RcppNormalizeRowwise ( M  X, bool bc, bool bs, Eigen::MatrixXd normdata )
    {

        static_assert(std::is_same<M, Eigen::MatrixXd >::value ||
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value ||
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                      "Error - type not allowed");

        Eigen::MatrixXd rX = X;

        Eigen::VectorXd std = normdata.row(1);
        Eigen::VectorXd mean = normdata.row(0);
        
        if( bc==true && bs==true )  {

            rX = (X.colwise() - mean).array().colwise() / std.array();

        }   else if (bc == true  && bs==false)   {

            rX = (X.colwise() - mean);

        }  else if ( bc == false && bs == true)   {

            rX = X.array().colwise() / std.array();
        }

        return(rX);
    }
    

    // Internal call - 
    //   To be used when we don't have SD and Mean computed and we need
    //   to compute this data to get normalized matrix
    template< typename M>
    inline M RcppNormalize_Data ( M  X, bool bc, bool bs, bool bRowMajor )
    {
        static_assert(std::is_same<M, Eigen::MatrixXd >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> >::value || 
                      std::is_same<M, Eigen::Map< Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> >::value,
                      "Error - type not allowed");
        
        
        X = RcppNormalizeColwise(X, bc, bs );
        
        return(X); // Return data as initial type
    };


    inline Eigen::MatrixXd RcppNormalize_Data_R_hdf5( Eigen::MatrixXd  X, bool bc, bool bs,
                                               bool btransp, Eigen::MatrixXd normdata)
    {
        Eigen::MatrixXd rX;

        if( btransp == true) {
            if( bc==true && bs==true )  {
                rX = (X.colwise() - normdata.row(0).transpose() ).array().colwise() / normdata.row(1).transpose().array();
            }   else if (bc == true  && bs==false)   {
                rX = (X.colwise() - normdata.row(0).transpose());
            }  else if ( bc == false && bs == true)   {
                rX = X.array().colwise() / normdata.row(1).transpose().array();
            }
        } else {
            if( bc==true && bs==true )  {
                rX = (X.rowwise() - normdata.row(0)).array().rowwise() / normdata.row(1).array();
            } else if (bc == true  && bs==false) {
                rX = (X.rowwise() - normdata.row(0));
            }  else if ( bc == false && bs == true)   {
                rX = X.array().rowwise() / normdata.row(1).array();
            }
        }

        return(rX);
    }
    
    
    
    /**
     * @brief HDF5 matrix normalization with pre-computed statistics
     * @details Normalizes an HDF5 matrix dataset using pre-computed statistics.
     * Supports both row-wise and column-wise normalization.
     * 
     * @param dsA Input matrix dataset
     * @param dsNormal Output normalized dataset
     * @param datanormal Pre-computed normalization parameters
     * @param wsize Block size for processing
     * @param bc Whether to center the data
     * @param bs Whether to scale the data
     * @param bbyrows Whether to normalize by rows
     * @param bcorrected Whether to use corrected standard deviation
     */
    inline void RcppNormalizeHdf5( BigDataStatMeth::hdf5Dataset* dsA,
                                          BigDataStatMeth::hdf5Dataset* dsNormal,
                                          Eigen::MatrixXd datanormal,
                                          Rcpp::Nullable<int> wsize, 
                                          bool bc, bool bs, bool bbyrows, bool bcorrected)
    {
        
        try{
            
            std::vector<hsize_t> stride = {1, 1},
                                 block = {1, 1};
            
            bool bgetTransposed;
            hsize_t nrows = dsA->nrows(), 
                    ncols = dsA->ncols(), 
                    nRowsCols, blocksize;
            double correction = 1;
            
            blocksize = BigDataStatMeth::get_block_size(wsize, nrows, ncols);
            
            if(bbyrows == false ) {
                nRowsCols = nrows;
                bgetTransposed = true;
            } else {
                nRowsCols = ncols;
                bgetTransposed = false;
            }
            
            if(bcorrected){
                if(bbyrows == false) {
                    correction = 1/sqrt(ncols - 1 );
                } else {
                    correction = 1/sqrt(nrows - 1 );
                }    
            }
            
            for(hsize_t i=0; i*blocksize <= nRowsCols ; i++)
            {
                std::vector<hsize_t> offset, 
                count;
                hsize_t sizetoread = 0;
                
                if( (i+1) * blocksize < nRowsCols ) {
                    sizetoread = blocksize;
                } else {
                    sizetoread = nRowsCols - ( i * blocksize );
                }
                
                if(bbyrows == false) {
                    offset = { i*blocksize, 0 };
                    count = { sizetoread, ncols };    
                } else {
                    offset = {  0, i*blocksize };
                    count = { nrows, sizetoread };
                }
                
                // Normalize and write data
                std::vector<double> vdA( count[0] * count[1] ); 
                dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdA.data(), count[0], count[1] );
                
                if(bbyrows == false) {
                    X = BigDataStatMeth::RcppNormalize_Data_R_hdf5(X, bc, bs, bgetTransposed, datanormal.block(0,offset[0], 2, count[0]));    
                } else {
                    X = BigDataStatMeth::RcppNormalize_Data_R_hdf5(X, bc, bs, bgetTransposed, datanormal.block(0,offset[1], 2, count[1]));
                }
                
                if(bcorrected){
                    X = X*correction;
                }
                
                dsNormal->writeRowMajorDatasetBlock( X, offset, count, stride, block);
                
            }
            
        }catch( H5::FileIException& error ) {
            checkClose_file(dsA, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5 (File IException)";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5 (DataSet IException)";
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5 (DataSpace IException)";
            return void();
        } catch( H5::DataTypeIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5 (DataType IException)";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception RcppNormalizeHdf5 : "<< ex.what();
        } catch (...) {
            checkClose_file(dsA, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception RcppNormalizeHdf5 (unknown reason)";
            return void();
        }
        
        return void();
    }
    
    
    
    // inline void RcppTypifyNormalizeHdf5( std::string filename, std::string strgroup, std::string strdataset,
    //                                             bool bc, bool bs, bool bbyrows)
    inline void RcppNormalizeHdf5( BigDataStatMeth::hdf5Dataset* dsA,
                                                bool bc, bool bs, bool bbyrows)
    {
        
        BigDataStatMeth::hdf5Dataset* dsmean = nullptr;
        BigDataStatMeth::hdf5Dataset* dssd = nullptr;
        BigDataStatMeth::hdf5Dataset* dsNormal = nullptr;
        
        try{
            
            Rcpp::Nullable<int> wsize = R_NilValue;
            Eigen::MatrixXd datanormal;
            hsize_t nrows, ncols;
            std::string strgroupout;
            bool bcorrected = false;
            
            nrows = dsA->nrows();
            ncols = dsA->ncols();
            
            strgroupout = "NORMALIZED/" + dsA->getGroupName();
            std::string strdatasetmean = "mean." + dsA->getDatasetName();
            std::string strdatasetsd = "sd." + dsA->getDatasetName();
            
            // Define blocksize atending number of elements in rows and cols
            if( bbyrows == false) {
                datanormal = Eigen::MatrixXd::Zero( 2, nrows);
                get_HDF5_mean_sd_by_column( dsA, datanormal, true, true, wsize);
            } else {
                datanormal = Eigen::MatrixXd::Zero( 2, ncols);
                get_HDF5_mean_sd_by_row( dsA, datanormal, true, true, wsize);
            }
            
            dsmean = new BigDataStatMeth::hdf5Dataset(dsA->getFileName(), strgroupout, strdatasetmean, true);
            dsmean->createDataset( datanormal.cols(), 1, "real");
            dsmean->writeDataset( Rcpp::wrap(datanormal.row(0)) );
            delete dsmean; dsmean = nullptr;
            
            dssd = new BigDataStatMeth::hdf5Dataset(dsA->getFileName(), strgroupout, strdatasetsd, true);
            dssd->createDataset( datanormal.cols(), 1, "real");
            dssd->writeDataset( Rcpp::wrap(datanormal.row(1)) );
            delete dssd; dssd = nullptr;
            
            dsNormal = new BigDataStatMeth::hdf5Dataset(dsA->getFileName(), strgroupout, dsA->getDatasetName(), true);
            dsNormal->createDataset( dsA, "real");
            
            if( dsA->getDatasetptr() != nullptr && dsNormal->getDatasetptr() != nullptr){
                BigDataStatMeth::RcppNormalizeHdf5( dsA, dsNormal, datanormal, wsize, bc, bs, bbyrows, bcorrected);
            }
            
            delete dsNormal; dsNormal = nullptr;
            
        } catch( H5::FileIException& error ) {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5_F (File IException)";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5_F (DataSet IException)";
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5_F (DataSpace IException)";
            return void();
        } catch( H5::DataTypeIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppNormalizeHdf5_F (DataType IException)";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception RcppNormalizeHdf5_F : "<< ex.what();
        } catch (...) {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception RcppNormalizeHdf5_F (unknown reason)";
            return void();
        }
        
        return void();
    }
    
    
    
    // inline void RcppTypifyNormalizeHdf5( std::string filename, std::string strgroup, std::string strdataset,
    //                                             bool bc, bool bs, bool bbyrows)
    inline void RcppTypifyNormalizeHdf5( BigDataStatMeth::hdf5Dataset* dsA,
                                                bool bc, bool bs, bool bbyrows)
    {
        
        BigDataStatMeth::hdf5Dataset* dsmean = nullptr;
        BigDataStatMeth::hdf5Dataset* dssd = nullptr;
        BigDataStatMeth::hdf5Dataset* dsNormal = nullptr;
        
        try{
            
            Rcpp::Nullable<int> wsize = R_NilValue;
            Eigen::MatrixXd datanormal;
            hsize_t nrows, ncols;
            std::string strgroupout;
            bool corrected = true;
            
            nrows = dsA->nrows();
            ncols = dsA->ncols();
            
            strgroupout = "NORMALIZED_T/" + dsA->getGroupName();
            std::string strgroupout_ms = strgroupout + "/mean_sd";
            std::string strdatasetmean = "mean." + dsA->getDatasetName();
            std::string strdatasetsd = "sd." + dsA->getDatasetName();
            
            // Define blocksize atending number of elements in rows and cols
            if( bbyrows == false) {
                datanormal = Eigen::MatrixXd::Zero(2,nrows);
                get_HDF5_mean_sd_by_column( dsA, datanormal, true, true, wsize);
            } else {
                datanormal = Eigen::MatrixXd::Zero(2,ncols);
                get_HDF5_mean_sd_by_row( dsA, datanormal, true, true, wsize);
            }
            
            dsmean = new BigDataStatMeth::hdf5Dataset(dsA->getFileName(), strgroupout_ms, strdatasetmean, true);
            dsmean->createDataset( datanormal.cols(), 1, "real");
            dsmean->writeDataset( Rcpp::wrap(datanormal.row(0)) );
            delete dsmean; dsmean = nullptr;
            
            dssd = new BigDataStatMeth::hdf5Dataset(dsA->getFileName(), strgroupout_ms, strdatasetsd, true);
            dssd->createDataset( datanormal.cols(), 1, "real");
            dssd->writeDataset( Rcpp::wrap(datanormal.row(1)) );
            delete dssd; dssd = nullptr;
            
            dsNormal = new BigDataStatMeth::hdf5Dataset(dsA->getFileName(), strgroupout, dsA->getDatasetName(), true);
            dsNormal->createDataset( dsA, "real");
            
            if( dsA->getDatasetptr() != nullptr && dsNormal->getDatasetptr() != nullptr){
                BigDataStatMeth::RcppNormalizeHdf5( dsA, dsNormal, datanormal, wsize, bc, bs, bbyrows, corrected);    
            }
            delete dsNormal; dsNormal = nullptr;
            
        } catch( H5::FileIException& error ) {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppTypifyNormalizeHdf5 (File IException)";
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppTypifyNormalizeHdf5 (DataSet IException)";
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppTypifyNormalizeHdf5 (DataSpace IException)";
            return void();
        } catch( H5::DataTypeIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nc++ exception RcppTypifyNormalizeHdf5 (DataType IException)";
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception RcppTypifyNormalizeHdf5 : "<< ex.what();
        } catch (...) {
            checkClose_file(dsA, dsmean, dssd, dsNormal);
            Rcpp::Rcerr<<"\nC++ exception RcppTypifyNormalizeHdf5 (unknown reason)";
            return void();
        }
        
        return void();
    }
    
    
    

}

#endif // BIGDATASTATMETH_HDF5_MATRIXSNORMALIZATION_HPP

