/**
 * @file matrixSdMean.hpp
 * @brief Mean and standard deviation calculations for HDF5 matrices
 * @details This header file provides implementations for computing mean and
 * standard deviation statistics for matrices stored in HDF5 format. The
 * implementation includes:
 * 
 * Key features:
 * - Row-wise statistics
 * - Column-wise statistics
 * - Block-based computation
 * - Memory-efficient algorithms
 * - Parallel processing support
 * 
 * Supported operations:
 * - Mean calculation
 * - Standard deviation calculation
 * - Corrected standard deviation
 * - Block-based processing
 * - Large matrix support
 * 
 * Performance features:
 * - Cache-friendly algorithms
 * - Dynamic block sizing
 * - Multi-threaded processing
 * - I/O optimization
 * - Memory management
 * 
 * The implementation uses:
 * - Efficient statistical algorithms
 * - Block-based computation
 * - HDF5 chunked storage
 * - Parallel I/O
 * - Vectorized operations
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXSDMEAN_HPP
#define BIGDATASTATMETH_HDF5_MATRIXSDMEAN_HPP


#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @brief Calculate optimal block size for processing
 * @details Determines the optimal block size for processing based on matrix
 * dimensions and memory constraints.
 * 
 * @param wsize User-specified block size (optional)
 * @param reference_size Primary dimension size
 * @param alternative_size Secondary dimension size
 * @return Optimal block size for processing
 */
inline hsize_t get_block_size( Rcpp::Nullable<int> wsize, hsize_t reference_size, hsize_t alternative_size) {
    
    hsize_t bsize = 0;
    
    if( wsize.isNull()) {
        if( reference_size > MAXELEMSINBLOCK ){
            bsize = 1;
        } else {
            hsize_t maxsize = std::max( alternative_size, reference_size);
            bsize = std::ceil( MAXELEMSINBLOCK / maxsize);
        }
    } else {
        if(reference_size > MAXELEMSINBLOCK){
            bsize = 1;
        } else {
            bsize = Rcpp::as<int> (wsize);
        }
    }
    
    return(bsize);
    
}

/**
 * @brief Calculate row-wise mean and standard deviation
 * @details Computes mean and standard deviation for each row of the matrix
 * using block-based processing for memory efficiency.
 * 
 * @param dsA Input matrix dataset
 * @param normalize Output matrix for mean and std values
 * @param wsize Block size for processing
 */
inline void get_HDF5_mean_sd_by_row( BigDataStatMeth::hdf5Dataset* dsA, 
                                     Eigen::MatrixXd& normalize, 
                                     bool bsd, bool bmean, 
                                     Rcpp::Nullable<int> wsize )
{
    
    try
    {
        
        hsize_t block_size = 0;
        hsize_t* dims_out = dsA->dim();

        std::vector<hsize_t> stride = {1, 1},
                             block = {1, 1},
                             offset = {0, 0},
                             count = {0, 0};
        
        block_size = get_block_size(wsize, dims_out[0], dims_out[1]);

        count[0] = dims_out[0];
        if( block_size < dims_out[1] ) {
            count[1] = block_size;
        } else{
            count[1] = dims_out[1];
        }

        // Read data in blocks of 500 columns
        for( hsize_t i=0; (i <= floor(dims_out[1]/block_size)) || i==0 ; i++)
        {
            
            // if( i>0 ) {
                

            if( offset[1] + block_size <= dims_out[1] ) {
                count[1] = block_size;
            } else {
                count[1] = dims_out[1] - offset[1];
            }
            // }

            std::vector<double> vdA( count[0] * count[1] ); 
            dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdA.data(), count[0], count[1] );

            Eigen::RowVectorXd mean = X.colwise().mean();
            normalize.block( 0, offset[1], 1, mean.size()) = mean;
            
            if(bsd){
                Eigen::RowVectorXd sd = ((X.rowwise() - mean).array().square().colwise().sum() / (X.rows() - 1)).sqrt();
                normalize.block( 1, offset[1], 1, sd.size()) = sd;
            }
            
            
            
            offset[1] = offset[1] + block_size;

        }
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        // error.printErrorStack();
        checkClose_file(dsA);
        Rf_error("c++ exception get_HDF5_mean_sd_by_row (File IException)");
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        // error.printErrorStack();
        checkClose_file(dsA);
        Rf_error("c++ exception get_HDF5_mean_sd_by_row (DataSet IException)");
    } catch(std::exception& error) {
        checkClose_file(dsA);
        Rf_error("c++ exception get_HDF5_mean_sd_by_row function: %s",error.what());
    }
    
    return void(); 
    
}

/**
 * @brief Calculate column-wise mean and standard deviation
 * @details Computes mean and standard deviation for each column of the matrix
 * using block-based processing for memory efficiency. Optimized for cases
 * where n << m (rows much fewer than columns).
 * 
 * @param dsA Input matrix dataset
 * @param normalize Output matrix for mean and std values
 * @param bsd compute sd
 * @param bmean compute mean
 * @param wsize Block size for processing
 */
inline void get_HDF5_mean_sd_by_column( BigDataStatMeth::hdf5Dataset* dsA,
                                        Eigen::MatrixXd& normalize, 
                                        bool bsd, bool bmean, 
                                        Rcpp::Nullable<int> wsize )
{
    
    // IntegerVector dims_out = get_HDF5_dataset_size(*dataset);
    
    try
    {

        hsize_t block_size = 0;
        hsize_t* dims_out = dsA->dim();
        
        std::vector<hsize_t> stride = {1, 1},
                             block = {1, 1},
                             offset = {0, 0},
                             count = {0, 0};
        
        
        block_size = get_block_size(wsize, dims_out[1], dims_out[0]);

        count[1] = dims_out[1];
        if( block_size < dims_out[0] )
            count[0] = block_size;
        else
            count[0] = dims_out[0];
        
        // Read data in blocks of 500 columns
        for(hsize_t i=0; (i <= floor(dims_out[0]/block_size)) || i==0; i++)
        {

            if( offset[0] + block_size <= dims_out[0] ) {
                count[0] = block_size;
            }else {
                count[0] = dims_out[0] - offset[0];
            }
            
            std::vector<double> vdA( count[0] * count[1] ); 
            dsA->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data() );
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> X (vdA.data(), count[0], count[1] );

            Eigen::VectorXd mean = X.rowwise().mean();
            normalize.block( 0, offset[0], 1, mean.size()) = mean.transpose();
            
            if(bsd) {
                Eigen::VectorXd sd = ((X.colwise() - mean).array().square().rowwise().sum() / (X.cols() - 1)).sqrt();
                normalize.block( 1, offset[0], 1, sd.size()) = sd.transpose();
            }
            
            offset[0] = offset[0] + block_size;

        }
        
    } catch( H5::FileIException& error ) { // catch failure caused by the H5File operations
        // error.printErrorStack();
        checkClose_file(dsA);
        Rf_error("c++ exception get_HDF5_mean_sd_by_column (File IException)");
    } catch( H5::DataSetIException& error ) { // catch failure caused by the DataSet operations
        // error.printErrorStack();
        checkClose_file(dsA);
        Rf_error("c++ exception get_HDF5_mean_sd_by_column (DataSet IException)");
    } catch(std::exception& error) {
        checkClose_file(dsA);
        Rf_error("c++ exception get_HDF5_mean_sd_by_column function: %s",error.what());
        // return void();
    }
    
    return void();  // successfully terminated
    
}


}

#endif // BIGDATASTATMETH_HDF5_MATRIXSDMEAN_HPP

