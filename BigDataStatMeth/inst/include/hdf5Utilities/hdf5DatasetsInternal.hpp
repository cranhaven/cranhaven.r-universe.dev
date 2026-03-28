/**
 * @file hdf5DatasetsInternal.hpp
 * @brief Internal HDF5 dataset management class
 * @details This header file provides a specialized class for managing internal HDF5
 * datasets. These are datasets used for internal storage and processing within the
 * package. The implementation includes:
 * 
 * Key features:
 * - Internal dataset creation and management
 * - Specialized data handling for internal operations
 * - Memory-efficient data access
 * - Automatic resource management
 * - Support for various data types
 */

#ifndef BIGDATASTATMETH_HDF5_DATASETINTERNAL_HPP
#define BIGDATASTATMETH_HDF5_DATASETINTERNAL_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @class hdf5DatasetInternal
 * @brief Class for managing internal HDF5 datasets
 * @details Provides specialized functionality for creating and managing HDF5 datasets
 * used for internal operations. Inherits from hdf5Dataset to extend basic dataset
 * functionality with internal-specific features.
 * 
 * Key capabilities:
 * - Internal dataset creation
 * - Specialized data access patterns
 * - Memory-efficient operations
 * - Resource management
 */
class hdf5DatasetInternal : public hdf5Dataset 
{
    
public:

    using hdf5Dataset::createDataset; 

    /**
     * @brief Constructor with file, group, and dataset name
     * @param file HDF5 file pointer
     * @param group Group path
     * @param datasetname Dataset name
     * @param overwrite Whether to overwrite existing dataset
     */
    hdf5DatasetInternal(H5::H5File* file, std::string group, std::string datasetname, bool overwrite) : 
        hdf5Dataset(file, group, datasetname, overwrite)
    { 
        internalDataset = true;
    }
    
    /**
     * @brief Constructor with file and dataset path
     * @param file HDF5 file pointer
     * @param dataset Full dataset path
     * @param overwrite Whether to overwrite existing dataset
     */
    hdf5DatasetInternal(H5::H5File* file, std::string dataset, bool overwrite) : 
        hdf5Dataset(file, dataset, overwrite)
    { 
        internalDataset = true;
    }
    
    /**
     * @brief Constructor with file object, group, and dataset name
     * @param oFile HDF5 file object
     * @param group Group path
     * @param datasetname Dataset name
     * @param overwrite Whether to overwrite existing dataset
     */
    hdf5DatasetInternal(BigDataStatMeth::hdf5File* oFile, std::string group, std::string datasetname, bool overwrite) : 
        hdf5Dataset(oFile, group, datasetname, overwrite)
    { 
        internalDataset = true;
    }
    
    /**
     * @brief Constructor with filename and dataset path
     * @param filename Name of HDF5 file
     * @param dataset Full dataset path
     * @param overwrite Whether to overwrite existing dataset
     */
    hdf5DatasetInternal(std::string filename, std::string dataset, bool overwrite) : 
    hdf5Dataset(filename, dataset, overwrite)
    { 
        internalDataset = true;
    }
    
    /**
     * @brief Constructor with filename, group, and dataset name
     * @param filename Name of HDF5 file
     * @param group Group path
     * @param datasetname Dataset name
     * @param overwrite Whether to overwrite existing dataset
     */
    hdf5DatasetInternal(std::string filename, std::string group, std::string datasetname, bool overwrite) : 
    hdf5Dataset(filename, group, datasetname, overwrite)
    { 
        internalDataset = true;
    }
    
 
    
    /**
     * @brief Create a new internal dataset with optional compression
     * @details Creates a new HDF5 internal dataset with specified dimensions, data type, and compression.
     * Internal datasets are marked with internal="1" attribute for specialized handling.
     * 
     * @param rows Number of rows
     * @param cols Number of columns
     * @param strdatatype Data type ("int", "numeric", "real", or "string")
     * @param compression_level Compression level (0=none, 1-9=gzip level, 6=balanced default)
     */
    void createDataset(size_t rows, size_t cols, std::string strdatatype, int compression_level = 6) 
    {
        try
        {
            H5::Exception::dontPrint();
            std::string fullDatasetPath = groupname + "/" + name;
            bool bRemoved = false;
            
            // dataset dimensions
            dimDataset[0] = rows;
            dimDataset[1] = cols;
            
            dimDatasetinFile[0] = rows;
            dimDatasetinFile[1] = cols;
            
            if( !exists_HDF5_element(pfile, groupname) ) {
                create_HDF5_groups(groupname);
            }
            
            H5::DataSpace dataspace( RANK2, dimDataset );
            
            bool bexists = exists_HDF5_element(pfile, fullDatasetPath);
            if( bexists == true && boverwrite == false) {
                Rcpp::Rcerr<<"\nDataset exits, please set overwrite = true to overwrite the existing dataset (DataSet IException)";
                return void();
            } else {
                
                if( boverwrite == true && bexists == true) {
                    remove_elements(pfile, getGroupName(), {name}); 
                    bRemoved = true;
                }
                
                type = strdatatype;
                
                // Configure compression and chunking
                hid_t cparms = H5P_DEFAULT;
                if (compression_level > 0) {
                    cparms = H5Pcreate(H5P_DATASET_CREATE);
                    
                    // Configure chunking (required for compression)
                    hsize_t chunk_dims[2] = {
                        std::min((hsize_t)1024, dimDataset[0]),
                        std::min((hsize_t)1024, dimDataset[1])
                    };
                    H5Pset_chunk(cparms, RANK2, chunk_dims);
                    
                    // Configure compression
                    H5Pset_deflate(cparms, compression_level);  // gzip compression
                    H5Pset_shuffle(cparms);                     // shuffle filter for better compression
                }
                
                if( type == "string") {
                    // Create the memory datatype.
                    H5::CompType strtype(sizeof(names));
                    strtype.insertMember("chr", HOFFSET(names, chr), H5::StrType(H5::PredType::C_S1, MAXSTRING ));
                    pdataset = new H5::DataSet(pfile->createDataSet(fullDatasetPath, strtype, dataspace, cparms));
                } else if( type == "int" || type == "logic" || type == "factor") {
                    H5::IntType datatype( H5::PredType::NATIVE_INT );
                    pdataset = new H5::DataSet(pfile->createDataSet( fullDatasetPath, datatype, dataspace, cparms ));
                    if(bRemoved == true) {
                        writeDataset(Rcpp::wrap(Eigen::MatrixXd::Zero(dimDataset[0], dimDataset[1]) ));    
                    }
                } else if( type == "numeric" || type == "real") {
                    H5::FloatType datatype( H5::PredType::NATIVE_DOUBLE ); 
                    pdataset = new H5::DataSet(pfile->createDataSet( fullDatasetPath, datatype, dataspace, cparms ));
                    if(bRemoved == true) {
                        writeDataset(Rcpp::wrap(Eigen::MatrixXd::Zero(dimDataset[0], dimDataset[1]) ));
                    }
                } else {
                    if (cparms != H5P_DEFAULT) H5Pclose(cparms);
                    Rcpp::Rcerr<<"\nDataset data type not allowed or no matrix defined (createDataset)";
                    return void();
                }
                
                // Clean up compression properties
                if (cparms != H5P_DEFAULT) H5Pclose(cparms);
            }
            
            dataspace.close();
            addAttribute( "internal", Rcpp::wrap("1") );
            
        } catch(H5::FileIException& error) {
            Rcpp::Rcerr<<"\nc++ exception createDataset_int (File IException)";
        } catch(H5::GroupIException& error) {
            Rcpp::Rcerr<<"\nc++ exception createDataset_int (Group IException)";
        } catch(H5::DataSetIException& error) {
            Rcpp::Rcerr<<"\nc++ exception createDataset_int (DataSet IException)";
        } 
        return void();
    }
    
    // /**
    //  * @brief Create a dataset based on another internal dataset
    //  * @details Creates a new internal dataset with the same dimensions as the
    //  * reference dataset but with a specified data type.
    //  * 
    //  * @param dsLike Reference internal dataset to copy dimensions from
    //  * @param strdatatype Data type for the new dataset
    //  */
    // void createDataset(BigDataStatMeth::hdf5DatasetInternal* dsLike, std::string strdatatype) 
    // {
    //     try{
    //         
    //         createDataset( dsLike->nrows(), dsLike->ncols(), strdatatype);
    //         
    //     } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
    //         Rcpp::Rcerr<<"\nc++ exception createDataset (File IException)";
    //     } catch(H5::GroupIException& error) { // catch failure caused by the H5File operations
    //         Rcpp::Rcerr<<"\nc++ exception createDataset (Group IException)";
    //     } catch(H5::DataSetIException& error) { // catch failure caused by the H5File operations
    //         Rcpp::Rcerr<<"\nc++ exception createDataset (DataSet IException)";
    //     } 
    //     
    //     return void();
    // }
    
    /**
     * @brief Create an internal dataset based on another internal dataset's dimensions with compression
     * @details Creates a new internal dataset with the same dimensions as the reference dataset
     * but with a specified data type and compression settings.
     * 
     * @param dsLike Reference internal dataset to copy dimensions from
     * @param strdatatype Data type for the new dataset
     * @param compression_level Compression level (0=none, 1-9=gzip level, 6=balanced default)
     */
    void createDataset(BigDataStatMeth::hdf5DatasetInternal* dsLike, std::string strdatatype, int compression_level = 6) 
    {
        try{
            createDataset( dsLike->nrows(), dsLike->ncols(), strdatatype, compression_level);
        } catch(H5::FileIException& error) {
            Rcpp::Rcerr<<"\nc++ exception createDataset (File IException)";
        } catch(H5::GroupIException& error) {
            Rcpp::Rcerr<<"\nc++ exception createDataset (Group IException)";
        } catch(H5::DataSetIException& error) {
            Rcpp::Rcerr<<"\nc++ exception createDataset (DataSet IException)";
        } 
        
        return void();
    }
    

    /**
     * @brief Create an unlimited internal dataset with optional compression
     * @details Creates a new internal HDF5 dataset with unlimited dimensions.
     * The dataset is marked as internal and unlimited and can grow in size.
     * 
     * @param rows Initial number of rows
     * @param cols Initial number of columns
     * @param strdatatype Data type for the dataset
     * @param compression_level Compression level (0=none, 1-9=gzip level, 6=balanced default)
     */
    void createUnlimitedDataset(size_t rows, size_t cols, std::string strdatatype, int compression_level = 6) 
    {
        try
        {
            H5::Exception::dontPrint();
            
            herr_t status;
            hid_t cparms; 
            std::string fullDatasetPath = groupname + "/" + name;
            
            // dataset dimensions
            dimDataset[0] = rows;
            dimDataset[1] = cols;
            
            // dataset dimensions in file
            dimDatasetinFile[0] = rows;
            dimDatasetinFile[1] = cols;
            
            // set dataset as unlimited;
            unlimited = true;
            
            // Declare unlimited dimensions
            hsize_t  maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
            H5::DataSpace dataspace ( RANK2, dimDataset, maxdims );
            
            // Enabling chunking
            hsize_t chunk_dims[2];
            chunk_dims[0] = rows;
            chunk_dims[1] = cols;
            
            cparms = H5Pcreate(H5P_DATASET_CREATE);
            status = H5Pset_chunk( cparms, RANK2, chunk_dims);
            
            if(status<0) {
                Rf_error( "c++ exception createUnlimitedDataset (setting chunk IException)" );
                return void();
            }
            
            // Configure compression
            if (compression_level > 0) {
                H5Pset_deflate(cparms, compression_level);  // gzip compression
                H5Pset_shuffle(cparms);                     // shuffle filter for better compression
            }
            
            if( !exists_HDF5_element(pfile, groupname) ) {
                create_HDF5_groups(groupname);
            }
            
            bool bexists = exists_HDF5_element(pfile, fullDatasetPath);
            if( bexists == true && boverwrite == false) {
                Rcpp::Rcout<<"\n Dataset exits, please set overwrite = true to overwrite data \n";
            } else {
                
                if( bexists == true && boverwrite == true) {
                    remove_elements(pfile, fullDatasetPath); 
                }
                
                // Create dataset
                if( strdatatype == "int") {
                    H5::IntType datatype( H5::PredType::NATIVE_INT );
                    pdataset = new H5::DataSet(pfile->createDataSet( fullDatasetPath, datatype, dataspace, cparms));
                } else {
                    H5::FloatType datatype( H5::PredType::NATIVE_DOUBLE ); 
                    pdataset = new H5::DataSet(pfile->createDataSet( fullDatasetPath, datatype, dataspace, cparms));
                }
            }
            
            dataspace.close();
            addAttribute( "internal", Rcpp::wrap("1") );
            
        } catch(H5::FileIException& error) {
            Rf_error( "c++ exception createUnlimitedDataset_internal (File IException) " );
        } catch(H5::GroupIException& error) {
            Rf_error( "c++ exception createUnlimitedDataset_internal (Group IException) " );
        } catch(H5::DataSetIException& error) {
            Rf_error( "c++ exception createUnlimitedDataset_internal (Dataset IException) " );
        } 
        return void();
    }
    
    
    /**
     * @brief Extend an unlimited internal dataset
     * @details Increases the dimensions of an unlimited internal dataset.
     * Only works with datasets created as unlimited.
     * 
     * @param rows New number of rows
     * @param cols New number of columns
     * @throws H5::DataSetIException if dataset is not unlimited
     */
    void extendUnlimitedDataset(const size_t rows, const size_t cols)
    {
        try
        {
            if(unlimited == true) {
#ifdef _OPENMP
                HDF5ThreadSafety::LockGuard lock_guard;
#endif
                
                H5::Exception::dontPrint();
                
                // Extend dataset size to:  oldDims + newDims
                hsize_t newdims[2];
                newdims[0] = rows;
                newdims[1] = cols;
                
                // hsize_t size[2];
                dimDataset[0] = dimDataset[0] + newdims[0];
                dimDataset[1] = dimDataset[1] + newdims[1];

                dimDatasetinFile[0] = dimDatasetinFile[0] + newdims[0];
                dimDatasetinFile[1] = dimDatasetinFile[1] + newdims[1];
                
                pdataset->extend( dimDataset );    
            } else {
                Rcpp::Rcout<<"\n Dataset is not an unlimited dataset, fixed datasets can't be extended\n";
                return void();
            }
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            Rf_error( "c++ exception extend_HDF5_matrix_subset_ptr (File IException)" );
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            Rf_error( "c++ exception extend_HDF5_matrix_subset_ptr (DataSet IException)" );
        }
        return void();
    }
    
    /**
     * @brief Write data to the internal dataset
     * @details Writes data to the internal dataset, handling different data types
     * and formats. Optimized for internal storage patterns.
     * 
     * Supports:
     * - Numeric matrices
     * - Integer matrices
     * - String data
     * - Vector data
     * 
     * @param DatasetValues R object containing the data to write
     * @throws H5::DataSetIException if write operation fails
     */
    void writeDataset( Rcpp::RObject DatasetValues )
    {
        
        try
        {
            
            // H5::Exception::dontPrint();
            std::vector<int> dims;
            
            if(Rcpp::is<Rcpp::NumericMatrix>(DatasetValues) || Rcpp::is<Rcpp::IntegerMatrix>(DatasetValues) ) {
                hsize_t ncolRObject = Rcpp::as<Rcpp::NumericMatrix>(DatasetValues).ncol();
                hsize_t nrowRObject = Rcpp::as<Rcpp::NumericMatrix>(DatasetValues).nrow();
                
                if( (nrowRObject == dimDataset[0] && ncolRObject == dimDataset[1]) || ncolRObject == 1 || nrowRObject == 1 ) {
                    
                    // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
                    HDF5ThreadSafety::LockGuard lock_guard;
#endif
                    
                    H5::DataSpace dataspace(RANK2, dimDataset);
                    
                    std::vector<double> matHiCValues = Rcpp::as<std::vector<double> >(Rcpp::as<Rcpp::NumericMatrix>(DatasetValues));
                    pdataset->write( &matHiCValues[0] , H5::PredType::NATIVE_DOUBLE);
                    dataspace.close();
                } else {
                    Rcpp::Rcout<<"\n Data you are trying to write (a complete dataset - Internal) differs from existing hdf5 dataset size\n";
                    return void();
                }
                
            } else if(Rcpp::is<Rcpp::NumericVector>(DatasetValues) || Rcpp::is<Rcpp::IntegerVector>(DatasetValues)) {
                
                hsize_t dims[] = {dimDataset[1]};
                
                // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
                HDF5ThreadSafety::LockGuard lock_guard;
#endif
                
                H5::DataSpace dataspace(RANK1, dims);
                
                if(Rcpp::is<Rcpp::IntegerVector>(DatasetValues) || Rcpp::is<Rcpp::LogicalVector>(DatasetValues) ) {
                    std::vector<int> vectHiCValues = Rcpp::as<std::vector<int> >(DatasetValues);
                    pdataset->write( vectHiCValues.data() , H5::PredType::NATIVE_INT);
                } else if(Rcpp::is<Rcpp::NumericVector>(DatasetValues) )  {
                    std::vector<double> vectHiCValues = Rcpp::as<std::vector<double> >(DatasetValues);
                    pdataset->write( vectHiCValues.data() , H5::PredType::NATIVE_DOUBLE);
                } 
                dataspace.close();
            } else if(Rcpp::is<Rcpp::StringVector >(DatasetValues) || Rcpp::is<Rcpp::StringMatrix >(DatasetValues)) {
                
                // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
                HDF5ThreadSafety::LockGuard lock_guard;
#endif
                
                // Create the memory datatype.
                H5::CompType strtype(sizeof(names));
                strtype.insertMember("chr", HOFFSET(names, chr), H5::StrType(H5::PredType::C_S1, MAXSTRING ));
                
                pdataset->write(convert_DataFrame_to_RangeList(DatasetValues, true), strtype);
                
            } else {
                Rf_error( "Matrix data type not allowed (writeDataset)" );
            }
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            close_dataset_file();
            Rcpp::Rcerr<<"\nc++ exception writeDataset_Internal (File IException)";
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            close_dataset_file();
            Rcpp::Rcerr<<"\nc++ exception writeDataset_Internal (DataSet IException)";
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            close_dataset_file();
            Rcpp::Rcerr<<"\nc++ exception writeDataset_Internal (Group IException)";
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            close_dataset_file();
            Rcpp::Rcerr<<"\nc++ exception writeDataset_Internal (DataSpace IException)";
        } catch(H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
            close_dataset_file();
            Rcpp::Rcerr<<"\nc++ exception writeDataset_Internal (Data TypeIException)";
        }
        return void();
    }
    
    /**
     * @brief Write block of data to internal dataset
     * @details Writes a block of data to the internal dataset with configurable
     * layout and optional transposition.
     * 
     * Features:
     * - Row/column-major formats
     * - Data transposition
     * - Block-based access
     * - Memory-efficient operations
     * 
     * @param DatasetValues Data to write
     * @param vOffset Starting position
     * @param vCount Number of elements
     * @param vStride Stride between elements
     * @param vBlock Block size
     * @param bTranspose Whether to transpose data
     */
    void writeDatasetBlock( Rcpp::RObject DatasetValues, std::vector<hsize_t> vOffset, 
                            std::vector<hsize_t> vCount, std::vector<hsize_t> vStride,
                            std::vector<hsize_t> vBlock, bool bTranspose)
    {
        try
        {
            // Turn off the auto-printing when failure occurs so that we can handle the errors appropriately
            H5::Exception::dontPrint();

            hsize_t hsOffset[2], hsCount[2], hsStride[2], hsBlock[2];

            // Specify size and shape of subset to write
            hsOffset[0] = vOffset[0]; hsOffset[1] = vOffset[1];
            hsStride[0] = vStride[0]; hsStride[1] = vStride[1]; // default 1
            hsBlock[0] = vBlock[0]; hsBlock[1] = vBlock[1]; // default 1
            
            if(Rcpp::is<Rcpp::NumericMatrix>(DatasetValues) || Rcpp::is<Rcpp::IntegerMatrix>(DatasetValues) ||
               Rcpp::is<Rcpp::NumericVector>(DatasetValues) || Rcpp::is<Rcpp::IntegerVector>(DatasetValues) ) {
                
                if(Rcpp::is<Rcpp::NumericMatrix>(DatasetValues) || Rcpp::is<Rcpp::IntegerMatrix>(DatasetValues)) {
                    if(bTranspose == false) {
                        hsCount[0] = vCount[0];
                        hsCount[1] = vCount[1];
                    } else {
                        hsCount[0] = vCount[1];
                        hsCount[1] = vCount[0];
                    }
                    
                } else if(Rcpp::is<Rcpp::NumericVector>(DatasetValues) || Rcpp::is<Rcpp::IntegerVector>(DatasetValues)) {
                        hsCount[0] = 1;
                        hsCount[1] = vCount[0];
                }

                if(vOffset[0] + hsCount[0] <= dimDataset[0] || vOffset[1] + hsCount[1] <= dimDataset[1]) {
                    
                    // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
                    HDF5ThreadSafety::LockGuard lock_guard;
#endif
                    
                    H5::DataSpace dataspace(RANK2, hsCount);
                    H5::DataSpace memspace(RANK2, hsCount, NULL);
                    
                    dataspace = pdataset->getSpace();
                    dataspace.selectHyperslab( H5S_SELECT_SET, hsCount, hsOffset, hsStride, hsBlock);
                    
                    if(Rcpp::is<Rcpp::NumericMatrix>(DatasetValues) || Rcpp::is<Rcpp::IntegerMatrix>(DatasetValues)) {
                        std::vector<double> matdata(hsCount[0]*hsCount[1]);
                        
                        if (bTranspose == false) {
                            matdata = Rcpp::as<std::vector<double> >(transpose(Rcpp::as<Rcpp::NumericMatrix>(DatasetValues)));
                        } else {
                            matdata = Rcpp::as<std::vector<double> >(Rcpp::as<Rcpp::NumericMatrix>(DatasetValues));
                        }
                        
                        pdataset->write(&matdata[0], H5::PredType::NATIVE_DOUBLE, memspace, dataspace);
                        memspace.close();
                        dataspace.close();
                    } else if(Rcpp::is<Rcpp::NumericVector>(DatasetValues) || Rcpp::is<Rcpp::IntegerVector>(DatasetValues)) {
                        std::vector<double> matdata = Rcpp::as<std::vector<double> >(Rcpp::as<Rcpp::NumericVector>(DatasetValues));
                        pdataset->write(&matdata[0], H5::PredType::NATIVE_DOUBLE, memspace, dataspace);
                        memspace.close();
                        dataspace.close();
                    }
                } else {
                    Rcpp::stop("\nIt is not possible to write block in current position (writeDatasetBlock)");
                }

            } else if(Rcpp::is<Rcpp::StringMatrix>(DatasetValues) || Rcpp::is<Rcpp::StringVector>(DatasetValues) ) {

                if(Rcpp::is<Rcpp::StringMatrix>(DatasetValues)) {
                    hsCount[0] = Rcpp::as<Rcpp::StringMatrix>(DatasetValues).rows();
                    hsCount[1] = Rcpp::as<Rcpp::StringMatrix>(DatasetValues).cols();
                } else if(Rcpp::is<Rcpp::StringVector>(DatasetValues)) {
                    hsCount[0] = 1;
                    hsCount[1] = Rcpp::as<Rcpp::StringVector>(DatasetValues).length();
                }

                if(vOffset[0] + hsCount[0] <= dimDataset[0] || vOffset[1] + hsCount[1] <= dimDataset[1]) {

                    
                    // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
                    HDF5ThreadSafety::LockGuard lock_guard;
#endif
                    
                    // Create the memory datatype.
                    H5::CompType strtype(sizeof(names));
                    strtype.insertMember("chr", HOFFSET(names, chr), H5::StrType(H5::PredType::C_S1, MAXSTRING ));

                    H5::DataSpace dataspace(RANK2, hsCount);
                    H5::DataSpace memspace(RANK2, hsCount, NULL);

                    dataspace = pdataset->getSpace();
                    dataspace.selectHyperslab( H5S_SELECT_SET, hsCount, hsOffset, hsStride, hsBlock);

                    pdataset->write(convert_DataFrame_to_RangeList(DatasetValues, false), strtype, memspace, dataspace);
                    memspace.close();
                    dataspace.close();

                } else {
                    Rcpp::stop("\nIt is not possible to write block in current position (writeDatasetBlock_internal)");
                }
            } else {
                Rcpp::stop("\nMatrix data type not allowed (writeDatasetBlock_internal)");
            }

        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_Internal (File IException)");
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_Internal (DataSet IException)");
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_Internal (Group IException)");
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_Internal (DataSpace IException)");
        } catch(H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_Internal (Data TypeIException)");
        }
        return void();
    }

    /**
     * @brief Write block of vector data to internal dataset
     * @details Writes a block of vector data to the internal dataset.
     * Optimized for vector operations.
     * 
     * @param DatasetValues Vector data to write
     * @param vOffset Starting position
     * @param vCount Number of elements
     * @param vStride Stride between elements
     * @param vBlock Block size
     */
    void writeDatasetBlock( std::vector<double> DatasetValues, std::vector<hsize_t> vOffset, 
                                    std::vector<hsize_t> vCount, std::vector<hsize_t> vStride,
                                    std::vector<hsize_t> vBlock)
    {
        try
        {
            // Turn off the auto-printing when failure occurs so that we can handle the errors appropriately
            H5::Exception::dontPrint();
            
            hsize_t hsOffset[2], hsCount[2], hsStride[2], hsBlock[2];
            
            // Specify size and shape of subset to write
            hsStride[0] = vStride[0]; hsStride[1] = vStride[1]; // default 1
            hsBlock[0] = vBlock[0]; hsBlock[1] = vBlock[1]; // default 1
            
            hsOffset[0] = vOffset[0]; hsOffset[1] = vOffset[1];    
            hsCount[0] = vCount[0]; hsCount[1] = vCount[1];
            
            
            if(vOffset[0] + hsCount[0] <= dimDataset[0] && vOffset[1] + hsCount[1] <= dimDataset[1] && 
               DatasetValues.size()<= dimDatasetinFile[0] * dimDatasetinFile[1]) {
                
                // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
                HDF5ThreadSafety::LockGuard lock_guard;
#endif
                
                H5::DataSpace dataspace(RANK2, hsCount);
                H5::DataSpace memspace(RANK2, hsCount, NULL);
                
                dataspace = pdataset->getSpace();
                dataspace.selectHyperslab( H5S_SELECT_SET, hsCount, hsOffset, hsStride, hsBlock);
                
                pdataset->write(&DatasetValues[0], H5::PredType::NATIVE_DOUBLE, memspace, dataspace);
                memspace.close();
                dataspace.close();
                
            } else {
                
                Rcpp::stop("\nIt is not possible to write block in current position (writeDatasetBlock_int)");
            }
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_internal std::vector (File IException)");
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_internal std::vector (DataSet IException)");
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_internal std::vector (Group IException)");
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_internal std::vector (DataSpace IException)");
        } catch(H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
            close_dataset();
            close_dataset_file();
            Rcpp::stop("\nc++ exception writeDatasetBlock_internal std::vector (Data TypeIException)");
        }
        return void();
    }

    /**
     * @brief Read a block of data from internal dataset
     * @details Reads a specified block of data from the internal dataset.
     * Optimized for internal data access patterns.
     * 
     * @param ivoffset Starting position for read
     * @param ivcount Number of elements to read
     * @param ivstride Stride between elements
     * @param ivblock Size of blocks
     * @param rdatablock Pointer to read data (double*) 
     * @return Pointer to read data (double*)
     */
    double* readDatasetBlock(std::vector<hsize_t> ivoffset, std::vector<hsize_t> ivcount,
                             std::vector<hsize_t> ivstride, std::vector<hsize_t> ivblock,
                             double* rdatablock)
    {
        
        try
        {
            // H5::Exception::dontPrint();
            
            hsize_t offset[2], count[2], stride[2], block[2];
            
            offset[0] = ivoffset[0]; offset[1] = ivoffset[1];
            count[0] = ivcount[0]; count[1] = ivcount[1];
            stride[0] = ivstride[0]; stride[1] = ivstride[1];
            block[0] = ivblock[0]; block[1] = ivblock[1];
            
            // Define Memory Dataspace. Get file dataspace and select a subset from the file dataspace.
            hsize_t dimsm[2];
            dimsm[0] = count[0]; 
            dimsm[1] = count[1];
            // Thread-safe HDF5 I/O operations
#ifdef _OPENMP
            HDF5ThreadSafety::LockGuard lock_guard;
#endif
            
            H5::DataSpace memspace(RANK2, dimsm, NULL);
            
            //  Get dataspace of the dataset.
            H5::DataSpace dataspace = pdataset->getSpace();
            dataspace.selectHyperslab(H5S_SELECT_SET, count, offset, stride, block); 
            
            H5T_class_t type_class = pdataset->getTypeClass();
            
            // Get class of datatype and print message if it's an integer.
            if( type_class == H5T_INTEGER || type_class == H5T_FLOAT ) {
                pdataset->read( rdatablock, H5::PredType::NATIVE_DOUBLE, memspace, dataspace );
            } else {
                Rf_error( "c++ exception readDatasetBlock (Data type not allowed, maybe are trying to read string matrix?)" );
                return(nullptr);
            }// else if (type_class == H5T_FLOAT) {
            //     pdataset->read( rdatablock, H5::PredType::NATIVE_DOUBLE, memspace, dataspace );
            // } 

            memspace.close();
            dataspace.close();
            
        } catch( H5::FileIException& error) { 
            close_dataset_file();
            Rf_error("\nc++ exception readDatasetBlock_Internal (File IException)");
            return(nullptr);
            // return void();
        } catch(H5::DataSetIException& error) { 
            close_dataset_file();
            Rf_error("\nc++ exception readDatasetBlock_Internal (DataSet IException)");
            return(nullptr);
            // return void();
        } catch(H5::GroupIException& error) { 
            close_dataset_file();
            Rf_error("\nc++ exception readDatasetBlock_Internal (Group IException)");
            return(nullptr);
            // return void();
        } catch(H5::DataSpaceIException& error) { 
            close_dataset_file();
            Rf_error("\nc++ exception readDatasetBlock_Internal (DataSpace IException)");
            return(nullptr);
            // return void();
        } catch(H5::DataTypeIException& error) { 
            close_dataset_file();
            Rf_error("\nc++ exception readDatasetBlock_Internal (Data TypeIException)");
            return(nullptr);
            // return void();
        }
        return(rdatablock);
    }
    
    
    
    H5::DataSet* getDatasetptr() { return(pdataset); }  // Return dataset pointer
    /**
     * @brief Get dataset name
     * @return Name of the internal dataset
     */
    std::string getDatasetName() { return(name); }
    /**
     * @brief Get group name
     * @return Name of the group containing the internal dataset
     */
    std::string getGroup() { return(getGroupName()); }
    /**
     * @brief Get file name
     * @return Name of the file containing the internal dataset
     */
    std::string getFileName() { return(getFilename()); }
    hsize_t nrows() { return(dimDataset[0]); }  // Return number of rows
    hsize_t ncols() { return(dimDataset[1]); }  // Return number of columns
    hsize_t* dim() { return(dimDataset); }  // Return dataset dimension (rows x columns)
    bool isUnlimited() { return(unlimited); }  // Return if dataset is an unlimited dataset
    
    /**
     * @brief Destructor
     * @details Closes the internal dataset and releases resources
     */
    ~hdf5DatasetInternal(){
        
    }
    
    
private:
    

    // ------------------------
    //   Variables declaration
    // ------------------------
    // bool internalDataset;
    
    
    // ------------------------
    //   Function declarations
    // ------------------------
    
    /**
     * @brief Close the dataset
     * @details Closes the internal dataset and releases associated resources
     */
    void close_dataset() 
    {
        pdataset->close();
    }
    
    /**
     * @brief Close dataset and file
     * @details Closes both the internal dataset and its associated file
     */
    void close_dataset_file()
    {
        pdataset->close();
        pfile->close();
    }

    
    /**
     * @brief Convert DataFrame to range list
     * @details Converts R DataFrame to HDF5-compatible range list format
     * for internal storage.
     * 
     * @param DatasetValues R DataFrame to convert
     * @param bFullDataset Whether to convert entire dataset
     * @return Array of name structures
     */
    names* convert_DataFrame_to_RangeList(Rcpp::RObject DatasetValues, bool bFullDataset) 
    {
        
        int datarows = Rcpp::as<Rcpp::StringVector>(DatasetValues).size();
        int isizetoWrite = datarows;
        
        if(bFullDataset) {
            isizetoWrite = dimDataset[0] * dimDataset[1]; } 
        
        names *names_list = new names[isizetoWrite];  // Convert to range list
        
        // Write data to dataset, if data to write is smaller than dataset then empty positions='\0' 
        for( int i = 0; i < isizetoWrite; i++ ) {
            int j = 0;
            if(i< datarows) {
                Rcpp::String wchrom = Rcpp::as<Rcpp::StringVector>(DatasetValues)(i);
                std::string word = wchrom.get_cstring();
                
                for( j = 0; (unsigned)j < word.size() && j < (MAXSTRING-1); j++ ) {
                    names_list[i].chr[j] = word[j]; 
                }
            }
            names_list[i].chr[j] = '\0'; // insert hdf5 end of string
        }
        return(names_list);
    }
    
    
    /**
     * @brief Get dimensions of existing dataset
     * @details Retrieves and stores the dimensions of an existing internal dataset
     */
    void getDimensExistingDataset()
    {
        try
        {
            H5::Exception::dontPrint();
            // Get dataspace from dataset
            H5::DataSpace dataspace = pdataset->getSpace();
            
            // Get the number of dimensions in the dataspace.
            int rank = dataspace.getSimpleExtentNdims();
            
            // Get the dimension size of each dimension in the dataspace
            hsize_t dims_out[2];
            hsize_t dims_max[2];
            int ndims = dataspace.getSimpleExtentDims( dims_out, dims_max);
            
            if( ndims > 0 ) {
                if(dims_max[0] == H5S_UNLIMITED) {
                    unlimited = true;
                }
                
                if( rank == 1) {
                    // dims = IntegerVector::create( static_cast<int>(dims_out[0]), static_cast<int>(1));
                    dimDataset[0] = dims_out[0];
                    dimDataset[1] = 1;
                } else if( rank == 2 ){
                    // dims = IntegerVector::create(static_cast<int>(dims_out[0]), static_cast<int>(dims_out[1]));
                    dimDataset[0] = dims_out[0];
                    dimDataset[1] = dims_out[1];
                }
            }
            
        } catch( H5::FileIException& error) { 
            Rf_error( "c++ exception getDimensExistingDataset (File IException)" );
        } catch(H5::DataSetIException& error) { 
            Rf_error( "c++ exception getDimensExistingDataset (DataSet IException)" );
        } catch(H5::GroupIException& error) { 
            Rf_error( "c++ exception getDimensExistingDataset (Group IException)" );
        } catch(H5::DataSpaceIException& error) { 
            Rf_error( "c++ exception getDimensExistingDataset (DataSpace IException)" );
        } 
        
        return void();
    }
    
    
};
}

#endif // BIGDATASTATMETH_HDF5_DATASETINTERNAL_HPP
