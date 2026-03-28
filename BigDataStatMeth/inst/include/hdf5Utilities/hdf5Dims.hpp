/**
 * @file hdf5Dims.hpp
 * @brief Dimension management utilities for HDF5 datasets
 * 
 * This file provides functionality for managing dimension names (row and column names)
 * in HDF5 datasets. It implements efficient storage and retrieval of dimension
 * information, with support for large datasets through block-wise processing.
 * 
 * Key features:
 * - Row and column name management
 * - Efficient string storage
 * - Block-wise processing for large datasets
 * - Automatic dimension validation
 * - Comprehensive error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_HDF5_DIMS_HPP
#define BIGDATASTATMETH_HDF5_DIMS_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth
{
    /**
     * @brief Class for managing dimension names in HDF5 datasets
     * 
     * This class extends hdf5Group to provide specialized functionality for
     * storing and managing dimension names (row and column names) associated
     * with HDF5 datasets.
     */
    class hdf5Dims : public hdf5Group
    {
    public:
        /**
         * @brief Constructs a dimension manager for an HDF5 dataset
         * 
         * @param pdataset Pointer to the HDF5 dataset to manage dimensions for
         * 
         * @note Creates a hidden group to store dimension information
         * @note If dimension information already exists, it will be removed
         */
        hdf5Dims(BigDataStatMeth::hdf5Dataset* pdataset) :
        hdf5Group(pdataset->getFileName(), pdataset->getGroup() + "/." + pdataset->getDatasetName() + "_dimnames")
        {
            pmaindataset = pdataset;
            
            if( exists_HDF5_element(pfile, groupname) ) {
                remove_elements(pmaindataset->getFileptr(), pmaindataset->getGroup(), "." + pmaindataset->getDatasetName() + "_dimnames" );
            }

            create_HDF5_groups(groupname);

        }

        /**
         * @brief Writes dimension names to the HDF5 file
         * 
         * @param rownames Vector of row names
         * @param colnames Vector of column names
         * 
         * @throws H5::FileIException on file operation errors
         * @throws H5::DataSetIException on dataset operation errors
         * @throws H5::GroupIException on group operation errors
         * @throws H5::DataSpaceIException on dataspace operation errors
         * @throws H5::DataTypeIException on datatype operation errors
         * 
         * @note Validates dimensions against the main dataset
         * @note Overwrites existing dimension names if they exist
         * 
         * Performance considerations:
         * - Uses block-wise processing for large string vectors
         * - Implements efficient string storage with fixed-length buffers
         * - Handles memory cleanup automatically
         */
        void writeDimnames( Rcpp::StringVector rownames, Rcpp::StringVector colnames)
        {
            try {
                // static_assert(std::is_same<T, Rcpp::RObject >::value,
                //               "Error - type not allowed");
                int nrows, ncols;

                if ( rownames.length() > 0 ) {
                    nrows = rownames.length();
                } else  {
                    nrows = 0;
                }

                if (  colnames.length() > 0 ) {
                    ncols = colnames.length();
                } else  {
                    ncols = 0;
                }
                
                if( nrows<1 && ncols<1) {
                    Rcpp::warning("Data not provided to write dimensions");
                } else {

                    // Write rownames
                    if( (unsigned)nrows == pmaindataset->nrows_file()) {
                        
                        std::string fullDatasetPath = groupname + "/" + strrows;

                        dimrownames[0] = nrows; dimrownames[1] = 1;
                        H5::DataSpace dataspace( RANK2, dimrownames );
                        bool bexists = exists_HDF5_element(pfile, fullDatasetPath);
                        if( bexists == true ) {
                            Rcpp::warning ("Rownames already exits and will be overwritten");
                            remove_elements(pmaindataset->getFileptr(), groupname, {strrows} );
                        }
                        
                        pmaindataset->setRownamesDatasetPath( fullDatasetPath );
                        writeStringVector( pdsrownames, fullDatasetPath, rownames );
                    }

                    // Write colnames
                    if( (unsigned)ncols == pmaindataset->ncols_file()) {
                        
                        std::string fullDatasetPath = groupname + "/" + strcols;

                        dimcolnames[0] = ncols; dimcolnames[1] = 1;

                        // H5::DataSpace dataspace( RANK2, dimcolnames );
                        bool bexists = exists_HDF5_element(pfile, fullDatasetPath);
                        if( bexists == true ) {
                            Rcpp::warning ("Rownames already exits and will be overwritten");
                            remove_elements(pmaindataset->getFileptr(), groupname, {strcols} );
                        }
                        pmaindataset->setColnamesDatasetPath( fullDatasetPath );
                        writeStringVector( pdscolnames, fullDatasetPath, colnames );

                    }
                }

            } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
                close_datasets();
                Rf_error( "c++ exception writeDimnames (File IException)" );
            } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
                close_datasets();
                Rf_error( "c++ exception writeDimnames (DataSet IException)" );
            } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
                close_datasets();
                Rf_error( "c++ exception writeDimnames (Group IException)" );
            } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
                close_datasets();
                Rf_error( "c++ exception writeDimnames (DataSpace IException)" );
            } catch(H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
                close_datasets();
                Rf_error( "c++ exception writeDimnames (Data TypeIException)" );
            }

          return void();

        }

        /**
         * @brief Virtual destructor
         */
        virtual ~hdf5Dims(){
        }

    protected:
        /**
         * @brief Structure for storing name strings
         */
        typedef struct names {
            char chr[MAXSTRING];  ///< Fixed-length character array for name storage
        } names;

        BigDataStatMeth::hdf5Dataset* pmaindataset;  ///< Pointer to main dataset
        H5::DataSet* pdsrownames = nullptr;          ///< Dataset for row names
        H5::DataSet* pdscolnames = nullptr;          ///< Dataset for column names
        std::string strrows = "1";                   ///< Identifier for row names
        std::string strcols = "2";                   ///< Identifier for column names
        hsize_t dimcolnames[2];                      ///< Dimensions for column names
        hsize_t dimrownames[2];                      ///< Dimensions for row names

        /**
         * @brief Closes all open datasets
         */
        void close_datasets()
        {
            pdsrownames->close();
            pdscolnames->close();

        }

        /**
         * @brief Converts R data frame to HDF5-compatible range list
         * 
         * @param DatasetValues R object containing values to convert
         * @param rowscols Identifier for row or column conversion
         * @param bFullDataset Flag indicating if full dataset conversion is needed
         * @return names* Pointer to array of converted names
         * 
         * @note Caller is responsible for freeing returned memory
         */
        names* convert_DataFrame_to_RangeList(Rcpp::RObject DatasetValues, std::string rowscols, bool bFullDataset)
        {

            int datarows = Rcpp::as<Rcpp::StringVector>(DatasetValues).size();
            int isizetoWrite = datarows;

            if(bFullDataset) {
                if( rowscols == "rows") {
                    isizetoWrite = dimrownames[0] * dimrownames[1];
                } else {
                    isizetoWrite = dimcolnames[0] * dimcolnames[1];
                }
            }

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

    private:
        /**
         * @brief Writes a string vector to an HDF5 dataset
         * 
         * @param dataset Pointer to HDF5 dataset
         * @param datasetname Name of the dataset
         * @param DatasetValues Vector of strings to write
         * 
         * Implementation details:
         * 1. Creates appropriate HDF5 datatype for strings
         * 2. Processes data in blocks for large vectors
         * 3. Handles string truncation and null termination
         * 4. Manages memory efficiently
         * 
         * @note Uses MAXSTRBLOCK for block size in processing
         * @note Automatically handles quotation mark removal
         */
        void writeStringVector( H5::DataSet* dataset, std::string datasetname, Rcpp::StringVector DatasetValues)
        {
            
            try
            {
                typedef struct name {
                    char chr[MAXSTRING];
                } name;
                
                // Turn off the auto-printing when failure occurs so that we can handle the errors appropriately
                H5::Exception::dontPrint();
                
                // Create the data space for the dataset.
                hsize_t vectorsize;
                
                if (Rcpp::is<Rcpp::StringVector>(DatasetValues))
                {
                    vectorsize = DatasetValues.length();
                    
                    // Define hdf5 dataspace size
                    hsize_t dims[] = {vectorsize};
                    H5::DataSpace dataspace(RANK1, dims);
                    
                    // Create the memory datatype.
                    H5::CompType mtype(sizeof(name));
                    mtype.insertMember("chr", HOFFSET(name, chr), H5::StrType(H5::PredType::C_S1, MAXSTRING ));
                    
                    // Create the dataset.
                    dataset = new H5::DataSet(pfile->createDataSet(datasetname, mtype, dataspace));
                    
                    //  Get dataset dataspace.
                    dataspace = dataset->getSpace();
                    
                    if(vectorsize > MAXSTRBLOCK) {
                        
                        // Number of blocks to process
                        int iblocsks = vectorsize/MAXSTRBLOCK;
                        
                        for(int i=0; i<=iblocsks; i++)
                        {
                            // Gets block size to read
                            hsize_t ilength = MAXSTRBLOCK;
                            if(i == iblocsks){
                                ilength = vectorsize - (i * MAXSTRBLOCK);
                            }
                            
                            // Convert Dataframe to range list
                            name *names_list = new name[ilength];
                            
                            for(int row=0; (unsigned)row< ilength; row++ )
                            {
                                Rcpp::String wchrom = Rcpp::as<Rcpp::StringVector>(DatasetValues)((i*MAXSTRBLOCK) + row);
                                std::string word = wchrom.get_cstring();
                                
                                boost::erase_all(word, "\"");
                                
                                int j=0;
                                for( j = 0; (unsigned)j < word.size() && j < (MAXSTRING-1); j++ ){
                                    names_list[row].chr[j] = word[j]; }
                                
                                names_list[row].chr[j] = '\0'; // insert hdf5 end of string
                            }
                            
                            // HyperSlab position and length
                            hsize_t start[1];
                            start[0] = (i*MAXSTRBLOCK);
                            hsize_t count[] = {ilength};
                            
                            H5::DataSpace memspace(RANK1, count, NULL);
                            
                            // Get position and write data in dataset
                            dataspace.selectHyperslab(H5S_SELECT_SET, count, start); 
                            dataset->write(names_list, mtype, memspace, dataspace);
                            
                            // Release resources
                            delete[] names_list;
                            memspace.close();
                        }
                        
                    } else {
                        
                        int datarows = Rcpp::as<Rcpp::StringVector>(DatasetValues).size();
                        
                        // Convert Dataframe to range list
                        name *names_list = new name[datarows];
                        
                        for(int i=0; i< datarows; i++ )
                        {
                            //..// name n;
                            Rcpp::String wchrom = Rcpp::as<Rcpp::StringVector>(DatasetValues)(i);
                            std::string word = wchrom.get_cstring();
                            
                            boost::erase_all(word, "\"");
                            
                            int j=0;
                            for( j=0; (unsigned)j < word.size() && j < (MAXSTRING-1); j++ ) {
                                        names_list[i].chr[j] = word[j];
                            }
                            
                            names_list[i].chr[j] = '\0'; // insert hdf5 end of string
                        }
                        
                        dataset->write(names_list, mtype);
                        delete[] names_list;
                    }
                    
                    // Release resources
                    dataspace.close();
                }
            } 
            catch(H5::FileIException& error) { // catch failure caused by the H5File operations
                Rcpp::Rcerr<<"c++ exception write_hdf5_string_vector (File IException)" << std::endl;
                return void();
            } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
                Rcpp::Rcerr<<"c++ exception write_hdf5_string_vector (DataSet IException)" << std::endl;
                return void();
            } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
                Rcpp::Rcerr<<"c++ exception write_hdf5_string_vector (Group IException)" << std::endl;
                return void();
            } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
                Rcpp::Rcerr<<"c++ exception write_hdf5_string_vector (DataSpace IException)" << std::endl;
                return void();
            } catch(std::exception &ex) {
                Rcpp::Rcerr << "c++ exception write_hdf5_string_vector: " << ex.what();
                return void();
            } catch (...) {
                Rcpp::Rcerr<<"C++ exception write_hdf5_string_vector (unknown reason)";
                return void();
            }
            
            dataset->close();
            return void();
        }
    };
}

#endif // BIGDATASTATMETH_HDF5_DIMS_HPP
