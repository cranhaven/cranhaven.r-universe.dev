/**
 * @file hdf5ImputeData.hpp
 * @brief Data imputation utilities for HDF5 datasets
 * 
 * This file provides functionality for imputing missing values in HDF5 datasets,
 * particularly focused on SNP (Single Nucleotide Polymorphism) data. It implements
 * discrete value imputation based on frequency distributions and supports parallel
 * processing for large datasets.
 * 
 * Key features:
 * - Discrete value imputation
 * - Probability-based value generation
 * - Parallel processing support
 * - Block-wise data handling
 * - Row-wise and column-wise operations
 * - Comprehensive error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_UTIL_IMPUTE_DATA_HPP
#define BIGDATASTATMETH_UTIL_IMPUTE_DATA_HPP

// #include <RcppEigen.h>
// #include "H5Cpp.h"

#include<random>

namespace BigDataStatMeth {

    /**
     * @brief Generates a discrete value for imputation based on probability distribution
     * 
     * This function takes a map of value-probability pairs and generates a random
     * value according to the probability distribution, excluding NA values (3).
     * 
     * @param probMap Map containing value-frequency pairs
     * @return int Generated value for imputation
     * 
     * Implementation details:
     * 1. Extracts probabilities from the map
     * 2. Removes NA probability (value 3)
     * 3. Normalizes probabilities
     * 4. Generates random value using discrete distribution
     * 
     * @note Uses mt19937 random number generator
     * @note NA values (3) are excluded from probability calculation
     */
    inline int get_value_to_impute_discrete(std::map<double, double> probMap)
    {
        try
        {
            std::vector <double> probs;
        
            // Get values and counts for each map element
            for( auto it = probMap.begin(); it != probMap.end(); ++it )
                probs.push_back( it->second );
            
            // remove last element (corresponds to 3=<NA>)
            probs.erase(probs.end() - 1);
            
            // Get total count
            double totalSNPS = std::accumulate(probs.begin(), probs.end(), decltype(probs)::value_type(0));
            
            // Get probabilities without <NA>
            for (std::vector<double>::iterator it = probs.begin() ; it != probs.end(); ++it)
                *it = *it/totalSNPS;
            
            // Generate value with given probabilities
            std::random_device rd;
            std::mt19937 gen(rd());
            
            std::discrete_distribution<> d(probs.begin(), probs.end());
            
            return (d(gen));
        } catch(const std::exception& e) {
            Rf_error( "c++ exception get_value_to_impute_discrete : %s", e.what());
            // std::cerr << e.what() << '\n';
            return(3);
        }
        
    }
    
    
    /**
     * @brief Converts a vector to an ordered map of value frequencies
     * 
     * Creates a map where keys are unique values from the input vector and
     * values are their frequencies of occurrence.
     * 
     * @param vdata Input vector of values
     * @return std::map<double, double> Map of value-frequency pairs
     * 
     * @throws std::exception on conversion errors
     * 
     * Implementation details:
     * 1. Sorts input vector for efficient counting
     * 2. Counts occurrences of each unique value
     * 3. Creates ordered map of frequencies
     */
    inline std::map<double, double> VectortoOrderedMap_SNP_counts( Eigen::VectorXd  vdata)
    {
        std::map<double, double> mapv;
        
        try 
        {
            int position = 0;
            std::vector<double> v(vdata.data(), vdata.data()+vdata.size());
            
            std::sort(v.begin(), v.end() ); // Sort vector to optimize search and count
            
            for (size_t i = 0; i <=  *std::max_element(v.begin(), v.end()) ; ++i)  
            {
                double mycount = std::count(v.begin() + position, v.end(), i);
                mapv[i] = mycount;
                position = position + mycount;
            }
            
        } catch(std::exception &ex) {
            Rf_error( "c++ exception VectortoOrderedMap_SNP_counts : %s", ex.what());
            return std::map<double, double>();
        } catch(...) { 
            Rf_error("c++ exception VectortoOrderedMap_SNP_counts (unknown reason)"); 
            return std::map<double, double>();
        } 
        
        return mapv;
    }
    
    
    /**
     * @brief Imputes missing values in an HDF5 dataset
     * 
     * This function performs imputation of missing values (represented by 3) in an
     * HDF5 dataset. It can operate on either rows or columns and supports parallel
     * processing for improved performance.
     * 
     * @param dsIn Input HDF5 dataset
     * @param dsOut Output HDF5 dataset for imputed data
     * @param bycols If true, process by columns; if false, process by rows
     * @param stroutdataset Name for the output dataset
     * @param threads Optional number of threads for parallel processing
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * @throws H5::DataTypeIException on datatype operation errors
     * @throws std::exception on general errors
     * 
     * Performance considerations:
     * - Uses OpenMP for parallel processing
     * - Implements block-wise reading and writing
     * - Optimizes memory usage through Eigen
     * 
     * Implementation details:
     * 1. Processes data in blocks for memory efficiency
     * 2. For each block:
     *    - Reads data into memory
     *    - Calculates value frequencies
     *    - Imputes missing values based on frequencies
     *    - Writes imputed data back to file
     * 3. Handles both row-wise and column-wise operations
     * 
     * @note Missing values are identified by the value 3
     * @note Block size is fixed at 1000 elements
     * @warning Current implementation uses a basic imputation strategy
     * 
     * Example:
     * @code
     * BigDataStatMeth::hdf5Dataset* input = new hdf5Dataset("data.h5", "/input");
     * BigDataStatMeth::hdf5DatasetInternal* output = new hdf5DatasetInternal("data.h5", "/output");
     * Rcpp_Impute_snps_hdf5(input, output, true, "imputed_data", 4);
     * @endcode
     */
    inline void Rcpp_Impute_snps_hdf5(BigDataStatMeth::hdf5Dataset* dsIn, BigDataStatMeth::hdf5DatasetInternal* dsOut,
                         bool bycols, std::string stroutdataset, Rcpp::Nullable<int> threads  = R_NilValue)
    {
        
        try{
            
            std::vector<hsize_t> stride = {1,1},
                                 block = {1,1};
            
            int ilimit,
                blocksize = 1000;
            
            hsize_t* dims_out = dsIn->dim();
            
            // id bycols == true : read all rows by group of columns ; else : all columns by group of rows
            if (bycols == true) {
                ilimit = dims_out[0];
            } else {
                ilimit = dims_out[1];
            };
            
            
            if( stroutdataset.compare("")!=0) {
                dsOut->createDataset(dims_out[0], dims_out[1], "real");
            } 
            
            dsOut->openDataset();
            
            int chunks = (ilimit + (blocksize - 1)) / blocksize; //(ilimit/blocksize);

            #pragma omp parallel num_threads(get_number_threads(threads, R_NilValue)) shared(dsIn, dsOut, chunks)
            {
                #pragma omp for schedule(auto)
                for( int i=0; i < chunks; i++) 
                {
                    
                    std::vector<hsize_t> offset = {0,0},
                                         count = {0,0};
                    
                    int iread;
                    
                    if( (i+1)*blocksize < ilimit) iread = blocksize;
                    else iread = ilimit - (i*blocksize);
                    
                    // id bycols == true : read all rows by group of columns ; else : all columns by group of rows
                    if(bycols == true) {
                        count[0] = iread; 
                        count[1] = dims_out[1];
                        offset[0] = i*blocksize;
                    } else {
                        count[0] = dims_out[0];
                        count[1] = iread; 
                        offset[1] = i*blocksize;
                    }
                    
                    // read block
                    std::vector<double> vdIn( count[0] * count[1] ); 
                    #pragma omp critical(accessFile)
                    {
                        dsIn->readDatasetBlock( { offset[0], offset[1]}, { count[0], count[1]}, stride, block, vdIn.data() );
                    }
                    Eigen::MatrixXd data = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (vdIn.data(), count[0], count[1] );
                    
                    if(bycols == true) // We have to do it by rows
                    {
                        for( int row = 0; row<data.rows(); row++)  // COMPLETE EXECUTION
                        {
                            std::map<double, double> myMap;
                            myMap = VectortoOrderedMap_SNP_counts(data.row(row));
                            
                            Eigen::VectorXd ev = data.row(row);
                            std::vector<double> v(ev.data(), ev.data() + ev.size());
                            
                            auto it = std::find_if(std::begin(v), std::end(v), [](int i){return i == 3;});
                            while (it != std::end(v)) {
                                
                                if(*it==3) *it = get_value_to_impute_discrete(myMap);
                                it = std::find_if(std::next(it), std::end(v), [](int i){return i == 3;});
                            }
                            
                            Eigen::VectorXd X = Eigen::Map<Eigen::VectorXd>(v.data(), v.size());
                            data.row(row) = X;
                            
                        }
                        
                    } else {
                        for( int col = 0; col<data.cols(); col++) 
                        {
                            std::map<double, double> myMap;
                            myMap = VectortoOrderedMap_SNP_counts(data.col(col));
                            
                            Eigen::VectorXd ev = data.col(col);
                            std::vector<double> v(ev.data(), ev.data() + ev.size());
                            
                            auto it = std::find_if(std::begin(v), std::end(v), [](int i){return i == 3;});
                            while (it != std::end(v)) {
                                if(*it==3) *it = get_value_to_impute_discrete(myMap);
                                it = std::find_if(std::next(it), std::end(v), [](int i){return i == 3;});
                            }
                            
                            Eigen::VectorXd X = Eigen::Map<Eigen::VectorXd>(v.data(), v.size());
                            data.col(col) = X;
                            
                        }
                    }
                    
                    #pragma omp critical(accessFile)
                    {
                        dsOut->writeDatasetBlock( Rcpp::wrap(data), offset, count, stride, block, false);
                    }
                }
                
            }
            
        } catch( H5::FileIException& error) { // catch failure caused by the H5File operations
            checkClose_file(dsIn, dsOut);
            Rf_error("c++ exception Rcpp_Impute_snps_hdf5 (File IException)");
        } catch( H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            checkClose_file(dsIn, dsOut);
            Rf_error("c++ exception Rcpp_Impute_snps_hdf5 (DataSet IException)");
        } catch( H5::GroupIException& error) { // catch failure caused by the Group operations
            checkClose_file(dsIn, dsOut);
            Rf_error("c++ exception Rcpp_Impute_snps_hdf5 (Group IException)");
        } catch( H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            checkClose_file(dsIn, dsOut);
            Rf_error("c++ exception Rcpp_Impute_snps_hdf5 (DataSpace IException)");
        } catch( H5::DataTypeIException& error) { // catch failure caused by the DataSpace operations
            checkClose_file(dsIn, dsOut);
            Rf_error("c++ exception Rcpp_Impute_snps_hdf5 (Data TypeIException)");
        } catch(std::exception &ex) {
            checkClose_file(dsIn, dsOut);
            Rf_error( "c++ exception Rcpp_Impute_snps_hdf5 : %s", ex.what());
        } catch (...) {
            checkClose_file(dsIn, dsOut);
            Rf_error("C++ exception Rcpp_Impute_snps_hdf5 (unknown reason)");
        }
        
        return void();
    }
    
}


#endif // BIGDATASTATMETH_UTIL_IMPUTE_DATA_HPP
