/**
 * @file hdf5SortDataset.hpp
 * @brief Utilities for sorting HDF5 datasets
 * 
 * This file provides functionality for sorting HDF5 datasets by rows or columns.
 * It implements efficient sorting operations with support for block-wise processing
 * and handles reordering of data while maintaining data integrity.
 * 
 * Key features:
 * - Row-wise and column-wise sorting
 * - Block-wise processing
 * - Memory-efficient operations
 * - Support for diagonal elements
 * - Comprehensive error handling
 * 
 * @note This module is part of the BigDataStatMeth library
 */

#ifndef BIGDATASTATMETH_UTIL_SORT_DATASETS_HPP
#define BIGDATASTATMETH_UTIL_SORT_DATASETS_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

    /**
     * @brief Finds all occurrences of a value in a range
     * 
     * Template function that searches for all occurrences of a value in a range
     * defined by iterators and returns their positions.
     * 
     * @tparam _InputIterator Iterator type for the range
     * @tparam T Type of the value to find
     * 
     * @param begin Iterator to the start of the range
     * @param end Iterator to the end of the range
     * @param val Value to search for
     * 
     * @return std::vector<_InputIterator> Vector of iterators pointing to matches
     * 
     * @note Time complexity: O(n) where n is the range size
     * @note Space complexity: O(m) where m is the number of matches
     */
    template<class _InputIterator, class T>
    std::vector<_InputIterator>
    find_all(_InputIterator begin, _InputIterator end, const T& val)
    {
        std::vector<_InputIterator> matches;
        while(begin != end)
        {
            if((*begin) == val)
                matches.push_back(begin);
            ++begin;
        }
        
        return matches;
    }

    /**
     * @brief Sorts an HDF5 dataset by rows or columns
     * 
     * This function performs sorting operations on an HDF5 dataset based on a provided
     * sorting specification. It supports both row-wise and column-wise sorting,
     * with special handling for diagonal elements.
     * 
     * @param dsIn Pointer to input HDF5 dataset
     * @param dsOut Pointer to output HDF5 dataset where sorted data will be stored
     * @param blockedSortlist List containing sorting specifications:
     *        - Column 1: Original order
     *        - Column 2: New order
     *        - Column 3: Diagonal indicators
     *        - Column 4: Additional order information
     * @param func Sorting function type:
     *        - "sortRows": Sort by rows
     *        - "sortCols": Sort by columns
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * 
     * Performance considerations:
     * - Uses block-wise reading and writing for memory efficiency
     * - Processes diagonal elements separately
     * - Minimizes data copying operations
     * 
     * Implementation details:
     * 1. Processes sorting specifications from blockedSortlist
     * 2. Handles diagonal elements (marked by 0 in diagonal column)
     * 3. Reads data blocks based on original order
     * 4. Writes data blocks to new positions
     * 5. Maintains data integrity during reordering
     * 
     * Example:
     * @code
     * BigDataStatMeth::hdf5Dataset* input = new hdf5Dataset("data.h5", "/input");
     * BigDataStatMeth::hdf5Dataset* output = new hdf5Dataset("data.h5", "/output");
     * Rcpp::List sortSpec = // ... sorting specifications ...
     * RcppSort_dataset_hdf5(input, output, sortSpec, "sortRows");
     * @endcode
     */
    inline void RcppSort_dataset_hdf5(BigDataStatMeth::hdf5Dataset* dsIn,
                                           BigDataStatMeth::hdf5Dataset* dsOut,
                                           Rcpp::List blockedSortlist,
                                           std::string func)
    {
        
        try {
            
            Rcpp::NumericVector oper = {0, 1};
            oper.names() = Rcpp::CharacterVector({ "sortRows", "sortCols"});
            
            Rcpp::StringVector rownames, colnames;
            std::vector<hsize_t> stride = {1, 1},
                                 block = {1, 1},
                                 offset = {0, 0},
                                 count = {0, 0};
            
            hsize_t* dims_out = dsIn->dim();
            
            for( int i = 0; i < blockedSortlist.length(); i++) {
                
                Rcpp::DataFrame df(blockedSortlist[i]);
                std::vector<double> order = df[0];
                std::vector<double> neworder = df[2];
                std::vector<double> diagonal = df[1];
                
                auto indices_0 = find_all(diagonal.begin(), diagonal.end(), 0);
                
                if( indices_0.size() > 0) {
                    // for(int t=0; t<indices_0.size(); t++){
                    //     Rcpp::Rcout<<"Indices val : " <<&indices_0[t]<<"\n";    
                    // }
                    
                } else {
                    if( oper.findName( func ) == 0 ) {
                        offset[0] = order[0] - 1;
                        count[0] = order.size();
                        count[1] = dims_out[1]; 
                        
                    } else if( oper.findName( func ) == 1 ) {
                        offset[1] = order[0] - 1;
                        count[1] = dims_out[1]; 
                        count[0] = order[order.size() - order[0]];
                    } 
                    
                    std::vector<double> vdIn( count[0] * count[1] ); 
                    dsIn->readDatasetBlock( {offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdIn.data() );
                    
                    if( oper.findName( func ) == 0 ) {
                        offset[0] = neworder[0]-1;
                    } else if( oper.findName( func ) == 1 ) {
                        offset[1] = neworder[0]-1;
                    }
                    
                    dsOut->writeDatasetBlock(vdIn, offset, count, stride, block);
                    
                }
            }
            
        } catch( H5::FileIException& error ) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception RcppSort_dataset_hdf5 (File IException )" << std::endl;
            return void();
        } catch( H5::DataSetIException& error ) { // catch failure caused by the dstosplit operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception RcppSort_dataset_hdf5 (dstosplit IException )" << std::endl;
            return void();
        } catch( H5::DataSpaceIException& error ) { // catch failure caused by the DataSpace operations
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"c++ exception RcppSort_dataset_hdf5 (DataSpace IException )" << std::endl;
            return void();
        } catch(std::exception &ex) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr << "c++ exception RcppSort_dataset_hdf5: " << ex.what();
            return void();
        } catch (...) {
            checkClose_file(dsIn, dsOut);
            Rcpp::Rcerr<<"C++ exception RcppSort_dataset_hdf5 (unknown reason)";
            return void();
        } 
        
        return void();
        
    }

}

#endif // BIGDATASTATMETH_UTIL_SORT_DATASETS_HPP
