/**
 * @file hdf5Utilities.hpp
 * @brief Core utility functions for HDF5 file operations
 * 
 * This file provides essential utility functions for working with HDF5 files,
 * including path validation, element management, and matrix operations. It implements
 * core functionality used throughout the BigDataStatMeth library for HDF5 operations.
 * 
 * Key features:
 * - Path existence checking
 * - Element removal
 * - Hard link creation
 * - Element renaming
 * - Matrix manipulation
 * - Comprehensive error handling
 * 
 * @note This module is part of the BigDataStatMeth library and is not intended to be 
 * used directly by the user. It is used internally by the library.
 */

#ifndef BIGDATASTATMETH_HDF5_UTILITIES_HPP
#define BIGDATASTATMETH_HDF5_UTILITIES_HPP

#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

// Functions

    /**
     * @brief Checks if a path exists in an HDF5 file
     * 
     * @param id HDF5 file or group identifier
     * @param path Path to check
     * @return bool True if path exists, false otherwise
     * 
     * @throws H5::FileIException on file operation errors
     */
    inline bool pathExists(hid_t id, const std::string& path)
    {
        try {
            return H5Lexists( id, path.c_str(), H5P_DEFAULT ) > 0;    
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            Rcpp::Rcerr<<"c++ exception pathExists (File IException)" << std::endl;
            return false;
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception pathExists: " << ex.what();
            return false;
        }  catch (...) {
            Rcpp::Rcerr<<"\nC++ exception pathExists (unknown reason)";
            return false;
        }
        
    }
    
    /**
     * @brief Checks if an HDF5 element (group or dataset) exists
     * 
     * @param file Pointer to HDF5 file
     * @param element Path to the element to check
     * @return bool True if element exists, false otherwise
     * 
     * @throws H5::FileIException on file operation errors
     * 
     * @note Handles trailing slashes in group paths
     */
    inline bool exists_HDF5_element(H5::H5File* file, std::string element)
    {
        bool bexists = false;
        try
        {
            // H5::Exception::dontPrint();
         
             if( element.substr(element.length(), element.length()) == "/" ) {
                 element = element.substr( 0, element.length()-1);
             } 
             
            // Search dataset
            if(pathExists( file->getId(), element)) 
                bexists = true;
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            file->close();
            Rcpp::Rcerr<<"c++ exception exists_HDF5_element (File IException)" << std::endl;
            return bexists;
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception exists_HDF5_element: " << ex.what();
            return(bexists);
        }  catch (...) {
            Rcpp::Rcerr<<"\nC++ exception exists_HDF5_element (unknown reason)";
            return(bexists);
        }   
        return bexists;
    }
    
    
    /**
     * @brief Removes multiple elements from an HDF5 file
     * 
     * @param file Pointer to HDF5 file
     * @param strgroup Group containing elements to remove
     * @param elements Vector of element names to remove
     * @return bool True if all removals successful, false otherwise
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * 
     * @note If elements vector is empty, removes the group itself
     */
    inline bool remove_elements(H5::H5File* file, std::string strgroup, Rcpp::StringVector elements)
    {
        
        bool bremok = true;
        
        try
        {
            H5::Exception::dontPrint();
            
            // Remove group
            if(elements.size() == 0) {
                H5std_string element = strgroup;
                
                int result = H5Ldelete(file->getId(), element.c_str(), H5P_DEFAULT);  
                if(result<0) {
                    Rcpp::Rcout<<"\n Error removing group: "<<element<<"\n";
                    bremok = false;
                } 
                
            } else { // Remove datasets
                for (int i=0; i<elements.size(); i++) 
                {
                    H5std_string element = strgroup + "/" + elements[i];
                    
                    int result = H5Ldelete(file->getId(), element.data(), H5P_DEFAULT);  
                    if(result<0) {
                        Rcpp::Rcout<<"\n Error removing : "<<element<<"\n";
                        bremok = false;
                    } 
                }    
            }
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (File IException)" << std::endl;
            return(bremok);
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (Group IException)" << std::endl;
            return(bremok);
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (DataSet IException)" << std::endl;
            return(bremok);
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (DataSpace IException)" << std::endl;
            return(bremok);
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception remove_HDF5_multiple_elements_ptr: " << ex.what();
            return(bremok);
        }  catch (...) {
            Rcpp::Rcerr<<"\nC++ exception remove_HDF5_multiple_elements_ptr (unknown reason)";
            return(bremok);
        }
        
        return(bremok);
    }


    /**
     * @brief Removes a single element from an HDF5 file
     * 
     * @param file Pointer to HDF5 file
     * @param element Full path to element to remove
     * @return bool True if removal successful, false otherwise
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::DataSpaceIException on dataspace operation errors
     * 
     * @note If element is not found, returns false
     */
    inline bool remove_elements(H5::H5File* file, H5std_string element)
    {
        
        bool bremok = true;
        
        try
        {
            H5::Exception::dontPrint();
            
            // H5std_string elementtoremove = element;
            
            int result = H5Ldelete(file->getId(), element.data(), H5P_DEFAULT);  
            if(result<0) {
                Rcpp::Rcout<<"\n Error removing : "<<element<<"\n";
                bremok = false;
            } 
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (File IException)" << std::endl;
            return(bremok);
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (Group IException)" << std::endl;
            return(bremok);
        } catch(H5::DataSetIException& error) { // catch failure caused by the DataSet operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (DataSet IException)" << std::endl;
            return(bremok);
        } catch(H5::DataSpaceIException& error) { // catch failure caused by the DataSpace operations
            Rcpp::Rcerr<<"c++ exception remove_HDF5_multiple_elements_ptr (DataSpace IException)" << std::endl;
            return(bremok);
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception remove_HDF5_multiple_elements_ptr: " << ex.what();
            return(bremok);
        }  catch (...) {
            Rcpp::Rcerr<<"\nC++ exception remove_HDF5_multiple_elements_ptr (unknown reason)";
            return(bremok);
        }
        
        return(bremok);
    }
    
    
    /////////////////////
    // NOT TESTED !!!! //
    /////////////////////
    /**
     * @brief Creates a hard link in an HDF5 file
     * 
     * @param file Pointer to HDF5 file
     * @param original Path to original element
     * @param link Path for new link
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws std::exception on other errors
     * 
     * @warning This function is not fully tested
     */
    inline void createHardLink( H5::H5File* file, std::string original, std::string link)
    {
        
        try{
            
            H5::Exception::dontPrint();
            
            const char * charOriginal = original.c_str();
            const char * charLink = link.c_str();
            
            herr_t status = H5Lcreate_hard(file->getId(), charOriginal, file->getId(), charLink, H5P_DEFAULT, H5P_DEFAULT);
            
            if(status<0) {
                Rcpp::Rcerr<<"c++ exception createHardLink (create_hard IException)" << std::endl;
                return void();
            }
            
        } catch(H5::FileIException& error) { 
            Rcpp::Rcerr<<"c++ exception createHardLink (File IException)" << std::endl;
            return void();
        } catch(H5::DataSetIException& error) { 
            Rcpp::Rcerr<<"c++ exception createHardLink (DataSet IException)" << std::endl;
            return void();
        } catch(H5::GroupIException& error) { 
            Rcpp::Rcerr<<"c++ exception createHardLink (Group IException)" << std::endl;
            return void();
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception createHardLink: " << ex.what();
            return void();
        }  catch (...) {
            Rcpp::Rcerr<<"\nC++ exception createHardLink (unknown reason)";
            return void();
        }
        
        return void();
    }


    /**
     * @brief Renames an element in an HDF5 file
     * 
     * @param file Pointer to HDF5 file
     * @param original Current path of element
     * @param link New path for element
     * 
     * @throws H5::FileIException on file operation errors
     * @throws H5::DataSetIException on dataset operation errors
     * @throws H5::GroupIException on group operation errors
     * @throws std::exception on other errors
     */
    inline void renameElement( H5::H5File* file, std::string original, std::string link)
    {
        
        try{
            
            H5::Exception::dontPrint();
            
            const char * charOriginal = original.c_str();
            const char * charLink = link.c_str();
            
            // Rcpp::Rcout<<"original: "<< original<<" - Desti: "<<link<<"\n";
            
            herr_t status = H5Lmove(file->getId(), charOriginal, file->getId(), charLink, H5P_DEFAULT, H5P_DEFAULT);
            
            if(status<0) {
                Rcpp::Rcerr<<"c++ exception renameElement (rename_element IException)" << std::endl;
                return void();
            } 
            
            
        } catch(H5::FileIException& error) { 
            Rcpp::Rcerr<<"c++ exception renameElement (File IException)" << std::endl;
            return void();
        } catch(H5::DataSetIException& error) { 
            Rcpp::Rcerr<<"c++ exception renameElement (DataSet IException)" << std::endl;
            return void();
        } catch(H5::GroupIException& error) { 
            Rcpp::Rcerr<<"c++ exception renameElement (Group IException)" << std::endl;
            return void();
        } catch(std::exception &ex) {
            Rcpp::Rcerr << "c++ exception renameElement: " << ex.what();
            return void();
        }  catch (...) {
            Rcpp::Rcerr<<"\nC++ exception renameElement (unknown reason)";
            return void();
        }
        
        return void();
    }


    /**
     * @brief Removes a row from an Eigen matrix
     * 
     * @param matrix Reference to matrix to modify
     * @param rowToRemove Index of row to remove
     * 
     * Implementation details:
     * 1. Shifts remaining rows up
     * 2. Resizes matrix to remove last row
     * 
     * @note Modifies matrix in place
     * @note Uses efficient block operations
     */
    inline void removeRow(Eigen::MatrixXd& matrix, unsigned int rowToRemove)
    {
        unsigned int numRows = matrix.rows()-1;
        unsigned int numCols = matrix.cols();
        
        if( rowToRemove < numRows )
            matrix.block(rowToRemove,0,numRows-rowToRemove,numCols) = matrix.bottomRows(numRows-rowToRemove).eval();
        
        matrix.conservativeResize(numRows,numCols);
    }
    
    /**
     * @brief Removes a column from an Eigen matrix
     * 
     * @param matrix Reference to matrix to modify
     * @param colToRemove Index of column to remove
     * 
     * Implementation details:
     * 1. Shifts remaining columns left
     * 2. Resizes matrix to remove last column
     * 
     * @note Modifies matrix in place
     * @note Uses efficient block operations
     */
    inline void removeColumn(Eigen::MatrixXd& matrix, unsigned int colToRemove)
    {
        unsigned int numRows = matrix.rows();
        unsigned int numCols = matrix.cols()-1;
        
        if( colToRemove < numCols )
            matrix.block(0,colToRemove,numRows,numCols-colToRemove) = matrix.rightCols(numCols-colToRemove).eval();
        
        matrix.conservativeResize(numRows,numCols);
    }

}

#endif // BIGDATASTATMETH_HDF5_UTILITIES_HPP

