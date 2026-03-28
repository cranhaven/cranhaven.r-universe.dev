/**
 * @file hdf5Groups.hpp
 * @brief HDF5 group management class and utilities
 * @details This header file provides a class for managing HDF5 groups and related
 * operations. The implementation includes:
 * 
 * Key features:
 * - Group creation and management
 * - Hierarchical group structure support
 * - Group existence checking
 * - Error handling
 * - Resource management
 */

#ifndef BIGDATASTATMETH_HDF5_GROUPS_HPP
#define BIGDATASTATMETH_HDF5_GROUPS_HPP


#include <RcppEigen.h>
#include "H5Cpp.h"

namespace BigDataStatMeth {

/**
 * @class hdf5Group
 * @brief Class for managing HDF5 groups
 * @details Provides functionality for creating and managing HDF5 groups.
 * Inherits from hdf5File to handle file operations.
 */
class hdf5Group : public hdf5File
{
    
public:
    
    /**
     * @brief Constructor with filename and group name
     * @param filename Name of HDF5 file
     * @param group Group name/path
     */
    hdf5Group(std::string filename, std::string group) :
    hdf5File(filename, false)
    {
        // Rcpp::Rcout<<"\nPassa per 1";
        // if( pfile != nullptr ){
            openFile("rw");
            groupname = group;
        // } else {
        //     Rf_error("c++ exception Please create or close the file before proceeding." );
        // }
    }
    
    
    /**
     * @brief Constructor with file pointer and group name
     * @param file HDF5 file pointer
     * @param group Group name/path
     */
    hdf5Group(H5::H5File* file, std::string group) : 
    hdf5File(file)
    {
        // Rcpp::Rcout<<"\nPassa per 2";
        
        if (pfile == nullptr) openFile("rw");  //..2025/08/13..// 
        groupname = group;          //..2025/08/13..// 
        
        //..2025/08/13..// if( pfile != nullptr ){
        //..2025/08/13..//     openFile("rw");
        //..2025/08/13..//     groupname = group;
        //..2025/08/13..// } else {
        //..2025/08/13..//     Rf_error("c++ exception Please create or close the file before proceeding");
        //..2025/08/13..// }
    }
    
    /**
     * @brief Constructor with file object and group name
     * @details Creates a new group, with option to force creation by removing existing group
     * 
     * @param objFile HDF5 file object
     * @param group Group name/path
     * @param forceGroup Whether to force group creation by removing existing group
     */
    hdf5Group(BigDataStatMeth::hdf5File* objFile, std::string group, bool forceGroup) : 
    hdf5File(objFile->getPath() , objFile->getFilename(), objFile->getFileptr(), false)
    {
        Rcpp::Rcout<<"\nPassa per 3";
        if( pfile != nullptr ){
            openFile("rw");
        } else {
            Rf_error("c++ exception Please create or close the file before proceeding." );
        }
        
        if( exists_HDF5_element(pfile, group) ) {
            if( forceGroup == true) {
                remove_elements(pfile, getGroupName(), {}); 
            } else {
                Rf_error("c++ exception. Data already exists in the file. Please set overwrite = true to proceed." );
            }
            
        }
        create_HDF5_groups(group);    
        
        groupname = group;
        
        
    }
    
    
    /**
     * @brief Constructor with file object and group name
     * @details Creates a new group if it doesn't exist
     * 
     * @param objFile HDF5 file object
     * @param group Group name/path
     */
    hdf5Group(BigDataStatMeth::hdf5File* objFile, std::string group) : 
    hdf5File(objFile->getPath() , objFile->getFilename(), objFile->getFileptr(), false)
    {
        if( pfile != nullptr ){
            openFile("rw");
        } else {
            Rf_error("c++ exception Please create file before proceed" );
        }
        
        if( !exists_HDF5_element(pfile, group) ) {
            create_HDF5_groups(group);    
        }
        
        groupname = group;
    }
    
    
    /**
     * @brief Create multiple nested groups
     * @details Creates a hierarchy of groups based on path separated by "/"
     * 
     * @param mGroup Group path with groups separated by "/"
     */
    // Create multiple group in hdf5 data file, groups must be separated by "/"
    void create_HDF5_groups( H5std_string mGroup)
    {
        try
        {
            H5::Exception::dontPrint();
            
            char * pch;
            std::string strgroup = mGroup;
            char*  cpgroup = &strgroup[0];
            std::string results = "";
            
            pch = strtok(cpgroup, "/"); 
            
            while (pch != NULL)  
            {  
                if( results.compare("") == 0 ) {
                    results = pch;
                } else {
                    results = results + "/" + pch;
                }
                
                if(!pathExists( pfile->getId(), results )) {
                    pfile->createGroup(results);
                }
                pch = strtok (NULL, "/");  
            }  
            
        } catch(H5::FileIException& error) { // catch failure caused by the H5File operations
            // pfile->close();
            Rf_error("c++ exception create_HDF5_groups_ptr (File IException)" );
            return void();
        } catch(H5::GroupIException& error) { // catch failure caused by the Group operations
            // pfile->close();
            Rf_error("c++ exception create_HDF5_groups_ptr (Group IException)" );
            return void();
        } 
        
        groupname = mGroup;
        return void();
    }
    
    
    std::string getGroupName() { return(groupname); }  // Return group name
    
    
    // Destructor
    ~hdf5Group(){
    }
    
protected:
    // Variables declaration
    std::string groupname;
    
private:
    
    // Variables declaration
    
    // Function declarations
    
    
    
};
}

#endif // BIGDATASTATMETH_HDF5_GROUPS_HPP
