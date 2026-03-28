/**
 * @file memOtherFunctions.hpp
 * @brief Utility functions for in-memory matrix operations
 * @details This header file provides utility functions for in-memory matrix
 * operations, particularly focused on block-based processing and vector operations.
 */

#ifndef BIGDATASTATMETH_ALGEBRA_MEM_OTHER_FUNCTIONS_HPP
#define BIGDATASTATMETH_ALGEBRA_MEM_OTHER_FUNCTIONS_HPP

#include <RcppEigen.h>

namespace BigDataStatMeth {

    /**
     * @brief Calculate block positions and sizes for matrix operations
     * @details Determines optimal block positions and sizes for block-based matrix
     * operations, ensuring efficient memory usage and processing.
     * 
     * @param maxPosition Maximum position to process
     * @param blockSize Size of each block
     * @param[out] starts Vector to store starting positions of blocks
     * @param[out] sizes Vector to store sizes of blocks
     */
    inline void getBlockPositionsSizes( hsize_t maxPosition, hsize_t blockSize, std::vector<hsize_t>& starts, std::vector<hsize_t>& sizes ){
        
        hsize_t isize = blockSize + 1;
        
        for (hsize_t ii = 0; ii < maxPosition; ii += blockSize)
        {
            if( ii + blockSize > maxPosition ) {
                isize = maxPosition - ii; }
            
            hsize_t sizetoRead = getOptimBlockSize( maxPosition, blockSize, ii, isize);
            
            starts.push_back(ii);
            sizes.push_back(sizetoRead);
            
            if( ii + blockSize > maxPosition ) isize = blockSize + 1;
            if( sizetoRead > blockSize ) {
                ii = ii - blockSize + sizetoRead; }
        }
        
    }
    
    
    /**
     * @brief Compute cumulative sum of a vector
     * @details Calculates the cumulative sum (running sum) of elements in a vector.
     * For a vector [a, b, c], returns [a, a+b, a+b+c].
     * 
     * @param x Input vector
     * @return Vector containing cumulative sums
     */
    inline Eigen::VectorXd cumsum(Eigen::VectorXd x)
    {
        // initialize an accumulator variable
        double acc = 0;
        // initialize the result vector
        Eigen::VectorXd res = Eigen::VectorXd::Zero(x.size());
        for(int i = 0; i < x.size(); i++){
            acc += x[i];
            res[i] = acc;
        }
        return res;
}

}

#endif // BIGDATASTATMETH_ALGEBRA_MEM_OTHER_FUNCTIONS_HPP
