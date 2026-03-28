/**
 * @file hdf5OmicsUtils.hpp
 * @brief Utility functions for omics data analysis in HDF5 format
 *
 * This file provides utility functions for processing and analyzing omics data,
 * particularly focused on genetic variant analysis. The current implementation
 * includes functionality for calculating Minor Allele Frequency (MAF) from
 * binary genotype data.
 *
 * Key features:
 * - MAF calculation for genetic variants
 * - Support for binary encoded genotypes (0/1)
 * - Efficient vector operations
 * - Integration with R/Rcpp data structures
 *
 * @note This utility library is designed to work with genomic data stored
 * in binary format, where variants are typically encoded as 0 (reference)
 * and 1 (alternate) alleles.
 */

#ifndef BIGDATASTATMETH_OMICS_UTILS_HPP
#define BIGDATASTATMETH_OMICS_UTILS_HPP

namespace BigDataStatMeth {

    /**
     * @brief Calculates the Minor Allele Frequency (MAF) from binary genotype data
     *
     * @param x NumericVector containing binary genotype data (0/1 encoded)
     * @return double MAF value between 0 and 0.5
     *
     * @details This function calculates MAF for a vector of binary genotypes:
     * - Counts occurrences of reference (0) and alternate (1) alleles
     * - Calculates frequency as (n0 + 0.5*n1)/total_length
     * - Ensures MAF is ≤ 0.5 by taking complement if necessary
     *
     * Implementation notes:
     * - Uses efficient std::count for allele counting
     * - Handles heterozygous genotypes (1) with weight 0.5
     * - Returns MAF in range [0, 0.5]
     *
     * Formula:
     * MAF = min(p, 1-p) where p = (n0 + 0.5*n1)/n
     * - n0: count of reference alleles (0)
     * - n1: count of alternate alleles (1)
     * - n: total number of genotypes
     *
     * Example usage:
     * @code
     * Rcpp::NumericVector genotypes = {0,0,1,0,1};
     * double maf = calc_freq(genotypes);
     * @endcode
     *
     * @note This implementation assumes binary encoding of genotypes
     * and may need modification for other encoding schemes.
     *
     * Performance:
     * - Time complexity: O(n) where n is vector length
     * - Space complexity: O(n) for vector copy
     */
    inline double calc_freq(Rcpp::NumericVector x)
    {
        
        int len = x.size();
        
        std::vector<double> xc = Rcpp::as<std::vector<double> >(x);
        
        int n0 = std::count (xc.begin(), xc.end(), 0);
        int n1 = std::count (xc.begin(), xc.end(), 1);
        
        double maf = (double(n0)/len) + 0.5*(double(n1)/len);
        
        if( maf > 0.5 ) { 
            maf = 1 - maf;
        }
        
        return maf;
        
    }




}

#endif // BIGDATASTATMETH_OMICS_UTILS_HPP
