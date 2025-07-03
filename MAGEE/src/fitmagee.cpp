/*  MAGEE : An R Package for Mixed Model Association Test for GEne-Environment Interaction
 *  Copyright (C) 2020--2024  Xinyu Wang, Han Chen, Duy T. Pham
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <fstream>
#include <cmath>
#include <cstring>
#include <cstdio>
#include <zlib.h>
#include <bzlib.h>
#define STRICT_R_HEADERS
#include <RcppArmadillo.h>
#include <R.h>
#include <Rmath.h>
#include "read_bgen.h"
#include "zstd/lib/zstd.h"
#include "libdeflate/libdeflate.h"
using namespace std;
using namespace arma;
using namespace Rcpp;


typedef unsigned int uint;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef long long unsigned int llui;

#ifndef DBL_EPSILON
#define DBL_EPSILON 2.2204460492503131e-16;
#endif

extern "C" {
  
SEXP magee_bgen13(SEXP bgenfile_in, SEXP groupIndex_in, SEXP fbytes_in, SEXP select_in, SEXP compression_in, SEXP n_in) {
       
    try {

      string bgenfile = Rcpp::as<string>(bgenfile_in);
      
      Rcpp::IntegerVector select(select_in);
      vector<uint> groupIndex  = Rcpp::as<vector<uint>>(groupIndex_in);
      vector<llui>* bytes = (vector<llui>*)R_ExternalPtrAddr(fbytes_in);
      vector<llui>& br    = *bytes;
      
      
      uint maxLA = 65536;
      std::vector<uchar> zBuf12;
      std::vector<uchar> shortBuf12;
      uint compression = Rcpp::as<uint>(compression_in);
      char* snpID   = new char[maxLA + 1];
      char* rsID    = new char[maxLA + 1];
      char* chrStr  = new char[maxLA + 1];
      char* allele1 = new char[maxLA + 1];
      char* allele0 = new char[maxLA + 1];
      
      int n0 = Rcpp::as<int>(n_in);
      Rcpp::NumericMatrix dos(n0, groupIndex.size());
      
      struct libdeflate_decompressor* decompressor = libdeflate_alloc_decompressor();
      
      FILE* fp = fopen(bgenfile.c_str(), "rb");


      int ret;
      for (size_t grp = 0; grp < groupIndex.size(); grp++) {
          fseek(fp, br[groupIndex[grp]-1], SEEK_SET);
          
          uint16_t LS;
          ret = fread(&LS, 2, 1, fp);
          ret = fread(snpID, 1, LS, fp);
          
          uint16_t LR;
          ret = fread(&LR, 2, 1, fp);
          ret = fread(rsID, 1, LR, fp);
          
          uint16_t LC;
          ret = fread(&LC, 2, 1, fp);
          ret = fread(chrStr, 1, LC, fp);
          
          uint32_t physpos;
          ret = fread(&physpos, 4, 1, fp);
          
          uint16_t LKnum;
          ret = fread(&LKnum, 2, 1, fp);
          if (LKnum != 2) {
            Rcout << "Error reading BGEN file: There are non-bi-allelic variants. \n"; return R_NilValue;
          }
          
          uint32_t LA;
          ret = fread(&LA, 4, 1, fp);
          ret = fread(allele1, 1, LA, fp);
          
          uint32_t LB;
          ret = fread(&LB, 4, 1, fp);
          ret = fread(allele0, 1, LB, fp);
          
          uint cLen; ret = fread(&cLen, 4, 1, fp);
          
          uchar* bufAt;
          if (compression == 1) {
            zBuf12.resize(cLen - 4);
            
            uint dLen; 
            ret = fread(&dLen, 4, 1, fp);
            ret = fread(&zBuf12[0], 1, cLen - 4, fp);
            shortBuf12.resize(dLen);
            
            uLongf destLen = dLen;
            if (libdeflate_zlib_decompress(decompressor, &zBuf12[0], cLen - 4, &shortBuf12[0], destLen, NULL) != LIBDEFLATE_SUCCESS) {
              Rcpp::stop("Decompressing " + string(rsID) + "genotype block failed with libdeflate.");
            }
            bufAt = &shortBuf12[0];
          }
          else if (compression == 2) {
            zBuf12.resize(cLen - 4);
            uint dLen; ret = fread(&dLen, 4, 1, fp);
            ret = fread(&zBuf12[0], 1, cLen - 4, fp);
            shortBuf12.resize(dLen);
            
            uLongf destLen = dLen;
            size_t ret = ZSTD_decompress(&shortBuf12[0], destLen, &zBuf12[0], cLen - 4);
            if (ret > destLen) {
              if (ZSTD_isError(ret)) {
                Rcout << "Error reading bgen file: Decompressing genotype block failed. \n"; return R_NilValue;
              }
            }
            bufAt = &shortBuf12[0];
          }
          else {
            zBuf12.resize(cLen);
            ret = fread(&zBuf12[0], 1, cLen, fp);
            bufAt = &zBuf12[0];
          }
          
          uint32_t N; 
          memcpy(&N, bufAt, sizeof(int32_t));
          uint16_t K; 
          memcpy(&K, &(bufAt[4]), sizeof(int16_t));
          if (K != 2) {
            Rcout << "Error reading bgen file: There are variants with more than 2 alleles. \n"; 
            return R_NilValue;
          }
          const uint32_t min_ploidy = bufAt[6];
          if (min_ploidy != 2) {
            Rcout << "Error reading bgen file: Minimum ploidy should be 2. \n"; 
            return R_NilValue;
          }
          const uint32_t max_ploidy = bufAt[7];
          if (max_ploidy != 2) {
            Rcout << "Error reading bgen file: Maximum ploidy should be 2. \n"; 
            return R_NilValue;
          }
          
          const unsigned char* missing_and_ploidy_info = &(bufAt[8]);
          const unsigned char* probs_start = &(bufAt[10 + N]);
          const uint32_t is_phased = probs_start[-2];
          if (is_phased < 0 || is_phased > 1) {
            Rcout << "Error reading bgen file: Phased value must be 0 or 1. \n"; 
            return R_NilValue;
          }
          
          const uint32_t B = probs_start[-1];
          if (B != 8 && B!= 16 && B !=24 && B != 32) {
            Rcout << "Error reading bgen file: Bits to store probabilities must be 8, 16, 24, or 32. \n";
            return R_NilValue;
          }
          
          const uintptr_t numer_mask = (1U << B) - 1;
          const uintptr_t probs_offset = B / 8;
          
          
          uint ncount = 0;
          uint idx_k = 0;
          if (!is_phased) {
            for (size_t i = 0; i < N; i++) {
              const uint32_t missing_and_ploidy = missing_and_ploidy_info[i];
              uintptr_t numer_aa;
              uintptr_t numer_ab;
              
              if (missing_and_ploidy == 2){
                Bgen13GetTwoVals(probs_start, B, probs_offset, &numer_aa, &numer_ab);
                probs_start += (probs_offset * 2);
                
                if (select[ncount] > 0){
                  double p11 = numer_aa / double(1.0 * numer_mask);
                  double p10 = numer_ab / double(1.0 * numer_mask);
                  
                  dos(idx_k, grp) = 2 * (1 - p11 - p10) + p10;
                  idx_k++;
                }
              } else if (missing_and_ploidy == 130){
                probs_start += (probs_offset * 2);
                
                if (select[ncount] > 0){
                    dos(idx_k, grp) = NA_REAL;
                    idx_k++;
                }
              } else {
                Rcout << "Error reading bgen file: Ploidy value " << missing_and_ploidy << " is unsupported. Must be 2 or 130. \n"; 
                return R_NilValue;
              }
              
              ncount++;
            }
            
          } else {
            for (size_t i = 0; i < N; i++) {
              const uint32_t missing_and_ploidy = missing_and_ploidy_info[i];
              uintptr_t numer_aa;
              uintptr_t numer_ab;
              
              if (missing_and_ploidy == 2){
                Bgen13GetTwoVals(probs_start, B, probs_offset, &numer_aa, &numer_ab);
                probs_start += (probs_offset * 2);
                
                if (select[ncount] > 0){
                  double p11 = numer_aa / double(1.0 * numer_mask);
                  double p10 = numer_ab / double(1.0 * numer_mask);
                  
                  dos(idx_k++, grp) = double(1.0 * missing_and_ploidy) - (p11 + p10);
                  idx_k++;
                }
                
              } else if (missing_and_ploidy == 130){
                probs_start += (probs_offset * 2);
                
                if (select[ncount] > 0){
                    dos(idx_k, grp) = NA_REAL;
                    idx_k++;
                }
              } else {
                Rcout << "Error reading bgen file: Ploidy value " << missing_and_ploidy << " is unsupported. Must be 2 or 130. \n"; return R_NilValue;
              }
              
              ncount++;
              
            }
          }
      }
     
      (void)ret;
      delete[] snpID;
      delete[] rsID;
      delete[] chrStr;
      delete[] allele1;
      delete[] allele0;
      fclose(fp);
      libdeflate_free_decompressor(decompressor);
      return(dos);
    } 
    catch( std::exception &ex ) {
      forward_exception_to_r( ex );
    } catch(...) {
      ::Rf_error( "C++ exception (unknown reason)..." );
    }
    return R_NilValue;

  }  
  
  
  
  SEXP magee_bgen11(SEXP bgenfile_in, SEXP groupIndex_in, SEXP fbytes_in, SEXP select_in, SEXP compression_in, SEXP nsamples_in, SEXP n_in) {
    
    try {
      
      string bgenfile = Rcpp::as<string>(bgenfile_in);
      
      Rcpp::IntegerVector select(select_in);
      vector<uint> groupIndex  = Rcpp::as<vector<uint>>(groupIndex_in);
      vector<llui>* bytes = (vector<llui>*)R_ExternalPtrAddr(fbytes_in);
      vector<llui>& br    = *bytes;
      
      
      uint maxLA = 65536;
      uint Nsamples    = Rcpp::as<uint>(nsamples_in);
      uint Compression = Rcpp::as<uint>(compression_in);
      std::vector<uchar> zBuf11;
      std::vector<uint16_t> shortBuf11;
      uLongf destLen1  = 6 * Nsamples;
      if (Compression == 0) {
        zBuf11.resize(destLen1);
      } else {
        shortBuf11.resize(destLen1);
      }
      
      const double scale = 1.0 / 32768;
      char* snpID   = new char[maxLA + 1];
      char* rsID    = new char[maxLA + 1];
      char* chrStr  = new char[maxLA + 1];
      char* allele1 = new char[maxLA + 1];
      char* allele0 = new char[maxLA + 1];
      int n0 = Rcpp::as<int>(n_in);
      
      Rcpp::NumericMatrix dos(n0, groupIndex.size());
      
      struct libdeflate_decompressor* decompressor = libdeflate_alloc_decompressor();
      
      FILE* fp = fopen(bgenfile.c_str(), "rb");
      
      
      int ret;
      for (size_t grp = 0; grp < groupIndex.size(); grp++) {
        fseek(fp, br[groupIndex[grp]-1], SEEK_SET);
        
        uint Nrow2; 
        ret = fread(&Nrow2, 4, 1, fp);
        if (Nrow2 != Nsamples) {
          Rcout << "ERROR: Number of samples with genotype probabilities does not match the number of sample in BGEN header block.\n";
          return R_NilValue;        
        }
        
        uint16_t LS;
        ret = fread(&LS, 2, 1, fp);
        ret = fread(snpID, 1, LS, fp);
        
        uint16_t LR;
        ret = fread(&LR, 2, 1, fp);
        ret = fread(rsID, 1, LR, fp);
        
        uint16_t LC;
        ret = fread(&LC, 2, 1, fp);
        ret = fread(chrStr, 1, LC, fp);
        
        uint32_t physpos;
        ret = fread(&physpos, 4, 1, fp);
        
        uint32_t LA;
        ret = fread(&LA, 4, 1, fp);
        ret = fread(allele1, 1, LA, fp);
        
        uint32_t LB;
        ret = fread(&LB, 4, 1, fp);
        ret = fread(allele0, 1, LB, fp);
        
        
        uint16_t* probs_start;
        if (Compression == 1) {
          uint cLen; 
          ret = fread(&cLen, 4, 1, fp);
          zBuf11.resize(cLen);
          ret = fread(&zBuf11[0], 1, cLen, fp);
          
          if (libdeflate_zlib_decompress(decompressor, &zBuf11[0], cLen, &shortBuf11[0], destLen1, NULL) != LIBDEFLATE_SUCCESS) {
            Rcpp::stop("Decompressing " + string(rsID) + "genotype block failed with libdeflate.");
          }
          probs_start = &shortBuf11[0];
          
        }
        else {
          ret = fread(&zBuf11[0], 1, destLen1, fp);
          probs_start = reinterpret_cast<uint16_t*>(&zBuf11[0]);
        }
     
     
        uint ncount = 0;
        uint idx_k  = 0;
        for (size_t i = 0; i < Nsamples; i++) {
          double p11 = probs_start[3 * i] * scale;
          double p10 = probs_start[3 * i + 1] * scale;
          double p00 = probs_start[3 * i + 2] * scale;
          
          if (select[ncount-1] > 0) {
            if (p11 == 0.0 && p10 == 0.0 && p00 == 0.0) {
              dos(select[ncount]-1, grp) = NA_REAL;
              idx_k++;
              
            } else {
              double pTot = p11 + p10 + p00;
              
              dos(idx_k, grp) = (2 * p00 + p10) / pTot;
              idx_k++;
            } 
          }
          ncount++;
        }
         
      }
      
      (void)ret;
      delete[] snpID;
      delete[] rsID;
      delete[] chrStr;
      delete[] allele1;
      delete[] allele0;
      libdeflate_free_decompressor(decompressor);
      fclose(fp);
      return(dos);
    } 
    catch( std::exception &ex ) {
      forward_exception_to_r( ex );
    } catch(...) {
      ::Rf_error( "C++ exception (unknown reason)..." );
    }
    return R_NilValue;
    
  }  
}
  
