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
#include <Rcpp.h>
#include "read_bgen.h"
#include "zstd/lib/zstd.h"
#include "libdeflate/libdeflate.h"
using namespace std;
using namespace arma;
using namespace Rcpp;


typedef unsigned int uint;
typedef unsigned char uchar;
typedef unsigned short ushort;

#ifndef DBL_EPSILON
#define DBL_EPSILON 2.2204460492503131e-16;
#endif

extern "C" 
{
  
    
  SEXP glmm_gei_bgen13( SEXP dupe_flag, SEXP res_in, SEXP nullObj_in, SEXP bgenfile_in, SEXP outfile_in, SEXP center_in, SEXP minmaf_in, SEXP maxmaf_in, SEXP minmac_in, SEXP missrate_in, SEXP minrsq_in, SEXP miss_method_in, SEXP nperbatch_in, 
                       SEXP ei_in, SEXP qi_in, SEXP isNullP_in, /*SEXP isNullEC_in,*/ SEXP strata_in,
                       SEXP select_in, SEXP begin_in, SEXP end_in, SEXP pos_in, SEXP nbgen_in, SEXP compression_in, SEXP metaOutput_in) {
    try{
      
      bool isNullP = Rcpp::as<bool>(isNullP_in);      
      int ei = Rcpp::as<int>(ei_in);
      int qi = Rcpp::as<int>(qi_in);
      bool isDupeID = Rcpp::as<bool>(dupe_flag);
      arma::mat P;
      arma::sp_mat Sigma_i;
      arma::sp_mat Sigma_iX;
      arma::sp_mat cov;
      arma::sp_mat J;             
      Rcpp::List null_obj(nullObj_in);
      Rcpp::List strata_list(strata_in);
      int strataList_size = 0;
      bool skip_strata = true;

      if (strata_list[0]!=R_NilValue) {
        skip_strata = false;
        strataList_size = strata_list.size();
      }

      arma::mat E = as<arma::mat>(null_obj["E"]);
      arma::mat EC;
      
      if (!isNullP) {
        P = as<arma::mat>(null_obj["P"]);
        (void)Sigma_i; (void)Sigma_iX; (void)cov;
        
      } else {
        Sigma_i = as<arma::sp_mat>(null_obj["Sigma_i"]);
        Sigma_iX = as<arma::sp_mat>(null_obj["Sigma_iX"]);
        cov = as<arma::sp_mat>(null_obj["cov"]);
        (void)P;
      }
      
      Rcpp::NumericVector res_r(res_in);
      arma::vec res(res_r.begin(), res_r.size(), false);
      const char center = Rcpp::as<char>(center_in);
      const double minmaf = Rcpp::as<double>(minmaf_in);
      const double maxmaf = Rcpp::as<double>(maxmaf_in);
      const double missrate = Rcpp::as<double>(missrate_in);
      const double minmac = Rcpp::as<double>(minmac_in);
      const double minrsq = Rcpp::as<double>(minrsq_in);
      const char miss_method = Rcpp::as<char>(miss_method_in);
      const bool metaOutput = Rcpp::as<bool>(metaOutput_in);
      string bgenfile = Rcpp::as<string>(bgenfile_in);
      string outfile  = Rcpp::as<string>(outfile_in);      
      const size_t npb = Rcpp::as<size_t>(nperbatch_in);
      Rcpp::IntegerVector select(select_in);

      string line, snp;
      size_t n = res.n_elem;
      size_t n_obs = res.n_elem;
      if (!isDupeID)
      {
        J = as<arma::sp_mat>(null_obj["J"]);
	n = J.n_rows;
      }
 
      vec g(n);
      uvec gmiss(n), snp_skip = zeros<uvec>(npb);
      mat G(n_obs, npb);
      string* tmpout = new string[npb];
      vector <string> biminfo;
      double gmean, mac, gsqmean, geno, gmax, gmin, rsq;
      size_t ncount, nmiss, npbidx = 0;
      double compute_time = 0.0;
      ofstream writefile(outfile.c_str(), std::ofstream::binary);

      struct libdeflate_decompressor* decompressor = libdeflate_alloc_decompressor();      
      uint maxLA = 65536;
      std::vector<uchar> zBuf12;
      std::vector<uchar> shortBuf12;
      uint compression = Rcpp::as<uint>(compression_in);
      char* snpID   = new char[maxLA + 1];
      char* rsID    = new char[maxLA + 1];
      char* chrStr  = new char[maxLA + 1];
      char* allele1 = new char[maxLA + 1];
      char* allele0 = new char[maxLA + 1];      
      uint begin = Rcpp::as<uint>(begin_in);
      uint end   = Rcpp::as<uint>(end_in);
      uint Nbgen = Rcpp::as<uint>(nbgen_in);
      vec g2(Nbgen);
      g2.fill(datum::nan);
      long long unsigned int byte = Rcpp::as<long long unsigned int>(pos_in);
      FILE* fp = fopen(bgenfile.c_str(), "rb");
      fseek(fp, byte, SEEK_SET);      
      int ret;
      mat strata_AF(end,strataList_size);
      mat strata_N(end,strataList_size);

      for (uint m = begin; m < end; m++) {
        stringstream writeout;        
        ushort LS; 
        ret = fread(&LS, 2, 1, fp);
        ret = fread(snpID, 1, LS, fp); 
        snpID[LS] = '\0';
        string str_snpID = "NA";
        if (LS != 0) { str_snpID = string(snpID); }
        
        ushort LR; 
        ret = fread(&LR, 2, 1, fp);
        ret = fread(rsID, 1, LR, fp);  
        rsID[LR] = '\0';
        
        ushort LC; 
        ret = fread(&LC, 2, 1, fp);
        ret = fread(chrStr, 1, LC, fp); 
        chrStr[LC] = '\0';
        
        uint physpos; 
        ret = fread(&physpos, 4, 1, fp);
	      string physpos_tmp = to_string(physpos);
	      
        ushort LKnum; 
        ret = fread(&LKnum, 2, 1, fp);
        if (LKnum != 2) { 
          Rcout << "Error reading BGEN file: There are non-bi-allelic variants. \n"; return R_NilValue;
        }
        
        uint32_t LA; 
        ret = fread(&LA, 4, 1, fp);
        ret = fread(allele1, 1, LA, fp); 
        allele1[LA] = '\0';
        
        uint32_t LB; 
        ret = fread(&LB, 4, 1, fp);
        ret = fread(allele0, 1, LB, fp); 
        allele0[LB] = '\0';
        
        uint cLen; ret = fread(&cLen, 4, 1, fp);
        
        uchar* bufAt;
        if (compression == 1) {
          zBuf12.resize(cLen - 4);
          uint dLen; ret = fread(&dLen, 4, 1, fp);
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
      
        uint32_t N; memcpy(&N, bufAt, sizeof(int32_t));
        uint16_t K; memcpy(&K, &(bufAt[4]), sizeof(int16_t));
        if (K != 2) {Rcout << "Error reading bgen file: There are variants with more than 2 alleles. \n"; return R_NilValue;}
        const uint32_t min_ploidy = bufAt[6];
        if (min_ploidy != 2) {Rcout << "Error reading bgen file: Minimum ploidy should be 2. \n"; return R_NilValue;}
        const uint32_t max_ploidy = bufAt[7];
        if (max_ploidy != 2) {Rcout << "Error reading bgen file: Maximum ploidy should be 2. \n"; return R_NilValue;}
        
        const unsigned char* missing_and_ploidy_info = &(bufAt[8]);
        const unsigned char* probs_start = &(bufAt[10 + N]);
        const uint32_t is_phased = probs_start[-2];
        if (is_phased < 0 || is_phased > 1) {Rcout << "Error reading bgen file: Phased value must be 0 or 1. \n"; return R_NilValue;}
        
        const uint32_t B = probs_start[-1];
        
      	if (B != 8 && B!= 16 && B !=24 && B != 32) {
          Rcout << "Error reading bgen file: Bits to store probabilities must be 8, 16, 24, or 32. \n"; return R_NilValue;
      	}
      	
	      const uintptr_t numer_mask = (1U << B) - 1;
        const uintptr_t probs_offset = B / 8;
        
        gmean=0.0;
	mac=0.0;
	gsqmean=0.0;
        gmax=-100.0;
        gmin=100.0;
	rsq=0.0;
        nmiss=0;
        ncount = 0;

        if (!is_phased) {
          for (size_t i = 0; i < N; i++) {
            const uint32_t missing_and_ploidy = missing_and_ploidy_info[i];
            uintptr_t numer_aa;
            uintptr_t numer_ab;
            
            if (missing_and_ploidy == 2){
              Bgen13GetTwoVals(probs_start, B, probs_offset, &numer_aa, &numer_ab);
              probs_start += (probs_offset * 2);
                      
            } else if (missing_and_ploidy == 130){
              probs_start += (probs_offset * 2);
	      if (select[ncount] > 0){
                gmiss[select[ncount]-1] = 1;
                nmiss++;
	      }
              ncount++;
              continue;
            
            } else {
		            Rcout << "Error reading bgen file: Ploidy value " << missing_and_ploidy << " is unsupported. Must be 2 or 130. \n"; return R_NilValue;    
            }
                        
            if (select[ncount] > 0){ 
              double p11 = numer_aa / double(1.0 * numer_mask);
              double p10 = numer_ab / double(1.0 * numer_mask);
              geno = 2 * (1 - p11 - p10) + p10;
              gmiss[select[ncount]-1] = 0;
              g[select[ncount]-1] = geno;
              g2[select[ncount]-1] = geno;
              gmean += geno;
	      gsqmean += geno * geno;
              if (geno > gmax) { gmax = geno; }
              if (geno < gmin) { gmin = geno; }
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
             
           } else if (missing_and_ploidy == 130){
             probs_start += (probs_offset * 2);
	     if (select[ncount] > 0){
               gmiss[select[ncount]-1] = 1;
               nmiss++;
	     }
             ncount++;
             continue;
             
           } else {
	             Rcout << "Error reading bgen file: Ploidy value " << missing_and_ploidy << " is unsupported. Must be 2 or 130. \n"; return R_NilValue;
           }
           
           if (select[ncount] > 0){ 
             double p11 = numer_aa / double(1.0 * numer_mask);
             double p10 = numer_ab / double(1.0 * numer_mask);
             geno = double(1.0 * missing_and_ploidy) - (p11 + p10);
             gmiss[select[ncount]-1] = 0;
             g[select[ncount]-1] = geno;
             g2[select[ncount]-1] = geno;
             gmean += geno;
	     gsqmean += geno * geno;
             if (geno > gmax) { gmax = geno; }
             if (geno < gmin) { gmin = geno; }
           } 
           ncount++;           
         }
       }
	  mac = gmean;
	  if((double)(n-nmiss)<mac) {
	    mac = (double)(n-nmiss) * 2.0 - mac;
	  }
          gmean/=(double)(n-nmiss);                                
	  gsqmean/=(double)(n-nmiss);
	  rsq = (gsqmean - gmean * gmean) * (double)(n-nmiss) / (double)(n-nmiss-1) / (gmean * (1.0 - gmean/2.0));

           if (skip_strata) {
	     writeout << str_snpID << "\t" << rsID << "\t" << chrStr << "\t" << physpos_tmp << "\t" << allele1 << "\t" << allele0 << "\t" << (n-nmiss) << "\t"<< gmean/2.0 << "\t" << mac << "\t" << rsq << "\t";
            } else {
                writeout << snpID << "\t" << rsID << "\t" << chrStr << "\t" << physpos_tmp << "\t" << allele1 << "\t" << allele0 << "\t" << (n-nmiss) << "\t" << gmean/2.0 << "\t" << mac << "\t" << rsq << "\t";
                std::vector<double> strata_range(strataList_size);
                for (int strata_idx = 0; strata_idx < strataList_size; strata_idx++) {
                      uvec strata_tmp = as<arma::uvec>(strata_list[strata_idx]);
                      uvec strata_gmiss = gmiss.elem(strata_tmp-1);
                      vec strata_g = g.elem(strata_tmp-1);
                      strata_AF(m,strata_idx) = mean(strata_g.elem(find(strata_gmiss == 0))) / 2.0;
                      vec tmp;
                      tmp= strata_g.elem(find(strata_gmiss == 0));
                      strata_N(m,strata_idx) =tmp.n_elem;            
                }
                for (int strata_idx = 0; strata_idx < strataList_size; strata_idx++) {
                      writeout << strata_N(m,strata_idx) << "\t";
                      writeout << strata_AF(m,strata_idx) << "\t";
                }
          
              }         

           for (size_t j=0; j<n; ++j) {
            if (gmiss[j]==1) {
             g[j] = gmean;
             if (center=='n' && miss_method=='o') {g[j] = 0.0;} // remove missing genotypes
            }
            if (center=='c') {
             g[j] -= gmean;
            }
          }
          double AF= gmean / 2.0; // convert mean to allele freq
          if(((double)nmiss/n>missrate) || (mac<minmac) || (rsq<minrsq) || ((AF<minmaf || AF>maxmaf) && (AF<1-maxmaf || AF>1-minmaf))) { // monomorphic, missrate, MAF, MAC, Rsq
            snp_skip[npbidx] = 1;            
          } 
          else {
	    if (!isDupeID){
	      G.col(npbidx) = J.t()*g;
	    } else {
	      G.col(npbidx) = g;
            }
          }
        
        tmpout[npbidx] = writeout.str();
        writeout.clear();
  
        npbidx++;

        if((m+1 == end) || (npbidx == npb)) {        
         if (npbidx != npb) {
           G.reshape(n_obs, npbidx);
           snp_skip = snp_skip.rows(0,npbidx-1);
         }

         uvec snp_idx = find(snp_skip == 0);
         G = G.cols(snp_idx);
         int ng = G.n_cols; 

         mat IV_U;
         mat IV_U1;
         mat IV_V_i;
         mat IV_V_i1;
         mat IV_E_i;
         mat IV_GE_i;
         mat STAT_JOINT_tmp;
         vec BETA_MAIN;
         vec STAT_INT;
         vec STAT_JOINT;
         vec SE_MAIN;
         mat BETA_INT;
         mat BETA_INT1;
         vec PVAL_MAIN(ng);
         PVAL_MAIN.fill(NA_REAL);
         vec PVAL_INT(ng);
         PVAL_INT.fill(NA_REAL);
         vec PVAL_JOINT(ng);
         PVAL_JOINT.fill(NA_REAL);
         size_t ngei1=ng*(ei+1);
  
         if (G.n_cols != 0) {           
           mat K;
           for (int ei_idx = 0; ei_idx < ei+qi; ei_idx++) {
              mat tmpK = G.each_col() % E.col(ei_idx);
              K = join_horiz(K, tmpK);
           }
        
           mat K1;
           mat O(E.n_rows,1.00000,fill::ones);
           mat E1;
           E1=join_horiz(O,E);
           for (int ei_idx = 0; ei_idx < ei+qi+1; ei_idx++) {
              mat tmpK1 = G.each_col() % E1.col(ei_idx);
              K1 = join_horiz(K1, tmpK1);
           }
           
            vec U = G.t() * res;                           
            sp_mat Gsp(G);
            sp_mat PG;
            if (!isNullP) {
              PG = P.t() * G;
            } else {
              sp_mat GSigma_iX =  Gsp.t() * Sigma_iX;
              PG = (Sigma_i.t() * Gsp) - (Sigma_iX * (GSigma_iX * cov.t()).t());
            }

            mat GPG = (G.t() * PG)  % kron(ones(1, 1), mat(ng, ng, fill::eye));         
            mat GPG_i;
            bool is_non_singular = inv(GPG_i, GPG);
            if (!is_non_singular) {
              GPG_i = pinv(GPG);
            }

            mat V_i;
            V_i = diagvec(GPG_i);           
            vec V_MAIN_adj = diagvec(GPG_i);
            V_MAIN_adj = V_MAIN_adj.rows(0, ng-1);            
            vec BETA_MAIN_adj = GPG_i.t() * U; 
            BETA_MAIN_adj= BETA_MAIN_adj.rows(0, ng-1);

            vec STAT_MAIN_adj(ng);
            STAT_MAIN_adj.fill(NA_REAL);
            for (size_t s = 0; s < V_MAIN_adj.size(); s++) {
              if (V_MAIN_adj[s] > 0) {
                STAT_MAIN_adj[s] = (BETA_MAIN_adj[s] * BETA_MAIN_adj[s]) / V_MAIN_adj[s];
              }
            } 
            
            BETA_MAIN = V_i % U.rows(0,ng-1);
            SE_MAIN = sqrt(V_i);
            vec STAT_MAIN = BETA_MAIN % U.rows(0,ng-1);
            for (size_t s = 0; s < STAT_MAIN.size(); s++) {
              if (STAT_MAIN[s] > 0) {
                PVAL_MAIN[s] = Rf_pchisq(STAT_MAIN[s], 1, 0, 0);
              }
            }

            mat KPK1;
            if (!isNullP) {
              KPK1 = K1.t() * (P.t() * K1);
            } else {
              mat KSigma_iX1 = K1.t() * Sigma_iX;
              KPK1 = (K1.t() * (Sigma_i.t() * K1)) - (KSigma_iX1 * (KSigma_iX1 * cov.t()).t());
            }

            KPK1 = KPK1 % kron(ones(ei+qi+1, ei+qi+1), mat(ng, ng, fill::eye));

            bool is_non_singular_K = inv(IV_V_i1,KPK1);
            if (!is_non_singular_K) {
              IV_V_i1=pinv(KPK1);
            }
            //IV_V_i1=inv(KPK1);
            mat cross_K_res = K1.t() *res;
            mat IV_U_kron1 = kron(ones(ei+qi+1,1), mat(ng, ng, fill::eye)); 
            IV_U1 = IV_U_kron1.each_col() % cross_K_res;
            BETA_INT1 = (IV_V_i1 * IV_U1);

            bool is_non_singular_IV_V1 = inv(IV_E_i,IV_V_i1(span(ng,ngei1-1),span(ng,ngei1-1)));
            if (!is_non_singular_IV_V1) {
              IV_E_i=pinv(IV_V_i1(span(ng,ngei1-1),span(ng,ngei1-1))); 
            }
            //IV_E_i=inv(IV_V_i1(span(ng,ngei1-1),span(ng,ngei1-1))); 
            IV_U=IV_E_i*BETA_INT1.rows(ng,ngei1-1);
            STAT_INT=diagvec(IV_U.t()*BETA_INT1.rows(ng,ngei1-1));

            //bool is_non_singular_IV_V2 = inv(IV_GE_i,IV_V_i1(span(0,ngei1-1),span(0,ngei1-1)));
            //if (!is_non_singular_IV_V2) {
              try {
                IV_GE_i=inv(IV_V_i1(span(0,ngei1-1),span(0,ngei1-1))); 
                //STAT_JOINT_tmp=solve(IV_V_i1(span(0,ngei1-1),span(0,ngei1-1)),BETA_INT1.rows(0,ngei1-1));
                STAT_JOINT_tmp=IV_GE_i*BETA_INT1.rows(0,ngei1-1);
                STAT_JOINT=diagvec(STAT_JOINT_tmp.t()*BETA_INT1.rows(0,ngei1-1));
            

                for (size_t s = 0; s < STAT_INT.size(); s++) {
                    PVAL_INT[s] = Rf_pchisq(STAT_INT[s], ei, 0, 0);
                    if (is_finite(PVAL_MAIN[s])) {
                      PVAL_JOINT[s] = Rf_pchisq(STAT_JOINT[s], 1+ei, 0, 0);
                    }
                  } 
              } 
            //}
            catch (const runtime_error& error){
                 Rcout << "It is singular matrix "<< "\n";
                 for (size_t s = 0; s < STAT_INT.size(); s++) {
                    PVAL_JOINT[s]=DBL_EPSILON;
                 }
                 
            }
            

            //IV_GE_i=inv(IV_V_i1(span(0,ngei1-1),span(0,ngei1-1))); 
            //STAT_JOINT_tmp=IV_GE_i*BETA_INT1.rows(0,ngei1-1);
            //STAT_JOINT=diagvec(STAT_JOINT_tmp.t()*BETA_INT1.rows(0,ngei1-1));
            

            for (size_t s = 0; s < STAT_INT.size(); s++) {
                PVAL_INT[s] = Rf_pchisq(STAT_INT[s], ei, 0, 0);
                //if (is_finite(PVAL_MAIN[s])) {
                  //PVAL_JOINT[s] = Rf_pchisq((STAT_MAIN[s]+STAT_INT[s]), 1+ei, 0, 0);
                //}
            }   
         }
    
         uvec b_idx = regspace<uvec>(0, ng, (ei+qi-1) * ng);
         uvec b_idx1 = regspace<uvec>(0, ng, (ei+qi) * ng);
         int ng_j = 0;
         for(size_t j=0; j<npbidx; ++j) {
           if(snp_skip[j] == 1) { // monomorphic, missrate, MAF
             writefile << tmpout[j] << "NA\tNA\tNA\tNA\tNA\tNA\tNA";             
             if (metaOutput) {
               for (int e=0; e < (ei+qi + ei+qi + ((ei+qi) * (ei+qi + 1) / 2)); e++) {
		 //if(e == 0) {
		 //writefile << "NA";
		 //} else {
		   writefile << "\tNA";
		   //}
               } 
             }
             writefile << "\n";
           } 
           else {         
             writefile << tmpout[j] << BETA_MAIN[ng_j] << "\t" << SE_MAIN[ng_j] << "\t";
             if (metaOutput) {
               // Beta Int the diaganol of BETA_INT1
               for (int b=0; b < ei+qi+1; b++) {
                int row = b_idx1[b] + ng_j ;
                writefile << BETA_INT1(row, ng_j ) << "\t";                 
               }
      
               // Var (the diaganol of IV_V_i1)                
               for (int b=0; b < ei+qi+1; b++) {
                 int col = b_idx1[b] + ng_j ;
                 for (int d=0; d < ei+qi+1; d++) {
                   if(b == d) {
                    int row = b_idx1[d] + ng_j ; 
                    writefile << sqrt(IV_V_i1(row, col)) << "\t";
                  } 
                }
               }

               // Cov (the lower triangle elements of IV_V_i1)
               for (int b=0; b < ei+qi+1; b++) {
                 int col = b_idx1[b] + ng_j ;
                 for (int d=0; d < ei+qi+1; d++) {
                   if(d > b) {
                     int row = b_idx1[d] + ng_j ;
                     writefile << IV_V_i1(row, col) << "\t";
                    }
                  }
                }            
             }
             else {
               //generate a squre matrix with elements 1 to E1.col*E1.col, for example E1.col=2, matrix [1,3;2,4] 
               //E1.col=3, matrix [1,4,7;2,5,8;3,6,9]
               mat O(E.n_rows,1,fill::ones);
               mat E1;
               E1=join_horiz(O,E);
               int ncolE=E1.n_cols;
               mat split_mat(ncolE,ncolE);
               for (int i=0; i<ncolE; i++) {
                 for (int j=0; j<ncolE; j++)
                 split_mat(i,j)=i+1-ncolE+ncolE*(j+1);
                }
                split_mat=split_mat(span(1,ei),span(1,ei));
                if ( split_mat.n_elem ==1){
               // Beta Int
                    for (int b=0; b < ei+1; b++) {
                        int row = b_idx1[b] + ng_j ;
                        //NOT the first ng row 
                        if (row>ng-1) {
                        writefile << BETA_INT1(row, ng_j ) << "\t";
                    }
                }

              // Var (the diaganol elements)                
                    for (int b=0; b < ei+1; b++) {
                        int col = b_idx1[b] + ng_j ;
                        for (int d=0; d < ei+1; d++) {
                           if(b == d ) {
                                int row = b_idx1[d] + ng_j ; 
                       //NOT the first ng row or first ng col
                                if (row > ng-1  && col >ng-1) {
                                    writefile << sqrt(IV_V_i1(row, col)) << "\t";
                                }
                            }
                        }
                    }               
                }

               else {
                    // Beta Int
                    for (int b=0; b < ei+1; b++) {
                        int row = b_idx1[b] + ng_j ;
                    //NOT the first ng row 
                        if (row>ng-1) {
                            writefile << BETA_INT1(row, ng_j ) << "\t";
                        }
                    }
                    for (int b=0; b < ei+1; b++) {
                        int col = b_idx1[b] + ng_j ;
                        for (int d=0; d < ei+1; d++) {
                            if(b == d ) {
                              int row = b_idx1[d] + ng_j ; 
                    //NOT the first ng row or first ng col
                              if (row > ng-1  && col >ng-1) {
                                writefile << sqrt(IV_V_i1(row, col)) << "\t";
                               }
                            }
                        }
                    }
                    // Cov (the lower triangle )
                    for (int b=0; b < ei+1; b++) {
                        int col = b_idx1[b] + ng_j ;
                        for (int d=0; d < ei+1; d++) {
                            if(d > b) {
                                int row = b_idx1[d] + ng_j ;
                                if (row > ng-1  && col >ng-1) {
                                    writefile << IV_V_i1(row, col) << "\t";
                                }
                            }
                        }
                    }
                }
             }
             writefile << PVAL_MAIN[ng_j] <<  "\t" << PVAL_INT[ng_j] << "\t" <<  PVAL_JOINT[ng_j] << "\n";
             //writefile << PVAL_MAIN[ng_j] <<  "\t" << PVAL_INT[ng_j] << "\t" <<  STAT_JOINT[ng_j] << "\n";
             ng_j++;
           }
         }
            npbidx = 0;
            snp_skip.zeros();
            G.reshape(n_obs, npb);   
        }
            if((m+1) % 100000 == 0) {writefile << flush;}        
      }
    
        if(end % 100000 != 0) {writefile << flush;}
        (void)ret;
        delete [] tmpout;
        delete [] snpID;
        delete [] rsID;
        delete [] chrStr;
        delete [] allele0;
        delete [] allele1;
        writefile.close();
        writefile.clear();
        libdeflate_free_decompressor(decompressor);
        fclose(fp); 
        return wrap(compute_time);
    } 
      catch( std::exception &ex ) {
         forward_exception_to_r( ex );
        } 
       catch(...) {
          ::Rf_error( "C++ exception (unknown reason)..." );
        }
     return R_NilValue;
}  
  
  SEXP glmm_gei_bgen11(SEXP dupe_flag, SEXP res_in, SEXP nullObj_in, SEXP bgenfile_in, SEXP outfile_in, SEXP center_in, SEXP minmaf_in, SEXP maxmaf_in, SEXP minmac_in, SEXP missrate_in, SEXP minrsq_in, SEXP miss_method_in, SEXP nperbatch_in, 
                       SEXP ei_in, SEXP qi_in, SEXP isNullP_in,  SEXP strata_in,
                       SEXP select_in, SEXP begin_in, SEXP end_in, SEXP pos_in, SEXP nbgen_in, SEXP compression_in, SEXP metaOutput_in) {
    try{
      
      bool isNullP = Rcpp::as<bool>(isNullP_in);
      int ei = Rcpp::as<int>(ei_in);
      int qi = Rcpp::as<int>(qi_in);
      bool isDupeID = Rcpp::as<bool>(dupe_flag);
      arma::mat P;
      arma::sp_mat Sigma_i;
      arma::sp_mat Sigma_iX;
      arma::sp_mat cov;
      arma::sp_mat J;
      
      Rcpp::List null_obj(nullObj_in);
      Rcpp::List strata_list(strata_in);
      int strataList_size = 0;
      bool skip_strata = true;

      if (strata_list[0]!=R_NilValue) {
        skip_strata = false;
        strataList_size = strata_list.size();
      }
      
      arma::mat E = as<arma::mat>(null_obj["E"]);
      arma::mat EC;
      
      if (!isNullP) {
        P = as<arma::mat>(null_obj["P"]);
        (void)Sigma_i; (void)Sigma_iX; (void)cov;        
      } else {
        Sigma_i = as<arma::sp_mat>(null_obj["Sigma_i"]);
        Sigma_iX = as<arma::sp_mat>(null_obj["Sigma_iX"]);
        cov = as<arma::sp_mat>(null_obj["cov"]);
        (void)P;
      }
      
      Rcpp::NumericVector res_r(res_in);
      arma::vec res(res_r.begin(), res_r.size(), false);
      const char center = Rcpp::as<char>(center_in);
      const double minmaf = Rcpp::as<double>(minmaf_in);
      const double maxmaf = Rcpp::as<double>(maxmaf_in);
      const double missrate = Rcpp::as<double>(missrate_in);
      const double minmac = Rcpp::as<double>(minmac_in);
      const double minrsq = Rcpp::as<double>(minrsq_in);
      const char miss_method = Rcpp::as<char>(miss_method_in);
      const bool metaOutput = Rcpp::as<bool>(metaOutput_in);
      string bgenfile = Rcpp::as<string>(bgenfile_in);
      string outfile  = Rcpp::as<string>(outfile_in);
      
      const size_t npb = Rcpp::as<size_t>(nperbatch_in);
      Rcpp::IntegerVector select(select_in);
      string line, snp;
      size_t n = res.n_elem;
      size_t n_obs = res.n_elem;
      if (!isDupeID)
      {
        J = as<arma::sp_mat>(null_obj["J"]);
	n = J.n_rows;
      }
 
      vec g(n);
      uvec gmiss(n), snp_skip = zeros<uvec>(npb);
      mat G(n_obs, npb);
      string* tmpout = new string[npb];
      vector <string> biminfo;
      double gmean, mac, gsqmean, geno, gmax, gmin, rsq;
      size_t ncount, nmiss, npbidx = 0;
      double compute_time = 0.0;
      ofstream writefile(outfile.c_str(), std::ofstream::binary);
      
      struct libdeflate_decompressor* decompressor = libdeflate_alloc_decompressor();
      
      uint maxLA = 65536;
      char* snpID   = new char[maxLA + 1];
      char* rsID    = new char[maxLA + 1];
      char* chrStr  = new char[maxLA + 1];
      char* allele1 = new char[maxLA + 1];
      char* allele0 = new char[maxLA + 1];
      uint Nbgen = Rcpp::as<uint>(nbgen_in);
      uint compression = Rcpp::as<uint>(compression_in);
      std::vector<uchar> zBuf11;
      std::vector<uint16_t> shortBuf11;
      uLongf destLen1 = 6 * Nbgen;
      zBuf11.resize(destLen1);
      shortBuf11.resize(destLen1);
      
      uint begin = Rcpp::as<uint>(begin_in);
      uint end   = Rcpp::as<uint>(end_in);
      long long unsigned int byte = Rcpp::as<long long unsigned int>(pos_in);
      FILE* fp = fopen(bgenfile.c_str(), "rb");
      fseek(fp, byte, SEEK_SET);
      
      int ret;
      mat strata_AF(end,strataList_size);
      mat strata_N(end,strataList_size);
      for (uint m = begin; m < end; m++) {
        stringstream writeout;        
        uint Nprob; ret = fread(&Nprob, 4, 1, fp); 
        if (Nprob != Nbgen) {
          Rcout << "Error reading bgen file: Number of samples with genotype probabilities does not match number of samples in BGEN file. \n"; return R_NilValue;
        }
        ushort LS; ret = fread(&LS, 2, 1, fp);
        ret = fread(snpID, 1, LS, fp); 
        snpID[LS] = '\0';
        string str_snpID = "NA";
        if (LS != 0) { str_snpID = string(snpID); }
        
        ushort LR; ret = fread(&LR, 2, 1, fp);
        ret = fread(rsID, 1, LR, fp);  
        rsID[LR] = '\0';
        
        ushort LC; ret = fread(&LC, 2, 1, fp);
        ret = fread(chrStr, 1, LC, fp); 
        chrStr[LC] = '\0';
        
        uint physpos; ret = fread(&physpos, 4, 1, fp);
        string physpos_tmp = to_string(physpos);
        
        uint32_t LA; 
        ret = fread(&LA, 4, 1, fp);
        ret = fread(allele1, 1, LA, fp);
        allele1[LA] = '\0';
        
        uint32_t LB; 
        ret = fread(&LB, 4, 1, fp);
        ret = fread(allele0, 1, LB, fp); 
        allele0[LB] = '\0';
                
        uint16_t* probs_start;
        if (compression == 1) {
          uint cLen; 
          ret = fread(&cLen, 4, 1, fp);
          ret = fread(&zBuf11[0], 1, cLen, fp);
                        
          if (libdeflate_zlib_decompress(decompressor, &zBuf11[0], cLen, &shortBuf11[0], destLen1, NULL) != LIBDEFLATE_SUCCESS) {
            Rcpp::stop("Decompressing " + string(rsID) + "genotype block failed with libdeflate.");
          }          
          probs_start = reinterpret_cast<uint16_t*>(&shortBuf11[0]);
        }
        else {
          ret = fread(&zBuf11[0], 1, destLen1, fp);
          probs_start = reinterpret_cast<uint16_t*>(&zBuf11[0]);
        }
        
        const double scale = 1.0 / 32768;        
        gmean=0.0;
	mac=0.0;
	gsqmean=0.0;
        gmax=-100.0;
        gmin=100.0;
	rsq=0.0;
        nmiss=0;
        ncount = 0;
        for (size_t i = 0; i < Nbgen; i++) {
          if (select[ncount] > 0) {            
            double p11 = probs_start[3 * i] * scale;
            double p10 = probs_start[3 * i + 1] * scale;
            double p00 = probs_start[3 * i + 2] * scale;
            if (p11 == 0.0 && p10 == 0.0 && p00 == 0.0){
              gmiss[select[ncount]-1] = 1;
              nmiss++;              
            } else {
              double pTot = p11 + p10 + p00;
              geno = (2 * p00 + p10) / pTot;
              gmiss[select[ncount]-1] = 0;
              g[select[ncount]-1] = geno;
              gmean += geno;
	      gsqmean += geno * geno;
              if (geno > gmax) { gmax = geno; }
              if (geno < gmin) { gmin = geno; }
            }             
          }
          ncount++;
        }
        
	mac = gmean;
	if((double)(n-nmiss)<mac) {
	  mac = (double)(n-nmiss) * 2.0 - mac;
	}
        gmean/=(double)(n-nmiss);
	gsqmean/=(double)(n-nmiss);
	rsq = (gsqmean - gmean * gmean) * (double)(n-nmiss) / (double)(n-nmiss-1) / (gmean * (1.0 - gmean/2.0));
     
           if (skip_strata) {
              writeout << str_snpID << "\t" << rsID << "\t" << chrStr << "\t" << physpos_tmp << "\t" << allele1 << "\t" << allele0 << "\t" << (n-nmiss) << "\t"<< gmean/2.0 << "\t" << mac << "\t" << rsq << "\t";
            } else {
                writeout << snpID << "\t" << rsID << "\t" << chrStr << "\t" << physpos_tmp << "\t" << allele1 << "\t" << allele0 << "\t" << (n-nmiss) << "\t" << gmean/2.0 << "\t" << mac << "\t" << rsq << "\t";
                std::vector<double> strata_range(strataList_size);
                for (int strata_idx = 0; strata_idx < strataList_size; strata_idx++) {
                      uvec strata_tmp = as<arma::uvec>(strata_list[strata_idx]);
                      uvec strata_gmiss = gmiss.elem(strata_tmp-1);
                      vec strata_g = g.elem(strata_tmp-1);
                      strata_AF(m,strata_idx) = mean(strata_g.elem(find(strata_gmiss == 0))) / 2.0;
                      vec tmp;
                      tmp= strata_g.elem(find(strata_gmiss == 0));
                      strata_N(m,strata_idx) =tmp.n_elem;            
                  }
                for (int strata_idx = 0; strata_idx < strataList_size; strata_idx++) {
                      writeout << strata_N(m,strata_idx) << "\t";
                      writeout << strata_AF(m,strata_idx) << "\t";
                }
              }         

           for (size_t j=0; j<n; ++j) {
            if (gmiss[j]==1) {
             g[j] = gmean;
             if (center=='n' && miss_method=='o') {g[j] = 0.0;} // remove missing genotypes
            }
            if (center=='c') {
             g[j] -= gmean;
            }
           }
          double AF= gmean / 2.0; // convert mean to allele freq
          if(((double)nmiss/n>missrate) || (mac<minmac) || (rsq<minrsq) || ((AF<minmaf || AF>maxmaf) && (AF<1-maxmaf || AF>1-minmaf))) { // monomorphic, missrate, MAF, MAC, Rsq
            snp_skip[npbidx] = 1;           
          } else {
	    if (!isDupeID){
	      G.col(npbidx) = J.t()*g;
	    } else {
	      G.col(npbidx) = g;
            }
	  }

        tmpout[npbidx] = writeout.str();
        writeout.clear();
        npbidx++;
                
        if((m+1 == end) || (npbidx == npb)) {          
          if (npbidx != npb) {
            G.reshape(n_obs, npbidx);
            snp_skip = snp_skip.rows(0,npbidx-1);
          }
          uvec snp_idx = find(snp_skip == 0);
          G = G.cols(snp_idx);
          int ng = G.n_cols; 
                    
          mat IV_U;
          mat IV_U1;
          mat IV_V_i;
          mat IV_V_i1;
          mat IV_E_i;
          mat IV_GE_i;
          mat STAT_JOINT_tmp;
          vec BETA_MAIN;
          vec STAT_INT;
          vec STAT_JOINT;
          vec SE_MAIN;
          mat BETA_INT;
          mat BETA_INT1;
          vec PVAL_MAIN(ng);
          PVAL_MAIN.fill(NA_REAL);
          vec PVAL_INT(ng);
          PVAL_INT.fill(NA_REAL);
          vec PVAL_JOINT(ng);
          PVAL_JOINT.fill(NA_REAL);
          size_t ngei1=ng*(ei+1);
  
          if (G.n_cols != 0) {
            mat K;
            for (int ei_idx = 0; ei_idx < ei+qi; ei_idx++) {
              mat tmpK = G.each_col() % E.col(ei_idx);
              K = join_horiz(K, tmpK);
            } 
            mat K1;
            mat O(E.n_rows,1,fill::ones);
            mat E1;
            E1=join_horiz(O,E);
            for (int ei_idx = 0; ei_idx < ei+qi+1; ei_idx++) {
              mat tmpK1 = G.each_col() % E1.col(ei_idx);
              K1 = join_horiz(K1, tmpK1);
            }

            vec U = G.t() * res;            
            sp_mat Gsp(G);
            sp_mat PG;
            if (!isNullP) {
              PG = P.t() * G;
            } else {
              sp_mat GSigma_iX =  Gsp.t() * Sigma_iX;
              PG = (Sigma_i.t() * Gsp) - (Sigma_iX * (GSigma_iX * cov.t()).t());
            }
            
            mat GPG = (G.t() * PG)  % kron(ones(1, 1), mat(ng, ng, fill::eye));
            mat GPG_i;
            bool is_non_singular = inv(GPG_i, GPG);
            if (!is_non_singular) {
              GPG_i = pinv(GPG);
            }
            
            mat V_i;
            V_i = diagvec(GPG_i);    
            vec V_MAIN_adj = diagvec(GPG_i);
            V_MAIN_adj     = V_MAIN_adj.rows(0, ng-1);            
            vec BETA_MAIN_adj = GPG_i.t() * U; 
            BETA_MAIN_adj     = BETA_MAIN_adj.rows(0, ng-1);           
            vec STAT_MAIN_adj(ng);
            STAT_MAIN_adj.fill(NA_REAL);
            for (size_t s = 0; s < V_MAIN_adj.size(); s++) {
              if (V_MAIN_adj[s] > 0) {
                STAT_MAIN_adj[s] = (BETA_MAIN_adj[s] * BETA_MAIN_adj[s]) / V_MAIN_adj[s];
              }
            } 
            
            BETA_MAIN = V_i % U.rows(0,ng-1);
            SE_MAIN = sqrt(V_i);
            vec STAT_MAIN = BETA_MAIN % U.rows(0,ng-1);
            for (size_t s = 0; s < STAT_MAIN.size(); s++) {
              if (STAT_MAIN[s] > 0) {
                PVAL_MAIN[s] = Rf_pchisq(STAT_MAIN[s], 1, 0, 0);
              }
            }
            
            mat KPK1;
            if (!isNullP) {
              KPK1 = K1.t() * (P.t() * K1);
            } else {
              mat KSigma_iX1 = K1.t() * Sigma_iX;
              KPK1 = (K1.t() * (Sigma_i.t() * K1)) - (KSigma_iX1 * (KSigma_iX1 * cov.t()).t());
            }

            KPK1 = KPK1 % kron(ones(ei+qi+1, ei+qi+1), mat(ng, ng, fill::eye));
            IV_V_i1=inv(KPK1);            
            mat cross_K_res = K1.t() *res;
            mat IV_U_kron1 = kron(ones(ei+qi+1,1), mat(ng, ng, fill::eye));
            IV_U1 = IV_U_kron1.each_col() % cross_K_res;
            BETA_INT1 = (IV_V_i1 * IV_U1);
            IV_E_i=inv(IV_V_i1(span(ng,ngei1-1),span(ng,ngei1-1))); 
            IV_U=IV_E_i*BETA_INT1.rows(ng,ngei1-1);
            STAT_INT=diagvec(IV_U.t()*BETA_INT1.rows(ng,ngei1-1));
            IV_GE_i=inv(IV_V_i1(span(0,ngei1-1),span(0,ngei1-1))); 
            STAT_JOINT_tmp=IV_GE_i*BETA_INT1.rows(0,ngei1-1);
            STAT_JOINT=diagvec(STAT_JOINT_tmp.t()*BETA_INT1.rows(0,ngei1-1));

            for (size_t s = 0; s < STAT_INT.size(); s++) {
                PVAL_INT[s] = Rf_pchisq(STAT_INT[s], ei, 0, 0);
                //if (is_finite(PVAL_MAIN[s])) {
                  PVAL_JOINT[s] = Rf_pchisq(STAT_JOINT[s], 1+ei, 0, 0);
                //}
              }
              
            } 
          
          uvec b_idx = regspace<uvec>(0, ng, (ei+qi-1) * ng);
          uvec b_idx1 = regspace<uvec>(0, ng, (ei+qi) * ng);
          int ng_j = 0;
          for(size_t j=0; j<npbidx; ++j) {
            if(snp_skip[j] == 1) { // monomorphic, missrate, MAF
              writefile << tmpout[j] << "NA\tNA\tNA\tNA\tNA\tNA\tNA";
              if (metaOutput) {
               for (int e=0; e < (ei+qi + ei+qi + ((ei+qi) * (ei+qi + 1) / 2)); e++) {
		 //if(e == 0) {
		 //writefile << "NA";
		 //} else {
		   writefile << "\tNA";
		   //}
               } 
              }
              writefile << "\n";
            } 
            else {
              writefile << tmpout[j] << BETA_MAIN[ng_j] << "\t" << SE_MAIN[ng_j] << "\t";
              if (metaOutput) {
               // Beta Int the diaganol of BETA_INT1
               for (int b=0; b < ei+qi+1; b++) {
                 int row = b_idx1[b] + ng_j ;
                 writefile << BETA_INT1(row, ng_j ) << "\t";                 
                }
      
               // Var (the diaganol of IV_V_i1)                
               for (int b=0; b < ei+qi+1; b++) {
                 int col = b_idx1[b] + ng_j ;
                 for (int d=0; d < ei+qi+1; d++) {
                   if(b == d) {
                     int row = b_idx1[d] + ng_j ;                      
                     writefile << sqrt(IV_V_i1(row, col)) << "\t";                     
                    } 
                  }
                }
       
               // Cov (the lower triangle elements of IV_V_i1)
               for (int b=0; b < ei+qi+1; b++) {
                 int col = b_idx1[b] + ng_j ;
                 for (int d=0; d < ei+qi+1; d++) {
                   if(d > b) {
                     int row = b_idx1[d] + ng_j ;
                     writefile << IV_V_i1(row, col) << "\t";
                    }
                  }
                }            
             }
             else {
               //generate a squre matrix with elements 1 to E1.col*E1.col, for example E1.col=2, matrix [1,3;2,4] 
               //E1.col=3, matrix [1,4,7;2,5,8;3,6,9]
               mat O(E.n_rows,1,fill::ones);
               mat E1;
               E1=join_horiz(O,E);
               int ncolE=E1.n_cols;
               mat split_mat(ncolE,ncolE);
               for (int i=0; i<ncolE; i++) {
                 for (int j=0; j<ncolE; j++)
                 split_mat(i,j)=i+1-ncolE+ncolE*(j+1);
                }
                split_mat=split_mat(span(1,ei),span(1,ei));
                if ( split_mat.n_elem ==1){
               // Beta Int
                 for (int b=0; b < ei+1; b++) {
                   int row = b_idx1[b] + ng_j ;
                   //NOT the first ng row 
                   if (row>ng-1) {
                      writefile << BETA_INT1(row, ng_j ) << "\t";
                    }
                 }

              // Var (the diaganol elements)                
                 for (int b=0; b < ei+1; b++) {
                     int col = b_idx1[b] + ng_j ;
                     for (int d=0; d < ei+1; d++) {
                       if(b == d ) {
                       int row = b_idx1[d] + ng_j ; 
                       //NOT the first ng row or first ng col
                       if (row > ng-1  && col >ng-1) {
                        writefile << sqrt(IV_V_i1(row, col)) << "\t";
                       }
                     }
                   }
                }
            }
               else {
                        // Beta Int
                       for (int b=0; b < ei+1; b++) {
                          int row = b_idx1[b] + ng_j ;
                   //NOT the first ng row 
                           if (row>ng-1) {
                               writefile << BETA_INT1(row, ng_j ) << "\t";
                            }
                        }
                        for (int b=0; b < ei+1; b++) {
                            int col = b_idx1[b] + ng_j ;
                            for (int d=0; d < ei+1; d++) {
                               if(b == d ) {
                                  int row = b_idx1[d] + ng_j ; 
                       //NOT the first ng row or first ng col
                                   if (row > ng-1  && col >ng-1) {
                                     writefile << sqrt(IV_V_i1(row, col)) << "\t";
                                    }
                                }
                            }
                        }
                        // Cov (the lower triangle )
                         for (int b=0; b < ei+1; b++) {
                               int col = b_idx1[b] + ng_j ;
                                for (int d=0; d < ei+1; d++) {
                                   if(d > b) {
                                      int row = b_idx1[d] + ng_j ;
                                       if (row > ng-1  && col >ng-1) {
                                         writefile << IV_V_i1(row, col) << "\t";
                                       }
                                    }
                                }
                           }
                }
             }
              writefile << PVAL_MAIN[ng_j] << "\t"  << PVAL_INT[ng_j] << "\t" <<  PVAL_JOINT[ng_j] << "\n";
              ng_j++;
            }
        }
          
          npbidx = 0; 
          snp_skip.zeros();
          G.reshape(n_obs, npb);
          
        }
        if((m+1) % 100000 == 0) {writefile << flush;}
    }
  
      if(end % 100000 != 0) {writefile << flush;}
      (void)ret;
      delete [] tmpout;
      delete [] snpID;
      delete [] rsID;
      delete [] chrStr;
      delete [] allele0;
      delete [] allele1;
      writefile.close();
      writefile.clear();
      libdeflate_free_decompressor(decompressor);
      fclose(fp);
      return wrap(compute_time);
    } 
    catch( std::exception &ex ) {
      forward_exception_to_r( ex );
    } catch(...) {
      ::Rf_error( "C++ exception (unknown reason)..." );
    }
    return R_NilValue;
  } 
} // end of extern "C"
