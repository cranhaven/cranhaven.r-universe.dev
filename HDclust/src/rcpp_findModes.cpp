/*==========================================================================================*/
  /*                                                                                          */
  /* Copyright (C) [Dec 2015]-[April 2017] Jia Li, Department of Statistics,                  */
  /* The Pennsylvania State University, USA - All Rights Reserved                             */
  /*                                                                                          */
  /* Unauthorized copying of this file, via any medium is strictly prohibited                 */
  /*                                                                                          */
  /* Proprietary and CONFIDENTIAL                                                             */
  /*                                                                                          */
  /* NOTICE: All information contained herein is, and remains the property of The             */
  /* Pennsylvania State University. The intellectual and technical concepts                   */
  /* contained herein are proprietary to The Pennsylvania State University and may            */
  /* be covered by U.S. and Foreign Patents, patents in process, and are protected            */
  /* by trade secret or copyright law. Dissemination of this information or                   */
  /* reproduction of this material is strictly forbidden unless prior written                 */
  /* permission is obtained from Jia Li at The Pennsylvania State University. If              */
  /* you obtained this code from other sources, please write to Jia Li.                       */
  /*                                                                                          */
  /*                                                                                          */
  /* The software is a part of the package for                                                */
  /* Clustering with Hidden Markov Models on Variable Blocks                                  */
  /*                                                                                          */
  /* Written by Jia Li <jiali@stat.psu.edu>, April 7, 2017                                    */ 
  /*                                                                                          */
  /*==========================================================================================*/
  #include <Rcpp.h>
  using namespace Rcpp;
  #include "utils_rcpp.h"
  #include "hmm.h"
  
  #ifdef _OPENMP
  #include <omp.h>
  #endif
  
  #include <string.h>
  
  
  // [[Rcpp::plugins(openmp)]]

using namespace Rcpp;


// [[Rcpp::export]]
S4 rcpp_findModes(NumericMatrix dataTranspose, S4 HmmVb, IntegerVector nthread)
{
  //double *wt=NULL;
  
  /*----------------------------------------------------------------*/
    /*---------------- Parse input data ------------------------------*/
    /*----------------------------------------------------------------*/
  int nseq = dataTranspose.ncol(); // num datapoints
  int dim = dataTranspose.nrow(); // data dimensionality
  
  // print parsed data
  //Rcout << "nseq = " << nseq << "\n";
  //Rcout << "dim = " << dim << "\n";
  
  double **u; // 2d representation of the data. Each row - new datapoint

  if (nseq<0||nseq>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  u = (double **)R_Calloc((size_t)nseq,double *);
  
  for (int i = 0; i < nseq; i++){
    u[i] = dataTranspose.begin() + i*dim;
  }
  
  /*----------------------------------------------------------------*/
    /*---------------- Parse model -----------------------------------*/
    /*----------------------------------------------------------------*/
  S4 VbStructure = HmmVb.slot("VbStructure");
  DIAGCOV = as<int>(HmmVb.slot("diagCov"));
  
  /*----------------------------------------------------------------*/
    /*---------------- Parse variable block structure ----------------*/
    /*----------------------------------------------------------------*/
    CondChain *md=NULL;
  
  if (!Rf_isNull(VbStructure)){
    md=(CondChain *)R_Calloc(1,CondChain);
    
    parseVbStructure(VbStructure, md);
    
    //~ Rcout << "nb = " << md->nb << "\n";
    //~ 
      //~ Rcout << "bdim:\n";
    //~ for (int i = 0; i < md->nb; i++)
      //~ Rcout << md->bdim[i] << " ";
    //~ Rcout << "\n";
    //~ 
      //~ Rcout << "numst:\n";
    //~ for (int i = 0; i < md->nb; i++)
      //~ Rcout << md->numst[i] << " ";
    //~ Rcout << "\n";
    //~ 
      //~ Rcout << "var:\n";
    //~ for (int i = 0; i < md->nb; i++){
      //~ for (int j = 0; j < md->bdim[i]; j++)
        //~ Rcout << md->var[i][j] << " ";
      //~ Rcout << "\n";
      //~}
  } else {
    Rcout << "VbStructure is NULL!\n";
    return S4();  
  }
  
  /*----------------------------------------------------------------*/
    /*---------------- Parse HMM chain -------------------------------*/
    /*----------------------------------------------------------------*/
    List HmmChain = HmmVb.slot("HmmChain");
  
  if (!Rf_isNull(HmmChain)){
    if (static_cast<int>(HmmChain.size()) != md->nb){
      Rcout << "number of Hmm models in HmmChain doesn't match with nb!\n";
      return S4();  
    }
    
    parseHmmChain(HmmChain, md);
    
  } else {
    Rcout << "HmmChain is NULL!\n";
    return S4();  
  }
  
  /*----------------------------------------------------------------*/
    /*----------------- Estimate Viterbi paths  ---------------------------------*/
    /*----------------------------------------------------------------*/
  ordervar(u,nseq,md->nb,md->bdim,md->var);
  
  int **optst;
  if (nseq<0 || nseq>SIZE_MAX){
    Rcpp::stop("Error in memory allocation, negative or too large size\n");
    
  }
  if (nseq<0||nseq>SIZE_MAX|| md->nb < 0 || md->nb > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  optst=(int **)R_Calloc((size_t)nseq,int *);
  for (int i=0;i<nseq;i++) optst[i]=(int *)R_Calloc((size_t)md->nb,int);
  
  
  #ifdef _OPENMP
  int num_threads = as<int>(nthread);
  omp_set_num_threads(num_threads);
  
  if (num_threads > 1)
    Rcout << "Number of threads used: " << num_threads << std::endl;
  #else
  int num_threads = as<int>(nthread);
  
  if (num_threads > 1)
    Rcout << "More than one thread is used with OpenMP not configured. Make sure your compiler supports OpenMP and reinstall the package" << std::endl;
  #endif
  
  #pragma omp parallel
  {
    if (md->maxnumst<0||md->maxnumst>SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    }
    double *meritbuf=(double *)R_Calloc((size_t)md->maxnumst,double);
    
    #pragma omp for
    for (int i=0;i<nseq;i++)
      viterbi(md,u[i],optst[i],NULL,meritbuf);
    
    R_Free(meritbuf);
  }
  
  
  
  //---------------------------------------------------------//
    //----------- Find unique Viterbi sequences ----------------//
    //---------------------------------------------------------//
    
  int *clsid, *ct;

  if (nseq<0 || nseq>SIZE_MAX){
    Rcpp::stop("Error in memory allocation, negative or too large size\n");
  }
  clsid=(int *)R_Calloc((size_t)nseq,int);
  
  int **newoptst, newnseq, *vseqid;

  FindDifSeq(optst, nseq, md->nb, &newoptst, &newnseq, clsid);
  if (newnseq<0 || newnseq>SIZE_MAX){
    Rcpp::stop("Error in memory allocation, negative or too large size\n");
  }  
  vseqid=(int *)R_Calloc((size_t)newnseq,int);
  
  for (int i=0; i<newnseq; i++) vseqid[i] = i;
  
  
  //=============================//
  // Compute modes               //
  //=============================//
  CompMode *cpm;
  cpm=(CompMode *)R_Calloc((size_t)newnseq,CompMode);
  
  double **mode = (double **)R_Calloc((size_t)newnseq,double *);
  
  for (int i=0;i<newnseq;i++) {
    SetCompMode(cpm+i,newoptst[i], md);
    cpm[i].logpdf=bwmem(md, cpm[i].mu, cpm[i].mode);
    mode[i]=cpm[i].mode;
  }

  double *sigmadat;
  if (dim<0 || dim>SIZE_MAX){
    Rcpp::stop("Error in memory allocation, negative or too large size\n");
  }
  sigmadat=(double *)R_Calloc((size_t)dim,double);

  //The overall deviation
  DataSigma(u,sigmadat,dim,nseq);

  //--------------------------------------------------------------------//
  //Second round alignment with reference based on newly computed modes //
  //--------------------------------------------------------------------//
    
  List newClust = wrapClust(mode, sigmadat, md->nb, dim, 
                            newnseq, newnseq, newoptst, vseqid);
 
    
  ct=(int *)R_Calloc((size_t)newnseq,int);
  for (int i=0;i<newnseq;i++) ct[i]=0;
  for (int i=0;i<nseq;i++) ct[clsid[i]]++;
  
  S4 HmmVbClust("HMMVBclust");
  
  HmmVbClust.slot("clustParam") = newClust;
  HmmVbClust.slot("clsid") = IntegerVector(clsid, clsid + nseq);
  HmmVbClust.slot("size") = IntegerVector(ct, ct + newnseq);
  
  // free memory
  for (int i=0; i<newnseq; i++) R_Free(newoptst[i]);
  R_Free(newoptst);
  
  R_Free(vseqid);
  R_Free(clsid);
  R_Free(ct);
  R_Free(sigmadat);
  
  R_Free(mode);
  for (int i=0; i<newnseq; i++) freeCompMode(cpm+i);
  R_Free(cpm);
  
  freeccm(&md);
  R_Free(md);  
  R_Free(u);
    
  return HmmVbClust;
  }
  
  
  
  
