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
#include "utils_rcpp.h"
#include "hmm.h"


#ifdef _OPENMP
	#include <omp.h>
#endif

#include <string.h>


// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;

// List wrapClust(double **mode, double *sigma, int nb, int dim,
// 				int ncls, int ndseq, int **path, int *seqcls)
// {
// 	NumericMatrix vmode(ncls, dim);
// 
//     for (int i = 0; i < ncls; i++)
// 		std::copy(mode[i], mode[i] + dim, vmode(i,_).begin());
// 
// 	List vseq(ndseq);
// 
// 	for (int i = 0; i < ndseq; i++)
// 		vseq[i] = IntegerVector(path[i], path[i] + nb);
// 
// 
// 	return List::create(Named("ncls") = ncls,
//                         Named("ndseq") = ndseq,
//                         Named("vseqid") = IntegerVector(seqcls, seqcls + ndseq),
//                         Named("sigma") = NumericVector(sigma, sigma + dim),
//                         Named("mode") = vmode,
//                         Named("vseq") = vseq);
// }
// 
// void freeClust(double **mode, double *sigma,
// 				int ncls, int ndseq, int **path, int *cls)
// {
// 	R_Free(cls);
// 	R_Free(sigma);
// 
// 	for (int i = 0; i < ncls; i++) R_Free(mode[i]);
// 	R_Free(mode);
// 
// 	for (int i = 0; i < ndseq; i++) R_Free(path[i]);
// 	R_Free(path);
// }

void parseRefClust(const List &rfsClust,  double ***refmode, double **refsigma,
				int *rncls, int *rnseq, int ***refpath, int **refcls)
{
	*rncls = rfsClust["ncls"];
	*rnseq = rfsClust["ndseq"];
	
	IntegerVector rvseqid = rfsClust["vseqid"];
	if (rvseqid.size()<0||rvseqid.size()>SIZE_MAX) {
	  Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
	}
	*refcls = (int *)R_Calloc((size_t)rvseqid.size(), int);
	std::copy(rvseqid.begin(), rvseqid.end(), *refcls);
	
	NumericMatrix rmode = rfsClust["mode"];
	if (rmode.nrow()<0||rmode.nrow()>SIZE_MAX) {
	  Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
	}
	*refmode = (double **)R_Calloc((size_t)rmode.nrow(), double *);
	for (int i = 0; i < rmode.nrow(); i++){
	  if (rmode.ncol()<0||rmode.ncol()>SIZE_MAX) {
	    Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
	  }
	  (*refmode)[i] = (double *)R_Calloc((size_t)rmode.ncol(), double);
	  std::copy(rmode(i,_).begin(), rmode(i,_).end(), (*refmode)[i]);
	}
	
	
	std::vector<IntegerVector> rvseq = rfsClust["vseq"];
	if (*rnseq<0||*rnseq>SIZE_MAX) {
	  Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
	}
	*refpath = (int **)R_Calloc((size_t)*rnseq, int *);
	for (int i = 0; i < *rnseq; i++){
		(*refpath)[i] = (int *)R_Calloc((size_t)rvseq.size(), int);
		std::copy(rvseq[i].begin(), rvseq[i].end(), (*refpath)[i]);
	}
	
	NumericVector rsigma = rfsClust["sigma"];
	if (rsigma.size()<0||rsigma.size()>SIZE_MAX) {
	  Rcpp::stop("Error in memory allocation, negative or too large size.\n");
	  
	}
	*refsigma = (double *)R_Calloc((size_t)rsigma.size(), double);
	std::copy(rsigma.begin(), rsigma.end(), *refsigma);
}

// void parseVbStructure(const S4 &VbStructure, CondChain *md)
// {	  
// 	md->nb = VbStructure.slot("nb");
// 	
// 	md->bdim = (int *)R_Calloc((size_t)md->nb,int);  
// 	md->numst = (int *)R_Calloc((size_t)md->nb,int);
// 	
// 	IntegerVector bstrBDim = VbStructure.slot("bdim");
// 	IntegerVector bstrNumStates = VbStructure.slot("numst");
// 	
// 	std::copy(bstrBDim.begin(), bstrBDim.end(), md->bdim);
// 	std::copy(bstrNumStates.begin(), bstrNumStates.end(), md->numst);
//     
// 	int dim = 0;
// 	for (int i = 0; i < md->nb; i++) dim += md->bdim[i];
// 	md->dim = dim;
// 	
// 	int maxnumst = 0;
// 	for (int i = 0; i < md->nb; i++)
// 		if (md->numst[i] > maxnumst) maxnumst = md->numst[i];
// 	  
// 	md->maxnumst = maxnumst;
// 
//     std::vector<IntegerVector> bstrVarOrder = VbStructure.slot("varorder");
// 	
//     md->var = (int **)R_Calloc((size_t)md->nb,int *);
// 	for (int i=0; i < md->nb; i++){
// 		md->var[i] = (int *)R_Calloc((size_t)md->bdim[i],int);
// 		std::copy(bstrVarOrder[i].begin(), bstrVarOrder[i].end(), md->var[i]);
// 		
// 		for (int j = 0; j < md->bdim[i]; j++)
// 			md->var[i][j]--;
// 	}
// 
//     // Set up the redundant fields for convenience of computation
// 	md->cbdim=(int *)R_Calloc((size_t)md->nb,int);
// 	md->cbdim[0]=0;
// 	md->cnumst=(int *)R_Calloc((size_t)md->nb,int);
// 	md->cnumst[0]=0;
// 	for (int i = 0; i < md->nb-1; i++) {
// 		md->cbdim[i+1] = md->cbdim[i] + md->bdim[i];
// 		md->cnumst[i+1] = md->cnumst[i] + md->numst[i];
//     }
// }
// 
// void parseHmmChain(const List &HmmChain, CondChain *md)
// {
// 	md->mds=(HmmModel **)R_Calloc((size_t)md->nb,HmmModel *);
//     for (int i = 0; i < md->nb; i++) {
// 		md->mds[i]=(HmmModel *)R_Calloc(1,HmmModel);
// 		
// 		S4 Hmm = HmmChain[i];
//     
// 		md->mds[i]->dim = Hmm.slot("dim");
// 		md->mds[i]->numst = Hmm.slot("numst");
// 		md->mds[i]->prenumst = Hmm.slot("prenumst");
//   
// 		NumericVector a00 = Hmm.slot("a00");
// 		
// 		md->mds[i]->a00 = (double *)R_Calloc((size_t)md->mds[i]->numst,double);
// 		std::copy(a00.begin(), a00.end(), md->mds[i]->a00);
//   
// 		NumericMatrix a = Hmm.slot("a");
// 		
// 		md->mds[i]->a = (double **)R_Calloc((size_t)md->mds[i]->prenumst,double *);
// 		
// 		for (int j = 0; j < md->mds[i]->prenumst; j++){
// 			md->mds[i]->a[j] = (double *)R_Calloc((size_t)md->mds[i]->numst,double);
// 			std::copy(a(j,_).begin(), a(j,_).end(), md->mds[i]->a[j]);
// 		}
// 		
// 		NumericMatrix mean = Hmm.slot("mean");
// 		std::vector<NumericMatrix> sigmaList = Hmm.slot("sigma");
// 		std::vector<NumericMatrix> sigmaInvList = Hmm.slot("sigmaInv");
// 		NumericVector sigmaDetLog = Hmm.slot("sigmaDetLog");
//   
// 		
// 		md->mds[i]->stpdf=(GaussModel **)R_Calloc((size_t)md->mds[i]->numst, GaussModel *);
// 		
// 		for (int j = 0; j < md->mds[i]->numst; j++){
// 			md->mds[i]->stpdf[j]=(GaussModel *)R_Calloc(1, GaussModel);
// 
// 			md->mds[i]->stpdf[j]->exist = 1;
// 			md->mds[i]->stpdf[j]->dim = Hmm.slot("dim");
// 			
// 			md->mds[i]->stpdf[j]->mean = (double *)R_Calloc((size_t)md->mds[i]->stpdf[j]->dim,double);
// 			std::copy(mean(j,_).begin(), mean(j,_).end(), md->mds[i]->stpdf[j]->mean);
// 			
// 			md->mds[i]->stpdf[j]->sigma_det_log = sigmaDetLog[j];
// 			
// 			md->mds[i]->stpdf[j]->sigma = (double **)R_Calloc((size_t)md->mds[i]->stpdf[j]->dim,double *);
// 			
// 			for (int k =0; k<md->mds[i]->stpdf[j]->dim; k++){
// 				md->mds[i]->stpdf[j]->sigma[k] = (double *)R_Calloc((size_t)md->mds[i]->stpdf[j]->dim,double);
// 				std::copy((sigmaList[j])(k,_).begin(), (sigmaList[j])(k,_).end(), md->mds[i]->stpdf[j]->sigma[k]);
// 			}
// 			
// 			md->mds[i]->stpdf[j]->sigma_inv = (double **)R_Calloc((size_t)md->mds[i]->stpdf[j]->dim,double *);
// 			
// 			for (int k = 0; k < md->mds[i]->stpdf[j]->dim; k++){
// 				md->mds[i]->stpdf[j]->sigma_inv[k] = (double *)R_Calloc((size_t)md->mds[i]->stpdf[j]->dim,double);
// 				std::copy((sigmaInvList[j])(k,_).begin(), (sigmaInvList[j])(k,_).end(), md->mds[i]->stpdf[j]->sigma_inv[k]);
// 			}
// 		}
// 	}    
// }

List augmentrefcls(int rncls, int rnseq, double **refmode, double *sigmadat,
		   int **refpath, int *refcls, int dim, int nb, int numcls, int *clsid, int *id3,
		   double **mode, int newnseq, int **newoptst, int *clsid2, int *newid, int nseq, 
		   int *id2, double ***combmode_pt)
{
  int i,j,k;
  double **newrefmode=NULL, **newrefmode2, **combmode;
  int *ct, rncls2, rnseq2, **newrefpath, *newrefcls, *newrefcls2;

  if (numcls<0|| numcls>SIZE_MAX || dim<0|| dim>SIZE_MAX ) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    
  }
  if (numcls>0 && id3!=NULL && mode!=NULL) {
    ct=(int *)R_Calloc((size_t)numcls,int);
    for (i=0;i<numcls;i++) ct[i]=0;
    newrefmode=(double **)R_Calloc((size_t)numcls,double *);
    for (i=0;i<numcls;i++) {
      newrefmode[i]=(double *)R_Calloc((size_t)dim,double);
      for (j=0;j<dim;j++) newrefmode[i][j]=0.0;
    }
    newrefmode2=(double **)R_Calloc((size_t)numcls,double *);
    for (i=0;i<numcls;i++) {
      newrefmode2[i]=(double *)R_Calloc((size_t)dim,double);
      for (j=0;j<dim;j++) newrefmode2[i][j]=0.0;
    }

    //Computes the average first, may argue using average directly
    for (i=0;i<nseq;i++) {
      if (clsid[i]>=rncls) {
	k=id3[newid[id2[i]]];
	ct[clsid[i]-rncls]++; //number of times new cluster appears
      for (j=0;j<dim;j++) newrefmode[clsid[i]-rncls][j]+=mode[k][j];
      }
    }
    
    for (i=0;i<numcls;i++) {
      if (ct[i]>0) {
	for (j=0;j<dim;j++) {
	  newrefmode[i][j]/=(double)ct[i];
	  newrefmode2[i][j]=newrefmode[i][j];
	}
      }
    }
    //Find minimum and maximum per dimension and per cluster
    for (i=0;i<nseq;i++) {
      if (clsid[i]>=rncls) {
	k=id3[newid[id2[i]]];
	for (j=0;j<dim;j++) {
	  if (mode[k][j]> newrefmode[clsid[i]-rncls][j])
	    newrefmode[clsid[i]-rncls][j]=mode[k][j]; //store maximum
	  if (mode[k][j]< newrefmode2[clsid[i]-rncls][j])
	    newrefmode2[clsid[i]-rncls][j]=mode[k][j]; //store minimum
	}
      }
    }
    
    for (i=0;i<numcls;i++) {
      if (ct[i]>0) //use the midpoint between the maximum and the minimum
	for (j=0;j<dim;j++) newrefmode[i][j]=0.5*(newrefmode[i][j]+newrefmode2[i][j]);
    }
    
    R_Free(ct);
    for (i=0;i<numcls;i++) R_Free(newrefmode2[i]);
    R_Free(newrefmode2);

  } else {//No new modes, only new paths
    numcls=0;
  }
  
  rncls2=rncls+numcls;
  rnseq2=rnseq+newnseq;

  if (rnseq2<0||rnseq2>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    
  }
  newrefpath=(int **)R_Calloc((size_t)rnseq2,int *);
  for (i=0;i<rnseq;i++) newrefpath[i]=refpath[i];
  for (i=rnseq;i<rnseq+newnseq;i++) newrefpath[i]=newoptst[i-rnseq];

  newrefcls=(int *)R_Calloc((size_t)rnseq2,int);
  newrefcls2=(int *)R_Calloc((size_t)rnseq2,int); //for use as sorted newrefcls[]
  for (i=0;i<rnseq;i++) newrefcls[i]=refcls[i];
  for (i=rnseq;i<rnseq+newnseq;i++) newrefcls[i]=clsid2[i-rnseq];

  if (numcls>0) {
    if (numcls+rncls<0||numcls+rncls>SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n");
      
    }
    combmode=(double **)R_Calloc((size_t)numcls+rncls,double *);
    for (i=0;i<rncls;i++) combmode[i]=refmode[i];
    for (i=0;i<numcls;i++) combmode[rncls+i]=newrefmode[i];
  }
  else {
    combmode=refmode;
  }
  
  int **bufoptst, *invid;
  if (rnseq2<0||rnseq2>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    
  }
  bufoptst=(int **)R_Calloc((size_t)rnseq2,int *);
  invid=(int *)R_Calloc((size_t)rnseq2,int);
  
  SortLexigraphicInt(newrefpath, bufoptst,invid,nb,rnseq2);
  
  for (i=0;i<rnseq+newnseq;i++) newrefcls2[i]=newrefcls[invid[i]];

  //int *ct2;
  //ct2=(int *)R_Calloc((size_t)rncls2,int);
  //for (i=0;i<rncls2;i++) ct2[i]=0;
  //for (i=0;i<nseq;i++) ct2[clsid[i]]++; //#occurrences for each cluster in this dataset

  //if (clsfile!=NULL)
    //printrefcls(clsfile, rncls2, rnseq2, combmode, sigmadat, bufoptst, newrefcls2, dim, nb, nseq,ct2);

  //R_Free(ct2);
  List newClust = wrapClust(combmode, sigmadat, nb, dim, 
				rncls2, rnseq2, bufoptst, newrefcls2);
  
  R_Free(newrefpath);
  R_Free(newrefcls);
  R_Free(newrefcls2);
  R_Free(bufoptst);
  R_Free(invid);
  //the memory of newrefmode[i] should NOT be released, taken by combmode
  if (newrefmode!=NULL) R_Free(newrefmode); 
  
  *combmode_pt=combmode;
  
  
  return newClust;
}

//Switch cluster label of very small clusters to the big one closest to the data point
int AdjustCluster(int *clsid, int nseq, int rncls, double **refmode, double **u, int dim, int mincls)
{
  int i,j,k,m,n;
  int *ct;
  double db1,db2;

  if (rncls<0||rncls>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    
  }
  ct=(int *)R_Calloc((size_t)rncls,int);
  for (i=0;i<rncls;i++) ct[i]=0;
  for (i=0;i<nseq;i++) ct[clsid[i]]++;
  for (i=0,k=-1,m=0,n=0;i<rncls;i++) {
    if (ct[i]>=mincls) {
      m++;
      n+=ct[i];
      if (k<0) k=i; //get the first big cluster id
    }
  }
  if (k<0) {
    Rcout << "Minimum cluster size is too big: No cluster has size >=" << mincls << "\n";
    return -1;
  }
  else {
    Rcout << "Data size: " << nseq <<" number of large clusters: " << m << " #points in large clusters: " << n << "\n";
    db1=(double)n/(double)nseq;
    if (db1<0.8)
      Rcout << "Warning: percentage of points in large clusters is small: %" << db1*100 << " < 80 percent\n";
  }

  for (i=0;i<nseq;i++) {
    if (ct[clsid[i]]<mincls) {
      m=k;
      db1=l2sq(refmode[k],u[i],dim);
      for (j=k+1;j<rncls;j++) {
	if (ct[j]>=mincls) {
	  db2=l2sq(refmode[j],u[i],dim);
	  if (db2<db1) {
	    db1=db2;
	    m=j;
	  }
	}
      }
      clsid[i]=m;
    }
  }

  R_Free(ct);
  return 0;
}


// [[Rcpp::export]]
S4 rcpp_clust(NumericMatrix dataTranspose, S4 HmmVb, Nullable<List> rfsClust_, List control, IntegerVector nthread)
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
  
  /*-----------------------------------------------------------------*/
  /*---------------- Parse clustering parameters --------------------*/
  /*-----------------------------------------------------------------*/
  float modethreshold;
  int mincls;
  int usemeandist=0;
  int get_likeld=0;
  
  
  if (!Rf_isNull(control)){
	mincls = control["minSize"];
	modethreshold = control["modeTh"];
	usemeandist = as<int>(control["useL1norm"]);
    get_likeld = as<int>(control["getlikelh"]);
	
	//~ Rcout << "mincls = " << mincls << "\n";
	//~ Rcout << "modethreshold = " << modethreshold << "\n";
	//~ Rcout << "usemeandist = " << usemeandist << "\n";
	
  } else {
	Rcout << "control is NULL!\n";
	return S4();  
  }
  
  /*----------------------------------------------------------------*/
  /*--------------- Parse existing cluster information -------------*/
  /*----------------------------------------------------------------*/

  double **refmode, *refsigma=NULL;
  int rncls=0, rnseq=0, **refpath, *refcls;
  
  List rfsClust;
 
  if (!Rf_isNull(rfsClust_)) {
	 
	 rfsClust = rfsClust_.get();
	 
	 parseRefClust(rfsClust,  &refmode, &refsigma,
				    &rncls, &rnseq, &refpath, &refcls);
  }

  /*----------------------------------------------------------------*/
  /*----------------- Estimate HMM  ---------------------------------*/
  /*----------------------------------------------------------------*/
  ordervar(u,nseq,md->nb,md->bdim,md->var);
    
  double *loglikehd;

  if (nseq<0||nseq>SIZE_MAX || md->nb<0 || md->nb>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
  }
  loglikehd=(double *)R_Calloc((size_t)nseq,double);
    
  int **optst;
  optst=(int **)R_Calloc((size_t)nseq,int *);
  if (md->nb < 0||md->nb > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    
  }
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
    if (md->maxnumst<0 || md->maxnumst >SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
    }
    if (md->maxnumst <0||md->maxnumst >SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
    }
    double *meritbuf=(double *)R_Calloc((size_t)md->maxnumst,double);
 
	#pragma omp for
	for (int i=0;i<nseq;i++)
		viterbi(md,u[i],optst[i],NULL,meritbuf);
    
    R_Free(meritbuf);
  }
    
  NumericVector loglh(nseq);
  if(get_likeld==1){
      comploglike(md,u,nseq,NULL,loglikehd);
      // output loglikelihood
      for (int i=0;i<nseq;i++){
          loglh[i]=loglikehd[i];
      }
  }
  
  
  //optst[nseq][md->nb] stores the viterbi optimal state sequence for each instance
  //Output the path information
  /****
  fprintf(stdout, "Optimal path of states for every instance:\n");
  for (i=0;i<nseq;i+=100) {
    for (j=0;j<md->nb;j++)
      fprintf(stdout, "%d ", optst[i][j]);
    fprintf(stdout, "\n");
  }
  ****/

  //fprintf(stderr, "Viterbi paths found\n");
  
  //---------------------------------------------------------//
  //----------- Check with reference cluster ----------------//
  //---------------------------------------------------------//
  
  int *clsid, noref, *ct;

  if (nseq<0||nseq>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n"); 
  }
  clsid=(int *)R_Calloc((size_t)nseq,int);
  for (int i=0;i<nseq;i++) clsid[i]=-1;
  if (!Rf_isNull(rfsClust_)) {
    noref=0;
    for (int i=0;i<nseq;i++) {
      clsid[i]=FindEntry(refpath, optst[i],md->nb,rnseq); // find if best viterbi sequence matches sequences in reference cluster
      if (clsid[i]>=0) {
		clsid[i]=refcls[clsid[i]];
      }
      else {
	noref++;	
      }
    }
  } else {noref=nseq;}

  if (noref==0) { //all done
	
	
	//printrefcls(clsfile, rncls, rnseq, refmode, refsigma, refpath, refcls, dim, md->nb, nseq, rct);

    if (mincls>1) {
      if (AdjustCluster(clsid, nseq,rncls,refmode,u, dim, mincls) < 0){
		// free memory
		for (int i=0;i<nseq;i++) R_Free(optst[i]);
		R_Free(optst);

		R_Free(clsid);
		freeccm(&md);
		R_Free(md);
		R_Free(u);

		if (!Rf_isNull(rfsClust_))
			freeClust(refmode, refsigma,
				   rncls, rnseq, refpath, refcls);
		return S4();
	  }
    }

    if (rncls<0||rncls > SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    }
    ct=(int *)R_Calloc((size_t)rncls,int);
	for (int i=0;i<rncls;i++) ct[i]=0;
    for (int i=0;i<nseq;i++) ct[clsid[i]]++;
	
    //printclsinfo(outfile,clsid,nseq,rncls);
    
    Rcout << "All found viterbi sequences match with sequences in reference clusters\n";
    
    S4 HmmVbClust("HMMVBclust");

	HmmVbClust.slot("clustParam") = rfsClust;
	HmmVbClust.slot("clsid") = IntegerVector(clsid, clsid + nseq);
	HmmVbClust.slot("size") = IntegerVector(ct, ct + rncls);
    if(get_likeld==1){HmmVbClust.slot("Loglikehd") = loglh;}
	
	// free memory
	for (int i=0;i<nseq;i++) R_Free(optst[i]);
	R_Free(optst);
	
	R_Free(ct);
	R_Free(clsid);
	freeccm(&md);
	R_Free(md);
	R_Free(u);
	
	if (!Rf_isNull(rfsClust_))
		freeClust(refmode, refsigma,
				   rncls, rnseq, refpath, refcls);
	
    return HmmVbClust;
  }

  //---------------------------------------------------------//
  // Samples that can't be treated by reference clusters     //
  //---------------------------------------------------------//

  int **optst2, **newoptst, newnseq, *newid, *id2;

  if (nseq<0||nseq>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  id2=(int *)R_Calloc((size_t)nseq,int);

  if (noref<nseq) {            
    if (noref<0||noref > SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    }
    optst2=(int **)R_Calloc((size_t)noref,int *);
    for (int i=0, k=0;i<nseq;i++){
      if (clsid[i]<0){//path doesn't exist in reference
	optst2[k]=optst[i];
	id2[i]=k;
	k++;
      } else {id2[i]=-1;}
    }
  }
  else {
    optst2=optst;
    for (int i=0;i<nseq;i++) id2[i]=i;
  }

  
  if (noref<0||noref > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  newid=(int *)R_Calloc((size_t)noref,int);
  
  FindDifSeq(optst2, noref, md->nb, &newoptst, &newnseq, newid);

  if (optst2 != optst){
	for (int i=0; i<nseq; i++)
		R_Free(optst[i]);
	R_Free(optst);
  }

  for (int i=0; i<noref; i++)
	R_Free(optst2[i]);
  R_Free(optst2);
  
  //fprintf(stderr, "After FindDifSeq, noref=%d, newnseq=%d\n",noref,newnseq);
  //=============================//
  // Compute modes               //
  //=============================//
  CompMode *cpm;
  if (newnseq<0||newnseq > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  cpm=(CompMode *)R_Calloc((size_t)newnseq,CompMode);
  
  for (int i=0;i<newnseq;i++) {
    SetCompMode(cpm+i,newoptst[i], md);
    cpm[i].logpdf=bwmem(md, cpm[i].mu, cpm[i].mode);
  }

  int numcls=0;
  double *sigmadat;
  if (dim<0||dim > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  sigmadat=(double *)R_Calloc((size_t)dim,double);

  //sigmadat here is the within component standard deviation
  //if (refexist){
  //  for (i=0;i<dim;i++) sigmadat[i]=refsigma[i];
  //}
  //else {OverallSigma(md,sigmadat);}

  //The overall deviation
  DataSigma(u,sigmadat,dim,nseq);

  //--------------------------------------------------------------------//
  //Second round alignment with reference based on newly computed modes //
  //--------------------------------------------------------------------//
  int norefmode, *clsid2;
  double **combmode;

  if (newnseq<0||newnseq > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  clsid2=(int *)R_Calloc((size_t)newnseq,int);
  norefmode=0;
  for (int i=0;i<newnseq;i++){//See whether new modes coincide with existing modes
    clsid2[i]=FindCluster(cpm[i].mode, dim, rncls, refmode, sigmadat, modethreshold, usemeandist);
    if (clsid2[i]<0) {
      norefmode++; //still not assigned to existing cluster,can't compute this way
    }     
  }

  for (int i=0;i<nseq;i++) {
    if (clsid[i]<0) {
      clsid[i]=clsid2[newid[id2[i]]];
    } 
  }

  if (norefmode==0) { //all done
     List newClust = augmentrefcls(rncls, rnseq, refmode, sigmadat, refpath, refcls, dim, md->nb, numcls,clsid,
		    NULL, NULL, newnseq, newoptst, clsid2, newid, nseq, id2, &combmode);
		    
	
    //Rcout << "All modes associated, nseq=" << nseq << ", noref=" << noref << ", newnseq=" << newnseq << "\n";
    Rcout << "All found modes match with modes in reference clusters\n";
    
    if (mincls>1) {
      if (AdjustCluster(clsid, nseq,rncls,refmode,u, dim, mincls) < 0){
		// free memory
		for (int i=0; i<newnseq; i++) freeCompMode(cpm+i);
		R_Free(cpm);
		R_Free(clsid);
		R_Free(clsid2);
		R_Free(sigmadat);

		freeccm(&md);
		R_Free(md);
		R_Free(u);

		if (!Rf_isNull(rfsClust_))
			freeClust(refmode, refsigma,
					   rncls, rnseq, refpath, refcls);

		R_Free(combmode);
		
		return S4();
	  }
    }

    if (rncls<0||rncls > SIZE_MAX) {
      Rcpp::stop("Error in memory allocation, negative or too large size.\n");
    }
    ct=(int *)R_Calloc((size_t)rncls,int);
	for (int i=0;i<rncls;i++) ct[i]=0;
    for (int i=0;i<nseq;i++) ct[clsid[i]]++;
	
	S4 HmmVbClust("HMMVBclust");

	HmmVbClust.slot("clustParam") = newClust;
	HmmVbClust.slot("clsid") = IntegerVector(clsid, clsid + nseq);
	HmmVbClust.slot("size") = IntegerVector(ct, ct + rncls);
    if(get_likeld==1){HmmVbClust.slot("Loglikehd") = loglh;}
	
	// free memory
	for (int i=0; i<newnseq; i++) freeCompMode(cpm+i);
	R_Free(cpm);
	R_Free(ct);
	R_Free(clsid);
	R_Free(clsid2);
	R_Free(sigmadat);
	
	freeccm(&md);
	R_Free(md);
	R_Free(u);
	
	if (!Rf_isNull(rfsClust_))
		freeClust(refmode, refsigma,
				   rncls, rnseq, refpath, refcls);
	
	R_Free(combmode);
  
	
    return HmmVbClust;
  }

  //--------------------------------------------------------------------//
  //  Samples left that have to form their own new clusters             //
  //--------------------------------------------------------------------//
  int *id3, *cls;
  double **mode;

  if (norefmode<0||norefmode > SIZE_MAX || newnseq <0||newnseq > SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  id3=(int *)R_Calloc((size_t)newnseq,int);
  cls=(int *)R_Calloc((size_t)norefmode,int);
  mode=(double **)R_Calloc((size_t)norefmode,double *);
  for (int i=0, k=0;i<newnseq;i++) {
    if (clsid2[i]<0) {
      mode[k]=cpm[i].mode;
      id3[i]=k;
      k++;
    } else {id3[i]=-1;}
  }

  //fprintf(stderr, "nseq=%d, newnseq=%d, norefmode=%d\n",nseq,newnseq, norefmode);
  groupmode(mode, dim, norefmode, cls, &numcls, sigmadat, modethreshold, usemeandist);

  //There're numcls new clusters and rncls old clusters, offset the cls[] labels
  //by rncls
  for (int i=0;i<nseq;i++) {
    if (clsid[i]<0) {
      int k=id3[newid[id2[i]]];
      clsid[i]=cls[k]+rncls;
      clsid2[newid[id2[i]]]=cls[k]+rncls;
    }
  }

  //--------------------------------------------------------------------//
  //  Print out information                                             //
  //--------------------------------------------------------------------//
  
  //fprintf(stdout, "nseq=%d, noref=%d, newnseq=%d, norefmode=%d, numcls=%d, rncls=%d, #clusters=%d\n",
	//  nseq, noref, newnseq, norefmode, numcls, rncls, numcls+rncls);

  
  List newClust = augmentrefcls(rncls, rnseq, refmode, sigmadat, refpath, refcls, dim, md->nb, numcls,clsid,
		  id3, mode, newnseq, newoptst, clsid2, newid, nseq, id2, &combmode);
		  
  //Output cluster labels
  if (mincls>1) {
    if (AdjustCluster(clsid, nseq,rncls+numcls,combmode,u, dim, mincls) < 0){
		// free memory
		for (int i=0; i<newnseq; i++) R_Free(newoptst[i]);
		R_Free(newoptst);

		R_Free(id2);
		R_Free(newid);
		R_Free(clsid);
		R_Free(clsid2);
		R_Free(id3);
		R_Free(cls);
		R_Free(sigmadat);

		R_Free(mode);
		for (int i=0; i<newnseq; i++) freeCompMode(cpm+i);
		R_Free(cpm);

		freeccm(&md);
		R_Free(md);  
		R_Free(u);

		if (!Rf_isNull(rfsClust_))
			freeClust(refmode, refsigma,
					   rncls, rnseq, refpath, refcls);

		for (int i = rncls; i < rncls+numcls; i++) R_Free(combmode[i]);
		R_Free(combmode);
		return S4();
	}
  }

  if (rncls+numcls<0||rncls+numcls>SIZE_MAX) {
    Rcpp::stop("Error in memory allocation, negative or too large size.\n");
  }
  ct=(int *)R_Calloc((size_t)rncls+numcls,int);
  for (int i=0;i<rncls+numcls;i++) ct[i]=0;
  for (int i=0;i<nseq;i++) ct[clsid[i]]++;

  S4 HmmVbClust("HMMVBclust");

  HmmVbClust.slot("clustParam") = newClust;
  HmmVbClust.slot("clsid") = IntegerVector(clsid, clsid + nseq);
  HmmVbClust.slot("size") = IntegerVector(ct, ct + rncls + numcls);
  if(get_likeld==1){HmmVbClust.slot("Loglikehd") = loglh;}

  // free memory
  for (int i=0; i<newnseq; i++) R_Free(newoptst[i]);
  R_Free(newoptst);
  
  R_Free(id2);
  R_Free(newid);
  R_Free(clsid);
  R_Free(clsid2);
  R_Free(ct);
  R_Free(id3);
  R_Free(cls);
  R_Free(sigmadat);
  
  R_Free(mode);
  for (int i=0; i<newnseq; i++) freeCompMode(cpm+i);
  R_Free(cpm);

  freeccm(&md);
  R_Free(md);  
  R_Free(u);
  
  if (!Rf_isNull(rfsClust_))
		freeClust(refmode, refsigma,
				   rncls, rnseq, refpath, refcls);
	
  for (int i = rncls; i < rncls+numcls; i++) R_Free(combmode[i]);
  R_Free(combmode);
  
  return HmmVbClust;
}



