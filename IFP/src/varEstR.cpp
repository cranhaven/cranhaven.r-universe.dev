/************************** varEstR.CPP ************************ ply 2009-8-7 *
*                                                                            *
* Variation Estimation of pMc in companion with R                            *
*                                                                            *
*****************************************************************************/

#include <iostream>
#include <math.h>
#include <stdio.h>
#include <R.h>
#include <Rmath.h>

using namespace std;

extern "C" int dgesv_(const int* n1, const int* n2, double* A, const int* n3, int* ipivot, double* b, const int* n4, int* info);

extern "C"{

  void varEstR(int *H0, int *nFpi, int *nPolyi, double *pMc, double *pM, double *pMdi, int *nConi, int *nCai, double *pMcV){

    int i, j, k=0;
    double rbinom(double n, double p);
    int nSim=1000;
    int nFp=*nFpi, nPoly=*nPolyi, nCon=*nConi, nCa=*nCai;
    double **pMd = new double*[nPoly];
    for (i=0; i<nPoly; i++) pMd[i] = new double[nPoly];
    for (i=0; i<nPoly; i++) for (j=0; j<nPoly; j++) {
      pMd[i][j]=pMdi[k];
      k++;
    }
    for (i=0; i<nFp; i++) H0[i]=H0[i]-1;
    double **pMcResult = new double*[nSim];
    for (i=0; i<nSim; i++) pMcResult[i] = new double[nPoly];
    int *okA =new int [nSim];
    for(i=0; i<nSim; i++) okA[i]=1;
    for (i=0; i<nSim; i++){

      GetRNGstate();
      double *x = new double[nPoly];
      for (j=0; j<nPoly; j++){
	x[j] = rbinom(nCa,pMc[j]);
      }
      PutRNGstate();

      double *pMt = new double[nPoly];
      double **pMdt = new double*[nPoly];
      for (j=0; j<nPoly; j++) pMdt[j] = new double[nPoly];

      int check=0;
      while(check == 0){

	GetRNGstate();

	for (j=0; j<nPoly; j++) {
	  pMt[j] = rbinom(nCon,pM[j]);
	  pMt[j] = pMt[j]/(double)nCon;
	}
	for (j=0; j<nPoly; j++){ 
	  for (k=0; k<nPoly; k++) {
	    pMdt[j][k] = -1;
	  }
	}

	double **pMdlb = new double*[nPoly];
	for (j=0; j<nPoly; j++) pMdlb[j] = new double[nPoly];
	double **pMdub = new double*[nPoly];
	for (j=0; j<nPoly; j++) pMdub[j] = new double[nPoly];

	for (j=0; j<nPoly; j++){
	  for (k=j+1; k<nPoly; k++){
	    if (pMt[j]<=0.5 && pMt[k]<=0.5){
	      pMdlb[j][k]=0;
	      if (pMt[j]<=pMt[k]) pMdub[j][k]=pMt[j];
	      else pMdub[j][k]=pMt[k];
	    }
	    if (pMt[j]<=0.5 && pMt[k]>0.5){
	      pMdub[j][k]=pMt[j];
	      if (pMt[j]>=1-pMt[k]) pMdlb[j][k]=pMt[k]-1+pMt[j];
	      else pMdlb[j][k]=0;
	    }
	    if (pMt[j]>0.5 && pMt[k]<=0.5){
	      pMdub[j][k]=pMt[k];
	      if (pMt[k]>=1-pMt[j]) pMdlb[j][k]=pMt[j]-1+pMt[k];
	      else pMdlb[j][k]=0;
	    }
	    if (pMt[j]>0.5 && pMt[k]>0.5){
	      pMdlb[j][k]=pMt[j]+pMt[k]-1;
	      if (1-pMt[j]>=1-pMt[k]) pMdub[j][k]=pMt[j];
	      else pMdub[j][k]=pMt[k];
	    }

	    pMdt[j][k]=rbinom(nCon,pMd[j][k]);
	    pMdt[j][k]=pMdt[j][k]/(double)nCon;
	    if (pMdub[j][k]-pMdlb[j][k]>=(double)1/nCon){
	      if (pMdt[j][k]>=pMdlb[j][k] && pMdt[j][k]<=pMdub[j][k]) pMdt[k][j]=pMdt[j][k];
	      else {
		int No=0;
		while((pMdt[j][k]<pMdlb[j][k] && No<20) || (pMdt[j][k]>pMdub[j][k] && No<20)){
		  pMdt[j][k]=rbinom(nCon,pMd[j][k]);
		  pMdt[j][k]=pMdt[j][k]/(double)nCon;
		  No++;
		}
		if (pMdt[j][k]>=pMdlb[j][k] && pMdt[j][k]<=pMdub[j][k]) pMdt[k][j]=pMdt[j][k];
	      }
	    }
	    else {      //ub-lb<1/nCon
	      for (int l=1; l<nCon+1; l++){
		double tMdc=(double)l/nCon;
		if (tMdc<=pMdub[j][k] && tMdc>=pMdlb[j][k]) pMdt[k][j]=pMdt[j][k];
	      }
	    }

	  } //k
	}  //j

	PutRNGstate();

	int count=0;
	for (j=0; j<nPoly; j++)
	  for (k=0; k<nPoly; k++){
	    if (pMdt[j][k]==-1) count++;
	  }
	if (count==nPoly) check=1;

	for (j=0; j<nPoly; j++) delete[] pMdub[j];
	delete [] pMdub;
	for (j=0; j<nPoly; j++) delete[] pMdlb[j];
	delete [] pMdlb;

      } //while check


      double *diff = new double[nFp];
      double **diffA = new double*[nFp];
      for (j=0; j<nFp; j++) diffA[j] = new double[nFp];

      for (j=0; j<nFp; j++){
	diff[j] = x[H0[j]]/(double)nCa-pMt[H0[j]];
	for (k=0; k<nFp; k++){
	  if (k==j){ diffA[j][k]=1;}
	  else{ diffA[j][k]=(pMdt[H0[j]][H0[k]]-pMt[H0[j]]*pMt[H0[k]])/(pMt[H0[k]]*(1-pMt[H0[k]]));}
	}
      }

      double *A = new double [nFp*nFp];
      for (j=0; j<nFp; j++) for (k=0; k<nFp; k++) A[k+nFp*j]=diffA[k][j];
      int c2, ok;
      c2=1;
      int *IPVT = new int[nFp];
      dgesv_(&nFp,&c2,A,&nFp,IPVT,diff,&nFp,&ok);

      double *pMct=new double[nPoly];
      for (j=0; j<nFp; j++){
	if(diff[j]<=1-pMt[H0[j]] && diff[j]>=-pMt[H0[j]]);
	else ok=1;
      }
      if (nFp==1)ok=0;
      if (ok==0){
	for (j=0; j<nPoly; j++){
	  pMct[j]=pMt[j];
	  int tCheck;
	  tCheck=0;
	  for (k=0; k<nFp; k++) if(j==H0[k]) tCheck=1;
	  if (tCheck==1){ pMct[j]=x[j]/(double)nCa;}
	  else{ for (k=0; k<nFp; k++) pMct[j]=pMct[j]+diff[k]*(pMdt[j][H0[k]]-pMt[j]*pMt[H0[k]])/(pMt[H0[k]]*(1-pMt[H0[k]]));}
	  if (pMct[j]<0.0 || pMct[j]>1.0) ok=1;
	}
      }

      if (ok==0){
	for (j=0; j<nPoly; j++) pMcResult[i][j]=pMct[j]*(double)nCa; //nCa need to be multiplied
	okA[i]=0;
      }

      delete [] x;
      for (j=0; j<nFp; j++) delete[] diffA[j];
      delete [] diffA;
      delete [] IPVT;
      delete [] diff;
      delete [] pMct;
      delete [] pMt;
      for (j=0; j<nPoly; j++) delete[] pMdt[j];
      delete [] pMdt;
      delete [] A;

    } //i nSim

    double *pMctVar = new double [nPoly];
    double *pMctMean = new double [nPoly];
    for(i=0; i<nPoly; i++){
      pMctMean[i]=0;
      pMctVar[i]=0;
    }
    int pMctNo=0;
    for(i=0; i<nSim; i++){
      if (okA[i]==0){
	pMctNo++;
	for(j=0; j<nPoly; j++){
	  pMctMean[j]+=pMcResult[i][j];
	}
      }
    }
    for(i=0; i<nPoly; i++) pMctMean[i]=pMctMean[i]/(double)pMctNo;
    for(i=0; i<nSim; i++){
      if (okA[i]==0){
	for(j=0; j<nPoly; j++) pMctVar[j]+=(pMcResult[i][j]-pMctMean[j])*(pMcResult[i][j]-pMctMean[j]);
	}
      }
    for(i=0; i<nPoly; i++) {
      pMcV[i]=pMctVar[i]/(double)(pMctNo-1);
   }
      for (j=0; j<nSim; j++) delete[] pMcResult[j];
      delete [] pMcResult;
      for (j=0; j<nPoly; j++) delete[] pMd[j];
      delete [] pMd;

      delete [] okA;
      delete [] pMctVar;
      delete [] pMctMean;

  } //void VarEst
	  
} //extern C	    

