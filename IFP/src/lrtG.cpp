/*************************** lrt.CPP ************************* ply 2014-3-25 *
*                                                                            *
* Likelihood Ratio Test of genotypes in companion with R                     *
*                                                                            *
*****************************************************************************/

#include <iostream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rmath.h>

using namespace std;

extern "C"{

  double CalcMean(double *, int);
  double CalcVar(double, double *, int);
  double CalcCovar(double, double, double *, double *, int);
  double *VarEst(int*, int, int, int *, double *, int **, int **, int *, int, int, double **);

  double *LRT(int *nFpTestt, int *nPolyi, int *genoO, int *genoCO, int *nCont, int *nCat, double *lrr, double *df){

    int i, j, k, y;
    int nFpTest = *nFpTestt, nPoly=*nPolyi, nCon=*nCont, nCa=*nCat;  
    int nCo = nCon/2;

    int *nM = new int[nPoly];
    int **geno = new int*[nPoly];
    for (i=0; i<nPoly; i++){
      geno[i] = new int[nCon];
      nM[i] = 0;
    }
    k=0;
    for (i=0; i<nPoly; i++) for (j=0; j<nCon; j++) {
      geno[i][j]=genoO[k];
      nM[i]+=genoO[k];
      k++;
    }

    int **gent = new int*[nPoly];
    for (i=0; i<nPoly; i++) gent[i] = new int[nCo+nCa];
    for (i=0; i<nPoly; i++) for (j=0; j<nCo; j++) {
      gent[i][j]=geno[i][2*j]+geno[i][2*j+1];
    }

    int *nMc = new int[nPoly];
    int **genoC = new int*[nPoly];
    for (i=0; i<nPoly; i++){
      genoC[i] = new int[nCa*2];
      nMc[i] = 0;
    }
    k=0;
    for (i=0; i<nPoly; i++) for (j=0; j<2*nCa; j++) {
      genoC[i][j]=genoCO[k];
      nMc[i]+=genoCO[k];
      k++;
    }

    int **nMMc = new int*[nPoly];
    for (i=0; i<nPoly; i++) nMMc[i] = new int[3];
    double **pMMc = new double*[nPoly];
    for (i=0; i<nPoly; i++) pMMc[i] = new double[3];
    int **gentC = new int*[nPoly];
    for (i=0; i<nPoly; i++) gentC[i] = new int[nCa];
    for (i=0; i<nPoly; i++) for (j=0; j<3; j++) nMMc[i][j]=0;
    for (i=0; i<nPoly; i++) for (j=0; j<nCa; j++) {
      gentC[i][j] = genoC[i][2*j]+genoC[i][2*j+1];
      if(gentC[i][j]==0) nMMc[i][0]++;
      if(gentC[i][j]==1) nMMc[i][1]++;
      if(gentC[i][j]==2) nMMc[i][2]++;
    }
    for (i=0; i<nPoly; i++) for (j=0; j<3; j++) pMMc[i][j]=(double)nMMc[i][j]/nCa;
   
    double **pMMr = new double*[nPoly];
    for (i=0; i<nPoly; i++) pMMr[i] = new double[3];
    int *genoD = new int[nCon];
    int *gentD = new int[nCo+nCa];
    int *genoDc = new int[nCa*2];
    double *pDD = new double[3];
    int *nDD = new int[3];
    int *nDDc = new int[3];
    double *pDDc = new double[3];
    int *nMMdd = new int[9];
    double *pMM = new double[9];
    double *pMMcV = new double[nPoly];
    int *cases = new int[nCon];
    int totalreturn = 0;
    int **nMMct = new int*[nPoly];
    for (i=0; i<nPoly; i++) nMMct[i] = new int[9];

    for (int nFp=1; nFp<=nFpTest; nFp++){

      int hypos = choose(nPoly,nFp);
      int *riskAllele = new int[nFp];
      int *info = new int[nFp];
      for (i=0; i<nFp; i++) info[i]=i;
    
      for(int z=0; z<hypos; z++){

	int zeroC = 0;

	for(i=0; i<nFp; i++){
	  //double pD, pDc;
	  //int nD=0;
	  //int nDc=0;
	  //for(j=0; j<nCon; j++) nD += geno[info[i]][j];
	  //for(j=0; j<nCa*2; j++) nDc += genoC[info[i]][j];
	  //pD = (double)nD/nCon;
	  //pDc = (double)nDc/nCa/2;
	  //if(pDc>pD) riskAllele[i] = 1;
	  //else riskAllele[i] = 0;
	  riskAllele[i] = 1;
	}
	
	for(i=0; i<3; i++){
	  nDDc[i]=0;
	  nDD[i]=0;
	}
	for(i=0; i<nCo; i++){
	  genoD[2*i] = 0;
	  genoD[2*i+1] = 0;
	  for(j=0; j<nFp; j++){
	    if(geno[info[j]][2*i]==riskAllele[j]) genoD[2*i] = 1;
	    if(geno[info[j]][2*i+1]==riskAllele[j]) genoD[2*i+1] = 1;
	  }
	  gentD[i] = genoD[2*i] + genoD[2*i+1];
	  nDD[gentD[i]]++;
	}

	for(i=0; i<nCa; i++){
	  genoDc[2*i] = 0;
	  genoDc[2*i+1] = 0;
	  for(j=0; j<nFp; j++){
	    if(genoC[info[j]][2*i]==riskAllele[j]) genoDc[2*i] = 1;
	    if(genoC[info[j]][2*i+1]==riskAllele[j]) genoDc[2*i+1] = 1;
	  }
	  nDDc[genoDc[2*i]+genoDc[2*i+1]]++;
	}

	// Testing for dominance
	

	GetRNGstate();

	for (i=0; i<nPoly; i++) for (j=0; j<9; j++) nMMct[i][j]=0;

	for(i=0; i<3; i++){
	  if(nDDc[i]>0 && nDD[i]==0){
	    for(j=0; j<nCa; j++){
	      if(genoDc[2*j]+genoDc[2*j+1]==i) for(k=0; k<nPoly; k++) nMMct[k][gentC[k][j]*3+i]++;
	    }
	  
	    zeroC = zeroC+1;
	    nDD[i] = 1;
	  }

	}

	PutRNGstate();

	for(i=0; i<3; i++){
	  pDD[i] = (double)nDD[i]/(nCo+zeroC);
	  pDDc[i] = (double)nDDc[i]/nCa;
	}

	//for(i=0; i<3; i++) pMMr[H0][i] = pDDc[i];
	for (i=0; i<nPoly; i++){
	  int hcheck=0;
	  for(j=0; j<nFp; j++) if(info[j]==i) hcheck++;
	  if(hcheck==0){
	    
	    for(j=0; j<9; j++) nMMdd[j]=0;
	    for(j=0; j<nCo; j++){
	      y = gent[i][j]*3+gentD[j];
	      nMMdd[y]++;
	    }

	    for(j=0; j<9; j++){
	      pMM[j]=(double)nMMdd[j]/(nCo+zeroC);
	      if(nMMct[i][j]>0){
		int gt = j % 3; //pow(3,nFp);  // 3^nFp
		pMM[j]=(double)nMMct[i][j]/nDDc[gt]/(nCo+zeroC);
	      }
	    }

	    for(j=0; j<9; j++){
	      int gt = j % 3; //pow(3,nFp);  // 3^nFp
	      if(pDD[gt]>0) pMM[j]=pMM[j]*pDDc[gt]/pDD[gt];
	      else pMM[j]=pMM[j]*pDDc[gt];
	    }
	    for(j=0; j<3; j++) pMMr[i][j] = 0;
	    for(j=0; j<9; j++){
	      int gt = (int)j / 3; //pow(3,nFp);
	      pMMr[i][gt] += pMM[j];
	    }

	  }
	}  //i nPoly
	
	pMMcV = VarEst(info, nFp, nPoly, riskAllele, pDDc, geno, gentC, genoDc, nCon, nCa, pMMr);
	double lrTotal=0;
	int dft = 0;
	for(i=0; i<nPoly; i++){
	  int hcheck=0;
	  for(j=0; j<nFp; j++) if(info[j]==i) hcheck++;
	  int mono = 0;
	  if((nM[i]==0 || nM[i]==nCon) && (nMc[i]==0 || nMc[i]==2*nCa)) mono++;
	  if(hcheck==0 && mono==0){
	    
	    double lr=0;
	    double var=1;
	    int k=0;
	    for(j=0; j<3; j++){
	      if(nMMc[i][j]>0){
		if(k<2 && pMMr[i][j]>=(double)1/(double)nCa){
		  var = var*nMMc[i][j]*(1-pMMc[i][j]);
		  k++;
		}
		if(pMMr[i][j]<(double)1/(double)nCa) pMMr[i][j]=(double)1/(double)nCa;
		//if(pMMr[i][j]<(double)5/(double)nCa) k=-3;
		lr += -2*((double)nMMc[i][j]*log(pMMr[i][j])-(double)nMMc[i][j]*log(pMMc[i][j]));
		dft++;
	      }
	      else{
		if(pMMr[i][j]>0) dft++;
		k=-3;
	      }
	    }
	    if(k==2) var = var - nMMc[i][0]*pMMc[i][1]*nMMc[i][1]*pMMc[i][0];
	    else var = nMMc[i][1]*(1-pMMc[i][1]);

	    double varAdj = var/(var+pMMcV[i]);
	    if(var+pMMcV[i]==0) varAdj = 1;
	    lrTotal += lr*varAdj;
	    //lrr[i] = var/(var+pMMcV[i]); //+pMMr[i][1]+pMMr[i][2]);
	  }
	}

	lrr[totalreturn] = lrTotal;
	df[totalreturn] = (double)(dft-nPoly+nFp);
	totalreturn++;

	int infoC = 0;
	for(i=0; i<nFp; i++){
	  if(infoC<2){
	    if(info[nFp-1-i]<nPoly-1-i){
	      infoC = 2;
	      info[nFp-1-i]++;
	    }
	    else if(nFp-2-i>=0){
	      if(info[nFp-1-(i+1)]<nPoly-1-(i+1)) info[nFp-1-i]=info[nFp-2-i]+2;
	      infoC = 1;
	    }
	  }
	}
	
      }  //z

      delete [] info;
      delete [] riskAllele;
	
    } //nFp

    delete [] nM;
    delete [] nMc;
    delete [] pDD;
    delete [] nDD;
    delete [] pDDc;
    delete [] nDDc;
    delete [] nMMdd;
    delete [] pMM;
    delete [] cases;
    delete [] pMMcV;
    delete [] genoD;
    delete [] genoDc;
    delete [] gentD;

    for (i=0; i<nPoly; i++) delete [] pMMr[i];
    delete [] pMMr;
    for (i=0; i<nPoly; i++) delete [] nMMc[i];
    delete [] nMMc;
    for (i=0; i<nPoly; i++) delete [] nMMct[i];
    delete [] nMMct;
    for (i=0; i<nPoly; i++) delete [] pMMc[i];
    delete [] pMMc;

    for (i=0; i<nPoly; i++) delete [] geno[i];
    delete [] geno;
    for (i=0; i<nPoly; i++) delete [] gent[i];
    delete [] gent;
    for (i=0; i<nPoly; i++) delete [] genoC[i];
    delete [] genoC;
    for (i=0; i<nPoly; i++) delete [] gentC[i];
    delete [] gentC;

    return lrr;
    return df;

    //delete [] nFpi;
    //delete [] nPolyi;
    //delete [] genoO;
    //delete [] genoCO;
    //delete [] nCont;
    //delete [] nCat;

    //delete [] info;

    //return lrr;

  } //void LRT


  double CalcMean(double *array, int max)

    { 

        double sum = 0; 

        for(int i = 0; i < max; i++) 

            sum += array[i]; 

        return (sum/max); 

    } 

  double CalcVar(double m, double *array, int max) 

    { 

        double temp = 0; 

        for(int i = 0; i < max; i++) 

        { 

             temp += (array[i] - m) * (array[i] - m) ; 

        } 

        return temp / (max-1); 

    } 

  double CalcCovar(double m1, double m2, double *array1, double *array2, int max) 

    { 

        double temp = 0; 

        for(int i = 0; i < max; i++) 

        { 

             temp += (array1[i] - m1) * (array2[i] - m2) ; 

        } 

        return temp / (max-1); 

    } 



  double *VarEst(int *info, int nFp, int nPoly, int *riskAllele, double *pMMc, int **geno, int **gentC, int *genoDc, int nCon, int nCa, double **pMMro){

    int i, j, y, k=0;
    int nSim=1000;
    int nCo = nCon/2;

    int **genoT = new int*[nPoly];
    for (i=0; i<nPoly; i++) genoT[i] = new int[nCon];
    int **gent = new int*[nPoly];
    for (i=0; i<nPoly; i++) gent[i] = new int[nCo+nCa];

    double **pMMr = new double*[nPoly*3];
    for (i=0; i<(nPoly*3); i++) pMMr[i] = new double[nSim];

    int *x = new int[3];
    int *nDD = new int[3];
    double *pDD = new double[3];
    double *pDDc = new double[3];
    int *nMMdd = new int[9];
    double *pMM = new double[9];
    int *cases = new int[nCa];
    int *genoD = new int[nCon];
    int *gentD = new int[nCo+nCa];
    int **nMMct = new int*[nPoly];
    for (i=0; i<nPoly; i++) nMMct[i] = new int[9];

    double *pMMcV = new double[nPoly];

    for (int z=0; z<nSim; z++){

    GetRNGstate();

      rmultinom(nCa,pMMc,3,x);
      int cont, cat;

      for(i=0; i<nCo; i++){
	int cont = (int)runif(0,nCo);
	for(j=0; j<nPoly; j++){
	  genoT[j][2*i]=geno[j][2*cont];
	  genoT[j][2*i+1]=geno[j][2*cont+1];
	  gent[j][i]=genoT[j][2*i]+genoT[j][2*i+1];
	}
	genoD[2*i] = 0;
	genoD[2*i+1] = 0;
	for(j=0; j<nFp; j++){
	  if(genoT[info[j]][2*i]==riskAllele[j]) genoD[2*i] = 1;
	  if(genoT[info[j]][2*i+1]==riskAllele[j]) genoD[2*i+1] = 1;
	}
	gentD[i] = genoD[2*i]+genoD[2*i+1];
      }

      int repCo = nCo/5;
      for(int repeat=0; repeat<repCo; repeat++){
      int ransam = 0;
      while(ransam==0){
	cat = (int)runif(0,nCa);
	cont = (int)runif(0,nCo);
	if(gentD[cont]==genoDc[2*cat]+genoDc[2*cat+1]) ransam=1;
      }
      for(i=0; i<nPoly; i++) gent[i][cont] = gentC[i][cat];
      }

      for(i=0; i<3; i++){
	pDDc[i]=(double)x[i]/(double)nCa;
	nDD[i]=0;
      }
      for(i=0; i<nCo; i++) nDD[gentD[i]]++;

      int zeroC = 0;
      for (i=0; i<nPoly; i++) for (j=0; j<9; j++) nMMct[i][j]=0;

      for(i=0; i<3; i++){
	if(x[i]>0 && nDD[i]==0){
	  for(j=0; j<nCa; j++){
	    if(genoDc[2*j]+genoDc[2*j+1]==i) for(k=0; k<nPoly; k++) nMMct[k][gentC[k][j]*3+i]++;
	  }
	  
	  zeroC = zeroC+1;
	  nDD[i] = 1;
	}
      }

      for(i=0; i<3; i++) pDD[i] = (double)nDD[i]/(nCo+zeroC);

      for (i=0; i<nPoly; i++){
	int hcheck=0;
	for(j=0; j<nFp; j++) if(info[j]==i) hcheck++;
	if(hcheck==0){

	  for(j=0; j<9; j++) nMMdd[j]=0;
	  for(j=0; j<nCo; j++){
	    y = gent[i][j]*3+gentD[j];
	    nMMdd[y]++;
	  }

	  for(j=0; j<9; j++){
	    pMM[j]=(double)nMMdd[j]/(nCo+zeroC);
	    if(nMMct[i][j]>0){
	      int gt = j % 3; //pow(3,nFp);  // 3^nFp
	      pMM[j]=(double)nMMct[i][j]/x[gt]/(nCo+zeroC);
	    }
	  }

	  for(j=0; j<9; j++){
	    int gt = j % 3; //pow(3,nFp);  // 3^nFp
	    if(pDD[gt]>0) pMM[j]=pMM[j]*pDDc[gt]/pDD[gt];
	    else pMM[j]=pMM[j]*pDDc[gt];
	  }
	  for(j=0; j<3; j++) pMMr[3*i+j][z] = 0;
	  for(j=0; j<9; j++){
	    int gt = (int)j / 3; //pow(3,nFp);
	    pMMr[3*i+gt][z] += pMM[j];
	  }
	  for(j=0; j<3; j++) pMMr[3*i+j][z] = pMMr[3*i+j][z]*nCa;

	}
      }  //i nPoly
	
    PutRNGstate();

    }  //z
      
    double *pMMm = new double[2];
    for(i=0; i<nPoly; i++){
 
      int hcheck=0;
      for(j=0; j<nFp; j++) if(info[j]==i) hcheck++;
      if(hcheck==0){
	pMMm[0] = CalcMean(pMMr[i*3],nSim);
	pMMm[1] = CalcMean(pMMr[i*3+1],nSim);

	if(pMMro[i][0]>1/(double)nCa && pMMro[i][2]>1/(double)nCa){
	  double pMMv1 = CalcVar(pMMm[0],pMMr[i*3],nSim);
	  double pMMv2 = CalcVar(pMMm[1],pMMr[i*3+1],nSim);
	  double pMMcov = CalcCovar(pMMm[0],pMMm[1],pMMr[i*3],pMMr[i*3+1],nSim);

	  pMMcV[i] = pMMv1*pMMv2-pMMcov*pMMcov;
	} 
	else {
	  pMMcV[i] = CalcVar(pMMm[1],pMMr[i*3+1],nSim);
	}
	//pMMcV[i]= pMMr[i*3][2]; //+pMMr[i*3+1][0]+pMMr[i*3+2][0];
      }      
    }
    delete [] pMMm;
	
    delete [] x;
    delete [] pDD;
    delete [] nDD;
    delete [] pDDc;
    delete [] nMMdd;
    delete [] pMM;
    delete [] cases;
    delete [] genoD;
    delete [] gentD;

    for (i=0; i<nPoly; i++) delete [] genoT[i];
    delete [] genoT;
    for (i=0; i<nPoly; i++) delete [] gent[i];
    delete [] gent;
    for (i=0; i<nPoly; i++) delete [] nMMct[i];
    delete [] nMMct;
    for (i=0; i<nPoly*3; i++) delete [] pMMr[i];
    delete [] pMMr;

    return pMMcV;
    delete [] pMMcV;

  } //void VarEst


	  
} //extern C	    

