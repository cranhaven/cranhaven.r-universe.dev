
#ifndef  USE_FC_LEN_T
# define USE_FC_LEN_T
#endif

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <stdio.h>
#include <R_ext/Lapack.h> 
#include <stdlib.h>
#ifndef FCONE
# define FCONE
#endif

#include "Linear.h"
#include "Binary.h"

// no explicit blas.h included; implicitly included in lapack.h
void fEBLinearMainEff(double *BASIS, double *y, double *a_gamma, double *b_gamma,double *Beta, 
				double *wald, double *intercept, int *n, int *kdim,int *VB,double *residual);
				

void fEBBinaryMainEff(double *BASIS, double * Targets, double *a_gamma, double * b_gamma,
				double * logLIKELIHOOD, double * Beta, double *wald,double *intercept, int *n, int *kdim,int *VB);				


void elasticNetLinear(double *BASIS, double *y, double *a_lambda, double *b_Alpha,
                      double *Beta, 
                      double *wald, double *intercept, int *n, int *kdim, int *verb,double *residual);

		
		void ElasticNetBinary(double *BASIS, double * Targets, double *a_Lambda,double *b_Alpha,
                        double * logLIKELIHOOD, double * Beta, double *wald,double *intercept, int *n, int *kdim);
				
double stdTargets(double* Targets,int N)
{
	int i;
	double meanT			= 0;
	double stdT				= 0;
	double varT;
	for(i=0;i<N;i++) meanT	= meanT + Targets[i];
	meanT					= meanT/N;
	for(i=0;i<N;i++) stdT	= stdT + pow((Targets[i] - meanT),2);
	varT					= stdT/(N-1);
	stdT					= sqrt(varT);
	return stdT;
}

//version Note: block coordinate ascent: after each block updated: update IBinv before next iteration

//transpose a matrix;
void transposeB(double *B, int M, int N) //MxN input
{
	int MN = M*N;
	double *tB,*readPtr1,*readPtr2;
	tB 	= (double* ) Calloc(MN, double); 
	
	int i,inci,incj;
	inci = 1;
	incj = N;
	for(i=0;i<N;i++)
	{
		readPtr1 = &tB[i];
		readPtr2 = &B[i*M];
		F77_CALL(dcopy)(&M,readPtr2,&inci,readPtr1,&incj);
	}
	
	F77_CALL(dcopy)(&MN,tB,&inci,B,&inci);
	Free(tB);
}

//input X, y, 
//		grid: the grid point for prior 1: after grid 1, early stop to grid 2; so on so forth
//prior 1: lassoNEG
//prior 2: lasso
//prior 3: elastic net
//internal function for cvFunc: compute nFold cv for one hyperparameter
//return negative logL c(mean(ml), sd(ml);
// the creating Xtrain, Xtest have to be repeated to allow early stop;
//API
void cvOnePara(double *BASIS, double *y, int *foldId, int *nfolds, 
				int *n, int *k,int *VB,
				double *hyperpara,double *nLogL,
				int *epistasis, int *pr, int *glm,
				int *group)
{
	//initialize 
	int N = *n;
	int p = *k;
	int verbose = *VB;
	int prior = *pr;
	int epis = *epistasis;
	int Kcv = *nfolds;
	int GLM = *glm;
	//transpose BASIS to avoid memory allocation repeation
	int MN = N*p;
	double *X 	= (double* ) Calloc(MN, double); 
	double *readPtr1,*readPtr2;
	int i,j,jj,kk,cv,inci,incj;
	int loc1, loc2;
	inci = 1;
	incj = 1;
	F77_CALL(dcopy)(&MN,BASIS,&inci,X,&incj);
	transposeB(X,N,p);
	double *Xtrain = (double* ) Calloc(MN, double);  
	double *Xtest  = (double* ) Calloc(MN, double); 
	double *Ytrain = (double* ) Calloc(N, double); 
	double *Ytest = (double* ) Calloc(N, double); 
	double *SSE = (double* ) Calloc(N, double); 
	//for each fold of cv, read Xtrain, Xtest from X; 
	//						transpose Xtrain, Xtest to function.
	
	int indTr, indTe, nTr, nTe;
	double *Beta;
	double a_gamma, b_gamma;
	double wald,intercept,Mu0,residual,logLIKELIHOOD;
	double *Intercept = (double* ) Calloc(2, double); 
	int nEff  = p;
	if(epis ==1) nEff = p*(p+1)/2;
	Beta = (double* ) Calloc(nEff*5, double); 
	jj = 0;
	double temp, meanSE;
	meanSE = 0;
	for(cv=1;cv<=Kcv;cv++)
	{
		//step 1: get X, Y ready		
		indTr = 0;
		indTe = 0;
		for(i =0;i<N;i++)
		{
			if(foldId[i] ==cv)//copy to Xtest
			{
				readPtr1 = &X[i*p];
				readPtr2 = &Xtest[indTe*p];
				F77_CALL(dcopy)(&p,readPtr1,&inci,readPtr2,&incj);
				Ytest[indTe] = y[i];				
				indTe++;

			}else//copy to Xtrain;
			{
				readPtr1 = &X[i*p];
				readPtr2 = &Xtrain[indTr*p];
				F77_CALL(dcopy)(&p,readPtr1,&inci,readPtr2,&incj);
				Ytrain[indTr] = y[i];				
				indTr++;				
			}//endif

		}//end for
		nTr = indTr;
		nTe = indTe;
		transposeB(Xtrain,p,nTr);
		transposeB(Xtest,p,nTe);	
		
		//step2: call the function;
		a_gamma = hyperpara[0];
		b_gamma = hyperpara[1];	
			
		if(prior ==1)//lassoNEG
		{
	
			if(epis == 0)
			{
				if(GLM==0)//0: linear; 1: logistic
				{
fEBLinearMainEff(Xtrain,Ytrain, &a_gamma, &b_gamma,Beta, 
				&wald, &intercept, &nTr, &p,&verbose,&residual);
				
				}else
				{
fEBBinaryMainEff(Xtrain,Ytrain, &a_gamma, &b_gamma,
				&logLIKELIHOOD, Beta, &wald, Intercept, &nTr, &p,&verbose);
					
				}
			}

		}else//lassoNE, EBEN
		{
			if(epis == 0)
			{
				if(GLM==0)
				{
elasticNetLinear(Xtrain, Ytrain, &b_gamma, &a_gamma,
				Beta, &wald, &intercept, &nTr, &p, &verbose,&residual);
					
				}else
				{
ElasticNetBinary(Xtrain,Ytrain, &b_gamma, &a_gamma,
				&logLIKELIHOOD, Beta, &wald, Intercept, &nTr, &p);
					
				}				
			}
		}// end of prior
		
		//step3: compute prediction in Ytest;
		int M = 0;
		for(i=0;i<nEff;i++)
		{
			if(Beta[nEff*2+i]!=0) M++;
		}
		double *PHI = (double* ) Calloc(nTe*M, double);
		double *beta = (double* ) Calloc(M, double);
		double *PHI_Mu = (double* ) Calloc(nTe, double);
		kk = 0;
		for(i=0;i<nEff;i++)
		{
			loc1 = (int)Beta[i] -1;
			loc2 = (int)Beta[nEff+i] -1;
			if(Beta[nEff*2+i]!=0)
			{
				beta[kk] = Beta[nEff*2+i];
				if(loc1 == loc2)
				{
					//copy loc1
					readPtr1 = &PHI[kk*nTe];
					readPtr2 = &Xtest[loc1*nTe];
					F77_CALL(dcopy)(&nTe,readPtr2,&inci,readPtr1,&incj);
				}else
				{
					for(j=0;j<nTe;j++) PHI[kk*nTe + j] = Xtest[loc1*nTe+j]*Xtest[loc2*nTe+j];
				}
				kk++;
			}
		}//end for
		for(i = 0;i<nTe;i++)
		{
			PHI_Mu[i]			= 0;
			for(j = 0;j<M;j++)	PHI_Mu[i]	= PHI_Mu[i] + PHI[j*nTe+i]*beta[j];
		}
	
	// 1) prediction error -->gaussian;
	// 2) likelihood; --> logistic;
		if(GLM==0) //MSE
		{
			for(i = 0;i<nTe;i++)
			{
				temp = Ytest[i] - intercept -PHI_Mu[i];
				SSE[jj] = temp*temp;
				meanSE = meanSE + SSE[jj];
				jj++;
			}
		}else//-logL
		{
			Mu0 = Intercept[0];
			for(i = 0;i<nTe;i++)
			{
				SSE[jj] = Ytest[i]*log(exp(Mu0+PHI_Mu[i])/(1+exp(Mu0+PHI_Mu[i]))) + 
						(1-Ytest[i])*log(1/(1+exp(Mu0+PHI_Mu[i])));
				SSE[jj] = -SSE[jj];
				meanSE = meanSE + SSE[jj];
				jj++;
			}
		}	
	Free(PHI);
	Free(beta);
	Free(PHI_Mu);
	
	}//end of for CV
	//mean sde for this hyperparameter;
	nLogL[0] = a_gamma;
	nLogL[1] = b_gamma;
	nLogL[2] = meanSE/N;
	nLogL[3] = stdTargets(SSE,N)/sqrt(Kcv);
	Free(X);
	Free(Xtrain);
	Free(Xtest);
	Free(Ytrain);
	Free(Ytest);
	Free(SSE);
	Free(Intercept);
	Free(Beta);
}
			

double norm(double*X,int N)
{
	int inci = 1;
	int incj = 1;
	double *readPtr1, *readPtr2,temp;
	readPtr1 = &X[0];
	readPtr2 = &X[0];
	temp = F77_CALL(ddot)(&N,readPtr1,&inci,readPtr2,&incj);	//res = ddot(n, x, incx, y, incy)
	temp = sqrt(temp);
	return temp;
}

//Projection function
void ProjectCorr(int *n, int *p,double*y0,double*BASIS,
		double*lambdaMax, int *epistasis)
{
	int N 				= n[0];
	int K 				= p[0];	
	int epis 			= *epistasis;
	double *y			= (double * ) Calloc(N, double);
	double *z 			= (double * ) Calloc(N, double);

	int i,j,l;	
	double normY, normX;
	normY = norm(y0,N);
	for(i=0;i<N;i++) y[i]	= y0[i]/normY;
	//compute abscor vector
	double *readPtr1,*readPtr2, corXY;
	int inci 				= 1;
	int incj 				= 1;
	lambdaMax[0] 			= 0;
	for(i =0;i<K;i++)
	{
		//1. center x;
		readPtr1 			= &BASIS[i*N];
		normX = norm(readPtr1,N);
		for(l=0;l<N;l++) z[l] = readPtr1[l]/normX;
		//2. compute corr;
		corXY = F77_CALL(ddot)(&N,z,&inci,y,&incj);
		if(corXY>lambdaMax[0]) lambdaMax[0] = corXY;
		
		//interactions
		if(epis!=0 && i<(K-1))
		{
			for(j=(i+1);j<K;j++)
			{
				readPtr2 	= &BASIS[j*N];
				//1.
				for(l=0;l<N;l++) z[l] 	= readPtr1[l]*readPtr2[l];
				normX = norm(z,N);				
				for(l=0;l<N;l++) z[l] = z[l]/normX;
				//2. 
				corXY = F77_CALL(ddot)(&N,z,&inci,y,&incj);
				if(corXY>lambdaMax[0]) lambdaMax[0] = corXY;		
			}//j<K			
		}//if i<(K-1)		
	}//i
	//free memory
	Free(y);
	Free(z);
}
