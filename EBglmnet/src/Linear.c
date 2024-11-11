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



void fEBInitialization_Gauss(double *Alpha, double *PHI, int *Used, int *Unused, double *BASIS, 
			double *Targets, double *Scales, int * initial, int N, int *m, int K, double *beta)
{
    //basis dimension
    int M,M_full,i,j,kk,index;
	M_full					= K;
	int IniLogic			= *initial;
    //INPUT
    if(IniLogic==0)							// is empty
    {
		m[0]				= 1;
		M					= m[0];
    }else									// not empty
    {
       	M					= m[0];
    }
    //output
    const double init_alpha_max     = 1e3;
	//lapack
	int inci =1;
	int incj =1;
	double *readPtr1;
	double b_blas = 1;
	//lapack end
    
	if(IniLogic==0)            // is empty
    {
        double proj_ini,proj;
        int loc1			= 0;
		int loc2			= 0;
        proj_ini			= 0;
		Used[0]				= 1;
        for(i=0;i<K;i++)
        {
         	proj			= 0;
			readPtr1 = &BASIS[i*N];
			proj = F77_CALL(ddot)(&N, readPtr1, &inci,Targets, &incj);
            proj			= proj/Scales[i];
            if(fabs(proj) > fabs(proj_ini))
            {
                proj_ini    = proj;
                loc1        = i;
                loc2        = i;
                Used[0]		= i + 1;
            }
        }
        //PHI2, duplicate for linear solver
        
        //
        if(loc1==loc2)
        {
			readPtr1 		= &BASIS[loc1*N];
			F77_CALL(dcopy)(&N,readPtr1,&inci,PHI,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
			b_blas 			= 1/Scales[loc1];
			F77_CALL(dscal)(&N,&b_blas,PHI,&inci); 		//dscal(n, a, x, incx) x = a*x	
        }else
        {
            index				= Used[0] -1;
           	for(i=0;i<N;i++)                PHI[i]			= BASIS[loc1*N+i]*BASIS[loc2*N+i]/Scales[index];
        }
		
		// beta
		double stdT;
		stdT					= varTargets(Targets,N);
		stdT					= sqrt(stdT);
		if (stdT<1e-6)	stdT	= 1e-6;
		beta[0]					= 1/pow((stdT*0.1),2);

		//Alpha
		double p,q;
		p						= 0;
		q						= 0;

		p = F77_CALL(ddot)(&N, PHI, &inci,PHI, &incj);
		q = F77_CALL(ddot)(&N, PHI, &inci,Targets, &incj);
		p						= p*beta[0];
		q						= q*beta[0];
		Alpha[0]				= p*p/(q*q-p);
		
        if(Alpha[0]< 0) Alpha[0]				= init_alpha_max;
        if(Alpha[0]> init_alpha_max) Alpha[0]				= init_alpha_max;

	}

	int IsUsed					= 0;
    kk							= 0;
    for(i=0;i<M_full;i++)
    {
        IsUsed					= 0;
        for(j=0;j<M;j++)
        {
            //index   = Used[j];
            if ((i+1)==Used[j])     IsUsed  = 1;
        }
        if(IsUsed==0)     
        {
            Unused[kk]		= (i+ 1);
            kk				= kk + 1;
        }
    }
}



void CacheBP(double **BASIS_PHI, double *BASIS_Targets, double *BASIS, double *PHI,
                   double *Targets, double *scales,int N,int K,int M,int M_full)
{
  double	zTargets;
  double *z2					= (double *) Calloc(M,double);
  double *cache1				= (double *) Calloc(N,double);
  double *cache2				= (double *) Calloc(N*M,double);
  
  
  int i,h,l;
  //int kk						= K;
  
  //part 1 1-k
  for (i						=0;i<K;i++)
  {
    for(l=0;l<M;l++)
    {
      z2[l]					= 0;
      for(h=0;h<N;h++)
      {
        cache2[h*M+l]		= PHI[l*N+h] * BASIS[i*N+ h];
        z2[l]				= z2[l] + cache2[h*M+l];
      }
      BASIS_PHI[l][i]	= (z2[l]/scales[i]);
    }
    
    zTargets					= 0;
    for(l=0;l<N;l++) 
    {
      cache1[l]				= BASIS[i*N+ l]*Targets[l];
      zTargets				= zTargets + cache1[l];	
    }
    BASIS_Targets[i]			= zTargets/scales[i];
    
  }
  Free(z2);
  Free(cache1);
  Free(cache2);
}



void CacheBPGmNeg(double *BASIS_PHI, double *BASIS_Targets, double *BASIS, double *PHI,
				double *Targets, double *scales,int N,int K,int M,int M_full)
{
	double	zTargets;
	double *z2					= (double *) Calloc(M,double);
	double *cache1				= (double *) Calloc(N,double);
	double *cache2				= (double *) Calloc(N*M,double);

	int i,h,l;

	//part 1 1-k
	for (i						=0;i<K;i++)
	{
		for(l=0;l<M;l++)
		{
			z2[l]					= 0;
			for(h=0;h<N;h++)
			{
				cache2[h*M+l]		= PHI[l*N+h] * BASIS[i*N+ h];
				z2[l]				= z2[l] + cache2[h*M+l];
			}
			BASIS_PHI[l*M_full+i]	= z2[l]/scales[i];
		}

		zTargets					= 0;
		for(l=0;l<N;l++) 
		{
			cache1[l]				= BASIS[i*N+ l]*Targets[l];
			zTargets				= zTargets + cache1[l];	
		}
		BASIS_Targets[i]			= zTargets/scales[i];

	}
	Free(z2);
	Free(cache1);
	Free(cache2);

}



void fEBLinearFullStat(double *beta, double * SIGMA, double *H, double *S_in, double * Q_in, double * S_out, 
				double * Q_out,   double *BASIS, double * Scales, double *PHI, double **BASIS_PHI,
				double *BASIS_Targets, double * Targets, int * Used, double *Alpha, double * Mu, 
				 double *gamma,int *n, int *m, int* kdim, int *iteration,int *i_iter)
{
    //basis dimension
    int N,K,M,i,j,p;
   	N					= *n;			// row number
    K					= *kdim;		// column number
	int M_full;
	M_full 				= K;
    M					= *m;
	//lapack
	int inci 			=1;
	int incj 			=1;
	//double *readPtr1;
	double a_blas 		= 1;
	double b_blas 		= 1;

	char transa 		= 'N';

	//int lda;
	//lapack end
	
	if(iteration[0]		== 1 && i_iter[0]==0)			// initialize SIGMA
	{
		H[0]			= 0;
		H[0] = F77_CALL(ddot)(&N, PHI, &inci,PHI, &incj);
		H[0]			=	H[0]* beta[0] + Alpha[0];
		
		//save a copy of H
		SIGMA[0]		= 1/H[0];

	}

	double * PHIt		= (double *) Calloc(M,double);

	transa = 'T';
	a_blas = 1;
	b_blas = 0;
	//lda 	= N;
	F77_CALL(dgemv)(&transa, &N, &M,&a_blas, PHI, &N, Targets, &inci, &b_blas,PHIt, &incj FCONE);

	transa = 'N';
	//lda = M;
	F77_CALL(dgemv)(&transa, &M, &M,&a_blas, SIGMA, &M, PHIt, &inci, &b_blas,Mu, &incj FCONE);
	b_blas = beta[0];
	F77_CALL(dscal)(&M,&b_blas,Mu,&inci); //dscal(n, a, x, incx)
	
	//gamma
	for(i=1;i<M;i++)	gamma[i]	= 1- SIGMA[i*M+i] *Alpha[i];

    //Main loop
    double *BPvector;
    BPvector			= (double *) Calloc(M,double);
    double tempSum,tempBPMu;

    for(i=0; i<M_full; i++)
    {
		for(j=0;j<M;j++)
		{
			BPvector[j]			= 0;
		for(p=0;p<M;p++)	BPvector[j]		= BPvector[j] + BASIS_PHI[p][i]*SIGMA[j*M+p]; 
		}

		
        tempSum					= 0;
		for(j=0;j<M;j++)		tempSum			= tempSum + BPvector[j]*BASIS_PHI[j][i];
		
		S_in[i]					= beta[0] - beta[0]*tempSum*beta[0];
		tempBPMu				= 0;
		for(p=0;p<M;p++) tempBPMu = tempBPMu + BASIS_PHI[p][i] *Mu[p];
		
		Q_in[i]					= beta[0]*(BASIS_Targets[i] - tempBPMu);
    }// Main loop ends
	F77_CALL(dcopy)(&M_full,S_in,&inci,S_out,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	F77_CALL(dcopy)(&M_full,Q_in,&inci,Q_out,&incj);  
	
    int index;

    for(i=0;i<M;i++)
    {
        index					= Used[i] -1;
        S_out[index]			= Alpha[i]*S_in[index]/(Alpha[i]-S_in[index]);
        Q_out[index]			= Alpha[i]*Q_in[index]/(Alpha[i]-S_in[index]);
	}
	Free(PHIt);	
	Free(BPvector);
}


void fEBLinearFullStatGmNeg(double *beta, double * SIGMA, double *H, double *S_in, double * Q_in, double * S_out, 
				double * Q_out,   double *BASIS, double * Scales, double *PHI, double *BASIS_PHI,
				double *BASIS_Targets, double * Targets, int * Used, double *Alpha, double * Mu, 
				 double *gamma,int *n, int *m, int* kdim, int *iteration,int *i_iter)
{
    //basis dimension
    int N,K,M,i;
   	N					= *n;			// row number
    K					= *kdim;		// column number
	int M_full;
	M_full 				= K;
    M					= *m;
		//lapack
	int inci 			=1;
	int incj 			=1;
	double *readPtr1;
	double a_blas 		= 1;
	double b_blas 		= 1;

	char transa 		= 'N';

	//int lda;
	//lapack end

	if(iteration[0]		== 1 && i_iter[0]==0)			// initialize SIGMA
	{
		H[0]			= 0;
		H[0] = F77_CALL(ddot)(&N, PHI, &inci,PHI, &incj);
		
		H[0]			=	H[0]* beta[0] + Alpha[0];
		
		//save a copy of H
		SIGMA[0]		= 1/H[0];
	}


	double * PHIt		= (double *) Calloc(M,double);

	transa = 'T';
	a_blas = 1;
	b_blas = 0;
	//lda 	= N;
	F77_CALL(dgemv)(&transa, &N, &M,&a_blas, PHI, &N, Targets, &inci, &b_blas,PHIt, &incj FCONE);

	transa = 'N';
	//lda = M;
	F77_CALL(dgemv)(&transa, &M, &M,&a_blas, SIGMA, &M, PHIt, &inci, &b_blas,Mu, &incj FCONE);
	b_blas = beta[0];
	F77_CALL(dscal)(&M,&b_blas,Mu,&inci); //dscal(n, a, x, incx)
	
	//gamma
	for(i=1;i<M;i++)	gamma[i]	= 1- SIGMA[i*M+i] *Alpha[i];

    //Main loop
        //temp parameters: BPvector
    double *BPvector;
    BPvector			= (double *) Calloc(M,double);
    double tempSum,tempBPMu;

    for(i=0; i<M_full; i++)
    {
		readPtr1 	= &BASIS_PHI[i];
		b_blas 		= 0;
		F77_CALL(dgemv)(&transa, &M, &M,&a_blas, SIGMA, &M, readPtr1, &M_full, &b_blas,BPvector, &incj FCONE);
		
		
		
        tempSum					= 0;
		readPtr1 	= &BASIS_PHI[i];
		tempSum		= F77_CALL(ddot)(&M, BPvector, &inci,readPtr1, &M_full);
		
		S_in[i]					= beta[0] - beta[0]*tempSum*beta[0];
		tempBPMu				= 0;
		readPtr1 	= &BASIS_PHI[i];
		tempBPMu 	= F77_CALL(ddot)(&M, Mu, &inci,readPtr1, &M_full);		
		Q_in[i]					= beta[0]*(BASIS_Targets[i] - tempBPMu);

    }// Main loop ends
	F77_CALL(dcopy)(&M_full,S_in,&inci,S_out,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	F77_CALL(dcopy)(&M_full,Q_in,&inci,Q_out,&incj);                 
    int index;

    for(i=0;i<M;i++)
    {
        index					= Used[i] -1;
        S_out[index]			= Alpha[i]*S_in[index]/(Alpha[i]-S_in[index]);
        Q_out[index]			= Alpha[i]*Q_in[index]/(Alpha[i]-S_in[index]);

	}
	Free(PHIt);	
	Free(BPvector);
 
}



// same in binary: void MatrixInverse(double * a,int N);


//888888888888888888888888888888888888888888888888888888888888888888888888888888888888
int ActionAdd(double **BASIS_PHI, double* BASIS, double*scales, double*PHI, double*Phi,
			double *beta, double* Alpha, double newAlpha, double*SIGMA, double*Mu, double*S_in,
			double*Q_in, int nu, double*SIGMANEW, int M_full,int N, int K, int M)
{
	double *BASIS_Phi		= (double *) Calloc(M_full,double);
	double *BASIS_B_Phi		= (double *) Calloc(M_full,double);
	double *mCi				= (double *) Calloc(M_full,double);
	double *z				= (double *) Calloc(N,double);
	//int kk					= K;
	int i,j,h;
	int index				= M + 1;
	double*   	tmp			= (double *) Calloc(M,double);
  	double*		tmpp		= (double *) Calloc(M,double);
	double s_ii,mu_i,TAU;
	//lapack
	int inci =1;
	int incj =1;
	double *readPtr1;
	double b_blas = 1.0;

	//lapack end
	
	for (i					= 0;i<K;i++)
	{
		BASIS_Phi[i]		= 0;
		for(h=0;h<N;h++)
		{
			z[h]			= BASIS[i*N+h]*Phi[h];
			BASIS_Phi[i]	= BASIS_Phi[i] + z[h];
		}
		BASIS_Phi[i]		= BASIS_Phi[i]/scales[i];
		BASIS_B_Phi[i]		= beta[0]*BASIS_Phi[i];
		//		
	}
	for(i=0;i<M;i++)
    {
        tmp[i]					= 0;
		readPtr1 	= &PHI[i*N];
		tmp[i]  = F77_CALL(ddot)(&N, readPtr1, &inci,Phi,&incj);
    }
	b_blas = beta[0];
	F77_CALL(dscal)(&M,&b_blas,tmp,&inci); 		//dscal(n, a, x, incx) x = a*x
	
	
    for(i=0;i<M;i++)
    {
        tmpp[i]					= 0;
		readPtr1 	= &SIGMA[i*M];
		tmpp[i]  = F77_CALL(ddot)(&M, readPtr1, &inci,tmp,&incj);	
    }//tmpp is tmp in matlab.

	Alpha[M]				= newAlpha;                 //new element

	readPtr1 	= &PHI[M*N];
	F77_CALL(dcopy)(&N,Phi,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x

	s_ii					= 1.0/(newAlpha + S_in[nu]);   
	
	mu_i					= s_ii*Q_in[nu];
	b_blas = -mu_i;
	F77_CALL(daxpy)(&M, &b_blas,tmpp, &inci,Mu, &incj); 
	
    Mu[M]					= mu_i;							//new element
	
	double * s_i			= (double *) Calloc(M,double);
	F77_CALL(dcopy)(&M,tmpp,&inci,s_i,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	b_blas = -s_ii;
	F77_CALL(dscal)(&M,&b_blas,s_i,&inci); 		//dscal(n, a, x, incx) x = a*x
	

	for(i=0;i<M;i++)
	{
		for(j=0;j<M;j++)
		{
			TAU				= -s_i[i]*tmpp[j];
			SIGMANEW[j*index+i]					= SIGMA[j*M+i] + TAU;
		}
	}

	readPtr1 = &SIGMANEW[M*index];
	F77_CALL(dcopy)(&M,s_i,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	readPtr1 = &SIGMANEW[M];
	F77_CALL(dcopy)(&M,s_i,&inci,readPtr1,&index);  //dcopy(n, x, incx, y, incy) ---> y = x
	
	SIGMANEW[M*index+M]		= s_ii;

	double temp;
	for(i=0;i<M_full;i++)
	{
		temp				= 0;
		for(j=0;j<M;j++)	temp				= temp + BASIS_PHI[j][i]*tmpp[j];
		
		mCi[i]				= BASIS_B_Phi[i] -  beta[0]*temp;
		S_in[i]				= S_in[i] - mCi[i]*mCi[i]*s_ii;
		Q_in[i]				= Q_in[i] - mu_i*mCi[i];
	}
	BASIS_PHI[M]=BASIS_Phi;
	int UPDATE_REQUIRED		= 1;
	Free(BASIS_B_Phi);	
	Free(mCi);
	Free(z);	
	Free(tmp);
	Free(tmpp);	
	Free(s_i);
	
	return  UPDATE_REQUIRED; 
 }


int ActionAddGmNeg(double *BASIS_PHI, double* BASIS, double*scales, double*PHI, double*Phi,
			double *beta, double* Alpha, double newAlpha, double*SIGMA, double*Mu, double*S_in,
			double*Q_in, int nu, double*SIGMANEW, int M_full,int N, int K, int M)
{
	double *BASIS_Phi		= (double *) Calloc(M_full,double);
	double *BASIS_B_Phi		= (double *) Calloc(M_full,double);
	double *mCi				= (double *) Calloc(M_full,double);
	double *z				= (double *) Calloc(N,double);
	///int kk					= K;
	int i,j,h;
	int index				= M + 1;
	double*   	tmp			= (double *) Calloc(M,double);
  	double*		tmpp		= (double *) Calloc(M,double);
	double s_ii,mu_i,TAU;
	//lapack
	int inci =1;
	int incj =1;
	double *readPtr1;
	double b_blas = 1.0;

	//lapack end
	for (i					= 0;i<K;i++)
	{
		BASIS_Phi[i]		= 0;
		for(h=0;h<N;h++)
		{
			z[h]			= BASIS[i*N+h]*Phi[h];
			BASIS_Phi[i]	= BASIS_Phi[i] + z[h];
		}
		BASIS_Phi[i]		= BASIS_Phi[i]/scales[i];
		BASIS_B_Phi[i]		= beta[0]*BASIS_Phi[i];
		//		
	}
	for(i=0;i<M;i++)
    {
        tmp[i]					= 0;
		readPtr1 	= &PHI[i*N];
		tmp[i]  = F77_CALL(ddot)(&N, readPtr1, &inci,Phi,&incj);
    }
	b_blas = beta[0];
	F77_CALL(dscal)(&M,&b_blas,tmp,&inci); 		//dscal(n, a, x, incx) x = a*x
	
    for(i=0;i<M;i++)
    {
        tmpp[i]					= 0;
		readPtr1 	= &SIGMA[i*M];
		tmpp[i]  = F77_CALL(ddot)(&M, readPtr1, &inci,tmp,&incj);		
    }//tmpp is tmp in matlab.
	Alpha[M]				= newAlpha;                 //new element

	readPtr1 	= &PHI[M*N];
	F77_CALL(dcopy)(&N,Phi,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x

	
	s_ii					= 1.0/(newAlpha + S_in[nu]);   
	
	mu_i					= s_ii*Q_in[nu];
	b_blas = -mu_i;
	F77_CALL(daxpy)(&M, &b_blas,tmpp, &inci,Mu, &incj); 
	
    Mu[M]					= mu_i;							//new element
	
	double * s_i			= (double *) Calloc(M,double);
	F77_CALL(dcopy)(&M,tmpp,&inci,s_i,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	b_blas = -s_ii;
	F77_CALL(dscal)(&M,&b_blas,s_i,&inci); 		//dscal(n, a, x, incx) x = a*x
	

	for(i=0;i<M;i++)
	{
		for(j=0;j<M;j++)
		{
			TAU				= -s_i[i]*tmpp[j];
			SIGMANEW[j*index+i]					= SIGMA[j*M+i] + TAU;
		}
	}

	readPtr1 = &SIGMANEW[M*index];
	F77_CALL(dcopy)(&M,s_i,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	readPtr1 = &SIGMANEW[M];
	F77_CALL(dcopy)(&M,s_i,&inci,readPtr1,&index);  //dcopy(n, x, incx, y, incy) ---> y = x
	
	SIGMANEW[M*index+M]		= s_ii;

	double temp;
	for(i=0;i<M_full;i++)
	{
		temp				= 0;
		readPtr1 			= &BASIS_PHI[i];
		temp 				= F77_CALL(ddot)(&M, readPtr1, &M_full,tmpp,&incj); 
		
		mCi[i]				= BASIS_B_Phi[i] -  beta[0]*temp;
		BASIS_PHI[M*M_full + i]					= BASIS_Phi[i];
		S_in[i]				= S_in[i] - mCi[i]*mCi[i]*s_ii;
		Q_in[i]				= Q_in[i] - mu_i*mCi[i];
	}
	
	int UPDATE_REQUIRED		= 1;
	Free(BASIS_Phi);
	Free(BASIS_B_Phi);	
	Free(mCi);
	Free(z);	
	Free(tmp);
	Free(tmpp);	
	Free(s_i);
	
	return  UPDATE_REQUIRED; 
 }
 

int ActionDel(double*PHI, double*Alpha, double*SIGMA, double*SIGMANEW, double**BASIS_PHI,
		double*Mu, double*S_in, double*Q_in, double *beta, int jj, int N, int M, int M_full)
 {
	 int i,j;
	int index				= M - 1;
	//lapack
	int inci =1;
	int incj =1;
	double *readPtr1, *readPtr2;
	//lapack end
	
	
	//
	Alpha[jj]				= Alpha[index];           //Alpha: M -> M - 1
            
	readPtr1 = &PHI[jj*N];
	readPtr2 = &PHI[index*N];
	F77_CALL(dcopy)(&N,readPtr2,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x

    int Mujj;        
	Mujj					= Mu[jj];
	for(i=0;i<M;i++)  Mu[i] = Mu[i] - Mujj*SIGMA[jj*M +i]/SIGMA[jj*M + jj];

	//
	Mu[jj]					= Mu[index];        
	//------------------------------------------------------------------------------
	double *tempSIGMA = (double *) Calloc((M*M),double);
	for(i=0;i<M;i++)
	{
		for(j=0;j<M;j++)	tempSIGMA[j*M + i]	= SIGMA[j*M + i] - SIGMA[jj*M+i]/SIGMA[jj*M+jj]*SIGMA[jj*M+j];
	}
	for(i=0;i<index;i++)
	{
		for(j=0;j<index;j++) SIGMANEW[j*index + i]	= tempSIGMA[j*M + i];
	}
	if(jj != index)// incase the one to be deleted is the last column.
	{
	//step 1: last column
	readPtr1 = &SIGMANEW[jj*index];
	readPtr2 = &tempSIGMA[index*M];
	F77_CALL(dcopy)(&index,readPtr2,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	//step 2: prepare for last row
	tempSIGMA[jj*M + M-1] = tempSIGMA[M*M-1];

	//last step 	
	readPtr1 = &SIGMANEW[jj];
	readPtr2 = &tempSIGMA[M-1];
	F77_CALL(dcopy)(&index,readPtr2,&M,readPtr1,&index);  //dcopy(n, x, incx, y, incy) ---> y = x
	}

	
	double temp;
	for(i=0;i<M_full;i++)
	{
		temp				= 0;
		for(j=0;j<M;j++)	temp				= temp + BASIS_PHI[j][i]*SIGMA[jj*M + j];
		S_in[i]				= S_in[i] +  pow(beta[0]*temp,2)/SIGMA[jj*M + jj];
		Q_in[i]				= Q_in[i] +  beta[0]*temp *Mujj /SIGMA[jj*M + jj];
	}
	double *ptr;
	ptr=BASIS_PHI[jj];
	BASIS_PHI[jj]=BASIS_PHI[index];
	BASIS_PHI[index]=ptr;
	
	int UPDATE_REQUIRED		=1;
	Free(tempSIGMA);
	return  UPDATE_REQUIRED;
}//end of ACTION_DELETE


int ActionDelGmNeg(double*PHI, double*Alpha, double*SIGMA, double*SIGMANEW, double*BASIS_PHI,
		double*Mu, double*S_in, double*Q_in, double *beta, int jj, int N, int M, int M_full)
 {
	 int i,j;
	int index				= M - 1;
	//lapack
	int inci =1;
	int incj =1;
	double *readPtr1, *readPtr2;
	//lapack end
	//
	Alpha[jj]				= Alpha[index];           //Alpha: M -> M - 1
            
	readPtr1 = &PHI[jj*N];
	readPtr2 = &PHI[index*N];
	F77_CALL(dcopy)(&N,readPtr2,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x

	
	
    int Mujj;        
	Mujj					= Mu[jj];
	for(i=0;i<M;i++)  Mu[i] = Mu[i] - Mujj*SIGMA[jj*M +i]/SIGMA[jj*M + jj];
	//
	Mu[jj]					= Mu[index];        
	//------------------------------------------------------------------------------
	double *tempSIGMA = (double *) Calloc((M*M),double);
	for(i=0;i<M;i++)
	{
		for(j=0;j<M;j++)	tempSIGMA[j*M + i]	= SIGMA[j*M + i] - SIGMA[jj*M+i]/SIGMA[jj*M+jj]*SIGMA[jj*M+j];
	}
	for(i=0;i<index;i++)
	{
		for(j=0;j<index;j++) SIGMANEW[j*index + i]	= tempSIGMA[j*M + i];
	}
	if(jj!=index)
	{
		//step 1: last column
		readPtr1 = &SIGMANEW[jj*index];
		readPtr2 = &tempSIGMA[index*M];
		F77_CALL(dcopy)(&index,readPtr2,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
		//step 2: prepare for last row
		tempSIGMA[jj*M + M-1] = tempSIGMA[M*M-1];

		//last step 	
		readPtr1 = &SIGMANEW[jj];
		readPtr2 = &tempSIGMA[M-1];
		F77_CALL(dcopy)(&index,readPtr2,&M,readPtr1,&index);  //dcopy(n, x, incx, y, incy) ---> y = x
	}
	
	double temp;
	for(i=0;i<M_full;i++)
	{
		temp				= 0;
		for(j=0;j<M;j++)	temp				= temp + BASIS_PHI[j*M_full + i]*SIGMA[jj*M + j];
		S_in[i]				= S_in[i] +  pow(beta[0]*temp,2)/SIGMA[jj*M + jj];
		Q_in[i]				= Q_in[i] +  beta[0]*temp *Mujj /SIGMA[jj*M + jj];
	}
	
	readPtr1 = &BASIS_PHI[jj*M_full];
	readPtr2 = &BASIS_PHI[index*M_full];
	F77_CALL(dcopy)(&M_full,readPtr2,&inci,readPtr1,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	
	int UPDATE_REQUIRED		=1;
	Free(tempSIGMA);
	return  UPDATE_REQUIRED;
}//end of ACTION_DELETE


double varTargets(double* Targets,int N)
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
	return varT;
}


void FinalUpdate(double *PHI, double *H,double *SIGMA, double *Targets,
			double *Mu, double *Alpha, double *beta, int N, int M)
{
	int i;
	//lapack
	int inci =1;
	int incj =1;

	double a_blas = 1;
	double b_blas = 1;

	int MM;
	char transa = 'T';
	char transb = 'N';
	int lda,ldb,ldc,ldk;
	//lapack end

	ldk  	= N;
	lda 	= N;
	ldb 	= N;
	ldc 	= M;
	b_blas 	= 0;	
	F77_CALL(dgemm)(&transa, &transb,&M, &M, &ldk,&a_blas, PHI, &lda, PHI, &ldb, &b_blas, H, &ldc FCONE FCONE);
	//dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
	//	C := alpha*op(A)*op(B) + beta*C,
	b_blas 	= beta[0];
	MM	 	= M*M;
	F77_CALL(dscal)(&MM,&b_blas,H,&inci); 		//dscal(n, a, x, incx) x = a*x
	
	for(i=0;i<M;i++)			H[i*M+i]		= H[i*M+i] + Alpha[i];
		
	F77_CALL(dcopy)(&MM,H,&inci,SIGMA,&incj);  //dcopy(n, x, incx, y, incy) ---> y = x
	

	MatrixInverse(SIGMA,M);				//inverse of H2 is needed for wald score	

double * PHIt				= (double *) Calloc(M,double);

	transa = 'T';
	a_blas = 1;
	b_blas = 0;
	F77_CALL(dgemv)(&transa, &N, &M,&a_blas, PHI, &N, Targets, &inci, &b_blas,PHIt, &incj FCONE);
	
	transa = 'N';
	F77_CALL(dgemv)(&transa, &M, &M,&a_blas, SIGMA, &M, PHIt, &inci, &b_blas,Mu, &incj FCONE);
	b_blas = beta[0];
	F77_CALL(dscal)(&M,&b_blas,Mu,&inci); //dscal(n, a, x, incx)
	
	Free(PHIt);	

}











