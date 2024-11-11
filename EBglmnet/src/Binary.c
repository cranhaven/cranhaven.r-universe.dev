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
#include "Binary.h"


//B: binomial; m: main; Neg: normal + exp. + gamma
void LinearSolver(double * a, double *logout, int N, int M,double *output)
{
	const int nrhs		= 1;
	const double Rcond	= 1e-5;
	int rank			= M;
	int *jpvt;
	jpvt				= (int * ) Calloc(M,int);
	const int lwork	= M*N + 4*N;
	double * work;
	work				= (double *) Calloc(lwork,double);

	int info			= 0;
	// *************************Call LAPACK library ************************
	F77_CALL(dgelsy)(&N, &M, &nrhs, a, &N, logout, &N,jpvt,&Rcond, &rank, work, &lwork,&info);
	if(info!=0) 	
	{
		Rprintf("Call linear solver degls error!\n");
		return;
	}
	int i;
	for (i=0;i<M;i++) output[i] = logout[i];
	Free(jpvt);
	Free(work);		
}

void fEBInitialization(double *Alpha, double * PHI2, int *Used, int *Unused, double *Mu2,
				double *BASIS, double *Targets, double *Scales, int * initial, int N, int *m, int K)
{
    //basis dimension
    int M,M_full,N_used,i,j,kk;
  	
	M_full					= K;
	int IniLogic			= *initial;
    //INPUT
    if(IniLogic==0)							// is empty
    {
		m[0]				= 2;
		M					= m[0];
        N_used				= 1;
    }else									// not empty
    {
       	N_used              = m[0]-1;
		M					= m[0];
    }
    //output
    const double init_alpha_max     = 1e3;
    const double init_alpha_min     = 1e-3;    
    
	if(IniLogic==0)            // is empty
    {
		//Rprintf("\t Inside Initialization, M: %d, K: %d\n",M, K);
        double *TargetPseudo,proj_ini,proj;
        int loc1 = 0;
        TargetPseudo		= (double *) Calloc(N,double);
        for(i=0;i<N;i++)            TargetPseudo[i]     = 2*Targets[i] -1;
        proj_ini			= 0;
		Used[0]				= 1;
        for(i=0;i<K;i++)
        {

         	proj			= 0;
            for(j=0;j<N;j++)                proj    = proj + BASIS[i*N+j]*TargetPseudo[j];
            proj			= proj/Scales[i];
            if(fabs(proj) > fabs(proj_ini))
            {
                proj_ini    = proj;
                loc1        = i;
                Used[0]		= i + 1;
            }
        }
        
        //PHI2, duplicate for linear solver
		double *PHIqr;
		PHIqr					= (double *) Calloc(N*M,double);
   	for(i=0;i<N;i++)		
		{
			PHI2[i]         = 1;
			PHIqr[i]		= 1;
		}
        
    //
    double * PHI;
    PHI						= (double *) Calloc(N,double);

    for(i=0;i<N;i++)
    {
        PHI[i]			= BASIS[loc1*N+i]/Scales[loc1];
        PHI2[N+i]		= PHI[i];
		    PHIqr[N+i]		= PHI[i];
      }
        
		  double *logout;
      logout                      = (double *) Calloc(N,double);
      for (i=0;i<N;i++)            logout[i]               = log(((TargetPseudo[i] * 0.9 + 1)/2)/(1-(TargetPseudo[i] * 0.9 + 1)/2));
		// Call function
		LinearSolver(PHIqr, logout, N, M, Mu2);

        if(Mu2[1] == 0) Alpha[0] 	= 1;
        else            Alpha[0]    = 1/(Mu2[1]*Mu2[1]);
        if(Alpha[0]< init_alpha_min) Alpha[0]				= init_alpha_min;
        if(Alpha[0]> init_alpha_max) Alpha[0]				= init_alpha_max;
	
		Free(TargetPseudo);
		Free(PHIqr);
		Free(PHI);
		Free(logout);
	
	
	
	}

	int IsUsed			= 0;
    kk                  = 0;
    for(i=0;i<M_full;i++)
    {
        IsUsed          = 0;
        for(j=0;j<N_used;j++)
        {
            if ((i+1)==Used[j])     IsUsed  = 1;
        }
        if(IsUsed==0)     
        {
            Unused[kk]  = (i+ 1);
            kk          = kk + 1;
        }
    }
}


void fEBSigmoid(double * y, double * PHI_Mu,int N) // from left to right 
{
	int i;
    for (i=0;i<N;i++)	y [i]	= 1/(1+exp(-PHI_Mu[i]));

}

double fEBDataError(double dataError,double *y,double *PHI_Mu,double *Targets,int N)
{
	int i;
    dataError					= 0;		// scalar
	fEBSigmoid(y,PHI_Mu,N);
    for (i = 0;i<N;i++)
    {
        if(y[i]!=0)		dataError   = dataError - Targets[i]*log(y[i]);
        if (y[i]!=1)	dataError   = dataError - (1-Targets[i])*log(1-y[i]);
    }   
	return dataError;
}

void MatrixInverse(double * a,int N)
{
	const char uplo			= 'U';
	int info				= 0;
	//*************************Call LAPACK library ************************
	F77_CALL(dpotrf)(&uplo, &N,a, &N,&info FCONE);
	if(info!=0) 	
	{
		Rprintf("Call 1st function. dpotrf error, Ill-conditioned Hessian!\n");
		return;
	}
	F77_CALL(dpotri)(&uplo,&N,a,&N,&info FCONE);
		if(info!=0) 	
	{
		Rprintf("Call 2nd function dpotri error!\n");
		return;
	}
	//a is upper triangular
	int i,j;
	for (i=1;i<N;i++)
	{
		for(j=0;j<i;j++)	a[j*N+i]	= a[i*N+j];
	}
}

void fEBCatPostMode(double * Mu2, double *beta,double *SIGMA2, double * H2, double *PHI2,
								double *Targets, double *Alpha,int N, int M)
{
	int i,j,k,L;
    //control parameters
    const double GRADIENT_MIN  	= 1e-6;
    double temp                 = pow(2.0,8);
    const double STEP_MIN       = 1/temp;
    const int   itsMax    		= 25;

    double *PHI_Mu, *y;
	PHI_Mu						= (double *) Calloc(N,double);
    for(i = 0;i<N;i++)
    {
        PHI_Mu[i]				= 0;
        for(j = 0;j<M;j++)		PHI_Mu[i]	= PHI_Mu[i] + PHI2[j*N+i]*Mu2[j];
    }
	double dataError			= 0;
	y							= (double *) Calloc(N,double);
	dataError					= fEBDataError(dataError,y,PHI_Mu,Targets,N);    
    //
    double regulariser			= 0;
    for(i = 1;i<M;i++)			regulariser		= regulariser + Alpha[i-1]*Mu2[i]*Mu2[i]/2;
    double newTotalError,g0,h0;
    newTotalError				= regulariser + dataError;
    double * errorLog,*e,*g2;
    errorLog					= (double *) Calloc(itsMax,double);
	e							= (double *) Calloc(N,double);
	g2							= (double *) Calloc(M,double);
    //
    g0							= 0;
    h0							= 0;
    //setup H
	int countGrad;
    double *DeltaMu, *Mu_new2;
    DeltaMu						= (double *) Calloc(M,double);
    Mu_new2						= (double *) Calloc(M,double);
    double step					= 1.0;

    //*****************************************************************************
    // Loops start here 
    for(i = 0;i<itsMax;i++)
    {
        errorLog[i]				= newTotalError;    //log error value
        //Gradient
        g0						= 0;
        h0						= 0;
        for(j = 0;j<N;j++)
        {
            e[j]				= Targets[j]-y[j];
            g0					= g0 + e[j];
            beta[j]				= y[j]*(1-y[j]);
            h0					= h0 + beta[j];
        }
        g2[0]					= g0;
        H2[0]					= h0;
        // first row and first column of H
        for(j=1;j<M;j++)
        {
            g2[j]				= 0;
            H2[j]				= 0;
            for(k=0;k<N;k++)
            {
                g2[j]			= g2[j] + PHI2[j*N+k]*e[k]; //Beta2 =Mu2
                H2[j]			= H2[j] +beta[k]*PHI2[j*N+k];
            }
            g2[j]				= g2[j]-Alpha[j-1]*Mu2[j];
            H2[j*M]				= H2[j];
        }
        //Hessian
        //h01
        //H;
        for(j=1;j<M;j++)
        {
            for(k = 1;k<M;k++)
            {
                H2[k*M+j]       = 0;
                for(L = 0;L<N;L++)		H2[k*M+j]   = H2[k*M+j] + PHI2[j*N+L]*beta[L]*PHI2[k*N+L];
                if(j==k)                H2[k*M+j]   = H2[k*M+j] + Alpha[k-1];
            }
        }
		//save a copy of H
		for(j=0;j<M;j++)
		{
			for (k=0;k<M;k++)	SIGMA2[k*M+j] = H2[k*M+j];
		}

		MatrixInverse(SIGMA2,M);				//inverse of H2 is needed for wald score
        countGrad               = 0;
        for(j = 0;j<M;j++) 
		{
            if(fabs(g2[j])<GRADIENT_MIN)	countGrad++;
		}
        if(countGrad==M)						//end loop
        {
            for (k = 0;k<M;k++)
            {
                DeltaMu[k]      = 0;
                for(L = 0;L<M;L++)	DeltaMu[k]  = DeltaMu[k] + SIGMA2[L*M+k]*g2[L];
            }
         
            break;
        }
        //Comput Full Newton step H^(-1)*g;            
         //sigma*g
		for (k = 0;k<M;k++)
		{
                DeltaMu[k]      = 0;
                for(L=0;L<M;L++)	DeltaMu[k]  = DeltaMu[k] + SIGMA2[L*M+k]*g2[L];
		}
        //
        step					= 1;
        while (step>STEP_MIN)
        {
            for(j=0;j<M;j++)	Mu_new2[j]      = Mu2[j] + step*DeltaMu[j];
            //
            for(j=0;j<N;j++)
            {
                PHI_Mu[j]       = 0;
                for(k=0;k<M;k++)	PHI_Mu[j]   = PHI_Mu[j] + PHI2[k*N+j]*Mu_new2[k];
            }
            dataError			= fEBDataError(dataError,y,PHI_Mu,Targets,N);
            //regulariser update            
            regulariser			= 0;
            for(j=1;j<M;j++)	regulariser  	= regulariser + Alpha[j-1]*Mu_new2[j]*Mu_new2[j]/2;
            newTotalError       = dataError + regulariser;
            if(newTotalError>=errorLog[i])      step		= step/2;
            else
            {
                for(j=0;j<M;j++)				Mu2[j]		= Mu_new2[j];
                step            =0;
            }
        }//end of while
        if(step==1) break;
    }
		Free(PHI_Mu);
	Free(y);	
	Free(errorLog);
	Free(e);	
	Free(g2);
	Free(DeltaMu);	
	Free(Mu_new2);
	
}

void fEBCatFullStat(double * beta, double * SIGMA2, double * H2, double *S_in, double * Q_in, 
				double * S_out, double * Q_out,  double *BASIS,double * Scales, double *PHI2, 
				double * Targets, int * Used, double *Alpha, double * Mu2, double * BasisCache,
				int *n, int *m, int* kdim)
{
    //basis dimension
    int N,K,M,i,j,p;
   	N					= *n;			// row number
    K					= *kdim;		// column number
    M					= *m;

	fEBCatPostMode(Mu2, beta,SIGMA2, H2, PHI2,Targets, Alpha,N, M);

    double *PHI_Mu,*y;  
    PHI_Mu        		= (double *) Calloc(N,double);
	y					= (double *) Calloc(N,double);
    for(i=0;i<N;i++)
    {
        PHI_Mu[i]		= 0;
        for(j=0;j<M;j++)            PHI_Mu[i]           = PHI_Mu[i] + PHI2[j*N+i]*Mu2[j];
    }
	fEBSigmoid(y, PHI_Mu,N);
        
    //e=Targets-y
    double *e;
    e					= (double *) Calloc(N,double);
    for(i=0;i<N;i++)    e[i]        = Targets[i] - y[i];
        
    //Main loop
    double *BPvector,*temp,*temp1,*temp2;
    BPvector			= (double *) Calloc(M,double);
    temp				= (double *) Calloc(M,double);
    temp1				= (double *) Calloc(M*N,double);
    temp2				= (double *) Calloc(N,double);
    double tempSum,BBsquare,tempZE;
    for(i=0;i<K;i++)
    {
        for(p=0;p<M;p++)
        {
            BPvector[p]     = 0;
            for(j=0;j<N;j++)
            {
                temp1[p*N+j]= BASIS[i*N+j]*PHI2[p*N+j]*beta[j];
                BPvector[p] = BPvector[p] + temp1[p*N+j];
            }
            BPvector[p]     = BPvector[p]/Scales[i];
        }

        tempSum             = 0;
		for(p=0;p<M;p++)
        {
            temp[p]      	= 0;
            for(j=0;j<M;j++)                temp[p]     = temp[p] + BPvector[j]*SIGMA2[p*M+j];
            temp[p]         = temp[p]*BPvector[p];
            tempSum         = tempSum + temp[p];
        }
        BBsquare            = 0;
        tempZE              = 0;
        for(p=0;p<N;p++)
        {
            BBsquare        = BBsquare + beta[p]*BasisCache[i*N+p];
            temp2[p]           = BASIS[i*N+p]*e[p];
            tempZE        	= tempZE + temp2[p];
        }
        S_in[i]             = BBsquare/(Scales[i]*Scales[i])-tempSum;
        Q_in[i]             = tempZE/Scales[i];

        S_out[i]            = S_in[i];
        Q_out[i]            = Q_in[i];
        
    }// Main loop ends
                    
    int N_used,index;
    N_used						= M-1;
    for(i=0;i<N_used;i++)
    {
        index					= Used[i] -1;
        S_out[index]			= Alpha[i]*S_in[index]/(Alpha[i]-S_in[index]);
        Q_out[index]			= Alpha[i]*Q_in[index]/(Alpha[i]-S_in[index]);
	}
	Free(PHI_Mu);
	Free(y);
	Free(e);
	Free(BPvector);
	Free(temp);
	Free(temp1);
	Free(temp2);

}



