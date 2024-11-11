
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
#include "elasticNet.h"

//B: binomial; m: main; Ne: normal + exp. _EN: elasticNet




void fEBBinary_EN(int *Used, double *Mu2, double *SIGMA2, double *H2, double *Alpha, double *PHI2,
				double *BASIS, double * Targets, double *Scales, double *a_lambda, double *b_Alpha,
				int *iteration, 
				int *n, int *kdim, int *m,double * LOGlikelihood,int basisMax);

void ElasticNetBinary(double *BASIS, double * Targets, double *a_Lambda,double *b_Alpha,
				double * logLIKELIHOOD, double * Beta, double *wald,double *intercept, int *n, int *kdim);

void ElasticNetBinaryLambdaMax(double *BASIS, double * Targets, double *Lambda_max,
				double * Beta, double *wald,double *intercept, int *n, int *kdim)
{
	int N					= *n;
	int K					= *kdim;
	int M_full				= K;
	// set a limit for number of basis

	int basisMax			= 1e6/N;
	if (basisMax>M_full)	basisMax = M_full;

	double temp				= 0;
	int i,j,l;
	double *Scales			= (double * ) R_alloc(M_full, sizeof(double));

	for (i					=0;i<K;i++)
	{
		Beta[i]				= i + 1;
		Beta[M_full + i]	= i + 1;
		Beta[M_full*2 + i]	= 0;
		Beta[M_full*3 + i]	= 0;
		temp				= 0;
		for(l=0;l<N;l++)	temp		= temp + BASIS[i*N + l]*BASIS[i*N + l];
		if(temp ==0) temp	= 1;
		Scales[i]			=sqrt(temp);
	}

	//
	double *Mu2, *SIGMA2, *H2, *Alpha, *PHI2;
	int * Used, *m;
	
	Used					= (int* ) R_alloc(basisMax, sizeof(int));
	Mu2						= (double * ) R_alloc(basisMax, sizeof(double));
	SIGMA2					= (double * ) R_alloc(basisMax*basisMax, sizeof(double));
	H2						= (double * ) R_alloc(basisMax*basisMax, sizeof(double));
	Alpha					= (double * ) R_alloc(basisMax, sizeof(double));
	PHI2					= (double * ) R_alloc(N*basisMax, sizeof(double));

	m						= (int* ) R_alloc(1, sizeof(int));
	m[0]					= 2;
	/***********************/

	int M;

    double *beta			= (double *)R_alloc(N,sizeof(double));
	int *Unused				= (int *) R_alloc(M_full,sizeof(int));

    int *IniLogic;
	IniLogic				= (int*) R_alloc(1,sizeof(int));

        IniLogic[0]			= 0;
        m[0]				= 2;
		M					= m[0];


	fEBInitialization(Alpha, PHI2, Used, Unused, Mu2, BASIS, Targets, Scales, IniLogic, N, m, K);

    double *basisCache;
	basisCache				= (double *) R_alloc(N*K,sizeof(double));
    for(i=0;i<K;i++)
    {
        for(j=0;j<N;j++)	basisCache[i*N+j] = BASIS[i*N+j]*BASIS[i*N+j];
    }
	
	double *S_in, *Q_in, *S_out, *Q_out;
	S_in					= (double *) R_alloc(M_full,sizeof(double));
	Q_in					= (double *) R_alloc(M_full,sizeof(double));
	S_out					= (double *) R_alloc(M_full,sizeof(double));
	Q_out					= (double *) R_alloc(M_full,sizeof(double));

	fEBCatFullStat(beta, SIGMA2, H2, S_in, Q_in, S_out, Q_out,  BASIS,Scales, PHI2, 
				Targets, Used, Alpha, Mu2, basisCache,n, m, kdim);

	/****************************/
	int index;
	Lambda_max[0]			= 0;
	temp					= 0;
	for(index=0;index<M_full;index++)
	{
		temp				= (Q_out[index]*Q_out[index] - S_out[index])/2;
		if(temp>Lambda_max[0])			Lambda_max[0] = temp;
	}
	// wald score
	M						= m[0];	
	double *tempW			= (double * ) R_alloc(M,sizeof(double));
	wald[0]					= 0;
	index = 0;
	for(i=0;i<M;i++)
    {
        tempW[i]      		= 0;
        for(j=0;j<M;j++)    tempW[i]     = tempW[i] + Mu2[j]*H2[i*M+j];       
        wald[0]				= wald[0]	 +tempW[i]*Mu2[i];
	}
	for(i=1;i<M;i++)
	{
    // blup collection
		index				= Used[i-1] - 1;
		Beta[M_full*2 + index]	= Mu2[i]/Scales[index];
		Beta[M_full*3 + index]  = SIGMA2[i*M + i]/(Scales[index]*Scales[index]);
	}
	//
	intercept[0]			= Mu2[0];
	intercept[1]			= SIGMA2[0];
}

//APi

void ElasticNetBinary(double *BASIS, double * Targets, double *a_Lambda,double *b_Alpha,
				double * logLIKELIHOOD, double * Beta, double *wald,double *intercept, int *n, int *kdim)
{
	int N					= *n;
	int K					= *kdim;
	int M_full				= K;
	const int iter_max		= 50;
	const double err_max	= 1e-3;
	// set a limit for number of basis

	int basisMax			= 1e6/N;
	if (basisMax>M_full)	basisMax = M_full;

	double vk				= 1e-30;
	double vk0				= 1e-30;
	double temp				= 0;
	int i,j,l;
	double *Scales			= (double * ) R_alloc(M_full, sizeof(double));

	for (i					=0;i<K;i++)
	{
		Beta[i]				= i + 1;
		Beta[M_full + i]	= i + 1;
		Beta[M_full*2 + i]	= 0;
		Beta[M_full*3 + i]	= 0;
		temp				= 0;
		for(l=0;l<N;l++)	temp		= temp + BASIS[i*N + l]*BASIS[i*N + l];
		if(temp ==0) temp	= 1;
		Scales[i]			=sqrt(temp);
	}
	//PartII kk

	//
	int iter				= 0;
	double err				= 1000;
	double *Mu2, *SIGMA2, *H2, *Alpha, *PHI2;
	int * Used,*iteration, *m;
	
	Used					= (int* ) R_alloc(basisMax, sizeof(int));
	Mu2						= (double * ) R_alloc(basisMax, sizeof(double));
	SIGMA2					= (double * ) R_alloc(basisMax*basisMax, sizeof(double));
	H2						= (double * ) R_alloc(basisMax*basisMax, sizeof(double));
	Alpha					= (double * ) R_alloc(basisMax, sizeof(double));
	PHI2					= (double * ) R_alloc(N*basisMax, sizeof(double));
	iteration				= (int* ) R_alloc(1, sizeof(int));
	m						= (int* ) R_alloc(1, sizeof(int));

	m[0]			= 2;
	while (iter<iter_max && err>err_max)
	{
		iter				= iter + 1;
		
		vk0					= vk;
		iteration[0]		= iter;
		fEBBinary_EN(Used, Mu2, SIGMA2, H2, Alpha,PHI2,	BASIS, Targets,Scales, a_Lambda,b_Alpha,
						iteration, n, kdim, m,logLIKELIHOOD,basisMax);

		vk					= 0;
		for(i=0;i<m[0]-1;i++)	vk = vk + Alpha[i];
		err					= fabs(vk - vk0)/m[0];
	}

	// wald score
	int M					= m[0];	
	double *tempW			= (double * ) R_alloc(M,sizeof(double));

	wald[0]					= 0;
	int index = 0;
	for(i=0;i<M;i++)
    {

        tempW[i]      		= 0;
        for(j=0;j<M;j++)    tempW[i]     = tempW[i] + Mu2[j]*H2[i*M+j];       
        wald[0]				= wald[0]	 +tempW[i]*Mu2[i];
	}
	for(i=1;i<M;i++)
	{
		index				= Used[i-1] - 1;
		Beta[M_full*2 + index]	= Mu2[i]/Scales[index];
		Beta[M_full*3 + index]  = SIGMA2[i*M + i]/(Scales[index]*Scales[index]);
	}
	//
	intercept[0]	= Mu2[0];
	intercept[1]	= SIGMA2[0];
}




/************** outputs are passed by COPY in R, cann't dynamic realloc memory **************************/
/************** Not a problem in C */
// function [Used,Mu2,SIGMA2,H2,Alpha,PHI2]=fEBBinaryMexBmNe(BASIS,Targets,PHI2,Used,Alpha,Scales,a,b,Mu2,iter)
void fEBBinary_EN(int *Used, double *Mu2, double *SIGMA2, double *H2, double *Alpha, double *PHI2,
				double *BASIS, double * Targets, double *Scales, double *a_Lambda,double *b_Alpha,
				int *iteration, 
				int *n, int *kdim, int *m,double * LOGlikelihood,int basisMax)
{
    //basis dimension
   int N,K,M_full,N_used,N_unused,M,i,j,L,iter;
   	N					= *n;			// row number
    K					= *kdim;		// column number
	M_full				= K;

    double *beta	= (double *)R_alloc(N,sizeof(double));
	int *Unused = (int *) R_alloc(M_full,sizeof(int));
    iter				= *iteration;
    const int	ACTION_REESTIMATE       = 0;			
	const int	ACTION_ADD          	= 1;
	const int 	ACTION_DELETE        	= -1;
    const int   ACTION_TERMINATE        = 10;    

    int *IniLogic;
	IniLogic			= (int*) R_alloc(1,sizeof(int));
    if (iter<=1)    
    {
        IniLogic[0]     = 0;
        m[0]            = 2;
		M				= m[0];
		N_used			= 1;
		N_unused		= M_full -1;

    }else
    {
		IniLogic[0]    = 1;
        M				= *m;          //Used + 1
		N_used			= M -1;
		N_unused		= M_full - N_used;
    }

	fEBInitialization(Alpha, PHI2, Used, Unused, Mu2, BASIS, Targets, Scales, IniLogic, N, m, K);
    double *basisCache;
	basisCache         = (double *) R_alloc(N*K,sizeof(double));
    for(i=0;i<K;i++)
    {
        for(j=0;j<N;j++)            basisCache[i*N+j] = BASIS[i*N+j]*BASIS[i*N+j];
    }
	
	double *S_in, *Q_in, *S_out, *Q_out;
	S_in				= (double *) R_alloc(M_full,sizeof(double));
	Q_in				= (double *) R_alloc(M_full,sizeof(double));
	S_out				= (double *) R_alloc(M_full,sizeof(double));
	Q_out				= (double *) R_alloc(M_full,sizeof(double));

	fEBCatFullStat(beta, SIGMA2, H2, S_in, Q_in, S_out, Q_out,  BASIS,Scales, PHI2, 
				Targets, Used, Alpha, Mu2, basisCache,n, m, kdim);

    double *DeltaML, *AlphaRoot,deltaLogMarginal,*phi,newAlpha,oldAlpha;
    double deltaInv,kappa,Mujj,s_ii,*tmp,*tmpp,mu_i;
    //
	int *Action, *anyToDelete,selectedAction;
	anyToDelete			= (int*) R_alloc(1,sizeof(int));
	DeltaML				=	(double *) R_alloc(M_full,sizeof(double));
	AlphaRoot			=	(double *) R_alloc(M_full,sizeof(double));
	Action				= (int *) R_alloc(M_full,sizeof(int));
  	phi					= (double *) R_alloc(N,sizeof(double));
    
  	tmp					= (double *) R_alloc(basisMax,sizeof(double));
  	tmpp				= (double *) R_alloc(basisMax,sizeof(double));

    int nu,jj,index;
    jj					= -1;
    int anyWorthwhileAction,UPDATE_REQUIRED;
  	//
    int i_iter;
    i_iter              = 0;
    int LAST_ITERATION  = 0;
	double logLikelihood,dL,logL0;
	logLikelihood		= 1e-30;
	dL					= 1e-30;
	double *PHI_Mu;
	PHI_Mu				= (double*) R_alloc(N,sizeof(double));
	
    while(LAST_ITERATION!=1)
    {
        i_iter						= i_iter + 1;
		logL0						= logLikelihood;
		fEBDeltaML_EN(DeltaML, Action, AlphaRoot,anyToDelete,Used, Unused, S_out, Q_out, Alpha,
				a_Lambda,b_Alpha, N_used, N_unused);
		//
        deltaLogMarginal			= 0.001;
        nu							= -1;
        for(i=0;i<M_full;i++)
        {
            if(DeltaML[i]>deltaLogMarginal)
            {
                deltaLogMarginal    = DeltaML[i];
                nu                  = i;
            }
        }
        //
        if(nu==-1)
		{
			anyWorthwhileAction     = 0;
			selectedAction          = -10;
		}else
		{
			anyWorthwhileAction	= 1;
		    selectedAction              = Action[nu];
			newAlpha                    = AlphaRoot[nu];
		}
        if(selectedAction==ACTION_REESTIMATE || selectedAction==ACTION_DELETE)
        {
            index                   = nu + 1; 
            for(i=0;i<N_used;i++)
            {
                if (Used[i]==index)	jj  = i;
            }
        }
        for(i=0;i<K;i++)
        {
            if (i==nu)
            {
                for(L=0;L<N;L++)    phi[L]  = BASIS[i*N+L]/Scales[i];
            }

        }
        //newAlpha                    = AlphaRoot[nu];
        if(anyWorthwhileAction==0)  selectedAction = ACTION_TERMINATE;
        if(selectedAction==ACTION_REESTIMATE)
        {
            if (fabs(log(newAlpha)-log(Alpha[jj]))<=1e-3 && anyToDelete[0] ==0)
            {	

                selectedAction		= ACTION_TERMINATE;
            }
        }

        UPDATE_REQUIRED				= 0;
        if(selectedAction==ACTION_REESTIMATE)
        {
            oldAlpha				= Alpha[jj];
            Alpha[jj]				= newAlpha;
            index					= jj + 1;

            deltaInv				= 1.0/(newAlpha-oldAlpha);
            kappa					= 1.0/(SIGMA2[index*M+index] + deltaInv);
            Mujj					= Mu2[jj+1];
            for(i=0;i<M;i++)		Mu2[i]    = Mu2[i] - Mujj *kappa * SIGMA2[index*M+i];
            UPDATE_REQUIRED			= 1;
        }
        /////////////////////////////////////////////////////////////////////////////////
        else if(selectedAction==ACTION_ADD)
        {
            index					= N_used + 1;
			if(index > (basisMax -10) && iter>1 && (N*K) > 1e7) {
				Rprintf("bases: %d, warning: out of Memory!\n",index);
			}//return;
			if(index > (basisMax -1) && iter>1 && (N*K) > 1e7) {
				Rprintf("bases: %d, out of Memory,exiting program!\n",index);
			}
            for(i=0;i<index;i++)
            {
                tmp[i]				= 0;
                for(j=0;j<N;j++) tmp[i] = tmp[i] + beta[j]*phi[j]*PHI2[i*N+j]; //M+1   x 1
            }
            for(i=0;i<index;i++)
            {
                tmpp[i]				= 0;
                for(j=0;j<index;j++) tmpp[i]     = tmpp[i] + tmp[j]*SIGMA2[i*index+j];
            }//tmpp is tmp in matlab.

            //Alpha: M -> M + 1
			Alpha[N_used]			= newAlpha;                     //new element

            //
            for(i=0;i<N;i++)		PHI2[index*N + i] =phi[i];		//new column
            //

            s_ii					= 1.0/(newAlpha + S_in[nu]);
            mu_i					= s_ii*Q_in[nu];
            for(i=0;i<index;i++) Mu2[i] = Mu2[i] - mu_i*tmpp[i];
            Mu2[index]				= mu_i;							//new element
            //
			
            Used[N_used]			= nu + 1;						//new element
			N_used					= N_used + 1;

            //
			N_unused				= N_unused - 1;
            for(i=0;i<N_unused;i++)
            {                
                if(Unused[i]== (nu + 1))		Unused[i] =Unused[N_unused];
            }
			m[0]					= N_used + 1;
			M						= m[0];
            UPDATE_REQUIRED			= 1;

        }
        //////////////////////////////////////////////////////////////////////////////////
        else if(selectedAction==ACTION_DELETE)
        {
            index					= N_used - 1;
            //
			Alpha[jj]				= Alpha[index];           //Alpha: M -> M - 1
            
            //
			for(i=0;i<N;i++)		PHI2[(jj+1)*N + i] =PHI2[N_used*N+i];
            
            Mujj					= Mu2[jj+1];
            for(i=0;i<M;i++)  Mu2[i] = Mu2[i] - Mujj*SIGMA2[(jj+1)*M +i]/SIGMA2[(jj+1)*M + jj+1];
            
            //
			Mu2[jj+1]				= Mu2[N_used];

            //Used; Unused;
            Used[jj]				= Used[index];
			N_used					= index;

            //
			N_unused				= N_unused + 1;
			Unused[N_unused -1]		= nu + 1;

			m[0]					= N_used + 1;
			M						= m[0];
            UPDATE_REQUIRED			= 1;
		}

        //
        if(UPDATE_REQUIRED==1)
        {
			fEBCatFullStat(beta, SIGMA2, H2, S_in, Q_in, S_out, Q_out,  BASIS,Scales, PHI2, 
			Targets, Used, Alpha, Mu2, basisCache,n, m, kdim);

        }

        if(selectedAction==ACTION_TERMINATE) LAST_ITERATION =1;
        if(i_iter==1000)   LAST_ITERATION = 1;
		logLikelihood = 0;
		for(i = 0;i<N;i++)
		{
			PHI_Mu[i]				= 0;
			for(j = 0;j<M;j++)		PHI_Mu[i]	= PHI_Mu[i] + PHI2[j*N+i]*Mu2[j];
			logLikelihood			= logLikelihood + Targets[i]*log(exp(PHI_Mu[i])/(1+exp(PHI_Mu[i]))) + 
										(1-Targets[i])*log(1/(1+exp(PHI_Mu[i])));
		}
		dL							= fabs((logLikelihood - logL0)/logL0);
		if(dL <1e-4) LAST_ITERATION = 1; 

    }

	LOGlikelihood[0] = logLikelihood;
    
}

/****************************************************************************/


