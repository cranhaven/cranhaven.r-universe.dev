
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
#include "NEG.h"

//B: binomial; m: main; Neg: normal + exp. + gamma
void fEBBinary_NEG(int *Used, double *Mu2, double *SIGMA2, double *H2, double *Alpha, double *PHI2,
                       double *BASIS, double * Targets, double *Scales, double *a_gamma, double *b_gamma,
                       int *iteration, int *n, int *kdim, int *m,double * LOGlikelihood,int basisMax,int verbose);


void fEBBinaryMainEff(double *BASIS, double * Targets, double *a_gamma, double * b_gamma,
				double * logLIKELIHOOD, double * Beta, double *wald,double *intercept, int *n, int *kdim,int *VB)
{
	int N					= *n;
	int K					= *kdim;
	int M_full				= K;
	int verbose = VB[0];
	const int iter_max		= 50;
	const double err_max	= 1e-8;
	// set a limit for number of basis
	if(verbose >1) Rprintf("start EBLasso-NEG with a: %f, \tb: %f\n",a_gamma[0], b_gamma[0]);
	int basisMax			= 1e6/N;
	if (basisMax>M_full)	basisMax = M_full;
	if(verbose >2) Rprintf("M_full: %d; basisMax: %d\n",M_full,basisMax);
	double vk				= 1e-30;
	double vk0				= 1e-30;
	double temp				= 0;
	int i,j,l;
	double *Scales			= (double * ) Calloc(M_full, double);
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
	int iter				= 0;
	double err				= 1000;
	double *Mu2, *SIGMA2, *H2, *Alpha, *PHI2;
	int * Used,*iteration, *m;
	
	Used					= (int* ) Calloc(basisMax, int);
	Mu2						= (double * ) Calloc(basisMax, double);
	SIGMA2					= (double * ) Calloc(basisMax*basisMax, double);
	H2						= (double * ) Calloc(basisMax*basisMax, double);
	Alpha					= (double * ) Calloc(basisMax, double);
	PHI2					= (double * ) Calloc(N*basisMax, double);
	iteration				= (int* ) Calloc(1, int);
	m						= (int* ) Calloc(1, int);
	if(verbose >1) Rprintf("outer loop starts\n");
	m[0]			= 2;
	while (iter<iter_max && err>err_max)
	{
		iter				= iter + 1;
		
		vk0					= vk;
		iteration[0]		= iter;
		fEBBinary_NEG(Used, Mu2, SIGMA2, H2, Alpha,PHI2,	BASIS, Targets,Scales, a_gamma, b_gamma,
						iteration, n, kdim, m,logLIKELIHOOD,basisMax,verbose);

		vk					= 0;
		for(i=0;i<m[0]-1;i++)	vk = vk + Alpha[i];
		err					= fabs(vk - vk0)/m[0];
		if(verbose >2) Rprintf("Iteration number: %d, err: %f\n",iter,err);
	}

	// wald score
	int M					= m[0];	
	double *tempW			= (double * ) Calloc(M,double);
	
	wald[0]					= 0;
	int index = 0;
	if(verbose >1) Rprintf("EBLASSO-NEG Finished, number of basis: %d\n",M);
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
	
	
	Free(Scales);
	Free(Used);
	Free(Mu2);
	Free(SIGMA2);
	Free(H2);
	Free(Alpha);
	Free(PHI2);
	Free(iteration);	
	Free(m);
	Free(tempW);	
}




/************** outputs are passed by COPY in R, cann't dynamic realloc memory **************************/
/************** Not a problem in C */
// function [Used,Mu2,SIGMA2,H2,Alpha,PHI2]=fEBBinaryMexBmNeg(BASIS,Targets,PHI2,Used,Alpha,Scales,a,b,Mu2,iter)
void fEBBinary_NEG(int *Used, double *Mu2, double *SIGMA2, double *H2, double *Alpha, double *PHI2,
                   double *BASIS, double * Targets, double *Scales, double *a_gamma, double *b_gamma,
                   int *iteration, int *n, int *kdim, int *m,double * LOGlikelihood,int basisMax, int verbose)
{
  //basis dimension
  int N,K,M_full,N_used,N_unused,M,i,j,L,iter;
  N					= *n;			// row number
  K					= *kdim;		// column number
  M_full				= K;
  
  double *beta	= (double *)Calloc(N,double);
  int *Unused = (int *) Calloc(M_full,int);
  iter				= *iteration;
  const int	ACTION_REESTIMATE       = 0;			
  const int	ACTION_ADD          	= 1;
  const int 	ACTION_DELETE        	= -1;
  const int   ACTION_TERMINATE        = 10;    
  
  int *IniLogic;
  IniLogic			= (int*) Calloc(1,int);
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
  if(verbose >3) Rprintf("\t Initialized basis %d, Alpha: %f, \n", Used[0],Alpha[0]);	
  double *basisCache;
  basisCache         = (double *) Calloc(N*K,double);
  for(i=0;i<K;i++)
  {
    for(j=0;j<N;j++)            basisCache[i*N+j] = BASIS[i*N+j]*BASIS[i*N+j];
  }
  
  double *S_in, *Q_in, *S_out, *Q_out;
  S_in				= (double *) Calloc(M_full,double);
  Q_in				= (double *) Calloc(M_full,double);
  S_out				= (double *) Calloc(M_full,double);
  Q_out				= (double *) Calloc(M_full,double);

  fEBCatFullStat(beta, SIGMA2, H2, S_in, Q_in, S_out, Q_out,  BASIS,Scales, PHI2, 
                 Targets, Used, Alpha, Mu2, basisCache,n, m, kdim);

  double *DeltaML, *AlphaRoot,deltaLogMarginal,*phi,newAlpha,oldAlpha;
  double deltaInv,kappa,Mujj,s_ii,*tmp,*tmpp,mu_i;
  //
  int *Action, *anyToDelete,selectedAction;
  anyToDelete			= (int*) Calloc(1,int);
  DeltaML				=	(double *) Calloc(M_full,double);
  AlphaRoot			=	(double *) Calloc(M_full,double);
  Action				= (int *) Calloc(M_full,int);
  phi					= (double *) Calloc(N,double);
  
  tmp					= (double *) Calloc(basisMax,double);
  tmpp				= (double *) Calloc(basisMax,double);
  
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
  PHI_Mu				= (double*) Calloc(N,double);
  if(verbose >3) Rprintf("check point 3: before loop \n");	
  while(LAST_ITERATION!=1)
  {
    i_iter						= i_iter + 1;
    if(verbose >4) Rprintf("\t inner loop %d \n",i_iter);
    logL0						= logLikelihood;
    fEBDeltaML_NEG(DeltaML, Action, AlphaRoot,anyToDelete,Used, Unused, S_out, Q_out, Alpha,
               a_gamma,b_gamma, N_used, N_unused);
    //
    deltaLogMarginal			= 0;
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
    //kk                          = K;                          
    for(i=0;i<K;i++)
    {
      if (i==nu)
      {
        for(L=0;L<N;L++)    phi[L]  = BASIS[i*N+L]/Scales[i];
      }

    }
    
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
      if(verbose >4) Rprintf("\t\t Action: Reestimate : %d \t deltaML: %f\n",nu + 1, deltaLogMarginal);		
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
      if(verbose >4) Rprintf("\t\t Action:add : %d \t deltaML: %f\n",nu + 1,deltaLogMarginal);		
      index					= N_used + 1;
      if(index > (basisMax -10) && (N*K) > 1e7) {
        Rprintf("bases: %d, warning: out of Memory, alloc more to Neffect!\n",index);
      }//return;
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
      if(verbose >4) Rprintf("\t\t Action: delete : %d deltaML: %f \n",nu + 1,deltaLogMarginal);		
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

    if(dL <1e-3) LAST_ITERATION = 1; 
    
  }
  
  LOGlikelihood[0] = logLikelihood;
  
  Free(beta);
  Free(Unused);
  Free(IniLogic);
  Free(basisCache);
  Free(S_in);
  Free(Q_in);
  Free(S_out);
  Free(Q_out);
  Free(anyToDelete);
  Free(DeltaML);	
  Free(AlphaRoot);	
  Free(Action);	
  Free(phi);	
  Free(tmp);	
  Free(tmpp);	
  Free(PHI_Mu);	
  
  
}

/****************************************************************************/


