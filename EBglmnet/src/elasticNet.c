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
#include "elasticNet.h"


void fEBDeltaML_EN(double *DeltaML, int *Action, double *AlphaRoot, int *anyToDelete,
                   int *Used, int * Unused, double * S_out, double * Q_out, double *Alpha,
                   double *a_lambda, double *b_Alpha, int N_used, int N_unused)
{
  //basis dimension
  int M_full,i,index;
  anyToDelete[0] = 0;
  M_full								= N_used + N_unused;
  //
  //Parameters setup      //Action is return as int
  const int	ACTION_REESTIMATE		= 0;			
  const int 	ACTION_ADD				= 1;
  const int	ACTION_DELETE			= -1;
  const double    logL_inf			= 0.0;
  double alpha, beta, gamma,delta,root2,logL,oldAlpha,lambda1,lambda2; //lambda
  lambda1 							= *a_lambda * (b_Alpha[0]);
  lambda2 							= *a_lambda * (1 - b_Alpha[0]);

  int   anyToAdd						= 0;
  const int CNPriorityAddition		= 0;
  const int CNPriorityDeletion		= 1;

  for(i=0;i<N_used;i++)
  {
    index						= Used[i] -1;
    DeltaML[index]				= 0;
    alpha						= S_out[index] - Q_out[index]*Q_out[index] + 2*lambda1 + lambda2;
    beta						= (S_out[index]+lambda2)*(S_out[index] + 4 *lambda1 + lambda2);
    gamma						= 2*lambda1*(S_out[index] + lambda2)*(S_out[index]+lambda2);
    delta						= beta*beta - 4*alpha*gamma;
    //case1
    if(alpha<0 && delta>0)
    {
      root2					= (- beta-sqrt(delta))/(2*alpha);
      logL					= (log(root2/(root2+S_out[index] + lambda2))+pow(Q_out[index],2)/(root2+S_out[index] + lambda2))*0.5 -lambda1/root2;
      if (logL > logL_inf)
      {
        AlphaRoot[index]    = root2 + lambda2;
        Action[index]       = ACTION_REESTIMATE;
        //
        oldAlpha            = Alpha[i]-lambda2;				
        DeltaML[index]  	= 0.5*(log(root2*(oldAlpha + S_out[index]+lambda2)/(oldAlpha*(root2 + S_out[index] + lambda2))) +
          Q_out[index]*Q_out[index]*(1/(root2 + S_out[index] +lambda2) - 1/(oldAlpha+S_out[index]+ lambda2))) -
          lambda1*(1/root2 - 1/oldAlpha);
      }
    }
    //case 2
    //case 3
    
    //DELETE
    else if (N_used>1)
    {
      anyToDelete[0]      = 1;
      Action[index]       = ACTION_DELETE;
      oldAlpha            = Alpha[i] - lambda2;
      logL                = (log(oldAlpha/(oldAlpha+S_out[index] + lambda2)) + pow(Q_out[index],2)/(oldAlpha + S_out[index] + lambda2))*0.5 - lambda1/oldAlpha;
      DeltaML[index]      = - logL;
    }
  }
  //ADDITION
  for(i=0;i<N_unused;i++)
  {
    index					= Unused[i] -1;
    DeltaML[index]			= 0;
    alpha						= S_out[index] - Q_out[index]*Q_out[index] + 2*lambda1 + lambda2;
    beta						= (S_out[index]+ lambda2)*(S_out[index] +lambda2 + 4 *lambda1);
    gamma						= 2*lambda1*(S_out[index]+lambda2)*(S_out[index] + lambda2);
    delta						= beta*beta - 4*alpha*gamma;
    //case1
    if(alpha<0 && delta>0)
    {
      root2					= (- beta-sqrt(delta))/(2*alpha);
      logL					= (log(root2/(root2 + S_out[index] + lambda2)) + pow(Q_out[index],2)/(root2 + S_out[index] + lambda2))*0.5 - lambda1/root2;
      if (logL > logL_inf)
      {
        AlphaRoot[index]    = root2 + lambda2;
        Action[index]       = ACTION_ADD;
        //
        DeltaML[index]  	= (log(root2/(root2 + S_out[index] + lambda2)) + pow(Q_out[index],2)/(root2 + S_out[index] +lambda2))*0.5 - lambda1/root2;
      }
    }
    //case 2
    //case 3        
  }
  
  //
  if((anyToAdd==1 && CNPriorityAddition==1) || (anyToDelete[0]==1 && CNPriorityDeletion==1))
  {
    for(i=0;i<M_full;i++)
    {
      if (Action[i] == ACTION_REESTIMATE)											DeltaML[i]     = 0;
      else if (Action[i] == ACTION_DELETE)
      {
        if(anyToAdd==1 && CNPriorityAddition==1 && CNPriorityDeletion!=1)    DeltaML[i]     = 0;
      }else if (Action[i] == ACTION_ADD)
      {
        if(anyToDelete[0] ==1 && CNPriorityDeletion==1 && CNPriorityAddition!=1) DeltaML[i] = 0;
      }
    }
  }    
}
