
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

#include "NEG.h"


void fEBDeltaML_NEG(double *DeltaML, int *Action, double *AlphaRoot, int *anyToDelete,
				int *Used, int * Unused, double * S_out, double * Q_out, double *Alpha,
				double *a_gamma, double *b_gamma, int N_used, int N_unused)
{
    //basis dimension
    int M_full,i,index;
	anyToDelete[0] 						= 0;
    M_full								= N_used + N_unused;
    //
    //Parameters setup 
    const int	ACTION_REESTIMATE		= 0;			
	const int 	ACTION_ADD				= 1;
	const int	ACTION_DELETE			= -1;
    const double    logL_inf			= 0.0;
    double alpha, beta, gamma,delta,root1,root2,logL,oldAlpha,a,b;
	a									= *a_gamma;
	b									= *b_gamma;
    int   anyToAdd						= 0;
    const int CNPriorityAddition		= 0;
    const int CNPriorityDeletion		= 1;
    for(i=0;i<N_used;i++)
    {
        index						= Used[i] -1;
		DeltaML[index]				= 0;
        alpha						= 2*a +2 + b*S_out[index] - b*Q_out[index]*Q_out[index];
        beta						= (4*a + 5)*S_out[index] + b*S_out[index]*S_out[index] -Q_out[index]*Q_out[index];
        gamma						= (2*a + 3)*S_out[index]*S_out[index];
        delta						= beta*beta - 4*alpha*gamma;
        //case1
        if(alpha>0 && delta>0 && beta<0)
        {
            root1					= (- beta-sqrt(delta))/(2*alpha);
            logL					= (log(root1/(root1+S_out[index]))+pow(Q_out[index],2)/(root1+S_out[index]))*0.5 + (a+1)*log(b*root1/(1+b*root1));
            if (logL > logL_inf)
            {
                AlphaRoot[index]    = root1;
                Action[index]       = ACTION_REESTIMATE;
                //
                oldAlpha            = Alpha[i];
                DeltaML[index]  	= 0.5*(log(root1*(oldAlpha+S_out[index])/(oldAlpha*(root1 + S_out[index])))+
                                    Q_out[index]*Q_out[index]*(1/(root1+S_out[index])-1/(oldAlpha+S_out[index])))+
                                    (a+1)*log((root1*(1+b*oldAlpha))/(oldAlpha*(1+b*root1)));
            }
        }
        //case 2
        else if(alpha==0 && beta<0)
        {
            root1               = -gamma/beta;
            AlphaRoot[index]    = root1;
            Action[index]       = ACTION_REESTIMATE;
            //
            oldAlpha            = Alpha[i];
            DeltaML[index]  	= 0.5*(log(root1*(oldAlpha+S_out[index])/(oldAlpha*(root1 + S_out[index])))+
                                    Q_out[index]*Q_out[index]*(1/(root1+S_out[index])-1/(oldAlpha+S_out[index])))+
                                    (a+1)*log((root1*(1+b*oldAlpha))/(oldAlpha*(1+b*root1)));
        }
        //case 3
        else if(alpha <0)
        {
            root2               = (- beta-sqrt(delta))/(2*alpha);
            AlphaRoot[index]    = root2;
            Action[index]       = ACTION_REESTIMATE;
            //
            oldAlpha            = Alpha[i];
            DeltaML[index]  	= 0.5*(log(root2*(oldAlpha+S_out[index])/(oldAlpha*(root2 + S_out[index])))+
                                    Q_out[index]*Q_out[index]*(1/(root2 + S_out[index])-1/(oldAlpha + S_out[index])))+
                                    (a+1)*log((root2*(1 + b*oldAlpha))/(oldAlpha*(1+b*root2)));
        }
        //DELETE
        else if (N_used>1)
        {
            anyToDelete[0]      = 1;
            Action[index]       = ACTION_DELETE;
            oldAlpha            = Alpha[i];
            logL                = (log(oldAlpha/(oldAlpha+S_out[index]))+pow(Q_out[index],2)/(oldAlpha+S_out[index]))*0.5 + (a+1)*log(b*oldAlpha/(1+b*oldAlpha));
            DeltaML[index]      = - logL;
        }
    }
    //ADDITION
    for(i=0;i<N_unused;i++)
    {
        index					= Unused[i] -1;
		DeltaML[index]			= 0;
        alpha					= 2*a + 2 + b*S_out[index] - b*Q_out[index]*Q_out[index];
        beta					= (4*a + 5)*S_out[index] + b*S_out[index]*S_out[index] -Q_out[index]*Q_out[index];
        gamma					= (2*a + 3)*S_out[index]*S_out[index];
        delta					= beta*beta - 4*alpha*gamma;
        //case1
        if(alpha>0 && delta>0 && beta<0)
        {
            root1					= (- beta-sqrt(delta))/(2*alpha);
            logL					= (log(root1/(root1+S_out[index]))+pow(Q_out[index],2)/(root1+S_out[index]))*0.5 + (a+1)*log(b*root1/(1+b*root1));
            if (logL > logL_inf)
            {
                AlphaRoot[index]    = root1;
                Action[index]       = ACTION_ADD;
                anyToAdd            = 1;
                //
                DeltaML[index]  	= (log(root1/(root1+S_out[index]))+pow(Q_out[index],2)/(root1+S_out[index]))*0.5 + (a+1)*log(b*root1/(1+b*root1));
            }
        }
        //case 2
        else if(alpha==0 && beta<0)
        {
            root1               = -gamma/beta;
            AlphaRoot[index]    = root1;
            Action[index]       = ACTION_ADD;
            anyToAdd            = 1;
            //
            DeltaML[index]  		= (log(root1/(root1+S_out[index]))+pow(Q_out[index],2)/(root1+S_out[index]))*0.5 + (a+1)*log(b*root1/(1+b*root1));
        }
        //case 3
        else if(alpha <0)
        {
            root2               = (- beta-sqrt(delta))/(2*alpha);
            AlphaRoot[index]    = root2;
            Action[index]       = ACTION_ADD;
            anyToAdd            = 1;
            //
            DeltaML[index]  	= (log(root2/(root2+S_out[index]))+pow(Q_out[index],2)/(root2+S_out[index]))*0.5 + (a+1)*log(b*root2/(1+b*root2));
        }
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
