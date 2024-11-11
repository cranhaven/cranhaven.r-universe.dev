
/***************************************************************************************
*                                                                                      *
*       Class definition for Multi Group Dependent Mixture Models 					   *
*                                                                                      *
*       Author: Ingmar Visser                                                          *
*                                                                                      *
*       Date: april 14, 2004   	
 		
 		Changed: july 19, 2004
*                                                                                      *
***************************************************************************************/
	

#ifndef MGDMM
#define MGDMM 1

#include <stdio.h>
#include <stdlib.h>

/* extern "C" { */
#include <R.h>
#include <Rmath.h>
/* } */

#include "matrix.h"
#include "mdmm.h"

class mgdmm {
	
	//data members
	private:
		int ngroups;
		int nrcomp;
		int *nstates;
		int nitems;
		int *itemtypes;
		
	public:
	   int *ncpars;
		mdmm *mods;
		matrix mixprop;
		matrix compsizes;
	
	// loglikelihood, gradients and hessian workspace
	public:
		matrix *fwd1;
		matrix *fwdt;
		
		matrix *psi1; 
		matrix *psit; 
		matrix *psitfinal; 
		
		matrix **omega1; 
		matrix **omegat; 
		matrix *omegafinal; 
		
	//member methods
	public:
		mgdmm();
		mgdmm(const int ng, const int nrc, int *nst, const int nit, int *itt, int xm);
		~mgdmm();		
		
	private:
		void initialize(const int ng, const int nrc, int *nst, const int nit, int *itt, int xm);
		
    public:
		void initWorkspace(void);
		
	public:
		void reset(const int ng, const int nrc, int *nst, const int nit, int *itt, int xm);
		void setPars(double *pars);
		void rescale();
		inline int getNrComp(void) {return(nrcomp);};
		inline int getNrGroups(void) {return(ngroups);};
		inline int getNPars(void) {return(ngroups*((int) compsizes.msum() + nrcomp));};
		
	public:
		void print(void);
		
};

#endif
