
/***************************************************************************************
*                                                                                      *
*       Class definition for Dependent Mixture Models for multivariate mixed time series   *
*                                                                                      *
*       Author: Ingmar Visser                                                          *
*                                                                                      *
*       Date: april 14, 2004   
		
		Changes: may 26, 2004, changed some implementational stuff in the 
		representation of the observation parameters. 
*                                                                                      *
***************************************************************************************/
	

#ifndef MDMM
#define MDMM 1

#include <stdio.h>
#include <stdlib.h>

#include <R.h>
#include <Rmath.h>

#include "matrix.h"

extern "C" { 
#include "derdist.h"	
}

#define MDMMBOUNDS 1

class mdmm {
	
	//data members
	private:
		int nstates;
		int nitems;
		int *modes;
		int npars;
		int obsMatCols;
		int xm;
		
		int trans;
		int obser;
		int inits;
	
	private:		
		matrix transPars;
		matrix initPars;
		matrix *respPars;
		
		matrix datxm;
		matrix resultStates;
	
	//member methods
	public:
		mdmm();
		mdmm(const int nst, const int nit, int *mod, int xm);
	//used by the constructors
	private:
		void initialize(const int nst, const int nit, int *mod, int xm);
	public:
		~mdmm();
		
	public:
		void reset(const int nst, const int nit, int *mod, int xm);
		void setPars(double *pars);
		void rescale(); 
		
	//access
		inline int getStates() {return(nstates);}
		inline int getNItems() {return(nitems);}
		inline int getNPars() {return(npars);}
		
		int getMode(const int it);
		matrix getDataProb(matrix dat); // should this matrix be const or not???
		double getDataProb(const int st, matrix dat); // should this matrix be const or not???
		double getTrPar(const int st1, const int st2);
		double getInProb(const int st);
		inline matrix getInProb(void) {return(initPars);}
		inline matrix getTrProb(void) {return(transPars);}
		
	//derivatives
		double derin(const int np, const int st);
		double derobs(const int np, const int st, matrix dat);
		double dertrans(const int np, const int st1, const int st2);
		double hesobs(const int np1, const int np2, const int st, matrix dat);
		
	private: 
	//auxiliary for getDataProb
		double getProb(const double dat, const int st, const int it); //no bounds checks, this is assumed to be done in the calling routine
	
	// auxiliaries for derivative functions	
		double derobs(const int np, const int st, const int it, double datit);
		double hesobs(const int np1, const int np2, const int st, const int it, double datit);
		int partype(const int np);
		int trstate1(const int np);
		int trstate2(const int np);
		int obsstate(const int np);
		int itemnr(const int np);
		int itempar(const int np);
		
	//auxiliary function returning the number of parameters of a particular itemtype=mode
		int nppDensity(const int mode);
		
	public:
		void print(void);
		
};

#endif
