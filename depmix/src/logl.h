
#ifndef LOGL
#define LOGL 1

#include <stdio.h>
#include <stdlib.h>
/* #include <fstream.h> */

#include "globals.h"

#include "mgdmm.h"
#include "mmts.h"

#include "matrix.h"

#include <R.h>	
#include <Rmath.h>

extern "C" {

//these routines set up the model and set up the data in formats that are readable by loglikelihood
void mixModelSetUp(int *ngroups, int *nrcomp, int *nstates, int *nitems, int *itemtypes, double *pars, int *xm, int *print);
void covSetUp(int *ngroups, int *nrcomp, int *nstates, int *nitems, int *itemtypes, double *pars, int *xm, int *print);

void multiDataSetUp(int *ngroups, int *printlevel);
void ngDataSetUp(int *groupnr, double *data, int *vars, int *modes, int *indReal, int *lengths, double *weights, int *xm, int *printlevel);

void multiCovSetUp(int *ngroups, int *printlevel);
void ngCovSetUp(int *groupnr, double *data, int *vars, int *modes, int *indReal, int *lengths, double *weights, int *xm, int *printlevel);

// the loglikelihood function
void loglikelihood(double *pars, int *tdcov, double *objf, int *grad, int *hess, double *gr, double *hs, 
				   int *grset, int *hsset, int *grInd, double *gradIndividual, double *scale, int *info); 

// posteriors
void posteriors(double *states, double *postdelta, int *comp, double *pars, int *tdcov, int *groupnr, int *indnr);

// criterion for stopping optimization, used in bootstrapping
void setCrit(double *criter);

} //end extern "C"

#endif
