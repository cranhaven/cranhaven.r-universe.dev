/*
 *  Part of R package Genome
 *  Copyright (C) 2009-2010  B. Wang
 *
 *  Unlimited use and distribution (see LICENCE).
 */

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void RGldx(double *x0,double *x, int *n);
void RGldFx(double *x0,double *x,int *n);
void RGldfx(double *x0,double *x,int *n);
//void RIsGld(double *x,int *n);

void FitGBD(double *x0,int *n,int *m,double *l,double *x);
void FitGBDMom(double *x0,int *m,double *l);

void GLDMoM(double *xmts, double *chisq, int *m,double *os, double *xbin);
void GLDMoP(double *xmts, double *chisq, int *m,double *os, double *xbin);
void GLDLMoM(double *xmts, double *chisq, int *m,double *os, double *xbin);


void RKSPvalue(double *x0);
void RLMoM(double *x0, int *size, double *lmts);

// ARL.c
void arl0(double *lcl, double *x, int *nx,
	 double *y, int *ny, double *z, int *n,
	 double *pm0, double *pI0, double *lambda);
void arl1(double *lcl, double *x, int *nx,
	 double *y, int *ny, double *z, int *n,
	 double *pm0, double *pI0, double *lambda);
void simucc(int *B, int *T, int *n, double *pm0, double *pI0,
	    double *lambda, double *Md, double *D);


static const R_FortranMethodDef FortEntries[] = {
  //  {"RIsGld", (DL_FUNC) & RIsGld, 2},
  {"RGldx", (DL_FUNC) & RGldx, 3},
  {"RGldFx", (DL_FUNC) & RGldFx, 3},
  {"RGldfx", (DL_FUNC) & RGldfx, 3},
  {"FitGBD", (DL_FUNC) & FitGBD, 5},

  {"GLDMoM", (DL_FUNC) & GLDMoM, 5},
  {"GLDMoP", (DL_FUNC) & GLDMoP, 5},
  {"GLDLMoM", (DL_FUNC) & GLDLMoM, 5},
  {"FitGBDMom", (DL_FUNC) & FitGBDMom, 3},
  {"RKSPvalue", (DL_FUNC) & RKSPvalue, 1},
  {"RLMoM", (DL_FUNC) & RLMoM, 3},
  {"arl1", (DL_FUNC) & arl1, 10},
  {"arl0", (DL_FUNC) & arl0, 10},
  {"simucc", (DL_FUNC) & simucc, 8},
  {NULL, NULL, 0}
};


void R_init_gb(DllInfo *dll)
{
  //    R_registerRoutines(dll, NULL, NULL, callMethods, NULL);
  R_registerRoutines(dll, NULL, NULL, FortEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
