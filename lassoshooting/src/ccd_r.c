#define HAVE_INLINE
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <math.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/RS.h>
#ifndef USE_FC_LEN_T
# define USE_FC_LEN_T
#endif
#include <R_ext/BLAS.h>

#include "ccd.h"

SEXP ccd(SEXP args) {
  param_t params;
  double *X = NULL,*y = NULL;
  double *givenXtX=NULL,*givenXty=NULL;
  double *givenbeta=NULL;
  double *givens=NULL;
  int Xm, Xn;
  int Ym, Yn;
  int bm, bn;
  int sm, sn;
  int XtXp,Xtym;
  Xtym = XtXp = Xm = Xn = Ym = Yn = bm = bn = sn = sm = -1;
  int wm = -1,wn = -1;

  double factor2=1.; // factor2=2 emulates LassoShooting.m

  //default parameters
  params.m = params.p = -1;
  params.tol = 1e-6;
  params.forcezero = -1;
  params.maxits = 10000;
  params.trace = 0;
  int *nopenalize = NULL;
  params.w = NULL;

  // argument handling
  args = CDR(args); 
  for(int i = 0; args != R_NilValue; i++, args = CDR(args)) {
    const char *name = CHAR(PRINTNAME(TAG(args)));
    if (params.trace > 1) {
      REprintf(__FILE__ ": parsing parameter %s\n",name);
    }
    if (CAR(args) == R_NilValue) {
      //if (params.trace) REprintf(__FILE__ ": parameter '%s' was null\n",name);
      continue;
    }
    if (strcasecmp(name, "x")==0) { 
			X = REAL(CAR(args)); 
			SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
			if (isNull(d)) {
				Xm = length(CAR(args));
				Xn = 1;
			} else {
				Xm = INTEGER(d)[0];
				Xn = INTEGER(d)[1];
			}
		}
    else if (strcasecmp(name, "y")==0) { 
      y = REAL(CAR(args)); 
      SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
      if(!isNull(d)) {
        Ym = INTEGER(d)[0];
        Yn = INTEGER(d)[1];
      } else {
        Ym = length(CAR(args));
        Yn = 1;
      }
    }
    else if (strcasecmp(name, "lambda")==0)  { 
      if (length(CAR(args)) != 1) { error(_("length of 'lambda' should be 1!\n")); }
      params.lambda = REAL(CAR(args))[0]; 
    }
    else if (strcasecmp(name, "forcezero")==0) { 
      if (length(CAR(args)) != 1) { error(_("length of 'forcezero' should be 1!\n")); }
      SEXP newforcezero;
      //double* nopen = REAL(CAR(args)); 
      PROTECT(newforcezero = AS_INTEGER(CAR(args)));
      params.forcezero = INTEGER(newforcezero)[0]; 
      UNPROTECT(1);
    } 
    else if (strcasecmp(name, "thr")==0) { 
      if (length(CAR(args)) != 1) { error(_("length of 'thr' should be 1!\n")); }
      params.tol = REAL(CAR(args))[0]; 
    } else if(strcasecmp(name,"factor")==0) {
      if (length(CAR(args)) != 1) { error(_("length of 'factor' should be 1!\n")); }
      factor2 = REAL(CAR(args))[0]; 
    }
    else if (strcasecmp(name, "maxit")==0) { 
      if (length(CAR(args)) != 1) { error(_("length of 'maxit' should be 1!\n")); }
      params.maxits = REAL(CAR(args))[0]; 
    } 
    else if (strcasecmp(name, "penaltyweight")==0) { 
      params.w = REAL(CAR(args)); 
      SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
      if(!isNull(d)) {
        wm = INTEGER(d)[0];
        wn = INTEGER(d)[1];
      } else {
        wm = length(CAR(args));
        wn = 1;
      }
      if (wn != 1) error(_("penaltyweight should be p x 1"));
    }
    else if (strcasecmp(name, "nopenalize")==0) { 
      SEXP newnopen;
      //double* nopen = REAL(CAR(args)); 
      PROTECT(newnopen = AS_NUMERIC(CAR(args)));
      double *nopen = REAL(newnopen);
      UNPROTECT(1);
      int N = length(CAR(args));
      nopenalize = (int*)R_alloc(N+1,sizeof(int));
      for(int i=0; i < N; ++i) {
        nopenalize[i] = nopen[i];
        if (nopenalize[i] < 0) 
          error(_("Can not penalize variables with negative index! The range is 0 to p-1.\n")); 
      }
      nopenalize[N] = -1;
    } 
    else if (strcasecmp(name, "trace")==0) { 
      if (length(CAR(args)) != 1) { error(_("length of 'trace' should be 1!\n")); }
      params.trace = REAL(CAR(args))[0]; 
      if (params.trace > 0) REprintf("Tracing on!\n");
    } 
		else if (strcasecmp(name, "beta")==0) {
			givenbeta = REAL(CAR(args));
      SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
      if(!isNull(d)) {
        bm = INTEGER(d)[0];
        bn = INTEGER(d)[1];
      } else {
        bm = length(CAR(args));
        bn = 1;
      }
		}
    else if (strcasecmp(name, "s") == 0) {
      givens = REAL(CAR(args));
      SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
      if(!isNull(d)) {
        sm = INTEGER(d)[0];
        sn = INTEGER(d)[1];
      } else {
        sm = length(CAR(args));
        sn = 1;
      }
		}
    else if (strcasecmp(name,"XtX")==0) {
      givenXtX = REAL(CAR(args)); 
      SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
      if (isNull(d)) error(_("X'X should be a square matrix"));
      int m = INTEGER(d)[0];
      int n = INTEGER(d)[1];
      if (m!=n) error(_("X'X should be a square matrix, it is %dx%d!"),m,n);
      XtXp = m;
    }
    else if (strcasecmp(name,"Xty")==0) {
      givenXty = REAL(CAR(args)); 
      SEXP d = getAttrib(CAR(args), R_DimSymbol); // dimensions
      int m,n;
      if (isNull(d)) {
	m = length(CAR(args));
	n = 1;
      } else {
	m = INTEGER(d)[0];
	n = INTEGER(d)[1];
      }
      if (n!=1) error(_("X'y should be Mx1, it is %dx%d!"),m,n);
      Xtym = m;
    }
    else {
      error(_("Unknown parameter '%s'!\n"),name); 
      return(R_NilValue);
    }
  }
  if (params.trace >=2) {
    REprintf("X m: %d n: %d\n",Xm,Xn); 
    REprintf("Y m: %d n: %d\n",Ym,Yn); 
  }
  if (Ym != -1 && Xm != -1 && Xm != Ym) error(_("X and Y has different number of samples"));
  int p = (Xn == -1)? XtXp : Xn; // no variables
  int m = (Xm == -1)? Xtym : Xm; // no equations
  if (wm != -1 && wm != p) error(_("penaltyweight should be p x 1\n"));
  params.p = p;
  params.m = m;

  params.XtX = (double*)R_alloc(p*p,sizeof(double));
	if (params.trace > 0) REprintf("using factor %f\n",factor2); 

  double zero=0.;
  double one=1;
  if (params.trace>=2)  REprintf("havextx: %d\n",givenXtX!=NULL);
  if (!givenXtX && Xm != -1 && Xn != -1) { 
    if (params.trace>=2) REprintf("calculating X'X with factor %f\n",factor2);
    // X is MxN   X'X  NxM*MxN -> NxN
    F77_CALL(dgemm)("T","N",&Xn,&Xn,&Xm, &one,X,&Xm, X,&Xm, &zero,params.XtX,&Xn FCONE FCONE);  // 2X'X -> XtX
  } 
  else if (givenXtX) {
    if (params.trace>=2) REprintf("givenXtX\n");
    for(int i=0; i < p*p; ++i)
      params.XtX[i] = givenXtX[i];
  }
  else {
    error(_("Need either X and y or XtX and Xty"));
  }
  if (params.trace>=2) REprintf("scaling X'X with %f, p: %d\n",factor2,p);
  for(int i=0; i < p*p; ++i)
    params.XtX[i] = factor2*params.XtX[i];
#ifdef DEBUG
  if (params.trace >= 2) {
    FILE*D = fopen("ccd.debug","a");
    for (int i=0; i < p; ++i)
      for (int j=0; j < p; ++j)
	fprintf(D,"X'X[%d,%d]: %f\n", i,j,params.XtX[i*p+j]);
    fclose(D);
  }
#endif


  params.Xty = (double*)R_alloc(p,sizeof(double)); // NxM*Mx1 -> Nx1
  if (!givenXty && Xm != -1 && Ym != -1) {
    F77_CALL(dgemv)("T", &Xm,&Xn, &factor2,X,&Xm, y,&Yn, &zero,params.Xty,&Yn FCONE); // Xty <- 2X'y
  } else if (givenXty) {
    for(int i=0; i < Xtym; ++i)
      params.Xty[i] = factor2*givenXty[i];
  } else {
    error(_("Need either X and y or XtX and Xty"));
  }
  if (params.trace > 2) REprintf("X'y_(1,1)=%.4f\n",params.Xty[0]/2.);
  if (params.trace > 2) REprintf("X'y_(2,1)=%.4f\n",params.Xty[1]/2.);
  if (params.trace > 2) REprintf("X'y_(3,1)=%.4f\n",params.Xty[2]/2.);

  params.nopenalize = nopenalize;

  if (givenbeta && (bn != 1 || bm != p)) {
    error(_("Given beta length must equal the number of predictors"));
  }
  if (givens && (sn != 1 || sm != p)) {
    error(_("Given s length must equal the number of predictors"));
  }

  SEXP beta_;
  PROTECT(beta_ = allocVector(REALSXP,p));
  params.beta = REAL(beta_);
  for(int i=0; i < params.p; ++i) {
    params.beta[i] = givenbeta ? givenbeta[i] : 0.;
  }
  params.factor2 = factor2;
  params.s = givens;
  ccd_common(&params);

#define NC 5
  char* names[NC] = {"coefficients","iterations","delta", "infnorm","s"};
  SEXP list_names, list, itsR, deltaR, infnormR, sR;
  PROTECT(itsR = NEW_INTEGER(1));
  int* itsRp = INTEGER_POINTER(itsR);
  *itsRp = params.its;
  PROTECT(deltaR = NEW_NUMERIC(1));
  double* deltaRp = NUMERIC_POINTER(deltaR);
  *deltaRp = params.delta;
  PROTECT(infnormR = NEW_NUMERIC(1));
  double* infnormRp = NUMERIC_POINTER(infnormR);
  *infnormRp = params.infnorm;

  int numelt = NC;
  if (params.s) {
    PROTECT(sR = allocVector(REALSXP,p));
    {
      double *s_ = REAL(sR);
      for(int i=0; i < p; ++i) s_[i] = params.s[i];
    }
  } else {
    -- numelt;
  }
  PROTECT(list_names = allocVector(STRSXP, numelt));
  for(int i = 0; i < numelt; i++)
    SET_STRING_ELT(list_names, i,  mkChar(names[i]));
 
  PROTECT(list = allocVector(VECSXP, NC));// Creating a list with NC vector elements
  SET_VECTOR_ELT(list, 0, beta_);     // attaching beta vector to list
  SET_VECTOR_ELT(list, 1, itsR);      // attaching its vector to list
  SET_VECTOR_ELT(list, 2, deltaR);    // attaching delta vector to list
  SET_VECTOR_ELT(list, 3, infnormR);  // attaching infnorm vector to list
  if (params.s)
    SET_VECTOR_ELT(list, 4, sR);        // attaching s vector to list
  setAttrib(list, R_NamesSymbol, list_names); //and attaching the vector names
  UNPROTECT(numelt+2);
  return list;
  /*
  p = 3;
  PROTECT(beta_ = allocVector(REALSXP, p));
  double *beta = REAL(beta_);
  beta[0] = 1;
  beta[1] = 3;
  beta[2] = 5;
  UNPROTECT(1);
  return beta_;
  */
} 

