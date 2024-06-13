#include "Rinternals.h"
#include "R.h"

void F77_NAME(singchk)(int *, int *, double *, double *, double *, double *,
		    double *, int *, double *, int *);


void F77_NAME(includ)(int *, int *, double *, double *, double *,
		      double *, double *, double *, double *, int *);


void F77_NAME(tolset)(int *, int *, double *, double *, double *,
		      double *, int *);


SEXP updateQR(SEXP X, SEXP y, SEXP w, SEXP bigQR, SEXP intercept){

  int i,j,n,p,nrbar;
  int ier;
  double *row;
  SEXP D,Rbar, thetab, sse;

  PROTECT(bigQR = duplicate(bigQR));

  D = VECTOR_ELT(bigQR,0);
  Rbar = VECTOR_ELT(bigQR,1);
  thetab = VECTOR_ELT(bigQR, 2);
  sse =VECTOR_ELT(bigQR,3);

  p=length(D);
  n=length(X)/(p-LOGICAL(intercept)[0]);
  nrbar=length(Rbar);
  ier=0;  

  row = (double *) R_alloc(p, sizeof(double));
  
  for(i=0; i<n; i++){
    if(LOGICAL(intercept)[0])
      row[0]=1.0;
    for(j=LOGICAL(intercept)[0]; j<p; j++)
      row[j]= REAL(X)[i+j*n];

    F77_CALL(includ)(&p, &nrbar, REAL(w)+i, row, 
		     REAL(y)+i, REAL(D), REAL(Rbar), REAL(thetab),
		     REAL(sse), &ier);
  }
  
  LOGICAL(VECTOR_ELT(bigQR,4))[0] = 0; /*checked*/
      
  UNPROTECT(1);
  
  return bigQR;
}


SEXP singcheckQR(SEXP bigQR){

  SEXP Rbar,D,thetab,sse,tol,work,lindep;
  int ier=0;
  int  p, nrbar;

  PROTECT(bigQR=duplicate(bigQR));

  D = VECTOR_ELT(bigQR,0);
  Rbar=VECTOR_ELT(bigQR,1);
  thetab=VECTOR_ELT(bigQR,2);
  sse=VECTOR_ELT(bigQR,3);
  tol=VECTOR_ELT(bigQR,5);

  p=length(D);
  nrbar=length(Rbar);


  PROTECT(work=allocVector(REALSXP,length(D)));
  PROTECT(lindep=allocVector(INTSXP,length(D)));

  /* tolset */
  F77_CALL(tolset)(&p, &nrbar, REAL(D), REAL(Rbar),
	   REAL(tol), REAL(work), &ier);

  /* singcheck */
  F77_CALL(singchk)(&p, &nrbar, REAL(D), REAL(Rbar), REAL(thetab),
		 REAL(sse), REAL(tol), INTEGER(lindep), REAL(work), &ier);
  
  LOGICAL(VECTOR_ELT(bigQR,4))[0]=1;

  UNPROTECT(3); /* lindep, work, bigQR */
    
  return bigQR;
    
}

