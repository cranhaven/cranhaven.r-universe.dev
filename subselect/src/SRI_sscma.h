#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

extern "C" 
SEXP eleaps(SEXP S,SEXP S2,SEXP Si,SEXP Segval,SEXP Segvct,
	SEXP E,SEXP Ei,SEXP Hegvct,SEXP HegvctTinv,SEXP HegvctEinv,
	SEXP wilksval,SEXP bartpival,SEXP lawhotval,SEXP ccr12val,
	SEXP  r,SEXP kmin,SEXP kmax,SEXP nsol,
	SEXP exclude,SEXP include,SEXP nexclude,SEXP ninclude,
	SEXP criterion,SEXP fixed,SEXP pcindices,SEXP nbindices,
	SEXP dim,SEXP timelimit,SEXP ntol,SEXP onlyforward);

namespace extendedleaps {
		
int callsscma(double *S,double *S2,double *Si,double *Segval,double *Segvct,
	double *E,double *Ei,double *Hegvct,double *HegvctTinv,double *HegvctEinv,
	double wilksval,double bartpival,double lawhotval,double ccr12val,int r,
	int kmin,int kmax,int nsol,int *out,int *in,int nout,int nin,
	const char *cmpcr,int fixed,int *pcind,int nind,int nvar,double timelimit,
	double ntol,bool onlyforward,int *subs,double *subsv,double *bestsv,int *bests,
	bool printmsg);
}

