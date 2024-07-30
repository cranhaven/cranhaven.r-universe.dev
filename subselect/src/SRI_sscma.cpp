#include "ErrMReals.h" 
#include "SRI_sscma.h"

using namespace extendedleaps;

extern "C" 
SEXP eleaps(SEXP S,SEXP S2,SEXP Si,SEXP Segval,SEXP Segvct,
	SEXP E,SEXP Ei,SEXP Hegvct,SEXP HegvctTinv,SEXP HegvctEinv,
	SEXP wilksval,SEXP bartpival,SEXP lawhotval,SEXP ccr12val,
	SEXP  r,SEXP kmin,SEXP kmax,SEXP nsol,
	SEXP exclude,SEXP include,SEXP nexclude,SEXP ninclude,
	SEXP criterion,SEXP fixed,SEXP pcindices,SEXP nbindices,
	SEXP dim,SEXP timelimit,SEXP maxaperr,SEXP onlyforward)
{
	SEXP subsets,values,bestsets,bestvalues,dimsub,dimval,dimbsets,ans,ans_names;

	bool optimal,nomemory;
	int nsol1 = INTEGER(nsol)[0]; 
	int dim1 = INTEGER(dim)[0]; 
	int kmax1 = INTEGER(kmax)[0]; 
	int kmin1 = INTEGER(kmin)[0]; 
	int klength = kmax1 - kmin1 + 1;
	int checkcolinearity = INTEGER(onlyforward)[0];  

	if (!checkcolinearity) ErrMReals::errmonitreal<double>::dropec = true;   
	else ErrMReals::errmonitreal<double>::dropec = false;   

   	PROTECT(subsets = allocVector(INTSXP,nsol1*kmax1*klength));
   	PROTECT(values = allocVector(REALSXP,nsol1*klength));
   	PROTECT(bestsets = allocVector(INTSXP,kmax1*klength));
   	PROTECT(bestvalues = allocVector(REALSXP,klength));

	int retcode = extendedleaps::callsscma(
		REAL(S),REAL(S2),REAL(Si),REAL(Segval),REAL(Segvct),
		REAL(E),REAL(Ei),REAL(Hegvct),REAL(HegvctTinv),REAL(HegvctEinv),
		REAL(wilksval)[0],REAL(bartpival)[0],REAL(lawhotval)[0],REAL(ccr12val)[0],
		INTEGER(r)[0],kmin1,kmax1,nsol1,
		INTEGER(exclude),INTEGER(include),INTEGER(nexclude)[0],INTEGER(ninclude)[0],
		CHAR(STRING_ELT(criterion,0)),INTEGER(fixed)[0],INTEGER(pcindices),INTEGER(nbindices)[0],
		dim1,REAL(timelimit)[0],REAL(maxaperr)[0],checkcolinearity,
		INTEGER(subsets),REAL(values),REAL(bestvalues),INTEGER(bestsets),
		false
	);

	if (retcode == 4) nomemory = true;
	else nomemory = false;
	if (retcode==0 || retcode==2)  optimal = true;
	else optimal = false;
	if (retcode==2 || retcode==3)  {
		Rprintf("\nWarning: Because of numerical problems caused by strong multicolinearity\n");
		Rprintf("some subsets were excluded from the analysis.\n");
		Rprintf("You can try to increase the number of subsets to be compared by reducing the value\n");
		Rprintf("of the function argument maxaperr but the numerical accuracy of results may be compromised\n\n");
	}

   	PROTECT(dimsub = allocVector(INTSXP,3));
   	INTEGER(dimsub)[0] = nsol1;
   	INTEGER(dimsub)[1] = kmax1;
    	INTEGER(dimsub)[2] = klength;
  	SET_DIM(subsets,dimsub); 

   	PROTECT(dimval = allocVector(INTSXP,2));
   	INTEGER(dimval)[0] = nsol1;
    	INTEGER(dimval)[1] = klength;
  	SET_DIM(values,dimval); 
  	
   	PROTECT(dimbsets = allocVector(INTSXP,2));
   	INTEGER(dimbsets)[0] = klength;
    	INTEGER(dimbsets)[1] = kmax1; 
  	SET_DIM(bestsets,dimbsets); 

  	PROTECT(ans = NEW_LIST(6));

 	SET_VECTOR_ELT(ans, 0, subsets);
  	SET_VECTOR_ELT(ans, 1, values);
  	SET_VECTOR_ELT(ans, 2, bestvalues);
  	SET_VECTOR_ELT(ans, 3, bestsets);
  	SET_VECTOR_ELT(ans, 4, ScalarInteger(optimal));
  	SET_VECTOR_ELT(ans, 5, ScalarInteger(nomemory));

 	PROTECT(ans_names = NEW_CHARACTER(6));
  	SET_STRING_ELT(ans_names, 0, mkChar("subsets"));
  	SET_STRING_ELT(ans_names, 1, mkChar("values"));
  	SET_STRING_ELT(ans_names, 2, mkChar("bestvalues"));
  	SET_STRING_ELT(ans_names, 3, mkChar("bestsets"));
  	SET_STRING_ELT(ans_names, 4, mkChar("found"));
  	SET_STRING_ELT(ans_names, 5, mkChar("nomemory"));
  	setAttrib(ans, R_NamesSymbol, ans_names);

	UNPROTECT(9);
  	return(ans);
}


