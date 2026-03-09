#include <Rcpp.h>

using namespace Rcpp;


/*
parameters description:
x = inputs
p = array of weights of inputs x[],
w = array of weights for OWA, n = the dimension of x, p, w.
    the weights need not add to one but should be  non-negative.
n = the dimension of x,w,p
temp[] = working memory, keeps the spline knots and coefficients computed in weightedOWAQuantifierBuild in weightedowa.cpp
Tnum  = the number of knots in the monotone spline, as computed in  weightedOWAQuantifierBuild in weightedowa.cpp
L = number of binary tree levels. Run time = O[(n-1)L]

x, p, w, Temp are Vectors REALSXP
n, L, Tnum  are integers INTSXP

Function Fn is the symmetric base aggregator.

 */


// WOWA


#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

SEXP OWA_R(SEXP n, SEXP x, SEXP w);

RcppExport SEXP WOWA_OWA(SEXP nSEXP, SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
//    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    __result = Rcpp::wrap(OWA_R(n, x, w));
    return __result;
END_RCPP
}

SEXP WAM_R(SEXP n, SEXP x, SEXP w);

RcppExport SEXP WOWA_WAM(SEXP nSEXP, SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
//    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    __result = Rcpp::wrap(WAM_R(n, x, w));
    return __result;
END_RCPP
}
static const R_CallMethodDef callMethods_OWA[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_OWA[] = {
    INTSXP, REALSXP, REALSXP
};

static const R_CMethodDef cMethods_OWA[] = {
   {"OWA_R", (DL_FUNC) &OWA_R, 3, myC_t_OWA},
   {NULL, NULL, 0, NULL}
};
static const R_CMethodDef cMethods_WAM[] = {
   {"WAM_R", (DL_FUNC) &WAM_R, 3, myC_t_OWA},
   {NULL, NULL, 0, NULL}
};



SEXP weightedf_R(SEXP x, SEXP p, SEXP w, SEXP n, SEXP Fn, SEXP L);
RcppExport SEXP WOWA_weightedf(SEXP xSEXP, SEXP pSEXP, SEXP wSEXP, SEXP nSEXP, SEXP FnSEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type p(pSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Fn(FnSEXP);
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);	
    __result = Rcpp::wrap(weightedf_R(x, p, w, n, Fn, L));
    return __result;
END_RCPP
}

static const R_CallMethodDef callMethods_weightedf[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_weightedf[] = {
    REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP // Check type of  SEXP Fn 
};

static const R_CMethodDef cMethods_weightedf[] = {
   {"weightedf_R", (DL_FUNC) &weightedf_R, 6, myC_t_weightedf},
   {NULL, NULL, 0, NULL}
};




//SEXP weightedOWAQuantifierBuild_R(SEXP p, SEXP w, SEXP n, SEXP temp, SEXP Tnum);
SEXP weightedOWAQuantifierBuild_R(SEXP p, SEXP w, SEXP n);
RcppExport SEXP WOWA_weightedOWAQuantifierBuild(SEXP pSEXP, SEXP wSEXP, SEXP nSEXP) {
//RcppExport SEXP WOWA_weightedOWAQuantifierBuild(SEXP pSEXP, SEXP wSEXP, SEXP nSEXP, SEXP tempSEXP, SEXP TnumSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type p(pSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
 //   Rcpp::traits::input_parameter< SEXP >::type temp(tempSEXP);
 //   Rcpp::traits::input_parameter< SEXP >::type Tnum(TnumSEXP);

	SEXP spl=weightedOWAQuantifierBuild_R(p, w, n);
 //   __result = Rcpp::wrap(weightedOWAQuantifierBuild_R(p, w, n, temp, Tnum));
    //return __result;
	return spl;
END_RCPP
}

static const R_CallMethodDef callMethods_weightedOWAQuantifierBuild[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_weightedOWAQuantifierBuild[] = {
    REALSXP, REALSXP, INTSXP
};

static const R_CMethodDef cMethods_weightedOWAQuantifierBuild[] = {
   {"weightedOWAQuantifierBuild_R", (DL_FUNC) &weightedOWAQuantifierBuild_R, 3, myC_t_weightedOWAQuantifierBuild},
   {NULL, NULL, 0, NULL}
};



SEXP weightedOWAQuantifier_R(SEXP x, SEXP p, SEXP w, SEXP n, SEXP temp, SEXP Tnum);
RcppExport SEXP WOWA_weightedOWAQuantifier(SEXP xSEXP, SEXP pSEXP, SEXP wSEXP, SEXP nSEXP, SEXP tempSEXP, SEXP TnumSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type p(pSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type temp(tempSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Tnum(TnumSEXP);
    __result = Rcpp::wrap(weightedOWAQuantifier_R(x, p, w, n, temp, Tnum));
    return __result;
END_RCPP
}

static const R_CallMethodDef callMethods_weightedOWAQuantifier[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_weightedOWAQuantifier[] = {
    REALSXP, REALSXP, REALSXP, INTSXP, REALSXP, INTSXP
};

static const R_CMethodDef cMethods_weightedOWAQuantifier[] = {
   {"weightedOWAQuantifier_R", (DL_FUNC) &weightedOWAQuantifier_R, 6, myC_t_weightedOWAQuantifier},
   {NULL, NULL, 0, NULL}
};



SEXP ImplicitWOWA_R(SEXP x, SEXP p, SEXP w, SEXP n);
RcppExport SEXP WOWA_ImplicitWOWA(SEXP xSEXP, SEXP pSEXP, SEXP wSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type p(pSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    __result = Rcpp::wrap(ImplicitWOWA_R(x, p, w, n));
    return __result;
END_RCPP
}

static const R_CallMethodDef callMethods_ImplicitWOWA[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_ImplicitWOWA[] = {
    REALSXP, REALSXP, REALSXP, INTSXP
};

static const R_CMethodDef cMethods_ImplicitWOWA[] = {
   {"ImplicitWOWA_R", (DL_FUNC) &ImplicitWOWA_R, 4, myC_t_ImplicitWOWA},
   {NULL, NULL, 0, NULL}
};


SEXP WAn_R(SEXP x, SEXP w, SEXP n, SEXP L, SEXP Fn);
RcppExport SEXP WOWA_WAn(SEXP xSEXP, SEXP wSEXP, SEXP nSEXP, SEXP LSEXP, SEXP FnSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Fn(FnSEXP);	
    __result = Rcpp::wrap(WAn_R(x, w, n, L, Fn));
    return __result;
END_RCPP
}

static const R_CallMethodDef callMethods_WAn[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_WAn[] = {
    REALSXP, REALSXP, INTSXP, INTSXP, INTSXP
};

static const R_CMethodDef cMethods_WAn[] = {
   {"WAn_R", (DL_FUNC) &WAn_R, 5, myC_t_WAn},
   {NULL, NULL, 0, NULL}
};






void
R_init_WOWA(DllInfo *info)
{
   R_registerRoutines(info, cMethods_weightedf, callMethods_weightedf, NULL, NULL);
   R_registerRoutines(info, cMethods_OWA, callMethods_OWA, NULL, NULL);
   R_registerRoutines(info, cMethods_WAM, callMethods_OWA, NULL, NULL);
   R_registerRoutines(info, cMethods_weightedOWAQuantifierBuild, callMethods_weightedOWAQuantifierBuild, NULL, NULL);
   R_registerRoutines(info, cMethods_weightedOWAQuantifier, callMethods_weightedOWAQuantifier, NULL, NULL);
   R_registerRoutines(info, cMethods_ImplicitWOWA, callMethods_ImplicitWOWA, NULL, NULL);
   R_registerRoutines(info, cMethods_WAn, callMethods_WAn, NULL, NULL);
   
   R_useDynamicSymbols(info, TRUE);
}


