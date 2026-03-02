#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _BayesDLMfMRI_Group_Functional_Backwards_Sampling(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Group_Functional_Equation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Group_FunctionalMultiTest(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Gruop_FunctionalTestLT(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Individual_Backwards_Sampling(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Individual_Functional_States(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Individual_FunctionalMultiTest(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BayesDLMfMRI_Individual_FunctionalTestLT(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_BayesDLMfMRI_Group_Functional_Backwards_Sampling", (DL_FUNC) &_BayesDLMfMRI_Group_Functional_Backwards_Sampling, 12},
    {"_BayesDLMfMRI_Group_Functional_Equation",           (DL_FUNC) &_BayesDLMfMRI_Group_Functional_Equation,           12},
    {"_BayesDLMfMRI_Group_FunctionalMultiTest",           (DL_FUNC) &_BayesDLMfMRI_Group_FunctionalMultiTest,           12},
    {"_BayesDLMfMRI_Gruop_FunctionalTestLT",              (DL_FUNC) &_BayesDLMfMRI_Gruop_FunctionalTestLT,              12},
    {"_BayesDLMfMRI_Individual_Backwards_Sampling",       (DL_FUNC) &_BayesDLMfMRI_Individual_Backwards_Sampling,       10},
    {"_BayesDLMfMRI_Individual_Functional_States",        (DL_FUNC) &_BayesDLMfMRI_Individual_Functional_States,        10},
    {"_BayesDLMfMRI_Individual_FunctionalMultiTest",      (DL_FUNC) &_BayesDLMfMRI_Individual_FunctionalMultiTest,      10},
    {"_BayesDLMfMRI_Individual_FunctionalTestLT",         (DL_FUNC) &_BayesDLMfMRI_Individual_FunctionalTestLT,         10},
    {NULL, NULL, 0}
};


