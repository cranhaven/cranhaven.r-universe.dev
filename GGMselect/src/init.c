/* ---------------------------------------------------------------
  GGMselect R package
  Copyright INRA 2017
  INRA, UR1404, Research Unit MaIAGE
  F78352 Jouy-en-Josas, France.
 
  URL: http://genome.jouy.inra.fr/logiciels/GGMselect
-------------------------------------------------------------- */

/* ++++++++++++++ INIT +++++++++++++++++++ */
#include "dclfunc.h"
/* +++  includes from R +++ */
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* loop.c */
static R_NativePrimitiveArgType GGMModC01_t[] = {
  INTSXP, INTSXP, INTSXP, REALSXP, REALSXP};

static R_NativePrimitiveArgType GGMloopAND_t[] = {
  INTSXP, INTSXP,  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, 
  REALSXP, REALSXP, REALSXP, REALSXP,
  INTSXP, INTSXP,  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, 
  REALSXP, INTSXP, REALSXP, 
  REALSXP, REALSXP, REALSXP, REALSXP,
  REALSXP, REALSXP, REALSXP, REALSXP,
  REALSXP, REALSXP, REALSXP, REALSXP,
  INTSXP};

static R_NativePrimitiveArgType GGMloopEWOR_t[] = {
    INTSXP, INTSXP,  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, 
    REALSXP, REALSXP, REALSXP, REALSXP,
    INTSXP, INTSXP,  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, 
    REALSXP, INTSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP,
    INTSXP};

static R_NativePrimitiveArgType GGMloopC01_t[] = {
 INTSXP, INTSXP,  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
  REALSXP, REALSXP, REALSXP, REALSXP,
INTSXP, INTSXP,  INTSXP, INTSXP, 
REALSXP, INTSXP, REALSXP,
 REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP,
    INTSXP};

 
static R_NativePrimitiveArgType GGMSCRa_t[] = {
  INTSXP, INTSXP, INTSXP, REALSXP, REALSXP,
INTSXP, REALSXP,
INTSXP, REALSXP,
INTSXP, REALSXP,
REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP,
  REALSXP, REALSXP, REALSXP};

static R_NativePrimitiveArgType GGMGrMin_t[] = {
INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, 
REALSXP, REALSXP,
INTSXP, INTSXP, 
REALSXP, INTSXP, INTSXP};


/* critQE.c */
static R_NativePrimitiveArgType  GGMloopGrSymQE_t[] = {
INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
INTSXP, INTSXP, INTSXP, INTSXP};

/* C methods */
static const R_CMethodDef cMethods[] = {
  {"GGMModC01", (DL_FUNC) &GGMModC01, 5, GGMModC01_t},
  {"GGMloopAND", (DL_FUNC) &GGMloopAND, 34, GGMloopAND_t},
  {"GGMloopEWOR", (DL_FUNC) &GGMloopEWOR, 34, GGMloopEWOR_t},
  {"GGMloopC01", (DL_FUNC) &GGMloopC01, 31, GGMloopC01_t},
  {"GGMSCRa", (DL_FUNC) &GGMSCRa, 22, GGMSCRa_t},
  {"GGMGrMin", (DL_FUNC) &GGMGrMin, 12, GGMGrMin_t},
  {"GGMloopGrSymQE", (DL_FUNC) &GGMloopGrSymQE, 9, GGMloopGrSymQE_t},
 {NULL, NULL, 0}
};

/* Call methods */
static const R_CallMethodDef callMethods[] = {
  {"GGMcalcSCRQE", (DL_FUNC) &GGMcalcSCRQE,7} ,
  {"GGMscrgcritQE", (DL_FUNC) &GGMscrgcritQE, 1},
  {"GGMcritminQE",   (DL_FUNC) &GGMcritminQE, 1},
  {"GGMbcSW",   (DL_FUNC) &GGMbcSW, 8},
 {NULL, NULL, 0}
};

/* Fortran */
static const R_FortranMethodDef FortMethods[] = {
  {"bouclet", (DL_FUNC) &F77_SUB(bouclet), 13},
   {NULL, NULL, 0}
};

void   R_init_GGMselect(DllInfo *info)
{
  R_registerRoutines(info, cMethods,callMethods, FortMethods, NULL);
 R_useDynamicSymbols(info, FALSE);

}


