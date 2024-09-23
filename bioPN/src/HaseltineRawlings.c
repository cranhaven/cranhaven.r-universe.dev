// -*- mode: C; c-indent-level: 2; c-basic-offset: 2; tab-width: 8 -*-
//
// Copyright (C) 2009-2014 Roberto Bertolusso and Marek Kimmel
//
// This file is part of bioPN.
//
// bioPN is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// bioPN is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with bioPN. If not, see <http://www.gnu.org/licenses/>.

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include "quicksort.h"
#include "helper.h"

//#define RB_PRINT_INCR_INFO
//#define RB_SAVE_INCR_INFO

#define a21 1./5.
#define c2 1./5.

#define a31 3./40.
#define a32 9./40.
#define c3 3./10.

#define a41 44./45.
#define a42 -56./15.
#define a43 32./9.
#define c4 4./5.

#define a51 19372./6561.
#define a52 -25360./2187.
#define a53 64448./6561.
#define a54 -212./729.
#define c5 8./9.

#define a61 9017./3168.
#define a62 -355./33.
#define a63 46732./5247.
#define a64 49./176.
#define a65 -5103./18656.
#define c6 1.

#define a71 35./384.
#define a73 500./1113.
#define a74 125./192.
#define a75 -2187./6784.
#define a76 11./84.
#define c7 1.

#define b41 5179./57600.
#define b43 7571./16695.
#define b44 393./640.
#define b45 -92097./339200.
#define b46 187./2100.
#define b47 1./40.

#define b51 35./384.
#define b53 500./1113.
#define b54 125./192.
#define b55 -2187./6784.
#define b56 11./84.

#define err_pow 1./6.

#define INCR_TO_SAVE 200000

#define RB_TIME
#define RB_SUBTIME

#ifdef RB_TIME
#include <time.h>
#endif

typedef enum {
  HZ_DOUBLE,
  HZ_CFUNCTION,
  HZ_RFUNCTION
} HZ_type;

#define MIN_INCR 1e-20


SEXP HaseltineRawlings(SEXP pre, SEXP post, SEXP h, SEXP slow, SEXP M, SEXP T, SEXP delta,
		       SEXP runs, SEXP place, SEXP transition, SEXP ect, SEXP rho)
{
  int k;

#ifdef RB_TIME
  clock_t c0, c1;
  c0 = clock();
#endif

  // Get dimensions of pre
  int *piTmp = INTEGER(getAttrib(pre, R_DimSymbol));
  int iTransitions = piTmp[0], iPlaces = piTmp[1];

  int *piPre = INTEGER(pre), *piPost = INTEGER(post);

  SEXP sexpTmp;

  int iTransition, iPlace, iTransitionPtr, iPlacePtr,
    iTransition2, iPlace2, iTransitionPtr2, iPlacePtr2;

  // Find out which elements of h are doubles and which functions
  SEXP sexpFunction;
  PROTECT(sexpFunction = allocVector(VECSXP, iTransitions));
  double *pdH = (double *) R_alloc(iTransitions, sizeof(double));
  DL_FUNC *pCFunction = (DL_FUNC *) R_alloc(iTransitions, sizeof(DL_FUNC *));
  int *piHzType = (int *) R_alloc(iTransitions, sizeof(int));
  for (iTransition = 0; iTransition < iTransitions; iTransition++) {
    if (inherits(sexpTmp = VECTOR_ELT(h, iTransition), "NativeSymbol")) {
      pCFunction[iTransition] = (void *) R_ExternalPtrAddr(sexpTmp);
      piHzType[iTransition] = HZ_CFUNCTION;    
    } else if (isNumeric(sexpTmp)){
      pdH[iTransition] = REAL(sexpTmp)[0];
      piHzType[iTransition] = HZ_DOUBLE;
    } else  if (isFunction(sexpTmp)) {
      SET_VECTOR_ELT(sexpFunction, iTransition, lang1(sexpTmp));
      piHzType[iTransition] = HZ_RFUNCTION;
    } else {
      error("Unrecongnized transition function type\n");
    }
  }

  // Setup Matrix S
  int *piS = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  int *piSlowTransition = (int *) R_alloc(iTransitions, sizeof(int));
  int *piFastTransition = (int *) R_alloc(iTransitions, sizeof(int));
  int iSlowTransitions = 0, iFastTransitions = 0;
  
  // Position of non zero cells in pre per transition
  int *piPreNZxRow = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  
  // Totals of non zero cells in pre per transition
  int *piPreNZxRowTot = (int *) R_alloc(iTransitions, sizeof(int));
  
  // Position of non zero cells in S per transition
  int *piSNZxRow = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  
  // Totals of non zero cells in S per transition
  int *piSNZxRowTot = (int *) R_alloc(iTransitions, sizeof(int));
  
  for (iTransition = 0; iTransition < iTransitions; iTransition++) {
    int iPreNZxRow_col = 0;
    int iSNZxRow_col = 0;
    for (iPlace = 0; iPlace < iPlaces; iPlace++) {
      if (piPre[iTransition + iTransitions * iPlace]) {
	piPreNZxRow[iTransition + iTransitions * iPreNZxRow_col++] = iPlace;
      }
      if ((piS[iTransition + iTransitions * iPlace] = 
	   piPost[iTransition + iTransitions * iPlace] - piPre[iTransition + iTransitions * iPlace])) {
	piSNZxRow[iTransition + iTransitions * iSNZxRow_col++] = iPlace;
      }
    }
    piPreNZxRowTot[iTransition] = iPreNZxRow_col;
    piSNZxRowTot[iTransition] = iSNZxRow_col;
    
    if (INTEGER(slow)[iTransition])
      piSlowTransition[iSlowTransitions++] = iTransition;
    else
      piFastTransition[iFastTransitions++] = iTransition;
  }
  
  int *piFastPlace = (int *) R_alloc(iPlaces, sizeof(int));
  int iFastPlaces = 0;
  
  // Position of non zero cells in pre per transition
  int *piSlowPreNZxCol = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  // Totals of non zero cells in pre per transition
  int *piSlowPreNZxColTot = (int *) R_alloc(iTransitions, sizeof(int));
  
  for (iPlace = 0; iPlace < iPlaces; iPlace++) {
    int iFastPlace = FALSE;
    for (iTransitionPtr = 0; iTransitionPtr < iFastTransitions; iTransitionPtr++) {
      iTransition = piFastTransition[iTransitionPtr];
      
      if(piS[iTransition + iTransitions * iPlace]) {
	iFastPlace = TRUE;
      }
    }
    if (iFastPlace)
      piFastPlace[iFastPlaces++] = iPlace;
    
    int iSlowPreNZxCol_row = 0;
    for (iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
      iTransition = piSlowTransition[iTransitionPtr];
      
      if(piPre[iTransition + iTransitions * iPlace]) {
	piSlowPreNZxCol[iSlowPreNZxCol_row++ + iTransitions * iPlace] = iTransition;
      }
    }
    piSlowPreNZxColTot[iPlace] = iSlowPreNZxCol_row;
  }
  
  // Hazards that need to be recalculated if a given transition has happened
  int *piHazardsToModxRow = (int *) R_alloc((iTransitions + 2) * iTransitions, sizeof(int));
  
  // Totals of hazards to recalculate for each transition that has happened
  int *piHazardsToModxRowTot = (int *) R_alloc(iTransitions + 2, sizeof(int));
  piHazardsToModxRowTot[iTransitions + 1] = 0;
  
  for (iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
    iTransition = piSlowTransition[iTransitionPtr];
    
    int iSlowTransitionHazardUpdatedByFastPlace = FALSE;
    int iHazardToCompTot = 0;
    
    for(iPlace = 0; iPlace < iPlaces; iPlace++) {
      
      if (piS[iTransition + iTransitions * iPlace]) {
	// Identify the transitions that need the hazards recalculated
	for(iTransitionPtr2 = 0; iTransitionPtr2 < piSlowPreNZxColTot[iPlace]; iTransitionPtr2++) {
	  iTransition2 = piSlowPreNZxCol[iTransitionPtr2 + iTransitions * iPlace];
	  int iAddThis = TRUE;
	  for (k = 0; k < iHazardToCompTot; k++) {
	    if(piHazardsToModxRow[iTransition + (iTransitions + 2) * k] == iTransition2) {
	      iAddThis = FALSE;
	      break;
	    }
	  }    
	  if (iAddThis)
	    piHazardsToModxRow[iTransition + (iTransitions + 2) * iHazardToCompTot++] = iTransition2;
	}
      }
      
      // Which slow transitions hazard have to be recalculated after updating the fast places.
      if (!iSlowTransitionHazardUpdatedByFastPlace && piPre[iTransition + iTransitions * iPlace]) {
	for(iPlacePtr2 = 0; iPlacePtr2 < iFastPlaces; iPlacePtr2++) {
	  iPlace2 = piFastPlace[iPlacePtr2];
	  if (iPlace2 == iPlace) {
	    iSlowTransitionHazardUpdatedByFastPlace = TRUE;
	    piHazardsToModxRow[iTransitions + 1 + 
			       (iTransitions + 2) * piHazardsToModxRowTot[iTransitions + 1]++] = iTransition;
	    break;
	  }
	}
      }
    }
    piHazardsToModxRowTot[iTransition] = iHazardToCompTot;
  }
  int iWhichSlowFromFastTransitions = iTransitions + 1;
  
  // For the initial calculation of all hazards...
  for(iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
    iTransition = piSlowTransition[iTransitionPtr];
    piHazardsToModxRow[iTransitions + (iTransitions + 2) * iTransitionPtr] = iTransition;
  }
  piHazardsToModxRowTot[iTransitions] = iSlowTransitions;
  
  double *pdK1 = (double *) R_alloc(7*iPlaces, sizeof(double));
  double *pdK2 = pdK1 + iPlaces;
  double *pdK3 = pdK2 + iPlaces;
  double *pdK4 = pdK3 + iPlaces;
  double *pdK5 = pdK4 + iPlaces;
  double *pdK6 = pdK5 + iPlaces;
  double *pdK7 = pdK6 + iPlaces;
  double *pdYdot = 0;
  
  double dEct = *REAL(ect);
  
  SEXP sexpTmpCrntMarking;
  PROTECT(sexpTmpCrntMarking = allocVector(REALSXP, iPlaces));
  double *pdTmpCrntMarking = REAL(sexpTmpCrntMarking);
  
  SEXP sexpCrntMarking;
  PROTECT(sexpCrntMarking = allocVector(REALSXP, iPlaces));
  double *pdCrntMarking = REAL(sexpCrntMarking);
  
  double *pdBakCrntMarking = (double *) R_alloc(iPlaces, sizeof(double));
  
  double *pdCrntDiffMarking = (double *) R_alloc(iPlaces, sizeof(double));
  
  double dDelta = *REAL(delta);
  int iTotalSteps, iSectionSteps;
  double dT = 0;
  void *pCManage_time = 0;
  SEXP sexpRManage_time = 0;
  if (inherits(T, "NativeSymbol")) {
    pCManage_time = (void *) R_ExternalPtrAddr(T);
    dT = ((double(*)(double, double *)) pCManage_time)(-1, pdCrntMarking);
  } else if (isNumeric(T)){
    dT = *REAL(T);
  } else if (isFunction(T)) {
    PROTECT(sexpRManage_time = lang1(T));
    
    defineVar(install("y"), sexpCrntMarking, rho);
    PROTECT(sexpTmp = allocVector(REALSXP, 1));
    *REAL(sexpTmp) = -1;
    defineVar(install("StartTime"), sexpTmp, rho);
    UNPROTECT_PTR(sexpTmp);
    dT = *REAL(VECTOR_ELT(eval(sexpRManage_time, rho),0));
  } else {
    error("Unrecognized time function type\n");
  }
  iTotalSteps = iSectionSteps = (int)(dT / dDelta) + 1;

  // Hazard vector
  double *pdHazard = (double *) R_alloc(iTransitions, sizeof(double));
  double *pdBakHazard = (double *) R_alloc(iTransitions, sizeof(double));
  
  int iRun, iRuns = *INTEGER(runs);
  
  SEXP sexpRun;
  PROTECT(sexpRun = allocVector(VECSXP, iRuns));
  
  int iTotalUsedRandomNumbers = 0;
  
  // DiscTime Vector
  SEXP sexpD_time;
  PROTECT(sexpD_time = allocVector(REALSXP, iTotalSteps));
  double *pdDiscTime = REAL(sexpD_time);
  double dTmp = 0;
  for (k = 0; k < iTotalSteps; k++) {
    pdDiscTime[k] = dTmp;
    dTmp += dDelta;
  }
  
  SEXP sexpMarkingRowNames;
  PROTECT(sexpMarkingRowNames = allocVector(INTSXP, iTotalSteps));
  piTmp = INTEGER(sexpMarkingRowNames);
  for (k = 0; k < iTotalSteps; k++)
    piTmp[k] = k+1;
  
  double **ppdMarking = (double **) R_alloc(iPlaces, sizeof(double *));
  double **ppdHazard = (double **) R_alloc(iTransitions, sizeof(double *));
  
#ifdef RB_SAVE_INCR_INFO
  double *pdIncr = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdIncrTime = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdAcumHazard = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdIntHazard = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdIntHazardTime = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
#endif
  
  int *piOrderedTransition = (int *) R_alloc(iTransitions, sizeof(int));
  
  GetRNGstate();
  for (iRun = 0; iRun < iRuns; iRun++) {
    
#ifdef RB_SAVE_INCR_INFO
    int iTotAccpIncr = 0, iTotRejIncr = 0, iTotGoBackIncr = 0, iTotIntHazardTime = 0, iTotIncrTime = 0;
    double dSumAccpIncr = 0;
    double dSumSqAccpIncr = 0;
#endif
    
    int iUsedRandomNumbers = 0;
    Rprintf("%d ", iRun+1);
    
    // Totals for kind of transition vector
    SEXP sexpTotXTransition;
    PROTECT(sexpTotXTransition = allocVector(INTSXP, iTransitions));
    int *piTotTransitions = INTEGER(sexpTotXTransition);
    SEXP sexpHazard;
    PROTECT(sexpHazard = allocVector(VECSXP, iTransitions));
    
    for(iTransition = 0; iTransition < iTransitions; iTransition++) {
      SET_VECTOR_ELT(sexpHazard, iTransition, sexpTmp = allocVector(REALSXP, iTotalSteps));
      ppdHazard[iTransition] = REAL(sexpTmp);

      piTotTransitions[iTransition] = 0;
    }
    for(iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
      piOrderedTransition[iTransitionPtr] = piSlowTransition[iTransitionPtr];
    }
    int iTillResort = 1000, iTotResort = 0;
    
    SEXP sexpMarking;
    PROTECT(sexpMarking = allocVector(VECSXP, iPlaces));
    //setAttrib(sexpMarking, R_NamesSymbol, place);
    //setAttrib(sexpMarking, R_RowNamesSymbol, sexpMarkingRowNames);
    //setAttrib(sexpMarking, R_ClassSymbol, ScalarString(mkChar("data.frame")));

    // Setup initial state
    double *pdTmp = REAL(M);
    for (iPlace = 0; iPlace < iPlaces; iPlace++) {
      SET_VECTOR_ELT(sexpMarking, iPlace, sexpTmp = allocVector(REALSXP, iTotalSteps));
      ppdMarking[iPlace] = REAL(sexpTmp);

      pdCrntMarking[iPlace] = pdBakCrntMarking[iPlace] = pdTmp[iPlace];
    }

    double dTime, dTarget = 0;
    int iTotTransitions = 0;
    
    double dIncr = MIN_INCR, dStartIncr;
    double dAbsoluteMaxError = 0;

    int iStep = 0;
    int iAcceptedIncr = 0, iRejectedIncr = 0, iInterruptCnt = 100000;
    double dNewHazard = 0;
    do {
      if (pCManage_time || sexpRManage_time) {
	double dEnd = 0;
	if (pCManage_time) {
	  dEnd = ((double(*)(double, double *)) pCManage_time)(dTarget, pdCrntMarking);
	} else {
	  defineVar(install("y"), sexpCrntMarking, rho);
	  PROTECT(sexpTmp = allocVector(REALSXP, 1));
	  *REAL(sexpTmp) = dTarget;
	  defineVar(install("StartTime"), sexpTmp, rho);
	  UNPROTECT_PTR(sexpTmp);
	  
	  sexpTmp = eval(sexpRManage_time, rho);
	  dEnd = *REAL(VECTOR_ELT(sexpTmp,0));
	  for(iPlace = 0; iPlace < iPlaces; iPlace++) {
	    pdCrntMarking[iPlace] = REAL(VECTOR_ELT(sexpTmp,1))[iPlace];
	  }
	}
	iSectionSteps = (int)(dEnd / dDelta) + 1;
      }
      
      for(iPlace = 0; iPlace < iPlaces; iPlace++) {
	ppdMarking[iPlace][iStep] = pdBakCrntMarking[iPlace] = pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace];
      }
      
      dTime = dTarget;
      dTarget += dDelta;

      dStartIncr = dIncr;
      
      // For the calculation of all hazards...
      int iLastTransition = iTransitions;
      
      double dAcumHazard = 0;
      for(iTransition = 0; iTransition < iTransitions; iTransition++) {
	ppdHazard[iTransition][iStep] = pdHazard[iTransition] = 0;
      }
      
      do {    
	// Get hazards only for the transitions associated with
	// places whose quantities changed in the last step.
	for(iTransitionPtr = 0; iTransitionPtr < piHazardsToModxRowTot[iLastTransition]; iTransitionPtr++) {
	  iTransition = piHazardsToModxRow[iLastTransition + (iTransitions + 2) * iTransitionPtr];
	  switch(piHzType[iTransition]) {
	  case HZ_DOUBLE:
	    dNewHazard = pdH[iTransition];
	    for(iPlacePtr = 0; iPlacePtr < piPreNZxRowTot[iTransition]; iPlacePtr++) {
	      iPlace = piPreNZxRow[iTransition + iTransitions * iPlacePtr];
	      for (k = 0; k < piPre[iTransition + iTransitions * iPlace]; k++)
		dNewHazard *= (pdCrntMarking[iPlace] - k) / (double)(k+1);
	    }
	    break;
	  case HZ_CFUNCTION:
	    dNewHazard = ((double(*)(double, double *)) pCFunction[iTransition])(dTime, pdCrntMarking);
	    break;
	  default:
	    // case HZ_RFUNCTION:
	    defineVar(install("y"), sexpCrntMarking, rho);
	    dNewHazard = REAL(eval(VECTOR_ELT(sexpFunction, iTransition), rho))[0];
	    break;
	  }
	  dAcumHazard += dNewHazard - pdHazard[iTransition];
	  pdHazard[iTransition] = dNewHazard;
	}
	
	double dLogRandom;
	if (iSlowTransitions) {
	  dLogRandom = log(unif_rand());
	  iUsedRandomNumbers++;
	} else
	  dLogRandom = -DBL_MAX;
	double dIntHazard = 0;
	
#ifdef RB_SAVE_INCR_INFO
	if (iTotAccpIncr < INCR_TO_SAVE) {
	  pdAcumHazard[iTotIntHazardTime] = dAcumHazard;
	  pdIntHazard[iTotIntHazardTime] = dLogRandom;
	  pdIntHazardTime[iTotIntHazardTime++] = dTime;
	}
#endif
	dIncr = dStartIncr;
	dStartIncr = -1;
	
	int iRKs;
	double dEvalTime = 0;
	int iContinue = TRUE;
	int iStartRK = 1;
	do {
	  if (dTime + dIncr > dTarget) {
	    dIncr = dTarget - dTime;
	  }
	  for(iRKs = iStartRK; iRKs < 8; iRKs++) {
	    switch (iRKs) {
	    case 1:
	      dEvalTime = dTime;
	      pdYdot = pdK1;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace];
	      }
	      break;
	    case 2:
	      dEvalTime = dTime + c2 * dIncr;
	      pdYdot = pdK2;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace] +
		  a21*pdK1[iPlace] * dIncr;
	      }
	      break;
	    case 3:
	      dEvalTime = dTime + c3 * dIncr;
	      pdYdot = pdK3;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace] +
		  (a31*pdK1[iPlace] + a32*pdK2[iPlace]) * dIncr;
	      }
	      break;
	    case 4:
	      dEvalTime = dTime + c4 * dIncr;
	      pdYdot = pdK4;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace] +
		  (a41*pdK1[iPlace] + a42*pdK2[iPlace] + a43*pdK3[iPlace]) * dIncr;
	      }
	      break;
	    case 5:
	      dEvalTime = dTime + c5 * dIncr;
	      pdYdot = pdK5;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace] +
		  (a51*pdK1[iPlace] + a52*pdK2[iPlace] + a53*pdK3[iPlace] +
		   a54*pdK4[iPlace]) * dIncr;
	      }
	      break;
	    case 6:
	      dEvalTime = dTime + c6 * dIncr;
	      pdYdot = pdK6;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace] +
		  (a61*pdK1[iPlace] + a62*pdK2[iPlace] + a63*pdK3[iPlace] +
		   a64*pdK4[iPlace] + a65*pdK5[iPlace]) * dIncr;
	      }
	      break;
	    case 7:
	      dEvalTime = dTime + c7 * dIncr;
	      pdYdot = pdK7;
	      for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
		iPlace = piFastPlace[iPlacePtr];
		pdYdot[iPlace] = 0;
		pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace] +
		  (a71*pdK1[iPlace] + a73*pdK3[iPlace] + a74*pdK4[iPlace] +
		   a75*pdK5[iPlace] + a76*pdK6[iPlace]) * dIncr;
	      }
	      break;
	    }
	    for(iTransitionPtr = 0; iTransitionPtr < iFastTransitions; iTransitionPtr++) {
	      iTransition = piFastTransition[iTransitionPtr];
	      switch(piHzType[iTransition]) {
	      case HZ_DOUBLE:
		dNewHazard = pdH[iTransition];
		for(iPlacePtr = 0; iPlacePtr < piPreNZxRowTot[iTransition]; iPlacePtr++) {
		  iPlace = piPreNZxRow[iTransition + iTransitions * iPlacePtr];
		  for (k = 0; k < piPre[iTransition + iTransitions * iPlace]; k++)
		    dNewHazard *= pdTmpCrntMarking[iPlace];
		}
		break;
	      case HZ_CFUNCTION:
		dNewHazard = ((double(*)(double, double *)) pCFunction[iTransition])(dEvalTime, pdTmpCrntMarking);
		break;
	      default:
		// case HZ_RFUNCTION:
		defineVar(install("y"), sexpTmpCrntMarking, rho);
		dNewHazard = REAL(eval(VECTOR_ELT(sexpFunction, iTransition), rho))[0];
		break;
	      }
	      pdHazard[iTransition] = dNewHazard;

	      for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
		iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
		pdYdot[iPlace] += piS[iTransition + iTransitions * iPlace] * dNewHazard;
	      }
	    }
	  }
	  double dInfNormError = 0, dInfNormMarking = 0;
	  for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
	    iPlace = piFastPlace[iPlacePtr];
	    double dYj = 
	      (b41*pdK1[iPlace] + b43*pdK3[iPlace] + b44*pdK4[iPlace] + b45*pdK5[iPlace] + b46*pdK6[iPlace] + b47*pdK7[iPlace]) * dIncr;
	    double dZj = 
	      (pdCrntDiffMarking[iPlace] = (b51*pdK1[iPlace] + b53*pdK3[iPlace] + b54*pdK4[iPlace] + b55*pdK5[iPlace] + b56*pdK6[iPlace]) * dIncr);
	    
	    double dThisError;
	    if ((dThisError = dYj-dZj) < 0.)
	      dThisError = -dThisError;
	    if (dThisError > dInfNormError)
	      dInfNormError = dThisError;

	    double dThisMarking;	    
	    if ((dThisMarking = pdCrntMarking[iPlace]) < 0.)
	      dThisMarking = -dThisMarking;
	    if (dThisMarking > dInfNormMarking)
	      dInfNormMarking = dThisMarking;
	  }
	  if (dInfNormMarking > 1.)
	    dInfNormMarking = 1.;
	  double dTau;
	  if ((dTau = dEct*dInfNormMarking) == 0.)
	    dTau = MIN_INCR;

	  if (dInfNormError == 0.) {
	    if (dTau == MIN_INCR)
	      dInfNormError = MIN_INCR*1e-4;
	    else
	      dInfNormError = MIN_INCR;
	  }

	  if (dInfNormError > dTau) {
	    // Current increment is rejected.
	    // Try a new one and retry integration step
	    dIncr = .8 * dIncr * R_pow(dTau/dInfNormError,err_pow);
	    iRejectedIncr++;
	    continue;
	  }
#ifdef RB_PRINT_INCR_INFO
	  Rprintf("Accepted dIncr: %e\n", dIncr);
#endif
	  
	  iAcceptedIncr++;
	  if (dInfNormError > dAbsoluteMaxError)
	    dAbsoluteMaxError = dInfNormError;

	  for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
	    iPlace = piFastPlace[iPlacePtr];
	    pdBakCrntMarking[iPlace] = pdCrntMarking[iPlace];
	    pdCrntMarking[iPlace] += pdCrntDiffMarking[iPlace];
	    pdK1[iPlace] = pdK7[iPlace];
	  }
	  if (iStartRK == 1)
	    iStartRK = 2;
	  
	  double dPrevAcumHazard = dAcumHazard;
	  for(iTransitionPtr = 0; iTransitionPtr < piHazardsToModxRowTot[iWhichSlowFromFastTransitions]; iTransitionPtr++) {
	    iTransition = piHazardsToModxRow[iWhichSlowFromFastTransitions + (iTransitions + 2) * iTransitionPtr];
	    switch(piHzType[iTransition]) {
	    case HZ_DOUBLE:
	      dNewHazard = pdH[iTransition];
	      for(iPlacePtr = 0; iPlacePtr < piPreNZxRowTot[iTransition]; iPlacePtr++) {
		iPlace = piPreNZxRow[iTransition + iTransitions * iPlacePtr];
		for (k = 0; k < piPre[iTransition + iTransitions * iPlace]; k++)
		  dNewHazard *= (pdCrntMarking[iPlace] - k) / (double)(k+1);
	      }
	      break;
	    case HZ_CFUNCTION:
	      dNewHazard = ((double(*)(double, double *)) pCFunction[iTransition])(dTime, pdCrntMarking);
	      break;
	    case HZ_RFUNCTION:
	      defineVar(install("y"), sexpCrntMarking, rho);
	      dNewHazard = REAL(eval(VECTOR_ELT(sexpFunction, iTransition), rho))[0];
	      break;
	    }
	    dAcumHazard += dNewHazard - (pdBakHazard[iTransition] = pdHazard[iTransition]);
	    pdHazard[iTransition] = dNewHazard;
	  }
	  
	  double dIncrIntHazard = (dPrevAcumHazard + dAcumHazard) * dIncr / 2;
	  double dDiff = dIntHazard + dIncrIntHazard + dLogRandom;
	  if (fabs(dDiff) < dEct) {
	    // Next check is needed because once in a while
	    // you will end here on a first attempt
	    // and not after having entered the else below
	    // on the previous iteration, and you want to
	    // avoid having dStartIncr < 0 which will be
	    // assigned to dIncr for the next cycle...
	    if (dStartIncr < 0)
	      dStartIncr = dIncr;
	    iContinue = FALSE;
	    dIntHazard += dIncrIntHazard;
#ifdef RB_PRINT_INCR_INFO
	    Rprintf("Exit: %e\n", dIncr);
#endif
	  } else if (dDiff < 0) {
	    dIntHazard += dIncrIntHazard;
	  } else {
	    if (dStartIncr < 0)
	      dStartIncr = dIncr;
	    dIncr *= - (dIntHazard + dLogRandom) / dIncrIntHazard;
	    
#ifdef RB_PRINT_INCR_INFO
	    Rprintf("Go Back: %e\n", dIncr);
#endif
#ifdef RB_SAVE_INCR_INFO
	    iTotGoBackIncr++;
#endif
	    
	    // Invalidate results
	    for(iPlacePtr = 0; iPlacePtr < iFastPlaces; iPlacePtr++) {
	      iPlace = piFastPlace[iPlacePtr];
	      pdCrntMarking[iPlace] = pdBakCrntMarking[iPlace];
	    }
	    dAcumHazard = dPrevAcumHazard;
	    for(iTransitionPtr = 0; iTransitionPtr < piHazardsToModxRowTot[iWhichSlowFromFastTransitions]; iTransitionPtr++) {
	      iTransition = piHazardsToModxRow[iWhichSlowFromFastTransitions + (iTransitions + 2) * iTransitionPtr];
	      pdHazard[iTransition] = pdBakHazard[iTransition];
	    }
	    iStartRK = 1;
	    continue;
	  }

	  // Update clock according to the last accepted increment
	  dTime += dIncr;

	  // Check if current state needs to be saved
	  if (dTime == dTarget) {
	    ++iStep;
	    // Update the state for the fixed incremented time.
	    for(iPlace = 0; iPlace < iPlaces; iPlace++) {
	      ppdMarking[iPlace][iStep] = pdCrntMarking[iPlace];
	    }
	    for(iTransition = 0; iTransition < iTransitions; iTransition++) {
	      ppdHazard[iTransition][iStep] = pdHazard[iTransition];
	    }
	    if (iStep == iSectionSteps - 1)
	      goto EXIT_LOOP;
	    
	    dTarget += dDelta;
	    
	    // Force check if user interrupted
	    iInterruptCnt = 1;
	  }
	  if (! --iInterruptCnt) {
	    // Allow user interruption
	    R_CheckUserInterrupt();
	    iInterruptCnt = 100000;
	  }
	  // Set new increment for next integration step
	  dIncr = .8 * dIncr * R_pow(dTau/dInfNormError,err_pow);
	} while (iContinue);
#ifdef RB_PRINT_INCR_INFO
	Rprintf("Find Transition\n", dIncr);
	Rprintf("Time: %f\n", dTime);
#endif
	
	while (!--iTillResort) {
	  quicksort(piOrderedTransition, piTotTransitions, 0, iSlowTransitions-1);
	  switch (iTotResort++) {
	  case 0:
	    iTillResort = 10000;
	    break;
	  case 1:
	    iTillResort = 100000;
	    break;
	  default:
	    iTillResort = 1000000;
	  }
	}
	double dPartialAcumHazard = 0;
	// Find out which transition happened
	double dRnd = runif(0, dAcumHazard);
	iUsedRandomNumbers++;
	for(iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
	  iTransition = piOrderedTransition[iTransitionPtr];
	  if (dRnd < (dPartialAcumHazard += pdHazard[iTransition])) {
	    piTotTransitions[iLastTransition = iTransition]++;
	    for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
	      iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
	      
	      // Update the state
	      if ((pdCrntMarking[iPlace] += piS[iTransition + iTransitions * iPlace]) < 0)
		pdCrntMarking[iPlace] = 0;
	      pdBakCrntMarking[iPlace] = pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace];
	    }
	    break;
	  }
	}
	++iTotTransitions;
      } while (TRUE);
    EXIT_LOOP:;
      Rprintf(".");
    } while (iSectionSteps < iTotalSteps);
    iTotalUsedRandomNumbers += iUsedRandomNumbers;
    Rprintf("\t%d\t%d\t%d\t%e\t%d\t%d", iTotTransitions, iUsedRandomNumbers, iTotalUsedRandomNumbers, dAbsoluteMaxError, iAcceptedIncr, iRejectedIncr);
    
#ifdef RB_SAVE_INCR_INFO
    double dMeanAccpIncr = dSumAccpIncr/iTotAccpIncr;
    double dSdAccpIncr = sqrt(dSumSqAccpIncr/iTotAccpIncr - dMeanAccpIncr*dMeanAccpIncr); 
    Rprintf("\t%d\t%d\t%d\t%e\t%e", iTotRejIncr, iTotGoBackIncr, iTotAccpIncr, dMeanAccpIncr, dSdAccpIncr);
#endif

#ifdef RB_SUBTIME
    c1 = clock();
    Rprintf ("\t To go: ");
    PrintfTime((double) (c1 - c0)/CLOCKS_PER_SEC/(iRun+1)*(iRuns-iRun-1));
#endif
    Rprintf("\n");

    SEXP sexpTotTransitions;
    PROTECT(sexpTotTransitions = allocVector(INTSXP, 1));
    INTEGER(sexpTotTransitions)[0] = iTotTransitions;
    
    SEXP sexpUsedRandomNumbers;
    PROTECT(sexpUsedRandomNumbers = allocVector(INTSXP, 1));
    INTEGER(sexpUsedRandomNumbers)[0] = iUsedRandomNumbers;
    
    SEXP sexpThisRun;
#ifdef RB_SAVE_INCR_INFO
    if (iRun >= 10)
      PROTECT(sexpThisRun = allocVector(VECSXP, 5));
    else
      PROTECT(sexpThisRun = allocVector(VECSXP, 10));
#else
    PROTECT(sexpThisRun = allocVector(VECSXP, 5));
#endif
    
    SET_VECTOR_ELT(sexpThisRun, 0, sexpMarking);
    UNPROTECT_PTR(sexpMarking);
    SET_VECTOR_ELT(sexpThisRun, 1, sexpHazard);
    UNPROTECT_PTR(sexpHazard);
    SET_VECTOR_ELT(sexpThisRun, 2, sexpTotXTransition);
    UNPROTECT_PTR(sexpTotXTransition);
    SET_VECTOR_ELT(sexpThisRun, 3, sexpTotTransitions);
    UNPROTECT_PTR(sexpTotTransitions);
    SET_VECTOR_ELT(sexpThisRun, 4, sexpUsedRandomNumbers);
    UNPROTECT_PTR(sexpUsedRandomNumbers);
#ifdef RB_SAVE_INCR_INFO
    if (iRun < 10) {
      SEXP sexpTmp;

      PROTECT(sexpTmp = allocVector(REALSXP, iTotIncrTime));
      pdTmp = REAL(sexpTmp);
      int i;
      for (i = 0; i < iTotIncrTime; i++)
	pdTmp[i] = pdIncr[i];
      SET_VECTOR_ELT(sexpThisRun, 5, sexpTmp);
      UNPROTECT_PTR(sexpTmp);
      
      PROTECT(sexpTmp = allocVector(REALSXP, iTotIncrTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIncrTime; i++)
	pdTmp[i] = pdIncrTime[i];
      SET_VECTOR_ELT(sexpThisRun, 6, sexpTmp);
      UNPROTECT_PTR(sexpTmp);
      
      PROTECT(sexpTmp = allocVector(REALSXP, iTotIntHazardTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIntHazardTime; i++)
	pdTmp[i] = pdAcumHazard[i];
      SET_VECTOR_ELT(sexpThisRun, 7, sexpTmp);
      UNPROTECT_PTR(sexpTmp);
      
      PROTECT(sexpTmp = allocVector(REALSXP, iTotIntHazardTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIntHazardTime; i++)
	pdTmp[i] = pdIntHazard[i];
      SET_VECTOR_ELT(sexpThisRun, 8, sexpTmp);
      UNPROTECT_PTR(sexpTmp);
      
      PROTECT(sexpTmp = allocVector(REALSXP, iTotIntHazardTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIntHazardTime; i++)
	pdTmp[i] = pdIntHazardTime[i];
      SET_VECTOR_ELT(sexpThisRun, 9, sexpTmp);
      UNPROTECT_PTR(sexpTmp);
    }
#endif
    
    SEXP sexpNames;
#ifdef RB_SAVE_INCR_INFO
    if (iRun >= 10)
      PROTECT(sexpNames = allocVector(VECSXP, 5));
    else
      PROTECT(sexpNames = allocVector(VECSXP, 10));
#else
    PROTECT(sexpNames = allocVector(VECSXP, 5));
#endif
    SET_VECTOR_ELT(sexpNames, 0, mkChar("M"));
    SET_VECTOR_ELT(sexpNames, 1, mkChar("hazard"));
    SET_VECTOR_ELT(sexpNames, 2, mkChar("transitions"));
    SET_VECTOR_ELT(sexpNames, 3, mkChar("tot.transitions"));
    SET_VECTOR_ELT(sexpNames, 4, mkChar("tot.rnd"));
#ifdef RB_SAVE_INCR_INFO
    if (iRun < 10) {
      SET_VECTOR_ELT(sexpNames, 5, mkChar("incr"));
      SET_VECTOR_ELT(sexpNames, 6, mkChar("incr.time"));
      SET_VECTOR_ELT(sexpNames, 7, mkChar("tot.hazard"));
      SET_VECTOR_ELT(sexpNames, 8, mkChar("int.tot.hazard"));
      SET_VECTOR_ELT(sexpNames, 9, mkChar("int.tot.hazard.time"));
    }
#endif
    setAttrib(sexpThisRun, R_NamesSymbol, sexpNames);
    UNPROTECT_PTR(sexpNames);

    SET_VECTOR_ELT(sexpRun, iRun, sexpThisRun);
    UNPROTECT_PTR(sexpThisRun);
  }
  PutRNGstate();

  SEXP sexpAns;
  PROTECT(sexpAns = allocVector(VECSXP, 4));
  SET_VECTOR_ELT(sexpAns, 0, place);
  SET_VECTOR_ELT(sexpAns, 1, transition);
  SET_VECTOR_ELT(sexpAns, 2, sexpD_time);
  UNPROTECT_PTR(sexpD_time);
  SET_VECTOR_ELT(sexpAns, 3, sexpRun);
  UNPROTECT_PTR(sexpRun);

  SEXP sexpNames;
  PROTECT(sexpNames = allocVector(VECSXP, 4));
  SET_VECTOR_ELT(sexpNames, 0, mkChar("place"));
  SET_VECTOR_ELT(sexpNames, 1, mkChar("transition"));
  SET_VECTOR_ELT(sexpNames, 2, mkChar("dt"));
  SET_VECTOR_ELT(sexpNames, 3, mkChar("run"));
  setAttrib(sexpAns, R_NamesSymbol, sexpNames);
  UNPROTECT_PTR(sexpNames);

#ifdef RB_TIME
  c1 = clock();
  double dCpuTime = (double) (c1 - c0)/CLOCKS_PER_SEC;
  Rprintf ("Elapsed CPU time: ");
  PrintfTime(dCpuTime);
  Rprintf ("\t(%fs)\n", dCpuTime);
#endif

  if (sexpRManage_time)
    UNPROTECT_PTR(sexpRManage_time);
  UNPROTECT_PTR(sexpFunction);
  UNPROTECT_PTR(sexpMarkingRowNames);
  UNPROTECT_PTR(sexpTmpCrntMarking);
  UNPROTECT_PTR(sexpCrntMarking);
  UNPROTECT_PTR(sexpAns);

  return(sexpAns);
}
