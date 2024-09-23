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

#define PRODUCTS_MAXIMUM_AMOUNT 100000000
#define HAZARD_VALUE_TO_IGNORE 1e-10

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


SEXP GillespieOptimDirect(SEXP pre, SEXP post, SEXP h, SEXP M, SEXP T, SEXP delta,
			  SEXP runs, SEXP place, SEXP transition, SEXP rho)
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
    iTransition2, iTransitionPtr2;

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

  // Position of non zero cells in pre per transition
  int *piPreNZxRow = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));

  // Totals of non zero cells in pre per transition
  int *piPreNZxRowTot = (int *) R_alloc(iTransitions, sizeof(int));

  // Position of non zero cells in S per transition
  int *piSNZxRow = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));

  // Totals of non zero cells in S per transition
  int *piSNZxRowTot = (int *) R_alloc(iTransitions, sizeof(int));

  int *piOrderedTransition = (int *) R_alloc(iTransitions, sizeof(int));

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
  }

  // Position of non zero cells in pre per place
  int *piPreNZxCol = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));

  // Totals of non zero cells in pre per place
  int *piPreNZxColTot = (int *) R_alloc(iPlaces, sizeof(int));

  for (iPlace = 0; iPlace < iPlaces; iPlace++) {
    int iPreNZxCol_row = 0;
    for (iTransition = 0; iTransition < iTransitions; iTransition++) {
      if (piPre[iTransition + iTransitions * iPlace]) {
	piPreNZxCol[iPreNZxCol_row++ + iTransitions * iPlace] = iTransition;
      }
    }
    piPreNZxColTot[iPlace] = iPreNZxCol_row;
  }

  // Hazards that need to be recalculated if a given transition has happened
  int *piHazardsToModxRow = (int *) R_alloc((iTransitions + 1) * iTransitions, sizeof(int));

  // Totals of hazards to recalculate for each transition that has happened
  int *piHazardsToModxRowTot = (int *) R_alloc(iTransitions + 1, sizeof(int));
  
  for(iTransition = 0; iTransition < iTransitions; iTransition++) {
    int iHazardToCompTot = 0;
    for(iPlace = 0; iPlace < iPlaces; iPlace++) {
      if (piS[iTransition + iTransitions * iPlace]) {
	// Identify the transitions that need the hazards recalculated
	for(iTransitionPtr2 = 0; iTransitionPtr2 < piPreNZxColTot[iPlace]; iTransitionPtr2++) {
	  iTransition2 = piPreNZxCol[iTransitionPtr2 + iTransitions * iPlace];
	  int iAddThis = TRUE;
	  for (k = 0; k < iHazardToCompTot; k++) {
	    if(piHazardsToModxRow[iTransition + (iTransitions + 1) * k] == iTransition2) {
	      iAddThis = FALSE;
	      break;
	    }
	  }	    
	  if (iAddThis)
	    piHazardsToModxRow[iTransition + (iTransitions + 1) * iHazardToCompTot++] = iTransition2;
	}
      }
    }
    piHazardsToModxRowTot[iTransition] = iHazardToCompTot;
  }
  // For the initial calculation of all hazards...
  for(iTransition = 0; iTransition < iTransitions; iTransition++) {
    piHazardsToModxRow[iTransitions + (iTransitions + 1) * iTransition] = iTransition;
  }
  piHazardsToModxRowTot[iTransitions] = iTransitions;

  SEXP sexpCrntMarking;
  PROTECT(sexpCrntMarking = allocVector(REALSXP, iPlaces));
  double *pdCrntMarking = REAL(sexpCrntMarking);

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
  } else  if (isFunction(T)) {
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

  int iRun, iRuns = *INTEGER(runs);

  // Hazard vector
  double *pdHazard = (double *) R_alloc(iTransitions, sizeof(double));

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

  GetRNGstate();
  for (iRun = 0; iRun < iRuns; iRun++) {

    int iUsedRandomNumbers = 0;
    Rprintf("%d ", iRun+1);

    // Totals for kind of transition vector
    SEXP sexpTotXTransition;
    PROTECT(sexpTotXTransition = allocVector(INTSXP, iTransitions));
    int *piTotTransitions = INTEGER(sexpTotXTransition);
  
    for(iTransition = 0; iTransition < iTransitions; iTransition++) {
      piTotTransitions[iTransition] = 0;

      piOrderedTransition[iTransition] = iTransition;
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

      pdCrntMarking[iPlace] = pdTmp[iPlace];
    }

    double dAcumHazard = 0;
    for(iTransition = 0; iTransition < iTransitions; iTransition++) {
      pdHazard[iTransition] = 0;
    }
      
    double dTime = 0, dTarget = 0;
    int iTotTransitions = 0;

    int iStep = 0;
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
	ppdMarking[iPlace][iStep] = pdCrntMarking[iPlace];
      }

      dTime = dTarget;
      dTarget += dDelta;
      
      // For the calculation of all hazards...
      int iLastTransition = iTransitions;

      do {    
	// Get hazards only for the transitions associated with
	// places whose quantities changed in the last step.
	for(iTransitionPtr = 0; iTransitionPtr < piHazardsToModxRowTot[iLastTransition]; iTransitionPtr++) {
	  iTransition = piHazardsToModxRow[iLastTransition + (iTransitions + 1) * iTransitionPtr];
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
	  dAcumHazard += dNewHazard - pdHazard[iTransition];
	  pdHazard[iTransition] = dNewHazard;
	}
	
	// Get Time to transition
	if (dAcumHazard > 0) {
	  dTime += exp_rand() / dAcumHazard;
	  //dTime -= log(unif_rand()) / dAcumHazard;
	  iUsedRandomNumbers++;
	} else
	  dTime = dT + 1;	
	
	while (dTime >= dTarget) {
	  ++iStep;
	  // Update the state for the fixed incremented time.
	  for(iPlace = 0; iPlace < iPlaces; iPlace++)
	    ppdMarking[iPlace][iStep] = pdCrntMarking[iPlace];
	  if (iStep == iSectionSteps - 1)
	    goto EXIT_LOOP;

	  dTarget += dDelta;
	}

	while (!--iTillResort) {
	  // Allow user interruption
	  R_CheckUserInterrupt();

	  quicksort(piOrderedTransition, piTotTransitions, 0, iTransitions-1);
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
	for(iTransitionPtr = 0; iTransitionPtr < iTransitions; iTransitionPtr++) {
	  iTransition = piOrderedTransition[iTransitionPtr];
	  if (dRnd < (dPartialAcumHazard += pdHazard[iTransition])) {
	    piTotTransitions[iLastTransition = iTransition]++;
	    for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
	      iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
	      
	      // Update the state
	      pdCrntMarking[iPlace] += piS[iTransition + iTransitions * iPlace];
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
    Rprintf("\t%d\t%d\t%d", iTotTransitions, iUsedRandomNumbers, iTotalUsedRandomNumbers);
#ifdef RB_SUBTIME
    c1 = clock();
    Rprintf ("\t To go: ");
    PrintfTime((double) (c1 - c0)/CLOCKS_PER_SEC/(iRun+1)*(iRuns-iRun-1));
#endif
    Rprintf ("\n");
  
    /*
    for(iTransition = 0; iTransition < iTransitions; iTransition++) {
      Rprintf("%d\t%d\n", piOrderedTransition[iTransition], piTotTransitions[piOrderedTransition[iTransition]]);
    }
    */

    SEXP sexpTotTransitions;
    PROTECT(sexpTotTransitions = allocVector(INTSXP, 1));
    INTEGER(sexpTotTransitions)[0] = iTotTransitions;

    SEXP sexpThisRun;
    PROTECT(sexpThisRun = allocVector(VECSXP, 3));

    SET_VECTOR_ELT(sexpThisRun, 0, sexpMarking);
    UNPROTECT_PTR(sexpMarking);
    SET_VECTOR_ELT(sexpThisRun, 1, sexpTotXTransition);
    UNPROTECT_PTR(sexpTotXTransition);
    SET_VECTOR_ELT(sexpThisRun, 2, sexpTotTransitions);
    UNPROTECT_PTR(sexpTotTransitions);

    SEXP sexpNames;
    PROTECT(sexpNames = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(sexpNames, 0, mkChar("M"));
    SET_VECTOR_ELT(sexpNames, 1, mkChar("transitions"));
    SET_VECTOR_ELT(sexpNames, 2, mkChar("tot.transitions"));
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
  UNPROTECT_PTR(sexpCrntMarking);
  UNPROTECT_PTR(sexpAns);
  return(sexpAns);
}
