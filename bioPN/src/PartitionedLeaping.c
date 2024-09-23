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
#define RB_SAVE_INCR_INFO

#define INCR_TO_SAVE 200000
#define ZERO 1e-20

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

#define MIN_INCR 1e-2


SEXP PartitionedLeaping(SEXP pre, SEXP post, SEXP h, SEXP M, SEXP T, SEXP delta,
			SEXP runs, SEXP place, SEXP transition, SEXP ect, SEXP rho)
{
  int k;
  double dTmp;

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

  int *piSSATransition = (int *) R_alloc(iTransitions, sizeof(int));
  int *piTauLeapTransition = (int *) R_alloc(iTransitions, sizeof(int));
  int *piCLETransition = (int *) R_alloc(iTransitions, sizeof(int));
  int *piDetermTransition = (int *) R_alloc(iTransitions, sizeof(int));
  int iSSATransitions, iTauLeapTransitions, iCLETransitions, iDetermTransitions;

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


  int *piOrderedTransition = (int *) R_alloc(iTransitions, sizeof(int *));

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


  int *piSNZxCol = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  int *piSNZxColTot = (int *) R_alloc(iPlaces, sizeof(int));
  int *piPreNZxCol = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  int *piPreNZxColTot = (int *) R_alloc(iTransitions, sizeof(int));

  for (iPlace = 0; iPlace < iPlaces; iPlace++) {
    int iSNZxCol_row = 0;
    int iPreNZxCol_row = 0;
    for (iTransition = 0; iTransition < iTransitions; iTransition++) {
      if(piS[iTransition + iTransitions * iPlace]) {
	piSNZxCol[iSNZxCol_row++ + iTransitions * iPlace] = iTransition;
      }
      if(piPre[iTransition + iTransitions * iPlace]) {
	piPreNZxCol[iPreNZxCol_row++ + iTransitions * iPlace] = iTransition;
      }
    }
    piSNZxColTot[iPlace] = iSNZxCol_row;
    piPreNZxColTot[iPlace] = iPreNZxCol_row;
  }

  double *pdG = (double *) R_alloc(iPlaces, sizeof(double));

  for (iPlace = 0; iPlace < iPlaces; iPlace++) {
    int iHOR = 0;
    
    pdG[iPlace] = 0;
    for(iTransitionPtr = 0; iTransitionPtr < piPreNZxColTot[iPlace]; iTransitionPtr++) {
      iTransition = piPreNZxCol[iTransitionPtr + iTransitions * iPlace];
      
      int iThisHOR = 0, iThisHORPlace = 0;
      for(iPlacePtr2 = 0; iPlacePtr2 < piPreNZxRowTot[iTransition]; iPlacePtr2++) {
	iPlace2 = piPreNZxRow[iTransition + iTransitions * iPlacePtr2];
	iThisHOR += piPre[iTransition + iTransitions * iPlace2];
	
	if (iPlace2 == iPlace)
	  iThisHORPlace = piPre[iTransition + iTransitions * iPlace2];
      }
      if (iThisHOR >= iHOR) {
	double dThisG = 0;
	switch (iThisHOR) {
	case 0:
	  // dThisG = 0;
	  break;
	case 1:
	  dThisG = 1;
	  break;
	case 2:
	  if (iThisHORPlace == 1)
	    dThisG = 2;
	  else if (iThisHORPlace == 2)
	    dThisG = 3;
	  else
	    error("G: case not considered\n");
	  break;
	case 3:
	  if (iThisHORPlace == 1)
	    dThisG = 3;
	  else if (iThisHORPlace == 2)
	    dThisG = 9./2.;
	  else if (iThisHORPlace == 3)
	    dThisG = 11./2.;
	  else
	    error("G: case not considered\n");
	  break;
	default:
	  error("G: case not considered\n");
	}
	iHOR = iThisHOR;
	if (dThisG > pdG[iPlace])
	  pdG[iPlace] = dThisG;
      }
    }
  }

  int *piSlowPlace = (int *) R_alloc(iPlaces, sizeof(int));
  int *piFastPlace = (int *) R_alloc(iPlaces, sizeof(int));
  int iSlowPlaces = 0, iFastPlaces = 0;

  // Position of non zero cells in S per place
  int *piFastSNZxCol = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  // Totals of non zero cells in S per place
  int *piFastSNZxColTot = (int *) R_alloc(iPlaces, sizeof(int));

  // Position of non zero cells in pre per transition
  int *piSlowPreNZxCol = (int *) R_alloc(iTransitions * iPlaces, sizeof(int));
  // Totals of non zero cells in pre per transition
  int *piSlowPreNZxColTot = (int *) R_alloc(iTransitions, sizeof(int));


  for (iPlace = 0; iPlace < iPlaces; iPlace++) {
    int iFastSNZxCol_row = 0;
    int iFastPlace = FALSE;
    for (iTransitionPtr = 0; iTransitionPtr < iFastTransitions; iTransitionPtr++) {
      iTransition = piFastTransition[iTransitionPtr];
	
      if(piS[iTransition + iTransitions * iPlace]) {
	iFastPlace = TRUE;
	piFastSNZxCol[iFastSNZxCol_row++ + iTransitions * iPlace] = iTransition;
      }
    }
    piFastSNZxColTot[iPlace] = iFastSNZxCol_row;
    if (iFastPlace)
      piFastPlace[iFastPlaces++] = iPlace;

    int iSlowPreNZxCol_row = 0;
    int iSlowPlace = FALSE;
    for (iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
      iTransition = piSlowTransition[iTransitionPtr];
	
      if(piPre[iTransition + iTransitions * iPlace]) {
	iSlowPlace = TRUE;
	piSlowPreNZxCol[iSlowPreNZxCol_row++ + iTransitions * iPlace] = iTransition;
      }
    }
    piSlowPreNZxColTot[iPlace] = iSlowPreNZxCol_row;
    if (iSlowPlace)
      piSlowPlace[iSlowPlaces++] = iPlace;
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
  // For the initial calculation of all hazards...
  for(iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
    iTransition = piSlowTransition[iTransitionPtr];
    piHazardsToModxRow[iTransitions + (iTransitions + 2) * iTransitionPtr] = iTransition;
  }
  piHazardsToModxRowTot[iTransitions] = iSlowTransitions;

  SEXP sexpTmpCrntMarking;
  PROTECT(sexpTmpCrntMarking = allocVector(REALSXP, iPlaces));
  double *pdTmpCrntMarking = REAL(sexpTmpCrntMarking);

  SEXP sexpCrntMarking;
  PROTECT(sexpCrntMarking = allocVector(REALSXP, iPlaces));
  double *pdCrntMarking = REAL(sexpCrntMarking);
  double *pdBakCrntMarking = (double *) R_alloc(iPlaces, sizeof(double));

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
  dTmp = 0;
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

#ifdef RB_SAVE_INCR_INFO
  double *pdIncr = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdIncrTime = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdAcumHazard = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdIntHazard = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
  double *pdIntHazardTime = (double *) R_alloc(INCR_TO_SAVE, sizeof(double));
#endif

  double *pdSSARescaling = (double *) R_alloc(iTransitions, sizeof(double));
  double *pdSSATau = (double *) R_alloc(iTransitions, sizeof(double));
  double dEpsilon = 0.03;
  double dApproxEqualOne = 3;
  double dGreaterGreaterOne = 100;

  GetRNGstate();
  for (iRun = 0; iRun < iRuns; iRun++) {

#ifdef RB_SAVE_INCR_INFO
    int iTotIntHazardTime = 0, iTotIncrTime = 0;
#endif
 
    int iUsedRandomNumbers = 0;
    Rprintf("%d ", iRun+1);

    // Totals for kind of transition vector
    SEXP sexpTotXTransition;
    PROTECT(sexpTotXTransition = allocVector(INTSXP, iTransitions));
    int *piTotTransitions = INTEGER(sexpTotXTransition);

    for(iTransition = 0; iTransition < iTransitions; iTransition++) {
      piTotTransitions[iTransition] = 0;
      pdSSARescaling[iTransition] = -1;
    }
    for(iTransitionPtr = 0; iTransitionPtr < iSlowTransitions; iTransitionPtr++) {
      piOrderedTransition[iTransitionPtr] = piSlowTransition[iTransitionPtr];
    }
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
    
    int iStep = 0;
    int iInterruptCnt = 10000000;
    double dNewHazard = 0;
    do {
      if (iStep) {
	--iStep;
	for(iPlace = 0; iPlace < iPlaces; iPlace++) {
	  pdCrntMarking[iPlace] = ppdMarking[iPlace][iStep];
	}
      }

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
	pdBakCrntMarking[iPlace] = pdTmpCrntMarking[iPlace] = pdCrntMarking[iPlace];
      }

      dTime = dTarget;

      for(iTransition = 0; iTransition < iTransitions; iTransition++) {
	pdHazard[iTransition] = 0;
      }

      do {
	// Get hazards for all transitions.
	for(iTransition = 0; iTransition < iTransitions; iTransition++) {
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
	    dNewHazard = ((double(*)(double *)) pCFunction[iTransition])(pdCrntMarking);
	    break;
	  default:
	    // case HZ_RFUNCTION:
	    defineVar(install("y"), sexpCrntMarking, rho);
	    dNewHazard = REAL(eval(VECTOR_ELT(sexpFunction, iTransition), rho))[0];
	    break;
	  }
	  // dAcumHazard += dNewHazard - pdHazard[iTransition];
	  pdHazard[iTransition] = dNewHazard;
	}
	
	for(iPlace = 0; iPlace < iPlaces; iPlace++)  // Save Marking
	  pdBakCrntMarking[iPlace] = pdCrntMarking[iPlace];
	
	double dTau = DBL_MAX;
	
	// Initial value of Tau
	for (iPlace = 0; iPlace < iPlaces; iPlace++) {
	  double dMHat = 0, dSigmaHatSq = 0;
	  for(iTransitionPtr = 0; iTransitionPtr < piSNZxColTot[iPlace]; iTransitionPtr++) {
	    iTransition = piSNZxCol[iTransitionPtr + iTransitions * iPlace];
	    
	    dMHat += (dTmp = piS[iTransition + iTransitions * iPlace] * pdHazard[iTransition]);
	    dSigmaHatSq += dTmp * piS[iTransition + iTransitions * iPlace];
	  }
	  double dE;
	  if ((dE = dEpsilon * pdCrntMarking[iPlace] / pdG[iPlace]) < 1)
	    dE = 1;
	  double dTLeap;
	  if ((dTLeap = dE/fabs(dMHat)) > (dTmp = dE*dE/dSigmaHatSq))
	    dTLeap = dTmp;
	  
	  if (dTLeap < dTau)
	    dTau = dTLeap;
	}
	
	//double dLogRandom = -log(unif_rand());
	//double dSSATau = DBL_MAX;
	int iNextSSATransition;

	int iLoop = TRUE;
	while (iLoop) {
	  // Classify transitions
	  iSSATransitions = iTauLeapTransitions = iCLETransitions = iDetermTransitions = 0;
	  // dAcumHazard = 0;

	  iNextSSATransition = -1;
	  double dSSATau = DBL_MAX;

	  for(iTransition = 0; iTransition < iTransitions; iTransition++) {

	    if (pdHazard[iTransition] < ZERO)
	      continue;
	    if ((dTmp = pdHazard[iTransition] * dTau) < dApproxEqualOne) {
	      piSSATransition[iSSATransitions++] = iTransition;
	      // dAcumHazard += pdHazard[iTransition];
	      if (pdSSARescaling[iTransition] > 0) // Rescale
		pdSSATau[iTransition] = pdSSARescaling[iTransition] / pdHazard[iTransition];
	      else { // Need to generate random number
		pdSSATau[iTransition] = -log(unif_rand()) / pdHazard[iTransition];
		pdSSARescaling[iTransition] = pdHazard[iTransition] * pdSSATau[iTransition];
	      }
	      if (pdSSATau[iTransition] < dSSATau) {
		iNextSSATransition = iTransition;
		dSSATau = pdSSATau[iTransition];
	      }
	    } else if (dTmp < dGreaterGreaterOne) {
	      piTauLeapTransition[iTauLeapTransitions++] = iTransition;
	    } else if (sqrt(dTmp) < dGreaterGreaterOne) {
	      piCLETransition[iCLETransitions++] = iTransition;
	    } else {
	      piDetermTransition[iDetermTransitions++] = iTransition;
	    }
	  }
	  if (iNextSSATransition >= 0) {
	    // dSSATau = dLogRandom / dAcumHazard;
	    
	    if (iSSATransitions && !(iTauLeapTransitions + iCLETransitions + iDetermTransitions)) // If all (possible) transitions are SSA
	      dTau = dSSATau;
	    else if (dSSATau < dTau) {	// If SSA fired before dTau
	      dTau = dSSATau;
	      continue;   // Go back to see if anything changed	     
	    }
	  }
	  if (dSSATau == dTau) { // If an SSA transition fired
	    iTransition = iNextSSATransition;
	    for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
	      iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
	      // Update the state
	      pdCrntMarking[iPlace] += piS[iTransition + iTransitions * iPlace];
	    }
	    /*
	    double dPartialAcumHazard = 0;
	    // Find out which transition happened
	    double dRnd = runif(0, dAcumHazard);
	    for(iTransitionPtr = 0; iTransitionPtr < iSSATransitions; iTransitionPtr++) {
	      iTransition = piSSATransition[iTransitionPtr];
	      if (dRnd < (dPartialAcumHazard += pdHazard[iTransition])) {
		for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
		  iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
		  // Update the state
		  pdCrntMarking[iPlace] += piS[iTransition + iTransitions * iPlace];
		}
		break;
	      }
	    }
	    */
	  }

	  int iTransitionFires;
	  
	  // Account for Tau Leaping reactions
	  for(iTransitionPtr = 0; iTransitionPtr < iTauLeapTransitions; iTransitionPtr++) {
	    iTransition = piTauLeapTransition[iTransitionPtr];
	    if ((iTransitionFires = rpois(pdHazard[iTransition] * dTau))) {
	      for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
		iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
		// Update the state
		pdCrntMarking[iPlace] += iTransitionFires * piS[iTransition + iTransitions * iPlace];
	      }
	    }
	  }
	  
	  // Account for CLE reactions
	  for(iTransitionPtr = 0; iTransitionPtr < iTauLeapTransitions; iTransitionPtr++) {
	    iTransition = piTauLeapTransition[iTransitionPtr];
	    if ((iTransitionFires = fround(pdHazard[iTransition] * dTau + sqrt(pdHazard[iTransition] * dTau) * norm_rand(),0))) {
	      for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
		iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
		// Update the state
		pdCrntMarking[iPlace] += iTransitionFires * piS[iTransition + iTransitions * iPlace];
	      }
	    }
	  }
	  
	  // Account for deterministic reactions
	  for(iTransitionPtr = 0; iTransitionPtr < iDetermTransitions; iTransitionPtr++) {
	    iTransition = piDetermTransition[iTransitionPtr];
	    if ((iTransitionFires = fround(pdHazard[iTransition] * dTau,0))) {
	      for(iPlacePtr = 0; iPlacePtr < piSNZxRowTot[iTransition]; iPlacePtr++) {
		iPlace = piSNZxRow[iTransition + iTransitions * iPlacePtr];
		// Update the state
		pdCrntMarking[iPlace] += iTransitionFires * piS[iTransition + iTransitions * iPlace];
	      }
	    }
	  }
	  
	  // Check no negative places have been generated
	  for(iPlace = 0; iPlace < iPlaces; iPlace++)  // Save Marking
	    if (pdCrntMarking[iPlace] < 0)
	      break;
	  
	  if (iPlace < iPlaces) { // At least one Place is negative. Rollback and reduce Tau by half
	    dTau *= .5;
	    for(iPlace = 0; iPlace < iPlaces; iPlace++)
	      pdCrntMarking[iPlace] = pdBakCrntMarking[iPlace];
	  } else // Everything is OK. Leave the loop.
	    iLoop = FALSE;
	}	
	// Advance the clock
	dTime += dTau;

	// Rescale SSA transitions that didn't fire
	for(iTransitionPtr = 0; iTransitionPtr < iSSATransitions; iTransitionPtr++) {
	  iTransition = piSSATransition[iTransitionPtr];
	  if (iTransition != iNextSSATransition) {
	    pdSSARescaling[iTransition] = pdHazard[iTransition] * (pdSSATau[iTransition] - dTau);
	  } else {
	    pdSSARescaling[iTransition] = -1;
	  }
	}
	
	while (dTime >= dTarget) {
	  // Update the state for the fixed incremented time.
	  for(iPlace = 0; iPlace < iPlaces; iPlace++) {
	    ppdMarking[iPlace][iStep] = pdBakCrntMarking[iPlace];
	  }
	  if (++iStep >= iSectionSteps)
	    goto EXIT_LOOP;
	  
	  dTarget += dDelta;

	  // Force check if user interrupted
	  iInterruptCnt = 1;
	}
	if (! --iInterruptCnt) {
	  // Allow user interruption
	  R_CheckUserInterrupt();
	  iInterruptCnt = 10000000;
	}
      } while (++iTotTransitions);
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
  
    SEXP sexpTotTransitions;
    PROTECT(sexpTotTransitions = allocVector(INTSXP, 1));
    INTEGER(sexpTotTransitions)[0] = iTotTransitions;
    
    SEXP sexpUsedRandomNumbers;
    PROTECT(sexpUsedRandomNumbers = allocVector(INTSXP, 1));
    INTEGER(sexpUsedRandomNumbers)[0] = iUsedRandomNumbers;
    
    SEXP sexpThisRun;
#ifdef RB_SAVE_INCR_INFO
    if (iRun >= 10)
      PROTECT(sexpThisRun = allocVector(VECSXP, 4));
    else
      PROTECT(sexpThisRun = allocVector(VECSXP, 9));
#else
    PROTECT(sexpThisRun = allocVector(VECSXP, 4));
#endif
    
    SET_VECTOR_ELT(sexpThisRun, 0, sexpMarking);
    UNPROTECT_PTR(sexpMarking);
    SET_VECTOR_ELT(sexpThisRun, 1, sexpTotXTransition);
    UNPROTECT_PTR(sexpTotXTransition);
    SET_VECTOR_ELT(sexpThisRun, 2, sexpTotTransitions);
    UNPROTECT_PTR(sexpTotTransitions);
    SET_VECTOR_ELT(sexpThisRun, 3, sexpUsedRandomNumbers);
    UNPROTECT_PTR(sexpUsedRandomNumbers);
#ifdef RB_SAVE_INCR_INFO
    if (iRun < 10) {
      SEXP sexpTmp;

      PROTECT(sexpTmp = allocVector(REALSXP, iTotIncrTime));
      pdTmp = REAL(sexpTmp);
      int i;
      for (i = 0; i < iTotIncrTime; i++)
	pdTmp[i] = pdIncr[i];
      SET_VECTOR_ELT(sexpThisRun, 4, sexpTmp);
      UNPROTECT_PTR(sexpTmp);

      PROTECT(sexpTmp = allocVector(REALSXP, iTotIncrTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIncrTime; i++)
	pdTmp[i] = pdIncrTime[i];
      SET_VECTOR_ELT(sexpThisRun, 5, sexpTmp);
      UNPROTECT_PTR(sexpTmp);

      PROTECT(sexpTmp = allocVector(REALSXP, iTotIntHazardTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIntHazardTime; i++)
	pdTmp[i] = pdAcumHazard[i];
      SET_VECTOR_ELT(sexpThisRun, 6, sexpTmp);
      UNPROTECT_PTR(sexpTmp);

      PROTECT(sexpTmp = allocVector(REALSXP, iTotIntHazardTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIntHazardTime; i++)
	pdTmp[i] = pdIntHazard[i];
      SET_VECTOR_ELT(sexpThisRun, 7, sexpTmp);
      UNPROTECT_PTR(sexpTmp);

      PROTECT(sexpTmp = allocVector(REALSXP, iTotIntHazardTime));
      pdTmp = REAL(sexpTmp);
      for (i = 0; i < iTotIntHazardTime; i++)
	pdTmp[i] = pdIntHazardTime[i];
      SET_VECTOR_ELT(sexpThisRun, 8, sexpTmp);
      UNPROTECT_PTR(sexpTmp);
    }
#endif

    SEXP sexpNames;
#ifdef RB_SAVE_INCR_INFO
    if (iRun >= 10)
      PROTECT(sexpNames = allocVector(VECSXP, 4));
    else
      PROTECT(sexpNames = allocVector(VECSXP, 9));
#else
    PROTECT(sexpNames = allocVector(VECSXP, 4));
#endif
    SET_VECTOR_ELT(sexpNames, 0, mkChar("M"));
    SET_VECTOR_ELT(sexpNames, 1, mkChar("transitions"));
    SET_VECTOR_ELT(sexpNames, 2, mkChar("tot.transitions"));
    SET_VECTOR_ELT(sexpNames, 3, mkChar("tot.rnd"));
#ifdef RB_SAVE_INCR_INFO
    if (iRun < 10) {
      SET_VECTOR_ELT(sexpNames, 4, mkChar("incr"));
      SET_VECTOR_ELT(sexpNames, 5, mkChar("incr.time"));
      SET_VECTOR_ELT(sexpNames, 6, mkChar("hazard"));
      SET_VECTOR_ELT(sexpNames, 7, mkChar("int.hazard"));
      SET_VECTOR_ELT(sexpNames, 8, mkChar("int.hazard.time"));
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
