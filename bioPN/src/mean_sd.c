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


#define RB_TIME
#define RB_SUBTIME

#ifdef RB_TIME
#include <time.h>
#endif


SEXP mean_sd(SEXP run, SEXP sumPlaces, SEXP saveSingleRunsAmount)
{
  int iTotRuns = length(run);
  int iTotSumPlaces = length(sumPlaces);
  int iTotTimePoints = length(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(run, 0), 0), 0));
  int iSaveSingleRunsAmount = *INTEGER(saveSingleRunsAmount);

  Rprintf("iTotRuns: %d\tiTotSumPlaces: %d\tiTotTimePoints: %d\tiSaveSingleRunsAmount: %d\n", 
	  iTotRuns, iTotSumPlaces, iTotTimePoints, iSaveSingleRunsAmount);

  SEXP sexpTmp;

  SEXP sexpAns;
  PROTECT(sexpAns = allocVector(VECSXP, 3));

  SEXP sexpRunPerSumPlace;
  SET_VECTOR_ELT(sexpAns, 0, sexpRunPerSumPlace = allocVector(VECSXP, iTotSumPlaces));

  SEXP sexpMean;
  SET_VECTOR_ELT(sexpAns, 1, sexpMean = allocVector(VECSXP, iTotSumPlaces));

  SEXP sexpSd;
  SET_VECTOR_ELT(sexpAns, 2, sexpSd = allocVector(VECSXP, iTotSumPlaces));

  SEXP sexpThisSumPlace;
  for (int iThisSumPlace = 0; iThisSumPlace < iTotSumPlaces; iThisSumPlace++) {
    sexpThisSumPlace = VECTOR_ELT(sumPlaces, iThisSumPlace);
    int iLengthThisSumPlace = length(sexpThisSumPlace);
    int *piThisSumPlace = INTEGER(sexpThisSumPlace);
    if (piThisSumPlace[0] <= 0)
      continue;

    SEXP sexpThisRunPerSumPlace;
    SET_VECTOR_ELT(sexpRunPerSumPlace, iThisSumPlace, sexpThisRunPerSumPlace = allocVector(VECSXP, iSaveSingleRunsAmount));
    for (int iThisSavedRun = 0; iThisSavedRun < iSaveSingleRunsAmount; iThisSavedRun++) {
      SET_VECTOR_ELT(sexpThisRunPerSumPlace, iThisSavedRun, allocVector(REALSXP, iTotTimePoints));
    }

    SET_VECTOR_ELT(sexpMean, iThisSumPlace, sexpTmp = allocVector(REALSXP, iTotTimePoints));
    double *pdThisMean = REAL(sexpTmp);
    SET_VECTOR_ELT(sexpSd, iThisSumPlace, sexpTmp = allocVector(REALSXP, iTotTimePoints));
    double *pdThisSd = REAL(sexpTmp);

    for (int iThisTimePoint = 0; iThisTimePoint < iTotTimePoints; iThisTimePoint++) {
      double dSum = 0, dSumOfSq = 0;

      for (int iThisRun = 0; iThisRun < iTotRuns; iThisRun++) {
	SEXP sexpThisRun = VECTOR_ELT(run, iThisRun);
	double dSumPlace = 0;
	for (int iThisSumPlace = 0; iThisSumPlace < iLengthThisSumPlace; iThisSumPlace++) {
	  int iPlace = piThisSumPlace[iThisSumPlace] - 1;
	  dSumPlace += REAL(VECTOR_ELT(VECTOR_ELT(sexpThisRun, 0), iPlace))[iThisTimePoint];
	}
	if (iThisRun < iSaveSingleRunsAmount) {
	  REAL(VECTOR_ELT(sexpThisRunPerSumPlace, iThisRun))[iThisTimePoint] = dSumPlace;
	}
	dSum += dSumPlace;
	dSumOfSq += dSumPlace * dSumPlace;
      }
      pdThisMean[iThisTimePoint] = dSum / (double)iTotRuns;
      if (iTotRuns > 1) {
	pdThisSd[iThisTimePoint] = sqrt((double)iTotRuns/(double)(iTotRuns - 1)*(dSumOfSq/(double)iTotRuns - pdThisMean[iThisTimePoint]*pdThisMean[iThisTimePoint]));
      } else {
	pdThisSd[iThisTimePoint] = 0;
      }
    }
  }
  UNPROTECT_PTR(sexpAns);

  return(sexpAns);
}
