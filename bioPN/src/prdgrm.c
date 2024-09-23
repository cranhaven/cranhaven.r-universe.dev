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

#include <assert.h>

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#define NINCR 10000


SEXP prdgrm(SEXP X)
{
  SEXP sexpAns;
  PROTECT(sexpAns = allocVector(VECSXP, 2));
  SEXP T;
  SET_VECTOR_ELT(sexpAns, 0, T = allocVector(REALSXP, NINCR - 1));
  double *pdT = (double *)REAL(T);
  SEXP P;
  SET_VECTOR_ELT(sexpAns, 1, P = allocVector(REALSXP, NINCR - 1));
  double *pdP = (double *)REAL(P);

  SEXP sexpNames;
  PROTECT(sexpNames = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(sexpNames, 0, mkChar("T"));
  SET_VECTOR_ELT(sexpNames, 1, mkChar("P"));
  setAttrib(sexpAns, R_NamesSymbol, sexpNames);
  UNPROTECT_PTR(sexpNames);

  //  double pi = acos(-1);

  int N = length(X);
  double *pdX = (double *)REAL(X);

  //  double *pdXc = (double *) R_alloc(N , sizeof(double));
  //  double *pdXs = (double *) R_alloc(N , sizeof(double));
  
  //  double dDT = double(N) / (2. * NINCR);

  double dDT = (double)N / NINCR;
  double dT = 0;
  for (int i = 0; i < NINCR - 1; i++) {
    dT += dDT;
    pdT[i] = dT;

    double dAcumc = 0, dAcumv = 0;
    for (int t = 1; t <= N; t++) {
      dAcumc += cos(2*PI*t/dT)*pdX[t-1];
      dAcumv += sin(2*PI*t/dT)*pdX[t-1];
    }
    //    pdP[i] = dAcumc*dAcumc/N + dAcumv*dAcumv/N;
    pdP[i] = (dAcumc*dAcumc + dAcumv*dAcumv) / N;
  }

  UNPROTECT_PTR(sexpAns);
  return(sexpAns);
}
