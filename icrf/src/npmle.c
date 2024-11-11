#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP getListElement(SEXP list, const char *str)
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (R_len_t i = 0; i < length(list); i++)
  {
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  return elmt;
}

SEXP EMICM (SEXP A) {
  SEXP statsPackage;
  SEXP e;
  SEXP sGetNamespace = install("getNamespace");
  PROTECT(e = lang2(sGetNamespace, ScalarString(mkChar("icrf"))));

  PROTECT(statsPackage = eval(e, R_GlobalEnv));

  SEXP npmle;
  SEXP a;
  PROTECT(a = lang2(install("EMICM"), A));
  PROTECT(npmle = eval(a, statsPackage));
  UNPROTECT(4);
  return(npmle);

  // SEXP statsPackage;
  // PROTECT(
  //   statsPackage = eval( lang2( install("getNamespace"),
  //                               ScalarString(mkChar("icrf")) ),
  //                               R_GlobalEnv
  //   )
  // );
  //
  // SEXP npmle;
  // PROTECT(npmle = eval(lang2(install("EMICM"), A), statsPackage));
  // UNPROTECT(2);
  // return(npmle);
}


/* NPMLE object to prob object */
void NP2prob (double *intmap, double *pf, int *nNPMLE,
              double *timepoints, int *ntime, double *tau,
              double *pfGrid) {
  /* intmap: 2 x *nNPMLE, pf: 1 x *nNPMLE
   * cdf, slope: 2 x (*nNPMLE or smallest table size containing tau)
   * cdfGrid, pfGrid, timepoints: 1 x *ntime
   */

  int i, j, ii = *nNPMLE, jj = 0;  //ii = column where tau is located.
  double pf2 = 0.0, lambda = 0.0;
  for (i = 0; i <*nNPMLE; ++i) {
    if (*tau <= intmap[2*i + 1]) {
      ii = i;
      if (isfinite(intmap[2*i + 1])) {
        pf2 = pf[i] * (*tau - intmap[2*i])/(intmap[2*i + 1] - intmap[2*i]);
      } else {
        if (1 - pf[i] > 0.0001) {
          lambda = -intmap[2*i] / (log(pf[i]));
          pf2 = pf[i] - exp(- *tau / lambda);
        } else {
          pf2 = 0.0;
        }
      }
      break;
    }
  } // if tau > max(intmap), then ii = *nNPMLE by default.
  // timepoints should be only upto tau except Inf!! (o/w the code needs to be modified to limit the pfGrid)

  double cdf[2 * (ii + 1)], cdfGrid[*ntime];
  double slope[2 * (ii + 1)];

  //updating cdf and slope
  cdf[0] = 0.0;
  cdf[1] = pf[0];
  slope[0] = pf[0]/(intmap[1] - intmap[0]);
  slope[1] = 0.0;
  for (i = 1; i < ii; ++i) {  // loop over intmap only before ii.
    cdf[2*i] = cdf[2*i - 1];
    cdf[2*i + 1] = cdf[2*i] + pf[i];
    slope[2*i] = pf[i] / (intmap[2 * i + 1] - intmap[2 * i]);
    slope[2*i + 1] = 0.0;
  }
  cdf[2*ii] = ii == 0 ? 0.0 : cdf[2*ii - 1];
  cdf[2*ii + 1] = cdf[2*ii] + pf2;
  slope[2*ii] = *tau > intmap[2 * ii]? pf2 / (*tau - intmap[2 * ii]): 0.0;
  slope[2*ii+1] = 0.0;

//Rprintf("intmap[last] = %f\n", intmap[2 * *nNPMLE - 1]);

    if (isfinite(intmap[2 * *nNPMLE - 1]) == 0) {
      // Rprintf("isfinite(intmap[last]) = %d\n", isfinite(intmap[2 * *nNPMLE - 1]));
      if ((1 - pf[*nNPMLE - 1] > 0.0001) &
          (*tau - intmap[2 * *nNPMLE - 2] > 0.0001)) {
        lambda = - intmap[2* *nNPMLE - 2] / (log(pf[*nNPMLE - 1]));
        pf2 = pf[*nNPMLE - 1] - exp(- *tau / lambda);
        slope[2*(*nNPMLE - 1)] =  pf2/(*tau - intmap[2 * *nNPMLE - 2]);
      } else {
        pf2 = 0.0;
        slope[2*(*nNPMLE - 1)] = 0.0;
      }
    }


/*Rprintf("cdf(1st row) = \n");
for (i =0; i < ii + 1; ++i) Rprintf("%f ", cdf[2*i]);
Rprintf("\ncdf(2nd row) = \n");
for (i =0; i < ii + 1; ++i) Rprintf("%f ", cdf[2*i+1]);
Rprintf("\nslope (1st row) = \n");
for (i =0; i < ii + 1; ++i) Rprintf("%f ", slope[2*i]);
Rprintf("\nslope (2nd row) = \n");
for (i =0; i < ii + 1; ++i) Rprintf("%f ", slope[2*i+1]);
Rprintf("\n");*/

  cdfGrid[0] = 0.0;
  pfGrid[0] = 0.0;

//Rprintf("cdfGrid[0] = %4.2f, pfGrid [0] = %4.2f\n", cdfGrid[0], pfGrid[0]);

  for (i = 1; i < *ntime; ++i) {  // loop over timepoints
//Rprintf("i = %d, jj = %d\n", i, jj);
    for (j = jj; j < 2 * (ii+1); ++j) { // search through intmap
      if (timepoints[i] < intmap[j]) {
//        Rprintf("intmap[%d] = %f, timpepoints = %f, cdf[j-1] = %4.2f, slope[j-1] =%4.2f\n",
//                j, intmap[j], timepoints[i], cdf[j-1], slope[j-1]);
        cdfGrid[i] = j == 0? 0.0: cdf[j-1] + slope[j-1] * (timepoints[i] - intmap[j-1]);
        pfGrid[i] = cdfGrid[i] - cdfGrid[i-1];
        jj = j;
        break;
      }
    }
    if (j == 2 *(ii+1)) {
      cdfGrid[i] = cdfGrid[i-1];
      pfGrid[i] = 0.0;
    }

  }

/*Rprintf("\ntimepoints ");
for (i = 0; i < *ntime; ++i) {
  Rprintf("%2.4f ", timepoints[i]);
}
Rprintf("\ncdfGrid ");
for (i = 0; i < *ntime; ++i) {
  Rprintf("%2.3f ", cdfGrid[i]);
}
Rprintf("\npfGrid ");
for (i = 0; i < *ntime; ++i) {
  Rprintf("%2.3f ", pfGrid[i]);
}
Rprintf("\n");*/

  cdfGrid[*ntime -1] = 1.0;
  pfGrid[*ntime-1] = 1.0 - cdfGrid[*ntime-2]; // prob at [tau, Inf]
}


void NPMLE (double *lr, int nLR, double *intmap, double *pf, int *nNPMLE,
            double *timepoints, int *ntime, double *tau, double *pfGrid) {
  int i, isInf = 0; //nObs = nLR

/* Rprintf("inside NPMLE\n lr[1,] = ");
for (i = 0; i < nLR; ++i) Rprintf("%2.2f ",lr[2*i]);
Rprintf("\n lr[2,] = ");
for (i = 0; i < nLR; ++i) Rprintf("%2.2f ",lr[2*i + 1]);
Rprintf("\n"); */
//Rprintf("nLR = %d, nNPMLE = %d\n", nLR, *nNPMLE);
  //if (nLR == 1) {//when the node happens to have only one observation, augment it.
  //  nObs = 2;
  //}
  SEXP lrMat;
  SEXP result;
  // LR matrix in R
  PROTECT(lrMat = allocMatrix(REALSXP, nLR, 2));   // making empty R matrix object
  double *lrMatReal = REAL(lrMat);                      // pointing to the real component of lrMat
  /*if (nLR == 1) {
    for (i = 0; i < nLR; ++i) {
      lrMatReal[i] = lr[0];   // filling in values
      lrMatReal[i + nLR] = lr[1];   // filling in values
    }
  } else {
    for (i = 0; i < nLR; ++i) {
      lrMatReal[i] = lr[2*i];   // filling in values
      lrMatReal[i + nLR] = lr[2*i + 1];   // filling in values
    }
  }*/
  for (i = 0; i < nLR; ++i) {
    lrMatReal[i] = lr[2*i];   // filling in values
    lrMatReal[i + nLR] = lr[2*i + 1];   // filling in values
  }
  for (i = 0; i < nLR; ++i) {
    if (lr[2*i + 1] == 1.0/0.0) {
      isInf = 1;   // filling in values
      break;
    }
  }

  // Running NPMLE
  PROTECT(result = EMICM(lrMat));                  // NPMLE execution

/*
Rprintf("inside NPMLE - lrMat \n lrMat[1,] = ");
for (i = 0; i < nLR; ++i) Rprintf("%2.2f ",lrMatReal[i]);
Rprintf("\n lrMat[2,] = ");
for (i = 0; i < nLR; ++i) Rprintf("%2.2f ",lrMatReal[i + nLR]);
Rprintf("\n");*/

//Rprintf("length of npmle result = %f\n", INTEGER(getAttrib(result, R_DimSymbol))[0]);
//Rprintf("first element name of npmle result = %s\n", CHARACTER(getAttrib(result, R_DimNamesSymbol))[0]);
//Rprintf("second element name of npmle result = %s\n", CHARACTER(getAttrib(result, R_DimNamesSymbol))[1]);
  // converting NPMLE into C objects
  *nNPMLE = INTEGER(getAttrib(getListElement(result, "intmap"), R_DimSymbol))[1];
  for (i = 0; i < *nNPMLE; ++i) {
    pf[i] = *nNPMLE == 1 ? 1.0: REAL(getListElement(result, "pf"))[i];
    intmap[2 * i] = REAL(getListElement(result, "intmap"))[2 * i]; // original dimension (2 x *nNPMLE)
    intmap[2 * i + 1] = REAL(getListElement(result, "intmap"))[2 * i + 1];
  }
  if (isInf) intmap[2 * *nNPMLE + 1] = 1.0/0.0;
  //EMICM sometimes incorrectly assigns the remaining probability to the final interval that is finite,
  //even if there are unbounded intervals in the data (so all the mass shouldn't be allocated to a finite interval).
  //See NPMLE_new.R line 41



/*Rprintf("pf\n");
for (i =0; i < *nNPMLE; ++i) {
  Rprintf("%2.4f ", pf[i]);
}
Rprintf("\nintmap row 1\n");
for (i =0; i < *nNPMLE; ++i) {
  Rprintf("%2.4f ", intmap[2*i]);
}
Rprintf("\nintmap row 2\n");
for (i =0; i < *nNPMLE; ++i) {
  Rprintf("%2.4f ", intmap[2*i + 1]);
}
Rprintf("\n");*/


  // interpolating NPMLE (*nNPMLE time points to *ntime)
  NP2prob (intmap, pf, nNPMLE, timepoints, ntime, tau, pfGrid);

  UNPROTECT(2);
}

