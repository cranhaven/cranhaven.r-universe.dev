
#include <Rmath.h>
#include <R.h>
#include <math.h>
#include "rf.h"
#include <stdlib.h> //for exit()
//#define RF_DEBUG2

void logRank(double* N1j, double* N2j, double* O1, double* numJ, double* denJ,
             int ntime, double* stat) {
  int j;
  double num, den;
  num = O1[0];
  den = 0.0;

  for (j = 0; j < ntime; ++j) {
    num -= (numJ[j] * N1j[j]);
  }
  for (j = 0; j < ntime; ++j) {
    den += (denJ[j] * N1j[j] * N2j[j]);
  }
  stat[0] = num * num / den;
//Rprintf("O1 = %f\n", O1[0]);
//Rprintf("N1j[0] = %f, N2j[0] = %f, ", N1j[0], N2j[0]);
//Rprintf("N1j[1] = %f, N2j[1] = %f, ", N1j[1], N2j[1]);
//Rprintf("N1j[2] = %f, N2j[2] = %f\n", N1j[2], N2j[2]);
//if (isnan(stat[0])) Rprintf("num = %f, den = %f, stat = %f, O1 = %f \n", num, den, stat[0], O1[0]);
}



/*--------------------------------------------------------------*/
void findLRSplit(double *x, int *jdex, double *prob,
                   int mdim, int ntime, int nsample,
                   int ndstart, int ndend, int *msplit, double *decsplit,
                   double *ubest, int *ndendl, int *jstat, int mtry,
                   int nodecnt, int *cat, int nthsize, int ert, int uniErt) {
  int last, icat[MAX_CAT], lc, nl, nr, npopl, npopr, tieVar, tieVal; //ncat[MAX_CAT],
  int i, j, kv, t, r = 0, ertr = 1, ertrTie, *mind, *ncase, mTried = 0;  //l,
  double *xt, *ut, *v, *yl, avcat[MAX_CAT], tavcat[MAX_CAT], ubestt; //sumcat[MAX_CAT],
  double crit, critmax, critvar; //critParent
  double *Oj, *Nj, *O1, *numj, *denj, *d, *dcum, *N1j, *N2j, rUnif = 0.0;
  //nodepred = ntime x nrnodes x ntree

  ut = (double *) Calloc(nsample, double);
  xt = (double *) Calloc(nsample, double);
  v  = (double *) Calloc(nsample, double);
  yl = (double *) Calloc(nsample * ntime, double);

  Oj = (double *) Calloc(ntime, double);
  Nj = (double *) Calloc(ntime, double);
  numj = (double *) Calloc(ntime, double);
  denj = (double *) Calloc(ntime, double);
  N1j = (double *) Calloc(ntime, double);
  N2j = (double *) Calloc(ntime, double);
  d  = (double *) Calloc(ntime, double);
  dcum  = (double *) Calloc(ntime, double);
  O1 = (double *) Calloc(1, double);

  mind  = (int *) Calloc(mdim, int);
  ncase = (int *) Calloc(nsample, int);
  zeroDouble(avcat, MAX_CAT);
  zeroDouble(tavcat, MAX_CAT);

  Nj[0] = 1.0 * nodecnt;
  for (t = 0; t < ntime; ++t) {
    for (j = ndstart; j <= ndend; ++j) {
      Oj[t] += prob[t + ntime * (jdex[j] - 1)];
    }
    //O1[0] += Oj[t];
  }
  for (t = 1; t < ntime; ++t) {
    Nj[t] = Nj[t-1] - Oj[t-1];
  }
  for (t = 0; t < ntime; ++t) {
    if (Nj[t] == 0) {
      numj[t] = 0.0;
      denj[t] = 0.0;
    } else {
      numj[t] = Oj[t] / Nj[t];
      denj[t] = numj[t] * (Nj[t] - Oj[t]) / Nj[t] / Nj[t];
    }
  }

  /* START BIG LOOP */
  *msplit = -1;
  *decsplit = 0.0;
  critmax = 0.0;
  ubestt = 0.0;
  for (i=0; i < mdim; ++i) mind[i] = i;

  last = mdim - 1;
  tieVar = 1;
  for (i = 0; i < mdim && mTried < mtry; ++i) {

    critvar = 0.0;
    zeroDouble(N1j, ntime);
    O1[0] = 0.0;
    for (t = 0; t < ntime; ++t) {
      N2j[t] = Nj[t];
    }
    j = (int) (unif_rand() * (last+1));
    kv = mind[j];
    swapInt(mind[j], mind[last]);
    last--;

    lc = cat[kv];
    if (lc == 1) {
      /* numeric variable */
      for (j = ndstart; j <= ndend; ++j) {
        xt[j] = x[kv + (jdex[j] - 1) * mdim];
        for (t = 0; t < ntime; ++t) {
          yl[t + ntime * j] = prob[t + ntime * (jdex[j] - 1)];
        }
      }
    } else {
      /* categorical variable - TBD!!!!!
      zeroInt(ncat, MAX_CAT);
      zeroDouble(sumcat, MAX_CAT);
      for (j = ndstart; j <= ndend; ++j) {
      l = (int) x[kv + (jdex[j] - 1) * mdim];
      sumcat[l - 1] += y[jdex[j] - 1];
      ncat[l - 1] ++;
      }
      / Compute means of Y by category. /
      for (j = 0; j < lc; ++j) {
      avcat[j] = ncat[j] ? sumcat[j] / ncat[j] : 0.0;
      }
      / Make the category mean the `pseudo' X data. /
      for (j = 0; j < nsample; ++j) {
      xt[j] = avcat[(int) x[kv + (jdex[j] - 1) * mdim] - 1];
      for (t = 0; t < ntime; ++t) {
      yl[t + ntime * j] = prob[t + ntime * (jdex[j] - 1)];
      }
      }
      */
    }
    /* copy the x data in this node. */
    for (j = ndstart; j <= ndend; ++j) v[j] = xt[j];
    for (j = 1; j <= nsample; ++j) ncase[j - 1] = j;
    R_qsort_I(v, ncase, ndstart + 1, ndend + 1);

    if (v[ndstart] >= v[ndend]) {
      continue;
    } else {
      mTried++;
    }
    /* ncase(n)=case number of v nth from bottom */
    /* Start from the right and search to the left. */
    //critParent = 0.0;
    npopl = 0;
    npopr = nodecnt;
    crit = 0.0;
    tieVal = 1;
    ertrTie = 0;

    if (ert) { // r = random split point index
      if (uniErt) {
        rUnif = unif_rand() * (v[ndend - nthsize + 1] - v[ndstart + nthsize - 1]) +
          v[ndstart + nthsize - 1];
        //Rprintf("warning: variable selection locked (line117)!!");
        //rUnif = i==0? 0.041467532:(i==1? -0.087643264 : (i==2? 0.000188747: (i==3? 0.118186069 : 0.163287015))); //experiment!

        for (j = ndstart + nthsize; j <= ndend - nthsize + 1; ++j) { //finding correspoding r.
          if (v[j] > rUnif) {
            r = j -1;
            break;
          }
        }
        //Rprintf("uniERT=1 rUnif = %4.2f, r = %d \n", rUnif, r);
      } else {
        r = (int) (unif_rand() * (ndend - ndstart + 2 - 2 * nthsize));
        r += ndstart + nthsize - 1; // picking only one split point for each candidate variable
        //Rprintf("uniERT=0 r = %d \n", r);
      }
    }

    /* Search through the "gaps" in the x-variable. */
    for (j = ndstart; j <= ndend - nthsize; ++j) {
      if (ert) {
        if (j > r && 1 - ertrTie) break; //no need to update after r.
        ertr = (j == r) || ertrTie;
      }
// Rprintf("%2.2f ", O1[0]);
      for (t = 0; t < ntime; ++t) {
        d[t] = yl[t + ntime * (ncase[j] - 1)];
        O1[0] += d[t];
//Rprintf("(%2.2f, %2.2f) ", d[t], O1[0]);
        if (t == 0) {
          dcum[t] = d[t];
          N1j[t] += 1.0;
          N2j[t] -= 1.0;
        } else {
          dcum[t] = dcum[t-1] + d[t];
          N1j[t] += (1.0 - dcum[t-1]);
          N2j[t] -= (1.0 - dcum[t-1]);
        }
      }
//Rprintf("O1 = (%2.2f) \n", O1[0]);
/* if (O1[0] == 0) {
  Rprintf("yl (j=j-1)");
  for (t = 0; t < ntime; ++t) {
    Rprintf("%4.2f ", yl[t + ntime * (ncase[j-1] - 1)]);
  }
  Rprintf("\nyl (j=j)");
  for (t = 0; t < ntime; ++t) {
    Rprintf("%4.2f ", yl[t + ntime * (ncase[j] - 1)]);
  }
  Rprintf("\n");
} */

      npopl++;
      npopr--;
      if ((v[j] < v[j+1] || ertr) && (j >= ndstart + nthsize - 1)) {
        if (ertr && v[j] == v[j+1]) {
          ertrTie = 1;
          continue;
        }
        logRank(N1j, N2j, O1, numj, denj, ntime, &crit);
#ifdef RF_DEBUG2
        //Rprintf("O1 = %f, N1j[0] = %f, N1j[1] = %f, N1j[2] = %f,  N2j[0] = %f, N2j[1] = %f, N2j[2] = %f, \n", O1[0], N1j[0], N1j[1], N1j[2], N2j[0], N2j[1], N2j[2]);
        Rprintf("O1 = %f, N1j[0] = %f, N1j[1] = %f, ...", O1[0], N1j[0], N1j[1]);
        Rprintf("kv = %d, j = %d, point = %f, logrank = %f\n", kv, j, v[j],  crit);
        //if (crit>100000) break;
        //if (isnan(crit)) break;
#endif
        if (crit > critvar) {
          ubestt = (ert && uniErt)? rUnif: (v[j] + v[j+1]) / 2.0;
          critvar = crit;
          tieVal = 1;
        }
        if (crit == critvar && critvar > 0.0) {
          tieVal++;
          if (unif_rand() < 1.0 / tieVal) {
            ubestt = (ert && uniErt)? rUnif: (v[j] + v[j+1]) / 2.0;
            critvar = crit;
          }
        }
      }
    }

    if (critvar > critmax) {
      *ubest = ubestt;
      *msplit = kv + 1;
      critmax = critvar;
      for (j = ndstart; j <= ndend; ++j) {
        ut[j] = xt[j];
      }
      if (cat[kv] > 1) {
        for (j = 0; j < cat[kv]; ++j) tavcat[j] = avcat[j];
      }
      tieVar = 1;
    }
    if (critvar == critmax && critvar > 0.0) {
      tieVar++;
      if (unif_rand() < 1.0 / tieVar) {
        *ubest = ubestt;
        *msplit = kv + 1;
        critmax = critvar;
        for (j = ndstart; j <= ndend; ++j) {
          ut[j] = xt[j];
        }
        if (cat[kv] > 1) {
          for (j = 0; j < cat[kv]; ++j) tavcat[j] = avcat[j];
        }
      }
    }

  }
#ifdef RF_DEBUG
  Rprintf("best = %d at %f with value = %f\n", *msplit, *ubest, critmax);
#endif
  *decsplit = critmax;

  /* If best split can not be found, set to terminal node and return. */
  if (*msplit != -1) {
    nl = ndstart;
    for (j = ndstart; j <= ndend; ++j) {
      if (ut[j] <= *ubest) {
        nl++;
        ncase[nl-1] = jdex[j];
      }
    }
    *ndendl = imax2(nl - 1, ndstart);
    nr = *ndendl + 1;
    for (j = ndstart; j <= ndend; ++j) {
      if (ut[j] > *ubest) {
        if (nr >= nsample) break;
        nr++;
        ncase[nr - 1] = jdex[j];
      }
    }
    if (*ndendl >= ndend) *ndendl = ndend - 1;
    for (j = ndstart; j <= ndend; ++j) jdex[j] = ncase[j];

    lc = cat[*msplit - 1];
    if (lc > 1) {
      for (j = 0; j < lc; ++j) {
        icat[j] = (tavcat[j] < *ubest) ? 1 : 0;
      }
      *ubest = pack(lc, icat);
    }
  } else *jstat = 1;

  Free(ncase);
  Free(mind);
  Free(v);
  Free(yl);
  Free(xt);
  Free(ut);
  Free(Oj);
  Free(Nj);
  Free(numj);
  Free(denj);
  Free(N1j);
  Free(N2j);
  Free(d);
  Free(dcum);
  Free(O1);

}


