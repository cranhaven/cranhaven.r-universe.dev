
#include <Rmath.h>
#include <R.h>
#include <math.h>
#include "rf.h"
#include <stdlib.h> //for exit()

void PETO(double* surv, double* lr, double *timepoints, int nsample, int ntime, double* stat, int type) {
  int n, t, tt;
  double l = 0.0, r = 0.0, sl = 0.0, sr = 0.0;
  for (n = 0; n < nsample; ++n) {
    tt = 0;
    l = lr[n];
    r = lr[n + nsample];
    for (t = 0; t < ntime; ++t) { //finding s(l)
      if (l <= timepoints[t]) {
//Rprintf("t = %d, timepoints = %2.2f, surv[] = %2.2f  ", t, timepoints[t], surv[n + t * nsample]);
        sl = surv[n + t * nsample];
        tt = t;
        break;
      }
    }
    for (t = tt; t < ntime; ++t) { //finding s(r)
      if (r <= timepoints[t]) {
        sr = surv[n + t * nsample];
        break;
      }
    }
//Rprintf("l, r, sl, sr = ");
//Rprintf("(%2.2f %2.2f %2.2f %2.2f) ", l, r, sl, sr);
//Rprintf("\n");
    if (type == 1) { // Peto - log rank
      if (sl <= sr) {
        stat[n] = 1 + log(sl);
      } else {
        stat[n] = ((sl<=0.0 ? 0.0 : sl * log(sl)) - (sr<=0.0 ? 0.0 : sr * log(sr)))/(sl - sr);
      }
    } else { // Peto - Wilcoxon's rank sum
      stat[n] = sl + sr - 1;
    }
  }
}





/*--------------------------------------------------------------*/
void findPetoSplit(double *x, int *jdex, double *peto,
                   int mdim, int ntime, int nsample,
                   int ndstart, int ndend, int *msplit, double *decsplit,
                   double *ubest, int *ndendl, int *jstat, int mtry,
                   int nodecnt, int *cat, int nthsize, int ert, int uniErt) {
  int last, ncat[MAX_CAT], icat[MAX_CAT], lc, nl, nr, npopl, npopr, tieVar, tieVal;
  int i, j, kv, l, r = 0, ertr = 1, ertrTie, *mind, *ncase, mTried = 0;
  double *xt, *ut, *v, *yl, sumcat[MAX_CAT], avcat[MAX_CAT], tavcat[MAX_CAT], ubestt;
  double crit, critmax, critvar, suml, sumr, sumnode = 0.0, d, critParent, rUnif = 0.0;
  //nodepred = ntime x nrnodes x ntree

  ut = (double *) Calloc(nsample, double);
  xt = (double *) Calloc(nsample, double);
  v  = (double *) Calloc(nsample, double);
  yl = (double *) Calloc(nsample, double);
  mind  = (int *) Calloc(mdim, int);
  ncase = (int *) Calloc(nsample, int);
  zeroDouble(avcat, MAX_CAT);
  zeroDouble(tavcat, MAX_CAT);

  // providing sumnode
  for (j = ndstart; j <= ndend; ++j) {
    sumnode += peto[jdex[j] - 1];
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
    j = (int) (unif_rand() * (last+1));
    kv = mind[j];
    swapInt(mind[j], mind[last]);
    last--;

    lc = cat[kv];
    if (lc == 1) {
      /* numeric variable */
      for (j = ndstart; j <= ndend; ++j) {
        xt[j] = x[kv + (jdex[j] - 1) * mdim];
        yl[j] = peto[jdex[j] - 1];
      }
    } else {
      zeroInt(ncat, MAX_CAT);
      zeroDouble(sumcat, MAX_CAT);
      for (j = ndstart; j <= ndend; ++j) {
        l = (int) x[kv + (jdex[j] - 1) * mdim];
        sumcat[l - 1] += peto[jdex[j] - 1];
        ncat[l - 1] ++;
      }
      /* Compute means of Y by category. */
      for (j = 0; j < lc; ++j) {
        avcat[j] = ncat[j] ? sumcat[j] / ncat[j] : 0.0;
      }
      /* Make the category mean the `pseudo' X data. */
      for (j = 0; j < nsample; ++j) {
        xt[j] = avcat[(int) x[kv + (jdex[j] - 1) * mdim] - 1];
        yl[j] = peto[jdex[j] - 1];
      }
    }
    /* copy the x data in this node. */
    for (j = ndstart; j <= ndend; ++j) v[j] = xt[j];
//for (j = ndstart; j <= ndend; ++j) Rprintf("vj = %2.2f ", v[j]);
    for (j = 1; j <= nsample; ++j) ncase[j - 1] = j;
    R_qsort_I(v, ncase, ndstart + 1, ndend + 1);

//for (j = ndstart; j <= ndend; ++j) Rprintf("vj = %2.2f ", v[j]);
//Rprintf("ert in Peto(118) = %d \n", ert);
//Rprintf("v[ndstart] = %2.2f, v[ndend] = %2.2f \n", v[ndstart], v[ndend]);
    if (v[ndstart] >= v[ndend]) {
      continue;
    } else {
      mTried++;
    }
    /* ncase(n)=case number of v nth from bottom */
    /* Start from the right and search to the left. */
    critParent = sumnode * sumnode / nodecnt;
    suml = 0.0;
    sumr = sumnode;
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
      d = yl[ncase[j] - 1];
      suml += d;
      sumr -= d;
      npopl++;
      npopr--;
      if ((v[j] < v[j+1] || ertr) && (j >= ndstart + nthsize - 1)) {
        if (ertr && v[j] == v[j+1]) {
          ertrTie = 1;
          continue;
        }
        crit = (suml * suml / npopl) + (sumr * sumr / npopr) - critParent;
        // Rprintf("crit = %2.2f, critvar = %2.2f \n", crit, critvar);
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
    if (critvar == critmax && critmax > 0.0) {
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
}


