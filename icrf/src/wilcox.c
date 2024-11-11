
#include <Rmath.h>
#include <R.h>
#include <math.h>
#include "rf.h"
//#define RF_DEBUG2
//#define RF_EXPT

void pIndex (double *ivec, double *jcum, double *auc, int ntime) {
// measures prob i is greater than j
    int n;
    *auc = 0.0;
    for (n = 0; n < ntime; ++n) {
      *auc += jcum[n] * ivec[n];
//Rprintf("auc = %f += jcum[n] %f * ivec[n] %f \n", *auc, jcum[n], ivec[n]);
    }
//Rprintf("auc = %f\n", *auc);
}

/*--------------------------------------------------------------*/
void findWilcoxonSplit(double *x, int *jdex, double *pIndexMat,
                       int mdim, int ntime, int nsample,
                       int ndstart, int ndend, int *msplit, double *decsplit,
                       double *ubest, int *ndendl, int *jstat, int mtry,
                       int nodecnt, int *cat, int nthsize, int ert, int uniErt) {
  int last, ncat[MAX_CAT * MAX_CAT], icat[MAX_CAT], lc, nl, nr, npopl, npopr, tieVar, tieVal;
  int i, j, kv, l, ll, t, r = 0, ertr = 1, ertrTie, *mind, *ncase, mTried = 0, ncat2;
  double *xt, *ut, *v, *yl, sumcat[MAX_CAT * MAX_CAT], avcat[MAX_CAT], tavcat[MAX_CAT], ubestt;
  double crit, critmax, critvar; //critParent
  double *d, rUnif = 0.0;
  //nodepred = ntime x nrnodes x ntree


  ut = (double *) Calloc(nsample, double);
  xt = (double *) Calloc(nsample, double);
  v  = (double *) Calloc(nsample, double);
  yl = (double *) Calloc(nsample * nsample, double);

  d  = (double *) Calloc(1, double);

  mind  = (int *) Calloc(mdim, int);
  ncase = (int *) Calloc(nsample, int);
  zeroDouble(avcat, MAX_CAT);
  zeroDouble(tavcat, MAX_CAT);

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
#ifdef RF_EXPT
    Rprintf("warning: variable selection locked (line 58)!!");
    j = i==0? 10:(i==1? 1: (i==2? 9: (i==3? 20: 8))); //experiment!
    j -= 1;
#endif
    kv = mind[j];
    swapInt(mind[j], mind[last]);
    last--;


    lc = cat[kv];
    if (lc == 1) {
      /* numeric variable */
      for (j = ndstart; j <= ndend; ++j) {
        xt[j] = x[kv + (jdex[j] - 1) * mdim];
        for (t = ndstart; t <= ndend; ++t) {
          yl[t + nsample * j] = pIndexMat[(jdex[t] - 1) + nsample * (jdex[j] - 1)];
        }
      }
    } else {
      /* categorical variable */
      for (j = ndstart; j <= ndend; ++j) {
        xt[j] = x[kv + (jdex[j] - 1) * mdim];
        for (t = ndstart; t <= ndend; ++t) {
          yl[t + nsample * j] = pIndexMat[(jdex[t] - 1) + nsample * (jdex[j] - 1)];
        }
      }
      zeroInt(ncat, MAX_CAT * MAX_CAT);
      zeroDouble(sumcat, MAX_CAT * MAX_CAT);
      for (j = ndstart; j <= ndend; ++j) {
        l = (int) x[kv + (jdex[j] - 1) * mdim];
        for (t = ndstart; t <= ndend; ++t) {
          ll = (int) x[kv + (jdex[t] - 1) * mdim];
          //sumcat[(l - 1) + lc * (ll - 1)] += yl[(jdex[j] - 1) + lc * (jdex[t] - 1)];
          sumcat[(l - 1) + lc * (ll - 1)] += yl[t + nsample * j];
          ncat[(l - 1) + lc * (ll - 1)] ++;
        }
      }
      /* Compute means of Y by category. */
      for (j = 0; j < lc; ++j) {
        avcat[j] = 0.0;
        ncat2 = 0;
        for (t = 0; t < lc; ++t) {
          if (j != t) {
            avcat[j] += sumcat[j + lc * t];
            ncat2    += ncat[j + lc * t];
          }
        }
        avcat[j] = ncat2 ? avcat[j] / ncat2 : 0.5;
      }
      /* Make the category mean the `pseudo' X data. */
      for (j = ndstart; j <= ndend; ++j) {
        l = (int) x[kv + (jdex[j] - 1) * mdim];
        xt[j] = avcat[l - 1];
        //for (t = ndstart; t <= ndend; ++t) {
        //  yl[t + nsample * j] = pIndexMat[(jdex[t] - 1) + nsample * (jdex[j] - 1)];
        //}
      }
      /**/
    }
    /* copy the x data in this node. */
    for (j = ndstart; j <= ndend; ++j) v[j] = xt[j];
    for (j = 1; j <= nsample; ++j) ncase[j - 1] = j;
    R_qsort_I(v, ncase, ndstart + 1, ndend + 1);
//Rprintf("i = %d, v[ndstart] = %2.2f, v[ndend] = %2.2f, >= = %d\n",
//        i, v[ndstart], v[ndend], v[ndstart] >= v[ndend]);
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
#ifdef RF_EXPT
  Rprintf("warning: variable selection locked (line117)!!");
  //rUnif = i==0? 0.041467532:(i==1? -0.087643264 : (i==2? 0.000188747: (i==3? 0.118186069 : 0.163287015))); //experiment!
  //rUnif = i==0? -0.12006383:(i==1? -0.12435330  : (i==2? 0.05500731: (i==3? 0.18008461 : 0.14866162))); //experiment!
  rUnif = i==0? -0.5853144:(i==1?   -0.3460866  : (i==2? 0.1605057: (i==3? 0.4262944 : 0.2679701))); //experiment!

#endif

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
    *d = 0.0;
    for (j = ndstart; j <= ndend - nthsize; ++j) {
      if (ert) {
        if (j > r && 1 - ertrTie) break; //no need to update after r.
        ertr = (j == r) || ertrTie;
      }
      for (t = j + 1; t <= ndend; ++t) {
        *d += yl[(ncase[t] - 1) + nsample * (ncase[j] - 1)];
      }
      for (t = ndstart; t < j; ++t) {
        *d -= yl[(ncase[j] - 1) + nsample * (ncase[t] - 1)];
      }

//Rprintf("v[j] = %2.2f, v[j+1] = %2.2f, v[j] < v[j+1] = %d, j = %d, ndstart + nthsize - 1 = %d, ertr = %d \n",
//    v[j], v[j+1], v[j] < v[j+1], j,  ndstart + nthsize - 1, ertr);
      npopl++;
      npopr--;
      if ((v[j] < v[j+1] || ertr) && (j >= ndstart + nthsize - 1)) {
        if (ertr && v[j] == v[j+1]) {
          ertrTie = 1;
          continue;
        }
        crit = fabs(*d/npopl/npopr - 0.5);  //actual value: should be divided by 2.
//Rprintf("j %d, *d %f, npopl %d, npopr %d, crit %f\n", j +1, *d, npopl, npopr, crit);
#ifdef RF_DEBUG2
        Rprintf("kv[j] = %d, point = %f, wilcox = %2.2f\n", kv, v[j],  crit);
        //if (crit>100000) break;
        //if (isnan(crit)) break;
#endif
        if (crit > critvar) {
//Rprintf("j %d, crit renewed crit %f\n", j+1, crit);
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
      // modified. they should be at least greater than zero.
      // because critvar==critmax==0.0 means no variable has been chosen.
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
    //if (*ndendl >= ndend) *ndendl = ndend - nthsize;
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
  Free(d);
}

