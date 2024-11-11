/*******************************************************************
 Copyright (C) 2001-7 Leo Breiman, Adele Cutler and Merck & Co., Inc.

 This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
*******************************************************************/

/******************************************************************
* buildtree and findbestsplit routines translated from Leo's
* original Fortran code.
*
*      copyright 1999 by leo Breiman
*      this is free software and can be used for any purpose.
*      It comes with no guarantee.
*
******************************************************************/
#include <Rmath.h>
#include <R.h>
#include "rf.h"
#include <stdlib.h>
#include <math.h>
//#define RF_DEBUG
//#define RF_DEBUG2

void survTree(double *x, double *prob, double *pIndexMat, double *peto,
              int mdim, int ntime, int nsample, int *lDaughter, int *rDaughter,
              double *upper, double *avnode, int *nodestatus, int nrnodes,
              int *treeSize, int nthsize, int mtry, int *mbest, int *cat,
              double *tgini, int *varUsed, int method, int ert, int uniErt,
              int udNPMLE, double *lr, double *pf, double *intmap, double *timepoints,
              double *tau, double *bw, int fold, int debug) {
  int i, j, k, t, ncur, *jdex, *nodestart, *nodepop, *nNPMLE, nregular;
  int ndstart, ndend, ndendl, nodecnt, jstat, msplit;
  double decsplit, ubest, *pfGrid, *lrTmp;
  //avnode = ntime x nrnodes (x ntree)

  nodestart = (int *) Calloc(nrnodes, int);
  nodepop   = (int *) Calloc(nrnodes, int);
  nNPMLE    = (int *) Calloc(1, int);
  pfGrid    = (double *) Calloc(udNPMLE? ntime : 1, double);
  lrTmp     = (double *) Calloc(udNPMLE? 2 * nsample : 1, double);


  /* initialize some arrays for the tree */
  zeroInt(nodestatus, nrnodes);
  zeroInt(nodestart, nrnodes);
  zeroInt(nodepop, nrnodes);
  zeroDouble(avnode, ntime * nrnodes);
  if (udNPMLE) zeroDouble(lrTmp, 2 * nsample);

  jdex = (int *) Calloc(nsample, int);
  for (i = 1; i <= nsample; ++i) jdex[i-1] = i;

  ncur = 0;
  nodestart[0] = 0;
  nodepop[0] = nsample;
  nodestatus[0] = NODE_TOSPLIT;

  ///* compute mean interval probability for root node - not essentially needed. */
  //for (i = 0; i < nsample; ++i) {
  //  for (t = 0; t < ntime; ++t) {
  //    avnode[t + 0 * ntime] += prob[t + ntime * (jdex[i] - 1)]/nsample;
  //  }
  //}



/*
Rprintf("p-index matrix \n");
for (int n = 0; n < 5; ++n) {
for (t = 0; t < 5; ++t) {
Rprintf("%f    ", pIndexMat[n + t * nsample]);
}
Rprintf("\n");
}
Rprintf("lr matrix \n");
for (int n = 0; n < 5; ++n) {
  for (t = 0; t < 10; ++t) {
    Rprintf("%f ",  prob[t + n * ntime]);
  }
Rprintf("\n");
}*/



  /* start main loop */
  for (k = 0; k < nrnodes - 2; ++k) {
    if (k > ncur || ncur >= nrnodes - 2) break;
    /* skip if the node is not to be split */
    if (nodestatus[k] != NODE_TOSPLIT) continue;

    /* initialize for next call to findbestsplit */
    ndstart = nodestart[k];
    ndend = ndstart + nodepop[k] - 1;
    nodecnt = nodepop[k];
    jstat = 0;
    decsplit = 0.0;

    //Rprintf("k = %d, nodestatus[k] = %d, nodecnt = %d\n", k, nodestatus[k], nodecnt);
    //if (nodepop[k] < nthsize * 2) {  //newly added
    //  nodestatus[k] = NODE_TERMINAL;
    //  continue;
    //}

#ifdef RF_DEBUG
    Rprintf("before findBestSplit: ndstart=%d, ndend=%d, jstat=%d, decsplit=%f\n",
            ndstart, ndend, jstat, decsplit);
#endif
    if (method == 1) {
      findWilcoxonSplit(x, jdex, pIndexMat, mdim, ntime, nsample, ndstart, ndend, &msplit,
                        &decsplit, &ubest, &ndendl, &jstat, mtry,
                        nodecnt, cat, nthsize, ert, uniErt);
    } else if (method == 2) {
      findLRSplit(x, jdex, prob, mdim, ntime, nsample, ndstart, ndend, &msplit,
                    &decsplit, &ubest, &ndendl, &jstat, mtry,
                    nodecnt, cat, nthsize, ert, uniErt);
    } else if (method >= 3) { //3 = Peto Logrank, 4 = Peto WRS
      findPetoSplit(x, jdex, peto, mdim, ntime, nsample, ndstart, ndend, &msplit,
                    &decsplit, &ubest, &ndendl, &jstat, mtry,
                    nodecnt, cat, nthsize, ert, uniErt);
    }

#ifdef RF_DEBUG
    Rprintf(" after findBestSplit: ndstart=%d, ndend=%d, jstat=%d, decsplit=%f, msplit=%d\n",
            ndstart, ndend, jstat, decsplit, msplit);

#endif
    //if (debug && k == 17) {
if (debug) {
  Rprintf("\nk = %d, (1) \n", k);
  Rprintf("(l, r) = \n");
  for(t = 0; t < nodepop[k]; ++t)
    Rprintf("(%2.2f %2.2f) ", lr[2 * (jdex[ndstart + t] - 1)], lr[2 * (jdex[ndstart + t] - 1) + 1]);
  Rprintf("\nLeftnode (l, r) = \n");
  for(t = 0; t < nodepop[ncur + 1]; ++t) Rprintf("(%2.2f %2.2f) ", lr[2 * (jdex[ndstart + t] - 1)], lr[2 * (jdex[ndstart + t] - 1) + 1]);
  Rprintf("\nRightnode (l, r) = \n");
  for(t = 0; t < nodepop[ncur + 2]; ++t) Rprintf("(%2.2f %2.2f) ", lr[2 * (jdex[ndendl + 1 + t] - 1)], lr[2 * (jdex[ndendl + 1 + t] - 1) + 1]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d ", k, nodepop[k], k, nodestatus[k]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d  ", ncur + 1, nodepop[ncur + 1], ncur + 1, nodestatus[ncur + 1]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d\n", ncur + 2, nodepop[ncur + 2], ncur + 2, nodestatus[ncur + 2]);
}

    if (jstat == 1 || nodepop[k] <  nthsize * 2 ) {
      /* Node is terminal: Mark it as such and move on to the next. */
      nodestatus[k] = NODE_TERMINAL;
      //if (k==0) { // this condition should be removed: even not a root node can have jstat == 1 without prediction done.
        if (udNPMLE) {  // update NPMLE?
          zeroDouble(pfGrid, ntime);
          zeroDouble(lrTmp, nodepop[k] * 2);
          for(t = 0; t < nodepop[k]; ++t) {
            lrTmp[2*t] = lr[2 * (jdex[ndstart + t] - 1)];
            lrTmp[2*t + 1] = lr[2 * (jdex[ndstart + t] - 1) + 1];
          }

          NPMLE (lrTmp, nodepop[k], intmap, pf, nNPMLE, timepoints, &ntime, tau, pfGrid);


  /* if (k == 18) {
    Rprintf("\nRight node\n 1st row: ");
    for(t = 0; t < nodepop[k]; ++t) Rprintf("%2.5f ", lrTmp[2*t]);
    Rprintf("\n 2nd row: ");
    for(t = 0; t < nodepop[k]; ++t) Rprintf("%2.5f ", lrTmp[2*t + 1]);
    Rprintf("\n pf: ");
    for (t = 0; t < *nNPMLE; ++t) {
      Rprintf("%4.2f ", pf[t]);
    }
    Rprintf("\n intmap: ");
    for (t = 0; t < 2 * *nNPMLE; ++t) {
      Rprintf("%4.2f ", intmap[t]);
    }
    Rprintf("\n timepoints: ");
    for (t = 0; t < ntime; ++t) {
      Rprintf("%4.2f ", timepoints[t]);
      if ((double)(t/10.0) == (t/10)) Rprintf("\n");
    }
    Rprintf("\n pfGrid: ");
    double tmpSum=0.0;
    for (t = 0; t < ntime; ++t) {
      Rprintf("%4.2f ", pfGrid[t]);
      tmpSum += pfGrid[t];
      if ((double)(t/10.0) == (t/10)) Rprintf(" sum = %2.3f\n", tmpSum);
    }
    Rprintf("\n sum(pfGrid) = %2.3f\n", tmpSum);
    Rprintf("\n");
  }*/


          // using this, update avnode. (prob needs not be updated. will be updated after each forest using survPred)
          for (t = 0; t < ntime; ++t) {
            avnode[t + k * ntime] = pfGrid[t];
            pfGrid[t] = 0.0;
          }
          // empty intmap, pf, nNPMLE for the next time
          for (t = 0; t < *nNPMLE; ++t) {
            pf[t] = 0.0;
            intmap[t] = 0.0;
            intmap[t + *nNPMLE] = 0.0;
          }
        } else {
          for (t = 0; t < ntime; ++t) { //initialize first
            avnode[t + k * ntime] = 0.0;
          }
          for (j = ndstart; j <= ndend; ++j) {
            for (t = 0; t < ntime; ++t) {
              avnode[t + k * ntime] += prob[t + ntime * (jdex[j] - 1)]/nodepop[k];
            }
          }
        }
      //}
      continue;
    }

    /* Found the best split. */
    mbest[k] = msplit;
    varUsed[msplit - 1] = 1;
    upper[k] = ubest;
    tgini[msplit - 1 + mdim * fold] += decsplit;
    nodestatus[k] = NODE_INTERIOR;

    /* leftnode no.= ncur+1, rightnode no. = ncur+2. */
    nodepop[ncur + 1] = ndendl - ndstart + 1;
    nodepop[ncur + 2] = ndend - ndendl;
    nodestart[ncur + 1] = ndstart;
    nodestart[ncur + 2] = ndendl + 1;

/*Rprintf("jdex left\n");
for (t = ndstart; t <= ndendl; ++t) {
  Rprintf("%d ", jdex[t]);
}
Rprintf("\njdex right\n");
for (t = ndendl+1; t <= ndend; ++t) {
  Rprintf("%d ", jdex[t]);
}
Rprintf("\n");*/

    /* compute mean interval probability for the root node. */
    if (k == 0 && nodepop[k] <  nthsize * 2) {
      nodestatus[k] = NODE_TERMINAL;
      if (udNPMLE) {  // update NPMLE?
        zeroDouble(pfGrid, ntime);
        zeroDouble(lrTmp, nodepop[k] * 2);
        for(t = 0; t < nodepop[k]; ++t) {
          lrTmp[2*t] = lr[2 * (jdex[ndstart + t] - 1)];
          lrTmp[2*t + 1] = lr[2 * (jdex[ndstart + t] - 1) + 1];
        }

        NPMLE (lrTmp, nodepop[k], intmap, pf, nNPMLE, timepoints, &ntime, tau, pfGrid);
        // using this, update avnode. (prob needs not be updated. will be updated after each forest using survPred)
        for (t = 0; t < ntime; ++t) {
          avnode[t + k * ntime] = pfGrid[t];
        }
        // empty intmap, pf, nNPMLE for the next time
        for (t = 0; t < *nNPMLE; ++t) {
          pf[t] = 0.0;
          intmap[t] = 0.0;
          intmap[t + *nNPMLE] = 0.0;
        }

      } else {
        for (t = 0; t < ntime; ++t) { //initialize first
          avnode[t + k * ntime] = 0.0;
        }
        for (j = ndstart; j <= ndend; ++j) {
          for (t = 0; t < ntime; ++t) {
            avnode[t + k * ntime] += prob[t + ntime * (jdex[j] - 1)]/nodepop[k];
          }
        }
      }
      continue;
    }


//Rprintf("nodepop[ncur + 1] = %d, nodepop[ncur + 2] = %d\n", nodepop[ncur + 1], nodepop[ncur + 2]);
    /* compute mean interval probability for the left daughter node. */
    nodestatus[ncur+1] = NODE_TOSPLIT;
//if (k == 12) { Rprintf("nodepop[%d + 1] = %d\n", ncur, nodepop[ncur + 1]);}
//if (k == 12) { Rprintf("nodestatus[%d + 1] = %d\n", ncur, nodestatus[ncur + 1]);}
//if (k<4) Rprintf("153: nodestatus[%d +1] = %d\n", ncur, nodestatus[ncur + 1]);
    if (nodepop[ncur + 1] <  nthsize * 2) {
      nodestatus[ncur + 1] = NODE_TERMINAL;
//if (k<4) Rprintf("156: nodestatus[%d +1] = %d\n", ncur, nodestatus[ncur + 1]);
      // calculate the node prediction only at terminal nodes
      if (udNPMLE) {  // update NPMLE?
        // if node size is less than 6 (nthsize), use the mother node.
        nregular = nodepop[ncur + 1] >= nthsize ? nodepop[ncur + 1]: nodepop[k];
        zeroDouble(pfGrid, ntime);
        zeroDouble(lrTmp, nregular * 2);
        for(t = 0; t < nregular; ++t) {
          lrTmp[2*t] = lr[2 * (jdex[ndstart + t] - 1)];
          lrTmp[2*t + 1] = lr[2 * (jdex[ndstart + t] - 1) + 1];
        }

if (debug) {
  Rprintf("\nk = %d, Leftnode \n", k);
  Rprintf("\nLeftnode (l, r) = \n");
  for(t = 0; t < nregular; ++t) Rprintf("(%2.2f %2.2f) ", lrTmp[2*t], lrTmp[2*t + 1]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d ", k, nodepop[k], k, nodestatus[k]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d  ", ncur + 1, nodepop[ncur + 1], ncur + 1, nodestatus[ncur + 1]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d\n", ncur + 2, nodepop[ncur + 2], ncur + 2, nodestatus[ncur + 2]);
}
        //NPMLE (lrTmp, nodepop[ncur + 1], intmap, pf, nNPMLE, timepoints, &ntime, tau, pfGrid);
        NPMLE (lrTmp, nregular, intmap, pf, nNPMLE, timepoints, &ntime, tau, pfGrid);
/* if (k == 9) {
  Rprintf("\nLeft node\n1st row: ");
  for(t = 0; t < nodepop[ncur + 2]; ++t) Rprintf("%2.3f ", lrTmp[2*t]);
  Rprintf("\n 2nd row: ");
  for(t = 0; t < nodepop[ncur + 2]; ++t) Rprintf("%2.3f ", lrTmp[2*t + 1]);
  Rprintf("\n");
} */
  //if (isnan(pfGrid[1])) {
  //  Rprintf("nodepop = %d, nNPMLE= %d \n", nodepop[ncur + 2], *nNPMLE);
    //for (t = 0; t < ntime * 2; ++t) {
    //Rprintf("%4.2f ", lrTmp[t]);
    //if (isnan(lrTmp[t])) {
    //  Rprintf("t = %d, jdex = %d,  lrTmp[t] = %4.2f \n", t, jdex[ndstart + (int)(t/2)], lrTmp[t]);
    //}
    //}
  //}

        //Rprintf("ndpop = %d\n", nodepop[ncur + 1]);
/*if (1) {
  Rprintf("\nintmap = \n");
  for (t = 0; t < *nNPMLE*2; ++t) {
    Rprintf("%4.2f ", intmap[t]);
  }
  Rprintf("\npf = \n");
  for (t = 0; t < *nNPMLE; ++t) {
    Rprintf("%4.2f ", pf[t]);
  }
  Rprintf("\nNPMLE. pfGrid = ");
  for (t = 0; t < ntime; ++t) {
    Rprintf("%4.4f ", pfGrid[t]);
  }
  Rprintf("avnode = ");
}*/


        // using this, update avnode. (prob needs not be updated. will be updated after each forest using survPred)
        for (t = 0; t < ntime; ++t) {
          avnode[t + (ncur + 1) * ntime] = pfGrid[t];
        }

        // empty intmap, pf, nNPMLE for the next time
        for (t = 0; t < *nNPMLE; ++t) {
          pf[t] = 0.0;
          intmap[t] = 0.0;
          intmap[t + *nNPMLE] = 0.0;
        }

      } else {  // if not, simply average conditional expectation.
        for (t = 0; t < ntime; ++t) { //initialize first
          avnode[t + (ncur + 1) * ntime] = 0.0;
        }
        for (j = ndstart; j <= ndendl; ++j) {
          for (t = 0; t < ntime; ++t) {
            avnode[t + (ncur + 1) * ntime] += prob[t + ntime * (jdex[j] - 1)]/nodepop[ncur + 1];
          }
        }
      }

  }
//if (k == 12) { Rprintf("nodestatus[%d + 1] = %d\n", ncur, nodestatus[ncur + 1]);}
    /* compute mean interval probability for the right daughter node. */

    nodestatus[ncur + 2] = NODE_TOSPLIT;
    if (nodepop[ncur + 2] < nthsize * 2) {
      nodestatus[ncur + 2] = NODE_TERMINAL;

      // calculate the node prediction only at terminal nodes
      if (udNPMLE) {  // update NPMLE?
        // if node size is less than 6 (nthsize), use the mother node.
        nregular = nodepop[ncur + 2] >= nthsize ? nodepop[ncur + 2]: nodepop[k];
        zeroDouble(pfGrid, ntime);
        zeroDouble(lrTmp, nregular * 2);
        for(t = 0; t < nregular; ++t) {
          lrTmp[2*t] = lr[2 * (jdex[ndend - t] - 1)];
          lrTmp[2*t + 1] = lr[2 * (jdex[ndend - t] - 1) + 1];
        }

if (debug) {
  Rprintf("\nk = %d, Rightnode \n", k);
  Rprintf("\nRightnode (l, r) = \n");
  for(t = 0; t < nregular; ++t) Rprintf("(%2.2f %2.2f) ", lrTmp[2*t], lrTmp[2*t + 1]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d ", k, nodepop[k], k, nodestatus[k]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d  ", ncur + 1, nodepop[ncur + 1], ncur + 1, nodestatus[ncur + 1]);
  Rprintf("\nnodepop[%d] = %d, nodestatus[%d] = %d\n", ncur + 2, nodepop[ncur + 2], ncur + 2, nodestatus[ncur + 2]);
}

        //Rprintf("ndpop = %d\n", nodepop[ncur + 2]);
        //NPMLE (lrTmp, nodepop[ncur + 2], intmap, pf, nNPMLE, timepoints, &ntime, tau, pfGrid);
        NPMLE (lrTmp, nregular, intmap, pf, nNPMLE, timepoints, &ntime, tau, pfGrid);
  //if (isnan(pfGrid[1])) {
    //Rprintf("nodepop = %d, nNPMLE= %d \n", nodepop[ncur + 2], *nNPMLE);
    //for (t = 0; t < ntime * 2; ++t) {
      //Rprintf("%4.2f ", lrTmp[t]);
      //if (isnan(lrTmp[t])) {
      //  Rprintf("t = %d, jdex = %d,  lrTmp[t] = %4.2f \n", t, jdex[ndstart + (int)(t/2)], lrTmp[t]);
      //}
    //}
  //}
/*Rprintf("NPMLE. pfGrid = ");
for (t = 0; t < ntime; ++t) {
Rprintf("%4.2f ", pfGrid[t]);
}
Rprintf("\n"); */
/*if (1) {
  Rprintf("\nintmap = \n");
  for (t = 0; t < *nNPMLE*2; ++t) {
    Rprintf("%4.2f ", intmap[t]);
  }
  Rprintf("\npf = \n");
  for (t = 0; t < *nNPMLE; ++t) {
    Rprintf("%4.2f ", pf[t]);
  }
  Rprintf("\nNPMLE. pfGrid = ");
  for (t = 0; t < ntime; ++t) {
    Rprintf("%4.4f ", pfGrid[t]);
  }
  Rprintf("avnode = ");
}*/
        // using this, update avnode. (prob needs not be updated. will be updated after each forest using survPred)
        for (t = 0; t < ntime; ++t) {
          avnode[t + (ncur + 2) * ntime] = pfGrid[t];
          //Rprintf("%4.4f ", avnode[t + (ncur + 2) * ntime]);
        }

        // empty intmap, pf, nNPMLE for the next time
        for (t = 0; t < *nNPMLE; ++t) {
          pf[t] = 0.0;
          intmap[t] = 0.0;
          intmap[t + *nNPMLE] = 0.0;
        }

      } else {
        for (t = 0; t < ntime; ++t) { //initialize first
          avnode[t + (ncur + 2) * ntime] = 0.0;
        }
        // calculate the node prediction only at terminal nodes
        for (j = ndendl + 1; j <= ndend; ++j) {
          for (t = 0; t < ntime; ++t) {
            avnode[t + (ncur + 2) * ntime] += prob[t + ntime * (jdex[j] - 1)]/nodepop[ncur + 2];
          }
        }
      }

    }
    /* map the daughter nodes */
    lDaughter[k] = ncur + 1 + 1;
    rDaughter[k] = ncur + 2 + 1;
//if (k<2) Rprintf("255: k =%d, ncur = %d, nodestatus[%d] = %d, lDaughter[k] = %d, rDaughter[k] = %d\n", k, ncur, k, nodestatus[k], lDaughter[k], rDaughter[k]);
    /* Augment the tree by two nodes. */
    ncur += 2;

#ifdef RF_DEBUG
    Rprintf(" after split: ldaughter=%d, rdaughter=%d, ncur=%d\n",
            lDaughter[k], rDaughter[k], ncur);
#endif

//if (k<2) Rprintf("264: k =%d, ncur = %d, nodepop[k] = %d, nodestatus[k] = %d\n", k, ncur, nodepop[k], nodestatus[k]);

}
  *treeSize = nrnodes;
  for (k = nrnodes - 1; k >= 0; --k) {
    if (nodestatus[k] == 0) (*treeSize)--;
    if (nodestatus[k] == NODE_TOSPLIT) {
      nodestatus[k] = NODE_TERMINAL;
    }
  }

#ifdef RF_DEBUG2
  Rprintf("nodestatus, pop, left/right nodes, var, ubest\n");
  for (k = 0; k < *treeSize; ++k) {
    Rprintf("k+1 = %d, status %d, pop = %d, left = %d, right = %d, mbest = %d, upper = %2.2f\n",
            k+1, nodestatus[k], nodepop[k], lDaughter[k], rDaughter[k], mbest[k], upper[k]);
  }
#endif

  Free(nodestart);
  Free(jdex);
  Free(nodepop);
  Free(nNPMLE);
  if (udNPMLE) Free(pfGrid);
  if (udNPMLE) Free(lrTmp);
  }

/*====================================================================*/
void predictSurvTree(double *x, int nsample, int mdim, int ntime,
                     int *lDaughter, int *rDaughter, int *nodestatus,
                     double *survPred, double *split, double *nodepred,
                     int *splitVar, int treeSize, int *cat, int maxcat,
                     int *nodex, int *nrnodes,
                     int ntimeSm, double *survPredSm, double *nodepredSm, int Sm) {
  int i, j, k, m, t, *cbestsplit;
  double dpack;
  // When Sm == 0, (int ntimeSm, double *survPredSm, double *nodepredSm) are not needed!

  /* decode the categorical splits */
  cbestsplit = (int *) Calloc(maxcat > 1? maxcat * treeSize : 1, int);
  if (maxcat > 1) {
    zeroInt(cbestsplit, maxcat * treeSize);
    for (i = 0; i < treeSize; ++i) {
      if (nodestatus[i] != NODE_TERMINAL && cat[splitVar[i] - 1] > 1) {
        dpack = split[i];
        /* unpack `npack' into bits */
        /* unpack(dpack, maxcat, cbestsplit + i * maxcat); */
        for (j = 0; j < cat[splitVar[i] - 1]; ++j) {
          cbestsplit[j + i*maxcat] = ((unsigned long) dpack & 1) ? 1 : 0;
          dpack = dpack / 2.0 ;
          /* cbestsplit[j + i*maxcat] = npack & 1; */
        }
      }
    }
  }

  for (i = 0; i < nsample; ++i) {
    k = 0;
    while (nodestatus[k] != NODE_TERMINAL) { /* go down the tree */
          m = splitVar[k] - 1;
      if (cat[m] == 1) {
        k = (x[m + i*mdim] <= split[k]) ?
        lDaughter[k] - 1 : rDaughter[k] - 1;
      } else {
        /* Split by a categorical predictor */
        k = cbestsplit[(int) x[m + i * mdim] - 1 + k * maxcat] ?
        lDaughter[k] - 1 : rDaughter[k] - 1;
      }
    }
    /* terminal node: assign prediction and move on to next */
//Rprintf("predicting at %dth node: ", k);
    survPred[i + 0 * nsample] = 1.0 - nodepred[0 + k * ntime];
    for (t = 1; t < ntime; ++t) {
      //Rprintf("%f ", nodepred[t + k * ntime]);
      survPred[i + t * nsample] = survPred[i + (t-1) * nsample] - nodepred[t + k * ntime];
    }
    if (Sm) {
      survPredSm[i + 0 * nsample] = 1.0 - nodepredSm[0 + k * ntimeSm];
      for (t = 1; t < ntimeSm; ++t) {
        //Rprintf("%f ", nodepred[t + k * ntime]);
        survPredSm[i + t * nsample] = survPredSm[i + (t-1) * nsample] - nodepredSm[t + k * ntimeSm];
      }
    }
    //Rprintf("\n");
    nodex[i] = k + 1;
  }
  if (maxcat > 1) Free(cbestsplit);
}
