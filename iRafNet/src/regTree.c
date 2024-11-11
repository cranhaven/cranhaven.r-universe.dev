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

void regTree(double *x, double *y, int mdim, int nsample, int *lDaughter,
             int *rDaughter,
             double *upper, double *avnode, int *nodestatus, int nrnodes,
             int *treeSize, int nthsize, int mtry, int *mbest, int *cat,
	     double *tgini, int *varUsed, double *sw) {
    int i, j, k, m, ncur, *jdex, *nodestart, *nodepop;
    int ndstart, ndend, ndendl, nodecnt, jstat, msplit;
    double d, ss, av, decsplit, ubest, sumnode;

    nodestart = (int *) Calloc(nrnodes, int);
    nodepop   = (int *) Calloc(nrnodes, int);

    /* initialize some arrays for the tree */
    zeroInt(nodestatus, nrnodes);
    zeroInt(nodestart, nrnodes);
    zeroInt(nodepop, nrnodes);
    zeroDouble(avnode, nrnodes);


    jdex = (int *) Calloc(nsample, int);
    for (i = 1; i <= nsample; ++i) jdex[i-1] = i;

    ncur = 0;
    nodestart[0] = 0;
    nodepop[0] = nsample;
    nodestatus[0] = NODE_TOSPLIT;

    /* compute mean and sum of squares for Y */
    av = 0.0;
    ss = 0.0;
    for (i = 0; i < nsample; ++i) {
		d = y[jdex[i] - 1];
		ss += i * (av - d) * (av - d) / (i + 1);
		av = (i * av + d) / (i + 1);
    }
    avnode[0] = av;

    /* start main loop */
    for (k = 0; k < nrnodes - 2; ++k) {
		if (k > ncur || ncur >= nrnodes - 2) break;
		/* skip if the node is not to be split */
		if (nodestatus[k] != NODE_TOSPLIT) continue;

		/* initialize for next call to findbestsplit */
		ndstart = nodestart[k];
		ndend = ndstart + nodepop[k] - 1;
		nodecnt = nodepop[k];
		sumnode = nodecnt * avnode[k];
		jstat = 0;
		decsplit = 0.0;
		
		findBestSplit(x, jdex, y, mdim, nsample, ndstart, ndend, &msplit,
                      &decsplit, &ubest, &ndendl, &jstat, mtry, sumnode,
                      nodecnt, cat, sw);
		if (jstat == 1) {
			/* Node is terminal: Mark it as such and move on to the next. */
			nodestatus[k] = NODE_TERMINAL;
			continue;
		}
        /* Found the best split. */
        mbest[k] = msplit;
        varUsed[msplit - 1] = 1;
		upper[k] = ubest;
		tgini[msplit - 1] += decsplit;
		nodestatus[k] = NODE_INTERIOR;
		
		/* leftnode no.= ncur+1, rightnode no. = ncur+2. */
		nodepop[ncur + 1] = ndendl - ndstart + 1;
		nodepop[ncur + 2] = ndend - ndendl;
		nodestart[ncur + 1] = ndstart;
		nodestart[ncur + 2] = ndendl + 1;
		
		/* compute mean and sum of squares for the left daughter node */
		av = 0.0;
		ss = 0.0;
		for (j = ndstart; j <= ndendl; ++j) {
			d = y[jdex[j]-1];
			m = j - ndstart;
			ss += m * (av - d) * (av - d) / (m + 1);
			av = (m * av + d) / (m+1);
		}
		avnode[ncur+1] = av;
		nodestatus[ncur+1] = NODE_TOSPLIT;
		if (nodepop[ncur + 1] <= nthsize) {
			nodestatus[ncur + 1] = NODE_TERMINAL;
		}

		/* compute mean and sum of squares for the right daughter node */
		av = 0.0;
		ss = 0.0;
		for (j = ndendl + 1; j <= ndend; ++j) {
			d = y[jdex[j]-1];
			m = j - (ndendl + 1);
			ss += m * (av - d) * (av - d) / (m + 1);
			av = (m * av + d) / (m + 1);
		}
		avnode[ncur + 2] = av;
		nodestatus[ncur + 2] = NODE_TOSPLIT;
		if (nodepop[ncur + 2] <= nthsize) {
			nodestatus[ncur + 2] = NODE_TERMINAL;
		}
		
		/* map the daughter nodes */
		lDaughter[k] = ncur + 1 + 1;
		rDaughter[k] = ncur + 2 + 1;
		/* Augment the tree by two nodes. */
		ncur += 2;
    }
    *treeSize = nrnodes;
    for (k = nrnodes - 1; k >= 0; --k) {
        if (nodestatus[k] == 0) (*treeSize)--;
        if (nodestatus[k] == NODE_TOSPLIT) {
            nodestatus[k] = NODE_TERMINAL;
        }
    }
    Free(nodestart);
    Free(jdex);
    Free(nodepop);
    
}

/*--------------------------------------------------------------*/
void findBestSplit(double *x, int *jdex, double *y, int mdim, int nsample,
		   int ndstart, int ndend, int *msplit, double *decsplit,
		   double *ubest, int *ndendl, int *jstat, int mtry,
		   double sumnode, int nodecnt, int *cat, double *sw) {
    int last, ncat[32], icat[32], lc, nl, nr, npopl, npopr;
    int i, kv, j, l, *mind, *ncase, mindo, k, mvar,kk,last1,jsel;
    double *xt, *ut, *v, *yl, sumcat[32], avcat[32], tavcat[32], ubestt, wsel ,jk;
    double crit, critmax, critvar, suml, sumr, d, critParent, *weights, *sumweights, sww;

    ut = (double *) Calloc(nsample, double);
    xt = (double *) Calloc(nsample, double);
    v  = (double *) Calloc(nsample, double);
    yl = (double *) Calloc(nsample, double);

    mind  = (int *) Calloc(mdim, int);
    ncase = (int *) Calloc(nsample, int);
    zeroDouble(avcat, 32);
    zeroDouble(tavcat, 32);

   
    mindo=mdim+1;
    weights =      (double *) Calloc(mdim, double);
    sumweights =      (double *) Calloc(mindo, double);
    
    /* START BIG LOOP */
    *msplit = -1;
    *decsplit = 0.0;
    critmax = 0.0;
    ubestt = 0.0;

    /* MODIFIED part   *****************************************************************************/ 

    for (i=0; i < mdim; ++i) mind[i] = i;

    last = mdim;

    for (k = 0; k < last; ++k) { /* for 1*/
           weights[k]=sw[k];
    }

  
  for (i = 0; i < mtry; ++i) {
    critvar = 0.0;
    /*Rprintf("prima\n");*/
    jk = unif_rand();
    sumweights[0]=0.0;
    
    for (k = 0; k < last; ++k) { /* for 1*/
                  sumweights[k+1]=weights[k]+sumweights[k];  /* compute cumulative */
                  /* Rprintf("sum = %f\n",sumweights[k+1] );*/ 
                 } /* end for 1*/
     
        for (k = 0; k < last; ++k) {
                if (sumweights[k+1] >= jk) {
                  if (sumweights[k] <= jk) {
                        kv=mind[k]; /* select variable */
                        wsel=weights[k];
                for (kk = 1; kk < last; ++kk) { /*    for2    */
                     if (kk > k) { /*if 1*/
                      sww=weights[kk-1];
                      weights[kk-1]=weights[kk];
                      weights[kk]=sww;

                      jsel=mind[kk-1];
                      mind[kk-1]=mind[kk];
                      mind[kk]=jsel; 
                      }/*end if 1*/
                     weights[kk-1]=weights[kk-1]/(1-wsel);
                     } /*  end for2  */
                     }
                  }
           }  /* end for */

      last=last-1;
      /*Rprintf("last = %d\n",last );*/

/* END MODIFIED part   *****************************************************************************/ 
	
		lc = cat[kv];
		if (lc == 1) {
			/* numeric variable */
			for (j = ndstart; j <= ndend; ++j) {
				xt[j] = x[kv + (jdex[j] - 1) * mdim];
				yl[j] = y[jdex[j] - 1];
			}
		} else {
			/* categorical variable */
            zeroInt(ncat, 32);
			zeroDouble(sumcat, 32);
			for (j = ndstart; j <= ndend; ++j) {
				l = (int) x[kv + (jdex[j] - 1) * mdim];
				sumcat[l - 1] += y[jdex[j] - 1];
				ncat[l - 1] ++;
			}
            /* Compute means of Y by category. */
			for (j = 0; j < lc; ++j) {
				avcat[j] = ncat[j] ? sumcat[j] / ncat[j] : 0.0;
			}
            /* Make the category mean the `pseudo' X data. */
			for (j = 0; j < nsample; ++j) {
				xt[j] = avcat[(int) x[kv + (jdex[j] - 1) * mdim] - 1];
				yl[j] = y[jdex[j] - 1];
			}
		}
        /* copy the x data in this node. */
		for (j = ndstart; j <= ndend; ++j) v[j] = xt[j];
		for (j = 1; j <= nsample; ++j) ncase[j - 1] = j;
		R_qsort_I(v, ncase, ndstart + 1, ndend + 1);
		if (v[ndstart] >= v[ndend]) continue;
		/* ncase(n)=case number of v nth from bottom */
		/* Start from the right and search to the left. */
		critParent = sumnode * sumnode / nodecnt;
		suml = 0.0;
		sumr = sumnode;
		npopl = 0;
		npopr = nodecnt;
		crit = 0.0;
		/* Search through the "gaps" in the x-variable. */
		for (j = ndstart; j <= ndend - 1; ++j) {
			d = yl[ncase[j] - 1];
			suml += d;
			sumr -= d;
			npopl++;
			npopr--;
			if (v[j] < v[j+1]) {
				crit = (suml * suml / npopl) + (sumr * sumr / npopr) -
					critParent;
				if (crit > critvar) {
					ubestt = (v[j] + v[j+1]) / 2.0;
					critvar = crit;
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
		}
    
    }   /* end for loop over selected variables */
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
    Free(weights);
    Free(sumweights);
    
    
}

