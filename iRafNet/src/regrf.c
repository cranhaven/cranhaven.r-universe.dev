/*******************************************************************
   Copyright (C) 2001-2012 Leo Breiman, Adele Cutler and Merck & Co., Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
*******************************************************************/

#include <R.h>
#include "rf.h"

void simpleLinReg(int nsample, double *x, double *y, double *coef,
		  double *mse, int *hasPred);


void regRF(double *x, double *y, int *xdim, int *sampsize,
	   int *nthsize, int *nrnodes, int *nTree, int *mtry, int *imp,
	   int *cat, int *maxcat, int *jprint, int *doProx, int *oobprox,
           int *biasCorr, double *yptr, double *errimp, double *impmat,
           double *impSD, double *prox, int *treeSize, int *nodestatus,
           int *lDaughter, int *rDaughter, double *avnode, int *mbest,
           double *upper, double *mse, int *keepf, int *replace,
           int *testdat, double *xts, int *nts, double *yts, int *labelts,
           double *yTestPred, double *proxts, double *msets, double *coef,
           int *nout, int *inbag, double *Sw) {
    /*************************************************************************
   Input:
   mdim=number of variables in data set
   nsample=number of cases

   nthsize=number of cases in a node below which the tree will not split,
   setting nthsize=5 generally gives good results.

   nTree=number of trees in run.  200-500 gives pretty good results

   mtry=number of variables to pick to split on at each node.  mdim/3
   seems to give genrally good performance, but it can be
   altered up or down

   imp=1 turns on variable importance.  This is computed for the
   mth variable as the percent rise in the test set mean sum-of-
   squared errors when the mth variable is randomly permuted.

  *************************************************************************/

    double errts = 0.0, averrb, meanY, meanYts, varY, varYts, r, xrand,
	errb = 0.0, resid=0.0, ooberr, ooberrperm, delta, *resOOB;

    double *yb, *xtmp, *xb, *ytr, *ytree, *tgini, *sw;

    int k, m, mr, n, nOOB, j, jout, idx, ntest, last, ktmp, nPerm,
        nsample, mdim, keepF, keepInbag,mindo;
    int *oobpair, varImp, localImp, *varUsed;

    int *in, *nind, *nodex, *nodexts;

    nsample = xdim[0];
    mdim = xdim[1];
    ntest = *nts;
    varImp = imp[0];
    localImp = imp[1];
    nPerm = imp[2];
    keepF = keepf[0];
    keepInbag = keepf[1];

    if (*jprint == 0) *jprint = *nTree + 1;

    sw         = (double *) S_alloc(mdim, sizeof(double));
    yb         = (double *) S_alloc(*sampsize, sizeof(double));
    xb         = (double *) S_alloc(mdim * *sampsize, sizeof(double));
    ytr        = (double *) S_alloc(nsample, sizeof(double));
    xtmp       = (double *) S_alloc(nsample, sizeof(double));
    resOOB     = (double *) S_alloc(nsample, sizeof(double));

    in        = (int *) S_alloc(nsample, sizeof(int));
    nodex      = (int *) S_alloc(nsample, sizeof(int));
    varUsed    = (int *) S_alloc(mdim, sizeof(int));
    nind = *replace ? NULL : (int *) S_alloc(nsample, sizeof(int));

    for (j = 0; j < mdim; ++j) {
        sw[j]=Sw[j];
    }
    if (*testdat) {
	ytree      = (double *) S_alloc(ntest, sizeof(double));
	nodexts    = (int *) S_alloc(ntest, sizeof(int));
    }
    oobpair = (*doProx && *oobprox) ?
	(int *) S_alloc(nsample * nsample, sizeof(int)) : NULL;

    /* If variable importance is requested, tgini points to the second
       "column" of errimp, otherwise it's just the same as errimp. */
    tgini = varImp ? errimp + mdim : errimp;

    averrb = 0.0;
    meanY = 0.0;
    varY = 0.0;

    zeroDouble(yptr, nsample);
    zeroInt(nout, nsample);
    for (n = 0; n < nsample; ++n) {
	varY += n * (y[n] - meanY)*(y[n] - meanY) / (n + 1);
	meanY = (n * meanY + y[n]) / (n + 1);
    }
    varY /= nsample;

    varYts = 0.0;
    meanYts = 0.0;
    if (*testdat) {
	for (n = 0; n < ntest; ++n) {
	    varYts += n * (yts[n] - meanYts)*(yts[n] - meanYts) / (n + 1);
	    meanYts = (n * meanYts + yts[n]) / (n + 1);
	}
	varYts /= ntest;
    }

    if (*doProx) {
        zeroDouble(prox, nsample * nsample);
	if (*testdat) zeroDouble(proxts, ntest * (nsample + ntest));
    }

    if (varImp) {
        zeroDouble(errimp, mdim * 2);
	if (localImp) zeroDouble(impmat, nsample * mdim);
    } else {
        zeroDouble(errimp, mdim);
    }
    if (*labelts) zeroDouble(yTestPred, ntest);

    /* print header for running output */
    if (*jprint <= *nTree) {
	Rprintf("     |      Out-of-bag   ");
	if (*testdat) Rprintf("|       Test set    ");
	Rprintf("|\n");
	Rprintf("Tree |      MSE  %%Var(y) ");
	if (*testdat) Rprintf("|      MSE  %%Var(y) ");
	Rprintf("|\n");
    }
    GetRNGstate();
    /*************************************
     * Start the loop over trees.
     *************************************/
    for (j = 0; j < *nTree; ++j) {
		idx = keepF ? j * *nrnodes : 0;
		zeroInt(in, nsample);
        zeroInt(varUsed, mdim);
        /* Draw a random sample for growing a tree. */
		if (*replace) { /* sampling with replacement */
			for (n = 0; n < *sampsize; ++n) {
				xrand = unif_rand();
				k = xrand * nsample;
				in[k] = 1;
				yb[n] = y[k];
				for(m = 0; m < mdim; ++m) {
					xb[m + n * mdim] = x[m + k * mdim];
				}
			}
		} else { /* sampling w/o replacement */
			for (n = 0; n < nsample; ++n) nind[n] = n;
			last = nsample - 1;
			for (n = 0; n < *sampsize; ++n) {
				ktmp = (int) (unif_rand() * (last+1));
                k = nind[ktmp];
                swapInt(nind[ktmp], nind[last]);
				last--;
				in[k] = 1;
				yb[n] = y[k];
				for(m = 0; m < mdim; ++m) {
					xb[m + n * mdim] = x[m + k * mdim];
				}
			}
		}
		if (keepInbag) {
			for (n = 0; n < nsample; ++n) inbag[n + j * nsample] = in[n];
		}
        /* grow the regression tree */
		regTree(xb, yb, mdim, *sampsize, lDaughter + idx, rDaughter + idx,
                upper + idx, avnode + idx, nodestatus + idx, *nrnodes,
                treeSize + j, *nthsize, *mtry, mbest + idx, cat, tgini,
                varUsed, sw);

		/*  DO PROXIMITIES */

		/* Variable importance */
	  }
    /* end of tree iterations=======================================*/

    for (m = 0; m < mdim; ++m) tgini[m] /= *nTree;
    
    
  
}

/*----------------------------------------------------------------------*/
