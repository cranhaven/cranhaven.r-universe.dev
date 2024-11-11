/*******************************************************************
   Copyright (C) 2001-2015 Leo Breiman, Adele Cutler and Merck & Co., Inc.

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
#include <math.h>
//#define RF_EXPT

void survRF(double *x, double *LR, double *prob, int *xdim, double *timepoints,
            double *timepointsSm,
            double *tau, int *rinf, int *tinf, int *tinfSm, int *sampsize,
      	   int *nthsize, int *nrnodes, int *nTree, int *mtry, int *nfold, int *imp,
      	   int *cat, int *maxcat, int *jprint, int *doProx, int *oobprox,
      	   int *udNPMLE, int *returnBest, int * ibsType, int *BestFold,
           double *survPTr, double *survPTrNO, double *survPTrNOSm, double *errimp, double *impmat,
           double *impSD, double *prox, int *treeSize, int *nodestatus,
           int *lDaughter, int *rDaughter, double *avnode, double *avnodeSm, int *mbest,
           double *upper, double *ibs, double *ibsNO, int *method, int *ert, int *uniErt,
           int *keepf, int *replace,
           int *testdat, double *bw, double *xts, int *nts, double *yts,
           int *labelts, double *sTestPred, double *proxts, double *interrts,
           int *nout, int *inbag, int *keepIB) {
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

   method = 1 "Wilcoxon", 2 "logrank", 3 "PetoLogrank", 4 "PetoWilcoxon"

  *************************************************************************/
    double xrand, ooberr1,  ooberr2, ooberrperm1, ooberrperm2,
    delta1 = 0.0, delta2 = 0.0, sampleSum, *ibsN;

    double *probb, *xtmp, *xb, *survTr, *survTrSm, *stree, *tgini,
    *probCum, *pIndexMat, *pIndexMatb,
    *lrb, *pf, *intmap, *peto, *petob, avTime,
    *survts, *timediff, *timediffSm, *probTmpSm, *probTmpSm2, *timepoints2, cdfAtTau;
    //avnodeSmCum, multiplier; //*surv, *survSm,
    //double *yp2, ypCum = 0.0, yp2Cum = 0.0, ratio = 1.0, cdfAtTau; //non-smoothed vector with the same gridline.

    int k, m, mr, n, t, nOOB, j, idx, ntest, last, ktmp, nPerm,
        nNAN1, nNAN2, nNAN1b, nNAN2b,  nNAN1bPerm, nNAN2bPerm,
        fold, cur, timeInc, timeInc2, ntime2,
        nsample, mdim, ntime, ntimeSm, keepF, keepInbag, nt; //nt = 1 if keepf = FALSE, ow = ntree
    int *oobpair, varImp, *varUsed; //localImp omitted.

    int *in, *nind, *nodex, *nodexts, *kdex, debug = 0;

    // declaring the best result placeholder (only when *returnBest = TRUE)
    double *BsurvPTr, *BsurvPTrNO, *BsurvPTrNOSm, *Bprox; // *Berrimp, *Bimpmat, *BimpSD,
    int *Bnodestatus, *BlDaughter, *BrDaughter, *Bmbest;
    int *BtreeSize, *Bnout, *Binbag;
    double *Bavnode, *BavnodeSm, *Bupper, *BsTestPred, *Bproxts;
    double imseCurrent, imseBest = 1.0/0.0;

    nsample = xdim[0];
    mdim    = xdim[1];
    ntime   = xdim[2];
    ntimeSm = xdim[3];
    nt      = xdim[4];
    ntest = *nts;
    varImp = imp[0];
    //localImp = imp[1];
    nPerm = imp[2];
    keepF = keepf[0];
    keepInbag = keepf[1];

/*double cumm = 0.0;
Rprintf("9th subject's cum prob: \n");
for (t=0; t < ntime; ++t) {
  cumm += prob[t + 9 * ntime];
  Rprintf("%2.2f ", cumm);
}
Rprintf("\n");*/


/*Rprintf("nodepred: \n");
for (n=0; n < 2; ++n) {
  Rprintf("\nTree %d: ", n + 1);
  for (cur = 0; cur < 10; ++cur) {
    Rprintf("\nnode %d: ", cur + 1);
    for (t=0; t < ntime; ++t) {
      Rprintf("%2.2f ", avnode[t + cur* ntime + n * *nrnodes * ntime]);
    }
  }
}*/

Rprintf("\n");

    if (*jprint == 0) *jprint = *nTree + 1;

    probb      = (double *) S_alloc(ntime * *sampsize, sizeof(double));
    xb         = (double *) S_alloc(mdim * *sampsize, sizeof(double));
    survTr        = (double *) S_alloc(nsample * ntime, sizeof(double));
    survTrSm      = (double *) S_alloc(nsample * ntimeSm, sizeof(double));
    xtmp       = (double *) S_alloc(nsample, sizeof(double));
    ibsN       = (double *) S_alloc(nsample * 2, sizeof(double));
    //surv       = (double *) S_alloc(nsample * ntime, sizeof(double));
    //survSm     = (double *) S_alloc(nsample * ntimeSm, sizeof(double));
    timediff   = (double *) S_alloc(ntime - 1, sizeof(double));
    timediffSm = (double *) S_alloc(ntimeSm - 1, sizeof(double));
    //yp2        = (double *) S_alloc(ntimeSm, sizeof(double));

    //if *returnBest = TRUE, then keep record at the best iter, and plug them back in the end.
    BsurvPTr = (double *) S_alloc(*returnBest ? nsample * ntime : 1, sizeof(double));
    BsurvPTrNO = (double *) S_alloc(*returnBest ? nsample * ntime : 1, sizeof(double));
    BsurvPTrNOSm = (double *) S_alloc(*returnBest ? nsample * ntimeSm : 1, sizeof(double));
    Bavnode = (double *) S_alloc(*returnBest ? ntime * *nrnodes * nt : 1, sizeof(double));
    BavnodeSm = (double *) S_alloc(*returnBest ? ntimeSm * *nrnodes * nt : 1, sizeof(double));
    Bprox = (double *) S_alloc(*returnBest && *doProx ? nsample * nsample : 1, sizeof(double));
    Bupper = (double *) S_alloc(*returnBest ? *nrnodes * nt : 1, sizeof(double));
    Bnodestatus = (int *) S_alloc(*returnBest ? *nrnodes * nt : 1, sizeof(int));
    BlDaughter = (int *) S_alloc(*returnBest ? *nrnodes * nt : 1, sizeof(int));
    BrDaughter = (int *) S_alloc(*returnBest ? *nrnodes * nt : 1, sizeof(int));
    Bmbest = (int *) S_alloc(*returnBest ? *nrnodes * nt : 1, sizeof(int));
    BtreeSize = (int *) S_alloc(*returnBest ? nt : 1, sizeof(int));
    Bnout = (int *) S_alloc(*returnBest ? nsample : 1, sizeof(int));
    Binbag = (int *)
      S_alloc(*returnBest && *keepIB ? nsample * *nTree : 1, sizeof(int));
    BsTestPred = (double *)
      S_alloc(*returnBest && *testdat ? ntest * ntimeSm : 1, sizeof(double));
    Bproxts = (double *)
      S_alloc(*returnBest && *doProx && testdat ? ntest * (nsample + ntest) : 1, sizeof(double));

    /* properly handling inf in C */
    for (n = 0; n < nsample; ++n) if (rinf[n] == 1) LR[nsample + n] = 1.0 /0.0;
    for (t = 0; t < ntime; ++t) if (tinf[t] == 1) timepoints[t] = 1.0 /0.0;
    for (t = 1; t < ntime; ++t) timediff[t-1] = timepoints[t] - timepoints[t-1];

    for (t = 0; t < ntimeSm; ++t) if (tinfSm[t] == 1) timepointsSm[t] = 1.0 /0.0;
    for (t = 1; t < ntimeSm; ++t) timediffSm[t-1] = timepointsSm[t] - timepointsSm[t-1];

    // last timepoint is only upto tau for timediff (for ibs calculation)
    if (ntime > 1) timediff[ntime - 2] = *tau - timepoints[ntime - 2];
    if (ntimeSm > 1) timediffSm[ntimeSm - 2] = *tau - timepointsSm[ntimeSm - 2];

    //if (*smooth2) {  /* If smoothing, ... */
      //timeInc = (int)(ntime * 0.2);
      //timeInc = (int)(*bw * 300.0/ * tau);    //for head
      //timeInc2 = *tau > 10.0? (int) *tau: 10; //for tail
      //avTime = *tau / ((ntime - 2) * 1.0);
      timeInc = ntime - 2;
      for (t = 1; t < ntime - 2; t++) {
        if (timepoints[t] > *bw * 4) {
          timeInc = t + 1;
          break;
        }
      }
      avTime = *tau < ntime * 0.1? *tau/ntime : 0.1;
      timeInc2 = (int) (*bw * 4 / avTime);

      ntime2 = ntime + timeInc + timeInc2;
      timepoints2   = (double *) S_alloc(ntime2, sizeof(double));
      probTmpSm2   = (double *) S_alloc(ntime2, sizeof(double));
      probTmpSm   = (double *) S_alloc(ntimeSm, sizeof(double));
      //avTime = *bw;
      for (t = 0; t < ntime - 1; ++t){   // extending the window for smoothing
        timepoints2[t + timeInc] = timepoints[t];
        probTmpSm2[t + timeInc] = 0.0;
      }
      for (t = 0; t <= timeInc; ++t){  // extending the window for smoothing
        timepoints2[timeInc - t] = -timepoints[t];
        probTmpSm2[timeInc - t] = 0.0;
      }
      for (t = 0; t <= timeInc2; ++t){  // extending the window for smoothing
        timepoints2[(ntime - 1) + timeInc + t] = timepoints2[(ntime - 1) + timeInc - 1] + avTime * (t + 1);
        probTmpSm2[ntime - 1 + t] = 0.0;
      }

    //} else {  /* If not smoothing, ... */
    //  timepoints2 = NULL;
    //  probTmpSm   = NULL;
    //  probTmpSm2   = NULL;
    //}
    //avnode = ntime x nrnodes x ntree
    //probTmpSm = ntime

    in         = (int *) S_alloc(nsample, sizeof(int));
    nodex      = (int *) S_alloc(nsample, sizeof(int));
    varUsed    = (int *) S_alloc(mdim, sizeof(int));
    nind = *replace ? NULL : (int *) S_alloc(nsample, sizeof(int));


  	stree      = (double *) S_alloc(*testdat? ntest * ntimeSm : 1, sizeof(double));
  	nodexts    = (int *) S_alloc(*testdat? ntest : 1, sizeof(int));
    survts     = (double *) S_alloc(*labelts? ntest * ntimeSm : 1, sizeof(double));
    oobpair = (*doProx && *oobprox) ?
	  (int *) S_alloc(nsample * nsample, sizeof(int)) : NULL;


    /* probability index calculation */
    probCum =
      (double *) S_alloc(*method == 1 ? ntime * nsample: 1, sizeof(double));
    pIndexMat =
      (double *) S_alloc(*method == 1 ? nsample * nsample: 1, sizeof(double));
    pIndexMatb =
      (double *) S_alloc(*method == 1 ? *sampsize * *sampsize: 1, sizeof(double));
    kdex = (int *) S_alloc(*sampsize, sizeof(int));
    lrb =
      (double *) S_alloc(*udNPMLE ? 2 * *sampsize : 1, sizeof(double));
    intmap =
      (double *) S_alloc(*udNPMLE ? 2 * *sampsize : 1, sizeof(double));
    pf =
      (double *) S_alloc(*udNPMLE ? *sampsize : 1, sizeof(double));
    peto =
      (double *) S_alloc(*method >= 3 ? nsample : 1, sizeof(double)); // 3, 4: Peto
    petob =
      (double *) S_alloc(*method >= 3 ? *sampsize : 1, sizeof(double)); // 3, 4: Peto


    /* If variable importance is requested, tgini points to the second
       "column" of errimp, otherwise it's just the same as errimp. */
    tgini = varImp ? errimp + 2 * mdim * *nfold : errimp;


    zeroDouble(ibs, *nfold); //zeroDouble(ibs, 1); //*ibs = 0.0;
    if (varImp) {
      zeroDouble(errimp, mdim * 3 * *nfold);
      //if (localImp) zeroDouble(impmat, nsample * mdim);
    } else {
      zeroDouble(errimp, mdim * *nfold);
    }

    GetRNGstate();
    /**************************************************************
    * Start the loop over FORESTS.
    **************************************************************/
for (fold = 0; fold < *nfold; ++fold) {
  Rprintf("Forest loop %d out of %d folds\n", fold + 1, *nfold);

    if (*method == 1) {
      zeroDouble(probCum, ntime * nsample);
      zeroDouble(pIndexMat, nsample * nsample);
      zeroDouble(pIndexMatb, *sampsize * *sampsize);

      // time-cumulated probability - 1/2 prob.j
      for (n = 0; n < nsample; ++n) {
        probCum[0 + n * ntime] = prob[0 + n * ntime];
        for (t = 1; t < ntime; ++t) {
          probCum[t + n * ntime] = probCum[t - 1 + n * ntime] + prob[t + n * ntime];
        }
        for (t = 0; t < ntime; ++t) { // This should NOT be combined with previous procedure.
          probCum[t + n * ntime] -= prob[t + n * ntime] /2.0;
        }
      }

      // calculating p-index for all i-j pairs
      for (n = 0; n < nsample; ++n) {
        pIndexMat[n + nsample * n] = 0.5;
        for (t = n + 1; t < nsample; ++t) {
          /* updating pIndexMat */
          pIndex (prob + n * ntime, probCum + t * ntime,
                  pIndexMat + t + n * nsample, ntime);
          pIndexMat[n + t * nsample] = 1 - pIndexMat[t + n * nsample];
        }
      }
      /*
      Rprintf("prob matrix \n");
      for (n = 0; n < 5; ++n) {
      for (t = 0; t < 10; ++t) {
      Rprintf("%f ",  prob[t + n * ntime]);
      }
      Rprintf("\n");
      }
      Rprintf("prob cum matrix \n");
      for (n = 0; n < 5; ++n) {
      for (t = 0; t < 10; ++t) {
      Rprintf("%f ",  probCum[t + n * ntime]);
      }
      Rprintf("\n");
      }


      Rprintf("p-index matrix \n");
      for (n = 0; n < 5; ++n) {
      for (t = 0; t < 5; ++t) {
      Rprintf("%f    ", pIndexMat[n + t * nsample]);
      }
      Rprintf("\n");
      }
      Rprintf("lr matrix \n");
      for (n = 0; n < 5; ++n) {
      Rprintf("(%f %f)\n",  LR[n],  LR[nsample + n]);
      }
      */

    } else {
      *probCum = 0.0;
      *pIndexMat = 0.0;
      *pIndexMatb = 0.0;
      *kdex = 0;
    }

    if (*method >= 3) { //Peto's log-rank (3) or WRS (4) test
      zeroDouble(peto, nsample);
      zeroDouble(petob, *sampsize);
      //finding marginal survival fn for the first forest loop.
      /* Not needed any more when having survPTrNO instead of yptrNO.
      for (n = 0; n < nsample; ++n) {
        surv[n + 0 * nsample] = 1.0 - survPTrNO[n + 0 * nsample];  //t == 0
        for (t = 1; t < ntime; ++t) {
          surv[n + t * nsample] = surv[n + (t-1) * nsample] - survPTrNO[n + t * nsample];
        }
      } */
      PETO(survPTrNO, LR, timepoints, nsample, ntime, peto, *method - 2); // 3 = PetoLR, 4 = PetoWRS
/*Rprintf("survPTrNO \n");
for(n = 0; n < 10; ++n) for(t = 0; t < ntime; ++t)  Rprintf("%2.2f ", survPTrNO[n + t * nsample]);
Rprintf("\n");
Rprintf("Peto stat\n");
for(n = 0; n < nsample; ++n) Rprintf("%2.2f ", peto[n]);
Rprintf("\n");*/
    } else {
      *peto = 0.0;
      *petob = 0.0;
    }

    // Initializing the forest after saving some information
    zeroDouble(survPTrNO, nsample * ntime); //update peto previously and initialize surv!
    zeroDouble(survPTrNOSm, nsample * ntimeSm); //update peto previously and initialize surv!
    //zeroDouble(surv, nsample * ntime);   //update peto previously and initialize surv!
    zeroDouble(survPTr, nsample * ntime);
    zeroDouble(avnode, ntime * *nrnodes * nt);
    zeroDouble(avnodeSm, ntimeSm * *nrnodes * nt);
    zeroInt(nodestatus, *nrnodes * nt);
    zeroInt(lDaughter, *nrnodes * nt);
    zeroInt(rDaughter, *nrnodes * nt);
    zeroInt(mbest, *nrnodes * nt);
    zeroDouble(upper, *nrnodes * nt);


    zeroInt(nout, nsample);
    nNAN1b = 0; //number of trees whose ooberror was not measured for any oob sample.
    nNAN2b = 0;
    nNAN1bPerm = 0; //number of trees whose ooberror was not measured for any oob sample - Permutation test.
    nNAN2bPerm = 0;

    if (*udNPMLE) {
      zeroDouble(lrb, 2 * *sampsize);  // unlike LR (n x 2), lrb is (2 x *sampsize)!!
      zeroDouble(intmap, *sampsize * 2);
      zeroDouble(pf, *sampsize);
    } else {
      *lrb = 0.0;
      *intmap = 0.0;
      *pf = 0.0;
    }

    if (*doProx) {
        zeroDouble(prox, nsample * nsample);
	      if (*testdat) zeroDouble(proxts, ntest * (nsample + ntest));
    }

    if (*testdat) zeroDouble(sTestPred, ntest * ntimeSm);
    if (*labelts) zeroDouble(survts, ntest * ntimeSm);

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
Rprintf("Tree ");
    for (j = 0; j < *nTree; ++j) {
Rprintf("%d ", j+1);
  		idx = keepF ? j * *nrnodes : 0;
  		zeroInt(in, nsample);
      zeroInt(varUsed, mdim);
      zeroInt(kdex, *sampsize);

        /* Draw a random sample for growing a tree. */
		if (*replace) { /* sampling with replacement */
			for (n = 0; n < *sampsize; ++n) {
				xrand = unif_rand();
				k = xrand * nsample;
				in[k] += 1;
				kdex[n] = k; // actually needed only for Wilcoxon
				if (*udNPMLE) {
				  lrb[2 * n] = LR[k];     //dimension flipped from LR into lrb!
				  lrb[2 * n + 1] = LR[k + nsample];
				}
				if (*method >= 3) {
				  petob[n] = peto[k];
				}
				for (t = 0; t < ntime; ++t) {
				  probb[t + n * ntime] = prob[t + k * ntime];
				}
				for(m = 0; m < mdim; ++m) {
					xb[m + n * mdim] = x[m + k * mdim];
				}
			}
		} else { /* sampling w/o replacement */
			for (n = 0; n < nsample; ++n) nind[n] = n;
			last = nsample - 1;
//Rprintf("nsample = %d, sampsize = %d\n n = ", nsample, *sampsize);
			for (n = 0; n < *sampsize; ++n) {
//Rprintf(" %d", n);
				ktmp = (int) (unif_rand() * (last+1));
                k = nind[ktmp];
                swapInt(nind[ktmp], nind[last]);
#ifdef RF_EXPT
    Rprintf("Warning-locked-354 ");
    k = n;
#endif
				last--;
				in[k] += 1;
				kdex[n] = k; // actually needed only for Wilcoxon
				for (t = 0; t < ntime; ++t) {
				  probb[t + n * ntime] = prob[t + k * ntime];
				}
				for(m = 0; m < mdim; ++m) {
					xb[m + n * mdim] = x[m + k * mdim];
				}
				if (*method >= 3) {
				  petob[n] = peto[k];
				}
				if (*udNPMLE) {
				  lrb[2 * n] = LR[k];     //dimension flipped from LR into lrb!
				  lrb[2 * n + 1] = LR[k + nsample];
				}
			}
		}

		if (*method == 1) {
		  for (n = 0; n < *sampsize; ++n) {
	      for (t = n; t < *sampsize; ++t) {
          pIndexMatb[n + t * *sampsize] =
            pIndexMat[kdex[n] + kdex[t] * nsample];
          pIndexMatb[t + n * *sampsize] =
            pIndexMat[kdex[t] + kdex[n] * nsample];
	      }
		  }

/* if (j==0) {
  Rprintf("p-index matrix \n");
  for (n = 0; n < 8; ++n) {
    for (t = 0; t < 8; ++t) {
      Rprintf("%f    ", pIndexMat[n + t * nsample]);
    }
    Rprintf("\n");
  }
  Rprintf("kdex \n");
  for (n = 0; n < 100; ++n) {
    Rprintf("%d    ", kdex[n]);
  }
  Rprintf("\n");
  Rprintf("p-index reorganized matrix \n");
  for (n = 0; n < 8; ++n) {
    for (t = 0; t < 8; ++t) {
      Rprintf("%f    ", pIndexMat[kdex[n] + kdex[t] * nsample]);
    }
    Rprintf("\n");
  }
  Rprintf("\np-index-b matrix \n");
  for (n = 0; n < 30; ++n) {
    for (t = 0; t < 8; ++t) {
      Rprintf("%f    ", pIndexMatb[n + t * *sampsize]);
    }
    Rprintf("\n");
  }
}*/

		}

		if (keepInbag) {
			for (n = 0; n < nsample; ++n) inbag[n + j * nsample] = in[n];
		}

//if (fold == 0 && j == 137) debug = 1; else debug = 0;
    /* grow the regression tree */
		survTree(xb, probb, pIndexMatb, petob, mdim, ntime, *sampsize,
             lDaughter + idx, rDaughter + idx,
             upper + idx, avnode + idx*ntime, nodestatus + idx, *nrnodes,
             treeSize + j, *nthsize, *mtry, mbest + idx, cat, tgini,
             varUsed, *method, *ert, *uniErt,
             *udNPMLE, lrb, pf, intmap, timepoints, tau, bw, fold, debug);

		// kernel smoothing.
		//if (*smooth2) {
		  for (cur = 0; cur < *nrnodes; ++cur) {
		    if (nodestatus[cur + idx] == NODE_TERMINAL) {
//Rprintf("\n(probTmpSm2, avnode) = \n");
//if (cur < 3) Rprintf("\n(cur = %d) \n", cur);
		      // probTmpSm2 is CDF with extended time frame for smoothing.
		      for (t = 0; t < ntime - 1; ++t) {
		        // < ntime - 1 b/c Inf not included but only upto tau.
		        // >= 1 b/c at t0 prob is always 0, but it needs to be replaced with some prob btw t0 and t1.
		        probTmpSm2[timeInc + t + 1] = avnode[t + cur* ntime + idx * ntime]/(timediff[t] < 0.0001? 10000.0: timediff[t]);
//if (cur == 93) Rprintf("%d: (%2.3f, %2.3f)\n", t, probTmpSm2[t + timeInc], avnode[t + cur* ntime + idx * ntime]);
//if (cur == 93) Rprintf("%d: (%2.3f, %2.3f) ", timeInc + t + 1, probTmpSm2[timeInc + t + 1], avnode[t + cur* ntime + idx * ntime]);
//if (cur == 93) Rprintf(" (%2.3f)\n", probTmpSm2[timeInc + t + 1] - avnode[t + cur* ntime + idx * ntime]);
//if (cur == 93) Rprintf("%d:  (%2.3f, %2.3f, %2.3f) ",timeInc + t + 1 ,  probTmpSm2[timeInc + t + 1], avnode[t + cur* ntime + idx * ntime], timediff[t]);
		      }
//Rprintf("\n fore-mirror\n");
		      for (t = 0; t < timeInc + 1; ++t) { // only updated timeInc - 1 (max_timeInc = ntime -2) b/c prob[0] is not needed.
		        probTmpSm2[timeInc - t] = avnode[t + cur* ntime + idx * ntime]/(timediff[t] < 0.0001? 10000.0: timediff[t]);
//if (cur == 93 & t < 10 | t > timeInc - 3) Rprintf("%d:  (%2.3f, %2.3f, %2.3f) ", timeInc - t,  probTmpSm2[timeInc - t], avnode[t + cur* ntime + idx * ntime], timediff[t]);
		      }
//Rprintf("\n hind-mirror\n");
		      double lambda = - *tau / log(avnode[ntime - 1 + cur* ntime + idx * ntime]);
		      if (avnode[ntime - 1 + cur* ntime + idx * ntime] < 1e-10) lambda = 0.0; // when terminal time probability is negative, it gives nan. Force it to zero. Resulting probTmpSm2 = 0.0.

		      for (t = timeInc + ntime; t < timeInc + ntime + timeInc2; ++t) {
		        //  probTmpSm2[t] = avnode[2 * ntime - 3 - t + timeInc + cur* ntime + idx * ntime];
		        probTmpSm2[t] = (exp(- timepoints2[t - 1]/lambda) -  exp(- timepoints2[t]/lambda))/avTime;
		      }
          cdfAtTau = 1.0 - avnode[ntime - 1 + cur* ntime + idx * ntime]; // needed for precision
		      // probTmpSm2[t+timeInc] * timediff[t-1]; is not precise and makes gradual deviation.
		      // This makes genuine CDF (CDF at tau = 1.0) not ending up with 1 but something like 0.94.
		      // The resulting smoothed curve is thus inflated.
		      // Alternative is using long double instead of double for everything involved.

/*if (cur == 59) {
  Rprintf("\n\navnode before smoothing\n");
  Rprintf("timepoints2,  probTmpSm2, avnode,  cumTmp, cumAvnode\n");
  double cumTmp = 0.0, cumAvnode = 0.0;
  //for (t = 0; t < ntime2; ++t) {
  for (t = timeInc; t < timeInc + ntime + 2 & timepoints2[t] < 2; ++t) {
    cumTmp += probTmpSm2[t] * timediff[t-1-timeInc];
    cumAvnode += avnode[t-timeInc + cur* ntime + idx * ntime];
    Rprintf("%2.3f,   %2.3f,  %2.3f,  %2.3f,  %2.3f  \n",
            timepoints2[t], probTmpSm2[t], avnode[t-timeInc + cur* ntime + idx * ntime],
            cumTmp, cumAvnode);
  }
  Rprintf("avnode last = %2.4f\n", avnode[ntime - 1 + cur* ntime + idx * ntime]);
} */

// This if cur==3 should be removed!!!!!!!!!!!
// if (cur == 2)
//Rprintf("ntime2 = %d, ntime = %d, timepoints2[0] = %2.3f, timepoint2[ntime2] = %2.3f ",
//        ntime2, ntime, timepoints2[0], timepoints2[ntime2-1]);
          //BDRksmooth(timepoints2, probTmpSm2, ntime2, timepoints, probTmpSm, ntime, *bw);
          ksmooth(timepoints2, probTmpSm2, ntime2, timepointsSm, probTmpSm, ntimeSm, *bw, 1, cdfAtTau);
//Rprintf("cur = %d\n", cur);
//if (cur >= 59) return;
//if (cur == 59) {
//  Rprintf("\n\n\n avnode after smoothing = ");
//  //for (t = 0; t < ntime2; ++t) {
//  for (t = 0; t < ntimeSm; ++t) {
//    Rprintf("(%2.3f: %2.2f)  ",timepointsSm[t], probTmpSm[t]);
//  }
//  Rprintf("\n");
//}
          for (t = 1; t < ntimeSm - 1; ++t) {
            avnodeSm[t + cur * ntimeSm + idx * ntimeSm] = probTmpSm[t] * timediffSm[t-1];
          }

          // filling in the residual (the last piece)
          avnodeSm[ntimeSm - 1 + cur * ntimeSm + idx * ntimeSm] = 1.0;
          for (t = 1; t < ntimeSm - 1; ++t) {
            avnodeSm[ntimeSm - 1 + cur * ntimeSm + idx * ntimeSm] -=
              avnodeSm[t + cur * ntimeSm + idx * ntimeSm]; //yp[last] is the residual.
          }


          //normalizing probs when smoothing does not sum up to 1.
          /*multiplier = avnodeSmCum / (1.0 - avnode[ntime - 1 + cur* ntime + idx * ntime]);
          if (isfinite(multiplier)) {
            for (t = 1; t < ntimeSm - 1; ++t) {
              avnodeSm[t + cur * ntimeSm + idx * ntimeSm] /= multiplier;
            }
          } else {
            avnodeSm[t + cur * ntimeSm + idx * ntimeSm] = 0.0;
          }
          // probability at tau.
          avnodeSm[ntimeSm - 1 + cur * ntimeSm + idx * ntimeSm] = avnode[ntime - 1 + cur* ntime + idx * ntime]; */
		    }
		  }
		//}
// Rprintf("\nntime %d, ntime2 %d \n", ntime, ntime2);
        /* predict the OOB data with the current tree */
		/* survTr is the prediction on OOB data by the current tree */
//Rprintf("\navnode = ");
//for (t = 0; t < 15; t++) Rprintf("%2.3f ", avnode[t]);
		predictSurvTree(x, nsample, mdim, ntime, lDaughter + idx,
                    rDaughter + idx, nodestatus + idx, survTr, upper + idx,
                    avnode + idx*ntime, mbest + idx, treeSize[j], cat, *maxcat,
                    nodex, nrnodes,
                    ntimeSm, survTrSm, avnodeSm + idx * ntimeSm, 1); // needs to be corrected!!!!!!!
		/* survPTr is the aggregated prediction by all trees grown so far (OOB) */
//Rprintf("\nsurvTr = ");
//for (t = 0; t < 15; t++) Rprintf("%2.3f ", survTr[t]);

/*Rprintf("nodex\n");
for (n = 0; n < nsample; ++n) {
  Rprintf("%d ", nodex[n]);
}
Rprintf("nodex\n");*/


		/* Not needed any more when having survTr instead of ytr.
		for (n = 0; n < nsample; ++n) {
		  surv[n + 0 * nsample] = 1.0 - survTr[n + 0 * nsample];  //t == 0
		  for (t = 1; t < ntime; ++t) {
		    surv[n + t * nsample] = surv[n + (t-1) * nsample] - survTr[n + t * nsample];
		    //surv and survTr (=ypred) are both nsample x ntime
		  }
		}
		for (n = 0; n < nsample; ++n) {
		  survSm[n + 0 * nsample] = 1.0 - survTrSm[n + 0 * nsample];  //t == 0
		  for (t = 1; t < ntimeSm; ++t) {
		    survSm[n + t * nsample] = survSm[n + (t-1) * nsample] - survTrSm[n + t * nsample];
		    //survSm and survTrSm (=ypred) are both nsample x ntimeSm
		  }
		} */
		/* tree-level prediction using oob sample */
		ibss(LR, survTrSm, timepointsSm, timediffSm, in, nsample, ntimeSm, ibsN, 1, tau);  //updating ibsN

		ooberr1 = 0.0;
		ooberr2 = 0.0;
		//jout = 0; /* jout is the number of cases that has been OOB so far */
		nOOB = 0; /* nOOB is the number of OOB samples for this tree */
		nNAN1 = 0;
		nNAN2 = 0;
		for (n = 0; n < nsample; ++n) {
			if (in[n] == 0) {
				nout[n]++;
        nOOB++;
        for (t = 0; t < ntime; ++t) {
          survPTr[n + t * nsample] = ((nout[n]-1) * survPTr[n + t * nsample] + survTr[n + t * nsample]) / nout[n];
        }
        if (!isnan(ibsN[n])) {
          ooberr1 += ibsN[n];
        } else {
          nNAN1++;
        }
        if (!isnan(ibsN[n + nsample])) {
          ooberr2 += ibsN[n + nsample];
        } else {
          nNAN2++;
        }
			}
		}
		if (nOOB - nNAN1 > 0) {
		  ibs[fold] += ooberr1/ (nOOB - nNAN1);
		} else {
		  nNAN1b++;
		}
		if (nOOB - nNAN2 > 0) {
		  ibs[fold + *nfold] += ooberr2/ (nOOB - nNAN2);
		} else {
		  nNAN2b++;
		}

		//Non-OOB prediction. simple average.
		for (n = 0; n < nsample; ++n) {
		    for (t = 0; t < ntime; ++t) {
		      survPTrNO[n + t * nsample] += survTr[n + t * nsample] / *nTree;
		    }
		    for (t = 0; t < ntimeSm; ++t) {
		      survPTrNOSm[n + t * nsample] += survTrSm[n + t * nsample] / *nTree;
		    }
		}

		// predict testset data with the current tree
		if (*testdat) {
//Rprintf("\nTESTSET NOW:\n");
			predictSurvTree(xts, ntest, mdim, ntimeSm, lDaughter + idx,
						   rDaughter + idx, nodestatus + idx, stree,
                           upper + idx, avnodeSm + idx*ntimeSm,
						   mbest + idx, treeSize[j], cat, *maxcat, nodexts, nrnodes,
						   ntime, stree, avnode + idx * ntime, 0);
			/// stree is the predicted survival for test data by the current tree /
			/// sTestPred is the average prediction by all trees grown so far /
			for (n = 0; n < ntest; ++n) {
			  for (t = 0; t < ntimeSm; ++t) {
				  sTestPred[n + t * ntest] += stree[n + t * ntest] / *nTree;
//*sTestPred += stree[n + t * ntest] / *nTree;
			  }
			}
		}

        /* Print running output. */
		if ((j + 1) % *jprint == 0) {
			Rprintf("%4d |", j + 1);
			//Rprintf(" %8.4g %8.2f ", 0.0, 100.0);
			//if(*labelts == 1) Rprintf("| %8.4g %8.2f ", 0,0,0);
			Rprintf("|\n");
		}

		/*  DO PROXIMITIES */
		if (*doProx && !(1 - *returnBest && fold < *nfold - 1)) {
			computeProximity(prox, *oobprox, nodex, in, oobpair, nsample);
			/* proximity for test data */
			if (*testdat) {
                /* In the next call, in and oobpair are not used. */
                computeProximity(proxts, 0, nodexts, in, oobpair, ntest);
				for (n = 0; n < ntest; ++n) {
					for (k = 0; k < nsample; ++k) {
						if (nodexts[n] == nodex[k]) {
							proxts[n + ntest * (k+ntest)] += 1.0;
						}
					}
				}
			}
		}

		/* Variable importance */
		if (varImp) {
			for (mr = 0; mr < mdim; ++mr) {
        if (varUsed[mr]) { /* Go ahead if the variable is used */
            /* make a copy of the m-th variable into xtmp */
            for (n = 0; n < nsample; ++n)
                xtmp[n] = x[mr + n * mdim];
            ooberrperm1 = 0.0;
            ooberrperm2 = 0.0;
            for (k = 0; k < nPerm; ++k) {
              permuteOOB(mr, x, in, nsample, mdim);
              predictSurvTree(x, nsample, mdim, ntimeSm, lDaughter + idx,
                             rDaughter + idx, nodestatus + idx, survTrSm,
                             upper + idx, avnodeSm + idx*ntimeSm, mbest + idx,
                             treeSize[j], cat, *maxcat, nodex, nrnodes,
                             ntime, survTr, avnode + idx * ntime, 0);

              ibss(LR, survTrSm, timepointsSm, timediffSm, in, nsample, ntimeSm, ibsN, 1, tau);  //updating ibsN

              //jout = 0; /* jout is the number of cases that has been OOB so far */
              nNAN1 = 0;
              nNAN2 = 0;
              for (n = 0; n < nsample; ++n) {
                if (in[n] == 0) {
                  if (!isnan(ibsN[n])) {
                    ooberrperm1 += ibsN[n];
                  } else {
                    nNAN1++;
                  }
                  if (!isnan(ibsN[n + nsample])) {
                    ooberrperm2 += ibsN[n + nsample];
                  } else {
                    nNAN2++;
                  }
                }
              }
              if (nOOB - nNAN1 > 0) {
                delta1 = (ooberrperm1 / nPerm - ooberr1) / (nOOB - nNAN1);
              } else {
                nNAN1bPerm++;
              }
              if (nOOB - nNAN2 > 0) {
                delta2 = (ooberrperm2 / nPerm - ooberr2) / (nOOB - nNAN2);
              } else {
                nNAN2bPerm++;
              }
            }
            errimp[mr + mdim * fold]                 += delta1;
            errimp[mr + mdim * fold + mdim * *nfold] += delta2;
            //impSD[mr + mdim * fold] += delta1 * delta1;
            impSD[mr + mdim * fold]                 = 0.0/0.0;  //TBD or remove impSD
            impSD[mr + mdim * fold + mdim * *nfold] = 0.0/0.0;  //TBD or remove impSD
            //localImp TBD or remove localImp

            /* copy original data back */
            for (n = 0; n < nsample; ++n)
                x[mr + n * mdim] = xtmp[n];
        }
      }
    }
  }
    /* end of tree iterations=======================================*/

    // training sample error (oob)
    ibs[fold] /= (*nTree - nNAN1b);
    ibs[fold + *nfold] /= (*nTree - nNAN2b);
    // training sample error (No Oob) (`in` is nothing here.)
    /* Not needed any more when having survPTrNOSm instead of yptrNOSm.
    for (n = 0; n < nsample; ++n) {
      survSm[n + 0 * nsample] = 1.0 - survPTrNOSm[n + 0 * nsample]; //t = 0
      for (t = 1; t < ntimeSm; ++t) {
        survSm[n + t * nsample] = survSm[n + (t - 1) * nsample] - survPTrNOSm[n + t * nsample];
      }
    } */ // unlike survSm above (within a tree loop), this survSm is a forest-wise value
    ibss(LR, survPTrNOSm, timepointsSm, timediffSm, in, nsample, ntimeSm, ibsN, 0, tau);  //updating ibsN
    // using this updated ibsN, update ibsNO!

    ibsNO[fold] = 0.0;
    ibsNO[fold + *nfold] = 0.0;
    nNAN1b = 0;
    nNAN2b = 0;
    for (n = 0; n < nsample; ++n) {
      if (isnan(ibsN[n])) {
        ++nNAN1b;
      } else {
        ibsNO[fold] += ibsN[n];
      }
      if (isnan(ibsN[n+ nsample])) {
        ++nNAN2b;
      } else {
        ibsNO[fold + *nfold] += ibsN[n + nsample];
      }
//Rprintf("(%2.3f, %2.3f) ", ibsN[n], ibsN[n + nsample]);
    }
    ibsNO[fold] /= (nsample - nNAN1b);
    ibsNO[fold + *nfold] /= (nsample - nNAN2b);

    /* compute testset error */
    if (*labelts) {
      /* Not needed any more when having sTestPred instead of yTestPred.
      for (n = 0; n < ntest; ++n) {
        survts[n + 0 * ntest] = 1.0 - sTestPred[n + 0 * ntest];  //t == 0
        for (t = 1; t < ntimeSm; ++t) {
          survts[n + t * ntest] = survts[n + (t-1) * ntest] - sTestPred[n + t * ntest];
          //survts and sTestPred are both ntest x ntime
        }
        //if (n==2) for (t=0; t<10; ++t) Rprintf("surv %f %f \n", yts[n + t*ntest], survts[n + t * ntest]);
      }*/
      testErr(yts, sTestPred, timepointsSm, timediffSm, ntest, ntimeSm, interrts + 2 * fold, tau);  //updating true ibs
      /*//initializing survts for the next loop
      for (n = 0; n < ntest * ntimeSm; ++n) {
        survts[n] = 0.5;
      }*/
    }

    Rprintf("\n   IMSE1 (OOB, non-OOB) = (%2.3f, %2.3f), IMSE2 = (%2.3f, %2.3f)",
            ibs[fold], ibsNO[fold], ibs[fold + *nfold], ibsNO[fold + *nfold]);
    if (*labelts) Rprintf(", test set error (int, sup) = (%2.3f, %2.3f)",
        interrts[0 + 2 * fold], interrts[1 + 2 * fold]);
    Rprintf("\n");

    if (*doProx && !(1 - *returnBest && fold < *nfold - 1)) {
  		for (n = 0; n < nsample; ++n) {
  			for (k = n + 1; k < nsample; ++k) {
            prox[nsample*k + n] /= *oobprox ?
                (oobpair[nsample*k + n] > 0 ? oobpair[nsample*k + n] : 1) :
                *nTree;
            prox[nsample * n + k] = prox[nsample * k + n];
        }
  			prox[nsample * n + n] = 1.0;
      }
  		if (*testdat) {
  			for (n = 0; n < ntest; ++n)
  				for (k = 0; k < ntest + nsample; ++k)
  					proxts[ntest*k + n] /= *nTree;
  		}
    }

    if (varImp) {
		for (m = 0; m < mdim; ++m) {
			errimp[m + mdim * fold]                 /= (*nTree - nNAN1bPerm);
		  errimp[m + mdim * fold + mdim * *nfold] /= (*nTree - nNAN2bPerm);
			//impSD[m] = sqrt( ((impSD[m] / *nTree) -
			//				  (errimp[m] * errimp[m])) / *nTree );  //TBD or remove impSD
			//if (localImp) {  //localImp TBD or remove localImp
      //          for (n = 0; n < nsample; ++n) {
      //              impmat[m + n * mdim] /= nout[n];
      //          }
			//}
        }
    }
    for (m = 0; m < mdim; ++m) tgini[m + mdim * fold] /= *nTree;

    /* Updatng the conditional probabilities for the next Forest loop*/
    for (n = 0; n < nsample; ++n) {
      sampleSum = 0.0;
      for (t = 0; t < ntime; ++t) {
        if ((LR[n] <= timepoints[t]) && ((LR[nsample + n] == 1.0/0.0)| (LR[nsample + n] > timepoints[t]))) {
          prob[t + n * ntime] = (t==0? 1.0: survPTrNO[n + (t-1) * nsample]) - survPTrNO[n + t * nsample] ;
          sampleSum += prob[t + n * ntime];
        } else {
          prob[t + n * ntime] = 0.0;
        }
      }
      // normalizing
      if (sampleSum == 0.0) {  // for degenerate case. Not ideal but approximate when grid is fine.
        for (t = 0; t < ntime; ++t) {
          if (LR[n] == timepoints[t]) prob[t + n * ntime] = 1.0;
        }
      } else {
        for (t = 0; t < ntime; ++t) {
          prob[t + n * ntime] /= sampleSum;
        }
      }
    }

    if (*returnBest) { //if *returnBest = TRUE, then keep record at the best iter, and plug them back in the end.
      imseCurrent = ibs[fold + *nfold * (*ibsType - 1)];
      if (imseCurrent <= imseBest) {
        imseBest = imseCurrent;
        *BestFold = fold + 1;

        // record the best result
        for (t = 0; t < nsample * ntime; t++) {
          BsurvPTr[t]   = survPTr[t];
          BsurvPTrNO[t] = survPTrNO[t];
        }
        for (t = 0; t < nsample * ntimeSm; t++) {
          BsurvPTrNOSm[t]   = survPTrNOSm[t];
        }
        if (*testdat)
          for (t = 0; t < ntest * ntimeSm; t++) {
            BsTestPred[t]   = sTestPred[t];
          }
        for (t = 0; t < ntime * *nrnodes * nt; t++) {
          Bavnode[t]   = avnode[t];
        }
        for (t = 0; t < ntimeSm * *nrnodes * nt; t++) {
          BavnodeSm[t] = avnodeSm[t];
        }
        for (t = 0; t < *nrnodes * nt; t++) {
          Bupper[t] = upper[t];
          Bnodestatus[t] = nodestatus[t];
          BlDaughter[t] = lDaughter[t];
          BrDaughter[t] = rDaughter[t];
          Bmbest[t] = mbest[t];
        }
        for (t = 0; t < nt; t++) {
          BtreeSize[t] = treeSize[t];
        }
        for (t = 0; t < nsample; t++) {
          Bnout[t] = nout[t];
        }
        if (*keepIB)
          for (t = 0; t < nsample * *nTree; t++) {
            Binbag[t] = inbag[t];
          }
        if (*doProx) {
          for (t = 0; t < nsample * nsample; t++) {
            Bprox[t] = prox[t];
          }
          if (*testdat)
          for (t = 0; t <  ntest * (nsample + ntest); t++) {
            Bproxts[t] = proxts[t];
          }
        }

      }
    }



} /* END OF FOREST LOOP */

    //Plugging in the best result back.
    if (*returnBest) {
      // record the best result
      for (t = 0; t < nsample * ntime; t++) {
        survPTr[t]   = BsurvPTr[t];
        survPTrNO[t] = BsurvPTrNO[t];
      }
      for (t = 0; t < nsample * ntimeSm; t++) {
        survPTrNOSm[t]   = BsurvPTrNOSm[t];
      }
      if (*testdat)
        for (t = 0; t < ntest * ntimeSm; t++) {
          BsTestPred[t]   = sTestPred[t];
        }
      for (t = 0; t < ntime * *nrnodes * nt; t++) {
        avnode[t]   = Bavnode[t];
      }
      for (t = 0; t < ntimeSm * *nrnodes * nt; t++) {
        avnodeSm[t] = BavnodeSm[t];
      }
      for (t = 0; t < *nrnodes * nt; t++) {
        upper[t] = Bupper[t];
        nodestatus[t] = Bnodestatus[t];
        lDaughter[t] = BlDaughter[t];
        rDaughter[t] = BrDaughter[t];
        mbest[t] = Bmbest[t];
      }
      for (t = 0; t < nt; t++) {
        treeSize[t] = BtreeSize[t];
      }
      for (t = 0; t < nsample; t++) {
        nout[t] = Bnout[t];
      }
      if (*keepIB)
        for (t = 0; t < nsample * *nTree; t++) {
          inbag[t] = Binbag[t];
        }
      if (*doProx) {
        for (t = 0; t < nsample * nsample; t++) {
          prox[t] = Bprox[t];
        }
        if (*testdat)
          for (t = 0; t <  ntest * (nsample + ntest); t++) {
            proxts[t] = Bproxts[t];
          }
      }
    }



PutRNGstate();
}

/*----------------------------------------------------------------------*/
void survForest(double *x, double *survPred, int *mdim, int *ntime, int *n, //int *ntimeTest,
                int *ntree, int *lDaughter, int *rDaughter,
                int *nodestatus, int *nrnodes, double *xsplit,
                double *avnodes, int *mbest, int *treeSize, int *cat,
                int *maxcat, int *keepPred, double *allpred, int *doProx,
                double *proxMat, int *nodes, int *nodex) {
    int i, j, t, idx1, idx2, *junk;
    double *stree; //, *streeTest;
    //avnodes = ntime x nrnodes x ntree
    //stree = 1 x ntime
    //streeTest = 1 x ntimeTest

    junk = NULL;
    stree = (double *) S_alloc(*n * *ntime, sizeof(double));
    //streeTest = (double *) S_alloc(*n * *ntimeTest, sizeof(double));
    if (*nodes) {
	    zeroInt(nodex, *n * *ntree);
    } else {
	    zeroInt(nodex, *n);
    }
    if (*doProx) zeroDouble(proxMat, *n * *n);
    if (*keepPred) zeroDouble(allpred, *n * *ntree * *ntime);
    idx1 = 0;
    idx2 = 0;
    for (i = 0; i < *ntree; ++i) {
    	zeroDouble(stree, *n * *ntime);
    	predictSurvTree(x, *n, *mdim, *ntime, lDaughter + idx1, rDaughter + idx1,
                      nodestatus + idx1, stree, xsplit + idx1,
                      avnodes + idx1 * *ntime, mbest + idx1, treeSize[i], cat, *maxcat,
                      nodex + idx2, nrnodes,
                      *ntime, stree, avnodes + idx1 * *ntime, 0);

//Rprintf("pred for i=9-1: nodex = %d\n", nodex[idx2]);
    	for (j = 0; j < *n; ++j) {
    	  for (t = 0; t < *ntime; ++t) {
//if(i==0 && j==0)  Rprintf("survPred[%d + %d *n] = %f, stree[%d + %d *n] = %f\n",
//   j, t, survPred[j + t * *n] + stree[j + t * *n],
//   j, t,  + stree[j + t * *n]);
    	    survPred[j + t * *n] += stree[j + t * *n];
    	  }
    	}

    	if (*keepPred) {//tbd!!!
  	   for (j = 0; j < *n; ++j) {
  	     for (t = 0; t < *ntime; ++t) {
  	       allpred[j + t * *n + i * *n * *ntime] = stree[j + t * *n];
  	     }
  	   }
    	}
    	/* if desired, do proximities for this round */
    	if (*doProx) computeProximity(proxMat, 0, nodex + idx2, junk, junk, *n);
    	idx1 += *nrnodes; /* increment the offset */
    	if (*nodes) idx2 += *n;
    }
    for (i = 0; i < *n * *ntime; ++i) survPred[i] /= *ntree;
    if (*doProx) {
	    for (i = 0; i < *n; ++i) {
  	    for (j = i + 1; j < *n; ++j) {
          proxMat[i + j * *n] /= *ntree;
  		    proxMat[j + i * *n] = proxMat[i + j * *n];
  	    }
  	    proxMat[i + i * *n] = 1.0;
	    }
    }

}
