
#include <R.h>
#include <math.h>
#include "rf.h"

//ibs for external call
void survError(double* LR, double* surv, double* trueSurv, double* timepoints,
               int* rinf, int* tinf, int *nsample, int *ntime, double* ibs,
               double* interr, double* tau, int* msr1, int* msr2) {
  int *inn, n, t;
  double *timediff, *ibsN;
  int nNAN1 = 0, nNAN2 = 0; //nan counters for types 1 and 2

  inn   = (int *) S_alloc(1, sizeof(int));
  timediff   = (double *) S_alloc(*ntime-1, sizeof(double));
  ibsN   = (double *) S_alloc(*msr1 ? *nsample * 2 : 1, sizeof(double));

  /* properly handling inf in C */
  if (*msr1) for (n = 0; n < *nsample; ++n) if (rinf[n] == 1) LR[*nsample + n] = 1.0 /0.0;
  for (t = 0; t < *ntime; ++t) if (tinf[t] == 1) timepoints[t] = 1.0 /0.0;
  for (t = 1; t < *ntime; ++t) timediff[t-1] = timepoints[t] - timepoints[t-1];
//Rprintf("L = %2.2f, R = %2.2f \n", LR[0], LR[0 + *nsample]);
//Rprintf("n = %d, ntime = %d \n", *nsample, *ntime);

  if (*msr1) {
    // initialize (ibsN is initialized inside ibss)
    ibs[0] = 0.0;
    ibs[1] = 0.0;
    // update ibsN
    ibss(LR, surv, timepoints, timediff, inn, *nsample, *ntime, ibsN, 0, tau);
    for (n = 0; n < *nsample; ++n) {
      // ibs type 1
      if (isnan(ibsN[n])) {
        ++nNAN1;
      } else {
        ibs[0] += ibsN[n];
      }

      // ibs type 2
      if (isnan(ibsN[n])) {
        ++nNAN2;
      } else {
        ibs[1] += ibsN[n + *nsample];
      }
    }
    ibs[0] /= (*nsample - nNAN1);
    ibs[1] /= (*nsample - nNAN2);
  }
  if (*msr2) {
    testErr(trueSurv, surv, timepoints, timediff, *nsample, *ntime, interr, tau);
  }
}


void ibss(double* LR, double* surv, double* timepoints, double* timediff, int* inn,
          int nsample, int ntime, double* ibsN, int oob, double* tau) {
  // surv = cumsum(ypred), timepoints should be only upto tau, timediff is of length ntime - 1.
  int n, t;
  double err1, err2, SL, SR, b, timeSum1, timeSum2;
  for (n = 0; n < nsample * 2; ++n) ibsN[n] = 0.0;
  // zeroDouble(ibsN, nsample);

  for (n = 0; n < nsample; ++n) {
    //Rprintf("L = %f, R = %f, n = %d \n", LR[n], LR[n + nsample], n + 1);
    if (oob == 1) if (inn[n] > 0) continue;
    //if (inn[n] == 0) {
      err1 = 0.0;
      err2 = 0.0;
      timeSum1 = 0.0;
      timeSum2 = 0.0;

      // Getting SL and SR
      SL = 1.0;
      SR = 0.0; //default values
      for (t = 0; (t < ntime - 1) & (timepoints[t] <= *tau); ++t) {
        if (LR[n] >= timepoints[t] &&  LR[n] < timepoints[t + 1]) {
          SL = surv[n + t * nsample];
        } else if (LR[n + nsample] > timepoints[t] &&  LR[n + nsample] <= timepoints[t + 1]) {
          SR = surv[n + t * nsample];
        }
      }
      if (SL <= SR) {
        b = 1.0;
      } else {
        b = 1.0 / (SL - SR);
      }

      for (t = 1; (t < ntime) & (timepoints[t] <= *tau); ++t) {
        if (isfinite(timediff[t-1])==0) break;
        if (timepoints[t] <= LR[n]) {
          err1 += timediff[t-1] * (1.0 - surv[n + t * nsample]) * (1.0 - surv[n + t * nsample]);
          timeSum1 += timediff[t-1];
        } else if  (timepoints[t] > LR[n + nsample]) {
          err1 += timediff[t-1] * surv[n + t * nsample] * surv[n + t * nsample];
          timeSum1 += timediff[t-1];
        } else { // for ibs type 2
          err2 += timediff[t-1] * ((1.0 - b) * surv[n + t * nsample] + b * SR) * ((1.0 - b) * surv[n + t * nsample] + b * SR);
          timeSum2 += timediff[t-1];
        }
//if (t > 70) Rprintf("time = %2.2f, err1 = %2.3f, surv = %2.3f\n", timepoints[t], err1, surv[n+t*nsample]);
//if (n==0) Rprintf("(%2.3f, %2.3f, %2.3f, %2.3f) ", timepoints[t], err1, timediff[t-1], surv[n+t*nsample]);
      }
      ibsN[n] = err1 / timeSum1;
      ibsN[n + nsample] = (err1 + err2) / (timeSum1 + timeSum2);
      /* if (err1 == 0) {
      Rprintf("err1 = %f, timeSum1 = %f, ibsN[%d] = %f   ", err1, timeSum1, n, ibsN[n]);
      Rprintf("LR = %f %f   \n", LR[n], LR[n + nsample]);
    } */

  //}
  }
}

/*
void cindex(double* LR, double* surv, double* timepoints, double* timediff, int* inn,
            int nsample, int ntime, double* cind, int oob, double* tau) {
  // surv = cumsum(ypred), timepoints should be only upto tau, timediff is of length ntime - 1.
  int n, t;
  double err1, err2, SL, SR, b, timeSum1, timeSum2;
  for (n = 0; n < nsample * 2; ++n) cind[n] = 0.0;
  // zeroDouble(cind, nsample);

  for (n = 0; n < nsample; ++n) {
    //Rprintf("L = %f, R = %f, n = %d \n", LR[n], LR[n + nsample], n + 1);
    if (oob == 1) if (inn[n] > 0) continue;
    //if (inn[n] == 0) {
    err1 = 0.0;
    err2 = 0.0;
    timeSum1 = 0.0;
    timeSum2 = 0.0;

    // Getting SL and SR
    SL = 1.0;
    SR = 0.0; //default values
    for (t = 0; t < ntime - 1; ++t) {
      if (LR[n] >= timepoints[t] &&  LR[n] < timepoints[t + 1]) {
        SL = surv[n + t * nsample];
      } else if (LR[n + nsample] > timepoints[t] &&  LR[n + nsample] <= timepoints[t + 1]) {
        SR = surv[n + t * nsample];
      }
    }
    if (SL <= SR) {
      b = 1.0;
    } else {
      b = 1.0 / (SL - SR);
    }

    for (t = 1; t < ntime; ++t) {
      if (isfinite(timediff[t-1])==0) break;
      if (timepoints[t] <= LR[n]) {
        err1 += timediff[t-1] * (1.0 - surv[n + t * nsample]) * (1.0 - surv[n + t * nsample]);
        timeSum1 += timediff[t-1];
      } else if  (timepoints[t] > LR[n + nsample]) {
        err1 += timediff[t-1] * surv[n + t * nsample] * surv[n + t * nsample];
        timeSum1 += timediff[t-1];
      } else { // for ibs type 2
        err2 += timediff[t-1] * ((1.0 - b) * surv[n + t * nsample] + b * SR) * ((1.0 - b) * surv[n + t * nsample] + b * SR);
        timeSum2 += timediff[t-1];
      }
      //if (t > 70) Rprintf("time = %2.2f, err1 = %2.3f, surv = %2.3f\n", timepoints[t], err1, surv[n+t*nsample]);
      //if (n==0) Rprintf("(%2.3f, %2.3f, %2.3f, %2.3f) ", timepoints[t], err1, timediff[t-1], surv[n+t*nsample]);
    }
    cind[n] = err1 / timeSum1;
    cind[n + nsample] = (err1 + err2) / (timeSum1 + timeSum2);
  //if (err1 == 0) {
  //  Rprintf("err1 = %f, timeSum1 = %f, cind[%d] = %f   ", err1, timeSum1, n, cind[n]);
  //  Rprintf("LR = %f %f   \n", LR[n], LR[n + nsample]);
  //}

    //}
}
}*/


void testErr(double* trueSurv, double* surv, double* timepoints, double* timediff,
             int ntest, int ntime, double* interrts, double* tau) {
  int n, t;
  double err0, err1, err2;
  //zeroDouble(interrts, 2);
  interrts[0] = 0.0;
  interrts[1] = 0.0;

  for (n = 0; n < ntest; ++n) {
    err0 = 0.0;  // current error
    err1 = 0.0;  // integrated error
    err2 = 0.0;  // sup error
    //if (n<3) Rprintf("n=%d\n", n+1);
    for (t = 0; t < ntime - 1; ++t) { //(ntime-1) diff requires one df.
      if (isfinite(timediff[t])==0) break;
      err0 = fabs(trueSurv[n + t * ntest] - surv[n + t * ntest]);
      err1 += timediff[t] * err0;
      if (err0 > err2) err2 = err0;
//if (n == 0 && t > 370) {Rprintf("n=1, t=%d, err: %f, interr %f, superr %f, timediff %f\n", t, err0, err1, err2, timediff[t]);}
//if (n == 2 && t > 370)  Rprintf("t=%d, trueSurv %f surv %f\n", t, trueSurv[n + t*ntest], surv[n + t * ntest]);
    }
    interrts[0] += err1/ntest;
    interrts[1] += err2/ntest;
//Rprintf("n=%d, int.i %f, cumint %f, sup.i %f, cumsup %f\n", n + 1, err1, interrts[0], err2, interrts[1]);
  }
}
