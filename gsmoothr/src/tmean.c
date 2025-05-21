#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <R.h>
#include <Rmath.h>

// void tmean(double *x, double *xs, long *sp, long *n_, double *_tr, long *_np, long *_pw)
void tmean(double *x, double *xs, int *sp, int *n_, double *_tr, int *_np, int *_pw)
{

/*
  #  x  -- original probe-level score
  # xs  -- smoothed probe-level score
  # sp  -- position
  # n_  -- length of each of the above vectors
  # tr  -- amount of trim (0-0.49)
  # np  -- (min) number of probes
  # _pw -- probe window
*/

  int ii=0, jj=0, kk=0, lo, hi, st=0, en=0, n=*n_, np=*_np, pw=*_pw;
  double dummy[1000], sm=0., tr=*_tr;

  for (ii=0; ii< n; ii++) {
  
	/* find set of probes to use */
    while( (sp[ii]-sp[st]) > pw )
	  st++;
    while( (en < n) && ((sp[en]-sp[ii]) < pw))
	  en++;
	en--;
	if ((en-st+1) < np)
	  continue;
  /*printf("ii=%d, st=%d, en=%d\n", ii, st, en);*/
  	
	/* assign current vector to dummy */
	kk=0;
	for(jj=st; jj <= en; jj++)
	  dummy[kk++] = x[jj];
	  
	/* sort vector inline */
	R_rsort(dummy, kk);

	/* take mean of (internal portion) of dummy */
	sm = 0.;
    lo = floor(((float) kk) * tr + 0.5); // +0.5 to make a 'round' function.
    hi = kk-lo-1;
    // printf("lo=%d hi=%d H-L+1=%d\n", lo, hi, (hi-lo+1));
	for(jj=lo; jj <= hi; jj++)
	  sm += dummy[jj];
	
	/* assign smoothed data */
	xs[ii] = sm/sqrt((float)(hi-lo + 1));
  }


/*
# ------------------------	
# R code for trimmed mean
# ------------------------	
#trimmedMean <- function(pos, score, probeWindow=600, meanTrim=.1, nProbes=10) {
#  st <- 1
#  en <- 1
#  n <- length(pos)
#  stopifnot( length(score)==n )
#  tmean <- rep(0,n)
#  for(ii in 1:n) {
#    while( (pos[ii]-pos[st]) > probeWindow )
#      st <- st + 1
#    while( (pos[en]-pos[ii]) < probeWindow & en < (n-1))
#      en <- en + 1
#    if ( (en-st+1) < nProbes )
#      next
#    tmean[ii] <- mean( score[st:en], trim=meanTrim )*sqrt(en-st+1)
#  }
#  tmean
#}
# ------------------------	
*/

}


void tmeanPos(double *xi, double *xo, int *spi, int *spo, int *ni_, int *no_, double *_tr, int *_np, int *_pw)
{

/*
  # xi  -- original probe-level score (input)
  # xo  -- smoothed score (output)
  # spi -- position to use for scoring (input)
  # spo -- position to score for (output)
  # ni_ -- length of input vectors
  # no_ -- length of output vectors
  # tr  -- amount of trim (0-0.49)
  # np  -- (min) number of probes
  # _pw -- probe window
*/

  int ii=0, jj=0, kk=0, lo, hi, st=0, en=0, ni=*ni_, no=*no_, np=*_np, pw=*_pw;
  double dummy[1000], sm=0., tr=*_tr, xx;

  for (ii=0; ii< no; ii++) {
  
	  /* find set of probes to use */
    while( (spo[ii]-spi[st]) > pw )
	    st++;
    while( ((spi[en]-spo[ii]) < pw) && (en < (ni-1)) )
	    en++;
	  if ((en-st+1) < np)
	    continue;
    /*  printf("@@ ii=%d, st=%d, spo-spi=%d\n", ii, st, spo[ii]-spi[st]); */
      	
	  /* assign current vector to dummy */
	  kk=0;
	  for(jj=st; jj <= en; jj++)
	    dummy[kk++] = xi[jj];
	  
    /* sort vector inline */
	  R_rsort(dummy, kk);

	  /* take mean of (internal portion) of dummy */
	  sm = 0.;
    lo = floor((float) kk * tr);
    hi = kk-lo-1;
 	  for(jj=lo; jj <= hi; jj++)
	    sm += dummy[jj];
      
    xx = sm/sqrt((float)(hi-lo+1));
    /*printf("ii=%d, sp=%d, st=%d, en=%d, kk=%d, lo=%d, hi=%d, sm=%5.4f xx=%5.4f\n", ii, spo[ii], st, en, kk, lo, hi, sm, xx);*/
	
	  /* assign smoothed data */
	  xo[ii] = sm/sqrt((float)(hi-lo+1));
  }

}
