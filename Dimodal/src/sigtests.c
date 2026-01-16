
/*****
      sigtests.c -
      Core calculations for feature tests.

      Compiled into shared library for R, not an executable.
      Define NO_PCGRNG while compiling to use R's sampling function for
      excursions, although it will be slower than using the PCG RNG.

      c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.
*****/


/**
   To Do:
   - 
**/


#include <float.h>
#include "sigtests.h"
#include "detectors.h"
#ifndef NO_PCGRNG
#include "pcg_variants.h"
#endif     /* NO_PCGRNG */


/**** Macros/Defines ****/

/* sub-matrix IDs in the temporary memory for max runlength, last for count */
#define RL_TOP         0
#define RL_PREV        1
#define RL_CURR        2
#define RL_NTMP        (RL_CURR+1)

/* true to use generic run length matrix operations no matter the tmat size */
#define RL_GENERIC     0


/**** Structures ****/


/**** Prototypes ****/

/* actual work functions */
static SEXP impl_test_runlen(int, int, double *, double *, int);
static SEXP impl_altperm(SEXP);
static SEXP impl_altperm_2symb(SEXP);
static SEXP impl_excursion(SEXP, int, int, int, int);
static SEXP impl_htprob(SEXP, SEXP, int);


/**** Public Interface ****/

/***
    C_test_runlen:  Determine the probability of a longest run of length rl
                    while stepping featlen times through a Markov chain with
                    transition matrix tmat and stationary state smat, used to
                    weight the starting points.
    args:           Rrl - longest run length
                    Rfeatlen - feature length, number of steps through chain
                    Rtmat - transition matrix
                    Rsmat - stationary matrix
    returns:   probability of run with rl
***/
SEXP C_test_runlen(SEXP Rrl, SEXP Rfeatlen, SEXP Rtmat, SEXP Rsmat) {
	double *tmat;                         /* transition matrix from Rtmat */
	double *smat;                         /* stationary matrix from Rsmat */
	int ns;                               /* number symbols (tmat/smat size) */
	int rl;                               /* longest run length */
	int featlen;                          /* feature size/steps through chain */

	if (!isReal(Rtmat) || !isReal(Rsmat)) {
		error("transition/stationary matrices not real numbers");
	}

	/* We evolve a super-transition matrix encoding the runs over featlen
	   steps, and then sum the probability for each state in tmat, weighting
	   by smat.  To not weight, fill smat with the same value, b.v. 1/#nstate.
	   To skip an initial state, put 0.0 in smat.  smat should sum to 1,
	   although this isn't checked.
	*/

	tmat = REAL(Rtmat);
	smat = REAL(Rsmat);
	ns = length(Rsmat);

	if (isReal(Rfeatlen)) {
		featlen = (int) round(asReal(Rfeatlen));
	} else if (isInteger(Rfeatlen)) {
		featlen = asInteger(Rfeatlen);
	} else {
		error("feature length must be integer or real value");
	}

	if (isReal(Rrl)) {
		rl = (int) round(asReal(Rrl));
	} else if (isInteger(Rrl)) {
		rl = asInteger(Rrl);
	} else {
		error("longest run length must be integer or real value");
	}

	if ((rl < 1) || (featlen < 1)) {
		return ScalarReal(0.0);
	} else if (featlen <= rl) {
		return ScalarReal(1.0);
	}
	
	return impl_test_runlen(rl, featlen, tmat, smat, ns);
}

/***
    C_altperm_symbols:  Create a permutation of the symbols specified by the
                        count of each in the argument.  Returns a vector with
                        the identifiers per position in Rngrp.  No two adjacent
                        symbols will be the same or the function raises an
                        error.  There must be more than one symbol.
    args:               Rngrp - number of each symbol
    returns:   vector of permuted symbol identifiers
***/
SEXP C_altperm_symbols(SEXP Rngrp) {

	if (!isReal(Rngrp) && !isInteger(Rngrp)) {
		error("argument must be real or integer");
	}
	if (length(Rngrp) < 2) {
		error("single-group permututation should be handled in R");
	}

	/* The general permutation does handle the 2 symbol case, but it's much
	   quicker to treat them as odd/even allocations rather than picking 
	   symbol by symbol. */
	if (2 == length(Rngrp)) {
		return impl_altperm_2symb(Rngrp);
	} else {
		return impl_altperm(Rngrp);
	}
}

/***
    C_excursion:  Pull ndraw samples from values xbase and reconstruct the
                  signal by doing a cumulative sum of them, storing the
                  height, the difference between the maximum and minimum sum.
                  Repeat nexcur times.  The draws are done uniformly, with
                  replacement.
    args:         Rxbase - data to draw from (integer or real)
                  Rnexcur - number of excursions to run
                  Rndraw - number of draws from xbase to make per excursion
                  Rispeak - TRUE to determine heights for peaks, FALSE flats
                  Rsort - boolean, TRUE to sort heights before return
    returns:   a vector of length nexcur of excursion heights
***/
SEXP C_excursion(SEXP Rxbase, SEXP Rnexcur, SEXP Rndraw, SEXP Rispeak,
                 SEXP Rsort) {
	int nexcur;                           /* excursion count, from Rnexcur */
	int ndraw;                            /* draw count, contents of Rndraw */
	int ispeak;                           /* 0 for flat height, else peak */
	int dosort;                           /* 0 to not sort result */

	if (!isReal(Rxbase) && !isInteger(Rxbase)) {
		error("draw base must have real or integer values");
	}
	
	if (isReal(Rnexcur)) {
		nexcur = (int) round(asReal(Rnexcur));
	} else if (isInteger(Rnexcur)) {
		nexcur = asInteger(Rnexcur);
	} else {
		error("number of excursions must be real or integer value");
	}
	if (nexcur <= 0) {
		error("must make at least one excursion");
	}

	if (isReal(Rndraw)) {
		ndraw = (int) round(asReal(Rndraw));
	} else if (isInteger(Rndraw)) {
		ndraw = asInteger(Rndraw);
	} else {
		error("number of draws must be real or integer value");
	}
	if (ndraw <= 0) {
		error("must draw at least one point");
	}

	if (isLogical(Rispeak)) {
		ispeak = asLogical(Rispeak);
		if (NA_LOGICAL == ispeak) {
			error("ispeak flat must be TRUE or FALSE");
		}
	} else {
		error("ispeak flag must be logical");
	}

	if (isLogical(Rsort)) {
		dosort = asLogical(Rsort);
		if (NA_LOGICAL == dosort) {
			error("sort flag must be TRUE or FALSE");
		}
	} else {
		error("sort flag must be logical");
	}

	return impl_excursion(Rxbase, nexcur, ndraw, ispeak, dosort);
}

/***
    C_htprob:  Search excursion/permutation values for test statistic 
               height(s), performing mid-distribution adjustment if values
               have ties.  If lower_tail true return number of values <= 
               height, if 0 >.  The distribution values must be sorted.
    args:      Rdist - excursion/permutation distribution values of heights
               Rhts - test statistic/height(s)
               Rlower_tail - true for distribution below/equal height, 0 above
    returns:   probability of heights (vector has same length as Rhts)
***/
SEXP C_htprob(SEXP Rdist, SEXP Rhts, SEXP Rlower_tail) {
	int lotail;                           /* contents of Rlower_tail */

	if (!isReal(Rdist) && !isInteger(Rdist)) {
		error("distribution samples must be real or integer");
	}
	if (length(Rdist) < 1) {
		error("there must be at least one distribution sample");
	}
	
	if (!isReal(Rhts) && !isInteger(Rhts)) {
		error("heights must be real or integer");
	}

	lotail = asLogical(Rlower_tail);
	if (NA_LOGICAL == lotail) {
		error("cannot treat lower tail value as a logical");
	}

	return impl_htprob(Rdist, Rhts, lotail);
}



/**** Implementations ****/

/***
    impl_test_runlen:  Evolve the run transition matrix based on a single-step
                      Markov chain model through featlen steps and evaluate
                      the probability of a run <= rl.  Consider 
                      trajectories from all states, weighting by smat.
    args:             rl - length of longest run
                      featlen - feature size containing longest run
                      tmat - transition matrix (first order Markov chain)
                      smat - initial state weights, stationary state vector
                      ns - size of tmat, smat
    returns:   probability that longest run is <= rl
***/
static SEXP impl_test_runlen(int rl, int featlen, double *tmat, double *smat,
                            int ns) {
	double *recur;                        /* recursion on chain final column */
	double *tmp;                          /* storage of partial results */
	double rowsum;                        /* row sum of final recursion */
	double prl;                        /* run probability at chain end */
	int topID;                            /* base of top sub-matrix in tmp */
	int prevID;                           /* base of previous tmp sub-matrix */
	int currID;                           /* base of current tmp sub-matrix */
	int rID;                              /* index into recur array */
	int t;                                /* step along feature */
	int ij;                               /* 2D index in array */
	int i, j, k;

	recur = NULL;
	tmp = NULL;

	/* Splitting tmat into its diagonal A and off-diagonal B, the probability
	   of a run of length at least L over N steps is
	     P{len <= L} = 1 - w r_{1,L,N}
       r_{1,L,N} = A^L + \sum_{j=1}{L} A^{J-1} B r_{1,L,N-j}
       r_{1,L,n} = 0 if n <= L, A if n == L
     where w is the initial probability of each symbol (ie. smat).
	   This function does the recursion on r, weights the result, and returns P.

	   It is a couple orders of magnitude speed-up over the R implementation,
	   fairly consistently from the small test datasets to the Kirkwood gap 
	   example. 
	*/
	
	/* tmat is ordered by columns in memory, Fortran style.
	   The diagonal on column j is at index j * (ns+1).
        0  2      0  3  6      0  4  8 12      0  5 10 15 20
        1  3      1  4  7      1  5  9 13      1  6 11 16 21
                  2  5  8      2  6 10 14      2  7 12 17 22
                               3  7 11 15      3  8 13 18 23
                                               4  9 14 19 24
	   Does this affect the performance, ie. is it worth transposing in C order?
  */

	recur = (double *) R_alloc(rl*ns*ns, sizeof(*recur));
	memset(recur, 0, rl*ns*ns*sizeof(*recur));

	tmp = (double *) R_alloc(RL_NTMP*ns*ns, sizeof(*tmp));
	memset(tmp, 0, RL_NTMP*ns*ns*sizeof(*tmp));
	topID = RL_TOP * ns * ns;
	prevID = RL_PREV * ns * ns;
	currID = RL_CURR * ns * ns;

	rID = (rl - 1) * ns * ns;
	if (!RL_GENERIC && (2 == ns)) {
		recur[rID+0] = tmat[0];
		recur[rID+3] = tmat[3];
	} else if (!RL_GENERIC && (3 == ns)) {
		recur[rID+0] = tmat[0];
		recur[rID+4] = tmat[4];
		recur[rID+8] = tmat[8];
	} else {
		for (j=0; j<ns; j++) {
			recur[rID+(j*(ns+1))] = tmat[j*(ns+1)];
		}
	}

	for (t=1; t<featlen; t++) {
		/* top = R * recur[0,,].  R 0 along diagonal (ns+1)*i, i row. */
		rID = 0;
		if (!RL_GENERIC && (2 == ns)) {
			tmp[topID+0] = tmat[2] * recur[rID+1];
			tmp[topID+2] = tmat[2] * recur[rID+3];
			tmp[topID+1] = tmat[1] * recur[rID+0];
			tmp[topID+3] = tmat[1] * recur[rID+2];
		} else if (!RL_GENERIC && (3 == ns)) {
			tmp[topID+0] = (tmat[3] * recur[rID+1]) + (tmat[6] * recur[rID+2]);
			tmp[topID+3] = (tmat[3] * recur[rID+4]) + (tmat[6] * recur[rID+5]);
			tmp[topID+6] = (tmat[3] * recur[rID+7]) + (tmat[6] * recur[rID+8]);
			tmp[topID+1] = (tmat[1] * recur[rID+0]) + (tmat[7] * recur[rID+2]);
			tmp[topID+4] = (tmat[1] * recur[rID+3]) + (tmat[7] * recur[rID+5]);
			tmp[topID+7] = (tmat[1] * recur[rID+6]) + (tmat[7] * recur[rID+8]);
			tmp[topID+2] = (tmat[2] * recur[rID+0]) + (tmat[5] * recur[rID+1]);
			tmp[topID+5] = (tmat[2] * recur[rID+3]) + (tmat[5] * recur[rID+4]);
			tmp[topID+8] = (tmat[2] * recur[rID+6]) + (tmat[5] * recur[rID+7]);
		} else if (!RL_GENERIC && (4 == ns)) {
			tmp[topID+0]  = (tmat[4]  * recur[rID+1])  + (tmat[8]  * recur[rID+2])  +
				              (tmat[12] * recur[rID+3]);
			tmp[topID+4]  = (tmat[4]  * recur[rID+5])  + (tmat[8]  * recur[rID+6])  +
				              (tmat[12] * recur[rID+7]);
			tmp[topID+8]  = (tmat[4]  * recur[rID+9])  + (tmat[8]  * recur[rID+10]) +
				              (tmat[12] * recur[rID+11]);
			tmp[topID+12] = (tmat[4]  * recur[rID+13]) + (tmat[8]  * recur[rID+14]) +
				              (tmat[12] * recur[rID+15]);
			tmp[topID+1]  = (tmat[1]  * recur[rID+0])  + (tmat[9]  * recur[rID+2])  +
				              (tmat[13] * recur[rID+3]);
			tmp[topID+5]  = (tmat[1]  * recur[rID+4])  + (tmat[9]  * recur[rID+6])  +
				              (tmat[13] * recur[rID+7]);
			tmp[topID+9]  = (tmat[1]  * recur[rID+8])  + (tmat[9]  * recur[rID+10]) +
				              (tmat[13] * recur[rID+11]);
			tmp[topID+13] = (tmat[1]  * recur[rID+12]) + (tmat[9]  * recur[rID+14]) +
				              (tmat[13] * recur[rID+15]);
			tmp[topID+2]  = (tmat[2]  * recur[rID+0])  + (tmat[6]  * recur[rID+1])  +
				              (tmat[14] * recur[rID+3]);
			tmp[topID+6]  = (tmat[2]  * recur[rID+4])  + (tmat[6]  * recur[rID+5])  +
				              (tmat[14] * recur[rID+7]);
			tmp[topID+10] = (tmat[2]  * recur[rID+8])  + (tmat[6]  * recur[rID+9])  +
				              (tmat[14] * recur[rID+11]);
			tmp[topID+14] = (tmat[2]  * recur[rID+12]) + (tmat[6]  * recur[rID+13]) +
				              (tmat[14] * recur[rID+15]);
			tmp[topID+3]  = (tmat[3]  * recur[rID+0])  + (tmat[7]  * recur[rID+1])  +
				              (tmat[11] * recur[rID+2]);
			tmp[topID+7]  = (tmat[3]  * recur[rID+4])  + (tmat[7]  * recur[rID+5])  +
				              (tmat[11] * recur[rID+6]);
			tmp[topID+11] = (tmat[3]  * recur[rID+8])  + (tmat[7]  * recur[rID+9])  +
				              (tmat[11] * recur[rID+10]);
			tmp[topID+15] = (tmat[3]  * recur[rID+12]) + (tmat[7]  * recur[rID+13]) +
				              (tmat[11] * recur[rID+14]);
		} else {
			memset(tmp+topID, 0, ns*ns*sizeof(*tmp));
			for (i=0; i<ns; i++) {
				for (j=0; j<ns; j++) {
					for (k=0; k<ns; k++) {
						if (i != k) {
							tmp[topID+i+(ns*j)] += tmat[i+(ns*k)] * recur[rID+k+(ns*j)];
						}
					}
				}
			}
		}
		
		/* prev = recur[t,,]. */
		rID = (rl - 1) * ns * ns;
		memcpy(tmp+prevID, recur+rID, ns*ns*sizeof(*tmp));

		/* recur[t,,] = top + A.  A not 0 along diagonal. */
		if (!RL_GENERIC && (2 == ns)) {
			/* recur[t,,] = top + A.  A not 0 for tmatID 0, 3. */
			recur[rID+0] = tmp[topID+0] + tmat[0];
			recur[rID+1] = tmp[topID+1];
			recur[rID+2] = tmp[topID+2];
			recur[rID+3] = tmp[topID+3] + tmat[3];
		} else if (!RL_GENERIC && (3 == ns)) {
			memcpy(recur+rID, tmp+topID, ns*ns*sizeof(*recur));
			recur[rID+0] += tmat[0];
			recur[rID+4] += tmat[4];
			recur[rID+8] += tmat[8];
		} else if (!RL_GENERIC && (4 == ns)) {
			memcpy(recur+rID, tmp+topID, ns*ns*sizeof(*recur));
			recur[rID+0]  += tmat[0];
			recur[rID+5]  += tmat[5];
			recur[rID+10] += tmat[10];
			recur[rID+15] += tmat[15];
		} else {
			memcpy(recur+rID, tmp+topID, ns*ns*sizeof(*recur));
			for (i=0,ij=0; i<ns; i++,ij+=ns+1) {
				recur[rID+ij] += tmat[ij];
			}
		}

		for (k=rl-2,rID=(rl-2)*ns*ns; 0<=k; k--,rID-=ns*ns) {
			/* curr = top + (A * prev).  A not zero for tmatID along diagonal. */
			if (!RL_GENERIC && (2 == ns)) {
				tmp[currID+0] = tmp[topID+0] + (tmat[0] * tmp[prevID+0]);
				tmp[currID+1] = tmp[topID+1] + (tmat[3] * tmp[prevID+1]);
				tmp[currID+2] = tmp[topID+2] + (tmat[0] * tmp[prevID+2]);
				tmp[currID+3] = tmp[topID+3] + (tmat[3] * tmp[prevID+3]);
			} else if (!RL_GENERIC && (3 == ns)) {
				tmp[currID+0] = tmp[topID+0] + (tmat[0] * tmp[prevID+0]);
				tmp[currID+1] = tmp[topID+1] + (tmat[4] * tmp[prevID+1]);
				tmp[currID+2] = tmp[topID+2] + (tmat[8] * tmp[prevID+2]);
				tmp[currID+3] = tmp[topID+3] + (tmat[0] * tmp[prevID+3]);
				tmp[currID+4] = tmp[topID+4] + (tmat[4] * tmp[prevID+4]);
				tmp[currID+5] = tmp[topID+5] + (tmat[8] * tmp[prevID+5]);
				tmp[currID+6] = tmp[topID+6] + (tmat[0] * tmp[prevID+6]);
				tmp[currID+7] = tmp[topID+7] + (tmat[4] * tmp[prevID+7]);
				tmp[currID+8] = tmp[topID+8] + (tmat[8] * tmp[prevID+8]);
			} else if (!RL_GENERIC && (4 == ns)) {
				tmp[currID+0]  = tmp[topID+0]  + (tmat[0]  * tmp[prevID+0]);
				tmp[currID+1]  = tmp[topID+1]  + (tmat[5]  * tmp[prevID+1]);
				tmp[currID+2]  = tmp[topID+2]  + (tmat[10] * tmp[prevID+2]);
				tmp[currID+3]  = tmp[topID+3]  + (tmat[15] * tmp[prevID+3]);
				tmp[currID+4]  = tmp[topID+4]  + (tmat[0]  * tmp[prevID+4]);
				tmp[currID+5]  = tmp[topID+5]  + (tmat[5]  * tmp[prevID+5]);
				tmp[currID+6]  = tmp[topID+6]  + (tmat[10] * tmp[prevID+6]);
				tmp[currID+7]  = tmp[topID+7]  + (tmat[15] * tmp[prevID+7]);
				tmp[currID+8]  = tmp[topID+8]  + (tmat[0]  * tmp[prevID+8]); 
				tmp[currID+9]  = tmp[topID+9]  + (tmat[5]  * tmp[prevID+9]);
				tmp[currID+10] = tmp[topID+10] + (tmat[10] * tmp[prevID+10]);
				tmp[currID+11] = tmp[topID+11] + (tmat[15] * tmp[prevID+11]);
				tmp[currID+12] = tmp[topID+12] + (tmat[0]  * tmp[prevID+12]);
				tmp[currID+13] = tmp[topID+13] + (tmat[5]  * tmp[prevID+13]);
				tmp[currID+14] = tmp[topID+14] + (tmat[10] * tmp[prevID+14]);
				tmp[currID+15] = tmp[topID+15] + (tmat[15] * tmp[prevID+15]);
			} else {
				for (j=0,ij=0; j<ns; j++) {
					for (i=0; i<ns; i++,ij++) {
						tmp[currID+ij] = tmp[topID+ij] + (tmat[i*(ns+1)] * tmp[prevID+ij]);
					}
				}
			}
			
			/* prev = recur[k,,]. */
			memcpy(tmp+prevID, recur+rID, ns*ns*sizeof(*tmp));
			/* recur[k,,] = curr. */
			memcpy(recur+rID, tmp+currID, ns*ns*sizeof(*tmp));
		}
	}

	prl = 0.0;
	for (j=0, rID=0; j<ns; j++,rID++) {
		for (i=0,rowsum=0.0; i<ns; i++) {
			rowsum+=recur[rID+(i*ns)];
		}
		prl += smat[j] * rowsum;
	}
	
	return ScalarReal(1.0 - prl);
}
	

/***
    impl_altperm:  Work function for altperm_symbols that creates a 
                   permutation of symbol identifiers (per position in the
                   argument, a vector of the counts of each symbol) such that
                   no same symbols are adjacent.  The returned vector contains
                   these identifiers, 1-based.
    args:          Rngrp - number of each symbol (real or integer vector)
    returns:   permuted symbol identifiers
***/
SEXP impl_altperm(SEXP Rngrp) {
	SEXP Rs;                              /* result */
	int *ngrp;                            /* contents of Rngrp */
	double *unif;                         /* uniform variate to pick symbols */
	double *rd;                           /* contents of Rngrp if real */
	int *s;                               /* permuted symbols */
	int *ri;                              /* contents of Rngrp if integer */
	int scnt;                             /* total symbols to allocate */
	int scnt_orig;                        /* copy of scnt */
	int adjcnt;                           /* total without previous symbol */
	int sID;                              /* index in s/Rs */
	int nsymb;                            /* number of symbols/length Rngrp */
	int smax;                             /* which symbol has most remaining */
	int u;                                /* unif over available groups */
	int i;

	ngrp = NULL;
	unif = NULL;
	s = NULL;
	
	nsymb = length(Rngrp);

	ngrp = (int *) R_alloc(nsymb, sizeof(*ngrp));
	if (isReal(Rngrp)) {
		rd = REAL(Rngrp);
		for (i=0,scnt=0; i<nsymb; i++) {
			if (!R_finite(rd[i])) {
				error("symbol count for permutation must have finite values");
			}
			ngrp[i] = (int) round(rd[i]);
			scnt += ngrp[i];
		}
	} else if (isInteger(Rngrp)) {
		ri = INTEGER(Rngrp);
		for (i=0,scnt=0; i<nsymb; i++) {
			if (NA_INTEGER == ri[i]) {
				error("symbol count for permutation cannot contain NA");
			}
			ngrp[i] = ri[i];
			scnt += ngrp[i];
		}
	} else {
		error("internal error - unsupported type for Rngrp");
	}
	adjcnt = scnt;
	sID = scnt - 1;
	
	unif = (double *) R_alloc(scnt, sizeof(*unif));
	GetRNGstate();
	for (i=0; i<scnt; i++) {
		unif[i] = unif_rand();
	}
	PutRNGstate();

	s = (int *) R_alloc(scnt+1, sizeof(*s));
	memset(s, 0, scnt*sizeof(*s));
	/* This is a barrier so we don't need to treat the first draw differently.
	   We will not copy into the result. */
	s[i] = -1;

	scnt_orig = scnt;
	while (0 < scnt) {
		smax = 0;
		/* Should pick a symbol randomly if more than one max. */
		for (i=1; i<nsymb; i++) {
			if (ngrp[smax] < ngrp[i]) {
				smax = i;
			}
		}

		if (((scnt % 2) && (((scnt + 1) / 2) < ngrp[smax])) ||
		    (!(scnt % 2) && ((scnt / 2) < ngrp[smax]))) {
			error("cannot generate alternating permutation (symbol imbalance)");
		} else if (!(scnt % 2) && ((scnt / 2) == ngrp[smax])) {
			if (s[scnt] == smax) {
				u = (int) round((unif[sID] * (scnt - ngrp[smax])) + 0.5);
				while ((s[sID] < (nsymb - 1)) &&
				       ((s[sID] == s[scnt]) ||
				        ((s[sID] != s[scnt]) && (ngrp[s[sID]] < u)))) {
					if (s[sID] != s[scnt]) {
						u -= ngrp[s[sID]];
					}
					s[sID] += 1;
				}
			} else {
				s[sID] = smax;
			}
		} else if ((scnt % 2) && (((scnt + 1) / 2) == ngrp[smax])) {
			s[sID] = smax;
		} else if ((scnt % 2) && (((scnt - 1) / 2) == ngrp[smax])) {
			adjcnt = scnt - ngrp[s[scnt]];
			u = (int) round((unif[sID] * adjcnt) + 0.5);
			while ((s[sID] < (nsymb - 1)) &&
			       ((s[sID] == s[scnt]) || (s[sID] == smax) ||
			        ((s[sID] != s[scnt]) && (s[sID] != smax) &&
			         (ngrp[s[sID]] < u)))) {
				if ((s[sID] != s[scnt]) && (s[sID] != smax)) {
					u -= ngrp[s[sID]];
				}
				s[sID] += 1;
			}
		} else {
			u = (int) round((unif[sID] * adjcnt) + 0.5);
			while ((s[sID] < (nsymb - 1)) &&
			       ((s[sID] == s[scnt]) ||
			        ((s[sID] != s[scnt]) && (ngrp[s[sID]] < u)))) {
				if (s[sID] != s[scnt]) {
					u -= ngrp[s[sID]];
				}
				s[sID] += 1;
			}
		}
		
		if ((s[sID] < 0) || (nsymb <= s[sID])) {
			error("internal error - symbol assignment OOB");
		}
		ngrp[s[sID]] -= 1;
		if (ngrp[s[sID]] < 0) {
			error("internal error - sent group count negative");
		}
		scnt -= 1;
		sID -= 1;
		adjcnt = scnt - ngrp[s[scnt]];
		
		R_CheckUserInterrupt();
	}
	scnt = scnt_orig;

	/* Shift to R indexing. */
	for (i=0; i<scnt; i++) {
		s[i] += 1;
	}

	Rs = PROTECT( allocVector(INTSXP, scnt) );
	ri = INTEGER(Rs);
	memcpy(ri, s, sizeof(*ri)*scnt);

	UNPROTECT(1);
	return Rs;
}

/***
    impl_altperm_2symb:  Work function for altperm_symbols when there are two
                         different, creating a permutation such that no two
                         adjacent are the same.  Raises an error if this is
                         not possible.  The returned vectors contains the
                         symbol identifiers (position in Rngrp), 1-based.
    args:                Rngrp - number of each symbol (real or integer vector)
    returns:   permuted symbol identifiers
***/
SEXP impl_altperm_2symb(SEXP Rngrp) {
	SEXP Rs;                              /* result */
	double *rd;                           /* contents of Rngrp if real */
	double u;                             /* random number */
	int *ri;                              /* contents of Rngrp if integer */
	int n1;                               /* count of first symbol */
	int n2;                               /* count of second symbol */
	int s;                                /* symbol placed in Rs */
	int i;

	if (isReal(Rngrp)) {
		rd = REAL(Rngrp);
		if (!R_finite(rd[0]) || !R_finite(rd[1])) {
			error("symbol count for permutation must have finite values");
		}
		n1 = (int) round(rd[0]);
		n2 = (int) round(rd[1]);
	} else if (isInteger(Rngrp)) {
		ri = INTEGER(Rngrp);
		if ((NA_INTEGER == ri[0]) || (NA_INTEGER == ri[1])) {
			error("symbol count for permutation cannot contain NA");
		}
		n1 = ri[0];
		n2 = ri[1];
	} else {
		error("unsupported type for symbol counts");
	}

	/* Determine initial symbol, the more populous, or random if even. */
	if (n1 == n2) {
		GetRNGstate();
		u = unif_rand();
		PutRNGstate();
		if (u < 0.5) {
			s = 1;
		} else {
			s = 2;
		}
	} else if ((n1 + 1) == n2) {
		s = 2;
	} else if (n1 == (n2+1)) {
		s = 1;
	} else {
		error("no alternating permutation possible because of symbol imbalances");
	}

	Rs = PROTECT( allocVector(INTSXP, n1+n2) );
	ri = INTEGER(Rs);
	for (i=0; i<(n1+n2); i++) {
		ri[i] = s;
		/* This toggles between 1 and 2.  Note 1 based. */
		s = 3 - s;
	}

	UNPROTECT(1);
	return Rs;
}

/***
    impl_excursion: Work function for the excursion test that pulls ndraw 
                    values from Rdata, with replacement, reconstructs the
                    signal, and returns the height of each of nexcur runs
    args:           Rdata - (differential) values to draw from
                    nexcur - number of excursions to perform
                    ndraw - size of each excursion
                    ispeak - 0 for flat height, true for peak
                    dosort - 0 to not sort results, true to do it
    returns:   vector of heights from each excursion
***/
SEXP impl_excursion(SEXP Rdata, int nexcur, int ndraw, int ispeak, int dosort) {
	SEXP Rht;                             /* excursion height */
	double *ht;                           /* height of draw */
	double *rd;                           /* data if double */
	int *ri;                              /* data if integer */
	double xcum;                          /* cumulative draw */
	double xinit;                         /* first cumulative value */
	double xmin, xmax;                    /* largest/smallest of xcum */
	int ndata;                            /* amount of data */
	int xID;                              /* sample index */
	int i, j;
#ifndef NO_PCGRNG
	pcg32_random_t rng1;                  /* random number stream */
	pcg32_random_t rng2;                  /* another random number stream */
	uint64_t pcgnx;                       /* draw size for PCG scaling */
	uint64_t pcgthr;                      /* threshold for bounded draw */
	uint64_t pcgdraw;                     /* bounded and combined RNG draw */
#endif     /* NO_PCGRNG */	

	Rht = PROTECT( allocVector(REALSXP, nexcur) );
	ht = REAL(Rht);
	
	if (isReal(Rdata)) {
		rd = REAL(Rdata);
		ri = NULL;
	} else if (isInteger(Rdata)) {
		rd = NULL;
		ri = INTEGER(Rdata);
	} else {
		error("unsupported data type");
	}
	
	ndata = length(Rdata);

	/* Timing experiments show the PCG algorithm is 5-6x faster than
	   R_unif_index.  Using the dqrng package interface to the library
	   is 3x faster than R.  Its sample.int function is 10-11x, but 
	   it generates much different probabilities of an excursion. */

	/* Excursions depend on the draw size.  Switching to supports from
	   minima can deliver large savings, up to 40% in testing. */

	GetRNGstate();
#ifndef NO_PCGRNG
	xID = floor(ndata * unif_rand());
	/* In case the 128 bit generators aren't available, we stitch together
	   two 32 bit and do the bounded draw inside the loop.  Note the PCG
	   RNG are initialized by the R seed.  Code based on the pcg32x2-demo. */
	pcg32_srandom_r(&rng1, xID, 11u);
	pcg32_srandom_r(&rng2, xID, 29u);
	pcgnx = (uint64_t) ndata;
	pcgthr = - pcgnx % (uint64_t) pcgnx;
#endif     /* NO_PCGRNG */

	for (i=0; i<nexcur; i++) {
		xcum = 0.0;
		xinit = 0.0;
		xmax = 0.0;
		xmin = 0.0;
		for (j=0; j<ndraw; j++) {
#ifdef NO_PCGRNG
			/* This is with replacement, just a uniform variate scaled to ndata.
			   We use the R_unif_index interface per the R mailing discussion
			   when the data size approaches the point where the RNG resolution
			   can become a problem, at ca. 2^25, but for small data go with
			   unif_rand which is 4 times faster.  Note the RNG sequences will
			   differ between the two. */
			if (ndata < ((1 << 25) / (double) nexcur)) {
				xID = floor(ndata * unif_rand());
			} else {
				xID = R_unif_index(ndata);
			}
#else
			/* But the PCG library is at least 5x faster than R_unif_index. */
			for (;;) {
				pcgdraw = (((uint64_t) pcg32_random_r(&rng1)) << 32) |
					         ((uint64_t) pcg32_random_r(&rng2));
				if (pcgthr <= pcgdraw) {
					xID = (int) (pcgdraw % pcgnx);
					break;
				}
			}
#endif     /* NO_PCGRNG */
			
			if (NULL != rd) {
				xcum += rd[xID];
			} else {
				xcum += ri[xID];
			}
			if (0 == j) {
				xmax = xcum;
				xmin = xcum;
				xinit = xcum;
			} else {
				if (xmax < xcum) {
					xmax = xcum;
				}
				if (xcum < xmin) {
					xmin = xcum;
				}
			}
		}
		if (ispeak) {
			if (xcum < xinit) {
				ht[i] = xmax - xcum;
			} else {
				ht[i] = xmax - xinit;
			}
		} else {
			ht[i] = xmax - xmin;
		}

		R_CheckUserInterrupt();
	}

	if (dosort) {
		R_qsort(ht, 1, nexcur);
	}

	PutRNGstate();
	
	UNPROTECT(1);
	return Rht;
}

/***
    impl_htprob:  Work function to determine position of heights in
                  distribution, returning the number of distribution values 
                  <= height(s) if lower.tail true, > if 0.  Places in 
                  mid-distribution if distribution values tied (per eps=0
                  in find_runs).
    args:         Rdist - excursion/permutation distribution of heights
                  Rhts - test statistic(s)/height(s)
                  lotail - true to count dist <= ht, 0 >
    returns:   vector of length Rhts with probability of height
***/
SEXP impl_htprob(SEXP Rdist, SEXP Rhts, int lotail) {
	SEXP Rhtprob;                         /* probability of height per dist */
	SEXP Rruns;                           /* Rdist converted to runs */
	SEXP Rrl;                             /* run lengths within Rruns */
	SEXP Rnskip;                          /* skip counts within Rruns */
	SEXP Rstats;                          /* statistics of Rruns */
	SEXP Rn;                              /* member of Rstats */
	double *pht;                          /* probability of height */
	double *duniq;                        /* unique distribution values */
	double *rd;                           /* double array from R */
	int *ri;                              /* integer array from R */
	int *rl;                              /* run lengths */
	int *nskip;                           /* number non-finite elements in run */
	int *cnt;                             /* number of unique values */
	int *cumcnt;                          /* position along original dist */
	double h;                             /* height to process */
	double htcnt;                         /* number dist values vs h */
	int nht;                              /* number of heights to process */
	int nrl;                              /* length of Rrl/original data */
	int nuniq;                            /* number unique dist values */
	int ndist;                            /* number of original dist values */
	int lo, hi;                           /* search bounds */
	int mid;                              /* search point */
	int i, j;
	
	Rhtprob = NULL;
	pht = NULL;
	rd = NULL;
	ri = NULL;
	duniq = NULL;
	cnt = NULL;
	cumcnt = NULL;

	ndist = length(Rdist);

	/* Here's where we assume the distribution values are sorted, otherwise
	   we would have to deal with that ourselves.  Unpacking this is a PITA.
	   If we didn't handle tied values with runs the logic for the search,
	   and especially the mid-distribution interval, would be more annoying. */
	Rruns = PROTECT( impl_runs(Rdist, 0.0) );
	Rrl = PROTECT( VECTOR_ELT(Rruns, RUNID_RUNS) );
	rl = INTEGER(Rrl);
	nrl = length(Rrl);
	Rnskip = PROTECT( VECTOR_ELT(Rruns, RUNID_SKIP) );
	nskip = INTEGER(Rnskip);
	Rstats = PROTECT( VECTOR_ELT(Rruns, RUNID_STATS) );
	Rn = PROTECT( VECTOR_ELT(Rstats, RUNID_STAT_NRUN) );
	nuniq = asInteger(Rn);

	if (isReal(Rdist)) {
		rd = REAL(Rdist);
		ri = NULL;
	} else if (isInteger(Rdist)) {
		rd = NULL;
		ri = INTEGER(Rdist);
	} else {
		error("unsupported type for Rdist");
	}
	
	duniq = (double *) R_alloc(nuniq, sizeof(*duniq));
	cnt = (int *) R_alloc(nuniq, sizeof(*cnt));
	cumcnt = (int *) R_alloc(nuniq, sizeof(*cumcnt));
	if (0 == rl[0]) {
		i = 0;
	} else {
		i = nskip[0];
	}
	j = 0;
	while ((i < nrl) && (j < nuniq)) {
		if (rd) {
			duniq[j] = rd[i];
		} else {
			duniq[j] = (double) ri[i];
		}
		cnt[j] = rl[i];
		/* Note the cumulative count does not include the current bin. */
		cumcnt[j] = ((0 == j) ? 0 : cumcnt[j-1] + cnt[j-1]);
		i += rl[i] + nskip[i];
		j += 1;
	}

	nht = length(Rhts);
	Rhtprob = PROTECT( allocVector(REALSXP, nht) );
	pht = REAL(Rhtprob);
	
	if (isReal(Rhts)) {
		rd = REAL(Rhts);
		ri = NULL;
	} else if (isInteger(Rhts)) {
		rd = NULL;
		ri = INTEGER(Rhts);
	} else {
		error("unsupported type for heights");
	}

	for (i=0; i<nht; i++) {
		if (rd) {
			if (!R_finite(rd[i])) {
				pht[i] = NA_REAL;
				continue;
			}
			h = rd[i];
		} else {
			if (NA_INTEGER == rd[i]) {
				pht[i] = NA_REAL;
				continue;
			}
			h = (double) ri[i];
		}

		lo = 0;
		hi = nuniq - 1;
		/* First handle edge cases, distribution all h or h outside. */
		if ((fabs(duniq[hi] - h) < DBL_EPSILON) &&
		    (fabs(h - duniq[lo]) < DBL_EPSILON)) {
			/* Mid-quantile approximation is half of distribution. */
			pht[i] = 0.5;
			continue;
		} else if ((h < duniq[lo]) && (fabs(h - duniq[lo]) > DBL_EPSILON)) {
			pht[i] = (lotail ? 0 : 1.0);
			continue;
		} else if ((h > duniq[hi]) && (fabs(h - duniq[hi]) > DBL_EPSILON)) {
			pht[i] = (lotail ? 1.0 : 0);
			continue;
		}

		while (lo <= hi) {
			mid = (hi + lo) / 2;
			if (fabs(duniq[mid] - h) < DBL_EPSILON) {
				break;
			} else if (duniq[mid] < h) {
				lo = mid + 1;
			} else {
				hi = mid - 1;
			}
		}

		if (lo > hi) {
			if (lotail) {
				/* No match, lo above h, so lower tail does not include. */
				pht[i] = cumcnt[lo] / (double) ndist;
			} else {
				/* Upper does, so not ndist-1. */
				pht[i] = (ndist - cumcnt[lo]) / (double) ndist;
			}
		} else {
			/* This is a half-grid mid-quantile function. */
			if ((1 == cnt[mid]) ||
			    (fabs(h - duniq[lo]) < DBL_EPSILON) ||
			    (fabs(duniq[hi] - h) < DBL_EPSILON)) {
				htcnt = cumcnt[mid] + (cnt[mid] / 2.0);
				/* Lower tail here does not include midpoint. */
				pht[i] = (lotail ? htcnt : ndist - htcnt) / (double) ndist;
			} else {
				lo = mid - 1;
				if (lo < 0) {
					lo = 0;
				}
				hi = mid + 1;
				if (nuniq <= hi) {
					hi = nuniq - 1;
				}
				htcnt =
					((h - duniq[lo]) * (cnt[mid] + 1.0) / (duniq[hi] - duniq[lo])) -0.5;
				if (htcnt < 0) {
					htcnt = 0;
				} else if (cnt[mid] < htcnt) {
					htcnt = cnt[mid];
				}
				htcnt += cumcnt[mid];
				/* Lower tail includes midpoint. */
				pht[i] = (lotail ? htcnt + 1 : ndist - 1 - htcnt) / (double) ndist;
			}
		}
	}

	UNPROTECT(6);
	return Rhtprob;
}

