
/*****
      utility.c -
      C support for utility functions.

      Compiled into shared library for R, not an executable.

      c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.
*****/


/**
   To Do:
   - 
**/

/*
  Verification of some data x for algorithm a:
    xmid <- .Call('C_midq_index', x, a)      # old version
    s <- .Call('C_midq', x, a, 0.0)          # old version had 0 eps
    xq <- .Call('C_eval_midq', s$x, s$q, ((1:length(x))-1)/(length(x)-1))
                                             # 0 based indices
    e <- ecdf(x) ; plot(x)
    lines(1:length(x), xmid, col='blue')
    points(1:length(x), xq, col='green')
    # red are endpoints, green on line between
    points((s$q*(length(x)-1))+1, s$x, col='red')   # back convert to 1 based

    err <- abs(xmid-xq) ; err[err<1e-14] <- 0 ; max(err)   # machine tol
*/


#include "detectors.h"
#include "utility.h"


/**** Macros/Defines ****/

/* choice of breaks b/t piecewise linear segments of mid-quantile */
#define MIDQ_MIDSTEP_X       1
#define MIDQ_MIDSTEP_Q       2
#define MIDQ_HALFGRID        3
#define MIDQ_MIDRUN          4

/* retrieve data at index _id stored either as double _xd or integer _xi, with
   the unused type NULL - data cast to double if needed */
#define ACCESS_XDI(_xd, _xi, _id)                            \
	((NULL != (_xd)) ? (_xd[_id]) : (double) (_xi[_id]))


/**** Structures ****/


/**** Prototypes ****/

/* actual work functions */
static SEXP impl_midq(SEXP, int, double);
static SEXP impl_eval_midq(double *, double *, int, double *, int);
static void segment_offgrid(SEXP, int *, int *, double, double, SEXP, SEXP);
static void segment_midrun(SEXP, int *, int *, SEXP, SEXP);



/**** Public Interface ****/

/***
    C_midq:  Determine the piecewise linear segments fitting data with
             ties, with endpoints per the algorithm.  If the algorithm
             does not match any of the MIDQ_ defines, draw segments through
             the middle of the runs.  Uses find.runs internally with eps
             the relative tolerated difference for real values - integers
             still match exactly.  The returned vector contains the
             endpoints of the segments, with open ends, ie. quantiles
             before the first or after the last tie to the first or last.
    args:    Rx - sorted data
             Ralgo - algorithm to assign knots between segments
             Rfeps - relative data difference for tied values
    returns:   vector of two vectors $x, $q the segment endpoints
***/
SEXP C_midq(SEXP Rx, SEXP Ralgo, SEXP Rfeps) {
	double feps;                          /* rel match tolerance for run */
	int algo;                             /* knot strategy */

	if (!isReal(Rx) && !isInteger(Rx)) {
		error("data for mid-quantile function must be real or integer");
	}
	if (length(Rx) < 3) {
		return Rx;
	}

	algo = asInteger(Ralgo);
	if (NA_INTEGER == algo) {
		error("mid-quantile interpolation strategy must be finite real or integer");
	}

	feps = asReal(Rfeps);
	
	return impl_midq(Rx, algo, feps);
}

/***
    C_eval_midq:  Return the data value at the quantile(s) using the piecewise
                  linear segments built by C_midqlut.  Quantiles outside the
                  segments, including impossible values < 0 or > 1, are taken
                  to the appropriate side.
    args:         Rxseg - PWL endpoints along data (midq()$x vector)
                  Rqseg - PWL endpoints along quantile (midq()$q vector)
                  Rq - quantiles to evaluate
    returns:   data from quantiles
***/
SEXP C_eval_midq(SEXP Rxseg, SEXP Rqseg, SEXP Rq) {
	double *xseg;                         /* contents of Rxseg */
	double *qseg;                         /* contents of Rqseg */
	double *q;                            /* contents of Rq */
	int nseg;                             /* number of segments */
	int nq;                               /* number of quantiles */

	if (!isReal(Rxseg) || !isReal(Rqseg) || (length(Rxseg) != length(Rqseg))) {
		error("segments not real values or lengths differ");
	}
	nseg = length(Rxseg);
	xseg = REAL(Rxseg);
	qseg = REAL(Rqseg);

	if (!isReal(Rq)) {
		error("quantiles must be real values");
	}
	nq = length(Rq);
	q = REAL(Rq);

	return impl_eval_midq(xseg, qseg, nseg, q, nq);
}



/**** Implementations ****/

/***
    impl_midq:  Fit the data with ties per find.run (and eps) with piecewise
                linear segments.
    args:       Rx - sorted data
                algo - interpolation strategy
                feps - relative difference between tied data points
    returns:   vector of two vectors $x, $q with the segment endpoints
***/
static SEXP impl_midq(SEXP Rx, int algo, double feps) {
	SEXP Rseg;                            /* segment endpoints */
	SEXP Rnames;                          /* element names for Rseg */
	SEXP Rxseg;                           /* segment endpoint along data */
	SEXP Rqseg;                           /* segment endpoint along quantile */
	SEXP Rruns;                           /* Rx converted to runs */
	SEXP Rrl;                             /* run lengths within data */
	SEXP Rnskip;                          /* non-finite skip counts */
	SEXP Rstats;                          /* runs statistics */
	SEXP Rnrun;                           /* run count member of Rstats */
	int *rl;                              /* contents of Rrl */
	int *nskip;                           /* contents of Rnskip */
	int nrun;                             /* number of runs */
	int nseg;                             /* number of segments */

	Rseg = NULL;
	Rnames = NULL;
	Rxseg = NULL;
	Rqseg = NULL;

	Rruns = PROTECT( impl_runs(Rx, feps) );
	Rrl = PROTECT( VECTOR_ELT(Rruns, RUNID_RUNS) );
	rl = INTEGER(Rrl);
	Rnskip = PROTECT( VECTOR_ELT(Rruns, RUNID_SKIP) );
	nskip = INTEGER(Rnskip);
	Rstats = PROTECT( VECTOR_ELT(Rruns, RUNID_STATS) );
	Rnrun = PROTECT( VECTOR_ELT(Rstats, RUNID_STAT_NRUN) );
	nrun = asInteger(Rnrun);

	Rseg = PROTECT( allocVector(VECSXP, 2) );

	switch(algo) {
	case MIDQ_MIDSTEP_X:   /* fall through */
	case MIDQ_MIDSTEP_Q:   /* fall through */
	case MIDQ_HALFGRID:
		nseg = nrun + 1;
		break;
	case MIDQ_MIDRUN:      /* fall through */
	default:
		nseg = nrun;
		break;
	}

	Rxseg = PROTECT( allocVector(REALSXP, nseg) );
	SET_VECTOR_ELT(Rseg, SEGID_X, Rxseg);

	Rqseg = PROTECT( allocVector(REALSXP, nseg) );
	SET_VECTOR_ELT(Rseg, SEGID_Q, Rqseg);

	Rnames = PROTECT( allocVector(STRSXP, 2) );
	SET_STRING_ELT(Rnames, SEGID_X, mkChar("x"));
	SET_STRING_ELT(Rnames, SEGID_Q, mkChar("q"));
	setAttrib(Rseg, R_NamesSymbol, Rnames);

	switch(algo) {
	case MIDQ_MIDSTEP_X:
		segment_offgrid(Rx, rl, nskip, 0.5, 0.0, Rxseg, Rqseg);
		break;
	case MIDQ_MIDSTEP_Q:
		segment_offgrid(Rx, rl, nskip, 0.0, 0.5, Rxseg, Rqseg);
		break;
	case MIDQ_HALFGRID:
		segment_offgrid(Rx, rl, nskip, 0.5, 0.5, Rxseg, Rqseg);
		break;
	case MIDQ_MIDRUN:
		/* fall through */
	default:
		segment_midrun(Rx, rl, nskip, Rxseg, Rqseg);
		break;
	}

	UNPROTECT(9);
	return Rseg;
}

/***
    impl_eval_midq:  Reconstruct the data at quantiles based on the piecewise
                     linear segments.  NA or NaN quantiles propagate, values
                     outside [0,1] map to 0 or 1.
    args:            xseg, qseg - segment endpoints (along data, quantiles)
                     nseg - number of segments
                     q - quantiles to evaluate
                     nq - number of quantiles
    returns:   data interpolated at quantiles
***/
static SEXP impl_eval_midq(double *xseg, double *qseg, int nseg,
                           double *q, int nq) {
	SEXP Rx;                              /* reconstructed data */
	double *x;                            /* contents of Rx */
	int i, j;

	Rx = PROTECT( allocVector(REALSXP, nq) );
	x = REAL(Rx);

	for (i=0,j=1; i<nq; i++) {
		if (ISNA(q[i]) || ISNAN(q[i])) {
			x[i] = q[i];
		} else if (q[i] <= qseg[0]) {
			x[i] = xseg[0];
		} else if (qseg[nseg-1] <= q[i]) {
			x[i] = xseg[nseg-1];
		} else {
			/* We search for the quantile among the segments.  Worst case this
			   is O(n^2), but that happens when the quantiles alternate between
			   starting and ending segments.  If quantiles are sorted, as is the
			   default, then this runs a little slower than linear time.
			   An alternative would be to sort q and then step upwards through
			   qseg - but this means using ordering indices into q rather than
			   sorting the array, and then back-mapping the midq. */
			while ((0 < j) && (q[i] < qseg[j-1])) {
				j -= 1;
			}
			if (0 == j) {
				error("cannot place quantile looking towards start");
			}
			while ((j < nseg) && (qseg[j] < q[i])) {
				j += 1;
			}
			if (j == nseg) {
				error("cannot place quantile looking towards end");
			}

			if ((qseg[j-1] <= q[i]) && (q[i] <= qseg[j])) {
				/* Is it worth pre-calculating the segment components? */
				x[i] = xseg[j-1] +
					     ((q[i]-qseg[j-1]) * (xseg[j]-xseg[j-1]) / (qseg[j]-qseg[j-1]));
			} else {
				error("bad search: quantile does not end up in segment");
			}
		}
	}

	UNPROTECT(1);
	return Rx;
}

/***
    segment_offgrid:  Build the endpoints of the piecewise linear segments
                      through runs in the data, shifting to the half grid 
                      along either the data (dx) and/or quantile/index (dq).
    args:             Rx - data that runs based on
                      rl - run lengths
                      nskip - number of skipped elements per run
                      dx - grid shift when data changes (0 or 0.5 for average)
                      dq - grid shift when index changes (0 or 0.5 delta)
                      Rxseg - data value at segment endpoint
                      Rqseg - quantile/index value at segment endpoint
    modifies:  contents of Rxseg, Rqseg already allocated
***/
static void segment_offgrid(SEXP Rx, int *rl, int *nskip, double dx, double dq,
                            SEXP Rxseg, SEXP Rqseg) {
	double *xd;                           /* Rx payload if real/double */
	double *xseg;                         /* vector within Rxseg */
	double *qseg;                         /* vector within Rqseg */
	int *xi;                              /* Rx payload if integer */
	double xl, xr;                        /* values of pairs of runs */
	int l, r;                             /* start of pairs of runs */
	int nx;                               /* length of data */
	int nseg;                             /* length of endpoints */
	int i;

	nx = length(Rx);
	if (isReal(Rx)) {
		xd = REAL(Rx);
		xi = NULL;
	} else if (isInteger(Rx)) {
		xd = NULL;
		xi = INTEGER(Rx);
	} else {
		error("unsupported data type");
	}

	nseg = length(Rxseg);
	xseg = REAL(Rxseg);
	qseg = REAL(Rqseg);

	l = (0 == rl[0]) ? 0 : nskip[0];
	xl = ACCESS_XDI(xd, xi, l);

	xseg[0] = xl;
	/* -1 shifts indices to 0 based indexing.  Must also shift maximmum, nx. */
	qseg[0] = (l - 1.0 + dq) / (nx - 1.0);
	i = 1;
	
	while ((i < nseg) && (l < nx)) {
		r = l + rl[l] + nskip[l];
		xr = ACCESS_XDI(xd, xi, (nx <= r) ? nx-1 : r);
		xseg[i] = xl + (dx * (xr - xl));
		qseg[i] = (r - 1.0 + dq) / (nx - 1.0);
		i += 1;

		l = r;
		xl = xr;
	}
	
	if ((i < nseg) || (l < nx)) {
		error("did not register all expected segments");
	}
}

/***
    segment_run:  Build the endpoints of the piecewise linear segments that
                  hit the middle of runs, or single data points.
    args:         Rx - data behind runs
                  rl - run lengths
                  nskip - number of skipped elements per run
                  Rxseg - data value at segment endpoint
                  Rqseg - quantile/index value at segment endpoint
    modifies:  contents of Rxseg, Rqseg
***/
static void segment_midrun(SEXP Rx, int *rl, int *nskip,
                           SEXP Rxseg, SEXP Rqseg) {
	double *xd;                           /* Rx payload if real/double */
	double *xseg;                         /* vector within Rxseg */
	double *qseg;                         /* vector within Rqseg */
	int *xi;                              /* Rx payload if integer */
	double xl, xr;                        /* values of pairs of runs */
	int l, r;                             /* start of pairs of runs */
	int nx;                               /* length of data */
	int nseg;                             /* length of endpoints */
	int i;

	nx = length(Rx);
	if (isReal(Rx)) {
		xd = REAL(Rx);
		xi = NULL;
	} else if (isInteger(Rx)) {
		xd = NULL;
		xi = INTEGER(Rx);
	} else {
		error("unsupported data type");
	}

	nseg = length(Rxseg);
	xseg = REAL(Rxseg);
	qseg = REAL(Rqseg);

	l = (0 == rl[0]) ? 0 : nskip[0];
	xl = ACCESS_XDI(xd, xi, l);

	i = 0;
	while ((i < nseg) && (l < nx)) {
		r = l + rl[l] + nskip[l];
		xr = ACCESS_XDI(xd, xi, r);
		xseg[i] = xl;
		qseg[i] = (l + ((rl[l] - 1) / 2.0)) / (nx - 1.0);
		i += 1;

		l = r;
		xl = xr;
	}

	if ((i < nseg) || (l < nx)) {
		error("did not register all expected segments");
	}
}
