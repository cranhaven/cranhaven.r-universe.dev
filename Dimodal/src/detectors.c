
/*****
      detectors.c -
      Feature detectors - runs, peaks, flats, level sections.  These work in
      general on arbitrary data.

      Compiled into shared library for R, not an executable.

      c 2024-2025 Greg Kreider, Primordial Machine Vision Systems, Inc.
*****/


/**
   To Do:
   - 
**/


#include <math.h>
#include <float.h>
#include "detectors.h"


/**** Macros/Defines ****/

/* bits in mmdata.flags
     ISMAX         set if extremum a maximum, clear if minimum
     LMINHT        set if minht to left minimum, clear if to right
     LRELHT        set if relht to left minimum, clear if to right
     KEEP          set if extremum valid, clear if fails height requirements
     ATEND         set for first and last maxima not necessarily at data limits
*/
#define MM_ISMAX          0x01
#define MM_LMINHT         0x02
#define MM_LRELHT         0x04
#define MM_KEEP           0x08
#define MM_ATEND          0x10

/* bits in level sections flags
     LVL_CORR      set for short section correction of critical value
     LVL_MPOS      positive slope
     LVL_MNEG      negative slope
     LVL_SLOPED    either LVL_M* bit
*/
#define LVL_CORR          0x01
#define LVL_MPOS          0x02
#define LVL_MNEG          0x04
#define LVL_SLOPED        (LVL_MPOS | LVL_MNEG)

/* how mmtree range compares to the ripple range
     RNG_OOB       index does not fit into level vector
     RNG_OUTSIDE   mmtree range does not overlap the ripple
     RNG_COVER     mmtree range (ie. both endpoints) within ripple
     RNG_OVERLAP   mmtree range includes one ripple endpoint
     RNG_INSIDE    ripple (ie. both endpoints) within mmtree range
*/
#define RNG_OOB           1
#define RNG_OUTSIDE       2
#define RNG_COVER         3
#define RNG_OVERLAP       4
#define RNG_INSIDE        5

/* Relative difference between values v1 and v2.  This uses the arguments 
   multiple times, so avoid side-effects.  This is sensitive to offsets or
   changes to the average.  (|x1|+|x2|)/2 for mean is meant to handle 
   zero-crossing, although this shouldn't happen with spacing. */
#define RELHT(_v1, _v2)                \
	fabs(2.0 * ((_v1) - (_v2)) / (fabs(_v1) + fabs(_v2)))


/**** Structures ****/

typedef unsigned char uchar;

struct mmdata {
	int pos;                              /* mid-run index in raw data */
	int runID;                            /* run number with extremum */
	int lID;                              /* index to extremum to left */
	int rID;                              /* index to extremum to right */
	int flags;                            /* extremum information */
	double valsd;                         /* data, standardized */
	double minht;                         /* smallest height to adj minimum */
	double relht;                         /* smallest relative ht diff */
};

struct mmtree {
	double *tmin;                         /* minimum segtree */
	double *tmax;                         /* maximum segtree */
	int ix;                               /* first index of bottom row/data */
	int nlvl;                             /* number levels in tree, incl. data */
	int nx;                               /* length of data */
};

struct flatsrc {
	int srcID;                            /* index of seed for flat range */
	int stID;                             /* starting index of flat */
	int endID;                            /* ending index (incl.) of flat */
	int len;                              /* length of flat */
};

struct msdstats {
	double mu;                            /* mean of data */
	double sigma;                         /* standard deviation */
	double xmin;                          /* smallest value */
	double xmax;                          /* largest value */
	int ndata;                            /* length data (may not be raw len) */
};


/**** Prototypes ****/

/* actual work functions - impl_runs prototyped in header */
static SEXP impl_peaks(SEXP, double, double, double, double);
static SEXP impl_flats_scan(SEXP, double, int, double, int);
static SEXP impl_flats_mmtree(SEXP, double, int, double, int);
static SEXP impl_lvlsec(double *, int, double, int);
static double lvlsec_critval(int, double, int);
/* data statistics/standardization */
static void stats_meansd(SEXP, struct msdstats *);
static void standardize(SEXP, struct msdstats *, double *, int *);
/* peak handling */
static inline void drop_endmax(struct mmdata *, int, double, double);
static void set_heights(struct mmdata *, int);
static SEXP build_peaks(double *, int *, struct mmdata *, int, double);
/* htheap for local peaks */
static void build_htheap(struct mmdata *, int, int *, int *);
static void del_htheap(int *, int *, int, struct mmdata *);
static int  find_htheapID(int *, int, int);
static inline void htheapify(int *, int, int, struct mmdata *);
/* flat handling */
static inline int scan_out(double *, int, int, double, double, int);
static int  sortfn_flats(const void *, const void *);
static void identify_flats_cover(struct flatsrc *, int, int, int *, int *);
static SEXP build_flats(double *, struct msdstats *, double, struct flatsrc *,
                        int *, int, int *);
/* mmsegtree and scanning for local flats */
static void build_mmtree(double *, int, struct mmtree *);
static int  walk_mmtree(struct mmtree *, int, int, double, double, int);
static inline int match_range(struct mmtree *, int, int, double, double);
static inline int same_level(int, int);
/* built-in self tests */
SEXP bist_flats(void);
static int  bist_mmtree(void);
static int  test_mmtree_allsrc(const char *, double *, int, int, int);
static int  test_mmtree(double *, int, int,  int, int);
static int  bist_build_flats(void);
static void test_init_flats(struct flatsrc *, int);
static void test_set_flat(struct flatsrc *, int, int, int);
static int  compare_flats(const char *, int *, int, int *, int);



/**** Public Interface ****/

/***
    C_find_runs:  Look for sequences of identical values in data x, treating
                  pairs whose difference is less than the fraction feps of
                  their average as the same (values w/i double precision also
                  treated as equal).  Ignores NA and NaN values and treats
                  infinities as equal.  Works with real, integer, logical,
                  and string vectors.  Returns a vector the same length as Rx
                  with the length of each run stored at the first value in
                  the run, and 0 else, with added attributes 'stID' the
                  position of the first run, 'nrun' the number of runs, and
                  'maxrun' the longest.
    args:         Rx - data
                  Rfeps - fractional difference allowed to match
    returns:   vector of run lengths at starting positions, plus attributes
***/
SEXP C_find_runs(SEXP Rx, SEXP Rfeps) {
	double feps;                          /* fractional diff for equality */

	/* Extending this to integer, logical, and character strings would not
	   be difficult, essentially simplifying the inner test in impl_runs.
	   The type handling would be a bigger PITA. */
	if (!isReal(Rx) && !isInteger(Rx) && !isLogical(Rx) && !isString(Rx)) {
		error("data type not supported");
	}

	if (!isReal(Rfeps)) {
		error("epsilon argument not a double");
	}
	feps = asReal(Rfeps);
	if ((feps < 0) || (1.0 < feps)) {
		error("feps must be between 0 and 1 (excl.)");
	}

	return impl_runs(Rx, feps);
}

/***
    C_find_peaks:  Look for local maxima and minima, eliminating those that
                   are too small per the absolute height fht (as fraction of 
                   the total data range) or relative frelht (as local about 
                   peak), considering values within relatie fhtie as the same.
    args:          Rx - data, double or integer
                   Rfht - absolute fractional minimum height
                   Rrelht - relative fractional minimum height
                   Rfhtie - fractional height for tied values
                   Rfhsupp - fraction of peak height for support width
    returns:   matrix with peaks
***/
SEXP C_find_peaks(SEXP Rx, SEXP Rfht, SEXP Rfrelht, SEXP Rfhtie,
                  SEXP Rfhsupp) {
	double fht;                           /* peak height fraction total range */
	double frelht;                        /* relative peak height to average */
	double fhtie;                         /* fuzzy run fractional height */
	double fhsupp;                        /* peak height fraction for support */

	if (!isReal(Rx) && !isInteger(Rx)) {
		error("data type not supported");
	}

	if (!isReal(Rfht)) {
		error("fht argument not a double");
	}
	fht = asReal(Rfht);
	if ((fht <= 0.0) || (1.0 <= fht)) {
		error("fht must be between 0 and 1 (both excl.)");
	}

	if (!isReal(Rfrelht)) {
		error("frelht argument not a double");
	}
	frelht = asReal(Rfrelht);
	if ((frelht <= 0.0) || (1.0 <= frelht)) {
		error("frelht must be between 0 and 1 (both excl.)");
	}

	if (!isReal(Rfhtie)) {
		error("fhtie argument not a double");
	}
	fhtie = asReal(Rfhtie);
	if ((fhtie < 0) || (1.0 < fhtie)) {
		error("fhtie must be between 0 and 1 (excl.)");
	}

	if (!isReal(Rfhsupp)) {
		error("fhsupp argument not a double");
	}
	fhsupp = asReal(Rfhsupp);
	if ((fhsupp < 0.0) || (1.0 < fhsupp)) {
		error("fhsupp must be between 0 and 1 (incl.)");
	}

	return impl_peaks(Rx, fht, frelht, fhtie, fhsupp);
}

/***
    C_find_flats:  Look for flats, the longest stretches centered about each
                   point within the fripple specification (as a fraction of
                   the data's range), keeping only those with absolute minimum
                   length or a length relative that of the data, whichever
                   longer.  Ignores NA or non-finite values.
    args:          Rx - data, real or integer
                   Rfripple - maximum height of flat
                   Rminlen - minimum length of flats, absolute value
                   Rfminlen - minimum length as fraction of data
                   Rnoutlier - number outliers to each side of source allowed
                   Rver - force algorithm (1 scan, 2 walk, 0 auto)
    returns:   matrix with flats
***/
SEXP C_find_flats(SEXP Rx, SEXP Rfripple, SEXP Rminlen, SEXP Rfminlen,
                  SEXP Rnoutlier, SEXP Rver) {
	double fripple;                       /* ripple specfication */
	double fminlen;                       /* relative minimum length */
	int minlen;                           /* absolute minimum length */
	int noutlier;                         /* number outliers allowed */
	int ver;                              /* algorithm version */

	if (!isReal(Rx) && !isInteger(Rx)) {
		error("data type not supported");
	}

	if (!isReal(Rfripple)) {
		error("fripple argument not a double");
	}
	fripple = asReal(Rfripple);
	if ((fripple <= 0.0) || (1.0 <= fripple)) {
		error("fripple must be between 0 and 1 (both excl.)");
	}

	if (isReal(Rminlen)) {
		minlen = (int) round(asReal(Rminlen));
	} else if (isInteger(Rminlen)) {
		minlen = asInteger(Rminlen);
	} else {
		error("flat minlen (absolute) must be an integer");
	}
	if (minlen <= 0) {
		error("flat minlen (absolute) must be positive");
	}

	if (!isReal(Rfminlen)) {
		error("flat fminlen (relative) not a double");
	}
	fminlen = asReal(Rfminlen);
	if ((fminlen <= 0.0) || (1.0 <= fminlen)) {
		error("flat fminlen (relative) be between 0 and 1 (both excl.)");
	}

	if (isReal(Rnoutlier)) {
		noutlier = (int) round(asReal(Rnoutlier));
	} else if (isInteger(Rnoutlier)) {
		noutlier = asInteger(Rnoutlier);
	} else {
		error("outlier count not an integer");
	}
	if (noutlier < 0) {
		error("outlier count must not be negative");
	}

	if (isReal(Rver)) {
		ver = (int) round(asReal(Rver));
	} else if (isInteger(Rver)) {
		ver = asInteger(Rver);
	} else {
		error("flat algorithm version must be real or integer");
	}

	if ((1 != ver) && (2 != ver)) {
		/* See overall timing for find.flats() for cross-point. */
		ver = (length(Rx) <= 10000) ? 1 : 2;
	}

	if (1 == ver) {
		return impl_flats_scan(Rx, fripple, minlen, fminlen, noutlier);
	} else {
		return impl_flats_mmtree(Rx, fripple, minlen, fminlen, noutlier);
	}
}

/***
    C_find_level_sections:  Invert the modehunt sloping section test to
                            identify intervals without a slope.  Assumes the
                            data has been sorted and has no non-finite
                            values.  Only supports real vectors.
    args:                   Rx - sorted, real, finite data to process
                            Ralpha - significance level of non-slope test
                            Rcorrect - TRUE for short section adjustment
    returns:   R vector with two two vectors, 'stID' and 'endID'
***/
SEXP C_find_level_sections(SEXP Rx, SEXP Ralpha, SEXP Rcorrect) {
	double *x;                            /* the actual data */
	double alpha;                         /* significance level of test */
	int nx;                               /* length of data */
	int correct;                          /* boolean for short section stat */
	int flags;                            /* encoded conditions for test */

	if (!isReal(Rx)) {
		error("data type not supported");
	}
	x = REAL(Rx);
	nx = length(Rx);

	if (!isReal(Ralpha)) {
		error("alpha argument not a double");
	}
	alpha = asReal(Ralpha);
	if ((alpha <= 0.0) || (1.0 <= alpha)) {
		error("alpha must be between 0 and 1 (both excl.");
	}

	if (!isLogical(Rcorrect)) {
		error("correct argument not a boolean");
	}
	correct = asLogical(Rcorrect);

	flags = 0;
	if (correct) {
		flags |= LVL_CORR;
	}

	return impl_lvlsec(x, nx, alpha, flags);
}


/**** Internal Implementation - Runs ****/

/***
    impl_runs:  Identify runs in the data, storing their length at the first
                data point in the run in the returned vector.
    args:       Rx - data
                feps - fractional difference of matching points
    returns:   runs R generalized vector (see find.runs)
***/
SEXP impl_runs(SEXP Rx, double feps) {
	SEXP Rruns;                           /* result */
	SEXP Rrl;                             /* run lengths */
	SEXP Rnskip;                          /* skipped (NA, NaN) element count */
	SEXP Rstats;                          /* summary information */
	SEXP Rrnames;                         /* element names for Rruns */
	SEXP Rsnames;                         /* element names for Rstats */
	SEXP Rival;                           /* generic integer value */
	double *xd;                           /* Rx if double */
	int *xi;                              /* Rx if integer or logical */
	const char *xc;                       /* Rx characters in string */
	int *rl;                              /* contents of Rruns */
	int *nskip;                           /* skipped elements within run */
	int *nrun;                            /* number of runs found */
	int *maxrun;                          /* length of longest run */
	int *ndata;                           /* data length for statistics */
	int nx;                               /* length of Rx */
	int i, j;

	xd = NULL;
	xi = NULL;
	xc = NULL;

	if (isReal(Rx)) {
		xd = REAL(Rx);
	} else if (isInteger(Rx)) {
		xi = INTEGER(Rx);
	} else if (isLogical(Rx)) {
		xi = LOGICAL(Rx);
	}
	
	nx = length(Rx);

	Rruns = NULL;
	Rrl = NULL;
	Rnskip = NULL;
	Rstats = NULL;

	Rrl = PROTECT( allocVector(INTSXP, nx) );
	rl = INTEGER(Rrl);
	memset(rl, 0, nx*sizeof(*rl));

	Rnskip = PROTECT( allocVector(INTSXP, nx) );
	nskip = INTEGER(Rnskip);
	memset(nskip, 0, nx*sizeof(*nskip));

	Rstats = PROTECT( allocVector(VECSXP, 3) );
	
	Rival = PROTECT( allocVector(INTSXP, 1) );
	nrun = INTEGER(Rival);
	SET_VECTOR_ELT(Rstats, RUNID_STAT_NRUN, Rival);
	
	Rival = PROTECT( allocVector(INTSXP, 1) );
	maxrun = INTEGER(Rival);
	SET_VECTOR_ELT(Rstats, RUNID_STAT_MAXRUN, Rival);

	Rival = PROTECT( allocVector(INTSXP, 1) );
	ndata = INTEGER(Rival);
	SET_VECTOR_ELT(Rstats, RUNID_STAT_NX, Rival);
	*ndata = nx;

	for (nskip[0]=0; nskip[0]<nx; nskip[0]++) {
		if (isReal(Rx)) {
			if (!ISNA(xd[nskip[0]]) && !ISNAN(xd[nskip[0]])) {
				break;
			}
		} else if (isInteger(Rx)) {
			if (NA_INTEGER != xi[nskip[0]]) {
				break;
			}
		} else if (isLogical(Rx)) {
			if (NA_LOGICAL != xi[nskip[0]]) {
				break;
			}
		} else if (isString(Rx)) {
			if (NA_STRING != STRING_ELT(Rx, nskip[0])) {
				break;
			}
		}
	}

	*nrun = 0;
	*maxrun = 0;
	for (i=nskip[0]; i<nx; ) {
		if (isString(Rx)) {
			xc = CHAR(STRING_ELT(Rx, i));
		}
		j = i + 1;
		/* If x were integer all but the x[i]==x[j] test would drop ... */
		while (j < nx) {
			if (isReal(Rx)) {
				if (ISNA(xd[j]) || ISNAN(xd[j])) {
					nskip[i] += 1;
				} else if (!( ((R_PosInf == xd[i]) && (R_PosInf == xd[j])) ||
				              ((R_NegInf == xd[i]) && (R_NegInf == xd[j])) ||
				              ((0.0 == xd[j]) && (0.0 == xd[i])) ||
				              ((0.0 == feps) ?
				               (fabs(xd[i]-xd[j]) < DBL_EPSILON) :
				               ((fabs(xd[j]-xd[i]) < DBL_EPSILON) ||
				                (RELHT(xd[j], xd[i]) < feps))) )) {
					/* Note values within tolerance always treated as equal. */
					break;
				}
			} else if (isInteger(Rx)) {
				if (NA_INTEGER == xi[j]) {
					nskip[i] += 1;
				} else if (xi[i] != xi[j]) {
					break;
				}
			} else if (isLogical(Rx)) {
				if (NA_LOGICAL == xi[j]) {
					nskip[i] += 1;
				} else if ((xi[i] && !xi[j]) || (!xi[i] && xi[j])) {
					break;
				}
			} else if (isString(Rx)) {
				if (NA_STRING == STRING_ELT(Rx, j)) {
					nskip[i] += 1;
				} else if (strcmp(xc, CHAR(STRING_ELT(Rx,j)))) {
					break;
				}
			}
			j += 1;
		}
		/* j marks the point outside the run, so length is inclusive. */
		rl[i] = j - i - nskip[i];
		*nrun += 1;
		if (*maxrun < rl[i]) {
			*maxrun = rl[i];
		}
		i = j;
		R_CheckUserInterrupt();
	}

	Rruns = PROTECT( allocVector(VECSXP, 3) );
	SET_VECTOR_ELT(Rruns, RUNID_RUNS, Rrl);
	SET_VECTOR_ELT(Rruns, RUNID_SKIP, Rnskip);
	SET_VECTOR_ELT(Rruns, RUNID_STATS, Rstats);

	Rrnames = PROTECT( allocVector(STRSXP, 3) );
	SET_STRING_ELT(Rrnames, RUNID_RUNS, mkChar("runs"));
	SET_STRING_ELT(Rrnames, RUNID_SKIP, mkChar("nskip"));
	SET_STRING_ELT(Rrnames, RUNID_STATS, mkChar("stats"));
	setAttrib(Rruns, R_NamesSymbol, Rrnames);

	Rsnames = PROTECT( allocVector(STRSXP, 3) );
	SET_STRING_ELT(Rsnames, RUNID_STAT_NRUN, mkChar("nrun"));
	SET_STRING_ELT(Rsnames, RUNID_STAT_MAXRUN, mkChar("maxrun"));
	SET_STRING_ELT(Rsnames, RUNID_STAT_NX, mkChar("nx"));
	setAttrib(Rstats, R_NamesSymbol, Rsnames);

	UNPROTECT(9);
	return Rruns;
}


/**** Internal Implementation - Peaks ****/

/***
    impl_peaks:  Identify prominent local minima and maxima in the data
                 according to the height specifications, after removing
                 mesas within the signal.
    args:        Rx - data from R
                 fht - minimum peak height relative data range
                 frelht - minimum local peak height relative minima
                 fhtie - fuzzy run fractional height for tied values
                 fhsupp - fraction of peak height to determine support width
    returns:   vector of vectors with peaks information (see find.peaks)
               vectors will have 0 length if data bad (constant, short)
***/
static SEXP impl_peaks(SEXP Rx, double fht, double frelht, double fhtie,
                       double fhsupp) {
	SEXP Rmm;                             /* extrema */
	SEXP Rruns;                           /* data without mesas */
	SEXP Rstats;                          /* run info */
	SEXP Rnrun;                           /* run count member of stats */
	struct mmdata *mm;                    /* local extrema */
	struct msdstats stat;                 /* statistics of Rx */
	double *xsd;                          /* standardized (by stdev) data */
	double *rsd;                          /* standardized value per run */
	int *rl;                              /* runs stored within Rruns */
	int *heap;                            /* minheap for peaks by height */
	int *nskip;                           /* skipped values per run */
	int *posID;                           /* index of mid-run */
	double thrabs;                        /* absolute minimum peak height */
	int nx;                               /* length of data */
	int nrun;                             /* number of runs/unique points */
	int nmm;                              /* number extrema found */
	int nheap;                            /* count of elements stored in heap */
	int lfail;                            /* true if merging left, 0 right */
	int irun;                             /* run counter */
	int i, j, k;

	Rmm = NULL;
	Rruns = NULL;
	Rstats = NULL;
	Rnrun = NULL;

	xsd = NULL;
	rsd = NULL;
	heap = NULL;
	mm = NULL;

	/* Step 1: Use fuzzy runs to identify plateaux.  We will step through the
	   data per the length+nskip strategy, which places the value at the first
	   element. */

	Rruns = PROTECT( impl_runs(Rx, fhtie) );
	rl = INTEGER( VECTOR_ELT(Rruns, RUNID_RUNS) );
	nskip = INTEGER( VECTOR_ELT(Rruns, RUNID_SKIP) );
	Rstats = PROTECT( VECTOR_ELT(Rruns, RUNID_STATS) );
	Rnrun = PROTECT( VECTOR_ELT(Rstats, RUNID_STAT_NRUN) );
	nrun = asInteger(Rnrun);

	/* Step 2: Simplify the data Rx by run. */
	
	stats_meansd(Rx, &stat);

	if ((nrun < 2) || (((stat.xmax - stat.xmin) / fhtie) < stat.mu)) {
		Rmm = build_peaks(NULL, NULL, NULL, 0, 0);
		UNPROTECT(3);
		return Rmm;
	}

	nx = length(Rx);

	/* Initialization not needed for any of these, we overwrite the garbage
	   from the alloc. */
	xsd = (double *) R_alloc(nx, sizeof(double));
	rsd = (double *) R_alloc(nrun, sizeof(double));
	posID = (int *) R_alloc(nrun, sizeof(int));
	heap = (int *) R_alloc(nrun, sizeof(int));
	nheap = 0;
	mm = (struct mmdata *) R_alloc(nrun, sizeof(struct mmdata));
	nmm = 0;

	stat.mu = 0;
	standardize(Rx, &stat, xsd, NULL);

	if (0 == rl[0]) {
		irun = nskip[0];
	} else {
		irun = 0;
	}
	
	for (j=0; j<2; j++) {
		rsd[j] = xsd[irun];
		/* rl[i]/2 puts in middle of run/mesa.  Rounds to 0 for 1-length runs. */
		posID[j] = irun + (rl[irun] / 2);
		irun += rl[irun] + nskip[irun];
	}
	
	mm[nmm].runID = 0;
	mm[nmm].pos = posID[0];
	mm[nmm].valsd = rsd[0];
	mm[nmm++].flags = MM_KEEP | ((rsd[0] > rsd[1]) ? MM_ISMAX : 0) | MM_ATEND;
	
	/* We've checked that nrun >= 2, so i and j are valid.  k maybe not.
	   The loop will end with j == nrun-1 for the last extremum. */
	for (i=0, j=1, k=2; k<nrun; i++, j++, k++) {
		rsd[k] = xsd[irun];
		posID[k] = irun + (rl[irun] / 2);
		irun += rl[irun] + nskip[irun];
		if ((rsd[i] < rsd[j]) && (rsd[k] < rsd[j])) {
			mm[nmm].flags = MM_KEEP | MM_ISMAX;
		} else if ((rsd[j] < rsd[i]) && (rsd[j] < rsd[k])) {
			mm[nmm].flags = MM_KEEP;
		} else {
			mm[nmm].flags = 0;
		}
		if (mm[nmm].flags & MM_KEEP) {
			mm[nmm].runID = j;
			mm[nmm].valsd = rsd[j];
			mm[nmm++].pos = posID[j];
		}
	}
	
	mm[nmm].runID = j;
	mm[nmm].pos = posID[j];
	mm[nmm].valsd = rsd[j];
	mm[nmm++].flags = MM_KEEP | ((rsd[j] > rsd[i]) ? MM_ISMAX : 0) | MM_ATEND;

	R_CheckUserInterrupt();

	/* Step 4: Set up the initial heights for each maximum. */

	for (i=0; i<(nmm-1); i++) {
		mm[i].lID = i - 1;
		mm[i].rID = i + 1;
	}
	mm[i].lID = i - 1;
	mm[i].rID = -1;
	for (i=0; i<nmm; i++) {
		set_heights(mm, i);
	}

	/* Step 5: Roughly sort the maxima by the smallest height to the adjacent
	   minima.  Store in a minheap. */

	build_htheap(mm, nmm, heap, &nheap);
	R_CheckUserInterrupt();

	thrabs = fht * (stat.xmax - stat.xmin) / stat.sigma;

	/* Step 6: Process all maxima in heap order.  Compare the absolute and
	   relative heights to the thresholds and, if below/too small, delete
	   the peak and corresponding minimum (in the L*HT direction).  Update
	   the heights after deletion, and the heap. */

	/* There's a bit of trickery that took some work to pass the test cases.
	   In set_height we create one-sided heights if a maximum does not have
	   a second maximum to a side, so that any merging/testing will be in
	   the correct direction.  All maxima in build_htheap get placed on the
	   heap, but we screen those at the first and last data point and do
	   nothing with them (TC2).  Still, they are on the heap for reference if
	   we merge an interior maximum that neighbors it (TC4).  	   

	   Another difficulty is when the first and last data point are not really
	   maxima and would be merged.  We want to keep these always, and so must
	   handle interior maxima that are really the first or last.  So, when we
	   want to merge a peak and it's value is greater than the merge target,
	   if that target is ATEND we transfer the marker to the peak and instead
	   lose the target.
	*/

	while (0 < nheap) {
		i = heap[0];
		del_htheap(heap, &nheap, 0, mm);
		if ((0 == i) || ((nmm - 1) == i)) {
			continue;
		}
		lfail = 0;
		if (mm[i].minht < thrabs) {
			lfail = mm[i].flags & MM_LMINHT;
		} else if (mm[i].relht < frelht) {
			lfail = mm[i].flags & MM_LRELHT;
		} else {
			continue;
		}

		if (lfail) {
			j = mm[mm[i].lID].lID;
			if (j < 0) {
				error("no neighboring maximum to left, cannot merge that direction");
			}
			if (!(mm[j].flags & MM_ISMAX)) {
				error("twice step extrema not a maximum");
			} else if (mm[i].valsd > mm[j].valsd) {
				/* It's OK to not merge to the first extremum. */
				if (0 == j) {
					/* No special action, just mark at end and continue. */
				} else if (mm[j].flags & MM_ATEND) {
					drop_endmax(mm, j, thrabs, frelht);
				} else {
					error("cannot merge left into lower peak");
				}
				mm[i].flags |= MM_ATEND;
				continue;
			}

			mm[j].rID = mm[i].rID;
			mm[mm[i].rID].lID = j;
			mm[mm[i].lID].flags &= ~MM_KEEP;
		} else {
			j = mm[mm[i].rID].rID;
			if (j < 0) {
				error("no neighboring maximum to right, cannot merge that direction");
			}
			if (!(mm[j].flags & MM_ISMAX)) {
				error("twice step extrema not a maximum");
			} else if (mm[i].valsd > mm[j].valsd) {
				/* It's OK to not merge to the last extremum. */
				if (j == (nmm - 1)) {
					/* No special action, just mark at end and continue. */
				} else if (mm[j].flags & MM_ATEND) {
					drop_endmax(mm, j, thrabs, frelht);
				}
				mm[i].flags |= MM_ATEND;
				continue;
			}

			mm[j].lID = mm[i].lID;
			mm[mm[i].lID].rID = j;
			mm[mm[i].rID].flags &= ~MM_KEEP;
		}

		mm[i].flags &= ~MM_KEEP;
		mm[i].lID = -1;
		mm[i].rID = -1;
		
		set_heights(mm, j);
		/* It may happen that the peak j has already been processed and is not
		   on the heap, in which case htheapify will ignore the invalid (-1)
		   find_htheapID result.  This happens at the edge.  Still not convinced
		   this is correct (that we shouldn't avoid the situation in the first
		   place), but it works.  See the stress testing and TC21 and onward. */
		htheapify(heap, nheap, find_htheapID(heap, nheap, j), mm);

		R_CheckUserInterrupt();
	}

	/* We need to have Rruns/nskip protected during the build, so cannot
	   unprotect it and return directly with Rmm. */
	Rmm = PROTECT( build_peaks(rsd, posID, mm, nmm, fhsupp) );

	UNPROTECT(4);
	return Rmm;
}


/**** Internal Implementation - Flats ****/

/***
    impl_flats_scan:  Identify flats within the ripple band specification with
                      minimum length (absolute or relative).  Scan outward 
                      from each point in the data - will be slow if the vector
                      is long and flats extend over a substantial fraction.
    args:             Rx - raw data, may have non-finite values, real or int
                      fripple - ripple spec as fraction of data range
                      minlen - absolute minimum length of flat
                      fminlen - shortest flat as fraction of data length
                      noutlier - number points outside ripple range allowed
    returns:   maxtrix with flats in rows (see find.flats)
***/
static SEXP impl_flats_scan(SEXP Rx,
                            double fripple, int minlen, double fminlen,
                            int noutlier) {
	struct flatsrc *srcs;                 /* flats around each data point */
	struct msdstats stat;                 /* statistics for Rx */
	double *xsd;                          /* standardized data */
	int *naskip;                          /* cum count of non-finite data */
	int *flats;                           /* accepted flats */
	double xlo, xhi;                      /* ripple bounds about source point */
	int nx;                               /* length of raw data */
	int nflat;                            /* number of accepted flats */
	int rellen;                           /* minimum relative flat length */
	int i;

	naskip = NULL;
	xsd = NULL;
	srcs = NULL;
	flats = NULL;

	nx = length(Rx);
	if (nx < 2) {
		error("must have at least two points to find flats");
	}

	stats_meansd(Rx, &stat);

	srcs = (struct flatsrc *) R_alloc(stat.ndata, sizeof(*srcs));
	flats = (int *) R_alloc(stat.ndata, sizeof(*flats));

	xsd = (double *) R_alloc(nx, sizeof(*xsd));
	naskip = (int *) R_alloc(nx, sizeof(*naskip));

	/* No shift.  Same test as in impl_peaks for constant signal. */
	/* The range of the data is not particularly robust, but is traditional.
	   We could instead define the ripple as a multiple of the standard
	   deviation, where fripple = 0.05 ~ 0.2-0.3 * stdev. */
	if (((stat.xmax - stat.xmin) / fripple) < stat.mu) {
		stat.sigma = 1;
		fripple = stat.xmax - stat.xmin;
	} else {
		/* 1/2 because we take +/- symmetrically about point. */
		fripple = fripple * (stat.xmax - stat.xmin) / (2.0 * stat.sigma);
	}
	stat.mu = 0;
	standardize(Rx, &stat, xsd, naskip);

	rellen = (int) round(fminlen * stat.ndata);
	if (minlen < rellen) {
		minlen = rellen;
	}

	/* Our initial implementation looked outward from each seed point in
	   either direction (towards 0 or ndata).  For very large data sets
	   this is horribly inefficient, O(n L) with L the average length of any
	   flat, since we expect significant overlap between nearby sources.

	   ex. Kirkwood gap data.  Select subsets of size 50k, 100k, 500k whose
	   orbital axis (radius) is within the bounds of those with valid 
	   diameters (0.974 - 5.295 AU); the 2140 asteroids with known diameter
	   yield no flats.  Find flats on interval spacings of different widths.
	   Determine average flat length at each source point, as fraction of the
	   data size, and average overlap length to previous source flat.  Use
	   ripple 0.025 and set seed before sampling 7.
	   #ast   w      avg len   as frac   avg olap    final detected
	   50k    100      15656      0.31      15645    1 flat  len 46367
	          250      14129      0.28      14119    2 flats len 13316, 32972
	          500      13643      0.28      13638    2 flats len 15462, 32962
	   100k   100      51789      0.52      51785    1 flat  len 93575
	          250      30981      0.31      30972    1 flat  len 93353
	          500      28516      0.29      28510    2 flats len 26462, 66130
	   500k   100     441961      0.88     441875    1 flat  len 492063
	          250     280605      0.56     280539    1 flat  len 490056
	          500     260057      0.52     260053    1 flat  len 489753
	   The problem is clear: an average flat of 30-50% of the data calculated
	   at each point, with nearly complete overlap to the previous source.

	   Scanning a sorted list for the ripple bounds would seem to be the 
	   natural approach, but the requirement that the flat be contiguous
	   makes this a hard problem, requiring scanning through the selection to
	   check, which defeats the purpose.

	   An incremental scan would be a natural way to attack the problem, but
	   we cannot know where within the previous flat points would fall ouside
	   the ripple range of the next source point.
	*/

	for (i=0; i<stat.ndata; i++) {
		xlo = xsd[i] - fripple;
		xhi = xsd[i] + fripple;
		srcs[i].srcID = i;
		srcs[i].stID = scan_out(xsd, i, 0, xlo, xhi, noutlier);
		srcs[i].endID = scan_out(xsd, i, stat.ndata-1, xlo, xhi, noutlier);
		srcs[i].len = srcs[i].endID - srcs[i].stID + 1;
		R_CheckUserInterrupt();
	}

	/* See repository for identify_flats_vote, originally used here as in
	   the development version. */
	identify_flats_cover(srcs, stat.ndata, minlen, flats, &nflat);

	return build_flats(xsd, &stat, fripple, srcs, flats, nflat, naskip);
}

/***
    impl_flats_mmtree:  Identify flats within the ripple band specification
                        with minimum length (absolute or relative).  Use a
                        segtree to cover large intervals with similar
                        values, as happens with large data sets - but has a
                        high overhead for small.
    args:               Rx - raw data, may have non-finite values, real or int
                        fripple - ripple spec as fraction of data range
                        minlen - absolute minimum length of flat
                        fminlen - shortest flat as fraction of data length
                        noutlier - number points outside ripple range allowed
    returns:   general vector to cast into a data frame, see find.flats
***/
static SEXP impl_flats_mmtree(SEXP Rx,
                              double fripple, int minlen, double fminlen,
                              int noutlier) {
	struct mmtree mmt;                    /* our segtrees */
	struct flatsrc *srcs;                 /* flats around each data point */
	struct msdstats stat;                 /* statistics about Rx */
	double *xsd;                          /* standardized data */
	int *naskip;                          /* cum count of non-finite data */
	int *flats;                           /* accepted flats, index into srcs */
	double xlo, xhi;                      /* ripple bounds about source point */
	int nx;                               /* length of raw data */
	int nflat;                            /* number of accepted flats */
	int rellen;                           /* minimum relative flat length */
	int i;
	
	xsd = NULL;
	naskip = NULL;
	srcs = NULL;
	
	memset(&mmt, 0, sizeof(mmt));

	nx = length(Rx);
	if (nx < 2) {
		error("must have at least two points to find flats");
	}

	stats_meansd(Rx, &stat);

	srcs = (struct flatsrc *) R_alloc(stat.ndata, sizeof(*srcs));
	flats = (int *) R_alloc(stat.ndata, sizeof(*flats));

	xsd = (double *) R_alloc(nx, sizeof(*xsd));
	naskip = (int *) R_alloc(nx, sizeof(*naskip));

	/* No shift.  Same test as in impl_peaks for constant signal. */
	if (((stat.xmax - stat.xmin) / fripple) < stat.mu) {
		stat.sigma = 1;
		fripple = stat.xmax - stat.xmin;
	} else {
		/* 1/2 because we take +/- symmetrically about point. */
		fripple = fripple * (stat.xmax - stat.xmin) / (2.0 * stat.sigma);
	}
	stat.mu = 0;
	standardize(Rx, &stat, xsd, naskip);

	rellen = (int) round(fminlen * stat.ndata);
	if (minlen < rellen) {
		minlen = rellen;
	}

	build_mmtree(xsd, stat.ndata, &mmt);

	for (i=0; i<stat.ndata; i++) {
		xlo = xsd[i] - fripple;
		xhi = xsd[i] + fripple;
		srcs[i].srcID = i;
		srcs[i].stID = walk_mmtree(&mmt, i, -1, xlo, xhi, noutlier);
		srcs[i].endID = walk_mmtree(&mmt, i, +1, xlo, xhi, noutlier);
		srcs[i].len = srcs[i].endID - srcs[i].stID + 1;
		R_CheckUserInterrupt();
	}

	identify_flats_cover(srcs, stat.ndata, minlen, flats, &nflat);
	
	return build_flats(xsd, &stat, fripple, srcs, flats, nflat, naskip);
}


/**** Internal Implementation - Level Sections ****/

/***
    impl_lvlsec:  Identify level sections in data that must be sorted before
                  passing here, and must not contain NA or NaN values.  Slope
                  test is done at significance level 1-alpha.
    args:         x - sorted, finite data to process
                  nx - length of x vector
                  alpha - significance level of non-slope test (kinda)
                  flags - LVL_* flags controlling 
    returns:   R vector with two vectors, 'stID' and 'endID'
***/
static SEXP impl_lvlsec(double *x, int nx, double alpha, int flags) {
	SEXP Rlvl;                            /* level sections */
	SEXP Rnames;                          /* element names for Rlvl */
	SEXP RstID;                           /* starting ends of level sections */
	SEXP RendID;                          /* final ends of level sections */
	double *cum;                          /* cumulative sum over row */
	double *sigma;                        /* standardization of test stat */
	double *gamma;                        /* short section correction */
	int *firstk;                          /* index first level section per j */
	int *stID;                            /* starting endpoint of a section */
	int *endID;                           /* ending endpt (incl.) of section */
	int *pt;                              /* contents of RstID or RendID */
	uchar *slope;                         /* slope of first level section */
	double cv;                            /* critical value for alpha */
	double cvcorr;                        /* corrected critical value */
	double dijk;                          /* interval spacing */
	double tjk;                           /* test statistic */
	int ttij;                             /* thresholded test statistic */
	int nlvl;                             /* number of level sections */
	int jst, jend;                        /* row bounds for section scan */
	int kend;                             /* col bounds for section scan */
	int i, j, k;

	Rlvl = NULL;
	Rnames = NULL;
	RstID = NULL;
	RendID = NULL;

	cum = NULL;
	gamma = NULL;
	firstk = NULL;
	stID = NULL;
	endID = NULL;

	if (nx < 3) {
		error("too little data (length < 3) to support interval spacing");
	}

	/* Step 1: Lots of setup - cumulative sum for intervals, critical value. */

	cum = (double *) R_alloc(nx, sizeof(*cum));
	sigma = (double *) R_alloc(nx, sizeof(*sigma));
	sigma[0] = 0.0;
	cum[0] = x[0];
	for (i=1; i<nx; i++) {
		sigma[i] = sqrt(i / 3.0);
		cum[i] = cum[i-1] + x[i];
	}

	cv = lvlsec_critval(nx, alpha, flags);

	if (flags & LVL_CORR) {
		gamma = (double *) R_alloc(nx, sizeof(*gamma));
		for (i=0; i<nx; i++) {
			gamma[i] = sqrt(2.0 * (1.0 - log((double) i / (nx + 2.0))));
		}
	}

	firstk = (int *) R_alloc(nx, sizeof(*firstk));
	slope = (uchar *) R_alloc(nx, sizeof(*slope));
	for (i=0; i<nx; i++) {
		firstk[i] = nx + 1;
		slope[i] = 0;
	}

	/* Step 2: Determine the first sloping interval per row j. */
	
	for (j=0; j<(nx-2); j++) {
		if (x[j+1] < x[j]) {
			error("data has not been sorted");
		}
		for (k=j+2; (k<nx) && (nx<firstk[j]); k++) {
			dijk = x[k] - x[j];
			if (0.0 == dijk) {
				tjk = 0.0;
			} else {
				tjk = (cum[k-1] - cum[j]) - ((k-j-1) * x[j]);
				tjk = (((2.0 * tjk) / dijk) - (k-j-1)) / sigma[k-j-1];
			}

			cvcorr = cv;
			if (flags & LVL_CORR) {
				cvcorr += gamma[k-j];
			}
			ttij = 0;
			if (tjk > cvcorr) {
				ttij = LVL_MPOS;
			} else if (tjk < -cvcorr) {
				ttij = LVL_MNEG;
			}
			if (ttij & LVL_SLOPED) {
				if (k < firstk[j]) {
					slope[j] = ttij & LVL_SLOPED;
					firstk[j] = k;
				}
			}
		}
		R_CheckUserInterrupt();
	}

	/* Convert the sloping interval markers into level sections. */
	stID = (int *) R_alloc(nx, sizeof(int));
	endID = (int *) R_alloc(nx, sizeof(int));
	nlvl = 0;

	/* Step 3: Identify level sections based on first sloping intervals.
	           The j,k here aren't our normal loop indices but the interval
	           notation from Duembgen/Walther/Rufibach. */
	j = 0;
	while (1) {
		jst = j;
		kend = firstk[jst];
		if ((nx - 1) <= kend) {
			kend = nx - 1;
			jend = nx - 1;
		} else {
			jend = jst;
			for (j=jst; j<firstk[jst]; j++) {
				if (firstk[j] < kend) {
					jend = j;
					kend = firstk[j];
				}
			}
			if ((nx - 5) <= kend) {
				kend = nx - 1;
			}
		}
		
		stID[nlvl] = jst;
		endID[nlvl] = kend;
		/* kend is the first sloping endpoint, so back off one unless at end. */
		if (kend < (nx - 1)) {
			endID[nlvl] -= 1;
		}
		nlvl += 1;
		if (nx < nlvl) {
			error("internal error - more sections than data points");
		}

		if ((nx - 1) == kend) {
			break;
		}

		/* Back down rows until slope changes.  This allows sections to overlap. */
		j = kend;
		if ((slope[jend] & LVL_SLOPED) != (slope[kend] & LVL_SLOPED)) {
			while ((0 < j) &&
			       ((slope[j-1] & LVL_SLOPED) == (slope[kend] & LVL_SLOPED))) {
				j -= 1;
			}
		}
		R_CheckUserInterrupt();
	}

	/* Rinternals has Rf_lengthgets to change the length of a vector, but that
	   function just allocates a new vector and copies data over.  We can do
	   that.  But this also allows us to adjust for R's 1-indexing. */
	RstID = PROTECT( allocVector(INTSXP, nlvl) );
	pt = INTEGER(RstID);
	for (i=0; i<nlvl; i++) {
		pt[i] = stID[i] + 1;
	}
	
	RendID = PROTECT( allocVector(INTSXP, nlvl) );
	pt = INTEGER(RendID);
	for (i=0; i<nlvl; i++) {
		pt[i] = endID[i] + 1;
	}
	
	Rlvl = PROTECT( allocVector(VECSXP, 2) );
	SET_VECTOR_ELT(Rlvl, 0, RstID);
	SET_VECTOR_ELT(Rlvl, 1, RendID);

	Rnames = PROTECT( allocVector(STRSXP, 2) );
	SET_STRING_ELT(Rnames, 0, mkChar("stID"));
	SET_STRING_ELT(Rnames, 1, mkChar("endID"));
	setAttrib(Rlvl, R_NamesSymbol, Rnames);

	UNPROTECT(4);
	
	return Rlvl;
}

/***
    lvlsec_critval:  Return the critival value for the Duembgen/Rufibach
                     slope test.  The significance level alpha is for the
                     _level_ sections and is changed to 1-alpha internally.
                     Returns the critical value with or without the short
                     section correction per the LVL_CORR flag.
    args:            nx - number of data points
                     alpha - significance level for level sections
                     flags - LVL_* control 
    returns:   critical value for level section statistic
***/
static double lvlsec_critval(int nx, double alpha, int flags) {
	double cv;                            /* critical value */

	alpha = 1.0 - alpha;

	/* This is our V2 model. */
	if (flags & LVL_CORR) {
		cv =
			1.45638 + (3.05017e-5 * nx) - (0.91411 * alpha) +
			(2.09873e-4 * nx * alpha) - (4.43401e-8 * nx*nx * alpha) +
			(1.27914e-8 * nx*nx * alpha*alpha) -
			(29.1864 / nx) - (0.51588 * log10(alpha)) + (0.16782 * log10(1.0-alpha));
	} else {
		cv =
			3.87589 + (1.14578e-4 * nx) - (0.74516 * alpha) +
			(3.49072e-4 * nx * alpha) - (6.46512e-8 * nx*nx * alpha) +
			(5.81907e-9 * nx*nx * alpha*alpha) -
			(51.0185 / nx) - (0.43067 * log10(alpha)) + (0.12376 * log10(1.0-alpha));
	}

	return cv;
}


/**** Data Statistics and Standardization ****/

/***
    stats_meansd:  Two-pass calculation of mean and standard deviation of
                   data in real or integer vector Rx.  Ignores non-finite
                   values (including infinities).
    args:          Rx - data to process
                   stat - structure to hold mean, stdev, range (modified)
    modifies:  stat members
***/
static void stats_meansd(SEXP Rx, struct msdstats *stat) {
	double *xd;                           /* contents of Rx if real */
	int *xi;                              /* contents of Rx if integer */
	double dsum;                          /* single pass total for real x */
	long isum;                            /* single pass total for integer x */
	int havept;                           /* true after first finite data */
	int nx;                               /* length of data */
	int i;

	nx = length(Rx);

	stat->mu = R_NaN;
	stat->sigma = R_NaN;
	stat->ndata = 0;
	stat->xmin = R_NaN;
	stat->xmax = R_NaN;
	havept = 0;

	if (isReal(Rx)) {
		xd = REAL(Rx);
		dsum = 0.0;
		for (i=0; i<nx; i++) {
			if (R_finite(xd[i])) {
				stat->ndata += 1;
				dsum += xd[i];
				if (!havept || (xd[i] < stat->xmin)) {
					stat->xmin = xd[i];
				}
				if (!havept || (stat->xmax < xd[i])) {
					stat->xmax = xd[i];
				}
				havept = 1;
			}
		}
		if (0 < stat->ndata) {
			stat->mu = dsum / (double) stat->ndata;
		}
		if (1 < stat->ndata) {
			dsum = 0.0;
			for (i=0; i<nx; i++) {
				if (R_finite(xd[i])) {
					dsum += (xd[i] - stat->mu) * (xd[i] - stat->mu);
				}
			}
			/* Bias correction. */
			stat->sigma = sqrt(dsum / (double) (stat->ndata - 1));
		}
	} else if (isInteger(Rx)) {
		xi = INTEGER(Rx);
		isum = 0;
		for (i=0; i<nx; i++) {
			if (NA_INTEGER != xi[i]) {
				stat->ndata += 1;
				isum += xi[i];
				if (!havept || (xi[i] < stat->xmin)) {
					stat->xmin = xi[i];
				}
				if (!havept || (stat->xmax < xi[i])) {
					stat->xmax = xi[i];
				}
				havept = 1;
			}
		}
		if (0 < stat->ndata) {
			stat->mu = (double) isum / (double) stat->ndata;
		}
		if (1 < stat->ndata) {
			/* isum could overflow. */
			dsum = 0;
			for  (i=0; i<nx; i++) {
				if (NA_INTEGER != xi[i]) {
					dsum += (xi[i] - stat->mu) * (xi[i] - stat->mu);
				}
			}
			stat->sigma = sqrt(dsum / (double) (stat->ndata - 1));
		}
	} else {
		error("data type not supported");
	}
}

/***
    standardize:  Scale the R vector (integer or real only) by its standard
                  deviation after centering on the mean - the stats are
                  supplied by argument rather than calling stats_meanvar here.
                  The result memory must already be allocated with length(Rx)
                  points.  If nskip is not NULL then it will contain the
                  number of non-finite values at each point, and xsd will not
                  contain them so that the point count will match that in
                  stat.  If nskip is NULL then xsd will contain the non-finite
                  values and have length(Rx).
    args:         Rx - data (integer or real) to standardize
                  stat - statistics about Rx
                  xsd - standardized result (already allocated w/ length Rx)
                  nskip - number of non-finite values per xsd (may be NULL)
    modifies:  xsd, nskip (if not NULL)
***/
static void standardize(SEXP Rx, struct msdstats *stat,
                        double *xsd, int *nskip) {
	double *xd;                           /* real data within Rx */
	int *xi;                              /* integer data within Rx */
	int nx;                               /* length of original data */
	int cnt;                              /* counter of data */
	int nast;                             /* first non-finite index */
	int i, j;

	nx = length(Rx);
	/* This is a sanity check, it will match stat->ndata. */
	cnt = 0;
	nast = -1;
	if (isReal(Rx)) {
		xd = REAL(Rx);
		for (i=0; i<nx; i++) {
			if (!R_finite(xd[i])) {
				if (NULL != nskip) {
					if (nast < 0) {
						nast = i;
					}
				} else {
					xsd[cnt] = xd[i];
					cnt += 1;
				}
			} else {
				if (NULL != nskip) {
					if (0 <= nast) {
						for (j=nast; j<=i; j++) {
							nskip[j] = ((0 == nast) ? 0 : nskip[nast-1]) + i - nast;
						}
						nast = -1;
					} else {
						nskip[i] = ((0 == i) ? 0 : nskip[i-1]);
					}
				}
				xsd[cnt] = (xd[i] - stat->mu) / stat->sigma;
				cnt += 1;
			}
		}
	} else if (isInteger(Rx)) {
		xi = INTEGER(Rx);
		for (i=0; i<nx; i++) {
			if (NA_INTEGER == xi[i]) {
				if (NULL != nskip) {
					nskip[i] = 1 + ((0 == i) ? 0 : nskip[i-1]);
				} else {
					xsd[cnt] = NA_REAL;
					cnt += 1;
				}
			} else {
				if (NULL != nskip) {
					nskip[i] = ((0 == i) ? 0 : nskip[i-1]);
				}
				xsd[cnt] = ((double) xi[i] - stat->mu) / stat->sigma;
				cnt += 1;
			}
		}
	} else {
		error("unsupported data type");
	}

	if ((NULL != nskip) && (cnt != stat->ndata)) {
		error("internal error - standardized data count did not match statistics");
	}
	if ((NULL == nskip) && (cnt != length(Rx))) {
		error("internal error - standardized data count did not match input");
	}
}


/**** Extrema Handling ****/

/***
    drop_endmax:  Remove the maximum at idel, merging it left or right as
                  appropriate.
    args:         mm - extrema
                  idel - index in mm of maximum to delete
                  thrabs - minimum height of maximum
                  frelht - minimum relative height of maximum
    modifies:  mm
***/
static inline void drop_endmax(struct mmdata *mm, int idel,
                               double thrabs, double frelht) {
	int lfail;                            /* true to merge left, 0 right */
	int minID;                            /* mm index of minimum being merged */

	if ((mm[idel].lID < 0) || (mm[idel].rID < 0)) {
		error("dropping end maximum that does not have both neighbors");
	}

	if (mm[idel].minht < thrabs) {
		lfail = mm[idel].flags & MM_LMINHT;
	} else if (mm[idel].relht < frelht) {
		lfail = mm[idel].flags & MM_LRELHT;
	} else {
		error("want to drop end maximum but it passes height checks");
	}

	if (lfail) {
		minID = mm[idel].lID;
		mm[mm[minID].lID].rID = mm[idel].rID;
		mm[mm[idel].rID].lID = mm[minID].lID;
		set_heights(mm, mm[minID].lID);
	} else {
		minID = mm[idel].rID;
		mm[mm[minID].rID].lID = mm[idel].lID;
		mm[mm[idel].lID].rID = mm[minID].rID;
		set_heights(mm, mm[minID].rID);
	}

	mm[idel].flags &= ~MM_KEEP;
	mm[minID].flags &= ~MM_KEEP;
	mm[idel].lID = -1;
	mm[idel].rID = -1;
	mm[minID].lID = -1;
	mm[minID].rID = -1;
}

/***
    set_heights:  Update the height - absolute and relative - of a peak.
                  Assumes the left and right neighbors have been set up.
    args:         mm - extrema
                  pkID - index of peak to update
    modifies:  mm members minht, relht, flags for pkID
***/
static void set_heights(struct mmdata *mm, int pkID) {
	double lht, rht;                      /* absolute peak height to left/right */
	double flrel, frrel;                  /* relative peak height to left/right */
	int lID, rID;                         /* index of minimum to left/right */

	if (!(mm[pkID].flags & MM_ISMAX)) {
		mm[pkID].minht = 0.0;
		mm[pkID].relht = 0.0;
		mm[pkID].flags &= ~(MM_LMINHT | MM_LRELHT);
		return;
	}

	lID = mm[pkID].lID;
	rID = mm[pkID].rID;
	
	if ((0 <= lID) && (0 <= mm[lID].lID)) {
		lht = mm[pkID].valsd - mm[lID].valsd;
		flrel = RELHT(mm[pkID].valsd, mm[lID].valsd);
	} else {
		lht = INFINITY;
		flrel = INFINITY;
	}
	if ((0 <= rID) && (0 <= mm[rID].rID)) {
		rht = mm[pkID].valsd - mm[rID].valsd;
		frrel = RELHT(mm[pkID].valsd, mm[rID].valsd);
	} else {
		rht = INFINITY;
		frrel = INFINITY;
	}

	if (lht < rht) {
		mm[pkID].minht = lht;
		mm[pkID].flags |= MM_LMINHT;
	} else {
		mm[pkID].minht = rht;
		mm[pkID].flags &= ~MM_LMINHT;
	}

	if (flrel < frrel) {
		mm[pkID].relht = flrel;
		mm[pkID].flags |= MM_LRELHT;
	} else {
		mm[pkID].relht = frrel;
		mm[pkID].flags &= ~MM_LRELHT;
	}
}

/***
    build_peaks:  Create an R list (generic vector) with all values 
                  describing the extrema following the find_peaks interface. 
                  If nmm is 0 neither rsd nor mm will be accessed, so pass
                  NULL for them; the vectors returned will all have length 0.
    args:         rsd - data, standardized, for each run
                  xpos - mid-run index of rsd point in original data
                  mm - local extrema
                  nmm - number of local extrema
                  fhsupp - fraction of peak height for support
***/
static SEXP build_peaks(double *rsd, int *xpos, struct mmdata *mm, int nmm,
                        double fhsupp) {
	SEXP Rmm;                             /* extrema */
	SEXP Rnames;                          /* element names for Rmm members */
	SEXP Rdval;                           /* real valued vector in result */
	SEXP Rival;                           /* integer valued vector in result */
	double *rval;                         /* data value at extrema */
	double *rlht;                         /* height of peak to left minimum */
	double *rrht;                         /* height of peak to right minimum */
	int *rpos;                            /* position of extrema */
	int *rismax;                          /* 0/1 flag if extrema is a maximum */
	int *lsupp;                           /* support range start in runs */
	int *rsupp;                           /* support range end in runs */
	int *ri;                              /* generic value in R integer vector */
	int *mmmap;                           /* map Rmm row/index to mm source */
	double xsupp;                         /* cut-off for support */
	int nkept;                            /* number of actual extrema */
	int mmID;                             /* index into the Rmm output */
	int i;

	Rmm = NULL;
	Rnames = NULL;
	Rdval = NULL;
	Rival = NULL;

	rpos = NULL;
	rismax = NULL;
	rval = NULL;
	rlht = NULL;
	rrht = NULL;
	
	/* Verify the final set of extrema and save them for R. */
	nkept = 0;
	for (i=0; i<nmm; i++) {
		if (mm[i].flags & MM_KEEP) {
			nkept += 1;
		}
	}
	
	Rmm = PROTECT( allocVector(VECSXP, 9) );
	Rnames = PROTECT( allocVector(STRSXP, 9) );
	setAttrib(Rmm, R_NamesSymbol, Rnames);

	Rival = PROTECT( allocVector(INTSXP, nkept) );
	rpos = INTEGER(Rival);
	SET_VECTOR_ELT(Rmm, 0, Rival);
	SET_STRING_ELT(Rnames, 0, mkChar("pos"));

	Rival = PROTECT( allocVector(LGLSXP, nkept) );
	rismax = INTEGER(Rival);
	SET_VECTOR_ELT(Rmm, 1, Rival);
	SET_STRING_ELT(Rnames, 1, mkChar("ismax"));

	Rdval = PROTECT( allocVector(REALSXP, nkept) );
	rval = REAL(Rdval);
	SET_VECTOR_ELT(Rmm, 2, Rdval);
	SET_STRING_ELT(Rnames, 2, mkChar("valsd"));

	mmmap = (int *) R_alloc(nmm, sizeof(*mmmap));
	
	for (i=0,mmID=0; i<nmm; i++) {
		if (mm[i].flags & MM_KEEP) {
			/* + 1 for R 1-based indexing. */
			rpos[mmID] = mm[i].pos + 1;
			rismax[mmID] = (mm[i].flags & MM_ISMAX) ? TRUE : FALSE;
			rval[mmID] = mm[i].valsd;
			mmmap[mmID] = i;
			mmID += 1;
		}
	}

	Rdval = PROTECT( allocVector(REALSXP, nkept) );
	rlht = REAL(Rdval);
	SET_VECTOR_ELT(Rmm, 3, Rdval);
	SET_STRING_ELT(Rnames, 3, mkChar("lht"));

	for (mmID=0; mmID<nkept; mmID++) {
		if ((0 == mmID) || !rismax[mmID]) {
			rlht[mmID] = NA_REAL;
		} else {
			rlht[mmID] = rval[mmID] - rval[mmID-1];
		}
	}

	Rdval = PROTECT( allocVector(REALSXP, nkept) );
	rrht = REAL(Rdval);
	SET_VECTOR_ELT(Rmm, 4, Rdval);
	SET_STRING_ELT(Rnames, 4, mkChar("rht"));

	for (mmID=0; mmID<nkept; mmID++) {
		if (((nkept - 1) == mmID) || !rismax[mmID]) {
			rrht[mmID] = NA_REAL;
		} else {
			rrht[mmID] = rval[mmID] - rval[mmID+1];
		}
	}

	Rival = PROTECT( allocVector(INTSXP, nkept) );
	ri = INTEGER(Rival);
	SET_VECTOR_ELT(Rmm, 5, Rival);
	SET_STRING_ELT(Rnames, 5, mkChar("lminID"));

	for (mmID=0; mmID<nkept; mmID++) {
		if ((0 == mmID) || !rismax[mmID]) {
			ri[mmID] = NA_INTEGER;
		} else {
			ri[mmID] = rpos[mmID-1];
		}
	}

	Rival = PROTECT( allocVector(INTSXP, nkept) );
	ri = INTEGER(Rival);
	SET_VECTOR_ELT(Rmm, 6, Rival);
	SET_STRING_ELT(Rnames, 6, mkChar("rminID"));

	for (mmID=0; mmID<nkept; mmID++) {
		if (((nkept-1) == mmID) || !rismax[mmID]) {
			ri[mmID] = NA_INTEGER;
		} else {
			ri[mmID] = rpos[mmID+1];
		}
	}

	Rival = PROTECT( allocVector(INTSXP, nkept) );
	lsupp = INTEGER(Rival);
	SET_VECTOR_ELT(Rmm, 7, Rival);
	SET_STRING_ELT(Rnames, 7, mkChar("lsuppID"));

	Rival = PROTECT( allocVector(INTSXP, nkept) );
	rsupp = INTEGER(Rival);
	SET_VECTOR_ELT(Rmm, 8, Rival);
	SET_STRING_ELT(Rnames, 8, mkChar("rsuppID"));

	/* Must go back to full scan because we don't keep the runID in Rmm. */
	for (mmID=0; mmID<nkept; mmID++) {
		lsupp[mmID] = NA_INTEGER;
		rsupp[mmID] = NA_INTEGER;
		if (rismax[mmID] && (0 < mmID)) {
			if (0.0 == fhsupp) {
				lsupp[mmID] = rpos[mmID];
			} else if (1.0 == fhsupp) {
				lsupp[mmID] = rpos[mmID-1];
			} else {
				xsupp = rval[mmID] - (fhsupp * rlht[mmID]);
				for (i=mm[mmmap[mmID]].runID; mm[mmmap[mmID-1]].runID<=i; i--) {
					if (rsd[i] < xsupp) {
						break;
					}
				}
				i += 1;
				lsupp[mmID] = xpos[i] + 1;
			}
		}
		
		if (rismax[mmID] && (mmID < (nkept-1))) {
			if (0.0 == fhsupp) {
				rsupp[mmID] = rpos[mmID];
			} else if (1.0 == fhsupp) {
				rsupp[mmID] = rpos[mmID+1];
			} else {
				xsupp = rval[mmID] - (fhsupp * rrht[mmID]);
				for (i=mm[mmmap[mmID]].runID; i<=mm[mmmap[mmID+1]].runID; i++) {
					if (rsd[i] < xsupp) {
						break;
					}
				}
				i -= 1;
				rsupp[mmID] = xpos[i] + 1;
			}
		}
	}

	UNPROTECT(11);
	return Rmm;
}


/**** htheap For Local Extrema ****/

/***
    build_htheap:  Put the peaks in a minheap according to their minht.
                   Assumes the memory allocated for heap can hold all 
                   extrema, ie. is at least nmm elements.
    args:          mm - extrema
                   nmm - number of extrema
                   heap - peak indices in mm, roughly sorted
                   nheap - actual entries in nheap
    modifies:  heap, nheap
***/
static void build_htheap(struct mmdata *mm, int nmm, int *heap, int *nheap) {
	int i;

	*nheap = 0;
	for (i=0; i<nmm; i++) {
		if (mm[i].flags & MM_ISMAX) {
			heap[*nheap] = i;
			*nheap += 1;
		}
	}
	if (0 == *nheap) {
		return;
	}

	for (i=(*nheap/2)-1; 0<=i; i--) {
		htheapify(heap, *nheap, i, mm);
	}
}

/***
    del_htheap:  Remove a heap entry by position in the heap.
    args:        heap - sorted indices of peaks in mm
                 nheap - size of heap
                 i - index in heap to delete
                 mm - extrema
    modifies:  heap, nheap
***/
static void del_htheap(int *heap, int *nheap, int i, struct mmdata *mm) {

	if (0 < *nheap) {
		heap[i] = heap[*nheap-1];
		*nheap -= 1;
		htheapify(heap, *nheap, i, mm);
	}
}

/***
    find_htheapID:  Return the index in heap for the stored index mmID.
    args:           heap - sorted indices of peaks in mm
                    nheap - size of heap
                    mmID - value inside heap to search for
                    mm - extrema
    returns:   position of mmID in heap, -1 if not in heap
***/
static int find_htheapID(int *heap, int nheap, int mmID) {
	int i;

	/* Scanning seems as efficient as searching depth-first through heap
	   (if we passed the height, could ignore children whose minht > mmID). */
	for (i=0; i<nheap; i++) {
		if (heap[i] == mmID) {
			return i;
		}
	}
	return -1;
}

/***
    htheapify:  Move elements around the heap starting at index i to ensure
                children have a larger minht in the extrema data.
    args:       heap - sorted indices of peaks in mm
                nheap - size of heap, or last element to adjust
                i - index to start balance (ignore if i invalid, ie. < 0)
                mm - extrema
    modifies:  heap
***/
static inline void htheapify(int *heap, int nheap, int i, struct mmdata *mm) {
	int lID, rID;                         /* left, right child ID */
	int minID;                            /* position of smaller value */
	int tmp;                              /* placeholder for swapping */
	int j;

	if (i < 0) {
		return;
	}

	minID = -1;
	j = i;
	while (1) {
		lID = (2 * j) + 1;
		rID = lID + 1;
		if ((lID < nheap) && (mm[heap[lID]].minht < mm[heap[j]].minht)) {
			minID = lID;
		} else {
			minID = j;
		}
		if ((rID < nheap) && (mm[heap[rID]].minht < mm[heap[minID]].minht)) {
			minID = rID;
		}
		if (minID != j) {
			tmp = heap[minID];
			heap[minID] = heap[j];
			heap[j] = tmp;
			j = minID;
		} else {
			break;
		}
	}
}


/**** Flat Handling ****/

/***
    scan_out:  Look outward from seed point src, starting at index ist and
               ending before iend, for values that are in the range except
               for one outlier.  Will not end on an outlier.
    args:      x - data to scan
               ist - start point of scan, between src (incl.) and iend (excl.)
               iend - end point of scan (excl.)
               xlo, xhi - bounds of valid data
               noutlier - number of points outside xlo,xhi range allowed
    modifies:  id
***/
static inline int scan_out(double *x, int ist, int iend,
                           double xlo, double xhi, int noutlier) {
	int endpt;                            /* interval bound, start or endpt */
	int dir;                              /* scan up (+1) or down (-1) */

	endpt = ist;
	if (ist != iend) {
		dir = (ist < iend) ? +1 : -1;
		while ((dir < 0) ? (iend < endpt) : (endpt < iend)) {
			endpt += dir;
			if ((x[endpt] < xlo) || (xhi < x[endpt])) {
				noutlier -= 1;
				if (noutlier < 0) {
					break;
				}
			}
		}

		while ((endpt != ist) && ((x[endpt] < xlo) || (xhi < x[endpt]))) {
			endpt -= dir;
		}
	}

	return endpt;
}

/***
    identify_flats_cover:  Identify flats by considering open/unassigned/bare
                           intervals not covered, registering flats if 
                           exposed length meets the minlen requirement.
                           flats must already be allocated with ndata elements.
                           nflat will be set to the actual number of entries
                           on return, and flats may contain garbage after that.
    args:                  srcs - flat regions at each data point
                           ndata - number of data poitns
                           minlen - minimum length of a flat
                           flats - identified flats, index into srcs
                           nflat - number of flats registered
    modifies:  flats, nflat
***/
static void identify_flats_cover(struct flatsrc *srcs, int ndata, int minlen,
                                 int *flats, int *nflat) {
	struct flatsrc *srt;                  /* sorted srcs */
	int *bare;                            /* intervals uncovered by flats */
	int nbare;                            /* number of uncovered intervals */
	int nivalop;                          /* number of intervals created/merged */
	int maxnbare;                         /* largest number of intervals */
	int len;                              /* uncovered point count in flat */
	int bst;                              /* index start of bare interval */
	int i, j;
	
	srt = NULL;
	bare = NULL;

	*nflat = 0;
	if (ndata <= 0) {
		return;
	}

	srt = (struct flatsrc *) R_alloc(ndata, sizeof(*srt));
	memcpy(srt, srcs, ndata * sizeof(*srcs));
	qsort(srt, ndata, sizeof(*srt), sortfn_flats);
	
	/* To avoid an O(n^2) algorithm (identify_flats_vote), we register flats by
	   tracking which points have not been covered by one.  This will be a 
	   simple array holding the start and end points of the unassigned 
	   intervals, with the start points in even indices and ends in odd.  We
	   don't expect a huge fracturing of the space and implement searches and
	   interval changes simply; if this isn't the case a tree would be better.

	   There will always be an even number of end points stored in bare.
	   Intervals are closed on both sides.  Intervals may have a length of one
	   (start == end).

	   A flat completely within an interval splits it in two.
	   An interval completely within a flat is removed.  
	   A flat overlapping an interval moves the interval's start/endpoint,
	   depending on which is covered by the flat.

	   The length of the flat for screening by minlen is the total shrinkage
	   of intervals.  There may be more than one interval changed by a flat
	   (think to either side).

	   Although in normal data sets the time of either the vote or cover is
	   negligible (less than 1 ms), at 100k samples of the asteroid data
	   LP flat detection goes from 507 s to 164, and Diw from 885 s to 199.
	*/

	bare = (int *) R_alloc(ndata, sizeof(*bare));
	nbare = 2;
	bare[0] = 0;
	bare[1] = ndata - 1;

	i = 0;
	nivalop = 0;
	maxnbare = nbare;
	while ((0 < nbare) && (i < ndata) && (minlen <= srt[i].len)) {
		len = 0;

		for (bst=0; bst<nbare; bst+=2) {
			if (srt[i].endID < bare[bst]) {
				/* We can stop, the bare intervals have moved beyond the flat. */
				break;
			}
			if (bare[bst+1] < srt[i].stID) {
				/* Not there yet. */
				continue;
			}
			
			if ((srt[i].stID <= bare[bst]) && (bare[bst+1] <= srt[i].endID)) {
				len += bare[bst+1] - bare[bst] + 1;
				nbare -= 2;
				for (j=bst; j<nbare; j++) {
					bare[j] = bare[j+2];
				}
				/* Check the next interval. */
				bst -= 2;
			} else if ((bare[bst] < srt[i].stID) && (srt[i].endID < bare[bst+1])) {
				len += srt[i].len;
				nbare += 2;
				for (j=nbare-1; j>=bst+1; j--) {
					bare[j] = bare[j-2];
				}
				bare[bst+1] = srt[i].stID - 1;
				bare[bst+2] = srt[i].endID + 1;
				/* Can skip the next new interval. */
				bst += 2;
			} else if (srt[i].stID <= bare[bst]) {
				len += srt[i].endID - bare[bst] + 1;
				bare[bst] = srt[i].endID - 1;
			} else if (bare[bst+1] <= srt[i].endID) {
				len += bare[bst+1] - srt[i].stID + 1;
				bare[bst+1] = srt[i].stID + 1;
			}
			nivalop += 1;
			if (maxnbare < nbare) {
				maxnbare = nbare;
			}
		}

		if (0 != (nbare % 2)) {
			error("have odd interval points");
		}
		for (j=1; j<nbare; j++) {
			if (bare[j-1] > bare[j]) {
				error("bare intervals not increasing sequence");
			}
		}

		if (minlen <= len) {
			/* Make sure in sorted order for return. */
			for (j=*nflat-1; (0<=j) && (srt[i].srcID<flats[j]); j--) {
				flats[j+1] = flats[j];
			}
			flats[j+1] = srt[i].srcID;
			*nflat += 1;
		}

		i += 1;
	}
}

/***
    sortfn_flats:  Comparison function to put the flats in decreasing length,
                   breaking ties by increasing source index.
    args:          val1, val2 - values to compare
    returns:   < 0 if val1 before val2, > 0 if after, 0 if tied (not possible)
***/
static int sortfn_flats(const void *val1, const void *val2) {
	struct flatsrc *src1;                 /* val1 coerced to actual data */
	struct flatsrc *src2;                 /* val2 coerced to actual data */

	src1 = (struct flatsrc *) val1;
	src2 = (struct flatsrc *) val2;

	if (src1->len == src2->len) {
		return (src1->srcID - src2->srcID);
	} else {
		return (src2->len - src1->len);
	}
}

/***
    build_flats:  Identify the longest flat from all source points
                  and store, together with summary information, in an
                  R list (generic vector) following the find_flats interface.
    args:         xsd - data flats built from
                  stat - statistics about raw data
                  ripple - ripple allowed to each side of source
                  srcs - flats per source point
                  flats - accepted flats (indices into srcs)
                  nflat - number of accepted flats
                  naskip - number of non-finite points skipped at each source
    returns:   vector with elements $src, $srcID, $endID, $len, $ht, $htsd
***/
static SEXP build_flats(double *xsd, struct msdstats *stat, double ripple,
                        struct flatsrc *srcs, int *flats, int nflat,
                        int *naskip) {
	SEXP Rflats;                          /* flats, our result */
	SEXP Rnames;                          /* element names for Rflats members */
	SEXP Rdval;                           /* real valued vector in result */
	SEXP Rival;                           /* integer valued vector in result */
	double *rd;                           /* data in Rdval */
	double *rhtsd;                        /* standardized height of flat */
	int *rsrc;                            /* source of flat */
	int *rstID;                           /* start point of flat */
	int *rendID;                          /* end point (incl.) of flat */
	int *rlen;                            /* length of flat */
	double xmin, xmax;                    /* data range over flat */
	double xlo, xhi;                      /* ripple bounds for flat */
	int i, j;

	Rflats = NULL;
	Rnames = NULL;
	Rdval = NULL;
	Rival = NULL;
	
	Rflats = PROTECT( allocVector(VECSXP, 7) );
	Rnames = PROTECT( allocVector(STRSXP, 7) );
	setAttrib(Rflats, R_NamesSymbol, Rnames);

	Rival = PROTECT( allocVector(INTSXP, nflat) );
	rsrc = INTEGER(Rival);
	for (i=0; i<nflat; i++) {
		rsrc[i] = flats[i] + naskip[flats[i]] + 1;
	}
	SET_VECTOR_ELT(Rflats, 0, Rival);
	SET_STRING_ELT(Rnames, 0, mkChar("src"));

	Rival = PROTECT( allocVector(INTSXP, nflat) );
	rstID = INTEGER(Rival);
	for (i=0; i<nflat; i++) {
		rstID[i] = srcs[flats[i]].stID + naskip[srcs[flats[i]].stID] + 1;
	}
	SET_VECTOR_ELT(Rflats, 1, Rival);
	SET_STRING_ELT(Rnames, 1, mkChar("stID"));

	Rival = PROTECT( allocVector(INTSXP, nflat) );
	rendID = INTEGER(Rival);
	for (i=0; i<nflat; i++) {
		rendID[i] = srcs[flats[i]].endID + naskip[srcs[flats[i]].endID] + 1;
	}
	SET_VECTOR_ELT(Rflats, 2, Rival);
	SET_STRING_ELT(Rnames, 2, mkChar("endID"));
	
	Rival = PROTECT( allocVector(INTSXP, nflat) );
	rlen = INTEGER(Rival);
	for (i=0; i<nflat; i++) {
		rlen[i] = rendID[i] - rstID[i] + 1;
	}
	SET_VECTOR_ELT(Rflats, 3, Rival);
	SET_STRING_ELT(Rnames, 3, mkChar("len"));

	Rdval = PROTECT( allocVector(REALSXP, nflat) );
	rd = REAL(Rdval);
	for (i=0; i<nflat; i++) {
		rd[i] = xsd[flats[i]] * stat->sigma;
	}
	SET_VECTOR_ELT(Rflats, 4, Rdval);
	SET_STRING_ELT(Rnames, 4, mkChar("srcval"));

	Rdval = PROTECT( allocVector(REALSXP, nflat) );
	rhtsd = REAL(Rdval);
	/* Note the height does not include any outliers. */
	for (i=0; i<nflat; i++) {
		xmin = xsd[srcs[flats[i]].srcID];
		xmax = xsd[srcs[flats[i]].srcID];
		xlo = xmin - ripple;
		xhi = xmax + ripple;
		for (j=srcs[flats[i]].stID; j<=srcs[flats[i]].endID; j++) {
			if ((xlo <= xsd[j]) && (xsd[j] <= xhi)) {
				xmin = fmin(xmin, xsd[j]);
				xmax = fmax(xmax, xsd[j]);
			}
		}
		rhtsd[i] = xmax - xmin;
	}
	SET_VECTOR_ELT(Rflats, 6, Rdval);
	SET_STRING_ELT(Rnames, 6, mkChar("htsd"));

	Rdval = PROTECT( allocVector(REALSXP, nflat) );
	rd = REAL(Rdval);
	for (i=0; i<nflat; i++) {
		rd[i] = rhtsd[i] * stat->sigma;
	}
	SET_VECTOR_ELT(Rflats, 5, Rdval);
	SET_STRING_ELT(Rnames, 5, mkChar("ht"));
 
	UNPROTECT(9);

	return Rflats;
}


/**** Flat mmsegtree ****/

/***
    build_mmtree:  Allocate and fill in the min and max segtrees.  Assumes
                   the data structure is not already allocated, ie. does no
                   freeing of any existing allocations.
    args:          xsd - data, lowest level of tree
                   nx - number of data points
                   mmt - minmax segtrees (allocated)
    modifies:  mmt members
***/
static void build_mmtree(double *xsd, int nx, struct mmtree *mmt) {
	double xl, xr;                        /* left/right children values */
	int nx2;                              /* data length rounded to power of 2 */
	int ntree;                            /* total allocation for tree */
	int ipar;                             /* parent node */
	int ist;                              /* first point on tree level */
	int ilast;                            /* last alloc'ed point on level */
	int i;

	/* The segtree is set up as a full binary array, twice the data length 
	   rounded up to a power of two, nx2, with extra values set to NA.
	   (The alternative is to halve the data length, but this requires tracking
	   the number of levels and the size of each.  Better memory use though.)
	     parent index   (i-1)/2
	     left child     (2*i)+1
	     right child    2*(i+1) = (2*i)+2
	   Nodes i1, i2 will be at the same level in the tree if the leftmost set
	   bit in i1+1 and i2+1 is the same.
	   Each array will contain a copy of the original data starting at index
	     ix = nx2 -1,
     a convenience despite the extra memory to avoid handling a separate array
     in the code.
	*/
	mmt->nx = nx;
	mmt->nlvl = 1 + (int) ceil(log2(nx));
	nx2 = 1 << (mmt->nlvl - 1);
	ntree = (2 * nx2) - 1;

	mmt->ix = nx2 - 1;
	mmt->tmin = (double *) R_alloc(ntree, sizeof(*(mmt->tmin)));
	mmt->tmax = (double *) R_alloc(ntree, sizeof(*(mmt->tmin)));

	memcpy(mmt->tmin+mmt->ix, xsd, nx*sizeof(*xsd));
	memcpy(mmt->tmax+mmt->ix, xsd, nx*sizeof(*xsd));

	for (i=mmt->ix+nx; i<ntree; i++) {
		mmt->tmin[i] = NA_REAL;
		mmt->tmax[i] = NA_REAL;
	}

	ist = mmt->ix;
	ilast = ist + nx2;
	while (0 < ist) {
		for (i=ist; i<ilast; i+=2) {
			ipar = (i - 1) / 2;
			
			xl = mmt->tmin[i];
			xr = mmt->tmin[i+1];
			/* NA value only mark the end of data.  If xl is NA then xr must also
			   be. */
			if (ISNA(xl)) {
				mmt->tmin[ipar] = NA_REAL;
			} else if (ISNA(xr)) {
				mmt->tmin[ipar] = xl;
			} else if (xl < xr) {
				mmt->tmin[ipar] = xl;
			} else {
				mmt->tmin[ipar] = xr;
			}

			xl = mmt->tmax[i];
			xr = mmt->tmax[i+1];
			if (ISNA(xl)) {
				mmt->tmax[ipar] = NA_REAL;
			} else if (ISNA(xr)) {
				mmt->tmax[ipar] = xl;
			} else if (xl < xr) {
				mmt->tmax[ipar] = xr;
			} else {
				mmt->tmax[ipar] = xl;
			}
		}

		ist = (ist - 1) / 2;
		ilast = (ilast - 1) / 2;
	}
}

/***
    walk_mmtree:  Find the range to one side of the source index that fits
                  within the ripple range.  Last point will always be within
                  the range.
    args:         mmt - minmax segtree for data (ie. local ranges)
                  srcID - source index of flat
                  dir - direction to search from source
                  xlo, xhi - max flat range over interval (ie. ripple range)
                  noutlier - number points outside xlo,xhi range allowed
    returns:   last index of data within xlo,xhi bounds
***/
static int walk_mmtree(struct mmtree *mmt, int srcID, int dir,
                       double xlo, double xhi, int noutlier) {
	int *block;                           /* ancestors of source */
	int ht;                               /* level in tree, data = 0 */
	int indir, outdir;                    /* search dir towards/away from i */
	int isib;                             /* sibling to current node */
	int rngsib;                           /* mm range vs. ripple at sibling */
	int iup;                              /* parent to current node */
	int iin;                              /* child inward towards source */
	int rngin;                            /* mm range vs. ripple at in child */
	int iout;                             /* child outward from source */
	int rngout;                           /* mm range vs. ripple at out child */
	int i;

	block = (int *) R_alloc(mmt->nlvl, sizeof(*block));

	if (dir < 0) {
		dir = -1;
	} else if (0 < dir) {
		dir = 1;
	} else {
		error("walk direction must not be zero");
	}

	/* Inbound from right child if dir == -1 goes to 0, +1 to -1.  
	   Outbound if dir == -1 goes to -1, +1 to 0. */
	indir = -(dir + 1) / 2;
	outdir = (dir - 1) / 2;

	/* Originally had thought that a penultimate mmtree, storing the next-
	   largest/smallest values, would help skip outliers.  But we know we
	   have only one outlier if the penultimate range is within the ripple 
	   spec and one of the min/max values is within the ripple but the other
	   is not - so many conditions gives a clue that this would be a tricky
	   mess to work out.  So we re-start the search after an outlier.  An
	   advantage is that we can allow arbitrarily many outliers. */

	/* The walk proceeds in two steps.  First we move outward from the 
	   source or last outlier, combining lateral shifts within the tree with
	   climbs.  A key realization is that the walk can never enter a direct
	   ancestor to the source, because the minmax range may then come from
	   points in the other direction.  This phase ends at a node where the
	   range overlaps or is outside the ripple spec.

	   Complicating this step, we also do outlier detection if we're in the
	   data row.

	   The second step is a descent to the outermost data point within the
	   ripple spec, without outliers.  If the inward child range is within
	   the ripple then we descend into the outer, otherwise the inner, unless
	   out of bound/outside ranges prevent us from entering the sub-tree. */

	/* There is a high overhead to the walk.  It begins to run faster than
	   the scan at 3000-4000 points, reaches a 2x improvement at 7500, 4.5x
	   at 10k, then grows to 40x at 100k, 85x at 250k, 130x at 500k, and
	   155x at 750k points.  For smaller data sets it can be half as fast,
	   with the minimum reached at 1000 points.
	     compare.timing(1:20, 500, 100)
	     compare.timing(c(1, 2, 5, 10, 15), 50000, 100)
	*/

	i = srcID + mmt->ix;
	ht = 0;
	block[ht] = i;
	block[ht+1] = (i - 1) / 2;
	
	while (0 <= noutlier) {
		/* Phase I: Move outward and then ascend and continue outward until the
		   minmax range is no longer within the ripple spec - somewhere underneath
		   is the next outlier. */
		isib = i + dir;
		rngsib = match_range(mmt, isib, i, xlo, xhi);
		if (RNG_OOB == rngsib) {
			/* Cannot move to invalid sibling. */
			break;
		}  else if (RNG_INSIDE == rngsib) {
			i = isib;
			while (1) {
				iup = (i - 1) / 2;
				if ((ht < mmt->nlvl) &&
				    (iup != block[ht+1]) &&
				    (RNG_INSIDE == match_range(mmt, iup, iup, xlo, xhi))) {
					ht += 1;
					block[ht+1] = (block[ht] - 1) / 2;
					i = iup;
				} else {
					isib = i + dir;
					if (!same_level(i, isib)) {
						/* No sibling to move to, we're done this step. */
						break;
					}
					if (RNG_INSIDE != match_range(mmt, isib, isib, xlo, xhi)) {
						break;
					}
					i = isib;
				}
			}
		} else if (RNG_OUTSIDE == rngsib) {
			/* At an outlier only if in the data row.  ht==0 is equivalent to
			   i > mmt->ix or same_level(i, srcID). */
			if (0 == ht) {
				noutlier -= 1;
				i = isib;
				continue;
			}
		} else if ((RNG_COVER == rngsib) || (RNG_OVERLAP == rngsib)) {
			i = isib;
		} else {
			error("unsupported range classification");
		}

		/* Phase II: Descend.  We end up back on the data row with a valid
		   point. */
		while (0 < ht) {
			iin = (2 * i) + 2 + indir;
			iout = (2 * i) + 2 + outdir;
			rngin = match_range(mmt, iin, iin, xlo, xhi);
			rngout = match_range(mmt, iout, iout, xlo, xhi);

			if ((RNG_OOB == rngin) || (RNG_OUTSIDE == rngin)) {
				/* Back down the inward child all the way to the bottom, then inward
				   one data point to the last valid. */
				i = iin;
				ht -= 1;
				while (0 < ht) {
					i = (2 * i) + 2 + indir;
					ht -= 1;
				}
				i -= dir;
			} else if ((RNG_OOB == rngout) || (RNG_OUTSIDE == rngout)) {
				i = iin;
				ht -= 1;
			} else if (RNG_INSIDE == rngin) {
				i = iout;
				ht -= 1;
			} else {
				i = iin;
				ht -= 1;
			}
			
			R_CheckUserInterrupt();
		}
	}

	/* Phase III: In case we've ended with more than one outlier, we need to
	   step back into range.  Worst case we finish at the source point. */
	while ((i != (srcID + mmt->ix)) &&
	       (RNG_INSIDE != match_range(mmt, i,srcID+mmt->ix, xlo, xhi))) {
		i -= dir;
	}

	return i - mmt->ix;
}

/***
    match_range:  Describe the overlap of the tree range to the ripple spec.
                  The sibling index is used to check lateral moves within the
                  tree; pass the test index i when checking a parent or child.
    args:         mmt - minmax segtree
                  i - index to test
                  isib - sibling index
                  xlo, xhi - ripple range
    returns:   RNG_* value describing overlap
***/
static inline int match_range(struct mmtree *mmt, int i, int isib,
                              double xlo, double xhi) {
	double xmin, xmax;                    /* range */

	if ((i != isib) && !same_level(i, isib)) {
		return RNG_OOB;
	} else {
		xmin = mmt->tmin[i];
		xmax = mmt->tmax[i];
		if (ISNA(xmin) || ISNA(xmax) || (xmax < xlo) || (xhi < xmin)) {
			return RNG_OUTSIDE;
		} else if ((xlo <= xmin) && (xmin <= xhi) &&
		           (xlo <= xmax) && (xmax <= xhi)) {
			return RNG_INSIDE;
		} else {
			return RNG_OVERLAP;
		}
	}
}

/***
    same_level:  Test if two indices to the mmtree are at the same depth.
    args:        i1, i2 - indices to compare
    returns:   true if levels same, 0 if not
***/
static inline int same_level(int i1, int i2) {
	int b;                                /* common bits in i1, i2 */

	/* Indices are at the same level if the leftmost set bit of i+1 is the same,
	   same, ie. if dividing by the common bits rounds to 1 for both inputs. */
	b = ((i1 + 1) & (i2 + 1));
	return (0 != b) && (1 == ((i1 + 1) / b)) && (1 == ((i2 + 1) / b));
	/*
	i1 += 1;
	i2 += 1;
	b = (i1 & i2);
	return (0 != b) && (1 == (i1 / b)) && (1 == (i2 / b));
	*/
}



/**** Built-In Self Tests ****/

/***
    bist_flats:  Test cases to check the consistency of the two approaches to
                 finding flats.  Undocumented function exposed (ie. not static)
                 in the shared library for testing.  A failure message is
                 printed only for those individual tests that don't match.
    returns:   R TRUE if all tests pass,  FALSE if any fail
               < 0 on failure (value depends on error)
***/
SEXP bist_flats(void) {
	int passed;                           /* overall pass/fail flag */

	passed = 1;

	/* First test cases check the tree walk against the scan.  Second 
	   check the new overlap algorithm. */
	passed &= bist_mmtree();
	passed &= bist_build_flats();

	return Rf_ScalarLogical(passed);
}

/***
    bist_mmtree:  Test cases to check the construction of the minmax segtree
                  for flats.
    returns:   true if all tests pass, 0 if any fail
***/
static int bist_mmtree(void) {
	double xsd[128];                      /* test data */
	int passed;                           /* overall pass/fail flag */

	passed = 1;

	/* TC 1.  mmsample(30). */
	xsd[ 0] =  2;   xsd[ 1] =  3;   xsd[ 2] = 11;   xsd[ 3] =  8;   xsd[ 4] =  7;
	xsd[ 5] =  5;   xsd[ 6] =  6;   xsd[ 7] =  1;   xsd[ 8] = 16;   xsd[ 9] =  4;
	xsd[10] = 14;   xsd[11] = 15;   xsd[12] = 10;   xsd[13] = 26;   xsd[14] = 12;
	xsd[15] =  9;   xsd[16] = 18;   xsd[17] = 28;   xsd[18] = 19;   xsd[19] = 17;
	xsd[20] = 21;   xsd[21] = 23;   xsd[22] = 20;   xsd[23] = 30;   xsd[24] = 29;
	xsd[25] = 27;   xsd[26] = 25;   xsd[27] = 13;   xsd[28] = 22;   xsd[29] = 24;

	passed &= test_mmtree_allsrc("tc 1", xsd, 30, 3, 0);
	passed &= test_mmtree_allsrc("tc 1", xsd, 30, 3, 1);
	/* Example of individual test case, to debug a failure.
	   build_mmtree(xsd, 30, &mmt);
	   passed &= test_mmtree(xsd, 30, 14, 3, 1);
	*/

	
	/* TC 2. mmsample(32). */
	xsd[ 0] =  3;   xsd[ 1] =  7;   xsd[ 2] = 28;   xsd[ 3] =  2;   xsd[ 4] =  6;
	xsd[ 5] =  5;   xsd[ 6] =  9;   xsd[ 7] =  8;   xsd[ 8] =  1;   xsd[ 9] = 15;
	xsd[10] = 12;   xsd[11] = 11;   xsd[12] = 13;   xsd[13] = 16;   xsd[14] = 14;
	xsd[15] = 10;   xsd[16] = 18;   xsd[17] = 23;   xsd[18] = 20;   xsd[19] = 19;
	xsd[20] = 31;   xsd[21] = 21;   xsd[22] = 24;   xsd[23] = 22;   xsd[24] = 17;
	xsd[25] = 26;   xsd[26] =  4;   xsd[27] = 27;   xsd[28] = 29;   xsd[29] = 25;
	xsd[30] = 30;   xsd[31] = 32;

	passed &= test_mmtree_allsrc("tc 2", xsd, 32, 2, 0);
	passed &= test_mmtree_allsrc("tc 2", xsd, 32, 2, 1);

	
	/* TC 3. mmsample(27). */
	xsd[ 0] =  2;   xsd[ 1] =  3;   xsd[ 2] = 11;   xsd[ 3] =  8;   xsd[ 4] =  7;
	xsd[ 5] =  5;   xsd[ 6] =  6;   xsd[ 7] =  1;   xsd[ 8] =  4;   xsd[ 9] = 14;
	xsd[10] = 10;   xsd[11] = 23;   xsd[12] = 12;   xsd[13] =  9;   xsd[14] = 16;
	xsd[15] = 21;   xsd[16] = 15;   xsd[17] = 19;   xsd[18] = 20;   xsd[19] = 18;
	xsd[20] = 27;   xsd[21] = 22;   xsd[22] = 13;   xsd[23] = 26;   xsd[24] = 24;
	xsd[25] = 25;   xsd[26] = 17;

	passed &= test_mmtree_allsrc("tc 3", xsd, 27, 4, 0);
	passed &= test_mmtree_allsrc("tc 3", xsd, 27, 4, 1);


	/* TC 4.  Simpler test case for outlier flats.  
     8 +/- 3 outliers 2 points to left (xsd[4]), 1 to right (xsd[9]).
     22 +/- 3 outliers immediate left (xsd[15]), none to right. */
	xsd[ 0] =  1;   xsd[ 1] =  2;   xsd[ 2] =  4;   xsd[ 3] =  5;   xsd[ 4] =  3;
	xsd[ 5] =  6;   xsd[ 6] =  7;   xsd[ 7] =  8;   xsd[ 8] =  9;   xsd[ 9] = 16;
	xsd[10] = 11;   xsd[11] = 10;   xsd[12] = 13;   xsd[13] = 12;   xsd[14] = 15;
	xsd[15] = 14;   xsd[16] = 21;   xsd[17] = 18;   xsd[18] = 19;   xsd[19] = 20;
	xsd[20] = 17;   xsd[21] = 22;   xsd[22] = 23;   xsd[23] = 24;   xsd[24] = 25;
	xsd[25] = 26;   xsd[26] = 27;   xsd[27] = 28;   xsd[28] = 29;   xsd[29] = 30;
	xsd[30] = 31;   xsd[31] = 32;

	passed &= test_mmtree_allsrc("tc 4", xsd, 32, 3, 0);
	passed &= test_mmtree_allsrc("tc 4", xsd, 32, 3, 1);

	
	/* TC 6.  galaxy interval width 16.  Mismatch at R src 34 or 39. */
	xsd[ 0] = 10171; xsd[ 1] =  9999; xsd[ 2] =  9957; xsd[ 3] =  9915;
	xsd[ 4] =  9754; xsd[ 5] =  9314; xsd[ 6] =  9141; xsd[ 7] =  3579;
	xsd[ 8] =  3676; xsd[ 9] =  1437;
	xsd[10] =  1311; xsd[11] =  1314; xsd[12] =   991; xsd[13] =   921;
	xsd[14] =   919; xsd[15] =   836; xsd[16] =   832; xsd[17] =   830;
	xsd[18] =   756; xsd[19] =   742;
	xsd[20] =   692; xsd[21] =   874; xsd[22] =  1082; xsd[23] =  1132;
	xsd[24] =   975; xsd[25] =   990; xsd[26] =  1012; xsd[27] =  1072;
	xsd[28] =  1219; xsd[29] =  1519;
	xsd[30] =  1712; xsd[31] =  1648; xsd[32] =  1746; xsd[33] =  1781;
	xsd[34] =  1989; xsd[35] =  1994; xsd[36] =  2021; xsd[37] =  1834;
	xsd[38] =  1685; xsd[39] =  1579;
	xsd[40] =  1674; xsd[41] =  1900; xsd[42] =  1872; xsd[43] =  1902;
	xsd[44] =  1777; xsd[45] =  1714; xsd[46] =  1540; xsd[47] =  1449;
	xsd[48] =  1563; xsd[49] =  1578;
	xsd[50] =  1357; xsd[51] =  1457; xsd[52] =  1464; xsd[53] =  1462;
	xsd[54] =  1815; xsd[55] =  1911; xsd[56] =  1794; xsd[57] =  1620;
	xsd[58] =  1970; xsd[59] =  2102;
	xsd[60] =  2719; xsd[61] =  3484; xsd[62] =  3754; xsd[63] =  8802;
	xsd[64] =  9305; xsd[65] = 10741;

	passed &= test_mmtree_allsrc("galaxy Diw=16 spacing", xsd, 66, 502, 0);
	passed &= test_mmtree_allsrc("galaxy Diw=16 spacing", xsd, 66, 502, 1);
	
	
	/* TC 7.  Spacing of galaxy data.  
	   The two versions also mis-match at R src 34 and 39. */
	xsd[ 0] = 178; xsd[ 1] = 133; xsd[ 2] =  75; xsd[ 3] = 217; xsd[ 4] = 452;
	xsd[ 5] = 179; xsd[ 6] =5678; xsd[ 7] =  86; xsd[ 8] =2249; xsd[ 9] = 133;
	xsd[10] =  48; xsd[11] = 327; xsd[12] = 125; xsd[13] =  18; xsd[14] = 260;
	xsd[15] =  13; xsd[16] =   6; xsd[17] =  91; xsd[18] =  33; xsd[19] =  56;
	xsd[20] =  12; xsd[21] =   6; xsd[22] = 116; xsd[23] = 183; xsd[24] =  10;
	xsd[25] =   7; xsd[26] =  51; xsd[27] =   4; xsd[28] =  55; xsd[29] =  16;
	xsd[30] = 177; xsd[31] =   9; xsd[32] =   4; xsd[33] =  17; xsd[34] =  19;
	xsd[35] =   6; xsd[36] = 194; xsd[37] = 214; xsd[38] = 166; xsd[39] =  26;
	xsd[40] =  25; xsd[41] =  29; xsd[42] = 111; xsd[43] = 151; xsd[44] = 355;
	xsd[45] = 209; xsd[46] = 113; xsd[47] = 107; xsd[48] =  39; xsd[49] = 225;
	xsd[50] =  24; xsd[51] =  33; xsd[52] =   7; xsd[53] =  65; xsd[54] =  60;
	xsd[55] = 121; xsd[56] = 251; xsd[57] =   1; xsd[58] = 141; xsd[59] =  26;
	xsd[60] = 292; xsd[61] =  35; xsd[62] =  22; xsd[63] = 221; xsd[64] =  54;
	xsd[65] =   4; xsd[66] = 124; xsd[67] =  40; xsd[68] =   5; xsd[69] = 418;
	xsd[70] = 156; xsd[71] =   4; xsd[72] =  77; xsd[73] = 351; xsd[74] = 273;
	xsd[75] = 643; xsd[76] =1057; xsd[77] = 305; xsd[78] =5070; xsd[79] = 724;
	xsd[80] =1490;

	passed &= test_mmtree_allsrc("galaxy spacing", xsd, 81, 283, 0);
	passed &= test_mmtree_allsrc("galaxy spacing", xsd, 81, 283, 1);

	return passed;
}

/***
    test_mmtree_allsrc:  For each data point, calculate the flat start and end
                         using both the v1 and v2 methods and compare if 
                         they're the same.  Prints the data point ID of any
                         mis-matches, without details.
    args:                testID - text describing the test case
                         xsd - data
                         ndata - number of data points
                         pm - ripple range about source point (absolute)
                         noutlier - number of outliers allowed
    returns:   true if flats the same for all source data points, 0 any differ
***/
static int test_mmtree_allsrc(const char *testID, double *xsd, int ndata,
                              int pm, int noutlier) {
	int passed;                           /* pass/fail flag */
	int i;

	passed = 1;
	for (i=0; i<ndata; i++) {
		if (!test_mmtree(xsd, ndata, i, pm, noutlier)) {
			Rprintf("    test %s - flat scan and tree walk differ at srcID %3d (%4.1f)\n",
			        testID, i, xsd[i]);
			passed = 0;
		}
	}

	return passed;
}

/***
    test_mmtree:  Generate a minmax segtree from the data and, if a valid
                  source data point is provided (0 <= src < ndata), walk
                  the tree and compare the result to the scan from v1.
    args:         xsd - data
                  ndata - number of data points
                  src - source data point of flat
                  pm - plus/minus range allowed (absolute) about xsd[src]
                  noutlier - number of outliers
    returns:   0 if comparison fails, true else (incl. no comparison)
***/
static int test_mmtree(double *xsd, int ndata, int src, int pm, int noutlier) {
	struct mmtree mmt;                    /* minmax tree */
	double xlo, xhi;                      /* ripple bounds */
	int stIDv1, endIDv1;                  /* flat bounds from scan */
	int stIDv2, endIDv2;                  /* flat bounds from tree walk */

	build_mmtree(xsd, ndata, &mmt);

	if ((src < 0) || (ndata <= src)) {
		return 1;
	}

	/* 0.05 slop to avoid fp accuracy problems with actual integer values. */
	xlo = xsd[src] - pm - 0.05;
	xhi = xsd[src] + pm + 0.05;
	
	stIDv2 = walk_mmtree(&mmt, src, -1, xlo,xhi, noutlier);
	endIDv2 = walk_mmtree(&mmt, src, +1, xlo,xhi, noutlier);
	
	stIDv1 = scan_out(xsd, src, 0, xlo,xhi, noutlier);
	endIDv1 = scan_out(xsd, src, ndata-1, xlo,xhi, noutlier);

	return ((stIDv1 == stIDv2) && (endIDv1 == endIDv2));
}

/***
    bist_build_flats:  Test cases to check the new processing of flats.
    returns:   true if all tests pass, 0 if any fail
***/
static int bist_build_flats(void) {
	struct flatsrc *flats;                /* test data */
	int *expsrc;                          /* expected source identifiers */
	int *actsrc;                          /* actual registered/accepted flats */
	int ndata;                            /* number of source points */
	int nf;                               /* number of flats we create */
	int nsrc;                             /* number of accepted flats */
	int minlen;                           /* shortest flat to pass */
	int passed;                           /* overall pass/fail flag */

	flats = NULL;
	expsrc = NULL;

	minlen = 30;

	ndata = 10 * minlen;                  
	flats = (struct flatsrc *) R_alloc(ndata, sizeof(*flats));
	
	expsrc = (int *) R_alloc(ndata, sizeof(*expsrc));
	actsrc = (int *) R_alloc(ndata, sizeof(*actsrc));
	memset(expsrc, 0, ndata * sizeof(*expsrc));

	passed = 1;
 
	/* TC 1: two long flats, two short, one long within first, none overlap. */
	/* Underline test messages to separate from debug traces.  Bold not shown
	   on timmy. */
	test_init_flats(flats, ndata);
	nf = 0;
	test_set_flat(&(flats[nf++]),   9,   5,  45);   /* yes */
	test_set_flat(&(flats[nf++]),  11,  10,  39);   /* no - within src 9 */
	test_set_flat(&(flats[nf++]),  51,  50,  78);   /* no - short by 1 */
	test_set_flat(&(flats[nf++]), 101, 110, 149);   /* yes */
	test_set_flat(&(flats[nf++]), 151, 150, 170);   /* no - short by 9 */
	expsrc[0] =   9;   expsrc[1] = 101;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 1", expsrc, 2, actsrc, nsrc);

	/* TC 2: five long flats, two abutting previous to each side, none overlap. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  41,  40,  80);   /* yes */
	test_set_flat(&(flats[nf++]),  82,  81, 120);   /* yes, abuts 41 to right */
	test_set_flat(&(flats[nf++]), 131, 130, 180);   /* yes */
	test_set_flat(&(flats[nf++]), 191, 190, 229);   /* yes, abuts 231 to left */
	test_set_flat(&(flats[nf++]), 231, 230, 275);   /* yes */
	expsrc[0] =  41;   expsrc[1] =  82;   expsrc[2] = 131;   expsrc[3] = 191;
	expsrc[4] = 231;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 2", expsrc, 5, actsrc, nsrc);

	/* TC 3: long flats at start/end of data, exactly filling bare intervals,
	         long covered that are outside bare intervals. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),   1,   0,  45);   /* yes, starting at 0 */
	test_set_flat(&(flats[nf++]),   2,   0,  40);   /* no, outside range */
	test_set_flat(&(flats[nf++]),  20,  20,  52);   /* no, last man covered */
	test_set_flat(&(flats[nf++]),  46,  46,  85);   /* yes, spanning L gap */
	test_set_flat(&(flats[nf++]),  86,  86, 135);   /* yes, center L */
	test_set_flat(&(flats[nf++]), 136, 136, 169);   /* yes, spanning R gap */
	test_set_flat(&(flats[nf++]), 170, 170, 219);   /* yes, center R */
	test_set_flat(&(flats[nf++]), 256, 256, 299);   /* yes, ending at ndata */
	test_set_flat(&(flats[nf++]), 257, 260, 299);   /* no, outside range */
	expsrc[0] =   1;   expsrc[1] =  46;   expsrc[2] =  86;   expsrc[3] = 136;
	expsrc[4] = 170;   expsrc[5] = 256;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 3", expsrc, 6, actsrc, nsrc);

	/* TC 4: Overlap flats, one long enough to register, one not. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  72,  72, 110);   /* no, exposed len 29 */
	test_set_flat(&(flats[nf++]), 101, 101, 150);   /* yes */
	test_set_flat(&(flats[nf++]), 140, 140, 180);   /* yes, exposed len 30 */
	expsrc[0] = 101;   expsrc[1] = 140;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 4", expsrc, 2, actsrc, nsrc);

	/* TC 5: Two long overlap flats to outside, two short to inside. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  21,  21,  60);   /* yes, exposed len 30 */
	test_set_flat(&(flats[nf++]),  51,  51,  99);   /* yes */
	test_set_flat(&(flats[nf++]),  95,  95, 128);   /* no,  exposed len 29 */
	test_set_flat(&(flats[nf++]), 172, 172, 205);   /* no,  exposed len 29 */
	test_set_flat(&(flats[nf++]), 201, 201, 249);   /* yes */
	test_set_flat(&(flats[nf++]), 245, 245, 279);   /* yes, exposed len 30 */
	expsrc[0] =  21;   expsrc[1] =  51;   expsrc[2] = 201;   expsrc[3] = 245;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 5", expsrc, 4, actsrc, nsrc);

	/* TC 6: As TC 5 but shift so outer flats short, inner pass. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  22,  22,  60);   /* no,  exposed len 29 */
	test_set_flat(&(flats[nf++]),  51,  51,  99);   /* yes */
	test_set_flat(&(flats[nf++]),  95,  95, 129);   /* yes, exposed len 30 */
	test_set_flat(&(flats[nf++]), 171, 171, 205);   /* no,  exposed len 30 */
	test_set_flat(&(flats[nf++]), 201, 201, 249);   /* yes */
	test_set_flat(&(flats[nf++]), 245, 245, 278);   /* no,  exposed len 29 */
	expsrc[0] =  51;   expsrc[1] =  95;   expsrc[2] = 171;   expsrc[3] = 201;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 6", expsrc, 4, actsrc, nsrc);

	/* TC 7: Two long flats that leave single bare point at each end, then two
	         that swallow those. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),   0,   0,  50);   /* no, single point left */
	test_set_flat(&(flats[nf++]),   1,   1,  90);   /* yes */
	test_set_flat(&(flats[nf++]), 221, 221, 298);   /* yes */
	test_set_flat(&(flats[nf++]), 251, 251, 299);   /* no, single point left */
	expsrc[0] =   1;   expsrc[1] = 221;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 7", expsrc, 2, actsrc, nsrc);

	/* The following tests capture failures when comparing the two versions
	   on real data. */

	/* TC 8: Single flat leaving single point at ends, then two swallowing.
	         Seen in enzyme Diw w=32. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),   0,   0,  50);   /* no, single point left */
	test_set_flat(&(flats[nf++]),   1,   1, 298);   /* yes */
	test_set_flat(&(flats[nf++]), 251, 251, 299);   /* no, single point left */
	expsrc[0] =   1;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 8", expsrc, 1, actsrc, nsrc);

	/* TC 9: Three flats leaving one point bare between, then two covering
	         from each side.  Seen in enzyme Diw w=32. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  51,  51, 100);   /* yes */
	test_set_flat(&(flats[nf++]),  71,  71, 101);   /* no, single point left */
	test_set_flat(&(flats[nf++]), 102, 102, 150);   /* yes */
	test_set_flat(&(flats[nf++]), 151, 151, 185);   /* no, single point left */
	test_set_flat(&(flats[nf++]), 152, 152, 200);   /* yes */
	expsrc[0] =  51;   expsrc[1] = 102;   expsrc[2] = 152;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 9", expsrc, 3, actsrc, nsrc);

	/* TC 10: Two long flats leaving middle section, then two flats starting
	          before and after.  Seen in enzyme Diw w=32. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),   0,   0, 100);   /* yes */
	test_set_flat(&(flats[nf++]),  90,  90, 130);   /* yes */
	test_set_flat(&(flats[nf++]), 169, 169, 220);   /* yes */
	test_set_flat(&(flats[nf++]), 200, 200, 299);   /* yes */
	expsrc[0] =   0;   expsrc[1] =  90;   expsrc[2] = 169;   expsrc[3] = 200;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 10", expsrc, 4, actsrc, nsrc);

	/* TC 11: Two long flats leaving middle section, then one flat to each side
	          abutting the open region.  Seen in enzyme Diw w=32. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),   0,   0, 100);   /* yes */
	test_set_flat(&(flats[nf++]),  60,  60, 101);   /* no, covered */
	test_set_flat(&(flats[nf++]), 199, 199, 235);   /* no, covered */
	test_set_flat(&(flats[nf++]), 200, 200, 299);   /* yes */
	expsrc[0] =   0;   expsrc[1] = 200;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 11", expsrc, 2, actsrc, nsrc);

	/* TC 12: Two long flats leaving small gap between, then cover from left.
	          Seen in astorb 1k sample.  Generated bad sequence in bare. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  42,  42, 156);   /* yes */
	test_set_flat(&(flats[nf++]), 153, 153, 189);   /* yes */
	test_set_flat(&(flats[nf++]), 193, 193, 250);   /* yes */
	/* Should have 0 41 190 192 251 299 as bare ends. */
	test_set_flat(&(flats[nf++]), 163, 163, 192);   /* no, len 3 open. */
	expsrc[0] =  42;   expsrc[1] = 193;   expsrc[2] = 153;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 12", expsrc, 3, actsrc, nsrc);

	/* TC 13: As TC 12, but overlap on other side. */
	test_init_flats(flats, nf);
	nf = 0;
	test_set_flat(&(flats[nf++]),  42,  42, 156);   /* yes */
	test_set_flat(&(flats[nf++]), 153, 153, 189);   /* yes */
	test_set_flat(&(flats[nf++]), 193, 193, 250);   /* yes */
	/* Should have 0 41 190 192 251 299 as bare ends. */
	test_set_flat(&(flats[nf++]), 190, 190, 222);   /* no, len 3 open. */
	expsrc[0] =  42;   expsrc[1] = 193;   expsrc[2] = 153;
	identify_flats_cover(flats, ndata, minlen, actsrc, &nsrc);
	passed &= compare_flats("bf 13", expsrc, 3, actsrc, nsrc);
	
	return passed;
}

/***
    test_init_flats:  Load dummy entries, all short (len < 5), into the
                      array of flat sources, which must already be allocated.
    args:             f - array of scan/walk results, at least nchanged elements
                      nsrc - number of data points needing dummy entries
    modifies:  f
***/
static void test_init_flats(struct flatsrc *f, int nsrc) {
	int i;

	for (i=0; i<nsrc; i++) {
		f[i].srcID = i;
		if (i < 4) {
			f[i].stID = i + 1;
		} else if ((nsrc - 4) < i) {
			f[i].stID = i - 5;
		} else {
			f[i].stID = i + 1;
		}
		f[i].endID = f[i].stID + 3;
		f[i].len = f[i].endID - f[i].stID + 1;
	}
}

/***
    test_set_flat:  Set up the flat information, overwriting the contents
                    of the single structure provided.  Length is calculated 
                    from endpoints.
    args:           f - scan/walk result to simulate
                    srcID - data point index centered in flat
                    stID, endID - endpoints of flat (incl.)
    modifies:  f
***/
static void test_set_flat(struct flatsrc *f, int srcID, int stID, int endID) {

	f->srcID = srcID;
	f->stID = stID;
	f->endID = endID;
	f->len = endID - stID + 1;
}

/***
    compare_flats:  Test that all members of each list are present in the
                    other.  Order is not important.
    args:           exp - expected source indices of flats
                    nexp - number of elements in exp
                    act - actual source indices of flats
                    nact - number of elements in act
    returns:   0 if set membership differs, true otherwise
***/
static int compare_flats(const char *testID, int *exp, int nexp,
                         int *act, int nact) {
	int passed;                           /* overall pass/fail flag */
	int i, j;

	passed = 1;
	
	if (nexp != nact) {
		passed = 0;
	} else {
		for (i=0; passed && (i<nexp); i++) {
			for (j=0; (j<nact) && (exp[i] != act[j]); j++) {
				/* Do nothing. */
			}
			if (j == nact) {
				passed = 0;
			}
		}
	}

	if (!passed) {
		Rprintf("    %s: flats built from sources differ from expected", testID);
	}

	return passed;
}


