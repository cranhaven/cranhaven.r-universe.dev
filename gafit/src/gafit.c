/*
 *  gafit.c -- Genetic Algorithm Curve Fitting
 *  Copyright (C) 2000 Telford Tendys
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <R.h>
#include <Rinternals.h>

/*
 * Old compatibility code for R versions before generational garbage collection
 * no longer used, these versions of R are no longer supported
 */
#if 0
#if R_VERSION < R_Version( 1, 2, 0 )
#define VECTOR_ELT(x,i) (VECTOR(x)[i])
#define SET_VECTOR_ELT(x,i,v) (VECTOR(x)[i]=(v))
#endif
#endif


#define SAMPLE_SCORE          0
#define BROWN_NOISE           1
#define WINNER_PROJECT        2
#define LOG_NOISE             3
#define CYCLE_COUNT           4
#define TOTAL_SPECIALS        5

/*
 * Get the next item in the environment
 * return R_NilValue when nothing is left to find
 *
 * Before calling for the first time set:
 * -- my_ht = HASHTAB( env )
 * -- my_fr = FRAME( env )
 * -- my_ip = 0
 *
 * These temporary variables get changed by the call to scan_env_guts
 * the SEXP values returned are the values in the environment (not the symbols)
 * WARNING: no protection is used here so don't do anything to invoke GC
 * during the scan (here it is only used for reading and writing data values,
 * no outside functions are called).
 */

static SEXP scan_env_guts( SEXP *ht, SEXP *fr, int *ip )
{
	SEXP result;

 new_frame:
	if( *fr != R_NilValue )
	{
		result = CAR( *fr );
		*fr = CDR( *fr );
		return( result );
	}
	if( *ht != R_NilValue )
	{
		if( *ip < LENGTH( *ht ))
		{
			*fr = VECTOR_ELT( *ht, *ip );
			++*ip;
			goto new_frame;
		}
	}
	return( R_NilValue );
}

/*
 * Copy a block of working memory that represents one whole sample and its meta data.
 * Blocks are refered to by index number, starting at 0 (of course)
 */

static void copy_sample( SEXP workspace, int dofcount, int from, int to )
{
	if( from == to ) return;
	dofcount += TOTAL_SPECIALS;
	from *= dofcount;
	to *= dofcount;
	memcpy( REAL( workspace ) + to, REAL( workspace ) + from, sizeof( double ) * dofcount );
}

/*
 * Project one sample towards or away from another sample.
 * Since the target is getting affected, the target gets to choose the projection distance.
 *
 * workspace -- a SEXP containing a big vector of reals
 * dofcount -- the size of each block within the vector of reals (TOTAL_SPECIALS less actually)
 * offset -- the index within a block of the projection factor being used:
 *                     2 => winner projection
 *                     3 => loser projection
 * source -- the index (measured in whole blocks) of the read-only item
 * target -- the index (measured in whole blocks) of the read/write item
 */

static void project_sample( SEXP workspace, int dofcount, int offset, int source, int target )
{
	double p1, p2, *dp1, *dp2;
	int i;

	if( source == target ) return;
	source *= ( TOTAL_SPECIALS + dofcount );
	target *= ( TOTAL_SPECIALS + dofcount );
	dp1 = dp2 = REAL( workspace );
	dp2 += target;
	p1 = dp2[ offset ];
	dp2 += TOTAL_SPECIALS;
	p2 = 1 - p1;
	dp1 += source + TOTAL_SPECIALS;
	for( i = 0; i < dofcount; i++ )
	{
		double d = p1 * *dp1 + p2 * *dp2; 
		if( R_FINITE( d )) *dp2 = d;
		dp1++; dp2++;
	}
}

/*
 * Brownian motion on a sample to cause random drift in the population
 * The target gets to choose its own magnitude for the noise
 * The log noise is similar but uses a multiplicative scale that is usually
 * somewhere near 1. This sort of noise is better for situations where two
 * parameters have vastly differing magnitude (actually log noise is better
 * in any situation where the parameter is roughly the correct size).
 */

static void single_brownian( double *dp, double amplitude, double logamplitude )
{
	double q = *dp + amplitude * norm_rand();
	q *= 1 + logamplitude * ( unif_rand() - 0.5 );
	if( R_FINITE( q )) *dp = q;
}

static void brownian_sample( SEXP workspace, int dofcount, int target )
{
	double p1, p2, *dp;
	int i;

	GetRNGstate();
	target *= ( TOTAL_SPECIALS + dofcount );
	dp = REAL( workspace );
	dp += target;
	p1 = dp[ BROWN_NOISE ];
	p2 = dp[ LOG_NOISE ];
	single_brownian( dp + BROWN_NOISE, p1, p2 );
	single_brownian( dp + LOG_NOISE, p1, p2 );
	single_brownian( dp + WINNER_PROJECT, 1e-2, 0 );
	if( dp[ WINNER_PROJECT ] > 3 ) dp[ WINNER_PROJECT ] = 3;
	if( dp[ WINNER_PROJECT ] < -2 ) dp[ WINNER_PROJECT ] = -2; /* hard limit to sane values */
	dp += TOTAL_SPECIALS;
	for( i = 0; i < dofcount; i++ ) single_brownian( dp++, p1, p2 );
	PutRNGstate();
}

/*
 * Copy the values out of the sample and put them into the environment
 * This is not as easy as it looks because the storage mode needs to be
 * coerced into something suitable. Ignore the specials here.
 */

static void inject_sample_env( SEXP workspace, int dofcount, SEXP indirect, SEXP index, int target )
{
	double *dp;
	int i;

	target *= ( TOTAL_SPECIALS + dofcount );
	dp = REAL( workspace );
	dp += target + TOTAL_SPECIALS;
	for( i = 0; i < dofcount; i++ )
	{
		int j = INTEGER( index )[ i ];
		SEXP p = VECTOR_ELT( indirect, i );

		switch( TYPEOF( p ))
		{
			case LGLSXP: LOGICAL( p )[ j ] = ( *dp > 0.5 ) ? 1 : 0; break;
			case INTSXP: INTEGER( p )[ j ] = floor( *dp + 0.5 ); break;
			case REALSXP: REAL( p )[ j ] = *dp; break;
			case CPLXSXP:
				COMPLEX( p )[ j ].r = *dp++;
				COMPLEX( p )[ j ].i = *dp; i++;
				break;
			default: error( "Yeagahargggh!" );
		}
		dp++;
	}
}

/*
 * Evaluate the sample in the given environment and get the score
 * the score gets saved in the workspace
 */

static void evaluate_sample( SEXP workspace, int dofcount, SEXP env, SEXP func, int target )
{
	SEXP r;

	PROTECT( r = eval( func, env ));
	REAL( workspace )[ target * ( dofcount + TOTAL_SPECIALS ) + SAMPLE_SCORE ] = asReal( r );
	UNPROTECT( 1 );
}

/*
 * bubble-sort (single pass only, bottom to top)
 * the samples so that the top gets the lowest scores while the bottom
 * gets the highest scores.
 *
 * thermal is the probability of any given sorting step actually
 * sorting the score or instead sorting based on the highest noise
 * value (encourage noisy samples to widen the stepsize).
 *
 * Note that non-finite values get pushed to the bottom regardless of
 * what the thermal noise may be doing, this is an attempt to ensure that
 * obviously stupid values (like NA, NaN, etc) don't propagate.
 */

static void bubble_samples( SEXP workspace, int dofcount, int samplecount, double thermal )
{
	int i, dofall;

	GetRNGstate();
	dofall = dofcount + TOTAL_SPECIALS;
	for( i = samplecount; --i; )
	{
		int j = dofall * i;
		int decision = -1;
		double d1, d2;

		if( thermal > unif_rand())
		{     /* sort the biggest step size to the top */
			d1 = fabs( REAL( workspace )[ j + BROWN_NOISE ])
				+ fabs( REAL( workspace )[ j + LOG_NOISE ]);
			d2 = fabs( REAL( workspace )[ j + BROWN_NOISE - dofall ])
				+ fabs( REAL( workspace )[ j + LOG_NOISE - dofall ]);
			if( d1 > d2 ) decision = 1;
		}
		else
		{     /* sort the lowest score to the top */
			d1 = REAL( workspace )[ j + SAMPLE_SCORE ];
			d2 = REAL( workspace )[ j - dofall + SAMPLE_SCORE ];
			if( d1 < d2 ) decision = 1;
		}
		if( !R_FINITE( d2 )) decision = 1;   /* force any stupid values to the bottom */
		if( decision > 0 )
		{     /* swap the two samples using the last entry in the table as scratch space */
			copy_sample( workspace, dofcount, i, samplecount );
			copy_sample( workspace, dofcount, i - 1, i );
			copy_sample( workspace, dofcount, samplecount, i - 1 );
		}
	}
	PutRNGstate();
}

/*
 * WARNING: name of this function is exported, cannot be same as other items inside this
 * package namespace (else it will conflict).
 *
 * func -- something generated by the expression() call in R
 *         when evaluated, it will return a single scalar which is the `score'
 *
 * env --  the environment in which the func is evaluated.
 *         This environment gets manipulated as new parameter values are inserted
 *         into it. This also represents the starting values and the degrees
 *         of freedom and the parameter names (i.e. anything found in this
 *         environment is presumed to be a parameter).
 *
 * therm -- the thermal noise to be injected into the bubble sort (probability value):
 *          0   implies always sort low to high (lowest score goes to the top)
 *          1   implies sort the most random to the top
 *
 * maxiter -- the count of iterations (actually counts backwards to zero then exits)
 */

SEXP gafit_C( SEXP func, SEXP env, SEXP thermal_R, SEXP maxiter_R, SEXP samplecount_R, SEXP step_R, SEXP tolerance_R )
{
	int maxiter;
	int degrees_of_freedom = 0;
    int samplecount;
	double thermal;
	double stepsize;
	double tolerance;
	SEXP t1;
	SEXP t2;
	SEXP p;
	SEXP workspace;
	SEXP indirect;
	SEXP index;
	int t3;
	int i;
	int cycle_count;

	if( EXPRSXP == TYPEOF( func ))
	{
		func = VECTOR_ELT( func, 0 );
		if( LANGSXP != TYPEOF( func )) error( "Expecting an expression (arg 1)" );
	}
	if( !isEnvironment( env )) error( "Expecting an environment (arg 2)" );
	PROTECT( func );
	maxiter = asInteger( maxiter_R );
	samplecount = asInteger( samplecount_R );
	if( samplecount < 3 ) error( "Too few samples" );
	thermal = asReal( thermal_R );
	stepsize = asReal( step_R );
	tolerance = asReal( tolerance_R );
/*
 * Scan through the environment and get all the values of all the items,
 * these can be logicals, integers, reals or complex. Their mode will never
 * be changed by this algorithm, only their values will change.
 *
 * This requires firstly one scan to add up all the lengths, then one to do
 * the real work.
 */
	t1 = HASHTAB( env );
	t2 = FRAME( env );
	t3 = 0;
	while(( p = scan_env_guts( &t1, &t2, &t3 )) != R_NilValue )
	{
		switch( TYPEOF( p ))
		{
			case CPLXSXP:
				degrees_of_freedom += LENGTH( p );
			case LGLSXP:
			case INTSXP:
			case REALSXP:
				degrees_of_freedom += LENGTH( p );
				break;

			default:
				error( "Parameters may ONLY be logical, numerical or complex" );
		}
	}
/*
 * Convert everything into reals, complex numbers become a pair of reals
 * This forms the set of starting values. Note that the SEXP and integer index
 * of every degree of freedom is saved, we cannot save the actual pointers to
 * the data because the vector-heap compaction phase of garbage collection
 * would break us. With complex numbers, the index is saved twice and it is
 * just a known thing that complex numbers are pairs of reals <shrug>.
 */
	PROTECT( workspace = allocVector( REALSXP,
									  ( 1 + samplecount ) * ( TOTAL_SPECIALS + degrees_of_freedom )));
	PROTECT( indirect = allocVector( VECSXP, degrees_of_freedom ));
	PROTECT( index = allocVector( INTSXP, degrees_of_freedom ));

	t1 = HASHTAB( env );
	t2 = FRAME( env );
	t3 = 0; i = TOTAL_SPECIALS;

	while(( p = scan_env_guts( &t1, &t2, &t3 )) != R_NilValue )
	{
		int j;

		for( j = 0; j < LENGTH( p ); j++ )
		{
			SET_VECTOR_ELT( indirect, i - TOTAL_SPECIALS, p );
			INTEGER( index )[ i - TOTAL_SPECIALS ] = j;
			switch( TYPEOF( p ))
			{
				case LGLSXP: REAL( workspace )[ i++ ] = LOGICAL( p )[ j ]; break;
				case INTSXP: REAL( workspace )[ i++ ] = INTEGER( p )[ j ]; break;
				case REALSXP: REAL( workspace )[ i++ ] = REAL( p )[ j ]; break;
				case CPLXSXP:
					REAL( workspace )[ i++ ] = COMPLEX( p )[ j ].r;
					SET_VECTOR_ELT( indirect, i, p );
					INTEGER( index )[ i ] = j;
					REAL( workspace )[ i++ ] = COMPLEX( p )[ j ].i;
					break;
			}
		}
	}
/*
 * Now everything in the environment is available in a simple and direct manner (faster)
 * 4 extra values are stored as well as the standard degrees of freedom, these are
 * the ``specials'': score, brownian amplitude, and two projection factors.
 *
 * These last two allow the samples to pivot off each other resulting in faster gradient
 * following and smarter handling of narrow `trench' landscapes. All of these `meta genes'
 * are real numbers to make it easier to store them with the normal genes. The projection
 * meta genes are not self-effecting so meta genes cannot project off each other. I doubt
 * that the meta landscape is going to benefit much from projections (well this is a guess).
 * The projections get fixed boundaries imposed on them for sanity reasons (from -2 up to 3).
 * The brownian motion IS self-effecting so that the step size can increase in an exponential
 * manner where necessary.
 */
	evaluate_sample( workspace, degrees_of_freedom, env, func, 0 );
	REAL( workspace )[ BROWN_NOISE ] = stepsize;
	REAL( workspace )[ LOG_NOISE ] = stepsize;
	REAL( workspace )[ WINNER_PROJECT ] = 2.0;
	for( i = 1; i < samplecount; i++ )
	{
		copy_sample( workspace, degrees_of_freedom, 0, i );
		brownian_sample( workspace, degrees_of_freedom, i );
		inject_sample_env( workspace, degrees_of_freedom, indirect, index, i );
		evaluate_sample( workspace, degrees_of_freedom, env, func, i );
	}
/*
 * At this point there are a bunch of noisy samples in workspace, the first one
 * (i.e. index 0) is not noisy and contains the original parameters plus an evaluation
 * of func within those parameters. This is the start of the iterations.
 */
	cycle_count = 0;
	while( maxiter-- )
	{
/*
 * Check for a score within acceptable tolerance (presume best is at the top).
 */
		if( tolerance > REAL( workspace )[ SAMPLE_SCORE ]) { break; }
		cycle_count++;
/*
 * Now sort the stack, see if something better comes along.
 */
		bubble_samples( workspace, degrees_of_freedom, samplecount, thermal );
/*
 * Copy the best into the worst (encourages samples to try to be best)
 */
		copy_sample( workspace, degrees_of_freedom, 0, samplecount - 1 );
/*
 * Modify all samples except the best
 */
		for( i = samplecount; --i; )
		{
			project_sample( workspace, degrees_of_freedom, WINNER_PROJECT, 0, i );
			brownian_sample( workspace, degrees_of_freedom, i );
			inject_sample_env( workspace, degrees_of_freedom, indirect, index, i );
			evaluate_sample( workspace, degrees_of_freedom, env, func, i );
		}
	}
/*
 * Save the best results in the environment variables so that they
 * can be returned to the user
 */
	if( !maxiter )
	{
		bubble_samples( workspace, degrees_of_freedom, samplecount, 0 );
		bubble_samples( workspace, degrees_of_freedom, samplecount, 0 );
	}
	inject_sample_env( workspace, degrees_of_freedom, indirect, index, 0 );
/*
 * Save the cycle_count in case the end user wants to see it.
 */
	REAL( workspace )[ CYCLE_COUNT ] = cycle_count;
	UNPROTECT( 4 );
	return( workspace );
}

