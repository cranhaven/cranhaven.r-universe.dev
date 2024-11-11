// Memory and time efficient subsetting of chromosomal tracks (chrom, start, end)
// Author : Sylvain Mareschal <mareschal@ovsa.fr>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

/*
	Assumes the first argument is either 'sub' for subtracking or 'size' for overlap counting only
	
	Assumes the following 3 arguments are the targetted region 'chrom', 'start' and 'end, as single non-NA integer-coercible values
	
	Assumes 'chrom', 'start' and 'end' columns are :
	- properly named (vector argument name or column name in a data.frame)
	- unique along all the vectors and data.frame columns provided
	- ordered by 'chrom' then 'start', with NAs at the end (standard R behavior)
	
	'chrom' may be integer (so target chrom needs to be integer-coercible) or factor (target chrom needs to be character-coercible)
	'chrom' needs to be sorted numerically (by internal codes for factors, the standard 'order()' behavior)
	
	Returns a single data.frame merging all columns.
	The data.frame will have no row if chrom can not be found or if no element is found in the targetted region.
	
	Raises errors if conditions are not fullfilled.
*/

SEXP track(SEXP args);


/*
	Raises various errors if the arguments provided does not consist in a valid track, else return TRUE
*/

SEXP checktrack(SEXP args);
