/*******************************************************************************
Dynamic vector memory allocation

This file is a modified version of the file nrutil.h of the R package
ltsa 1.4.4 [1]

References
[1] McLeod AI, Yu H, Krougly ZL (2007) Algorithms for linear time series
    analysis: With R package. Journal of Statistical Software 23(5):1-26
*******************************************************************************/

#include "trenchR.h"

VECTOR Vector( long n )
{
	// allocate a double vector and set default values to 0

	VECTOR vector;
	vector=(VECTOR)Calloc(n, typeof(double));
	memset( vector, 0, n * sizeof(double) );

	return vector;
}

void free_vector( VECTOR vector )
{
	Free( vector );
}
