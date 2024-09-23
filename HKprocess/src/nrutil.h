/*******************************************************************************

This file is a modified version of the file nrutil.h of the R package
ltsa 1.4.4 [1]

References
[1] McLeod AI, Yu H, Krougly ZL (2007) Algorithms for linear time series
    analysis: With R package. Journal of Statistical Software 23(5):1-26
*******************************************************************************/

#ifndef _UTILS_H_
#define _UTILS_H_

typedef double*  VECTOR;

VECTOR Vector( long n );

void free_vector( VECTOR hVector );

#endif /* _UTILS_H_ */
