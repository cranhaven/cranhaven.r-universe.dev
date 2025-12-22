/*
 *  utility.c
 *
 *  Created by Wei Sun on Fri Feb 15 2008.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include "utility.h"

/**********************************************************************
 * 
 * reorg
 *
 * Reorganize a vector to a matrix of given size. 
 *
 * Allocation done by R_alloc, so that R does the cleanup.
 *
 **********************************************************************/

void reorg(double *v, double ***m, int nrow, int ncol)
{
    int i;
    
    *m = (double **)R_alloc(nrow, sizeof(double*));
    
    (*m)[0] = v;
    if(nrow>1){
        for(i=1; i<nrow; i++){
            (*m)[i] = (*m)[i-1] + ncol;
        }
    }
}

void reorg_int(int *v, int ***m, int nrow, int ncol)
{
    int i;
    
    *m = (int **)R_alloc(nrow, sizeof(int*));
    
    (*m)[0] = v;
    if(nrow>1){
        for(i=1; i<nrow; i++){
            (*m)[i] = (*m)[i-1] + ncol;
        }
    }
}


