
#include "MatTransMix.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "array.h"
#include "cephes_eigens.h"



void cephes_symmeigens_down(int p, double *eval, double **A, double (*determinant)){
	int i, j;
	double *As, *Evec, *Evalues;

	MAKE_VECTOR(As, p * (p + 1) / 2);

	for (i = 0; i < p; i++) {
		for (j = 0; j <= i; j++) As[(i * i + i)/2 + j] = A[i][j];
	}

	MAKE_VECTOR(Evec, p * p);	
	MAKE_VECTOR(Evalues, p);

	cephes_eigens(As, Evec, Evalues, p);
	
	for (i = 0; i < p; i++){
		eval[i] = Evalues[p - i - 1];
	}

	for (i = 0; i < p; i++) {
		for (j = 0; j < p; j++){
			A[j][p-i-1] = Evec[p * i + j];
		}
	}
	                        

	(*determinant)=1.0;
	
	for (i = 0; i < p; i++) (*determinant)*=eval[i];
	
	FREE_VECTOR(As);
	FREE_VECTOR(Evalues);
	FREE_VECTOR(Evec);

	return;
}


