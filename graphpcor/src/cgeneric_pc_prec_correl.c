
/* cgeneric_pc_prec_correl.c
 *
 * Copyright (C) 2025 Elias Krainski
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * The author's contact information:
 *
 *        Elias T Krainski
 *        CEMSE Division
 *        King Abdullah University of Science and Technology
 *        Thuwal 23955-6900, Saudi Arabia
 */

#include "graphpcor.h"
#include "graphpcor_utils.h"

double *inla_cgeneric_pc_prec_correl(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp *data)
{

	// This is a cgeneric implementation for the
	// PC-prior for a correlation matrix parametrized
	// from the Canonical Partial Correlation - CPC.
	// from its precision matrix

	// It returns for if 'cmd' is
	// 'graph': i,j index set for the upper triangle of Q;
	// 'Q': the inverse of C;
	// 'mu': 0.0 (zero);
	// 'initial': theta[k] = 3.0
	// 'log_prior': the PC-prior

	double *ret = NULL;
	int i, j, k, N, M, nth;

	// the size of the model
	assert(data->n_ints > 1);
	assert(!strcasecmp(data->ints[0]->name, "n"));	       // this will always be the case
	N = data->ints[0]->ints[0];
	assert(N > 0);
	M = (int) ((double) N * ((double) (N + 1)) / 2.0);
	nth = (int) ((double) N * ((double) (N - 1)) / 2.0);

	assert(!strcasecmp(data->ints[1]->name, "debug"));     // this will always be the case
//	int debug = data->ints[1]->ints[0];

//  printf("debug = %d\n", debug);

	assert(!strcasecmp(data->doubles[0]->name, "lambda"));
	double lambda = data->doubles[0]->doubles[0];
	assert(lambda > 0);

	assert(!strcasecmp(data->doubles[1]->name, "lconst"));
	double lconst = data->doubles[1]->doubles[0];
/*
	if (debug > 999) {
		printf("N=%d, nth=%d, M=%d, lambda=%f, lc=%f\n", N, nth, M, lambda, lconst);
	}
*/
	//double daux;
	double param[nth];

	if (theta) {
//	  printf("%d %d %d\n", N, M, nth);
	  for(i=0; i<nth; i++) {
	    param[i] = data->doubles[2]->doubles[i];
//	    printf("%2.3f, %2.3f \n", theta[i], param[i]);
	  }
	  int one = 1;
	  char trans = 'N';
	  double alpha = 1.0, beta = 1.0;
	  // param = H^{-1/2}\theta + theta_0
	//  printMat(data->mats[0]->x, nth, nth, "h.5\n");
	  dgemv_(&trans, &nth, &nth, &alpha, &data->mats[0]->x[0], &nth,
          &theta[0], &one, &beta, &param[0], &one, F_ONE);
/*
	  printf("\n");
	  for(i=0; i<nth; i++) {
	    printf("%2.3f %2.3f\n", theta[i], param[i]);
	  }
*/

/*
		param[nth - 1] = atan2(theta[nth - 1], theta[nth - 2]);
		if (param[nth - 1] < 0) {
			param[nth - 1] += 2.0 * M_PI;
		}
		daux = SQR(theta[nth - 1]) + SQR(theta[nth - 2]);
		if (nth > 2) {
			for (i = nth - 2; i > 0; i--) {
				// phi[i]
				param[i] = atan2(sqrt(daux), theta[i - 1]);
				daux += SQR(theta[i - 1]);
			}
		}
		// r = sqrt(sum_i(theta[i]^2))
		param[0] = sqrt(daux);
		for(i=0; i<nth; i++) {
		  if(fabs(param[i])<0.000000001) {
		    param[i]      = 0.0000000005;
		  }
		}
 */
/*
		if (debug > 999) {
			printMat(param, 1, nth, "param:\n");
		}
*/
	} else {
		for (i = 0; i < nth; i++) {
			param[i] = NAN;
		}
	}

	switch (cmd) {
	case INLA_CGENERIC_GRAPH:
	{
		k = 2;
		ret = Calloc(k + 2 * M, double);
		ret[0] = N;				       /* dimension */
		ret[1] = M;				       /* number of (i <= j) */

		for (i = 0; i < N; i++) {
			for (j = i; j < N; j++) {
				ret[M + k] = j;
				ret[k++] = i;
			}
		}
/*
		if (debug > 999) {
			printf("ii: ");
			for (k = 0; k < M; k++) {
				printf("%f ", ret[2 + k]);
			}
			printf("\njj: ");
			for (k = 0; k < M; k++) {
				printf("%f ", ret[2 + M + k]);
			}
			printf("\n");
		}
*/

	}
		break;
	case INLA_CGENERIC_Q:
	{
		// Q = (CC)^{-1}
		int offset = 2;

		ret = Calloc(offset + M, double);
		ret[0] = -1;				       /* REQUIRED */
		ret[1] = M;				       /* REQUIRED */

		// Cholesky of the correlation matrix
		int std=1;
		theta2Qcorrel(N, std, &param[0], &ret[offset]);

		int info;
		char uplo = 'L';
		dpptri_(&uplo, &N, &ret[offset], &info, F_ONE);	// chol2inv

	}
		break;
	case INLA_CGENERIC_MU:
	{
		// return (N, mu). if N==0 then mu is not needed as its taken to be mu[]==0
		ret = Calloc(1, double);
		ret[0] = 0;
	}
		break;

	case INLA_CGENERIC_INITIAL:
	{
		// return c(P, initials)
		// where P is the number of hyperparameters
		ret = Calloc(nth + 1, double);
		ret[0] = nth;
		for(i=1; i<=nth; i++) {
		  ret[i] = 0.00001;
		}
	}
		break;

	case INLA_CGENERIC_LOG_PRIOR:
	{
		ret = Calloc(1, double);
		double z[nth];
		for(i=0; i<nth; i++) {
		  z[i] = param[i] - data->doubles[2]->doubles[i];
//		  printf("%2.3f %2.3f \n", param[i], z[i]);
		}
		int one = 1;
		char trans = 'T';
		double alpha = 1.0, beta = 0.0;
		dgemv_(&trans, &nth, &nth, &alpha, &data->mats[0]->x[0],
         &nth, &z[0], &one, &beta, &param[0], &one, F_ONE);
		double d = 0.0;
		for(i=0; i<nth; i++) {
		  d += z[i]*param[i];
		}
	//	printf("%2.5f\n", d);

		// the log prior:
		// lconst should be equal to
		// log(lambda) -gamma(1+m/2)-log(m)-(m/2)*log(pi)
		ret[0] = lconst -lambda * d + fabs(1/sqrt(2*d));
/*
		if (debug > 999) {
			printf("log det Jacobian = %2.7f\n", ldJacobian);
		}
*/

	}
		break;

	case INLA_CGENERIC_VOID:
	case INLA_CGENERIC_LOG_NORM_CONST:
	case INLA_CGENERIC_QUIT:
	default:
		break;
	}

	return (ret);
}
