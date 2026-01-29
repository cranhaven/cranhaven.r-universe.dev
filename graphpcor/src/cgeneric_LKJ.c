
/* cgeneric_LKJ.c
 *
 * Copyright (C) 2024 Elias Krainski
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

double *inla_cgeneric_LKJ(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp *data)
{

	// Lewandowski-Kurowicka-Joe - LKJ prior
	// This is the cgeneric implementatin of the
	// LKJ prior for a correlation matrix C with dimension N,
	// given (scalar) parameter 'eta'.
	// Parametrized from the
	// Canonical Partial Correlation - CPC.
	//     cpc: x[j] = tanh(\theta[i])
	// See #correlation-matrix-inverse-transform at
	//  https://mc-stan.org/docs/reference-manual/transforms.html

	// It returns for if 'cmd' is
	// 'graph': i,j index set for the upper triangle of Q;
	// 'Q': the inverse of C;
	// 'mu': 0.0 (zero);
	// 'initial': theta[k] = 3.0
	// 'log_prior': the LKJ prior for c_k, k = 1,...,M

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

	assert(!strcasecmp(data->doubles[0]->name, "eta"));
	double eta = data->doubles[0]->doubles[0];
	assert(eta > 0);

	assert(!strcasecmp(data->doubles[1]->name, "lc"));
	double lc = data->doubles[1]->doubles[0];

	assert(!strcasecmp(data->ints[2]->name, "sfixed"));   // this will always be the case
	int nsigmas = data->ints[2]->len;
	assert(nsigmas==N);
	int nsfixed = 0, sfixed[nsigmas];
	for (i = 0; i < nsigmas; i++) {
	  sfixed[i] = data->ints[2]->ints[i];
	  nsfixed += sfixed[i];
	}

	assert(!strcasecmp(data->doubles[2]->name, "sigmaref"));
	inla_cgeneric_vec_tp *sigmaref = data->doubles[2];
	assert(sigmaref->len > 0);
	assert(nsigmas == sigmaref->len);
	for (i = 0; i < nsigmas; i++) {
	  assert(sigmaref->doubles[i] > 0);
	}
	assert(!strcasecmp(data->doubles[3]->name, "sigmaprob"));
	inla_cgeneric_vec_tp *sigmaprob = data->doubles[3];
	assert(sigmaprob->len > 0);
	assert(nsigmas == sigmaprob->len);
	int nunkparams[3];
	nunkparams[0] = nsigmas - nsfixed;
	nunkparams[1] = nth;	// num params low L
	nunkparams[2] = nunkparams[0] + nunkparams[1];

	double sigmas[N];
	if(theta) {
	  k = 0;
	  for (i = 0; i < N; i++) {
	    if (sfixed[i]) {
	      sigmas[i] = sigmaref->doubles[i];
	    } else {
	      sigmas[i] = exp(theta[k++]);
	    }
	  }
	  assert(nunkparams[0]==k);
	}

	/*
	if (debug > 999) {
		printf("N=%d, nth=%d, M=%d, eta=%f, lc=%f\n", N, nth, M, eta, lc);
	}
*/

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
	// parametrized as correlation matrix inverse transform
	  double ldet, aJac;
	  cpcCholesky(&N, &theta[nunkparams[0]],
               &ret[offset], &ldet, &aJac);

    // include sigmas in the Cholesky
    k = 2;
    for(i=0; i<N; i++) {
      for(j=i; j<N; j++) {
        ret[k] *= sigmas[j];
        k++;
      }
    }

    // chol2inv
    int info;
    char uplo = 'L';
    dpptri_(&uplo, &N, &ret[offset], &info, F_ONE);

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
		ret[0] = nunkparams[2];
		for (i = 0; i < nunkparams[2]; i++) {
			ret[1 + i] = 0.0;
		}
	}
		break;

	case INLA_CGENERIC_LOG_PRIOR:
	{
		ret = Calloc(1, double);

		// ll : Cholesky of the correlation matrix
		// ldet: its log determinant
		// aJac: absolute of the determinant
		double ll[N * (N + 1) / 2];
		double ldR, ldJ;
		cpcCholesky(&N, &theta[0], &ll[0], &ldR, &ldJ);

		// store the log-prior
		ret[0] = ldR * (eta - 1.0) -lc;
//		printf("p(R|eta) = %2.4f\n", ret[0]);
		ret[0] += ldJ;

		// PC prior for sigma[i] (if not fixed)
		if(nunkparams[0]>0) {
		  double lam;
		  for (i = 0; i < nunkparams[0]; i++) {
		    if (!sfixed[i]) {
		      k = i; //theta->ints[i];
		      lam = -log(sigmaprob->doubles[k]) / sigmaref->doubles[k];
		      ret[0] += pclogsigma(theta[i], lam);
		    }
		  }
		}

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
