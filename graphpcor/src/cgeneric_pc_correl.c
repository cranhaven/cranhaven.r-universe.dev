
/* cgeneric_pc_correl.c
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

double *inla_cgeneric_pc_correl(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp *data)
{

	// This is the cgeneric implementatin of the
	// PC-prior for a correlation matrix C with dimension N,
	// given (scalar) parameter 'lambda'.
	// The correlation matrix is parametrized using the
	//  1. Canonical Partial Correlation, Lewandowski-Kurowicka-Joe (2009).
	//  2. hypershere decomposition, Rapisarda, Brigo and Mercurio (2007).
	// It returns for if 'cmd' is
	// 'graph': i,j index set for the upper triangle of Q;
	// 'Q': the inverse of C;
	// 'mu': 0.0 (zero);
	// 'initial': theta[k] = 3.0
	// 'log_prior': the PC-prior

	double *ret = NULL;
	int i, j, k, N, M;

	// the size of the model
	assert(data->n_ints > 1);
	assert(!strcasecmp(data->ints[0]->name, "n"));	       // this will always be the case
	N = data->ints[0]->ints[0];
	assert(N > 0);
	M = (int) ((double) N * ((double) (N + 1)) / 2.0);

	assert(!strcasecmp(data->ints[1]->name, "debug"));     // this will always be the case
	int debug = data->ints[1]->ints[0];

  assert(!strcasecmp(data->ints[2]->name, "itheta"));   // this will always be the case
  inla_cgeneric_vec_tp *itheta = data->ints[2];
  assert(M == itheta->len);

  assert(!strcasecmp(data->ints[3]->name, "sfixed"));   // this will always be the case
  int nsigmas = data->ints[3]->len;
  int nsfixed = 0, sfixed[nsigmas];
  for (i = 0; i < nsigmas; i++) {
    sfixed[i] = data->ints[3]->ints[i];
    nsfixed += sfixed[i];
  }
  int nunkparams[3];
  nunkparams[0] = nsigmas - nsfixed;
  nunkparams[1] = M-nsigmas;   // TO DO: corparamsfixed
  nunkparams[2] = nunkparams[0] + nunkparams[1];

  assert(!strcasecmp(data->doubles[0]->name, "lambda"));
	double lambda = data->doubles[0]->doubles[0];
	assert(lambda > 0);

	assert(!strcasecmp(data->doubles[1]->name, "sigmaref"));
	inla_cgeneric_vec_tp *sigmaref = data->doubles[1];
	assert(sigmaref->len > 0);
	assert(nsigmas == sigmaref->len);
	for (i = 0; i < nsigmas; i++) {
	  assert(sigmaref->doubles[i] > 0);
	}
	assert(!strcasecmp(data->doubles[2]->name, "sigmaprob"));
	inla_cgeneric_vec_tp *sigmaprob = data->doubles[2];
	assert(sigmaprob->len > 0);
	assert(nsigmas == sigmaprob->len);

	assert(!strcasecmp(data->doubles[3]->name, "lconst"));

/*
	if (debug > 999) {
		printf("N=%d, nth=%d, M=%d, lambda=%f\n", N, nth, M, lambda);
	}
*/

  double sigmas[N];
  if (theta) {
    k = 0;
    for (i = 0; i < N; i++) {
      if (sfixed[i]) {
        sigmas[i] = sigmaref->doubles[i];
      } else {
        sigmas[i] = exp(theta[k++]);
      }
    }
    assert(k==nunkparams[0]);
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

	}
		break;
	case INLA_CGENERIC_Q:
	{
		// Q = (CC)^{-1}, with C = LL'
		int offset = 2;

		ret = Calloc(offset + M, double);
		ret[0] = -1;				       /* REQUIRED */
		ret[1] = M;				       /* REQUIRED */

// Cholesky of the correlation matrix
//    if(parametrization==1) {
      // parametrized as correlation matrix inverse transform
      double ldet, aJac;
      cpcCholesky(&N, &theta[nunkparams[0]], &ret[offset], &ldet, &aJac);
  //  } else {
    //  double hld;
      //theta2gamma2Lcorr(N, &hld, &theta[0], &ret[offset]);
    //}

/*
		if (debug > 999) {
			printf("L:\n");
			for (i = 0; i < N; i++) {
				k = i;
				for (j = 0; j <= i; j++) {
					printf("%2.3f ", ret[offset + k]);
					k += (N - j - 1);
				}
				printf("\n");
			}
		}
*/

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
		ret = Calloc(nunkparams[2] + 1, double);
	  ret[0] = nunkparams[2];
	  for (i = 0; i < nunkparams[0]; i++) {
	    ret[1 + i] = 0.0;
	  }
	  for (i = nunkparams[0]; i < nunkparams[2]; i++) {
	    ret[1 + i] = 0.0;
	  }

	}
		break;

	case INLA_CGENERIC_LOG_PRIOR:
	{
		ret = Calloc(1, double);
	  // p(theta|lambda) = p(xi|lambda) |det(I(theta0))|
	  // lconst = |det(I)^{1/2}|
	  ret[0] = pcmultivar(
	    nunkparams[1], lambda,
	    &data->doubles[4]->doubles[0],
      &data->mats[0]->x[0],
      &data->doubles[3]->doubles[3],
      &theta[nunkparams[0]]);

      // PC prior for sigma[i]
      if(nunkparams[0]>0) {
        double lam;
        k=0;
        for (i = 0; i < nunkparams[0]; i++) {
          if (!sfixed[i]) {
            lam = -log(sigmaprob->doubles[i]) / sigmaref->doubles[i];
            ret[0] += pclogsigma(theta[k++], lam);
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
