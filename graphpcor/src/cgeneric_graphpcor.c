
/* cgeneric_graphpcor.c
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

double *inla_cgeneric_graphpcor(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp *data)
{

	// theta : vector of unknown parameters
	// 0 <= length(theta) <= n + m
	// actualtheta : vector of n+m model parameters
	// actualtheta = { log(sigmas), lowtheta }
	// sigmas[i] = exp(actualtheta[i])
	// actualtheta[n+1:m] = H^{1/2}(actualtheta[n+1:m] - base[1:m])
	// = H^{1/2} actualtheta[n+1:m] - thetabasescaled

	double *ret = NULL;
	int i, j, k;

	// the size of the model
	assert(data->n_ints > 1);
	assert(!strcasecmp(data->ints[0]->name, "n"));	       // this will always be the case
	int N = data->ints[0]->ints[0];
	assert(N > 0);

	assert(!strcasecmp(data->ints[1]->name, "debug"));     // this will always be the case
	int debug = data->ints[1]->ints[0];

	assert(!strcasecmp(data->ints[2]->name, "ne"));	       // this will always be the case
	int ne = data->ints[2]->ints[0];
	assert(ne > 0);
	int M = N + ne;

	assert(!strcasecmp(data->ints[3]->name, "nfi"));       // this will always be the case
	int nfi = data->ints[3]->ints[0];

	assert(!strcasecmp(data->ints[4]->name, "ii"));	       // this will always be the case
	inla_cgeneric_vec_tp *ii = data->ints[4];
	assert(M == ii->len);

	assert(!strcasecmp(data->ints[5]->name, "jj"));	       // this will always be the case
	inla_cgeneric_vec_tp *jj = data->ints[5];
	assert(M == jj->len);

	assert(!strcasecmp(data->ints[6]->name, "iuq"));       // this will always be the case
	inla_cgeneric_vec_tp *iuq = data->ints[6];
	assert(M == iuq->len);

	assert(!strcasecmp(data->ints[7]->name, "iuqpac"));    // this will always be the case
	inla_cgeneric_vec_tp *iuqpac = data->ints[7];
	assert(M == iuqpac->len);

	assert(!strcasecmp(data->ints[8]->name, "ifi"));       // this will always be the case
	inla_cgeneric_vec_tp *ifi = data->ints[8];
	assert(nfi == ifi->len);

	assert(!strcasecmp(data->ints[9]->name, "jfi"));       // this will always be the case
	inla_cgeneric_vec_tp *jfi = data->ints[9];
	assert(nfi == jfi->len);

	assert(!strcasecmp(data->ints[10]->name, "itheta"));   // this will always be the case
	inla_cgeneric_vec_tp *itheta = data->ints[10];
	assert(M == itheta->len);

	assert(!strcasecmp(data->ints[11]->name, "sfixed"));   // this will always be the case
	int nsigmas = data->ints[11]->len;
	int nsfixed = 0, sfixed[nsigmas];
	for (i = 0; i < nsigmas; i++) {
		sfixed[i] = data->ints[11]->ints[i];
		nsfixed += sfixed[i];
	}
	int nunkparams[3];
	nunkparams[0] = nsigmas - nsfixed;
	nunkparams[1] = ne;				       // TO DO: corparamsfixed
	nunkparams[2] = nunkparams[0] + nunkparams[1];

	assert(!strcasecmp(data->doubles[0]->name, "lambda"));
	double lambda = data->doubles[0]->doubles[0];
	assert(lambda > 0.0);

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
	double lconst = data->doubles[3]->doubles[0];

	// printf("lconst = %2.5f \n", lconst);

	assert(!strcasecmp(data->doubles[4]->name, "thetabase"));
	assert(data->doubles[4]->len == ne);

	assert(!strcasecmp(data->mats[0]->name, "Ihalf"));
	assert(data->mats[0]->nrow == ne);
	assert(data->mats[0]->ncol == ne);

	double actualtheta[M];
	double sigmas[N];

	if (theta) {
		k = 0;
		for (i = 0; i < N; i++) {
			if (sfixed[i]) {
				actualtheta[i] = log(sigmaref->doubles[itheta->ints[i]]);
			} else {
				actualtheta[i] = theta[k++];
			}
			sigmas[i] = exp(actualtheta[i]);
		}
/*
		if (debug > 99) {
			printf("number of fixed sigma = %d\n", nsfixed);
			for (i = 0; i < N; i++)
				printf("%d ", sfixed[i]);
			printMat(sigmas, 1, N, "\nsigmas\n");
		}
*/
		for (i = 0; i < ne; i++) {
			actualtheta[itheta->ints[N + i]] = theta[k++];
		}

		/*
		 if (debug > 99) {
		   printMat(data->mats[0]->x, ne, ne, "I.5\n");
		 }
*/


	} else {
		for (i = 0; i < N; i++) {
			actualtheta[i] = NAN;
			sigmas[i] = NAN;
		}
		for (i = 0; i < ne; i++) {
			actualtheta[i] = NAN;
		}
	}

	switch (cmd) {
	case INLA_CGENERIC_GRAPH:
	{
		k = 2;
		ret = Calloc(k + 2 * M, double);
		ret[0] = N;				       /* dimension */
		ret[1] = M;
		/*
		 * number of (i <= j)
		 */
		for (i = 0; i < M; i++) {
			ret[k] = ii->ints[i];
			ret[M + k] = jj->ints[i];
			k++;
		}

	}
		break;
	case INLA_CGENERIC_Q:
	{
		int offset = 2;
		ret = Calloc(offset + M, double);
		// memset(ret + offset, 0, M * sizeof(double));
		ret[0] = -1;				       /* REQUIRED */
		ret[1] = M;				       /* REQUIRED */

		int m2 = N * (N + 1) / 2;
		double ll[N * N], qtemp[m2];

		// star L with diag, off-diag are zero
		k = 0;
		for (i = 0; i < N; i++) {
			for (j = 0; j < N; j++) {
				if (i == j) {
					ll[k] = ((double) N-i);
				} else {
					ll[k] = 0.0;
				}
				k++;
			}
		}

		/*
		 if (debug > 9999) {
			printMat(ll, N, N, "L[i,j]:\n");
		 }*/

		// add low theta to L
		k = 0;
		for (i = 0; i < M; i++) {
			if (ii->ints[i] != jj->ints[i]) {
				ll[iuq->ints[i]] = actualtheta[N+k++];
			}
		}

/*
		 if (debug > 9999) {
			printMat(ll, N, N, "L[i,j]:\n");
		 } */

		if (nfi > 0) {

		  /*
	    if (debug > 9999) {
				printf("filling %d entries\n", nfi);
			}*/

			fillL(&N, &nfi, &ifi->ints[0], &jfi->ints[0], &ll[0]);

		  /*
		   if (debug > 9999) {
				printMat(ll, N, N, "L[i,j]:\n");
		   }*/


		}
		// copy L to q (to be worked later in-place)
		k = 0;
		for (i = 0; i < N; i++) {
			for (j = i; j < N; j++) {
				qtemp[k++] = ll[N * i + j];
			}
		}

		/*
		 if (debug > 9999) {
			printf("L0 (upper)\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = i; j < N; j++) {
					printf("%2.3f ", qtemp[k++]);
				}
				printf("\n");
			}
		 } */


		// chol2inv: to compute V0 = Q_0^{-1}
		int info;
		char uplo = 'L';
		dpptri_(&uplo, &N, &qtemp[0], &info, F_ONE);

		/*
		 if (debug > 9999) {
			printf("V0 (upper)\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = i; j < N; j++) {
					printf("%2.3f ", qtemp[k++]);
				}
				printf("\n");
			}
		}
*/

		// si = diag(V0)^{1/2}
		// C = diag(1/si) V0 diag(1/si)
		double si[N];
		k = 0;
		for (i = 0; i < N; i++) {
			si[i] = sigmas[i] / sqrt(qtemp[k]);
			k += (N - i);
		}

		/*
		 if (debug > 999) {
			printMat(si, 1, N, "si:\n");
		 } */

		k = 0;
		for (i = 0; i < N; i++) {
			for (j = i; j < N; j++) {
				qtemp[k++] *= (si[i] * si[j]);
			}
		}

		/*
		 if (debug > 9999) {
			printf("V (upper)\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = i; j < N; j++) {
					printf("%2.3f ", qtemp[k++]);
				}
				printf("\n");
			}
		 } */

		// chol(V)
		dpptrf_(&uplo, &N, &qtemp[0], &info, F_ONE);

		/*
		if (debug > 9999) {
			printf("chol(V) (upper)\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = i; j < N; j++) {
					printf("%2.3f ", qtemp[k++]);
				}
				printf("\n");
			}
		} */


		// Q = chol2inv(chol(V))
		dpptri_(&uplo, &N, &qtemp[0], &info, F_ONE);

		/*
		 if (debug > 9999) {
			printf("Q (upper)\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = i; j < N; j++) {
					printf("%2.3f ", qtemp[k++]);
				}
				printf("\n");
			}
		 } */


		// copy the non-zero to return
		for (i = 0; i < M; i++) {
			ret[offset + i] = qtemp[iuqpac->ints[i]];
		}

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
			ret[1 + i] = -1.0;
		}
	}
		break;

	case INLA_CGENERIC_LOG_PRIOR:
	{
		ret = Calloc(1, double);

		// p(theta|lambda) = p(xi|lambda) |det(I(theta0))|
		// lconst = |det(I)^{1/2}|
		ret[0] = pcmultivar(ne, lambda,
                      &data->doubles[4]->doubles[0],
                      &data->mats[0]->x[0],
                      &lconst,
                      &theta[nunkparams[0]]);

		// PC prior for sigma[i]
		if(nunkparams[0]>0) {
		  double lam;
		  for (i = 0; i < nunkparams[0]; i++) {
		    if (!sfixed[i]) {
		      k = itheta->ints[i];
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
