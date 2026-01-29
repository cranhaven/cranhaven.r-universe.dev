
/* cgeneric_treepcor.c
 *
 * Copyright (C) 2023 Elias Krainski
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
 *        Elias Krainski
 *        CEMSE Division
 *        King Abdullah University of Science and Technology
 *        Thuwal 23955-6900, Saudi Arabia
 */

#include "graphpcor.h"
#include "graphpcor_utils.h"

double *inla_cgeneric_treepcor(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp *data)
{

	double *ret = NULL;
	int i, j, k, N, M, np, nparam;

	// the size of the model
	assert(data->n_ints > 1);
	assert(!strcasecmp(data->ints[0]->name, "n"));	       // this will always be the case
	N = data->ints[0]->ints[0];
	assert(N > 0);
	int N2 = N * N;
	M = (int) ((double) N * ((double) (N + 1)) / 2.0);

	assert(!strcasecmp(data->ints[1]->name, "debug"));     // this will always be the case
//	int debug = data->ints[1]->ints[0];

	assert(!strcasecmp(data->ints[2]->name, "np"));	       // this will always be the case
	np = data->ints[2]->ints[0];
	assert(np > 0);
	nparam = N + np;
	double v2[np];

/*
	if (debug > 99) {
		printf("(np = %d, N = %d, M = %d), N2 = %d\n", np, N, M, N2);
	}
*/

	assert(!strcasecmp(data->ints[3]->name, "nv"));	       // this will always be the case
//	inla_cgeneric_vec_tp *nv = data->ints[3];
	//assert(nv->len == np);

	assert(!strcasecmp(data->ints[4]->name, "ipar"));      // this will always be the case
	inla_cgeneric_vec_tp *ipar = data->ints[4];
	assert(ipar->len == N);

	assert(!strcasecmp(data->ints[5]->name, "iiv"));       // this will always be the case
	inla_cgeneric_vec_tp *iiv = data->ints[5];

	assert(!strcasecmp(data->ints[6]->name, "jjv"));       // this will always be the case
	inla_cgeneric_vec_tp *jjv = data->ints[6];
	assert(iiv->len == jjv->len);

	assert(!strcasecmp(data->ints[7]->name, "itop"));      // this will always be the case
	inla_cgeneric_vec_tp *itop = data->ints[7];

	assert(!strcasecmp(data->ints[8]->name, "ii"));	       // this will always be the case
	inla_cgeneric_vec_tp *ii = data->ints[8];

	assert(!strcasecmp(data->ints[9]->name, "jj"));	       // this will always be the case
	inla_cgeneric_vec_tp *jj = data->ints[9];

	assert(!strcasecmp(data->doubles[0]->name, "lambda"));
	double lambda = data->doubles[0]->doubles[0];
	assert(lambda > 0);

	assert(!strcasecmp(data->doubles[1]->name, "slambdas"));
	inla_cgeneric_vec_tp *slambdas = data->doubles[1];
	assert(slambdas->len > 0);
	assert(slambdas->len == N);

	assert(!strcasecmp(data->doubles[2]->name, "schildren"));
	inla_cgeneric_vec_tp *sch = data->doubles[2];
	assert(sch->len > 0);
	assert(sch->len == N);

	if (theta) {
		for (i = 0; i < np; i++) {
			v2[i] = exp(2.0 * theta[N + i]);
			/*
			 * if(debug>1) { printf("%2.1f %2.1f\n", theta[N+i], v2[i]); }
			 */
		}
	} else {
		for (i = 0; i < np; i++) {
			v2[i] = NAN;
		}
	}

	switch (cmd) {

	case INLA_CGENERIC_GRAPH:
	{
		k = 2;
		ret = Calloc(k + 2 * M, double);
		ret[0] = N;				       /* dimension */
		ret[1] = M;				       /* number of (i <= j) */
		for (i = 0; i < M; i++) {
			assert(ii->ints[i] <= jj->ints[i]);
			ret[k++] = ii->ints[i];
		}
		for (i = 0; i < M; i++) {
			ret[k] = jj->ints[i];
			/*
			 * if(debug>1){ printf("%d %2.1f %2.1f\n", i, ret[k-M], ret[k]); }
			 */
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

		char uplo = 'U';
		int info = 0;
		// double *qq = (double *) calloc(N*N, sizeof(double));
		double mcov[N2], qq[N2];

/*
		if (debug > 999) {
			printf(" nv:\n");
			for (i = 0; i < nv->len; i++) {
				printf("%d ", nv->ints[i]);
			}
			printf("\n iiv:\n");
			for (i = 0; i < iiv->len; i++) {
				printf("%d ", iiv->ints[i]);
			}
			printf("\n jjv:\n");
			for (i = 0; i < jjv->len; i++) {
				printf("%d ", jjv->ints[i]);
			}
			printf("\n itop[i,j]:\n");
			k = 0;
			for (i = 0; i < N; i++) {
				printf("sch[%d] = %2.1f, ", i, sch->doubles[i]);
				for (j = 0; j < N; j++) {
					printf("%d ", itop->ints[k]);
					k++;
				}
				printf("\n");
			}
		}
*/

		correlation_parent_children(np, N, iiv->len, &iiv->ints[0], &jjv->ints[0], &ipar->ints[0], &itop->ints[0], &sch->doubles[0], &v2[0],
					    &mcov[0]);

		k = 0;
		for (i = 0; i < N; i++) {
			for (j = 0; j < N; j++) {
				if (i == j) {
					qq[k] = 1.0;
				} else {
					qq[k] = 0.0;
				}
				k++;
			}
		}

		k = 0;
		for (i = 0; i < N; i++) {
			for (j = 0; j < N; j++) {
				mcov[k++] *= exp(theta[i] + theta[j]);
			}
		}

/*
		if (debug > 999) {
			printf("VV[i,j]:\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = 0; j < N; j++) {
					printf("%2.3f ", mcov[k++]);
				}
				printf("\n");
			}
		}
*/

		dposv_(&uplo, &N, &N, &mcov[0], &N, &qq[0], &N, &info, F_ONE);

/*
		if (debug > 99) {
			printf("INFO for dposv for Q is %d\n", info);
		}

		if (debug > 999) {
			printf("V[i,j]:\n");
			k = 0;
			for (i = 0; i < N; i++) {
				for (j = 0; j < N; j++) {
					printf("%2.3f ", mcov[k++]);
				}
				printf("\n");
			}
			k = 0;
			printf("Q[i,j]:\n");
			for (i = 0; i < N; i++) {
				for (j = 0; j < N; j++) {
					printf("%2.1f ", qq[k++]);
				}
				printf("\n");
			}
		}
*/

		// copy to ret
		k = 0;
		int k2 = offset;
		for (i = 0; i < N; i++) {
			for (j = 0; j < N; j++) {
				if (j >= i) {
					ret[k2++] = qq[k];
				}
				k++;
			}
		}
		assert(k2 == (offset + M));

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
		ret = Calloc(nparam + 1, double);
		ret[0] = nparam;
		for (i = 0; i < N; i++) {
			ret[1 + i] = -1.0;
		}
		for (i = 0; i < np; i++) {
			ret[1 + N + i] = 0.0;
		}
	}
		break;

	case INLA_CGENERIC_LOG_PRIOR:
	{
		ret = Calloc(1, double);
		// PC-priors for log(sd(c_i)) = theta[i-1];
		ret[0] = 0.0;
		double lam = 0;
		for (i = 0; i < N; i++) {
			lam = slambdas->doubles[i];
			ret[0] += log(lam) - lam * exp(theta[i]) + theta[i];
		}

//      char uplo = 'U';
		// int info=0, l;
		// double hldet0, hldet1, trc, kld;
		// double v2a[np], v2b[np], vp0[np], vp1[np], s0[N], s1[N];
		// double C0[N2], C1[N2], cc0[N2], cc1[N2];

		double d, dh, dd, hs = 0.005, kld[np], kldh[np];

		theta_parent_children_kldh(np, N, iiv->len,
					   &iiv->ints[0], &jjv->ints[0], &ipar->ints[0],
					   &itop->ints[0], &sch->doubles[0], &theta[N], hs, &kld[0], &kldh[0]);

/*
		if (debug > 99) {
			for (i = 0; i < np; i++)
				printf("k and d : %2.7f and %2.7f \n", kld[i], kldh[i]);
		}
*/

		for (i = 0; i < np; i++) {
			d = sqrt(2 * kld[i]);
			dh = sqrt(2.0 * kldh[i]);
			dd = fabs(dh - d) / hs;
			ret[0] += log(lambda) - lambda * d + log(dd);
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
