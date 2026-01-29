
/* cgeneric_Wishart.c
 *
 * Copyright (C) 2024 Elias T Krainski
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

// this file contains inla_generic:
// inla_cgeneric_Wishart          for precision matrix

#include "graphpcor.h"
#include "graphpcor_utils.h"

double *inla_cgeneric_Wishart(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp *data)
{

	// This cgeneric model main objective is to compute the
	// Wishart prior for a precision matrix Q
	//
	// The parametrization for Q consider Q = LL' for
	// diag(L) = {d_1, ..., d_N}, d_i = \exp(\theta_i)
	// such that
	// | d_1 |
	// L = | l_1 d_2 |
	// | l_2 l_3 d_3 |
	// where l_i = \theta_{N+i}, i = 1 ... M-N = N(N-1)/2
	//
	// Given parameters
	// dof: degrees of freedom
	// R: inverse scale positive definite matrix,
	// given as diagonal and its upper (lower) part:
	// R_1, ..., R_n, R_{n+1}, ..., R_{M}, where M = N(N+1)/2
	// for N = 3 we have M = 6 and
	// | R_1 R_4 R_5 |
	// R = | R_4 R_2 R_6 |
	// | R_5 R_6 R_3 |
	//
	// It returns for if 'cmd' is
	// 'graph': i,j index set for the upper triangle of Q;
	// 'Q': the result of LL';
	// 'mu': 0.0 (zero);
	// 'initial': theta[k] = 3.0
	// 'log_prior': the Wishart prior for Q

	double *ret = NULL;
	int i, j, k, N, M;

	// the size of the model
	assert(data->n_ints > 1);
	assert(!strcasecmp(data->ints[0]->name, "n"));	       // this will always be the case
	N = data->ints[0]->ints[0];
	assert(N > 0);
	M = (int) ((double) N * ((double) (N + 1)) / 2.0);

	assert(!strcasecmp(data->ints[1]->name, "debug"));     // this will always be the case
//	int debug = data->ints[1]->ints[0];

	assert(!strcasecmp(data->doubles[0]->name, "dof"));
	double dof = data->doubles[0]->doubles[0];
	assert(dof > (N + 1));

	assert(!strcasecmp(data->doubles[1]->name, "R"));
	assert(data->doubles[1]->len == (N * N));
	double *R = &data->doubles[1]->doubles[0];

	assert(!strcasecmp(data->doubles[2]->name, "lcprior"));
	double lcprior = data->doubles[2]->doubles[0];
	assert(lcprior > 0);

/*
	if (debug > 999) {
		printf("Inputs\nN: %d, M: %d, dof: %f, lc: %f\n", N, M, dof, lcprior);
		printMat(R, N, N, "R");
	}
*/

	double dk[N];
	if (theta) {
		k = 0;
		for (i = 0; i < N; i++) {
			dk[i] = exp(theta[i]);
		}
/*
		if (debug > 999) {
			printMat(dk, 1, N, "Diag");
			printMat(dlq, N, N, "dlq");
		}
*/
	} else {
		for (i = 0; i < N; i++) {
			dk[i] = NAN;
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

	}
		break;
	case INLA_CGENERIC_Q:
	{
		int offset = 2;

		ret = Calloc(offset + M, double);
		// memset(ret + offset, 0, M * sizeof(double));
		ret[0] = -1;				       /* REQUIRED */
		ret[1] = M;				       /* REQUIRED */

		dl2Qu(N, &dk[0], &theta[N], &ret[2]);

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
		// return c(L, initials)
		// where M is the number of parameters
		// M = (# diagonal) + (# lower triangle)
		// N + N(N-1)/2
		ret = Calloc(M + 1, double);
		ret[0] = M;

		for (i = 1; i <= M; i++) {
			ret[i] = 0.0;
		}

		/*
		 * double dlQ[M]; exchangeableU(N, 0.9, &dlQ[0]);
		 *
		 * char uplo = 'L'; int info; dpptrf_(&uplo, &N, &dlQ[0], &info, F_ONE); // chol dpptri_(&uplo, &N, &dlQ[0], &info, F_ONE); //
		 * chol2inv
		 *
		 * dpptrf_(&uplo, &N, &dlQ[0], &info, F_ONE); // chol
		 *
		 * k2 = N; k=0; for(i=0; i<N; i++) { for(j=i; j<N; j++) { if(j==i) { ret[1+i] = log(dlQ[k++]); } else { ret[1+k2++] = dlQ[k++]; } }
		 * }
		 */

	}
		break;

	case INLA_CGENERIC_LOG_PRIOR:
	{

		// return log(c) + [ (r-(N+1))*log(|Q|) - tr(QR) ]/2 + |J|
		// where
		// c = 2^(rN/2) * |R|^{-r/2} *
		// \pi^(N(N-1)/4) *
		// \prod_j=1^N Gamma((r+1-j)/2)
		// log(c)t is input
		// J is the Jacobian

		ret = Calloc(1, double);

		double qq[N * N], m1[N * N], m2[N * N], J[M * M];

		// build the precision matrix
		theta2precision(N, &theta[0], &qq[0]);
/*
		if (debug > 999) {
			printMat(qq, N, N, "Q:");
		}
*/
		// RW product: m2 =- qq %*% R
		char uplo = 'L';
		double alpha = 1.0, beta = 0.0;
		dsymm_(&uplo, &uplo, &N, &N, &alpha, &qq[0], &N, &R[0], &N, &beta, &m2[0], &N, F_ONE);
/*
		if (debug > 999) {
			printMat(m2, N, N, "QR:");
		}
*/
		// trace
		double qrtrace = 0.0;
		k = 0;
		for (i = 0; i < N; i++) {
			qrtrace += m2[k];
			k += N + 1;
		}
/*
		 if (debug > 9) {
			printf("trace(QR) = %f \n", qrtrace);
		}
*/
		// Cholesky of Q
		int info = 0;
		dpotrf_(&uplo, &N, &qq[0], &N, &info, F_ONE);
		double halfldetQ = 0.0;
		for (i = 0; i < N; i++) {
			halfldetQ += log(qq[i * N + i]);       // theta[i];
		}
/*
		if (debug > 9) {
			printf("0.5*ldet(Q) = %f \n", halfldetQ);
		}
*/
		// compute J (numerical, using central difference)
		// h is defined as in priorfunc_wishart_generic() function
		// of r-inla/inlaprog/src/inla-priors.c
		double h, daux;
		h = 1.0e-6 * pow(exp(-halfldetQ), 1.0 / ((double) N));
		for (i = 0; i < N; i++) {
			if (h > 0.05 * qq[i * N + i])
				h = 0.0499 * qq[i * N + i];
		}
		double h2 = 2.0 * h;
/*
		if (debug > 9) {
			printf("hldQ = %f, h = %f\n", halfldetQ, h);
		}
*/
		// compute J[M*M], first wrt diag(Q) then others
		int kj;
		for (kj = 0; kj < M; kj++) {

			// compute Q- and Q+
			daux = theta[kj];
			theta[kj] = daux - h;
			theta2precision(N, &theta[0], &m1[0]);
			theta[kj] = daux + h;
			theta2precision(N, &theta[0], &m2[0]);
			theta[kj] = daux;

			// store derivatives
			k = 0;
			for (i = 0; i < N; i++) {
				daux = (m2[i * N + i] - m1[i * N + i]) / h2;
				J[kj * M + k] = daux;
				k++;
			}
			for (i = 0; i < N; i++) {
				for (j = i + 1; j < N; j++) {
					daux = (m2[i * N + j] - m1[i * N + j]) / h2;
					J[kj * M + k] = daux;
					k++;
				}
			}

		}
/*
		if (debug > 999) {
			printMat(J, M, M, "J:");
		}
*/
		int pivot[M], lwork = 2 * M + M + 1;
		double work[lwork], tau[M];
		dgeqp3_(&M, &M, &J[0], &M, &pivot[0], &tau[0], &work[0], &lwork, &info, F_ONE);
		double ldJacobian = 0.0;
		for (i = 0; i < M; i++) {
			ldJacobian += log(fabs(J[i * M + i]));
		}
/*
		if (debug > 99) {
			printf("\ndet Jacobian = %f\n", ldJacobian);
		}
*/
		if (ldJacobian < 0) {
			ldJacobian *= -1.0;
		}
		// the log-prior
		ret[0] = lcprior;			       // constant part
		ret[0] += (dof - ((double) N) - 1.0) * halfldetQ;
		ret[0] -= 0.5 * qrtrace;
		ret[0] += ldJacobian;

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
