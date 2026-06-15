#include "utils.h"

/************************************************************************************/
/* ---------- ---------- Kalman filter: Sequential Processing ---------- ---------- */
/************************************************************************************/
void cfkf_SP(
	/* inputs */
	int m, int d, int n,
	double *a0, double *P0,
	double *dt, int incdt,
	double *ct, int incct,
	double *Tt, int incTt,
	double *Zt, int incZt,
	double *HHt, int incHHt,
	double *GGt, int incGGt,
	double *yt,
	/* output */
	double *loglik)
{
	/*  Notation: */

	// m: the dimension of the state vector
	// d: the dimension of observations
	// n: the total number of observations

	/*  loglik is the output of the function and denotes the log-likelihood */
	/*  cfkf uses 'dcopy', 'dgemm' and 'daxpy' from BLAS */

	int m_x_m = m * m;
	int m_x_d = m * d;
	int NAsum;
	int t = 0;

	// integers and double precisions used in dcopy and dgemm
	int intone = 1;
	double dblone = 1.0, dblminusone = -1.0, dblzero = 0.0;
	// To transpose or not transpose matrix
	char *transpose = "T", *dont_transpose = "N";

	/* NA detection */
	int *NAindices = malloc(sizeof(int) * d);
	int *positions = malloc(sizeof(int) * d);

	/* Reduced arrays when NA's at time t */
	double *yt_temp = malloc(sizeof(double) * (d - 1));
	double *ct_temp = malloc(sizeof(double) * (d - 1));
	double *Zt_temp = malloc(sizeof(double) * (d - 1) * m);
	double *GGt_temp = malloc(sizeof(double) * (d - 1));

	double *Zt_t = malloc(sizeof(double) * (d * m));
	double *Zt_tSP = malloc(sizeof(double) * m);

	// SEQUENTIAL PROCESSING DEFINED VARIABLES:

#ifdef DEBUGME
	int SP_int = 3;
	int i_int = 0;
#endif

	int N_obs = 0;
	// Doubles for the SP iteration:
	double V;
	double Ft;
	double tmpFtinv;

	double *at = malloc(sizeof(double) * m);
	double *Pt = malloc(sizeof(double) * m * m);
	double *Kt = malloc(sizeof(double) * m);

	// SP temporary arrays:
	double *tmpmxSP = (double *)R_Calloc(m, double);
	double *tmpmxm = (double *)R_Calloc(m_x_m, double);

	*loglik = 0;

	/* at = a0 */
	F77_NAME(dcopy)
	(&m, a0, &intone, at, &intone);

	/* Pt = P0 */
	F77_NAME(dcopy)
	(&m_x_m, P0, &intone, Pt, &intone);

	/*********************************************************************************/
	/* ---------- ----------------- Begin Kalman Filter ----------------- ---------- */
	/*********************************************************************************/

	// Recursion across all time steps:
	while (t < n)
	{

		// How many NA's at time t?
		NAsum = numberofNA(&yt[d * t], NAindices, positions, d);

#ifdef DEBUGME
		Rprintf("\nNumber of NAs in iter %i: %i\n", t, NAsum);
#endif

		/*********************************************************************************/
		/* ---------- ---------- ---------- filter step ---------- ---------- ---------- */
		/*********************************************************************************/

		/*****************************************/
		/* ---------- case 1: no NA's:---------- */
		/*****************************************/
		if (NAsum == 0)
		{
			// Create Zt for time t
			F77_NAME(dcopy)
			(&m_x_d, &Zt[m_x_d * t * incZt], &intone, Zt_t, &intone);

			// Increment number of observations:
			N_obs += d;

			// Sequential Processing - Univariate Treatment of the Multivariate Series:
			for (int SP = 0; SP < d; SP++)
			{

#ifdef DEBUGME
				Rprintf("SP = %i", SP);
#endif

				// Get the specific values of Z for SP:
				for (int j = 0; j < m; j++)
				{
					Zt_tSP[j] = Zt_t[SP + j * d];
				}

				// Step 1 - Measurement Error:
				// Compute Vt[SP,t] = yt[SP,t] - ct[SP,t * incct] - Zt[SP,,t * incZt] %*% at[SP,t]

				// vt[SP,t] = yt[SP,t] - ct[SP,t * incct]
				V = yt[SP + d * t] - ct[SP + d * t * incct];

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						print_array(Zt_tSP, 1, m, "Zt[SP,]");
						Rprintf("\n V - Pre Mat Mult: %f", V);
					}
				}
#endif

				// vt[SP,t] = vt[SP,t] - Zt[SP,, t * incZt] %*% at[,t]
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose, &intone,
				 &intone, &m, &dblminusone,
				 Zt_tSP, &intone,
				 at, &m,
				 &dblone, &V, &intone FCONE FCONE);

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						Rprintf("\n V - Post Mat Mult: %f", V);
					}
				}
#endif

				// Step 2 - Function of Covariance Matrix:
				// Compute Ft = Zt[SP,,t * incZt] %*% Pt %*% t(Zt[SP,,t * incZt]) + diag(GGt)[SP]

				// First, Let us calculate:
				// Pt %*% t(Zt[SP,,t * incZt])
				// because we use this result twice

				F77_NAME(dgemm)
				(dont_transpose, transpose, &m,
				 &intone, &m, &dblone,
				 Pt, &m,
				 Zt_tSP, &intone,
				 &dblzero, tmpmxSP, &m FCONE FCONE);

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						print_array(tmpmxSP, m, 1, "tmpmxSP");
					}
				}
#endif

				// Ft = GGt[SP]
				Ft = GGt[SP + (d * t * incGGt)];

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						Rprintf("\n Ft: %f \n", Ft);
					}
				}
#endif

				// Ft = Zt[SP,,t*incZt] %*% tmpmxSP + Ft
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose, &intone,
				 &intone, &m, &dblone,
				 Zt_tSP, &intone,
				 tmpmxSP, &m,
				 &dblone, &Ft, &intone FCONE FCONE);

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						Rprintf("\n New Ft: %f \n", Ft);
					}
				}
#endif

				// Step 3 - Calculate the Kalman Gain:
				// Compute Kt = Pt %*% t(Zt[SP,,i * incZt]) %*% (1/Ft)

				// Inv Ft:
				tmpFtinv = 1 / Ft;

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						Rprintf("\n Inverse Ft: %f \n", tmpFtinv);
					}
				}
#endif

				// Kt is an m x 1 matrix

				// We already have tmpSPxm:
				// Kt = tmpmxSP %*% tmpFtinv
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose,
				 &m, &intone, &intone,
				 &dblone, tmpmxSP, &m,
				 &tmpFtinv, &intone,
				 &dblzero, Kt, &m FCONE FCONE);

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						print_array(Kt, m, 1, "Kalman Gain");
					}
				}
#endif

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						Rprintf("\n V - Post Mat Mult: %f", V);
					}
				}
#endif

				// Step 4 - Correct State Vector mean and Covariance:

				// Correction to att based upon prediction error:
				// att = Kt %*% V + att
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose,
				 &m, &intone, &intone,
				 &dblone, Kt, &m,
				 &V, &intone,
				 &dblone, at, &m FCONE FCONE);

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						print_array(at, m, 1, "at Correction");
					}
				}
#endif

				// Correction to covariance based upon Kalman Gain:
				// ptt = ptt - ptt %*% t(Z[SP,,i * incZt]) %*% t(Ktt)
				// ptt = ptt - tempmxSP %*% t(Ktt)
				F77_NAME(dgemm)
				(dont_transpose, transpose,
				 &m, &m, &intone,
				 &dblminusone,
				 tmpmxSP, &m,
				 Kt, &m,
				 &dblone, Pt, &m FCONE FCONE);

#ifdef DEBUGME
				if (SP == SP_int)
				{
					if (t == i_int)
					{
						print_array(Pt, m, m, "pt correction");
					}
				}
#endif

				// Step 5 - Update Log-Likelihood Score:
				*loglik -= 0.5 * (log(Ft) + (V * V * tmpFtinv));

#ifdef DEBUGME
				Rprintf("\n Log-Likelihood: %f \n", *loglik);
#endif
			}
		}
		/*******************************************/
		/* ---------- case 2: some NA's ---------- */
		/*******************************************/
		else
		{
			int d_reduced = d - NAsum;
			N_obs += d_reduced;

			// Temporary, reduced arrays:
			reduce_array(&yt[d * t], d, 1, yt_temp, positions, d_reduced);
			reduce_array(&ct[d * t * incct], d, 1, ct_temp, positions, d_reduced);
			reduce_array(&Zt[m_x_d * t * incZt], d, m, Zt_temp, positions, d_reduced);
			reduce_array(&GGt[d * t * incGGt], d, 1, GGt_temp, positions, d_reduced);

#ifdef DEBUGME
			print_int_array(positions, 1, d_reduced, "positions");
			print_array(yt_temp, 1, d_reduced, "yt_temp");
			print_array(ct_temp, 1, d_reduced, "ct_temp");
			print_array(Zt_temp, d_reduced, m, "Zt_temp");
			print_array(GGt_temp, 1, d_reduced, "GGt_temp");
#endif

			// Sequential Processing - Univariate Treatment of the Multivariate Series:
			for (int SP = 0; SP < d_reduced; SP++)
			{

#ifdef DEBUGME
				Rprintf("SP = %i", SP);
#endif

				// Get the specific values of Z for SP:
				for (int j = 0; j < m; j++)
				{
					Zt_tSP[j] = Zt_temp[SP + j * d_reduced];
				}

#ifdef DEBUGME
				print_array(Zt_tSP, 1, m, "Zt_tSP:");
#endif

				// Step 1 - Measurement Error:
				// Compute Vt[SP,t] = yt[SP,t] - ct[SP,t * incct] + Zt[SP,,t * incZt] %*% at[SP,t]

				// vt[SP,t] = yt[SP,t] - ct[SP,t * incct]
				V = yt_temp[SP] - ct_temp[SP];

#ifdef DEBUGME
				Rprintf("\n Pre mat-mult V = %f", V);
#endif

				// vt[SP,t] = vt[SP,t] - Zt[SP,, t * incZt] %*% at[,t]
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose, &intone,
				 &intone, &m, &dblminusone,
				 Zt_tSP, &intone,
				 at, &m,
				 &dblone, &V, &intone FCONE FCONE);

#ifdef DEBUGME
				Rprintf("\n Post mat-mult V = %f", V);
#endif

				// Step 2 - Function of Covariance Matrix:
				// Compute Ft = Zt[SP,,t * incZt] %*% Pt %*% t(Zt[SP,,t * incZt]) + diag(GGt)[SP]

				// First, Let us calculate:
				// Pt %*% t(Zt[SP,,t * incZt])
				// because we use this result twice

				F77_NAME(dgemm)
				(dont_transpose, transpose, &m,
				 &intone, &m, &dblone,
				 Pt, &m,
				 Zt_tSP, &intone,
				 &dblzero, tmpmxSP, &m FCONE FCONE);

#ifdef DEBUGME
				print_array(tmpmxSP, m, 1, "tmpmxSP:");
#endif

				// Ft = GGt[SP]
				Ft = GGt_temp[SP];

#ifdef DEBUGME
				Rprintf("\n Ft: %f \n", Ft);
#endif

				// Ft = Zt[SP,,i*incZt] %*% tmpmxSP + Ft
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose, &intone,
				 &intone, &m, &dblone,
				 Zt_tSP, &intone,
				 tmpmxSP, &m,
				 &dblone, &Ft, &intone FCONE FCONE);

				// Step 3 - Calculate the Kalman Gain:
				// Compute Kt = Pt %*% t(Zt[SP,,i * incZt]) %*% (1/Ft)

#ifdef DEBUGME
				Rprintf("\n New Ft: %f \n", Ft);
#endif

				// Inv Ft:
				tmpFtinv = 1 / Ft;

#ifdef DEBUGME
				Rprintf("\n Inverse Ft: %f \n", tmpFtinv);
#endif

				// Kt is an m x 1 matrix

				// We already have tmpSPxm:
				// Kt = tmpmxSP %*% tmpFtinv
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose, &m,
				 &intone, &intone, &dblone,
				 tmpmxSP, &m,
				 &tmpFtinv, &intone,
				 &dblzero, Kt, &m FCONE FCONE);

				// Step 4 - Correct State Vector mean and Covariance:

				// Correction to att based upon prediction error:
				// att = Kt %*% V + att
				F77_NAME(dgemm)
				(dont_transpose, dont_transpose, &m,
				 &intone, &intone, &dblone,
				 Kt, &m,
				 &V, &intone,
				 &dblone, at, &m FCONE FCONE);

				// Correction to covariance based upon Kalman Gain:
				// ptt = ptt - ptt %*% t(Z[SP,,i * incZt]) %*% t(Ktt)
				// ptt = ptt - tempmxSP %*% t(Ktt)
				F77_NAME(dgemm)
				(dont_transpose, transpose, &m,
				 &m, &intone, &dblminusone,
				 tmpmxSP, &m,
				 Kt, &m,
				 &dblone, Pt, &m FCONE FCONE);

				// Step 5 - Update Log-Likelihood Score:
				*loglik -= 0.5 * (log(Ft) + V * V * tmpFtinv);
				// Increment number of observations for the Log-likelihood at the end:
			}
#ifdef DEBUGME
			Rprintf("\n SP Iteration Completed Successfully.\n");
#endif
		}
		/*********************************************************************************/
		/*  ---------- ---------- ------- prediction step -------- ---------- ---------- */
		/*********************************************************************************/

		/* ---------------------------------------------------------------------- */
		/* at[,t + 1] = dt[,t * incdt] + Tt[,,t * incTt] %*% att[,t]              */
		/* ---------------------------------------------------------------------- */

#ifdef DEBUGME
		print_array(at, 1, m, "at:");
#endif

		// tmpmxm = Tt[,,i * incTt] %*% att[,i]
		F77_NAME(dgemm)
		(dont_transpose, dont_transpose, &m,
		 &intone, &m, &dblone,
		 &Tt[m_x_m * t * incTt], &m,
		 at, &m,
		 &dblzero, tmpmxSP, &m FCONE FCONE);

		/* at[,t + 1] = dt[,t] */
		F77_NAME(dcopy)
		(&m, &dt[m * t * incdt], &intone, at, &intone);
		F77_NAME(daxpy)
		(&m, &dblone, tmpmxSP, &intone, at, &intone);

#ifdef DEBUGME
		print_array(at, 1, m, "atp1:");
		print_array(Pt, m, m, "Pt:");
#endif

		/* ------------------------------------------------------------------------------------- */
		/* Pt[,,t + 1] = Tt[,,t * incTt] %*% Ptt[,,t] %*% t(Tt[,,t * incTt]) + HHt[,,t * incHHt] */
		/* ------------------------------------------------------------------------------------- */

		/* tmpmxm = Ptt[,,i] %*% t(Tt[,,i * incTt]) */
		F77_NAME(dgemm)
		(dont_transpose, transpose, &m,
		 &m, &m, &dblone,
		 Pt, &m,
		 &Tt[m_x_m * t * incTt], &m,
		 &dblzero, tmpmxm, &m FCONE FCONE);

		/* Pt[,,i + 1] = HHt[,,i * incHHt] */
		F77_NAME(dcopy)
		(&m_x_m, &HHt[m_x_m * t * incHHt], &intone, Pt, &intone);

#ifdef DEBUGME
		print_array(&HHt[m_x_m * t * incHHt], m, m, "HHt:");
#endif

		/* Pt[,,i + 1] = Tt[,,i * incTt] %*% tmpmxm + Pt[,,i + 1] */
		F77_NAME(dgemm)
		(dont_transpose, dont_transpose, &m,
		 &m, &m, &dblone,
		 &Tt[m_x_m * t * incTt], &m,
		 tmpmxm, &m,
		 &dblone, Pt, &m FCONE FCONE);

#ifdef DEBUGME
		print_array(Pt, m, m, "Ptp1:");
#endif

#ifdef DEBUGME
		print_array(at, 1, m, "at:");
		print_array(Pt, m, m, "Pt:");
		Rprintf("\n---------- iteration nr. %i ----------\n", t + 1);
#endif

		// end iteration
		t++;
	}
	/**************************************************************/
	/* ---------- ---------- end recursions ---------- ---------- */
	/**************************************************************/

#ifdef DEBUGME
	Rprintf("\n Log-Likelihood: %f \n", *loglik);
#endif

	// Update the final Log-Likelihood Score:
	*loglik -= 0.5 * N_obs * log(2 * M_PI);

	// Memory clean - R_Free vectors / matrices:
	R_Free(tmpmxSP);
	R_Free(tmpmxm);

	free(positions);
	free(yt_temp);
	free(ct_temp);
	free(Zt_temp);
	free(GGt_temp);
	free(Zt_t);
	free(Zt_tSP);
	free(NAindices);
	free(Kt);
	free(at);
	free(Pt);

#ifdef DEBUGME
	Rprintf("\n---------- Recursion Complete ----------\n");
#endif
}
/*********************************************************************************/
/* ---------- ------------------ End Kalman Filter ------------------ ---------- */
/*********************************************************************************/

SEXP fkf_SP(SEXP a0, SEXP P0, SEXP dt, SEXP ct, SEXP Tt,
			SEXP Zt, SEXP HHt, SEXP GGt, SEXP yt)
{

	// Dimensions required for the called cfkf_SP function:
	int m = length(a0);
	int d = INTEGER(GET_DIM(yt))[0];
	int n = INTEGER(GET_DIM(yt))[1];

	SEXP loglik;
	PROTECT(loglik = NEW_NUMERIC(1));

	cfkf_SP(m, d, n,
			NUMERIC_POINTER(a0), NUMERIC_POINTER(P0),
			NUMERIC_POINTER(dt), INTEGER(GET_DIM(dt))[1] == n,
			NUMERIC_POINTER(ct), INTEGER(GET_DIM(ct))[1] == n,
			NUMERIC_POINTER(Tt), INTEGER(GET_DIM(Tt))[2] == n,
			NUMERIC_POINTER(Zt), INTEGER(GET_DIM(Zt))[2] == n,
			NUMERIC_POINTER(HHt), INTEGER(GET_DIM(HHt))[2] == n,
			NUMERIC_POINTER(GGt), INTEGER(GET_DIM(GGt))[1] == n,
			NUMERIC_POINTER(yt),
			NUMERIC_POINTER(loglik));

	UNPROTECT(1);
	return (loglik);
}
