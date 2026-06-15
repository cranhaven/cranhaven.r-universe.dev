#include "utils.h"

/************************************************************************************/
/* ---------- ---------- Kalman filter and smoother: Sequential Processing ---------- ---*/
/************************************************************************************/
// This function combines the cfkf_SP_verbose and cfks_SP functions into a singular function call
// This allows you to perform Kalman filtering and smoothing all in the one call.
void cfkfs_SP(
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
    double *loglik,
    double *at_output, double *att_output,
    double *pt_output, double *ptt_output,
    double *Ftinv_output,
    double *vt_output,
    double *Kt_output,
    double *att_smooth,
    double *Ptt_smooth)
{
    /*  Notation: */

    // m: the dimension of the state vector
    // d: the dimension of observations
    // n: the total number of observations

    /*  loglik is the output of the function and denotes the log-likelihood */
    /*  cfkf uses 'dcopy', 'dgemm' and 'daxpy' from BLAS */

    // Copy the values of att and Ptt for smoothing:
    int m_x_n = m * n;
    int m_x_m_x_n = m * m * n;
    int m_x_m = m * m;
    int m_x_d = m * d;
    int NAsum;
    int t = 0;

    // integers and double precisions used in dcopy and dgemm
    int intone = 1;
    double dblone = 1.0, dblminusone = -1.0, dblzero = 0.0;
    // To transpose or not transpose matrix
    char *transpose = "T", *dont_transpose = "N";

    // A temporary scalar value is needed through both filtering and smoothing:
    // Kalman filter - Ft
    // Kalman smoother - Ftinv, vt,i
    double tmp_scalar;

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

    // Save Z_SP for all observations:
    double *Z_SP = malloc(sizeof(double) * (m * d * n));
    // Save the total number of observations at every t:
    int *d_t = malloc(sizeof(int) * n);

    // SEQUENTIAL PROCESSING DEFINED VARIABLES:

#ifdef DEBUGME
    int SP_int = 3;
    int i_int = 0;
#endif

    int N_obs = 0;
    // Doubles for the SP iteration:

    double *at = malloc(sizeof(double) * m);
    double *Pt = malloc(sizeof(double) * m_x_m);
    // double *Kt = malloc(sizeof(double) * m);

    // SP temporary arrays:
    double *tmpmxSP = (double *)R_Calloc(m, double);
    double *tmpmxm = (double *)R_Calloc(m_x_m, double);

    *loglik = 0;

    /* temporary arrays */
    double *tmpN = (double *)R_Calloc(m_x_m, double);
    double *tmpr = (double *)R_Calloc(m, double);

    /* recursion parameters */
    double *N = (double *)R_Calloc(m_x_m, double);
    double *r = (double *)R_Calloc(m, double);
    double *L = (double *)R_Calloc(m_x_m, double);

    // Develop Identity matrix (for L):
    double *identity_matrix = (double *)R_Calloc(m_x_m, double);
    for (int i = 0; i < m; i++)
    {
        identity_matrix[i * m + i] = 1.0;
    }

    // intone = 1;

    /* at = a0 */
    F77_NAME(dcopy)
    (&m, a0, &intone, at, &intone);

    /* Pt = P0 */
    F77_NAME(dcopy)
    (&m_x_m, P0, &intone, Pt, &intone);

    // att
    //  F77_NAME(dcopy)(&m, at, &intone, &at_output[m * t + m], &intone);
    //  F77_NAME(dcopy)(&m, Pt, &intone, &pt_output[m * t + m], &intone);

    /*Initial State Outputs:*/
    F77_NAME(dcopy)
    (&m, at, &intone, &at_output[0], &intone);
    F77_NAME(dcopy)
    (&m_x_m, Pt, &intone, &pt_output[0], &intone);

    for (int x = 0; x < n; x++)
    {
        d_t[x] = d;
    }

    /*********************************************************************************/
    /* ---------- ----------------- Begin Kalman Filter ----------------- ---------- */
    /*********************************************************************************/

    // Recursion across all time steps:
    while (t < n)
    {

        // How many NA's at time t?
        NAsum = numberofNA(&yt[d * t], NAindices, positions, d);

        d_t[t] -= NAsum;

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
                    // Zt_tSP[j] = Zt_t[SP + j*d];
                    Z_SP[(m * d * t) + (m * SP) + j] = Zt_t[SP + j * d];
                }

                // Step 1 - Measurement Error:
                // Compute Vt[SP,t] = yt[SP,t] - ct[SP,t * incct] + Zt[SP,,t * incZt] %*% at[SP,t]
                vt_output[SP + d * t] = yt[SP + d * t] - ct[SP + d * t * incct];

#ifdef DEBUGME
                if (SP == SP_int)
                {
                    if (t == i_int)
                    {
                        print_array(Zt_tSP, 1, m, "Zt[SP,]");
                        // Rprintf("\n V - Pre Mat Mult: %f", V);
                    }
                }
#endif

                // vt[SP,t] = vt[SP,t] - Zt[SP,, t * incZt] %*% at[,t]
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose,
                 &intone, &intone, &m,
                 &dblminusone,
                 &Z_SP[(m * d * t) + (m * SP)], &intone,
                 at, &m,
                 &dblone, &vt_output[SP + d * t], &intone FCONE FCONE);

                // #ifdef DEBUGME
                // if(SP == SP_int)
                // {
                // 	if(t == i_int){
                // 	Rprintf("\n V - Post Mat Mult: %f", V);
                // 	}
                // }
                // #endif

                // Step 2 - Function of Covariance Matrix:
                // Compute Ft = Zt[SP,,t * incZt] %*% Pt %*% t(Zt[SP,,t * incZt]) + diag(GGt)[SP]

                // First, Let us calculate:
                // Pt %*% t(Zt[SP,,t * incZt])
                // because we use this result twice

                F77_NAME(dgemm)
                (dont_transpose, transpose,
                 &m, &intone, &m,
                 &dblone,
                 Pt, &m,
                 &Z_SP[(m * d * t) + (m * SP)], &intone,
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
                tmp_scalar = GGt[SP + (d * t * incGGt)];

#ifdef DEBUGME
                if (SP == SP_int)
                {
                    if (t == i_int)
                    {
                        Rprintf("\n Ft: %f \n", tmp_scalar);
                    }
                }
#endif

                // Ft = Zt[SP,,t*incZt] %*% tmpmxSP + Ft
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose, &intone,
                 &intone, &m, &dblone,
                 &Z_SP[(m * d * t) + (m * SP)], &intone,
                 tmpmxSP, &m,
                 &dblone, &tmp_scalar, &intone FCONE FCONE);

#ifdef DEBUGME
                if (SP == SP_int)
                {
                    if (t == i_int)
                    {
                        Rprintf("\n New Ft: %f \n", tmp_scalar);
                    }
                }
#endif

                // Step 3 - Calculate the Kalman Gain:
                // Compute Kt = Pt %*% t(Zt[SP,,i * incZt]) %*% (1/Ft)

                // Inv Ft:
                //  tmpFtinv =
                Ftinv_output[SP + d * t] = 1 / tmp_scalar;

#ifdef DEBUGME
                if (SP == SP_int)
                {
                    if (t == i_int)
                    {
                        Rprintf("\n Inverse Ft: %f \n", Ftinv_output[SP + d * t]);
                    }
                }
#endif

                // Kt is an m x 1 matrix

                // We already have tmpSPxm:
                // Kt = tmpmxSP %*% tmpFtinv
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose,
                 &m, &intone, &intone,
                 &dblone,
                 tmpmxSP, &m,
                 &Ftinv_output[SP + d * t], &intone,
                 &dblzero, &Kt_output[m_x_d * t + (m * SP)], &m FCONE FCONE);

                // print_array(&Kt_output[m_x_d * t + (m * SP)], m, 1, "Kalman Gain");

#ifdef DEBUGME
                if (SP == SP_int)
                {
                    if (t == i_int)
                    {
                        print_array(&Kt_output[m * t + (m * SP)], m, 1, "Kalman Gain");
                    }
                }
#endif

                // Step 4 - Correct State Vector mean and Covariance:

                // Correction to att based upon prediction error:
                // att = Kt %*% V + att
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose,
                 &m, &intone, &intone,
                 &dblone,
                 &Kt_output[m_x_d * t + (m * SP)], &m,
                 &vt_output[SP + d * t], &intone,
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
                 &dblminusone, tmpmxSP, &m,
                 &Kt_output[m_x_d * t + (m * SP)], &m,
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
                *loglik -= 0.5 * (log(tmp_scalar) + (vt_output[SP + d * t] * vt_output[SP + d * t] * Ftinv_output[SP + d * t]));

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
            int d_reduced = d_t[t];
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
                    Z_SP[(m * d * t) + (m * SP) + j] = Zt_temp[SP + j * d_reduced];
                }

#ifdef DEBUGME
                print_array(Zt_tSP, 1, m, "Zt_tSP:");
#endif

                // Step 1 - Measurement Error:
                // Compute Vt[SP,t] = yt[SP,t] - ct[SP,t * incct] - Zt[SP,,t * incZt] %*% at[SP,t]

                // vt[SP,t] = yt[SP,t] - ct[SP,t * incct]
                //  V = yt_temp[SP] - ct_temp[SP];
                vt_output[SP + d * t] = yt_temp[SP] - ct_temp[SP];

                // #ifdef DEBUGME
                // Rprintf("\n Pre mat-mult V = %f", V);
                // #endif
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose,
                 &intone, &intone, &m,
                 &dblminusone,
                 &Z_SP[(m * d * t) + (m * SP)], &intone,
                 at, &m,
                 &dblone, &vt_output[SP + d * t], &intone FCONE FCONE);

                // #ifdef DEBUGME
                // Rprintf("\n Post mat-mult V = %f", V);
                // #endif

                // Step 2 - Function of Covariance Matrix:
                // Compute Ft = Zt[SP,,t * incZt] %*% Pt %*% t(Zt[SP,,t * incZt]) + diag(GGt)[SP]

                // First, Let us calculate:
                // Pt %*% t(Zt[SP,,t * incZt])
                // because we use this result twice

                F77_NAME(dgemm)
                (dont_transpose, transpose,
                 &m, &intone, &m,
                 &dblone,
                 Pt, &m,
                 &Z_SP[(m * d * t) + (m * SP)], &intone,
                 &dblzero, tmpmxSP, &m FCONE FCONE);

#ifdef DEBUGME
                print_array(tmpmxSP, m, 1, "tmpmxSP:");
#endif

                // Ft = GGt[SP]
                tmp_scalar = GGt_temp[SP];

#ifdef DEBUGME
                Rprintf("\n Ft: %f \n", tmp_scalar);
#endif

                // Ft = Zt[SP,,i*incZt] %*% tmpmxSP + Ft
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose, &intone,
                 &intone, &m, &dblone,
                 &Z_SP[(m * d * t) + (m * SP)], &intone,
                 tmpmxSP, &m,
                 &dblone, &tmp_scalar, &intone FCONE FCONE);

                // Step 3 - Calculate the Kalman Gain:
                // Compute Kt = Pt %*% t(Zt[SP,,i * incZt]) %*% (1/Ft)

#ifdef DEBUGME
                Rprintf("\n New Ft: %f \n", tmp_scalar);
#endif

                // Inv Ft:
                Ftinv_output[SP + d * t] = 1 / tmp_scalar;

#ifdef DEBUGME
                Rprintf("\n Inverse Ft: %f \n", Ftinv_output[SP + d * t]);
#endif

                // Kt is an m x 1 matrix

                // We already have tmpSPxm:
                // Kt = tmpmxSP %*% tmpFtinv
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose,
                 &m, &intone, &intone,
                 &dblone,
                 tmpmxSP, &m,
                 &Ftinv_output[SP + d * t], &intone,
                 &dblzero, &Kt_output[m_x_d * t + (m * SP)], &m FCONE FCONE);

                // Step 4 - Correct State Vector mean and Covariance:

                // Correction to att based upon prediction error:
                // att = Kt %*% V + att
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose, &m,
                 &intone, &intone, &dblone,
                 &Kt_output[m_x_d * t + (m * SP)], &m,
                 &vt_output[SP + d * t], &intone,
                 &dblone, at, &m FCONE FCONE);

                // Correction to covariance based upon Kalman Gain:
                // ptt = ptt - ptt %*% t(Z[SP,,i * incZt]) %*% t(Ktt)
                // ptt = ptt - tempmxSP %*% t(Ktt)
                F77_NAME(dgemm)
                (dont_transpose, transpose, &m,
                 &m, &intone, &dblminusone,
                 tmpmxSP, &m,
                 &Kt_output[m_x_d * t + (m * SP)], &m,
                 &dblone, Pt, &m FCONE FCONE);

                // Step 5 - Update Log-Likelihood Score:
                *loglik -= 0.5 * (log(tmp_scalar) + vt_output[SP + d * t] * vt_output[SP + d * t] * Ftinv_output[SP + d * t]);
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

        // att
        F77_NAME(dcopy)
        (&m, at, &intone, &att_output[m * t], &intone);
        F77_NAME(dcopy)
        (&m_x_m, Pt, &intone, &ptt_output[m_x_m * t], &intone);

        /* at[,t + 1] = dt[,t] + at[,t] */
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

        // att
        F77_NAME(dcopy)
        (&m, at, &intone, &at_output[m * t + m], &intone);
        F77_NAME(dcopy)
        (&m_x_m, Pt, &intone, &pt_output[m_x_m * t + m_x_m], &intone);

        // end iteration
        t++;
    }
    // Update the final Log-Likelihood Score:
    *loglik -= 0.5 * N_obs * log(2 * M_PI);

#ifdef DEBUGME
    Rprintf("\n---------- Kalman filter Complete ----------\n");
#endif

#ifdef DEBUGME
    Rprintf("\n Log-Likelihood: %f \n", *loglik);
#endif

    /**************************************************************/
    /* ---------- ---------- end Kalman filter ------- ---------- */
    /**************************************************************/

    /**************************************************************/
    /* ---------- ---------- begin Kalman smoother ------- -------- */
    /**************************************************************/

    // Set dimensions
    F77_NAME(dcopy)
    (&m_x_n, att_output, &intone, att_smooth, &intone);
    F77_NAME(dcopy)
    (&m_x_m_x_n, ptt_output, &intone, Ptt_smooth, &intone);

#ifdef DEBUGME
    Rprintf("\n---------- Kalman smoother begin ----------\n");
#endif

    // Kalman smoothing iterates backwards:
    t = n - 1;

    /* ---------- Begin iterations --------------*/
    while (t > -1)
    {
        // Rprintf("t: %i\n", t);

        /* ahat_t = P_t %*% r_t-1 + a_t */
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &intone, &m,
         &dblone,
         &Ptt_smooth[m_x_m * t], &m,
         r, &m,
         &dblone, &att_smooth[m * t], &m FCONE FCONE);

        /* V_t = P_t - P_t %*% N_t-1 %*% P_t */
        // Step 1: tmpmxm = P_t %*% N_t-1:
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &m, &m,
         &dblone,
         &Ptt_smooth[m_x_m * t], &m,
         N, &m,
         &dblzero, tmpmxm, &m FCONE FCONE);

        /* Pt[,,i] = Pt[,,i] - tmpmxm%*% Pt[,,i] */
        F77_NAME(dcopy)
        (&m_x_m, &Ptt_smooth[m_x_m * t], &intone, Pt, &intone);
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &m, &m,
         &dblminusone,
         tmpmxm, &m,
         Pt, &m,
         &dblone, &Ptt_smooth[m_x_m * t], &m FCONE FCONE);

        // Move from r_t,0 to r_(t-1),pt:
        // r_(t-1),p_t = t(T_t-1) %*% r_t,0:
        F77_NAME(dcopy)
        (&m, r, &intone, tmpr, &intone);
        F77_NAME(dgemm)
        (transpose, dont_transpose,
         &m, &intone, &m,
         &dblone,
         &Tt[m_x_m * t * incTt], &m,
         tmpr, &m,
         &dblzero, r, &m FCONE FCONE);

        // N_(t-1,p_t )= t(T_t-1) N_(t,0) T_(t-1)

        // Step 1 - tmpmxm = t(T_t-1) %*% N
        F77_NAME(dgemm)
        (transpose, dont_transpose,
         &m, &m, &m,
         &dblone,
         &Tt[m_x_m * t * incTt], &m,
         N, &m,
         &dblzero, tmpmxm, &m FCONE FCONE);

        // Step 2 - N = tmpmxm %*% T_(t-1)
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &m, &m,
         &dblone,
         tmpmxm, &m,
         &Tt[m_x_m * t * incTt], &m,
         &dblzero, N, &m FCONE FCONE);
        // print_array(N, m, m, "N at start:");
        // print_array(r, m, intone, "r at start:");

        /*********************************************************************************/
        /* ---------- ---------- ---------- smoothing step ---------- ---------- -------- */
        /*********************************************************************************/
        // Sequential Processing - Univariate Treatment of the Multivariate Series:
        //  Missing values / NA's have already been accounted for in the respective matrices when detecting NA's in the Kalman filter.
        for (int SP = d_t[t] - 1; SP > -1; SP--)
        {
            // Rprintf("SP: %i\n", SP);

            // Get the specific values of Z for SP:
            F77_NAME(dcopy)
            (&m, &Z_SP[m_x_d * t + (m * SP)], &intone, Zt_tSP, &intone);

            /* L_(t,i) = I_m - K_(t,i) %*% Z_(t,i) %*% F_(t,i)^-1 */

            // Step 1: L = I_m
            F77_NAME(dcopy)
            (&m_x_m, identity_matrix, &intone, L, &intone);

            // Step 2: L_(t,i) = - K_(t,i) %*% Z_(t,i) + L_(t,i):
            F77_NAME(dgemm)
            (dont_transpose, transpose,
             &m, &m, &intone,
             &dblminusone,
             &Kt_output[m_x_d * t + (m * SP)], &m,
             Zt_tSP, &m,
             &dblone, L, &m FCONE FCONE);
            // 	print_array(L, m, m, "L_t,i:");

            /* N_t,i-1 = t(Z_t,i) %*% F^-1 %*% Z_t,i + t(L) %*% N_t,i %*% L */
            tmp_scalar = Ftinv_output[(d * t) + SP];
            // Step 1: tmpmxm = t(Z_t) %*% F^-1 %*% Z_t
            F77_NAME(dgemm)
            (dont_transpose, transpose,
             &m, &m, &intone,
             &tmp_scalar,
             Zt_tSP, &m,
             Zt_tSP, &m,
             &dblzero, tmpmxm, &m FCONE FCONE);
            // print_array(Zt_tSP, m, 1, "Zt:");
            // print_array(tmpmxm, m, m, "t(Zt) * Ft^-1 * Zt:");

            // Step 2: tmpN = t(L) %*% N_t,i
            F77_NAME(dgemm)
            (transpose, dont_transpose,
             &m, &m, &m,
             &dblone,
             L, &m,
             N, &m,
             &dblzero, tmpN, &m FCONE FCONE);
            // print_array(tmpN, m, m, "t(L) * N:");

            // Step 3: N = tmpN %*% L
            F77_NAME(dgemm)
            (dont_transpose, dont_transpose,
             &m, &m, &m,
             &dblone,
             tmpN, &m,
             L, &m,
             &dblzero, N, &m FCONE FCONE);
            // print_array(N, m, m, "t(L) * N * L:");
            // print_array(tmpmxm, m, m, "t(L) * N * L:");

            // Step 4: N = N + tmpmxm
            F77_NAME(daxpy)
            (&m_x_m, &dblone, tmpmxm, &intone, N, &intone);

            /* r_t,i-1 = t(Z_t,i) %*% f_t,i^-1 %*% v_t,i + t(L_t,i) %*% r_t,i */

            // Step 1: f_t,i^-1 * v_t,i (scalar * scalar)
            tmp_scalar *= vt_output[(d * t) + SP];
            // Step 2: r = t(L_t,i) %*% r_t,i
            F77_NAME(dcopy)
            (&m, r, &intone, tmpr, &intone);
            F77_NAME(dgemm)
            (transpose, dont_transpose,
             &m, &intone, &m,
             &dblone,
             L, &m,
             tmpr, &m,
             &dblzero, r, &m FCONE FCONE);

            // Step 3: r_t,i-1 = Zt_tmp + r:
            F77_NAME(daxpy)
            (&m, &tmp_scalar, Zt_tSP, &intone, r, &intone);
        }
        // The values of r_t,0 and N_t,0 are identical to r_t-1 and N_t-1, respectively:

        // print_array(att_smooth, m, n, "att_smoothed end:");

        // print_array(N, m, m, "N at end:");
        // print_array(r, m, intone, "r at end:");

        // Iterate backwards through time:
        t--;
    }

    // print_array(att_smooth, m, n, "att_smoothed end:");

    // Memory clean - R_Free vectors / matrices:
    free(NAindices);
    free(positions);
    free(yt_temp);
    free(ct_temp);
    free(Zt_temp);
    free(GGt_temp);
    free(Zt_t);
    free(Zt_tSP);
    free(Z_SP);
    free(d_t);
    free(at);
    free(Pt);

    R_Free(tmpmxSP);
    R_Free(tmpmxm);
    R_Free(tmpN);
    R_Free(tmpr);
    R_Free(N);
    R_Free(r);
    R_Free(L);
    R_Free(identity_matrix);
}
/*********************************************************************************/
/* ---------- ------------ End Kalman Filter and smoother ------------ --------- */
/*********************************************************************************/

/*********************************************************************************/
/* ---------- --------------- Convert between R and C  -------------- ---------- */
/*********************************************************************************/
SEXP fkfs_SP(SEXP a0, SEXP P0, SEXP dt, SEXP ct, SEXP Tt,
             SEXP Zt, SEXP HHt, SEXP GGt, SEXP yt)
{

    // Dimensions required for the called cfkfs_SP function:
    int m = length(a0);
    int d = INTEGER(GET_DIM(yt))[0];
    int n = INTEGER(GET_DIM(yt))[1];

    int m_x_n = m * n;
    int m_x_m_x_n = m * m * n;
    int m_x_d_x_n = m * d * n;
    int d_x_n = d * n;
    double dbl_NA = NA_REAL;
    int intzero = 0, intone = 1;

    // Symbolic expression - R data types, essentially:
    SEXP ans, ans_names, class_name;
    // SEXP the outputs:
    SEXP loglik, at_output, att_output, pt_output, ptt_output, Ftinv_output, vt_output, Kt_output, att_smoothed_output, Ptt_smoothed_output;
    SEXP dim_at, dim_att, dim_Pt, dim_Ptt, dim_Kt, dim_Ftinv, dim_ahatt, dim_Vt;

    /* Allocate memory for objects to be returned and set values to NA. */
    PROTECT(loglik = NEW_NUMERIC(1));
    PROTECT(att_output = NEW_NUMERIC(m * n));
    PROTECT(at_output = NEW_NUMERIC(m * (n + 1)));
    PROTECT(ptt_output = NEW_NUMERIC(m * m * n));
    PROTECT(pt_output = NEW_NUMERIC(m * m * (n + 1)));
    PROTECT(Ftinv_output = NEW_NUMERIC(d * n));
    PROTECT(vt_output = NEW_NUMERIC(d * n));
    PROTECT(Kt_output = NEW_NUMERIC(m * d * n));
    PROTECT(att_smoothed_output = NEW_NUMERIC(m * n));
    PROTECT(Ptt_smoothed_output = NEW_NUMERIC(m * m * n));

    // Set dimensions
    F77_NAME(dcopy)
    (&m_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(att_output), &intone);
    F77_NAME(dcopy)
    (&m_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(at_output), &intone);
    F77_NAME(dcopy)
    (&m_x_d_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(Kt_output), &intone);
    F77_NAME(dcopy)
    (&m_x_m_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(ptt_output), &intone);
    F77_NAME(dcopy)
    (&m_x_m_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(pt_output), &intone);
    F77_NAME(dcopy)
    (&d_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(vt_output), &intone);
    F77_NAME(dcopy)
    (&d_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(Ftinv_output), &intone);

    F77_NAME(dcopy)
    (&m_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(att_smoothed_output), &intone);
    F77_NAME(dcopy)
    (&m_x_m_x_n, &dbl_NA, &intzero, NUMERIC_POINTER(Ptt_smoothed_output), &intone);

    cfkfs_SP(m, d, n,
             // Input
             NUMERIC_POINTER(a0), NUMERIC_POINTER(P0),
             NUMERIC_POINTER(dt), INTEGER(GET_DIM(dt))[1] == n,
             NUMERIC_POINTER(ct), INTEGER(GET_DIM(ct))[1] == n,
             NUMERIC_POINTER(Tt), INTEGER(GET_DIM(Tt))[2] == n,
             NUMERIC_POINTER(Zt), INTEGER(GET_DIM(Zt))[2] == n,
             NUMERIC_POINTER(HHt), INTEGER(GET_DIM(HHt))[2] == n,
             NUMERIC_POINTER(GGt), INTEGER(GET_DIM(GGt))[1] == n,
             // Output
             NUMERIC_POINTER(yt),
             NUMERIC_POINTER(loglik),
             NUMERIC_POINTER(at_output), NUMERIC_POINTER(att_output),
             NUMERIC_POINTER(pt_output), NUMERIC_POINTER(ptt_output),
             NUMERIC_POINTER(Ftinv_output),
             NUMERIC_POINTER(vt_output),
             NUMERIC_POINTER(Kt_output),
             NUMERIC_POINTER(att_smoothed_output),
             NUMERIC_POINTER(Ptt_smoothed_output));

    /* Produce named return list */
    PROTECT(ans = NEW_LIST(13));
    PROTECT(ans_names = NEW_CHARACTER(13));
    SET_STRING_ELT(ans_names, 0, mkChar("att"));
    SET_STRING_ELT(ans_names, 1, mkChar("at"));
    SET_STRING_ELT(ans_names, 2, mkChar("Ptt"));
    SET_STRING_ELT(ans_names, 3, mkChar("Pt"));
    SET_STRING_ELT(ans_names, 4, mkChar("yt"));
    SET_STRING_ELT(ans_names, 5, mkChar("Tt"));
    SET_STRING_ELT(ans_names, 6, mkChar("Zt"));
    SET_STRING_ELT(ans_names, 7, mkChar("Ftinv"));
    SET_STRING_ELT(ans_names, 8, mkChar("vt"));
    SET_STRING_ELT(ans_names, 9, mkChar("Kt"));
    SET_STRING_ELT(ans_names, 10, mkChar("ahatt"));
    SET_STRING_ELT(ans_names, 11, mkChar("Vt"));
    SET_STRING_ELT(ans_names, 12, mkChar("logLik"));

    setAttrib(ans, R_NamesSymbol, ans_names);
    // Set matrix dimensions:
    PROTECT(dim_at = NEW_INTEGER(2));
    PROTECT(dim_att = NEW_INTEGER(2));
    PROTECT(dim_ahatt = NEW_INTEGER(2));

    INTEGER(dim_at)
    [0] = m;
    INTEGER(dim_at)
    [1] = n + 1;

    INTEGER(dim_att)
    [0] = m;
    INTEGER(dim_att)
    [1] = n;

    INTEGER(dim_ahatt)
    [0] = m;
    INTEGER(dim_ahatt)
    [1] = n;

    setAttrib(at_output, R_DimSymbol, dim_at);
    setAttrib(att_output, R_DimSymbol, dim_att);
    setAttrib(att_smoothed_output, R_DimSymbol, dim_ahatt);

    /* Set array dimensions */
    PROTECT(dim_Pt = NEW_INTEGER(3));
    PROTECT(dim_Ptt = NEW_INTEGER(3));
    PROTECT(dim_Vt = NEW_INTEGER(3));

    INTEGER(dim_Pt)
    [0] = m;
    INTEGER(dim_Pt)
    [1] = m;
    INTEGER(dim_Pt)
    [2] = n + 1;

    INTEGER(dim_Ptt)
    [0] = m;
    INTEGER(dim_Ptt)
    [1] = m;
    INTEGER(dim_Ptt)
    [2] = n;

    INTEGER(dim_Vt)
    [0] = m;
    INTEGER(dim_Vt)
    [1] = m;
    INTEGER(dim_Vt)
    [2] = n;

    setAttrib(pt_output, R_DimSymbol, dim_Pt);
    setAttrib(ptt_output, R_DimSymbol, dim_Ptt);
    setAttrib(Ptt_smoothed_output, R_DimSymbol, dim_Vt);

    PROTECT(dim_Kt = NEW_INTEGER(3));
    INTEGER(dim_Kt)
    [0] = m;
    INTEGER(dim_Kt)
    [1] = d;
    INTEGER(dim_Kt)
    [2] = n;
    setAttrib(Kt_output, R_DimSymbol, dim_Kt);

    /* Set array dimensions */
    PROTECT(dim_Ftinv = NEW_INTEGER(2));
    INTEGER(dim_Ftinv)
    [0] = d;
    INTEGER(dim_Ftinv)
    [1] = n;

    setAttrib(Ftinv_output, R_DimSymbol, dim_Ftinv);
    setAttrib(vt_output, R_DimSymbol, dim_Ftinv);

    /* Fill the list */
    SET_VECTOR_ELT(ans, 0, att_output);
    SET_VECTOR_ELT(ans, 1, at_output);
    SET_VECTOR_ELT(ans, 2, ptt_output);
    SET_VECTOR_ELT(ans, 3, pt_output);
    SET_VECTOR_ELT(ans, 4, yt);
    SET_VECTOR_ELT(ans, 5, Tt);
    SET_VECTOR_ELT(ans, 6, Zt);
    SET_VECTOR_ELT(ans, 7, Ftinv_output);
    SET_VECTOR_ELT(ans, 8, vt_output);
    SET_VECTOR_ELT(ans, 9, Kt_output);
    SET_VECTOR_ELT(ans, 10, att_smoothed_output);
    SET_VECTOR_ELT(ans, 11, Ptt_smoothed_output);
    SET_VECTOR_ELT(ans, 12, loglik);

    /* Set the class to 'fkf' */
    PROTECT(class_name = NEW_CHARACTER(1));
    SET_STRING_ELT(class_name, 0, mkChar("fkfs.SP"));
    classgets(ans, class_name);

    UNPROTECT(21);
    return (ans);
}
