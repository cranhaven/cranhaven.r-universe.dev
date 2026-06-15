#include "utils.h"

/************************************************************************************/
/* ---------- --------- Kalman Smoothing Through Sequential Processing --------- ---*/
/************************************************************************************/
// This function performs Kalman smoothing. It iterates backwards through t.
void cfks_SP(/* Inputs */
             int m, int d, int n,
             double *Zt, int incZt,
             double *yt,
             double *vt,
             double *Tt, int incTt,
             double *Kt,
             double *Ftinv,
             double *att,
             double *Ptt)
/* No outputs? Returns ahatt in att and Vt in Ptt */
{

#ifdef DEBUGME
    Rprintf("\n---------- Recursion Start ----------\n");
#endif

    // N is an (m x m x n)
    // r is an m x m:
    // K_t is an m x d x n.
    // So, K_t,i is an m x 1.
    // Z_(t,i) is a 1 x m.
    // K_t is an m x n.
    // Ftinv is a d x n.

    int m_x_m = m * m;
    int m_x_d = m * d;

    /* integers and double precisions used in dcopy and dgemm */
    int intone = 1;
    double dblone = 1.0, dblminusone = -1.0, dblzero = 0.0;
    char *transpose = "T", *dont_transpose = "N";

    /* temporary arrays */
    double *tmpmxm = (double *)R_Calloc(m_x_m, double);
    double *tmpPt = (double *)R_Calloc(m_x_m, double);
    double *tmpN = (double *)R_Calloc(m_x_m, double);
    double *tmpr = (double *)R_Calloc(m, double);

    /* NA detection */
    int NAsum;
    int *NAindices = malloc(sizeof(int) * d);
    int *positions = malloc(sizeof(int) * d);

    /* create reduced arrays for SP and when NULL's are present */
    double *Zt_t = malloc(sizeof(double) * (d * m));
    double *Zt_temp = malloc(sizeof(double) * m);
    double *Zt_NA = malloc(sizeof(double) * (d - 1) * m);

    double tmp_scalar;

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
    // print_array(identity_matrix, m, m, "Identity:");

    // Kalman smoothing iterates backwards:
    int t = n - 1;

    // Rprintf("Initial n: %i\n", n);
    // Rprintf("Initial t: %i\n", t);
    // Rprintf("Initial m: %i\n", m);
    // Rprintf("Initial d: %i\n", d);

    /* ---------- Begin iterations --------------*/
    while (t > -1)
    {
        // Rprintf("t: %i\n", t);

        /* ahat_t = P_t %*% r_t-1 + a_t */
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &intone, &m,
         &dblone,
         &Ptt[m_x_m * t], &m,
         r, &m,
         &dblone, &att[m * t], &m FCONE FCONE);

        /* V_t = P_t - P_t %*% N_t-1 %*% P_t */
        // Step 1: tmpmxm = P_t %*% N_t-1:
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &m, &m,
         &dblone,
         &Ptt[m_x_m * t], &m,
         N, &m,
         &dblzero, tmpmxm, &m FCONE FCONE);

        /* Pt[,,i] = Pt[,,i] - tmpmxm%*% Pt[,,i] */
        F77_NAME(dcopy)
        (&m_x_m, &Ptt[m_x_m * t], &intone, tmpPt, &intone);
        F77_NAME(dgemm)
        (dont_transpose, dont_transpose,
         &m, &m, &m,
         &dblminusone,
         tmpmxm, &m,
         tmpPt, &m,
         &dblone, &Ptt[m_x_m * t], &m FCONE FCONE);

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
        /************************/
        /* check for NA's in observation yt[,t] */
        /************************/
        NAsum = numberofNA(&yt[d * t], NAindices, positions, d);

        /*********************************************************************************/
        /* ---------- ---------- ---------- smoothing step ---------- ---------- -------- */
        /*********************************************************************************/
        // Case 1: No NA's:
        if (NAsum == 0)
        {
            // Create Zt for time t
            F77_NAME(dcopy)
            (&m_x_d, &Zt[m_x_d * t * incZt], &intone, Zt_t, &intone);

            // Sequential Processing - Univariate Treatment of the Multivariate Series:
            for (int SP = d - 1; SP > -1; SP--)
            {
                // Rprintf("SP: %f\n", SP);

                // Get the specific values of Z for SP:
                for (int j = 0; j < m; j++)
                {
                    Zt_temp[j] = Zt_t[SP + j * d];
                }

                /* L_(t,i) = I_m - K_(t,i) %*% Z_(t,i) %*% F_(t,i)^-1 */

                // Step 1: L = I_m
                F77_NAME(dcopy)
                (&m_x_m, identity_matrix, &intone, L, &intone);

                // Step 2: L_(t,i) = - K_(t,i) %*% Z_(t,i) + L_(t,i):
                F77_NAME(dgemm)
                (dont_transpose, transpose,
                 &m, &m, &intone,
                 &dblminusone,
                 &Kt[m_x_d * t + (m * SP)], &m,
                 Zt_temp, &m,
                 &dblone, L, &m FCONE FCONE);
                // 	print_array(L, m, m, "L_t,i:");

                /* N_t,i-1 = t(Z_t,i) %*% F^-1 %*% Z_t,i + t(L) %*% N_t,i %*% L */
                tmp_scalar = Ftinv[(d * t) + SP];
                // Step 1: tmpmxm = t(Z_t) %*% F^-1 %*% Z_t
                F77_NAME(dgemm)
                (dont_transpose, transpose,
                 &m, &m, &intone,
                 &tmp_scalar,
                 Zt_temp, &m,
                 Zt_temp, &m,
                 &dblzero, tmpmxm, &m FCONE FCONE);
                // print_array(Zt_temp, m, 1, "Zt:");
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
                tmp_scalar *= vt[(d * t) + SP];
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
                (&m, &tmp_scalar, Zt_temp, &intone, r, &intone);
            }
        }
        /*******************************************/
        /* ---------- case 2: some NA's ---------- */
        /*******************************************/
        else
        {
            int d_reduced = d - NAsum;

            // Temporary, reduced arrays:
            reduce_array(&Zt[m_x_d * t * incZt], d, m, Zt_NA, positions, d_reduced);

            // #ifdef DEBUGME
            // print_int_array(positions, 1, d_reduced, "positions");
            // print_array(Zt_NA, d_reduced, m, "Zt_NA");
            // #endif

            // Sequential Processing - Univariate Treatment of the Multivariate Series:
            for (int SP = d_reduced - 1; SP > -1; SP--)
            {

                // Get the specific values of Z for SP:
                for (int j = 0; j < m; j++)
                {
                    Zt_temp[j] = Zt_NA[SP + j * d_reduced];
                }

                /* L_(t,i) = I_m - K_(t,i) %*% Z_(t,i) %*% F_(t,i)^-1 */

                // Step 1: L = I_m
                F77_NAME(dcopy)
                (&m_x_m, identity_matrix, &intone, L, &intone);

                // Step 2: L_(t,i) = - K_(t,i) %*% Z_(t,i) + L_(t,i):
                F77_NAME(dgemm)
                (dont_transpose, transpose,
                 &m, &m, &intone,
                 &dblminusone,
                 &Kt[m_x_d * t + (m * SP)], &m,
                 Zt_temp, &m,
                 &dblone, L, &m FCONE FCONE);
                // print_array(L, m, intone, "L_t,i:");

                /* N_t,i-1 = t(Z_t) %*% F^-1 %*% Z_t,i + t(L) %*% N_t,i %*% L */
                tmp_scalar = Ftinv[(d * t) + SP];
                // Step 1: tmpmxm = t(Z_t) %*% F^-1 %*% Z_t
                F77_NAME(dgemm)
                (dont_transpose, transpose,
                 &m, &m, &intone,
                 &tmp_scalar,
                 Zt_temp, &m,
                 Zt_temp, &m,
                 &dblzero, tmpmxm, &m FCONE FCONE);
                // Step 2: tmpN = t(L) %*% N_t,i
                F77_NAME(dgemm)
                (transpose, dont_transpose,
                 &m, &m, &m,
                 &dblone,
                 L, &m,
                 N, &m,
                 &dblzero, tmpN, &m FCONE FCONE);
                // Step 3: N = tmpN %*% L
                F77_NAME(dgemm)
                (dont_transpose, dont_transpose,
                 &m, &m, &m,
                 &dblone,
                 tmpN, &m,
                 L, &m,
                 &dblzero, N, &m FCONE FCONE);
                // Step 4: N = N + tmpmxm
                F77_NAME(daxpy)
                (&m_x_m, &dblone, tmpmxm, &intone, N, &intone);

                /* r_t,i-1 = t(Z_t,i) %*% f_t,i^-1 %*% v_t,i + t(L_t,i) %*% r_t,i */

                // Step 1: f_t,i^-1 * v_t,i (scalar * scalar)
                tmp_scalar *= vt[(d * t) + SP];
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
                (&m, &tmp_scalar, Zt_temp, &intone, r, &intone);
            }
        }

        // The values of r_t,0 and N_t,0 are identical to r_t-1 and N_t-1, respectively:

        // print_array(N, m, m, "N at end:");
        // print_array(r, m, intone, "r at end:");

        // Iterate backwards through time:
        t--;
    }

    // Memory clean - R_Free vectors / matrices:
    R_Free(tmpmxm);
    R_Free(tmpPt);
    R_Free(tmpN);
    R_Free(tmpr);
    R_Free(N);
    R_Free(r);
    R_Free(L);
    R_Free(identity_matrix);

    free(NAindices);
    free(positions);
    free(Zt_temp);
    free(Zt_t);
    free(Zt_NA);

    // Rprintf("\n---------- Function End ----------\n");
}

/*********************************************************************************/
/* ---------- --------------- Convert between R and C  -------------- ---------- */
/*********************************************************************************/
SEXP fks_SP(SEXP Tt, SEXP Zt, SEXP yt, SEXP vt, SEXP Kt, SEXP Ftinv, SEXP att_input, SEXP Ptt_input)
{

    // Dimensions required for the called cfks_SP function:
    int m = INTEGER(GET_DIM(Tt))[0];
    int d = INTEGER(GET_DIM(vt))[0];
    int n = INTEGER(GET_DIM(vt))[1];

    // Symbolic expression - R data types. Essentially :
    SEXP ans, ans_names, class_name;
    SEXP dim_att, dim_Ptt;

    // Copy att and Ptt - tao ensure the input values don't change:
    SEXP att, Ptt;
    int m_x_n = m * n;
    int m_x_m_x_n = m * m * n;
    int intone = 1;
    PROTECT(att = NEW_NUMERIC(m_x_n));
    PROTECT(Ptt = NEW_NUMERIC(m_x_m_x_n));
    // Set dimensions
    F77_NAME(dcopy)
    (&m_x_n, NUMERIC_POINTER(att_input), &intone, NUMERIC_POINTER(att), &intone);
    F77_NAME(dcopy)
    (&m_x_m_x_n, NUMERIC_POINTER(Ptt_input), &intone, NUMERIC_POINTER(Ptt), &intone);

    cfks_SP(m, d, n,
            // Input
            NUMERIC_POINTER(Zt), INTEGER(GET_DIM(Zt))[2] == n,
            NUMERIC_POINTER(yt),
            NUMERIC_POINTER(vt),
            NUMERIC_POINTER(Tt), INTEGER(GET_DIM(Tt))[2] == n,
            NUMERIC_POINTER(Kt),
            NUMERIC_POINTER(Ftinv),
            NUMERIC_POINTER(att),
            NUMERIC_POINTER(Ptt));

    /* Produce named return list */
    PROTECT(ans = NEW_LIST(2));
    PROTECT(ans_names = NEW_CHARACTER(2));
    SET_STRING_ELT(ans_names, 0, mkChar("ahatt"));
    SET_STRING_ELT(ans_names, 1, mkChar("Vt"));

    setAttrib(ans, R_NamesSymbol, ans_names);

    /* Coerce vectors to matrices and arrays */

    /* Set matrix dimensions */
    PROTECT(dim_att = NEW_INTEGER(2));

    INTEGER(dim_att)
    [0] = m;
    INTEGER(dim_att)
    [1] = n;

    setAttrib(att, R_DimSymbol, dim_att);

    /* Set array dimensions */
    PROTECT(dim_Ptt = NEW_INTEGER(3));

    INTEGER(dim_Ptt)
    [0] = m;
    INTEGER(dim_Ptt)
    [1] = m;
    INTEGER(dim_Ptt)
    [2] = n;

    setAttrib(Ptt, R_DimSymbol, dim_Ptt);

    /* Fill the list */
    SET_VECTOR_ELT(ans, 0, att);
    SET_VECTOR_ELT(ans, 1, Ptt);

    /* Set the class to 'fkf' */
    PROTECT(class_name = NEW_CHARACTER(1));
    SET_STRING_ELT(class_name, 0, mkChar("fks.SP"));
    classgets(ans, class_name);

    UNPROTECT(7);
    return (ans);
}
