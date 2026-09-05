#include <stdbool.h>
#include <gsl/gsl_sf.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_linalg.h>
#include "my_header.h"
#include "utils.h"

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

int *sample_size_ptr = NULL;
static char sampler_method[256];

static double ****X1 = NULL;
static double ***newYY = NULL;
static double ***newCC = NULL;

/*
 * Main training entry point called from R.
 *
 * The routine converts nested R lists into row-addressable C arrays, initializes
 * the latent responses and selection state, runs the MCMC updates, and returns
 * posterior samples/summaries in the historical R list layout.
 */
SEXP main_function(SEXP h0_R, SEXP hh_R, SEXP alpha_R, SEXP psi_R, SEXP alpha0_R, SEXP beta0_R,
                  SEXP seed_R, SEXP nu_R,
                  SEXP method1_R, SEXP n_platforms_R,
                  SEXP platform_models_R, SEXP model_platforms_R, SEXP n_subgroups_R,
                  SEXP sample_size, SEXP nbr_features, SEXP nbr_cov,
                  SEXP X1_filtered, SEXP newYY_list, SEXP type_outcome,
                  SEXP newCC_list,
                  SEXP sample, SEXP burnin)
{
    clock_t t = clock();
    /* platform_models_R maps each platform to the subgroups using it;
     * model_platforms_R is the inverse mapping from subgroup to platforms. */

    double h0 = REAL(h0_R)[0]; // scaling factor
    double h11 = REAL(hh_R)[0];
    double alpha = REAL(alpha_R)[0];   // weight of prior beliefs
    double psi = REAL(psi_R)[0];       // control var of prior distributions
    double alpha0 = REAL(alpha0_R)[0]; // prior
    double beta0 = REAL(beta0_R)[0];   // prior scaling factor

    long seed = (long)REAL(seed_R)[0];

    PROTECT(method1_R);
    strncpy(sampler_method, CHAR(STRING_ELT(method1_R, 0)), sizeof(sampler_method) - 1);
    sampler_method[sizeof(sampler_method) - 1] = '\0';

    PROTECT(n_platforms_R);
    PROTECT(platform_models_R);
    PROTECT(model_platforms_R);
    PROTECT(n_subgroups_R);
    PROTECT(sample_size);
    PROTECT(nbr_features);
    PROTECT(nbr_cov);
    PROTECT(X1_filtered);
    PROTECT(newYY_list);
    PROTECT(newCC_list);


    int K = asInteger(nbr_cov);
    int type_out = asInteger(type_outcome);
    int sample_c = asInteger(sample);
    int burnin_c = asInteger(burnin);

    int n_platforms = asInteger(n_platforms_R);
    Rprintf("We have %d platforms  in total \n", n_platforms);

    // We read model indices for each plaform
    int **platform_models_c = malloc(n_platforms * sizeof(int *));
    int *n_platform_models_c = malloc(n_platforms * sizeof(int));

    for (int i = 0; i < n_platforms; i++)
    {
        SEXP mPM = VECTOR_ELT(platform_models_R, i);
        int sizeM = LENGTH(mPM);
        n_platform_models_c[i] = sizeM;
        platform_models_c[i] = INTEGER(mPM);
    }

    Rprintf("\n");
    for (int i = 0; i < n_platforms; i++)
    {
        Rprintf("Platform %d is involved in  %d subgroups\n", i + 1, n_platform_models_c[i]);
        Rprintf("Platform %d is involved in subgroups: ", i + 1);
        for (int j = 0; j < n_platform_models_c[i]; j++)
        {
            Rprintf("%d ", 1 + platform_models_c[i][j]);
        }
        Rprintf("\n\n");
    }

    int n_subgroups = asInteger(n_subgroups_R);
    // We read platform indices for each model/subgroup
    int **model_platforms_c = malloc(n_subgroups * sizeof(int *));
    int *n_model_platforms_c = malloc(n_subgroups * sizeof(int));

    for (int i = 0; i < n_subgroups; i++)
    {
        SEXP mPM = VECTOR_ELT(model_platforms_R, i);
        int sizeM = LENGTH(mPM);
        n_model_platforms_c[i] = sizeM;
        model_platforms_c[i] = INTEGER(mPM);
    }

    Rprintf("\n");

    for (int i = 0; i < n_subgroups; i++)
    {
        Rprintf("\nNumber of platforms for subgroup %d is %d: \n\n", i + 1, n_model_platforms_c[i]);
        Rprintf("Platforms for subgroup %d are: ", i + 1);
        for (int j = 0; j < n_model_platforms_c[i]; j++)
        {
            Rprintf("Platform/View %d, ", 1 + model_platforms_c[i][j]);
        }
        Rprintf("\n");
    }

    sample_size_ptr = INTEGER(sample_size);

    Rprintf("\nSample sizes for each selected subgroup:\n");
    for (int i = 0; i < n_subgroups; i++)
    {
        Rprintf(" %d", sample_size_ptr[i]);
    }
    Rprintf("\n");

    int *G = INTEGER(nbr_features);
    Rprintf("Number of features for each platform:\n");
    for (int i = 0; i < n_platforms; i++)
    {
        Rprintf(" %d", G[i]);
    }
    Rprintf("\n");

    double ****X0 = r_list_list_matrix_to_c(n_subgroups, X1_filtered);

    X1 = X0;
    for (int i = 0; i < n_subgroups; i++)
    {
        SEXP subgroup = VECTOR_ELT(X1_filtered, i);
        int n_platforms = LENGTH(subgroup);
        Rprintf("Subgroup %d: %d platforms\n", i + 1, n_platforms);
        for (int j = 0; j < n_platforms; j++)
        {
            SEXP df = VECTOR_ELT(subgroup, j);
            SEXP dims = getAttrib(df, R_DimSymbol);
            int n_rows = INTEGER(dims)[0];
            int n_cols = INTEGER(dims)[1];
            Rprintf("  Platform %d: %d rows, %d cols\n", j + 1, n_rows, n_cols);
        }
    }

    /* Read outcome and covariate arrays from R list storage. */
    double ***newYY_arr = r_list_matrix_to_c(n_subgroups, newYY_list);

    newYY = newYY_arr;

    double ***newCC_ptrs = r_list_matrix_to_c(n_subgroups, newCC_list);
    newCC = newCC_ptrs;
    const char likelihood_type[] = "NonLocal";

    double *h = malloc(n_subgroups * sizeof(double));
    for (int i = 0; i < n_subgroups; i++)
    {
        h[i] = h11;
    }
    double h1 = h0, hg = h11;

    gsl_rng *r = gsl_rng_alloc(gsl_rng_rand48);
    gsl_rng_set(r, seed);

    // Initialize censor index, acceptance ratios, and other variables
    int **censored_index = malloc(n_subgroups * sizeof(int *));
    double **ylatent = malloc(n_subgroups * sizeof(double *));

    for (int m = 0; m < n_subgroups; m++)
    {
        censored_index[m] = malloc(sample_size_ptr[m] * sizeof(int));
        ylatent[m] = dvector(0, sample_size_ptr[m] - 1);
    }
    int n_censored[n_subgroups];
    double **ymean = malloc(n_subgroups * sizeof(double *));
    double **yobs;
    _Bool **yobsb;
    if (type_out == 2) // binary
    {
        yobsb = malloc(n_subgroups * sizeof(_Bool *));
    }
    else // survival or continuous
    {
        yobs = malloc(n_subgroups * sizeof(double *));
    }

    for (int i = 0; i < n_subgroups; i++)
    {
        n_censored[i] = 0;
        ymean[i] = dvector(0, sample_size_ptr[i] - 1);
        if ((type_out == 1) || (type_out == 3)) // right censored or continuous
            yobs[i] = dvector(0, sample_size_ptr[i] - 1);
        else // binary
            yobsb[i] = (_Bool *)malloc(sample_size_ptr[i] * sizeof(_Bool));

        _Bool Delta[sample_size_ptr[i]];
        for (int j = 0; j < sample_size_ptr[i]; j++)
        {
            if (type_out == 1)
                Delta[j] = (_Bool)newYY_arr[i][j][1];
            else
                Delta[j] = 1;
            if ((type_out == 1) || (type_out == 3)) // right censored or continuous
                ymean[i][j] = ylatent[i][j] =yobs[i][j] = newYY_arr[i][j][0];
            else //binary
                ylatent[i][j] = yobsb[i][j] = (_Bool)newYY_arr[i][j][0];
            // To check for binary
           // ymean[i][j] = ylatent[i][j] = yobs[i][j];
            // ylatent[i][j] = gsl_ran_exponential(r, ylatent[i]);

            if ((Delta[j] == 0)&& (type_out == 1))
            {
                ylatent[i][j] += 0.01;
                ymean[i][j] = 0;
            }
             if (type_out == 2)
                ymean[i][j] = 0;
            
        }
        find_indices_not_equal(sample_size_ptr[i], Delta, 1, censored_index[i], &n_censored[i]);
        if (type_out == 1)
            Rprintf("\nNumber of censored values for subgroup %d is %d\n", i + 1, n_censored[i]);
    }
    // Memory for MCMC acceptance (only needed for censored or binary latent
    // updates; for continuous outcomes accept_y is left NULL).
    double **accept_y = NULL;
    if ((type_out == 1) || (type_out == 2))
    {
        accept_y = malloc(n_subgroups * sizeof(double *));
        for (int m = 0; m < n_subgroups; m++)
        {
            accept_y[m] = calloc(sample_size_ptr[m], sizeof(double));
        }
    }

    double ***theta = calloc(n_platforms, sizeof(double **));
    double ***betaTh = calloc(n_platforms, sizeof(double **));

    double ***accept_theta = calloc(n_platforms, sizeof(double **));
    double **accept_gamma = calloc(n_platforms, sizeof(double *));
    _Bool ***gamma = calloc(n_platforms, sizeof(_Bool **));
    double ***gamma_mean = calloc(n_platforms, sizeof(double **));

    for (int l = 0; l < n_platforms; l++)
    {
        theta[l] = calloc(n_platform_models_c[l], sizeof(double *));
        betaTh[l] = calloc(n_platform_models_c[l], sizeof(double *));
        accept_theta[l] = calloc(n_platform_models_c[l], sizeof(double *));
        accept_gamma[l] = calloc(n_platform_models_c[l], sizeof(double));
        gamma[l] = calloc(n_platform_models_c[l], sizeof(_Bool *));
        gamma_mean[l] = calloc(n_platform_models_c[l], sizeof(double *));
        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
            gamma[l][m] = calloc(G[l], sizeof(_Bool));
            gamma_mean[l][m] = calloc(G[l], sizeof(double));
            accept_theta[l][m] = calloc(n_platform_models_c[l], sizeof(double));
            theta[l][m] = calloc(n_platform_models_c[l], sizeof(double));
            betaTh[l][m] = calloc(n_platform_models_c[l], sizeof(double));
            for (int m1 = 0; m1 < n_platform_models_c[l]; m1++)
            {
                betaTh[l][m][m1] = beta0;
            }
        }
    }
    // double *nu=calloc(n_platforms,sizeof(double*));
    double *nu = REAL(nu_R);
    for (int l = 0; l < n_platforms; l++)
    {
        int initg = exp(nu[l]) * G[l] / (1 + exp(nu[l]));
        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
            for (int i = 0; i < initg; i++)
            {
                int ii = gsl_rng_uniform_int(r, G[l]);
                gamma[l][m][ii] = 1;
            }
            for (int m1 = 0; m1 < n_platform_models_c[l]; m1++)
            {
                if (strcmp(sampler_method, "BMS") != 0)
                {
                    theta[l][m][m1] = 0.1 * (m != m1);
                }
                else
                {
                    theta[l][m][m1] = 0;
                }
            }
        }
    }

    // int model;
    //srand(seed);
    GetRNGstate();
    double log_likelihood[n_subgroups], logdet[n_subgroups], scal[n_subgroups];
    Rprintf("\n");

    initialize_sampler_state(type_out, ylatent, newCC, X1,
                gamma, n_platforms, G, n_subgroups,
                platform_models_c, n_platform_models_c,
                model_platforms_c,
                n_model_platforms_c, sample_size_ptr,
                log_likelihood, logdet, scal, h, h1, h0, hg, alpha, psi, K);

    double *mrf = calloc(n_platforms, sizeof(double));
    for (int l = 0; l < n_platforms; l++)
    {
        compute_mrf_normalizer(n_platform_models_c[l], theta[l], nu[l], &mrf[l]);
        Rprintf("%.3lf ", mrf[l]);
    }

    // int s, su, su1;

    double *log_posterior_sample = dvector(0, burnin_c + sample_c - 1);
    _Bool ****gamma_sample = malloc(sample_c * sizeof(_Bool ***));

    for (int s = 0; s < sample_c; s++)
    {
        gamma_sample[s] = malloc(n_platforms * sizeof(_Bool **)); // n_platforms
        for (int l = 0; l < n_platforms; l++)
        {
            gamma_sample[s][l] = bmatrix(0, n_platform_models_c[l] - 1, 0, G[l] - 1);
        }
    }

    if (!gamma_sample)
    {
        nrerror("allocation failure; take appropriate action");
    }

    double ***theta_sample = malloc(n_platforms * sizeof(double **));
    for (int l = 0; l < n_platforms; l++)
    {
        int n_theta_pairs = n_platform_models_c[l] *
                            (n_platform_models_c[l] - 1) / 2;
        theta_sample[l] = n_theta_pairs > 0
            ? dmatrix(0, sample_c - 1, 0, n_theta_pairs - 1)
            : NULL;
    }
    _Bool thetaFreed = false;
    if (strcmp(sampler_method, "BMS") == 0)
    {
        for (int l = 0; l < n_platforms; l++)
        {
            int n_theta_pairs = n_platform_models_c[l] *
                                (n_platform_models_c[l] - 1) / 2;
            if (n_theta_pairs > 0)
                free_dmatrix(theta_sample[l], 0, sample_c - 1,
                             0, n_theta_pairs - 1);
        }
        free(theta_sample);
        thetaFreed = true;
    }

    Rprintf("\n");

    /* Report progress ~10 times; guard against a zero interval (and the
       resulting division by zero) when the chain is shorter than 10. */
    int report_every = (burnin_c + sample_c) / 10;
    if (report_every < 1)
        report_every = 1;

    for (int s = 0; s < burnin_c + sample_c; s++)
    {
        for (int m = 0; m < n_subgroups; m++)
        {
             sample_gamma_indicators(m, n_platforms, model_platforms_c[m], n_model_platforms_c[m], G, sample_size_ptr[m],
                        ylatent[m], newCC[m], X1[m], gamma, &log_likelihood[m], &logdet[m], &scal[m], nu, theta,
                        n_platform_models_c, platform_models_c, accept_gamma, r, likelihood_type, h[m], h1, h0, hg, K, alpha, psi);
            if ((type_out == 1) && (n_censored[m] > 0)) // right censored outcome and we have censored subjects    
            {
                sample_censored_latent_response(m, n_platforms, model_platforms_c[m], n_model_platforms_c[m], G, sample_size_ptr[m],
                             ylatent[m], yobs[m], newCC[m], X1[m], gamma, &scal[m], &log_likelihood[m],
                             n_censored[m], censored_index[m], logdet[m], r, n_platform_models_c, platform_models_c,
                             accept_y[m], h[m], h1, h0, hg, K, alpha, psi);
                if (s >= burnin_c)
                {
                    for (int i = 0; i < n_censored[m]; i++)
                    {
                        int jj = censored_index[m][i];
                        ymean[m][jj] += ylatent[m][jj] / sample_c;
                    }
                }
            }

            if (type_out == 2)  // binary outcome
            {
                 sample_binary_latent_response(m, n_platforms, model_platforms_c[m], n_model_platforms_c[m], G, sample_size_ptr[m],
                                  ylatent[m], yobsb[m], newCC[m], X1[m], gamma, &log_likelihood[m],
                                r, n_platform_models_c, platform_models_c,
                               accept_y[m], h[m], h1, h0, hg, K, alpha, psi);
                if (s >= burnin_c)
                {
                    for (int i = 0; i < sample_size_ptr[m]; i++)
                    { 
                        // printf(" yyy= %lf",ylatent[m][i]);
                        ymean[m][i] += ylatent[m][i] / sample_c;
                    }   
                }
            }
        } // end of loop with m

        if (strcmp(sampler_method, "BMS") != 0)
        {
            for (int l = 0; l < n_platforms; l++)
            {
                 sample_mrf_theta(G[l], n_platform_models_c[l], theta[l], accept_theta[l], &mrf[l],
                          gamma[l], nu[l], alpha0, betaTh[l], r);
            }
            if (s >= burnin_c)
            {
                for (int l = 0; l < n_platforms; l++)
                {
                    int m1 = 0;
                    for (int i = 1; i < n_platform_models_c[l]; i++)
                    {
                        for (int j = 0; j < i; j++)
                        {
                            theta_sample[l][s - burnin_c][m1] = theta[l][i][j];
                            m1++;
                        }
                    }
                }
            }
        }
        if (s >= burnin_c)
        {
            for (int l = 0; l < n_platforms; l++)
            {
                for (int m = 0; m < n_platform_models_c[l]; m++)
                {
                    for (int j = 0; j < G[l]; j++)
                    {
                        gamma_mean[l][m][j] += gamma[l][m][j] / (double)sample_c;
                        gamma_sample[s - burnin_c][l][m][j] = gamma[l][m][j];
                    }
                }
            }
        }

        log_posterior_sample[s] = log_posterior(log_likelihood, gamma, nu, theta, mrf, alpha0, betaTh, n_subgroups,
                              n_platforms, G, n_platform_models_c);

        // Print status every 10% of the MCMC samples
        if (s % report_every == 1)
        {
            Rprintf("\nNbr of MCMC samples = %d\n", s);
            Rprintf("LogPosterior=%f\n", log_posterior_sample[s]);
            for (int l = 0; l < n_platforms; l++)
            {
                Rprintf("Nbr of selected features in platform %d = ", l + 1);
                for (int m = 0; m < n_platform_models_c[l]; m++)
                {
                    double sum = 0;
                    for (int g = 0; g < G[l]; g++)
                        sum += gamma[l][m][g];
                    Rprintf("%.4f ", sum);
                }
                Rprintf("\n");
            }
            for (int l = 0; l < n_platforms; l++)
            {
                Rprintf("theta in platform %d = ", l + 1);
                for (int m = 0; m < n_platform_models_c[l]; m++)
                {
                    for (int m1 = 0; m1 < m; m1++)
                    {
                        Rprintf("%f ", theta[l][m][m1]);
                    }
                }
                Rprintf("\n");
            }
        }
    } // end of loop with s index for the number of mcmc samples
    Rprintf("\nAcceptance ratio\n");
    for (int l = 0; l < n_platforms; l++)
    {
        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
            Rprintf("%.4f ", accept_gamma[l][m] / (sample_c + burnin_c));
        }
        Rprintf("\n");
    }

    if (accept_y != NULL)
    {
        Rprintf("\nAcceptance ratio for latent Y\n");
        for (int m = 0; m < n_subgroups; m++)
        {
            for (int i = 0; i < sample_size_ptr[m]; i++)
                Rprintf("%.4f ", accept_y[m][i] / (sample_c + burnin_c));
            Rprintf("\n\n");
        }
    }
    int i0, j0;

    // export gamma_sample to R
    SEXP gamma_sample_R;
    PROTECT(gamma_sample_R = allocVector(VECSXP, sample_c));
    for (int s = 0; s < sample_c; s++)
    {
        SEXP GamS_R;
        PROTECT(GamS_R = allocVector(VECSXP, n_platforms));
        for (int l = 0; l < n_platforms; l++)
        {
            SEXP gamSMatrix = PROTECT(c_array_to_r_matrix_int(gamma_sample[s][l], n_platform_models_c[l], G[l]));
            SET_VECTOR_ELT(GamS_R, l, gamSMatrix);
            UNPROTECT(1);
        }
        SET_VECTOR_ELT(gamma_sample_R, s, GamS_R);
        UNPROTECT(1);
    }

    // export gamma_mean
    SEXP GamMean_R;
    PROTECT(GamMean_R = allocVector(VECSXP, n_platforms));
    /// return gamma_mean
    for (int l = 0; l < n_platforms; l++)
    {
        SEXP gamXmeanMatrix = PROTECT(c_array_to_r_matrix(gamma_mean[l], n_platform_models_c[l], G[l]));
        SET_VECTOR_ELT(GamMean_R, l, gamXmeanMatrix);
        UNPROTECT(1);
    }

    if (strcmp(sampler_method, "BMS") != 0)
    {
        for (int l = 0; l < n_platforms; l++)
        {
            int n_theta_pairs = n_platform_models_c[l] *
                                (n_platform_models_c[l] - 1) / 2;
            if (n_theta_pairs == 0)
                continue;

            double *ThetaXSM = malloc((size_t)n_theta_pairs * sizeof(double));
            if (!ThetaXSM)
                nrerror("allocation failure for theta posterior means");

            mean_array_columns(sample_c, n_theta_pairs,
                               theta_sample[l], ThetaXSM);
            int m1 = 0;
            for (int i = 1; i < n_platform_models_c[l]; i++)
            {
                for (int j = 0; j < i; j++)
                {
                    theta[l][i][j] = theta[l][j][i] = ThetaXSM[m1];
                    m1++;
                }
            }
            free(ThetaXSM);
        }
    }

    // return ThetaMean
    SEXP thetaMean_R;
    PROTECT(thetaMean_R = allocVector(VECSXP, n_platforms));
    for (int l = 0; l < n_platforms; l++)
    {
        SEXP thetaMatrix = PROTECT(c_array_to_r_matrix(theta[l], n_platform_models_c[l], n_platform_models_c[l]));
        SET_VECTOR_ELT(thetaMean_R, l, thetaMatrix);
        UNPROTECT(1);
    }

    // return theta_sample
    // --- Convert theta_sample to an R matrix ---
    // For the BMS method theta_sample has already been freed (thetaFreed); in
    // that case we return an empty list element per platform instead of
    // dereferencing freed memory.
    SEXP thetaSampleMatrix_R;
    PROTECT(thetaSampleMatrix_R = allocVector(VECSXP, n_platforms));
    for (int l = 0; (l < n_platforms) && (!thetaFreed); l++)
    {
        SEXP thetaSampleMatrix;
        int nrowThetaSample = sample_c;                                            // Rows for ThetaXSample
        int ncolThetaSample = n_platform_models_c[l] * (n_platform_models_c[l] - 1) / 2; // Columns for ThetaXSample
        PROTECT(thetaSampleMatrix = allocVector(REALSXP, nrowThetaSample * ncolThetaSample));
        double *thetaSamplePtr = REAL(thetaSampleMatrix);
        for (i0 = 0; i0 < nrowThetaSample; i0++)
        {
            for (j0 = 0; j0 < ncolThetaSample; j0++)
            {
                thetaSamplePtr[i0 + nrowThetaSample * j0] = theta_sample[l][i0][j0];
            }
        }
        SEXP dimThetaSample;
        PROTECT(dimThetaSample = allocVector(INTSXP, 2));
        INTEGER(dimThetaSample)
        [0] = nrowThetaSample; // Rows
        INTEGER(dimThetaSample)
        [1] = ncolThetaSample; // Columns
        setAttrib(thetaSampleMatrix, R_DimSymbol, dimThetaSample);

        SET_VECTOR_ELT(thetaSampleMatrix_R, l, thetaSampleMatrix);
        UNPROTECT(2);
    }
    // We convert ymean to an R subject
    SEXP YMean_R = PROTECT(array_to_r_list(ymean, n_subgroups, sample_size_ptr));

    SEXP logposterior_R;
    PROTECT(logposterior_R = allocVector(REALSXP, burnin_c + sample_c));
    for (int s = 0; s < burnin_c + sample_c; s++)
        REAL(logposterior_R)
    [s] = log_posterior_sample[s];

    int listSize = 6;
    SEXP list;
    SEXP listNames;
    PROTECT(list = allocVector(VECSXP, listSize));

    // Add common elements
    SET_VECTOR_ELT(list, 0, GamMean_R);
    SET_VECTOR_ELT(list, 1, thetaMean_R);
    SET_VECTOR_ELT(list, 2, YMean_R);
    SET_VECTOR_ELT(list, 3, logposterior_R);
    SET_VECTOR_ELT(list, 4, gamma_sample_R);
    SET_VECTOR_ELT(list, 5, thetaSampleMatrix_R);

    PROTECT(listNames = allocVector(STRSXP, listSize));
    SET_STRING_ELT(listNames, 0, mkChar("gam_mean"));
    SET_STRING_ELT(listNames, 1, mkChar("theta_mean"));
    SET_STRING_ELT(listNames, 2, mkChar("estimate_latent_y"));
    SET_STRING_ELT(listNames, 3, mkChar("log_posterior"));
    SET_STRING_ELT(listNames, 4, mkChar("gam_sample"));
    SET_STRING_ELT(listNames, 5, mkChar("theta_sample"));
    setAttrib(list, R_NamesSymbol, listNames);

    /// We free memories ...
    for (int s = 0; s < sample_c; s++)
    {
        for (int l = 0; l < n_platforms; l++)
        {
            free_bmatrix(gamma_sample[s][l], 0, n_platform_models_c[l] - 1, 0, G[l] - 1);
        }
        free(gamma_sample[s]);
    }
    free(gamma_sample);

    for (int m = 0; m < n_subgroups; m++)
    {
        free(censored_index[m]);
        free(ylatent[m]);
       // free(yobs[m]);
    }
    free(censored_index);
    free(ylatent);
    if (type_out == 2)//binary
    {
        for (int m = 0; m < n_subgroups; m++)
        {
            free(yobsb[m]);
        }
        free(yobsb);
    } else {
        for (int m = 0; m < n_subgroups; m++)
        {
            free(yobs[m]);
        }
        free(yobs);
    }
    
    if ((type_out == 1) || (type_out == 2))
    {
        for (int m = 0; m < n_subgroups; m++)
        {
            free(accept_y[m]);
        }
        free(accept_y);
    }

    if (!thetaFreed)
    {
        for (int l = 0; l < n_platforms; l++)
        {
            int n_theta_pairs = n_platform_models_c[l] *
                                (n_platform_models_c[l] - 1) / 2;
            if (n_theta_pairs > 0)
                free_dmatrix(theta_sample[l], 0, sample_c - 1,
                             0, n_theta_pairs - 1);
        }
        free(theta_sample);
    }
    gsl_rng_free(r);
    free(mrf);

    for (int l = 0; l < n_platforms; l++)
    {

        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
            free(gamma[l][m]);
            free(gamma_mean[l][m]);
            free(accept_theta[l][m]);
            free(theta[l][m]);
            free(betaTh[l][m]);
        }
        free(gamma[l]);
        free(gamma_mean[l]);
        free(accept_theta[l]);
        free(accept_gamma[l]);
        free(theta[l]);
        free(betaTh[l]);
    }
    free(gamma);
    free(accept_gamma);
    free(gamma_mean);
    free(accept_theta);
    free(theta);
    free(betaTh);
    free_r_list_list_matrix_to_c(X0, n_subgroups, X1_filtered);
    X0 = NULL;

    for (int m = 0; m < n_subgroups; m++)
    {
        free(ymean[m]);
        free_dmatrix(newCC_ptrs[m], 0, sample_size_ptr[m] - 1, 0, K - 1);
        free_dmatrix(newYY_arr[m], 0, sample_size_ptr[m] - 1, 0, 2 - 1);
    }
    free(newCC_ptrs);
    free(newYY_arr);
    free(ymean);

    free(model_platforms_c);
    free(n_model_platforms_c);
    free(platform_models_c);
    free(n_platform_models_c);
    free(h);
    free(log_posterior_sample);

    int protect_count = 19; // Base protect count
    UNPROTECT(protect_count);
PutRNGstate();
    Rprintf("\n");
    clock_t t1 = clock() - t;
    double time_taken = ((double)t1) / CLOCKS_PER_SEC; // in seconds
    Rprintf("\nTime taken in minutes before prediction is %f\n", time_taken / 60);
    Rprintf("\nTime taken in hours before prediction is  %f\n", time_taken / 3600);
    return list;
}
