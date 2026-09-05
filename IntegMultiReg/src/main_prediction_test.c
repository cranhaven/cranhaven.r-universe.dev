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
#include <Rinternals.h>

/*
 * External test-set prediction entry point called from R.
 *
 * This mirrors main_function_prediction() up to posterior-model inference, then
 * applies Bayesian model averaging to the supplied test covariates/features.
 */
SEXP main_function_prediction_test(SEXP h0_R, SEXP hh_R, SEXP alpha_R, SEXP psi_R, 
                                SEXP alpha0_R, SEXP beta0_R,
                                SEXP seed_R, SEXP nu_R,
                                SEXP latent_y_R, SEXP gamma_sample_R, SEXP Theta_R,
                                SEXP method1_R, SEXP n_platforms_R,
                                SEXP platform_models_R, SEXP model_platforms_R, SEXP n_subgroups_R,
                                SEXP sample_size, SEXP nbr_features, SEXP nbr_cov,
                                SEXP X1_filtered, SEXP newCC_list,
                                SEXP sample, SEXP X1test, SEXP C_test, 
                                SEXP samplesize_test_R, SEXP max_models_R)
{
    int protect_count = 0;
    PROTECT(h0_R);
    protect_count++;
    PROTECT(alpha_R);
    protect_count++;
    PROTECT(psi_R);
    protect_count++;
    PROTECT(alpha0_R);
    protect_count++;
    PROTECT(beta0_R);
    protect_count++;
    PROTECT(seed_R);
    protect_count++;
    PROTECT(nu_R);
    protect_count++;
    PROTECT(sample);
    protect_count++;
    PROTECT(n_subgroups_R);
    protect_count++;
    PROTECT(n_platforms_R);
    protect_count++;
    PROTECT(platform_models_R);
    protect_count++;
    PROTECT(model_platforms_R);
    protect_count++;
    PROTECT(nbr_features);
    protect_count++;
    PROTECT(sample_size);
    protect_count++;
    PROTECT(nbr_cov);
    protect_count++;
    PROTECT(samplesize_test_R);
    protect_count++;
    PROTECT(max_models_R);
    protect_count++;

    double h0 = REAL(h0_R)[0];         // scaling factor
    double h11 = REAL(hh_R)[0];         // scaling factor
    double alpha = REAL(alpha_R)[0];   // weight of prior beliefs
    double psi = REAL(psi_R)[0];       // control var of prior distributions
    double alpha0 = REAL(alpha0_R)[0]; // prior
    double beta0 = REAL(beta0_R)[0];   // prior scaling factor
    int sample_c = asInteger(sample);
    int n_subgroups = asInteger(n_subgroups_R);
    int *G = INTEGER(nbr_features);
    int *sample_size_ptr = INTEGER(sample_size);
    int *samplesize_test = INTEGER(samplesize_test_R);
    int K = asInteger(nbr_cov);
    double *nu = REAL(nu_R);
    int n_platforms = asInteger(n_platforms_R);
    int max_models_requested = asInteger(max_models_R);

    double *post = dvector(0, sample_c - 1);
    int *model_index = malloc(sample_c * sizeof(int));
    int *high_model_index = malloc(sample_c * sizeof(int));
    double **ymean = r_list_vector_double_to_c(n_subgroups, latent_y_R);
    double ***CC = r_list_matrix_to_c(n_subgroups, newCC_list);
    double ***CCtest = r_list_matrix_to_c(n_subgroups, C_test);
    double ****XX = r_list_list_matrix_to_c(n_subgroups, X1_filtered);
    double ****XXtest = r_list_list_matrix_to_c(n_subgroups, X1test);
    _Bool ****gamma_sample = r_list_list_matrix_to_c_bool(sample_c, gamma_sample_R);
    double ***theta = r_list_matrix_to_c(n_platforms, Theta_R);

    int **platform_models_c = malloc(n_platforms * sizeof(int *));
    int *n_platform_models_c = malloc(n_platforms * sizeof(int));
    for (int i = 0; i < n_platforms; i++)
    {
        SEXP mPM = VECTOR_ELT(platform_models_R, i);
        int sizeM = LENGTH(mPM);
        n_platform_models_c[i] = sizeM;
        platform_models_c[i] = INTEGER(mPM);
    }

    int **model_platforms_c = malloc(n_subgroups * sizeof(int *));
    int *n_model_platforms_c = malloc(n_subgroups * sizeof(int));

    for (int i = 0; i < n_subgroups; i++)
    {
        SEXP mPM = VECTOR_ELT(model_platforms_R, i);
        int sizeM = LENGTH(mPM);
        n_model_platforms_c[i] = sizeM;
        model_platforms_c[i] = INTEGER(mPM);
    }
    double *mrf = calloc(n_platforms, sizeof(double));
    for (int l = 0; l < n_platforms; l++)
    {
        compute_mrf_normalizer(n_platform_models_c[l], theta[l], nu[l], &mrf[l]);
    }
    double *h = malloc(n_subgroups * sizeof(double));
    for (int i = 0; i < n_subgroups; i++)
    {
        h[i] = h11;
    }
    double h1 = h0;
    double hg = h11;
    double ***betaTh = calloc(n_platforms, sizeof(double **));
    for (int l = 0; l < n_platforms; l++)
    {
        betaTh[l] = calloc(n_platform_models_c[l], sizeof(double *));
        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
            betaTh[l][m] = calloc(n_platform_models_c[l], sizeof(double));
            for (int m1 = 0; m1 < n_platform_models_c[l]; m1++)
            {
                betaTh[l][m][m1] = beta0;
            }
        }
    }
    int n_unique_models;
    const char likelihood_type[] = "NonLocal";
    double ***beta = infer_posterior_models(ymean, CC, XX, sample_c, gamma_sample,
                                     nu, theta, mrf, h, h1, h0,
                                     hg, alpha0,
                                     alpha, psi, G, n_subgroups, n_platforms,
                                     n_platform_models_c, n_model_platforms_c, model_platforms_c,
                                     platform_models_c, sample_size_ptr, K,
                                     betaTh, likelihood_type, post, model_index,
                                     high_model_index, &n_unique_models, max_models_requested);

    double **ypredT = calloc((n_subgroups), sizeof(double *));
    int max_models = MIN(n_unique_models, max_models_requested);
    for (int m = 0; m < n_subgroups; m++)
    {
        double *tmp = NULL;
        if (samplesize_test[m] > 0)
        {
            tmp = predict_bma(m, K, n_model_platforms_c[m], samplesize_test[m],
                           model_platforms_c[m], n_platform_models_c, platform_models_c, G,
                           CCtest[m], XXtest[m], gamma_sample, beta, post, max_models,
                           model_index, high_model_index, sample_c);
        }
        else if (samplesize_test[m] == 0)
        {
            tmp = calloc(samplesize_test[m], sizeof(double));
        }
        ypredT[m] = tmp;
    }
    SEXP Ypred_R = PROTECT(array_to_r_list(ypredT, n_subgroups, samplesize_test));
    protect_count++;
    UNPROTECT(protect_count);

    free_r_list_list_matrix_to_c(XX, n_subgroups, X1_filtered);
    XX = NULL;
    free_r_list_list_matrix_to_c(XXtest, n_subgroups, X1test);
    XXtest = NULL;

    for (int m = 0; m < n_subgroups; m++)
    {
        free_dmatrix(CC[m], 0, sample_size_ptr[m] - 1, 0, K - 1);
        free_dmatrix(CCtest[m], 0, samplesize_test[m] - 1, 0, K - 1);
        free(ypredT[m]);
        free(ymean[m]);
    }
    free(CC);
    free(CCtest);
    free(ypredT);
    free(ymean);
    for (int l = 0; l < n_platforms; l++)
    {
        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
            free(theta[l][m]);
            free(betaTh[l][m]);
        }
        free(theta[l]);
        free(betaTh[l]);
    }
    free(theta);
    free(betaTh);

    for (int s = 0; s < n_unique_models; s++)
    {
        for (int m = 0; m < n_subgroups; m++)
        {
            free(beta[s][m]);
        }
        free(beta[s]);
    }
    free(beta);
    for (int s = 0; s < sample_c; s++)
    {
        for (int l = 0; l < n_platforms; l++)
        {
            for (int m = 0; m < n_platform_models_c[l]; m++)
            {
                free(gamma_sample[s][l][m]);
            }
            free(gamma_sample[s][l]);
        }
        free(gamma_sample[s]);
    }
    free(gamma_sample);
    free(model_index);
    free(high_model_index);
    free(post);
    free(platform_models_c);
    free(n_platform_models_c);
    free(model_platforms_c);
    free(n_model_platforms_c);
    free(h);
    free(mrf);
    return Ypred_R;
}
