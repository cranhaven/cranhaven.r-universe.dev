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
 * Cross-validation prediction entry point called from R.
 *
 * The MCMC output from main_function() is converted back into C arrays, duplicate
 * posterior models are collapsed, and each CV fold is scored for survival,
 * binary, or continuous outcomes.
 */
SEXP main_function_prediction(SEXP h0_R, SEXP hh_R, SEXP alpha_R, SEXP psi_R, SEXP alpha0_R, SEXP beta0_R,
                            SEXP seed_R, SEXP nu_R,
                            SEXP latent_y_R, SEXP gamma_sample_R, SEXP Theta_R,
                            SEXP method1_R, SEXP n_platforms_R,
                            SEXP platform_models_R, SEXP model_platforms_R, SEXP n_subgroups_R,
                            SEXP sample_size, SEXP nbr_features, SEXP nbr_cov,
                            SEXP X1_filtered, SEXP newYY_list, SEXP newCC_list,
                            SEXP type_outcome,
                            SEXP sample, SEXP kcv_R, SEXP rounds_R, SEXP max_models_R)
{
    clock_t t = clock();
    int protect_count = 0;

    double h0 = REAL(h0_R)[0]; // scaling factor
    double h11 = REAL(hh_R)[0];
    double alpha = REAL(alpha_R)[0];   // weight of prior beliefs
    double psi = REAL(psi_R)[0];       // control var of prior distributions
    double alpha0 = REAL(alpha0_R)[0]; // prior
    double beta0 = REAL(beta0_R)[0];   // prior scaling factor

    PROTECT(platform_models_R);
    protect_count++;
    PROTECT(model_platforms_R);
    protect_count++;
    int n_cv_rounds = asInteger(rounds_R);
    int sample_c = asInteger(sample);
    int n_subgroups = asInteger(n_subgroups_R);
    int max_models_requested = asInteger(max_models_R); // Maximum number of models to be used for prediction and Bayesian model averaging

    int n_unique_models;
    int type_out = asInteger(type_outcome);
    double *nu = REAL(nu_R);
    int n_platforms = asInteger(n_platforms_R);
    int n_folds = asInteger(kcv_R);
    int fold;
    double *post = dvector(0, sample_c - 1);
    int *model_index = malloc(sample_c * sizeof(int));
    int *high_model_index = malloc(sample_c * sizeof(int));
    double ***YY = r_list_matrix_to_c(n_subgroups, newYY_list);
    double **ymean = r_list_vector_double_to_c(n_subgroups, latent_y_R);
    double ***CC = r_list_matrix_to_c(n_subgroups, newCC_list);
    double ****XX = r_list_list_matrix_to_c(n_subgroups, X1_filtered);
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
    int *G = INTEGER(nbr_features);
    int *sample_size_ptr = INTEGER(sample_size);
    int K = asInteger(nbr_cov);
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

    const char likelihood_type[] = "NonLocal";
    double ***beta = infer_posterior_models(ymean, CC, XX, sample_c, gamma_sample,
                                     nu, theta, mrf, h, h1, h0,
                                     hg, alpha0,
                                     alpha, psi, G, n_subgroups, n_platforms,
                                     n_platform_models_c, n_model_platforms_c, model_platforms_c,
                                     platform_models_c, sample_size_ptr, K,
                                     betaTh, likelihood_type, post, model_index,
                                     high_model_index, &n_unique_models, max_models_requested);

    int max_models = MIN(n_unique_models, max_models_requested);
    int N = 0;
    for (int l = 0; l < n_subgroups; l++)
        N += sample_size_ptr[l];
    double Cidx[n_subgroups + 1], CidxT[n_subgroups + 1], sdCidx[n_subgroups + 1];
    double **ypredT = malloc((n_subgroups + 1) * sizeof(double *));
    double **yT;
    _Bool **yTb;
    if (type_out == 2)
        yTb = malloc((n_subgroups + 1) * sizeof(_Bool *));
    else
        yT = malloc((n_subgroups + 1) * sizeof(double *));

    _Bool **delT = malloc((n_subgroups + 1) * sizeof(_Bool *));
    int jt[n_subgroups + 1];
    for (int m = 0; m < n_subgroups + 1; m++)
    {
        if (m < n_subgroups)
        {
            ypredT[m] = malloc(sample_size_ptr[m] * sizeof(double));
            if (type_out == 2)
                yTb[m] = malloc(sample_size_ptr[m] * sizeof(_Bool));
            else
                yT[m] = malloc(sample_size_ptr[m] * sizeof(double));
            delT[m] = malloc(sample_size_ptr[m] * sizeof(_Bool));
        }
        else
        {
            ypredT[m] = malloc(N * sizeof(double));
            if (type_out == 2)
                yTb[m] = malloc(N * sizeof(_Bool));
            else
                yT[m] = malloc(N * sizeof(double));
            delT[m] = malloc(N * sizeof(_Bool));
        }
        Cidx[m] = 0;
        sdCidx[m] = 0;
        jt[m] = 0;
    }
    int **censored_index = calloc(n_subgroups, sizeof(int *));
    int *n_censored = calloc(n_subgroups, sizeof(int));
    _Bool **Delta = calloc(n_subgroups, sizeof(_Bool *));
    for (int i = 0; i < n_subgroups; i++)
    {
        censored_index[i] = calloc(sample_size_ptr[i], sizeof(int));
        Delta[i] = calloc(sample_size_ptr[i], sizeof(_Bool));
        n_censored[i] = 0;

        for (int j = 0; j < sample_size_ptr[i]; j++)
        {
            if (type_out == 1) // for survival outcome
            {
                Delta[i][j] = (_Bool)YY[i][j][1];
            }
            else
            {
                Delta[i][j] = 1; // for continuous and binary outcome
            }
        }
        find_indices_not_equal(sample_size_ptr[i], Delta[i], 1, censored_index[i], &n_censored[i]);
    }

    long seed = (long)REAL(seed_R)[0];
    gsl_rng *r = gsl_rng_alloc(gsl_rng_rand48);
    gsl_rng_set(r, seed);
    int n_uncensored[n_subgroups];
    int **uncensored_index = malloc(n_subgroups * sizeof(int *));
    for (int m = 0; m < n_subgroups; m++)
    {
        uncensored_index[m] = malloc((sample_size_ptr[m] - n_censored[m]) * sizeof(int));
    }

    int cv_round;
    double **c_index_list = calloc(n_cv_rounds, sizeof(double *));
    double **total_c_index_list = calloc(n_cv_rounds, sizeof(double *));
    for (cv_round = 0; cv_round < n_cv_rounds; cv_round++)
    {
        c_index_list[cv_round] = calloc((n_subgroups + 1), sizeof(double));
        total_c_index_list[cv_round] = calloc((n_subgroups + 1), sizeof(double));
        for (int m = 0; m < n_subgroups; m++)
        {
            if (n_censored[m] > 0)
                gsl_ran_shuffle(r, censored_index[m], n_censored[m], sizeof(int));
            find_indices_not_equal(sample_size_ptr[m], Delta[m], 0, uncensored_index[m], &n_uncensored[m]);
            if (n_uncensored[m] > 0)
                gsl_ran_shuffle(r, uncensored_index[m], n_uncensored[m], sizeof(int));
            jt[m] = 0;
            sdCidx[m] = 0;
            Cidx[m] = 0;
        }
        Cidx[n_subgroups] = 0;
        jt[n_subgroups] = 0;
        sdCidx[n_subgroups] = 0;

        for (fold = 0; fold < n_folds; fold++)
        {
            int j = 0;
            double *ytotal;
            _Bool *ytotalb;
            if ((type_out == 1) || (type_out == 3)) // survival outcome or continuous
                ytotal = dvector(0, N - 1);
            else // binary outcome
                ytotalb = calloc(N, sizeof(_Bool));

            _Bool *deltatotal = malloc(N * sizeof(_Bool));
            double *ypredtotal = dvector(0, N - 1);

            for (int m = 0; m < n_subgroups; m++)
            {
                int test_sample_size;
                _Bool *ytestb;
                double *ytest;
                _Bool deltatest[sample_size_ptr[m]];
                int test_index[sample_size_ptr[m]], train_index[sample_size_ptr[m]];
                make_cv_partition(fold, n_folds, sample_size_ptr[m], &test_sample_size, censored_index[m], n_censored[m],
                          uncensored_index[m], test_index, train_index);

                double *ypred = predict_cv_fold(type_out, m, K, n_model_platforms_c[m], model_platforms_c[m],
                                        n_platform_models_c, platform_models_c, G,
                                        sample_size_ptr[m], test_sample_size, test_index, train_index,
                                        ymean[m], CC[m], XX[m], gamma_sample, alpha, psi,
                                        max_models, model_index, high_model_index, sample_c);

                if (type_out == 2) // binary outcome
                    ytestb = calloc(test_sample_size, sizeof(_Bool));
                else
                    ytest = calloc(test_sample_size, sizeof(double));

                for (int i = 0; i < test_sample_size; i++)
                {
                    deltatest[i] = Delta[m][test_index[i]];
                    if (type_out == 2) // binary outcome
                    {
                        ytestb[i] = (_Bool)YY[m][test_index[i]][0];
                    }
                    else
                    {
                        ytest[i] = YY[m][test_index[i]][0];
                    }
                }
                double ci;
                if (type_out == 1) // survival outcome
                    ci = concordance_index(test_sample_size, ypred, ytest, deltatest);
                else if (type_out == 2)                      // binary outcome
                    ci = auc(test_sample_size, ypred, ytestb); // this is the AUC
                else
                    ci = mean_squared_error(test_sample_size, ypred, ytest); // continuous outcome

                Cidx[m] += ci;
                sdCidx[m] += pow(ci, 2);
                for (int i = 0; i < test_sample_size; i++)
                {

                    if (type_out == 2)
                    {
                        yTb[m][i + jt[m]] = ytestb[i];
                        ytotalb[i + j] = ytestb[i];
                    }
                    else
                    {
                        ytotal[i + j] = ytest[i];
                        yT[m][i + jt[m]] = ytest[i];
                    }
                    delT[m][i + jt[m]] = deltatest[i];
                    deltatotal[i + j] = deltatest[i];
                    ypredtotal[i + j] = ypred[i];
                    ypredT[m][i + jt[m]] = ypred[i];
                }
                j += test_sample_size;
                jt[m] += test_sample_size;
                if (type_out == 2)
                    free(ytestb);
                else
                    free(ytest);
                free(ypred);
            }
            for (int i = 0; i < j; i++)
            {
                ypredT[n_subgroups][i + jt[n_subgroups]] = ypredtotal[i];
                if (type_out == 2)
                {
                    yTb[n_subgroups][i + jt[n_subgroups]] = ytotalb[i];
                }
                else
                    yT[n_subgroups][i + jt[n_subgroups]] = ytotal[i];

                delT[n_subgroups][i + jt[n_subgroups]] = deltatotal[i];
            }
            jt[n_subgroups] += j;
            double ci4;

            if (type_out == 1) // survival outcome
                ci4 = concordance_index(j, ypredtotal, ytotal, deltatotal);
            else if (type_out == 2)                // binary outcome
                ci4 = auc(j, ypredtotal, ytotalb); // this is the AUC to modify this part
            else
                ci4 = mean_squared_error(j, ypredtotal, ytotal); // continuous outcome

            Cidx[n_subgroups] += ci4;
            sdCidx[n_subgroups] += pow(ci4, 2);
            if (type_out == 2)
                free(ytotalb);
            else
                free(ytotal);

            free(deltatotal);
            free(ypredtotal);
        }
        if (cv_round >= 2)
            Rprintf("\n%drd %d-CVfold\n", cv_round + 1, n_folds);
        else if (cv_round == 0)
            Rprintf("\n%dst %d-CVfold\n", cv_round + 1, n_folds);
        else if (cv_round == 1)
            Rprintf("\n%dnd %d-CVfold\n", cv_round + 1, n_folds);

        char metric_name[3][10] = {"C-indices", "AUCs", "MSEs"};
        for (int m = 0; m < n_subgroups + 1; m++)
        {
            Cidx[m] = Cidx[m] / n_folds;
            if (n_folds > 1)
                sdCidx[m] = pow((sdCidx[m] - n_folds * pow(Cidx[m], 2)) / (n_folds - 1), 0.5);
            else
                sdCidx[m] = NA_REAL;
            if (m < n_subgroups)
            {
                Rprintf("Average of %s from %d-CVfold of group %d is  %f\n", metric_name[type_out - 1], n_folds, m + 1, Cidx[m]);
            }
            else
            {
                Rprintf("Average of %s from %d-CVfold of all samples  is  %f\n", metric_name[type_out - 1], n_folds, Cidx[m]);
            }
            int subset_size;
            if (m == n_subgroups)
                subset_size = N;
            else
                subset_size = sample_size_ptr[m];
            if (type_out == 1) // survival outcome
                CidxT[m] = concordance_index(subset_size, ypredT[m], yT[m], delT[m]);
            else if (type_out == 2)                      // binary outcome
                CidxT[m] = auc(subset_size, ypredT[m], yTb[m]);
            else if (type_out == 3)                      // continuous outcome
                CidxT[m] = mean_squared_error(subset_size, ypredT[m], yT[m]);
            if (m < n_subgroups)
                Rprintf("\nTotal %s from group %d is %f\n", metric_name[type_out - 1], m + 1, CidxT[m]);
            else
                Rprintf("\nTotal %s from all samples is %f\n", metric_name[type_out - 1], CidxT[m]);
            c_index_list[cv_round][m] = Cidx[m];
            total_c_index_list[cv_round][m] = CidxT[m];
        }

    }

    // Convert to R outputs
    SEXP total_c_index_R = PROTECT(c_array_to_r_matrix(total_c_index_list, n_cv_rounds, n_subgroups + 1));
    protect_count++;
    SEXP c_index_R = PROTECT(c_array_to_r_matrix(c_index_list, n_cv_rounds, n_subgroups + 1));
    protect_count++;
    int list_size = 2;
    SEXP list;
    SEXP list_names;
    PROTECT(list = allocVector(VECSXP, list_size));
    protect_count++;
    SET_VECTOR_ELT(list, 0, total_c_index_R);
    SET_VECTOR_ELT(list, 1, c_index_R);
    PROTECT(list_names = allocVector(STRSXP, list_size));
    protect_count++;
    SET_STRING_ELT(list_names, 0, mkChar("total_cindex"));
    SET_STRING_ELT(list_names, 1, mkChar("subset_cindex"));
    setAttrib(list, R_NamesSymbol, list_names);

    /* We free allocated memories */
    free_r_list_list_matrix_to_c(XX, n_subgroups, X1_filtered);
    XX = NULL;
    for (int m = 0; m < n_subgroups; m++)
    {
        free(ymean[m]);
        free_dmatrix(CC[m], 0, sample_size_ptr[m] - 1, 0, K - 1);
        free_dmatrix(YY[m], 0, sample_size_ptr[m] - 1, 0, 2 - 1);
    }
    free(CC);
    free(YY);
    free(ymean);
    for (int m = 0; m < n_subgroups; m++)
    {
        free(censored_index[m]);
        free(uncensored_index[m]);
        free(Delta[m]);
    }
    free(n_censored);
    free(censored_index);
    free(uncensored_index);
    free(Delta);
    gsl_rng_free(r);
    for (int m = 0; m < n_subgroups + 1; m++)
    {
        free(ypredT[m]);
        if (type_out == 2)
            free(yTb[m]);
        else
            free(yT[m]);
        free(delT[m]);
    }
    free(ypredT);
    if (type_out == 2)
        free(yTb);
    else
        free(yT);
    free(delT);
    for (int s = 0; s < n_unique_models; s++)
    {
        for (int m = 0; m < n_subgroups; m++)
        {
            free(beta[s][m]);
        }
        free(beta[s]);
    }
    free(beta);

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
    free(h);
    free(post);
    free(platform_models_c);
    free(n_platform_models_c);
    free(model_platforms_c);
    free(n_model_platforms_c);
    for (cv_round = 0; cv_round < n_cv_rounds; cv_round++)
    {
        free(c_index_list[cv_round]);
        free(total_c_index_list[cv_round]);
    }
    free(c_index_list);
    free(total_c_index_list);
    free(mrf);

    UNPROTECT(protect_count);

    t = clock() - t;
    double time_taken = ((double)t) / CLOCKS_PER_SEC; // in seconds
    Rprintf("\n\nTime taken for assessing prediction in seconds is %f\n", time_taken);
    Rprintf("\nTime taken for assessing prediction in minutes is %f\n", time_taken / 60);
    Rprintf("\nTime taken for assessing prediction in hours is %f\n", time_taken / 3600);
    return list;
}
