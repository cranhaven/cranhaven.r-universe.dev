#include <gsl/gsl_blas.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_cblas.h>
#include <math.h>
#include <stdlib.h>
#include "my_header.h"
#include "utils.h"

/*
 * Update probit latent responses for a binary-outcome subgroup.
 *
 * Observed 1 values are constrained to positive latent responses and observed
 * 0 values to negative latent responses.  Each proposal is accepted using the
 * non-local-prior marginal likelihood for the current selected model.
 */
void sample_binary_latent_response(int model, int n_platforms, int *selected_platforms, int n_selected_platforms, int *n_features, int sample_size,
                        double *latent_y, _Bool *observed_y, double **covariates, double ***features, _Bool ***gamma, double *log_likelihood,
                        gsl_rng *rng, int *n_platform_models, int **platform_models,
                        double *accept_y, double slab_scale, double covariate_scale, double intercept_scale, double first_platform_scale, int n_covariates, double alpha, double psi)
{
    (void)n_platforms;

    int *selected_feature_index[n_selected_platforms];
    int n_selected_features[n_selected_platforms];
    for (int i = 0; i < n_selected_platforms; i++)
    {
        int platform_index = selected_platforms[i];
        int platform_model_index = -1;
        for (int ss = 0; ss < n_platform_models[platform_index]; ss++)
        {
            if (platform_models[platform_index][ss] == model)
            {
                platform_model_index = ss;
                break;
            }
        }
        if (platform_model_index == -1)
        {
            Rf_error("Subgroup not found");
        }

        selected_feature_index[i] = malloc(n_features[platform_index] * sizeof(int));
        if (!selected_feature_index[i])
        {
            Rf_error("malloc failed for selected_feature_index[%d]\n", i);
        }
        n_selected_features[i] = 0;
        find_indices_not_equal(n_features[platform_index], gamma[platform_index][platform_model_index], 0, selected_feature_index[i], &n_selected_features[i]);
    }
    int i, j;
    double **design = build_design_matrix(n_covariates, n_selected_platforms, n_selected_features, selected_feature_index, covariates, features, selected_platforms, sample_size);
    int n_subjects = sample_size;

    int total_selected_features = 0;
    for (int p = 0; p < n_selected_platforms; p++)
    {
        total_selected_features += n_selected_features[p];
    }
    int k_val = 1 + n_covariates + total_selected_features;
    int max_iter = 25;
    double tolerance = pow(10, -3);
    int moment_order = 1;
    double *precision = build_posterior_precision(k_val, n_covariates, n_selected_features[0], n_subjects, slab_scale, covariate_scale, intercept_scale, first_platform_scale, design);
    double precision_copy[k_val * k_val];
    for (i = 0; i < k_val; i++)
        for (j = 0; j <= i; j++)
            precision_copy[i * k_val + j] = precision_copy[j * k_val + i] = precision[i * k_val + j];
    gsl_matrix_view m = gsl_matrix_view_array(precision, k_val, k_val);
    gsl_linalg_cholesky_decomp(&m.matrix);
    double ynew[n_subjects];
    for (i = 0; i < n_subjects; i++)
        ynew[i] = latent_y[i];
    for (i = 0; i < sample_size; i++)
    {

        /*
         * Propose a latent value constrained by the observed binary response.
         * Following Chekouo et al. (2016), the exponential proposal uses scale
         * 1 / |y_old|.  GSL parameterizes exponentials by their mean, so the
         * reciprocal is passed here.
         */
        if (observed_y[i] == 1)
            ynew[i] = gsl_ran_exponential(rng, 1.0 / latent_y[i]);
        else
            ynew[i] = -gsl_ran_exponential(rng, -1.0 / latent_y[i]);

        double *beta_mode = malloc(k_val * sizeof(double));
        double new_log_likelihood = log_likelihood_nonlocal(k_val, n_covariates, n_selected_features[0], n_subjects, alpha, psi, ynew, design, precision_copy,
                                          &m.matrix, beta_mode, moment_order, slab_scale, covariate_scale, intercept_scale, first_platform_scale, max_iter, tolerance, 0);
        free(beta_mode);
        double accept_u = gsl_ran_flat(rng, 0, 1);
        double log_accept_ratio = new_log_likelihood - *log_likelihood + log(ynew[i] / latent_y[i]);

        if (log(accept_u) < log_accept_ratio)
        {
            latent_y[i] = ynew[i];
            accept_y[i] += 1;
            *log_likelihood = new_log_likelihood;
        }
        ynew[i] = latent_y[i];
    }
    free(precision);

    for (i = 0; i < n_subjects; i++)
        free(design[i]);
    free(design);
    for (int i = 0; i < n_selected_platforms; i++)
        free(selected_feature_index[i]);
}
