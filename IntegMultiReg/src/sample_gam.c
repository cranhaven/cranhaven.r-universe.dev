#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf.h>
#include "my_header.h"

/*
 * Update the variable-selection indicators for one availability subgroup.
 *
 * Platforms inside the subgroup are updated sequentially.  After each proposed
 * gamma move, the design matrix is rebuilt, so later platform proposals in the
 * same sweep condition on earlier accepted moves.
 */
void sample_gamma_indicators(
    int subgroup, int n_platforms, int *selected_platforms,
    int n_selected_platforms, int *n_features, int sample_size,
    double *latent_y, double **covariates, double ***features,
    _Bool ***gamma, double *log_likelihood, double *logdet,
    double *quadratic_form, double *nu, double ***theta,
    int *n_platform_models, int **platform_models, double **accept_gamma,
    gsl_rng *rng, const char *likelihood_type, double slab_scale,
    double covariate_scale, double intercept_scale, double first_platform_scale,
    int n_covariates, double alpha, double psi)
{
    (void)n_platforms;

    /* Selected feature indices and counts for each active platform. */
    int *selected_feature_index[n_selected_platforms];
    int n_selected_features[n_selected_platforms];
    for (int i = 0; i < n_selected_platforms; i++)
    {
        int platform_index = selected_platforms[i];
        int platform_model_index = -1;
        for (int ss = 0; ss < n_platform_models[platform_index]; ss++)
        {
            if (platform_models[platform_index][ss] == subgroup)
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
        find_indices_not_equal(
            n_features[platform_index], gamma[platform_index][platform_model_index],
            0, selected_feature_index[i], &n_selected_features[i]);
    }

    /* Process each active platform sequentially. */
    for (int i = 0; i < n_selected_platforms; i++)
    {
        int platform_index = selected_platforms[i];
        int platform_model_index = -1;
        for (int ss = 0; ss < n_platform_models[platform_index]; ss++)
        {
            if (platform_models[platform_index][ss] == subgroup)
            {
                platform_model_index = ss;
                break;
            }
        }
        if (platform_model_index == -1)
        {
            Rf_error("Subgroup not found");
        }

        int old_n_selected_features = n_selected_features[i];
        int *old_selected_feature_index = malloc(old_n_selected_features * sizeof(int));
        for (int j = 0; j < old_n_selected_features; j++)
        {
            old_selected_feature_index[j] = selected_feature_index[i][j];
        }

        _Bool *gamma_proposal = malloc(n_features[platform_index] * sizeof(_Bool));
        propose_gamma_update(
            n_features[platform_index], gamma[platform_index][platform_model_index],
            gamma_proposal, 0.5, rng);

        int new_n_selected_features = 0;
        find_indices_not_equal(
            n_features[platform_index], gamma_proposal, 0,
            selected_feature_index[i], &new_n_selected_features);

        n_selected_features[i] = new_n_selected_features;


        double **proposed_design = build_design_matrix(
            n_covariates, n_selected_platforms, n_selected_features,
            selected_feature_index, covariates, features, selected_platforms,
            sample_size);

        double new_logdet = 0;
        double new_quadratic_form = 0;
        double new_log_likelihood = 0;
        if (strcmp(likelihood_type, "Local") == 0)
        {
            double Sigma[sample_size * sample_size];
            int total_selected_features = 0;
            for (int t = 0; t < n_selected_platforms; t++)
            {
                total_selected_features += n_selected_features[t];
            }

            for (int j = 0; j < sample_size; j++)
            {
                for (int s = 0; s <= j; s++)
                {
                    double a = 0;
                    for (int f = 0; f < total_selected_features; f++)
                    {
                        a += proposed_design[j][f] * proposed_design[s][f];
                    }
                    Sigma[j * sample_size + s] =
                        Sigma[s * sample_size + j] = slab_scale + a;
                }
                Sigma[j * sample_size + j] += 1;
            }

            new_quadratic_form = cholesky_quadratic_form(
                sample_size, Sigma, latent_y, &new_logdet);
            new_log_likelihood =
                -(sample_size / 2.0) * log(2 * IMR_PI) +
                gsl_sf_lngamma(sample_size / 2.0 + alpha) -
                gsl_sf_lngamma(alpha) - 0.5 * new_logdet -
                ((sample_size / 2.0) + alpha) *
                    log(1 + new_quadratic_form / (2 * psi));
        }
        else
        {
            const int max_iter = 40;
            const int moment_order = 1;
            const double tolerance = 1e-3;
            int total_selected_features = 0;
            for (int p = 0; p < n_selected_platforms; p++)
            {
                total_selected_features += n_selected_features[p];
            }
            int k_val = 1 + n_covariates + total_selected_features;

            double *precision = build_posterior_precision(
                k_val, n_covariates, n_selected_features[0], sample_size,
                slab_scale, covariate_scale, intercept_scale,
                first_platform_scale, proposed_design);
            double precision_copy[k_val * k_val];
            for (int m = 0; m < k_val; m++)
            {
                for (int j = 0; j <= m; j++)
                {
                    precision_copy[m * k_val + j] = precision_copy[j * k_val + m] = precision[m * k_val + j];
                }
            }
            gsl_matrix_view precision_view =
                gsl_matrix_view_array(precision, k_val, k_val);
            gsl_linalg_cholesky_decomp(&precision_view.matrix);
            double *beta_mode = malloc(k_val * sizeof(double));
            new_log_likelihood = log_likelihood_nonlocal(
                k_val, n_covariates, n_selected_features[0], sample_size,
                alpha, psi, latent_y, proposed_design, precision_copy,
                &precision_view.matrix, beta_mode, moment_order, slab_scale,
                covariate_scale, intercept_scale, first_platform_scale,
                max_iter, tolerance, 0);
            free(precision);
            free(beta_mode);
        }
        for (int row = 0; row < sample_size; row++)
        {
            free(proposed_design[row]);
        }
        free(proposed_design);

        int changed_feature_index[2] = {0, 0};
        int d = 0;
        double log_prior_ratio = 0;
        for (int g = 0; g < n_features[platform_index]; g++)
        {
            int dif = gamma_proposal[g] - gamma[platform_index][platform_model_index][g];
            if (dif != 0)
            {
                changed_feature_index[d] = g;
                double tx = 0;
                for (int s = 0; s < n_platform_models[platform_index]; s++)
                {
                    if (s != platform_model_index)
                    {
                        tx += theta[platform_index][platform_model_index][s] * gamma[platform_index][s][g];
                    }
                }
                tx += nu[platform_index];
                log_prior_ratio += dif * tx;
                d++;
            }
        }

        /* MH ratio = marginal-likelihood change + MRF/sparsity-prior change. */
        double u_val = gsl_ran_flat(rng, 0, 1);
        if (log(u_val) < new_log_likelihood - *log_likelihood + log_prior_ratio)
        {
            for (int g = 0; g < d; g++)
            {
                gamma[platform_index][platform_model_index][changed_feature_index[g]] = gamma_proposal[changed_feature_index[g]];
            }
            *log_likelihood = new_log_likelihood;
            if (strcmp(likelihood_type, "Local") == 0)
            {
                *logdet = new_logdet;
                *quadratic_form = new_quadratic_form;
            }
            accept_gamma[platform_index][platform_model_index] += 1;
        }
        else
        {
            n_selected_features[i] = old_n_selected_features;
            for (int t = 0; t < old_n_selected_features; t++)
            {
                selected_feature_index[i][t] = old_selected_feature_index[t];
            }
        }
        free(old_selected_feature_index);
        free(gamma_proposal);
    }

    for (int i = 0; i < n_selected_platforms; i++)
    {
        free(selected_feature_index[i]);
    }
}

double cholesky_quadratic_form(
    int n, double matrix[n * n], double vector[n], double *logdet)
{
    gsl_matrix_view chol = gsl_matrix_view_array(matrix, n, n);
    gsl_vector_view rhs = gsl_vector_view_array(vector, n);
    gsl_vector *solution = gsl_vector_alloc(n);

    gsl_linalg_cholesky_decomp(&chol.matrix);
    gsl_linalg_cholesky_solve(&chol.matrix, &rhs.vector, solution);
    *logdet = cholesky_logdet(&chol.matrix);

    double quadratic_form = 0;
    for (int i = 0; i < n; i++)
    {
        quadratic_form += vector[i] * gsl_vector_get(solution, i);
    }
    gsl_vector_free(solution);
    return quadratic_form;
}

double cholesky_logdet(gsl_matrix *chol)
{
    size_t n = chol->size1;

    double logdet = 0.0;

    for (size_t i = 0; i < n; i++)
    {
        logdet += log(gsl_matrix_get(chol, i, i));
    }
    return 2 * logdet;
}

void propose_gamma_update(
    int n_features, _Bool *current_gamma, _Bool *proposal_gamma,
    float flip_probability, gsl_rng *rng)
{
    for (int i = 0; i < n_features; i++)
        proposal_gamma[i] = current_gamma[i];

    int n_excluded = 0;
    int n_included = 0;
    int excluded_index[n_features];
    find_indices_not_equal(
        n_features, current_gamma, 1, excluded_index, &n_excluded);

    int included_index[n_features];
    find_indices_not_equal(
        n_features, current_gamma, 0, included_index, &n_included);

    double u = gsl_ran_flat(rng, 0, 1);

    if ((u < flip_probability) || (n_excluded == 0) || (n_included == 0))
    {
        int feature = gsl_rng_uniform_int(rng, n_features);
        proposal_gamma[feature] = 1 - current_gamma[feature];
    }
    else
    {
        int excluded = gsl_rng_uniform_int(rng, n_excluded);
        int included = gsl_rng_uniform_int(rng, n_included);
        proposal_gamma[excluded_index[excluded]] =
            current_gamma[included_index[included]];
        proposal_gamma[included_index[included]] =
            current_gamma[excluded_index[excluded]];
    }
}

/* Collect indices where values[i] != excluded_value. */
void find_indices_not_equal(
    int n, _Bool *values, int excluded_value, int *index, int *n_found)
{
    int ii_data[n];
    int idx = 0;
    int ii = 0;
    _Bool exitg2 = 0;
    _Bool guard2 = 0;
    while ((exitg2 == 0) && (ii < n))
    {
        guard2 = 0;
        if (values[ii] != excluded_value)
        {
            idx++;
            ii_data[idx - 1] = ii;
            if (idx >= n)
            {
                exitg2 = 1;
            }
            else
            {
                guard2 = 1;
            }
        }
        else
        {
            guard2 = 1;
        }

        if (guard2 == 1)
        {
            ii++;
        }
    }

    int loop_ub = idx;
    for (idx = 0; idx < loop_ub; idx++)
    {
        index[idx] = ii_data[idx];
    }
    *n_found = loop_ub;
}

double **build_design_matrix(
    int n_covariates, int n_selected_platforms, int *n_selected_features,
    int **selected_feature_index, double **covariates, double ***features,
    int *selected_platforms, int sample_size)
{
    int i, j, p;

    int total_features = n_covariates;
    for (p = 0; p < n_selected_platforms; p++)
    {
        total_features += n_selected_features[p];
    }

    double **design = malloc(sample_size * sizeof(double *));
    if (!design)
    {
        Rf_error("malloc failed for design");
    }
    for (i = 0; i < sample_size; i++)
    {
        design[i] = malloc(total_features * sizeof(double));
        if (!design[i])
        {
            Rf_error("malloc failed for design[%d]", i);
        }
        for (j = 0; j < total_features; j++)
        {
            design[i][j] = 0.0;
        }
    }

    /* Columns are always-included covariates followed by selected features
     * from each active platform, in the same platform order as the subgroup. */
    for (i = 0; i < sample_size; i++)
    {
        int col = 0;
        for (j = 0; j < n_covariates; j++)
        {
            design[i][col] = covariates[i][j];
            col++;
        }
        for (p = 0; p < n_selected_platforms; p++)
        {
            int platform_index = selected_platforms[p];
            for (j = 0; j < n_selected_features[p]; j++)
            {
                design[i][col] =
                    features[platform_index][i][selected_feature_index[p][j]];
                col++;
            }
        }
    }

    return design;
}
