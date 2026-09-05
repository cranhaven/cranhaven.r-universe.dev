#include <stdbool.h>
#include <gsl/gsl_sf.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "my_header.h"
#include "utils.h"

/*
 * Update latent event times for censored observations in one subgroup.
 *
 * For right-censored outcomes the latent time must exceed the observed
 * censoring time.  The proposal samples from a bounded exponential tail and is
 * accepted with the corresponding proposal-density correction.
 */
void sample_censored_latent_response(int model, int n_platforms, int *selected_platforms, int n_selected_platforms, int *n_features, int sample_size,
                  double *latent_y, double *observed_y, double **covariates, double ***features, _Bool ***gamma, double *quadratic_form, double *log_likelihood,
                  int n_censored, int *censored_index, double logdet, gsl_rng *rng, int *n_platform_models, int **platform_models,
                  double *accept_y, double slab_scale, double covariate_scale, double intercept_scale, double first_platform_scale, int n_covariates, double alpha, double psi)
{
  (void)n_platforms;
  (void)quadratic_form;
  (void)logdet;

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
  double censoring_time = 0;
    int total_selected_features = 0;
    for (int p = 0; p < n_selected_platforms; p++)
    {
      total_selected_features += n_selected_features[p];
    }
    int k_val = 1 + n_covariates + total_selected_features;
    double ymax = 1000;
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
    for (i = 0; i < n_censored; i++)
    {
      int subject_index = censored_index[i];
      censoring_time = observed_y[subject_index];
      double u1 = gsl_ran_flat(rng, 0, 1);
      double old_tail_scale = latent_y[subject_index] - censoring_time;
      double old_tail_ratio = (ymax + 0.5 - censoring_time) / old_tail_scale;
      double proposed_y = censoring_time - old_tail_scale * log(1 - u1 * (1 - exp(-old_tail_ratio)));
      double new_tail_scale = proposed_y - censoring_time;
      double new_tail_ratio = (ymax + 0.5 - censoring_time) / new_tail_scale;
      ynew[subject_index] = proposed_y;
      double *beta_mode = malloc(k_val * sizeof(double));
      double new_log_likelihood = log_likelihood_nonlocal(k_val, n_covariates, n_selected_features[0], n_subjects, alpha, psi, ynew, design, precision_copy,
                                        &m.matrix, beta_mode, moment_order, slab_scale, covariate_scale, intercept_scale, first_platform_scale, max_iter, tolerance, 0);
      free(beta_mode);
      double accept_u = gsl_ran_flat(rng, 0, 1);
      double log_accept_ratio = new_log_likelihood - *log_likelihood + log(old_tail_scale) +
                      (proposed_y - censoring_time) / old_tail_scale + log(1 - exp(-old_tail_ratio)) -
                      log(new_tail_scale) - (latent_y[subject_index] - censoring_time) / new_tail_scale - log(1 - exp(-new_tail_ratio));
      if (log(accept_u) < log_accept_ratio)
      {
        latent_y[subject_index] = proposed_y;
        accept_y[subject_index] += 1;
        *log_likelihood = new_log_likelihood;
      }
      else
      {
        ynew[subject_index] = latent_y[subject_index];
      }
    }
    free(precision);
  
  for (i = 0; i < n_subjects; i++)
    free(design[i]);
  free(design);
  for (int i = 0; i < n_selected_platforms; i++)
    free(selected_feature_index[i]);
}
