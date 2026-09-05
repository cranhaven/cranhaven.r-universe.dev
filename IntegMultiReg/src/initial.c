#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf.h>
#include <R.h>
#include <Rinternals.h>
#include "my_header.h"
#include "utils.h"

/*
 * Initialize likelihood quantities before the MCMC loop.
 *
 * The routine builds each subgroup's current design matrix from the initial
 * gamma indicators.  For binary outcomes it also creates a first probit latent
 * response using a ridge fit so the sampler starts in a feasible region.
 */
void initialize_sampler_state(int type_out, double **Y, double ***newCC, double ****X1,
                 _Bool ***gamma, int n_platforms, int *G, int n_subgroups,
                 int **platform_models_c, int *n_platform_models_c,
                 int **model_platforms_c, int *n_model_platforms_c, int *sample_size_ptr,
                 double *log_likelihood, double *logdet, double *scal,
                 double *h, double h1, double h0, double hg, double alpha, double psi, int K)
{

  for (int m = 0; m < n_subgroups; m++)
  {
    int N = sample_size_ptr[m];
    int **selected_feature_index = calloc(n_model_platforms_c[m], sizeof(int *));
    int *n_selected_features = calloc(n_model_platforms_c[m], sizeof(int));
    for (int l = 0; l < n_model_platforms_c[m]; l++)
    {
      int platform_index = model_platforms_c[m][l];
      int platform_model_index = -1;
      for (int ss = 0; ss < n_platform_models_c[platform_index]; ss++)
      {
        if (platform_models_c[platform_index][ss] == m)
        {
          platform_model_index = ss;
          break;
        }
      }
      if (platform_model_index == -1)
      {
        error("Error: subgroup not found\n");
      }

      selected_feature_index[l] = calloc(G[platform_index], sizeof(int));
      find_indices_not_equal(G[platform_index], gamma[platform_index][platform_model_index], 0, selected_feature_index[l], &n_selected_features[l]);
    }

    double **PG = build_design_matrix(K, n_model_platforms_c[m], n_selected_features, selected_feature_index,
                          newCC[m], X1[m], model_platforms_c[m], N);
    int total_selected_features = 0;
    for (int l = 0; l < n_model_platforms_c[m]; l++)
    {
      total_selected_features += n_selected_features[l];
    }
    if (type_out == 2)
    { // binary outcome
      int tot = 1 + K + total_selected_features;
      double *X_data = calloc(N*tot, sizeof(double));
      int ll = 0;
      for (int i = 0; i < N; i++)
      {
        for (int j = 0; j < tot; j++)
        {
          if (j == 0)
            X_data[ll] = 1;
          else
            X_data[ll] = PG[i][j - 1];
          ll++;
        }
      }
      double *ypred = calloc(N, sizeof(double));

      ridge_predict_only(X_data, Y[m], N, tot, 1, ypred);
      for (int i = 0; i < N; i++)
      {
        if (Y[m][i] == 1)
        {
           Y[m][i] = r_lefttruncnorm(0, ypred[i], 1);
        }
        else if (Y[m][i] == 0)
        {
          Y[m][i] = r_righttruncnorm(0, ypred[i], 1);
        }
      }
      free(X_data);
      free(ypred);
    }

    int maxiter = 25;
    double stop = pow(10, -3);
    int rr = 1;
    int k = 1 + K + total_selected_features;
    double *precision = build_posterior_precision(k, K, n_selected_features[0], N, h[m], h1, h0, hg, PG);
    double precision_copy[k * k];
    for (int i = 0; i < k; i++)
      for (int j = 0; j <= i; j++)
        precision_copy[i * k + j] = precision_copy[j * k + i] = precision[i * k + j];
    gsl_matrix_view m11 = gsl_matrix_view_array(precision, k, k);
    gsl_linalg_cholesky_decomp(&m11.matrix);
    double *beta_mode = malloc(k * sizeof(double));
    log_likelihood[m] = log_likelihood_nonlocal(k, K, n_selected_features[0], N, alpha, psi, Y[m], PG, precision_copy, &m11.matrix, beta_mode, rr, h[m], h1, h0, hg, maxiter, stop, 0);
    free(precision);
    free(beta_mode);

    for (int i = 0; i < N; i++)
      free(PG[i]);
    free(PG);
    free(n_selected_features);
    for (int l = 0; l < n_model_platforms_c[m]; l++)
      free(selected_feature_index[l]);
    free(selected_feature_index);
  }
}
