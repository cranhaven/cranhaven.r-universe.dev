#include <stdbool.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf.h>
#include "my_header.h"
#include "utils.h"

/*
 * Predict one held-out fold by refitting coefficients for the highest-posterior
 * variable-selection models and averaging their fold predictions.
 */
double *predict_cv_fold(int type_out, int model, int K, int n_selected_platforms, int *selected_platforms,
                int *n_platform_models, int **platform_models, int *G,
                int model_sample_size, int test_sample_size, int *test_index, int *train_index,
                double *y, double **C, double ***X, _Bool ****gamma_sample, double alpha, double psi,
                int max_models, int *model_index, int *high_model_index, int sample)
{

  int l, i, j, j1, in, i1, i2;
  double *weight = malloc(max_models * sizeof(double));
  // yhat=predicted survival time
  double *yhat = dvector(0, test_sample_size - 1);
  for (i = 0; i < test_sample_size; i++)
    yhat[i] = 0;
  double **yh = dmatrix(0, max_models - 1, 0, test_sample_size - 1);
  for (l = 0; l < max_models; l++)
  {
    int l0 = high_model_index[l];
    int l1 = model_index[l0];

    int **selected_feature_index = malloc(n_selected_platforms * sizeof(int *));
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
        Rf_error("Subgroup %d not found for platform %d\n", 1 + model, 1 + platform_index);
      }
      selected_feature_index[i] = malloc(G[platform_index] * sizeof(int));
      if (!selected_feature_index[i])
      {
        Rf_error("malloc failed for selected_feature_index[%d]\n", i);
      }
      n_selected_features[i] = 0;
      find_indices_not_equal(G[platform_index], gamma_sample[sample - 1 - l1][platform_index][platform_model_index], 0, selected_feature_index[i], &n_selected_features[i]);
    }

    double **PG = build_design_matrix(K, n_selected_platforms, n_selected_features, selected_feature_index, C, X, selected_platforms, model_sample_size);

    int train_sample_size = model_sample_size - test_sample_size;
    int total_nx = 0;
    for (int p = 0; p < n_selected_platforms; p++)
    {
      total_nx += n_selected_features[p];
    }
    int k = 1 + K + total_nx;

    double *precision = malloc(k * k * sizeof(double));
    for (j = 0; j < k; j++)
    {
      for (j1 = 0; j1 <= j; j1++)
      {
        double a = 0;
        if ((j == 0) && (j1 == 0))
        {
          a = train_sample_size;
        }
        else if (j1 == 0)
        {
          for (i2 = 0; i2 < train_sample_size; i2++)
          {
            i1 = train_index[i2];
            a += PG[i1][j - 1];
          }
        }
        else if ((j != 0) && (j1 != 0))
        {
          for (i2 = 0; i2 < train_sample_size; i2++)
          {
            i1 = train_index[i2];
            a += PG[i1][j - 1] * PG[i1][j1 - 1];
          }
        }
        if (j == j1)
          a += .001; // To always make the matrix positive definite
        precision[j * k + j1] = precision[j1 * k + j] = a;
      }
    }

    double *xty = calloc(k, sizeof(double));
    for (j = 0; j < k; j++)
    {
      double a1 = 0;
      for (i2 = 0; i2 < train_sample_size; i2++)
      {
        i1 = train_index[i2];
        if (j == 0)
          a1 += y[i1];
        else
          a1 += PG[i1][j - 1] * y[i1];
      }
      xty[j] = a1;
    }
    gsl_vector_view b = gsl_vector_view_array(xty, k);

    gsl_vector *x = gsl_vector_alloc(k);
    gsl_matrix_view Aip = gsl_matrix_view_array(precision, k, k);
    int status = gsl_linalg_cholesky_decomp(&Aip.matrix);
    if (status)
    {
      Rprintf("Cholesky failed (subgroup %d): %s\n", model, gsl_strerror(status));
      weight[l] = -INFINITY;
      gsl_vector_free(x);
      free(xty);
      free(precision);
      for (i = 0; i < model_sample_size; i++)
        free(PG[i]);
      free(PG);
      for (int platform = 0; platform < n_selected_platforms; platform++)
        free(selected_feature_index[platform]);
      free(selected_feature_index);
      continue;
    }

    gsl_linalg_cholesky_solve(&Aip.matrix, &b.vector, x);

    double *beta = malloc(k * sizeof(double));
    for (j = 0; j < k; j++)
    {
      beta[j] = gsl_vector_get(x, j);
    }
    gsl_vector_free(x);
    double xxy = 0;
    for (in = 0; in < test_sample_size; in++)
    {
      yh[l][in] = 0;
      i = test_index[in];
      for (j = 0; j < k; j++)
      {
        if (j == 0)
          yh[l][in] += beta[0];
        else
          yh[l][in] += PG[i][j - 1] * beta[j];
      }
      xxy += pow(y[i] - yh[l][in], 2);
    }
    double xx = 0;
    double *yht = malloc(train_sample_size * sizeof(double));
    for (in = 0; in < train_sample_size; in++)
    {
      yht[in] = 0;
      i = train_index[in];
      for (j = 0; j < k; j++)
      {
        if (j == 0)
          yht[in] += beta[0];
        else
          yht[in] += PG[i][j - 1] * beta[j];
      }
      xx += pow(y[i] - yht[in], 2);
    }
    free(beta);
    free(yht);
    free(xty);
    int nutest = 2 * alpha + train_sample_size;
    double sigm2 = (psi + xx) / nutest;
    weight[l] = (test_sample_size / 2.0) * log(sigm2) + 0.5 * (2 * alpha + model_sample_size) * log(1 + xxy / (nutest * sigm2));
    for (i = 0; i < model_sample_size; i++)
      free(PG[i]);
    free(PG);
    free(precision);
    for (int i = 0; i < n_selected_platforms; i++)
    {
      free(selected_feature_index[i]);
    }
    free(selected_feature_index);
  }
  double wmax = max(max_models, weight);
  double sumw = 0;
  for (l = 0; l < max_models; l++)
  {
    weight[l] = exp(weight[l] - wmax);
    sumw += weight[l];
  }
  double *probtest;
  if (type_out == 2) // binary outcome
  {
    // Normalize the model-averaging weights once (not inside the per-test-point
    // loop, which would repeatedly divide the shared weights by sumw).
    for (l = 0; l < max_models; l++)
    {
      weight[l] = weight[l] / sumw;
    }
    probtest = malloc(test_sample_size * sizeof(double));
    for (i = 0; i < test_sample_size; i++)
    {
      probtest[i] = 0.0; // must be initialized before accumulating below
      double *log_prob = malloc(max_models * sizeof(double));

      for (l = 0; l < max_models; l++)
      {
        log_prob[l] = -log(2) + gsl_sf_log_erfc(-yh[l][i] / sqrt(2));
      }
      double logprobmax = max(max_models, log_prob);
      for (l = 0; l < max_models; l++)
      {
        probtest[i] += weight[l] * exp(log_prob[l] - logprobmax);
      }
      probtest[i] = probtest[i] * exp(logprobmax);
      free(log_prob);
    }
  }
  else if ((type_out == 1)||(type_out==3)) // survival outcome
  {
    for (in = 0; in < test_sample_size; in++)
    {
      double a = 0;
      for (l = 0; l < max_models; l++)
      {
        a += yh[l][in] * weight[l];
      }
      yhat[in] = a / sumw;
    }
  }

  free(weight);

  free_dmatrix(yh, 0, max_models - 1, 0, test_sample_size - 1);

  if ((type_out == 1) || (type_out == 3)) // survival outcome or continuous  
  {
    return yhat;
  }
  else 
  {
    return probtest;
  }
}

/* Harrell-style concordance index for censored survival predictions. */
double concordance_index(int n, double *prediction, double *observed_time, _Bool *event)
{
  int i, j;
  double concordance_denominator = 0;
  double concordance_numerator = 0;
  double time1, time2, prediction1, prediction2;
  for (i = 0; i < n; i++)
  {
    time1 = observed_time[i];
    prediction1 = prediction[i];
    for (j = 0; j < n; j++)
    {
      if (i != j)
      {
        time2 = observed_time[j];
        prediction2 = prediction[j];
        concordance_numerator +=
            (prediction2 > prediction1) * (time2 > time1) * (event[i] == 1) +
            (prediction2 < prediction1) * (time2 < time1) * (event[j] == 1) +
            0.5 * ((prediction2 == prediction1) || (time2 == time1)) * (event[i] == 1) * (event[j] == 0) +
            0.5 * ((prediction2 == prediction1) || (time2 == time1)) * (event[j] == 1) * (event[i] == 0);
        concordance_denominator +=
            (time2 > time1) * (event[i] == 1) +
            (time2 < time1) * (event[j] == 1) +
            (time2 == time1) * (event[i] == 1) * (event[j] == 0) +
            (time2 == time1) * (event[i] == 0) * (event[j] == 1);
      }
    }
  }
  return concordance_numerator / concordance_denominator;
}

/*
 * Predict all samples for one subgroup by Bayesian model averaging over the
 * posterior model list inferred from MCMC gamma samples.
 */
double *predict_bma(int model, int K, int n_selected_platforms, int sample_size, int *selected_platforms,
                 int *n_platform_models, int **platform_models, int *G,
                 double **C, double ***X, _Bool ****gamma_sample,
                 double ***beta, double *post, int max_models,
                 int *model_index, int *high_model_index, int sample)
{
  int l, i, j;
  double *yhat = dvector(0, sample_size - 1);
  for (i = 0; i < sample_size; i++)
    yhat[i] = 0;
  for (l = 0; l < max_models; l++)
  {
    int l0 = high_model_index[l];
    int l1 = model_index[l0];

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
        Rf_error("Subgroup %d not found for platform %d\n", 1 + model, 1 + platform_index);
      }
      selected_feature_index[i] = malloc(G[platform_index] * sizeof(int));
      if (!selected_feature_index[i])
      {
        Rf_error("malloc failed for selected_feature_index[%d]\n", i);
      }
      n_selected_features[i] = 0;
      find_indices_not_equal(G[platform_index], gamma_sample[sample - 1 - l1][platform_index][platform_model_index], 0, selected_feature_index[i], &n_selected_features[i]);
    }

    double **PG = build_design_matrix(K, n_selected_platforms, n_selected_features, selected_feature_index, C, X, selected_platforms, sample_size);
    int total_nx = 0;
    for (int p = 0; p < n_selected_platforms; p++)
    {
      total_nx += n_selected_features[p];
    }
    int k = 1 + K + total_nx;
    for (i = 0; i < sample_size; i++)
    {
      double yh = 0;
      for (j = 0; j < k; j++)
      {
        if (j == 0)
          yh += beta[l0][model][0];
        else
          yh += PG[i][j - 1] * beta[l0][model][j];
      }
      yhat[i] += yh * post[l];
    }
    for (i = 0; i < sample_size; i++)
      free(PG[i]);
    free(PG);
    for (int i = 0; i < n_selected_platforms; i++)
      free(selected_feature_index[i]);
  }
  return yhat;
}

/*
 * Collapse duplicated gamma samples, keep the highest-posterior models, and
 * compute regression coefficients used by prediction and cross-validation.
 */
double ***infer_posterior_models(double **y, double ***C, double ****X, int sample, _Bool ****gamma_sample,
                          double *nu, double ***theta, double *mrf, double *h, double h1, double h0,
                          double hg, double alpha0,
                          double alpha, double psi, int *G, int n_subgroups, int n_platforms,
                          int *n_platform_models_c, int *n_model_platforms_c, int **model_platforms_c,
                          int **platform_models_c, int *sample_size_ptr, int K,
                          double ***betaTh, const char *likelihood_type, double *post, int *model_index,
                          int *high_model_index, int *n_unique_models_out, int max_models)

{

  _Bool *all_true = calloc(n_subgroups, sizeof(_Bool));
  for (int m = 0; m < n_subgroups; m++)
    all_true[m] = 1;
  int i, j;
  int n_unique_models = 0;
  model_index[n_unique_models] = 0;
  for (i = 0; i < sample; i++)
  {
    for (j = 0; j < n_unique_models; j++)
    {
      _Bool **result = calloc(n_platforms, sizeof(_Bool *));
      for (int l = 0; l < n_platforms; l++)
      {
        result[l] = calloc(n_platform_models_c[l], sizeof(_Bool));
        for (int m = 0; m < n_platform_models_c[l]; m++)
        {
          result[l][m] = 1;
        }
      }
      int model_differs = 0;
      for (int l = 0; l < n_platforms; l++)
      {
        for (int k = 0; k < n_platform_models_c[l]; k++)
        {
          result[l][k] = bool_vectors_equal(G[l], gamma_sample[sample - 1 - i][l][k], gamma_sample[sample - 1 - model_index[j]][l][k]);
          if (result[l][k] == 0)
          {
            model_differs = 1;
            break;
          }
        }
        if (model_differs)
        {
          break; // breaks outer loop
        }
      }
      double sumres = 0;

      for (int l = 0; l < n_platforms; l++)
      {
        sumres += bool_vectors_equal(n_platform_models_c[l], result[l], all_true);
        free(result[l]);
      }
      free(result);
      if (sumres == n_platforms)
        break; // if compar is 1 for each platform

    } // end of the j loop

    if (j == n_unique_models)
    {
      model_index[n_unique_models] = i;
      n_unique_models++;
    }
    if (n_unique_models == 100 * max_models)
    { // we limit the number of models to maximum of 100 times maxmodels
      break;
    }
  } // end of the i loop
  *n_unique_models_out = n_unique_models;
  free(all_true);
  Rprintf("\n\n\nNumber of different gamma values for variable selection (nbr of models) = %d", n_unique_models);
  Rprintf("\n");
  double ***beta = malloc(n_unique_models * sizeof(double **));
  int l;
  for (l = 0; l < n_unique_models; l++)
  {
    beta[l] = malloc(n_subgroups * sizeof(double *));
    int l1 = model_index[l];
    double loglik[n_subgroups];
    for (int m = 0; m < n_subgroups; m++)
    {
      int N = sample_size_ptr[m];
      int **selected_feature_index = calloc(n_model_platforms_c[m], sizeof(int *));
      int *n_selected_features = calloc(n_model_platforms_c[m], sizeof(int));
      for (int ll = 0; ll < n_model_platforms_c[m]; ll++)
      {
        int platform_index = model_platforms_c[m][ll];
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
	          Rf_error("Subgroup %d not found for platform %d\n", m, platform_index);
	        }
        selected_feature_index[ll] = calloc(G[platform_index], sizeof(int));
        find_indices_not_equal(G[platform_index], gamma_sample[sample - 1 - l1][platform_index][platform_model_index], 0, selected_feature_index[ll], &n_selected_features[ll]);
      }
      double **PG = build_design_matrix(K, n_model_platforms_c[m], n_selected_features, selected_feature_index,
                            C[m], X[m], model_platforms_c[m], N);
      int total_selected_features = 0;
      for (int ll = 0; ll < n_model_platforms_c[m]; ll++)
      {
	        total_selected_features += n_selected_features[ll];
	      }

      int s;

      if (strcmp(likelihood_type, "Local") == 0)
      {
        double a;
        double Sigma[N * N];
        for (int i = 0; i < N; i++)
        {
          for (int j = 0; j <= i; j++)
          {
            a = 0;
            for (s = 0; s < total_selected_features; s++)
            {
              a += PG[i][s] * PG[j][s];
            }
            Sigma[i * N + j] = Sigma[j * N + i] = h0 + h[m] * a;
          }
          Sigma[i * N + i] += 1;
        }
        double logdet = 0;
        double scal = cholesky_quadratic_form(N, Sigma, y[m], &logdet);
        loglik[m] = -(N / 2.0) * log(IMR_PI * 2 * alpha) + gsl_sf_lngamma(N / 2.0 + alpha) - gsl_sf_lngamma(alpha) - 0.5 * logdet - ((N / 2.0) + alpha) * log(1 + scal / (2 * psi));
      }
      else
      {
        int maxiter = 40;
        double stop = 1e-3;
        int rr = 1;
        int k = 1 + K + total_selected_features;
        double *precision = build_posterior_precision(k, K, n_selected_features[0], N, h[m], h1, h0, hg, PG);
        double precision_copy[k * k];
        for (int i = 0; i < k; i++)
        {
          for (int j = 0; j <= i; j++)
          {
	            precision_copy[i * k + j] = precision_copy[j * k + i] = precision[i * k + j];
	          }
	        }
	        gsl_matrix_view m11 = gsl_matrix_view_array(precision, k, k);
        gsl_linalg_cholesky_decomp(&m11.matrix);
        beta[l][m] = malloc(k * sizeof(double));
	        loglik[m] = log_likelihood_nonlocal(k, K, n_selected_features[0], N, alpha, psi, y[m], PG, precision_copy, &m11.matrix,
	                                   beta[l][m], rr, h[m], h1, h0, hg, maxiter, stop, 0);
	        free(precision);
	      }
      for (int i = 0; i < N; i++)
        free(PG[i]);
      free(PG);
      free(n_selected_features);
      for (int ll = 0; ll < n_model_platforms_c[m]; ll++)
        free(selected_feature_index[ll]);
      free(selected_feature_index);
    }
    post[l] = log_posterior(loglik, gamma_sample[sample - 1 - l1], nu, theta, mrf, alpha0, betaTh, n_subgroups,
                      n_platforms, G, n_platform_models_c);
  } // end number of models l=0

  sort_descending_index(n_unique_models, post, high_model_index);
  double max_log_post = post[0];

  int n_top_models = MIN(max_models, n_unique_models);
  for (l = 0; l < n_top_models; l++)
  {
    post[l] = exp(post[l] - max_log_post);
  }
  double sum_post = n_top_models * mean(n_top_models, post);

	for (l = 0; l < n_top_models; l++)
	{
	    post[l] = post[l] / sum_post;
	}
  return beta;
}

/*
 * Build one CV fold while preserving the censored/uncensored composition.
 */
void make_cv_partition(int fold, int n_folds, int model_sample_size, int *test_sample_size, int *censored_index, int n_censored, int *uncensored_index, int *test_index, int *train_index)
{
  int n_uncensored = model_sample_size - n_censored;
  int i = 0;
  int i1 = 0;
  int i2 = 0;

  for (i = 0; i < n_censored; i++)
  {
    if ((fold * n_censored / n_folds > i) || ((fold + 1) * n_censored / n_folds <= i))
    {
      train_index[i1] = censored_index[i];
      i1++;
    }
    else
    {
      test_index[i2] = censored_index[i];
      i2++;
    }
  }
  for (i = 0; i < n_uncensored; i++)
  {
    if ((fold * n_uncensored / n_folds > i) || ((fold + 1) * n_uncensored / n_folds <= i))
    {
      train_index[i1] = uncensored_index[i];
      i1++;
    }
    else
    {
      test_index[i2] = uncensored_index[i];
      i2++;
    }
  }
  *test_sample_size = i2;
}
