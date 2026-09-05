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

static int factorial_int(int n)
{
  int f = 1;
  int i;
  for (i = 1; i <= n; i++)
  {
    f *= i;
  }
  return f;
}
/*
 * Build X'X plus prior precision terms for the current selected model.
 *
 * Coefficient order is intercept, always-included covariates, then selected
 * molecular features.  Different prior scales are used for those blocks.
 */
double *build_posterior_precision(
    int k, int K, int ng, int N, double h, double h1, double h0, double hg,
    double **design)
{
  double *precision = malloc(k * k * sizeof(double));
  int i, j, l;
  for (i = 0; i < k; i++)
  {
    for (j = 0; j <= i; j++)
    {
      double a = 0;
      if ((i == 0) && (j == 0))
      {
        a = N + (1 / h0);
      }
      else if (j == 0)
      {
        for (l = 0; l < N; l++)
          a += design[l][i - 1];
      }
      else if ((i != 0) && (j != 0))
      {
        for (l = 0; l < N; l++)
          a += design[l][i - 1] * design[l][j - 1];
        if (i == j)
        {
          /*
           * Coefficient indexing in precision is:
           *   0       : intercept
           *   1..K    : always-included clinical covariates
           *   K+1..   : selected molecular features.
           *
           * The intercept is handled by the (i == 0 && j == 0) branch above.
           * Use <= here so the Kth covariate is not accidentally assigned the
           * molecular-feature prior scale.
           */
          if (i <= K)
            a += (1.0 / h1);
          else if (i <= K + ng)
            a += (1.0 / hg);
          else
            a += (1.0 / h);
        }
      }
      precision[i * k + j] = precision[j * k + i] = a;
      // printf("%f %d %d \n",precision[i*k+j],i,j);
    }
  }
  return precision;
}

/*
 * Marginal log-likelihood under the product-moment non-local prior.
 *
 * The caller provides the precision matrix and its Cholesky factor.  This
 * routine finds the posterior mode of beta under the non-local prior penalty
 * and uses a Laplace-style approximation to the integrated likelihood.
 */
double log_likelihood_nonlocal(
    int k, int K, int ng, int N, double alpha, double psi, double *y,
    double **design, double *precision, const gsl_matrix *chol_precision,
    double *beta_mode, int r, double h, double h1, double h0, double hg,
    int max_iter, double tolerance, _Bool positive_beta)
{
  double *adjusted_precision = malloc(k * k * sizeof(double));
  double *xty = malloc(k * sizeof(double));
  int i, j, l;
  double a;
  for (i = 0; i < k; i++)
  {
    a = 0;
    for (l = 0; l < N; l++)
    {
      if (positive_beta == 0)
      {
        if (i == 0)
          a += y[l];
        else
          a += design[l][i - 1] * y[l];
      }
      else
      { // positive_beta==1 and without constant
        a += design[l][i] * y[l];
      }
    }
    xty[i] = a;
  }

  gsl_vector_view b = gsl_vector_view_array(xty, k);

  gsl_vector *x = gsl_vector_alloc(k);

  gsl_linalg_cholesky_solve(chol_precision, &b.vector, x);

  double nu = N + 2 * r * k + 2 * alpha;
  double s2 = 0;
  double beta_hat[k];

  for (i = 0; i < k; i++)
  {
    beta_hat[i] = gsl_vector_get(x, i);
    s2 += xty[i] * beta_hat[i];
  }
  gsl_vector_free(x);
  double yy = 0;
  for (i = 0; i < N; i++)
  {
    yy += pow(y[i], 2);
  }
  s2 = (2 * psi + yy - s2) / nu;
  if (positive_beta == 0)
  {
    for (i = 0; i < k; i++)
    {
      beta_mode[i] = beta_hat[i];
    }
  }
  else
  {
    for (i = 0; i < k; i++)
    {
      beta_mode[i] = beta_hat[i] * (beta_hat[i] > 0);
    }
  }
  maximize_nonlocal_beta(xty, nu, s2, precision, max_iter, tolerance, beta_mode, k, r, positive_beta);
  for (i = 0; i < k; i++)
  {
    for (j = 0; j <= i; j++)
    {
      adjusted_precision[i * k + j] = adjusted_precision[j * k + i] = precision[i * k + j];
      if (i == j)
        adjusted_precision[i * k + i] += (2 * r * nu * s2 / (nu - 2)) * (1 / pow(beta_mode[i], 2));
    }
  }
  double L1 = gsl_sf_lngamma(nu / 2) + (alpha * log(psi)) + (nu / 2) * log(2);
  if (positive_beta == 1)
    L1 += k * log(2);
  double betaAibeta = 0;
  double sumlogbeta = 0;
  double difbetaAibeta = 0;
  for (i = 0; i < k; i++)
  {
    sumlogbeta += log(pow(beta_mode[i], 2));
    for (j = 0; j < i; j++)
    {
      betaAibeta += 2 * precision[i * k + j] * beta_hat[i] * beta_hat[j];
      difbetaAibeta += 2 * precision[i * k + j] * (beta_hat[i] - beta_mode[i]) * (beta_hat[j] - beta_mode[j]);
    }
    betaAibeta += pow(beta_hat[i], 2) * precision[i * k + i];
    difbetaAibeta += pow(beta_hat[i] - beta_mode[i], 2) * precision[i * k + i];
  }

  double L2 = -(nu / 2) * log(2 * psi + yy - betaAibeta) + r * sumlogbeta;
  double L3 = -((nu - 2) / (2 * nu * s2)) * difbetaAibeta;

  gsl_matrix_view adjusted_precision_view = gsl_matrix_view_array(adjusted_precision, k, k);
  gsl_linalg_cholesky_decomp(&adjusted_precision_view.matrix);
  double L4 = -0.5 * cholesky_logdet(&adjusted_precision_view.matrix);
  double doublefact = factorial_int(2 * r - 1) / ((1 << (r - 1)) * factorial_int(r - 1));
  double L5 = -gsl_sf_lngamma(alpha) - k * log(doublefact) - (N / 2.0) * log(2 * IMR_PI) - ((k - K - ng - 1) / 2.0 + r * (k - K - ng - 1)) * log(h) - (K / 2.0 + r * K) * log(h1) - (ng / 2.0 + r * ng) * log(hg) - (0.5 + r) * log(h0);
  double log_likelihood = L1 + L2 + L3 + L4 + L5;
  free(xty);
  free(adjusted_precision);
  return (log_likelihood);
}

/*
 * Coordinate ascent for the beta mode induced by the product-moment prior.
 * `beta_mode` is both the starting point and the output mode.
 */
void maximize_nonlocal_beta(
    double *xty, double nu, double s2, double *precision, int max_iter,
    double tolerance, double *beta_mode, int k, int r, _Bool positive_beta)
{
  int i, m, m1;
  double a = (nu * s2) / (nu - 2);
  int converged = 0;
  double *beta = malloc(k * sizeof(double));
  for (i = 0; i < k; i++)
  {
    beta[i] = beta_mode[i];
  }
  i = 0;
  while ((i < max_iter) && (converged == 0))
  {
    for (m = 0; m < k; m++)
    {
      double am = precision[m * k + m];
      double precision_beta_without_m = 0;
      if (k > 0)
      {
        for (m1 = 0; m1 < k; m1++)
          if (m1 != m)
            precision_beta_without_m += precision[m * k + m1] * beta[m1];
      }
      double delta = pow(precision_beta_without_m - xty[m], 2) + (8 * r * a * am);
      double f1 = (-precision_beta_without_m + xty[m] + sqrt(delta)) / (2 * am);
      beta[m] = f1;
      if (positive_beta == 0)
      {
        double f2 = -2 * r * a / (am * f1);
        double objective_difference = beta_mode[m] * ((f1 - f2) / (f1 * f2)) - log(pow(f1, 2)) + log(pow(f2, 2));
        if (objective_difference > 0)
          beta[m] = f2;
      }
    } // end of m
    double beta_delta[k];
    for (m1 = 0; m1 < k; m1++)
    {
      beta_delta[m1] = beta_mode[m1] - beta[m1];
    }

    if ((norm(k, beta_delta) / (norm(k, beta) + norm(k, beta_mode))) < tolerance)
      converged = 1;
    for (m1 = 0; m1 < k; m1++)
    {
      beta_mode[m1] = beta[m1];
    }
    i++;
  }
  free(beta);
}
