#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_matrix.h>
#include "my_header.h"

/*
 * Metropolis-Hastings update for one platform's MRF interaction matrix.
 *
 * theta[i][j] controls how strongly the same feature is encouraged to be
 * selected in subgroup i and subgroup j.  The MRF normalizing constant changes
 * when theta changes, so it is recomputed for every proposal.
 */
void sample_mrf_theta(
    int n_features, int n_models, double **theta, double **accept_theta,
    double *mrf_normalizer, _Bool **gamma, double nu, double alpha0,
    double **beta0, gsl_rng *rng)
{
    const double proposal_variance = 0.05;

    for (int i = 1; i < n_models; i++)
    {
        for (int j = 0; j < i; j++)
        {
            double theta_current = theta[i][j];
            double old_rate = MAX(theta_current / proposal_variance, 0.001);
            double old_shape = MAX(theta_current, 0.2) * old_rate;
            double theta_proposal = gsl_ran_gamma(rng, old_shape, 1 / old_rate);

            double shared_selected = 0;
            for (int g = 0; g < n_features; g++)
            {
                shared_selected += gamma[i][g] * gamma[j][g];
            }

            theta[i][j] = theta[j][i] = theta_proposal;
            double new_rate = MAX(theta_proposal / proposal_variance, 0.001);
            double new_shape = MAX(theta_proposal, 0.2) * new_rate;

            double proposed_mrf_normalizer;
            compute_mrf_normalizer(
                n_models, theta, nu, &proposed_mrf_normalizer);

            double log_accept_ratio =
                (alpha0 - 1) * (log(theta_proposal) - log(theta_current)) +
                (theta_proposal - theta_current) *
                    (2 * shared_selected - beta0[i][j]) -
                n_features *
                    (log(proposed_mrf_normalizer) - log(*mrf_normalizer));

            double accept = MIN(
                exp(log_accept_ratio +
                    log(gsl_ran_gamma_pdf(theta_current, new_shape,
                                          1 / new_rate)) -
                    log(gsl_ran_gamma_pdf(theta_proposal, old_shape,
                                          1 / old_rate))),
                1);
            double uni = gsl_ran_flat(rng, 0, 1);
            if (uni < accept)
            {
                *mrf_normalizer = proposed_mrf_normalizer;
                accept_theta[i][j] += 1;
                accept_theta[j][i] = accept_theta[i][j];
            }
            else
            {
                theta[i][j] = theta[j][i] = theta_current;
            }
        }
    }
}
