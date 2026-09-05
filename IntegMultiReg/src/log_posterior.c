#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sf.h>
#include "my_header.h"
#include <math.h>

/*
 * Joint log-posterior up to constants independent of the current state.
 *
 * Components:
 *   1. subgroup marginal log-likelihoods,
 *   2. platform-specific MRF/sparsity prior for gamma,
 *   3. gamma prior on the MRF interaction parameters theta.
 */
double log_posterior(double *loglik, _Bool ***gamma, double *nu, double ***theta,
               double *mrf, double alpha0, double ***betaTh, int nbrsugbroups,
               int n_platforms, int *G, int *n_platform_models_c)
{
    double logPost = 0;
    int g, m, m1;
    for (m = 0; m < nbrsugbroups; m++)
    {
        logPost += loglik[m];
    }
    /// Prior gamma
    double logPostGam = 0;
    for (int l = 0; l < n_platforms; l++)
    {
        double sumGX = 0;
        double thetGamX = 0;
        for (m = 0; m < n_platform_models_c[l]; m++)
        {
            for (g = 0; g < G[l]; g++)
            {
                sumGX += gamma[l][m][g];
                for (m1 = 0; m1 < m; m1++)
                {
                    thetGamX += 2 * (gamma[l][m][g] * gamma[l][m1][g] * theta[l][m][m1]);
                }
            }
        }
        logPostGam += nu[l] * sumGX + thetGamX - G[l] * log(mrf[l]);
    }

    // Prior theta
    double logPriorT = 0;
    for (int l = 0; l < n_platforms; l++)
    {
        double logPriorTX = 0;
        double sumTX = 0;
        for (m = 0; m < n_platform_models_c[l]; m++)
        {
            for (m1 = 0; m1 < m; m1++)
            {
                if (theta[l][m][m1] > pow(10, -3))
                    logPriorTX += log(theta[l][m][m1]);
                sumTX += theta[l][m][m1] * betaTh[l][m][m1];
            }
        }
        logPriorTX = (alpha0 - 1) * logPriorTX + sumTX;
        logPriorT += logPriorTX;
    }
    return logPost + logPostGam + logPriorT;
}
