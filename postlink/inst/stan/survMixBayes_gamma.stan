// survMixBayes_gamma.stan

data {
  // Core Data
  int<lower=1> N;                         // Number of observations
  int<lower=1> K;                         // Number of predictors
  matrix[N, K] X;                         // Predictor matrix
  vector<lower=0>[N] time;                // Survival times (strictly positive)
  int<lower=0, upper=1> event[N];         // 1 = observed event, 0 = right-censored

  // Prior Hyperparameters (Passed from R)
  // Intercept priors (applied to beta[1] only)
  real prior_intercept1_mu;
  real<lower=0> prior_intercept1_sd;
  real prior_intercept2_mu;
  real<lower=0> prior_intercept2_sd;

  // Slope priors (applied to beta[2:K])
  real prior_beta1_mu;
  real<lower=0> prior_beta1_sd;
  real prior_beta2_mu;
  real<lower=0> prior_beta2_sd;

  real<lower=0> prior_phi1_alpha;
  real<lower=0> prior_phi1_beta;
  real<lower=0> prior_phi2_alpha;
  real<lower=0> prior_phi2_beta;

  real<lower=0> prior_theta_alpha;
  real<lower=0> prior_theta_beta;
}

parameters {
  // Model Parameters
  real<lower=0, upper=1> theta;     // Mixing proportion for component 1
  vector[K] beta1;                  // Regression coefficients for component 1
  vector[K] beta2;                  // Regression coefficients for component 2
  real<lower=0> phi1;               // Shape parameter for component 1
  real<lower=0> phi2;               // Shape parameter for component 2
}

transformed parameters {
  // Vectorized Linear Predictors
  vector[N] eta1 = X * beta1;
  vector[N] eta2 = X * beta2;
}

model {
  // Priors: separate intercept and slope priors
  beta1[1] ~ normal(prior_intercept1_mu, prior_intercept1_sd);
  beta2[1] ~ normal(prior_intercept2_mu, prior_intercept2_sd);
  if (K > 1) {
    beta1[2:K] ~ normal(prior_beta1_mu, prior_beta1_sd);
    beta2[2:K] ~ normal(prior_beta2_mu, prior_beta2_sd);
  }
  phi1 ~ gamma(prior_phi1_alpha, prior_phi1_beta);
  phi2 ~ gamma(prior_phi2_alpha, prior_phi2_beta);

  theta ~ beta(prior_theta_alpha, prior_theta_beta);

  // Likelihood (with Right-Censoring)
  for (n in 1:N) {
    real lp1;
    real lp2;
    // Rate = shape / mean = phi / exp(eta) = phi * exp(-eta)
    if (event[n] == 1) {
      // Observed event: log probability density function (lpdf)
      lp1 = gamma_lpdf(time[n] | phi1, phi1 * exp(-eta1[n]));
      lp2 = gamma_lpdf(time[n] | phi2, phi2 * exp(-eta2[n]));
    } else {
      // Right-censored: log complementary cumulative distribution function (lccdf)
      lp1 = gamma_lccdf(time[n] | phi1, phi1 * exp(-eta1[n]));
      lp2 = gamma_lccdf(time[n] | phi2, phi2 * exp(-eta2[n]));
    }

    target += log_mix(theta, lp1, lp2);
  }
}

generated quantities {
  // Posterior Predictions / Mixture Assignments
  int<lower=1, upper=2> z[N];
  for (n in 1:N) {
    vector[2] lw;
    if (event[n] == 1) {
      lw[1] = log(theta) + gamma_lpdf(time[n] | phi1, phi1 * exp(-eta1[n]));
      lw[2] = log1m(theta) + gamma_lpdf(time[n] | phi2, phi2 * exp(-eta2[n]));
    } else {
      lw[1] = log(theta) + gamma_lccdf(time[n] | phi1, phi1 * exp(-eta1[n]));
      lw[2] = log1m(theta) + gamma_lccdf(time[n] | phi2, phi2 * exp(-eta2[n]));
    }

    z[n] = categorical_rng(softmax(lw));
  }
}
