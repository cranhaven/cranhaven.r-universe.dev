// glmMixBayes_poisson.stan

data {
  // Core Data
  int<lower=1> N;             // Number of observations
  int<lower=0> y[N];          // Poisson response variable (counts)
  int<lower=1> K;             // Number of predictors
  matrix[N, K] X;             // Predictor matrix

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

  real<lower=0> prior_theta_alpha;
  real<lower=0> prior_theta_beta;
}

parameters {
  // Model Parameters
  real<lower=0, upper=1> theta; // Mixing proportions (constrained to sum to 1)
  vector[K] beta1;              // Regression coefficients for component 1
  vector[K] beta2;              // Regression coefficients for component 2
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
  theta ~ beta(prior_theta_alpha, prior_theta_beta);

  // Likelihood
  for (n in 1:N) {
    target += log_mix(
      theta,
      poisson_log_lpmf(y[n] | eta1[n]),
      poisson_log_lpmf(y[n] | eta2[n])
    );
  }
}

generated quantities {
  // Posterior Predictions / Mixture Assignments
  int<lower=1, upper=2> z[N];
  for (n in 1:N) {
    vector[2] lw;
    lw[1] = log(theta) + poisson_log_lpmf(y[n] | eta1[n]);
    lw[2] = log1m(theta) + poisson_log_lpmf(y[n] | eta2[n]);
    z[n] = categorical_rng(softmax(lw));
  }
}
