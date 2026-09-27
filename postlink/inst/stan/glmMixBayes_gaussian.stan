// glmMixBayes_gaussian.stan

data {
  // Core Data
  int<lower=1> N;             // Number of data points
  int<lower=1> K;             // Number of predictors
  matrix[N, K] X;             // Predictor matrix
  vector[N] y;                // Response vector

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

  real prior_sigma1_loc;
  real<lower=0> prior_sigma1_scale;
  real prior_sigma2_loc;
  real<lower=0> prior_sigma2_scale;

  real<lower=0> prior_theta_alpha;
  real<lower=0> prior_theta_beta;
}

parameters {
  // Model Parameters
  real<lower=0, upper=1> theta; // Mixture weight for the first component
  real<lower=0> sigma1;         // Standard deviation of the first component
  real<lower=0> sigma2;         // Standard deviation of the second component
  vector[K] beta1;              // Regression coefficients for the first component
  vector[K] beta2;              // Regression coefficients for the second component
}

transformed parameters {
  // Vectorized Linear Predictors
  vector[N] mu1 = X * beta1;
  vector[N] mu2 = X * beta2;
}

model {
  // Priors: separate intercept and slope priors
  beta1[1] ~ normal(prior_intercept1_mu, prior_intercept1_sd);
  beta2[1] ~ normal(prior_intercept2_mu, prior_intercept2_sd);
  if (K > 1) {
    beta1[2:K] ~ normal(prior_beta1_mu, prior_beta1_sd);
    beta2[2:K] ~ normal(prior_beta2_mu, prior_beta2_sd);
  }
  sigma1 ~ cauchy(prior_sigma1_loc, prior_sigma1_scale);
  sigma2 ~ cauchy(prior_sigma2_loc, prior_sigma2_scale);

  theta ~ beta(prior_theta_alpha, prior_theta_beta);

  // Likelihood
  for (n in 1:N) {
    target += log_sum_exp(
      log(theta) + normal_lpdf(y[n] | mu1[n], sigma1),
      log1m(theta) + normal_lpdf(y[n] | mu2[n], sigma2)
    );
  }
}

generated quantities {
  // Posterior Predictions / Mixture Assignments
  int<lower=1, upper=2> z[N];

  for (n in 1:N) {
    vector[2] lw;
    lw[1] = log(theta) + normal_lpdf(y[n] | mu1[n], sigma1);
    lw[2] = log1m(theta) + normal_lpdf(y[n] | mu2[n], sigma2);
    z[n] = categorical_rng(softmax(lw));
  }
}
