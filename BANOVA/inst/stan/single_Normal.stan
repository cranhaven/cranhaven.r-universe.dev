data {
  int<lower=0> N;
  int<lower=0> J;
  matrix[N, J] X;
  vector[N] y;
}

parameters {
  vector[J] beta1;
  real<lower=0> tau_ySq;
}

transformed parameters {
  vector[N] y_hat;
  y_hat = X*beta1;
}

model {
  real tau_y;
  vector[J] tau_beta1;
  tau_y = sqrt(tau_ySq);
  y ~ normal(y_hat, tau_y);
  tau_y ~ cauchy(0, 2.5);
  for (i in 1:J){
    beta1[i] ~ normal(0, 100);
  }
}

generated quantities {
  real var_f;
  real r_2;
  var_f = variance(y_hat);
  r_2 = var_f/(var_f + tau_ySq);
}

