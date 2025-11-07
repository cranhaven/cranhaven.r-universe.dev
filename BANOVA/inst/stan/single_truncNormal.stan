data {
  int<lower=0> N;
  int<lower=0> J;
  matrix[N, J] X;
  int<lower = 0, upper = 1> no_lower_bound; 
  int<lower = 0, upper = 1> no_upper_bound; 
  real L; // Lower bound for y values
  real U; // Upper bound for y values
  real <lower = L, upper=U> y[N];
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
  
  if (no_lower_bound)
    for (i in 1:N){
      y[i] ~ normal(y_hat[i], tau_y) T[, U];
    }
  else if (no_upper_bound)
    for (i in 1:N){
      y[i] ~ normal(y_hat[i], tau_y) T[L, ];
    }
  else
    for (i in 1:N){
      y[i] ~ normal(y_hat[i], tau_y) T[L, U];
    }
    
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

