data {
  int<lower=0> N;
  int<lower=0> J;
  int<lower=0> M;
  int<lower=0> K;
  matrix[N, J] X;
  matrix[M, K] Z;
  int<lower=0> id[N];
  
  int<lower = 0, upper = 1> no_lower_bound; 
  int<lower = 0, upper = 1> no_upper_bound; 
  real L; // Lower bound for y values
  real U; // Upper bound for y values
  real <lower = L, upper=U> y[N];
}

parameters {
  matrix[J, M] beta1;
  matrix[K, J] beta2; 
  real<lower=0> tau_ySq;
  vector<lower=0>[J] tau_beta1Sq;
} 

transformed parameters {
  vector[N] y_hat;
  vector[N] y_pred;
  matrix[M, J] mu_beta1;
  mu_beta1 = Z*beta2;
  for (i in 1:N){
    y_hat[i] = X[i,]*beta1[,id[i]];
  }
  for (i in 1:N){
    y_pred[i] = X[i,]*mu_beta1[id[i],]';
  }
}

model {
  real tau_y;
  vector[J] tau_beta1;
  tau_y = sqrt(tau_ySq);
  tau_beta1 = sqrt(tau_beta1Sq);
  
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
  
  tau_ySq ~ inv_gamma(1, 1);
  for (i in 1:J){
    beta1[i,] ~ normal(mu_beta1[,i], tau_beta1[i]);
  }
  tau_beta1Sq ~ inv_gamma(1, 1);
  for (i in 1:J){
    beta2[,i] ~ normal(0, 100);
  }
}

generated quantities {
  real r_2;
  r_2 = variance(y_pred)/(variance(y_hat) + tau_ySq);
}

