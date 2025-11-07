data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of dependent variables
  int<lower=0> J; // number of subject-level effects
  
  int<lower=0> M; // number of unique subjects
  int<lower=0> K; // number of population-level effects
  
  vector[J] X[N]; // subject level regressors
  vector[K] Z[M]; // population level regressors
  vector[L] y[N]; // response variables
  
  int<lower=0> id[N];
}

parameters {
  matrix[L, J] beta1[M]; // subject-level effects. Array of size L, each element is an M by J matrix
  matrix[K, J] beta2[L]; // population-level effects
  
  cholesky_factor_corr[L] L_Omega; // Cholesky factor of the correlation matrix of y
  vector<lower=0>[L] L_sigma;      // Standard Deviations of y
  
  matrix<lower=0>[L, J] tau_beta1Sq; // Variance of beta1
} 

transformed parameters {
  vector[L] y_hat[N];              // means for the distribution in y
  matrix[L, J] mu_beta1[M];        // means of elements in beta1
  vector[L] y_pred[N];             // predictions for the dependent variables in y
  matrix[L, L] L_Sigma;            // element of choleski decomposition of the covariance matrix
  matrix<lower=0>[L, J] tau_beta1; // standard deviation for beta1
  
  for (n in 1:N) {
     for (l in 1:L) {
       y_hat[n, l] = dot_product(X[n], beta1[id[n],l]);
     }
   }
   
  for (m in 1:M){
    for (j in 1:J){
       for (l in 1:L){
         mu_beta1[m, l, j] = dot_product(Z[m], beta2[l, 1:K, j]);
      }
    }
  } 
  
  for (n in 1:N) {
    for (l in 1:L) {
      y_pred[n, l] = dot_product(X[n], mu_beta1[id[n],l]);
    }
  }

  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  
  tau_beta1 = sqrt(tau_beta1Sq);
  
}

model {
  // Priors 
  for (m in 1:M){
    for (j in 1:J){
       for (l in 1:L){
         beta1[m, l, j] ~ normal(mu_beta1[m, l, j], tau_beta1[l,j]);
      }
    }
  }
  L_Omega ~ lkj_corr_cholesky(1);
  L_sigma ~ cauchy(0, 2.5);
  to_vector(tau_beta1Sq) ~ inv_gamma(1, 1);
  for(l in 1:L){
    to_vector(beta2[l]) ~ normal(0, 100);
  }
  
  
  for (n in 1:N){
    y[n] ~ multi_normal_cholesky(y_hat[n], L_Sigma);
  }
}


generated quantities {
  real r_2[L];
  matrix[L, L] Omega; // Correlation matrix
  matrix[L, L] Sigma; // Covariance matrix
  
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  Sigma = quad_form_diag(Omega, L_sigma); 
  for (l in 1:L){
    r_2[l] =  variance(y_pred[1:N, l])/(variance(y_hat[1:N, l]) + Sigma[l,l]); 
  }
}
