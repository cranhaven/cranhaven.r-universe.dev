data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of dependent variables
  int<lower=0> J; // number of subject-level effects

  vector[J] X[N]; // array of size N which contains J-long vectors 
  vector[L] y[N]; // response variable
}

parameters {
  matrix[L, J] beta1;              // Subject-level effects
  cholesky_factor_corr[L] L_Omega; // Cholesky factor of the correlation matrix
  vector<lower=0>[L] L_sigma;      // Standard Deviations of y
  
} 

transformed parameters {
  vector[L] y_hat[N];        //mean of the distribution of y
  matrix[L, L] L_Sigma;     // covariance matrix
  
   for (n in 1:N) {
     for (l in 1:L) {
       y_hat[n, l] = dot_product(X[n], beta1[l]);
     }
   }
   L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
}

model {
  //Priors
  to_vector(beta1) ~ normal(0, 100);
  L_Omega ~ lkj_corr_cholesky(1);
  L_sigma ~ cauchy(0, 2.5);
  
  y ~ multi_normal_cholesky(y_hat, L_Sigma);
}

generated quantities {
  real var_f[L];
  real r_2[L];
  matrix[L, L] Omega; // Correlation matrix
  matrix[L, L] Sigma; // Covariance matrix
  
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  Sigma = quad_form_diag(Omega, L_sigma); 
  for (l in 1:L){
    var_f[l] = variance(y_hat[,l]);
    r_2[l] = var_f[l]/(var_f[l] + Sigma[l,l]); 
  }
}
