// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-

#include <RcppArmadillo.h>
#include <RcppGSL.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

using namespace Rcpp;
 
 // ------ RNG Framework ------

 // Function to initialize the GSL random number generator
 void initializeRNG_simulate_data(const long long int& seed, gsl_rng* rng_device) {
   gsl_rng_set(rng_device, seed);
 }

// Function used to set a seed
void setSeed_simulate_data(const long long int& seed, gsl_rng* rng_device) {
  initializeRNG_simulate_data(seed, rng_device);
}

// Generates a random observation from Normal(mu, sd^2)
double rnorm_simulate_data(const double& mu, const double& sd, gsl_rng* rng_device) {
  return gsl_ran_gaussian(rng_device, sd) + mu;
}

// [[Rcpp::export]]
arma::vec simulate_y(const arma::mat& X, const arma::mat& beta, const arma::vec& phi, const arma::ivec& delta, const arma::ivec& groups, long long int starting_seed) {
  // Initialize the random number generator
  gsl_rng* global_rng = gsl_rng_alloc(gsl_rng_default);
  setSeed_simulate_data(starting_seed, global_rng);
  
  arma::vec sd = 1.0 / arma::sqrt(phi);
  int n = X.n_rows;
  arma::vec out(n);
  arma::mat means = X * beta.t();
  double out_i;

  for(int i = 0; i < n; i++) {
    out(i) = rnorm_simulate_data(means(i, groups(i) - 1), sd(groups(i) - 1), global_rng);

    if(delta(i) == 0) { // if it's a censored observation
      out_i = out(i);
      while(out_i >= out(i)) {
        out_i = rnorm_simulate_data(means(i, groups(i) - 1), sd(groups(i) - 1), global_rng);
      }

      out(i) = out_i;
    }
  }

  return out;
}
