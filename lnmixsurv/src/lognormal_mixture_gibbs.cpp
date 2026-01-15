// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-

#include <RcppArmadillo.h>
#include <RcppGSL.h>
#include <RcppParallel.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

using namespace Rcpp;

// Importing the RcppParallelLibs Function from RcppParallel Package to NAMESPACE
//' @importFrom RcppParallel RcppParallelLibs
 
// ------ RNG Framework ------

// Function to initialize the GSL random number generator with MT19937
void initializeRNG(const long long int& seed, gsl_rng*& rng_device) {
    rng_device = gsl_rng_alloc(gsl_rng_mt19937);  // Allocate MT19937 RNG
    gsl_rng_set(rng_device, seed);  // Set the seed
}

// Function used to set a seed
void setSeed(const long long int& seed, gsl_rng*& rng_device) {
    if (rng_device == nullptr) {
        initializeRNG(seed, rng_device);  // Initialize if not already done
    } else {
        gsl_rng_set(rng_device, seed);  // Reset the seed if already initialized
    }
}

// Squares a double
double square(const double& x) {
  return x * x;
}

// Generates a random observation from Uniform(0, 1) 
double runif_0_1(gsl_rng* rng_device) {
  return gsl_rng_uniform(rng_device);
}

// Generates a random observation from Normal(mu, sd^2)
double rnorm_(const double& mu, const double& sd, gsl_rng* rng_device) {
  return gsl_ran_gaussian(rng_device, sd) + mu;
}

// Generates a random observation from Gamma(alpha, beta), with mean alpha/beta
double rgamma_(const double& alpha, const double& beta, gsl_rng* rng_device) {
  return gsl_ran_gamma(rng_device, alpha, 1.0 / beta);
}

// Sample one value (k-dimensional) from a 
// Dirichlet(alpha_1, alpha_2, ..., alpha_k)
arma::vec rdirichlet(const arma::vec& alpha, gsl_rng* rng_device) {
  int K = alpha.n_elem;
  arma::vec sample(K);
  
  for (int k = 0; k < K; ++k) {
    sample(k) = rgamma_(alpha(k), 1.0, rng_device);
  }
  
  sample /= arma::sum(sample);
  return sample;
}

// Generates a random observation from a MultivariateNormal(mean, covariance)
arma::vec rmvnorm(const arma::vec& mean, const arma::mat& covariance, gsl_rng* rng_device) {
  int numDims = mean.n_elem;
  arma::vec sample(numDims);
  
  arma::mat L = arma::chol(covariance, "lower");
  
  arma::vec Z(numDims);
  
  for (int j = 0; j < numDims; j++) {
    Z(j) = rnorm_(0.0, 1.0, rng_device);
  }
  
  sample = mean + L * Z;
  return sample;
}

/* AUXILIARY FUNCTIONS */

// Function to make a square matrix symmetric
arma::mat makeSymmetric(const arma::mat& A) {
  return (0.5 * (A + A.t()));
}

// Creates a sequence from start to end with 1 step
arma::ivec seq(const int& start, const int& end) {
  arma::vec out_vec = arma::linspace<arma::vec>(start, end, end - start + 1);
  return arma::conv_to<arma::ivec>::from(out_vec);
}

// Function for replicating a numeric value K times.
arma::vec repl(const double& x, const int& times) {
  return arma::ones<arma::vec>(times) * x;
}

// Sample a random object from a given vector
// Note: it just samples numeric objects (because of c++ class definition) and just one object per time.
int numeric_sample(const arma::ivec& groups,
                   const arma::vec& probs, gsl_rng* rng_device) {
  double u = runif_0_1(rng_device);
  double cumulativeProb = 0.0;
  int n = probs.n_elem;
  for (int i = 0; i < n; ++i) {
    cumulativeProb += probs(i);
    
    if (u <= cumulativeProb) {
      return groups(i);
    }
  }
  
  // This point should never be reached and it's here just for compiling issues
  return 0;
}

void sample_groups_advanced(const int& G, const arma::vec& y, const arma::vec& eta, 
                            const arma::vec& sd, const arma::mat& beta,
                            const arma::mat& X, gsl_rng* rng_device,
                            arma::ivec& vec_groups) {
  // Initializing variables used for sampling groups
  int n = X.n_rows;
  int p = X.n_cols;
  arma::ivec sequence = seq(0, G - 1);
  arma::uvec indexg;
  arma::mat Vg0 = arma::diagmat(repl(30.0, p));
  arma::mat identity_p = arma::eye(p, p);
  arma::mat Vg0_inv = arma::solve(Vg0, identity_p, arma::solve_opts::likely_sympd);
  arma::vec xi;
  arma::rowvec xit;
  arma::mat Xg;
  arma::mat Xgt;
  arma::mat Xgt_Xg;
  arma::vec yg;
  arma::mat S_inv(p, p);
  arma::mat S(p, p);
  arma::vec Xgt_yg;
  arma::vec Mg;
  arma::mat denom_mat(n, G);
  arma::vec probsEqual(G);
  probsEqual.fill(1.0 / G);
  arma::vec phi = 1.0 / arma::square(sd);
  
  double m;
  double yi;
  double a;
  double sigma2;
  double denom;
  
  for (int g = 0; g < G; g++) {
    indexg = arma::find(vec_groups == g); // find indexes of which observations belongs to group g
    
    if(indexg.empty()) {
      continue; // continue to next group because if there's no one at group g, a lot of computational problem is going to happen next
    }
    
    Xg = X.rows(indexg);
    Xgt = Xg.t();
    yg = y(indexg);
    Xgt_yg = Xgt * yg;
    S = phi(g) * Xg.t() * Xg + Vg0_inv;
    
    if(arma::det(S) == 0) {
      S_inv = Vg0;
    } else {
      if (arma::det(makeSymmetric(S)) < 1e-10) { // regularization if matrix is poorly conditioned
        S += 1e-8 * arma::eye(p, p);
      }

      S_inv = arma::solve(makeSymmetric(S), identity_p, 
                          arma::solve_opts::likely_sympd);
    }
    
    Mg = X * S_inv * Xgt_yg;
    
    for (int i = 0; i < n; i++) {
      xit = X.row(i);
      yi = y(i);
      m = 1.0 / arma::as_scalar(xit * S_inv * xit.t());
      a = (m - phi(g)) * (vec_groups(i) == g) +
        m * (vec_groups(i) != g);
      
      sigma2 = (a + phi(g))/(a * phi(g));
      
      denom_mat(i, g) = 
        eta(g) * (R::dnorm(yi,
                  square(phi(g)) * sigma2 * (Mg(i) - yi / (a + phi(g))),
                  sqrt(sigma2), 
                  false) * (vec_groups(i) == g) +
                    R::dnorm(yi,
                             square(phi(g)) * sigma2 * ((a + phi(g)) * Mg(i)/(a + phi(g) + 1.0)),
                             sqrt(sigma2),
                             false) * (vec_groups(i) != g));
      
      if(g == (G - 1)) { // if it's the last group
        denom = arma::sum(denom_mat.row(i));
        
        if(denom > 0) {
          vec_groups(i) = numeric_sample(sequence, denom_mat.row(i).t() / denom, 
                     rng_device);
        } else {
          vec_groups(i) = numeric_sample(sequence, probsEqual, rng_device);
        }
      }
    }
  }
}

void sample_groups_fast(const int& G, const arma::vec& y, const arma::vec& eta, 
                        const arma::vec& sd, const arma::mat& beta,
                        const arma::mat& X, gsl_rng* rng_device, arma::ivec& vec_groups) {
  arma::vec probs(G);
  arma::mat mean = X * beta.t();
  
  double denom;
  int n = y.n_elem;
  
  for (int i = 0; i < n; i++) {
    denom = 0.0;
    
    for (int g = 0; g < G; g++) {
      probs(g) = eta(g) * R::dnorm(y(i), mean(i, g), sd(g), false);
      denom += probs(g); 
    }
    
    probs = (denom == 0) * (repl(1.0 / G, G)) + (denom != 0) * (probs / denom);
    
    vec_groups(i) = numeric_sample(seq(0, G - 1), probs, rng_device);
  }
}

// Function used to sample the latent groups for each observation.
arma::ivec sample_groups(const int& G, const arma::vec& y, const arma::vec& eta, 
                         const arma::vec& phi, const arma::mat& beta,
                         const arma::mat& X, gsl_rng* rng_device,
                         const arma::ivec& groups_old, const bool& fast_groups) {
  
  arma::ivec vec_groups = groups_old;
  arma::vec sd = 1.0 / sqrt(phi);
  
  if(fast_groups) {
    sample_groups_fast(G, y, eta, sd, beta, X, rng_device, vec_groups);
  } else {
    sample_groups_advanced(G, y, eta, sd, beta, X, rng_device, vec_groups);
  }
  
  return vec_groups;
}

// Function used to sample random groups for each observation proportional to the eta parameter
arma::ivec sample_groups_start(const int& G, const arma::vec& y, 
                               const arma::vec& eta, gsl_rng* rng_device) {
  int n = y.n_rows;
  arma::ivec vec_groups(n);
  
  for (int i = 0; i < n; i++) {
    vec_groups(i) = numeric_sample(seq(0, G - 1), eta, rng_device);
  }
  
  return(vec_groups);
}

double augment_yi(const double& yi, const double& mean, const double& sd, gsl_rng* rng_device) {
  double out_i = yi; // value to augment
  int count = 0; // avoid infite loop
  
  while(out_i <= yi) { // ensure that the sampled observation is bigger than the censored one
    out_i = rnorm_(mean, sd, rng_device);
    
    // break if it seems like it's going to run forever
    // should never happen
    if(count > 10000) {
      out_i = yi + 0.01;
      break;
    }
    
    count ++;
  }
  
  return out_i;
}

// Function used to simulate survival time for censored observations.
// Here, delta is a vector such that delta(i) == 0 implies that the 
// i-th observation is censored. Otherwise, the i-th observation is a
// failure time.
arma::vec augment(const int& G, const arma::vec& y, const arma::ivec& groups,
                  const arma::ivec& delta, const arma::vec& sd, 
                  const arma::mat& beta, const arma::mat& X, gsl_rng* rng_device) {
  arma::vec out = y;
  arma::mat mean = X * beta.t(); // pre-compute the mean matrix
  arma::uvec censored_indexes = arma::find(delta == 0); // finding which observations are censored
  
  for (int i : censored_indexes) {
    out(i) = augment_yi(y(i), arma::as_scalar(mean(i, groups(i))), sd(groups(i)), rng_device);
  }
  
  return out;
}

// Create a table for each numeric element in the vector groups.
// For now, we are just going to use to evaluate how much observations
// we have at each group, given a vector of latent groups.
arma::ivec groups_table(const int& G, const arma::ivec& groups) {
  arma::ivec out(G);
  arma::ivec index;
  for (int g = 0; g < G; g++) {
    index = groups(arma::find(groups == g));
    out(g) = index.n_rows;
  }
  
  return out;
}

/* Auxiliary functions for EM algorithm */

// Compute weights matrix
arma::mat compute_W(const arma::vec& y, const arma::mat& X, const arma::vec& eta, 
                    const arma::mat& beta, const arma::vec& sigma, 
                    const int& G, const int& n, double& denom, arma::mat& mat_denom, const arma::rowvec& repl_vec) {
  arma::mat out(n, G);
  
  for(int g = 0; g < G; g++) {
    mat_denom.col(g) = eta(g) * arma::normpdf(y,
                  X * beta.row(g).t(),
                  repl(sigma(g), n));
  }
  
  for(int i = 0; i < n; i++) {
    denom = arma::sum(mat_denom.row(i));
    if(denom > 0) {
      out.row(i) = mat_denom.row(i) / denom;
    } else {
      out.row(i) = repl_vec;
    }
  }
  
  return out;
}

// Function used to computed the expected value of a truncated normal distribution
double compute_expected_value_truncnorm(const double& alpha, const double& mean, const double& sigma) {
  double out;
  
  if (R::pnorm(alpha, 0.0, 1.0, true, false) < 1.0) {
    out = mean + sigma *
      (R::dnorm(alpha, 0.0, 1.0, false)/(R::pnorm(alpha, 0.0, 1.0, false, false)));
  } else {
    out = mean + sigma *
      (R::dnorm(alpha, 0.0, 1.0, false)/0.0001);
  }
  
  return out;
}

// Create the latent variable z for censored observations
arma::vec augment_em(const arma::vec& y, const arma::uvec& censored_indexes,
                     const arma::mat& X, const arma::mat& beta,
                     const arma::vec& sigma, const arma::mat& W,
                     const int& G, const arma::mat& mean,
                     const int& n) {
  arma::vec out = y;
  arma::mat alpha_mat(n, G);
  
  for(int g = 0; g < G; g++) {
    alpha_mat.col(g) = (y - mean.col(g))/sigma(g);
  }
  
  for (int i : censored_indexes) {
    out(i) = 0.0;
    
    for (int g = 0; g < G; g++) {
      out(i) += W(i, g) * compute_expected_value_truncnorm(arma::as_scalar(alpha_mat(i, g)), arma::as_scalar(mean(i, g)), sigma(g));
    }
  }
  
  return out;
}

// Function used to sample groups from W. It samples one group by row based on the max weight.
arma::ivec sample_groups_from_W(const arma::mat& W, const int& N) {
  arma::vec out(N);
  
  for(int r = 0; r < N; r++) {
    out(r) = W.row(r).index_max();
  }
  
  return(arma::conv_to<arma::ivec>::from(out));
}

// Sample initial values for the EM parameters
void sample_initial_values_em(arma::vec& eta, arma::vec& phi, arma::mat& beta, arma::vec& sd, const int& G, const int& k, gsl_rng* rng_device) {
  eta = rdirichlet(repl(rgamma_(1.0, 1.0, rng_device), G), rng_device);
  
  for (int g = 0; g < G; g++) {
    phi(g) = rgamma_(0.1, 0.1, rng_device);
    
    for (int c = 0; c < k; c++) {
      beta(g, c) = rnorm_(0.0, 20.0, rng_device);
    }
  }
  
  sd = 1.0 / sqrt(phi);
}

// Update the matrix beta for the group g
void update_beta_g(const arma::vec& colg, const arma::mat& X, const int& g, const arma::vec& z, arma::mat& beta,
                   arma::sp_mat& Wg) {
  Wg = arma::diagmat(colg);
  arma::mat S = X.t() * Wg * X; 

  if(arma::det(makeSymmetric(S)) < 1e-10) { // regularization if matrix is poorly conditioned
    S += 1e-8 * arma::eye(S.n_cols, S.n_cols);
  }

  beta.row(g) = arma::solve(makeSymmetric(S), X.t() * Wg * z, arma::solve_opts::likely_sympd).t();
}

// Update the parameter phi(g)
void update_phi_g(const double& denom, const arma::uvec& censored_indexes, const arma::mat& X, const arma::vec& colg, const arma::vec& y, const arma::vec& z,
                  const arma::vec& sd, const arma::mat& beta, const arma::vec& var, const int& g, const int& n, arma::vec& phi, gsl_rng* rng_device,
                  double& alpha, double& quant) {
  alpha = 0.0;
  quant = arma::as_scalar(arma::square(z - (X * beta.row(g).t())).t() * colg);
  
  for(int i : censored_indexes) {
    alpha = (y(i) - arma::as_scalar(X.row(i) * beta.row(g).t())) / sd(g);
    
    if(R::pnorm(alpha, 0.0, 1.0, true, false) < 1.0) {
      quant += colg(i) * var(g) * (1.0 + alpha * R::dnorm(alpha, 0.0, 1.0, false)/(R::pnorm(alpha, 0.0, 1.0, false, false)) - square(R::dnorm(alpha, 0.0, 1.0, false)/(R::pnorm(alpha, 0.0, 1.0, false, false))));
    } else {
      quant += colg(i) * var(g) * (1.0 + alpha * R::dnorm(alpha, 0.0, 1.0, false)/0.0001 - square(R::dnorm(alpha, 0.0, 1.0, false)/0.0001));
    }
  }
  
  // to avoid numerical problems
  if (quant == 0.0) {
    phi(g) = rgamma_(0.5, 0.5, rng_device); // resample phi
  } else {
    phi(g) = denom / quant;
  }

  // to avoid numerical problems
  if(phi(g) > 1e5 || phi.has_nan()) {
    phi(g) = rgamma_(0.5, 0.5, rng_device); // resample phi
  }
}

// Update the model parameters with EM
void update_em_parameters(const int& n, const int& G, arma::vec& eta, arma::mat& beta, arma::vec& phi, const arma::mat& W, const arma::mat& X, 
                          const arma::vec& y, const arma::vec& z, const arma::uvec& censored_indexes, const arma::vec& sd, gsl_rng* rng_device,
                          double& quant, double& denom, double& alpha, arma::sp_mat& Wg, arma::vec& colg) {
  arma::vec var = arma::square(sd);
  
  for (int g = 0; g < G; g++) {
    colg = W.col(g);
    
    eta(g) = arma::sum(colg) / n; // updating eta(g)
    
    if (arma::any(eta == 0.0)) { // if there's a group with no observations
      eta = rdirichlet(repl(1.0, G), rng_device);
    }

    update_beta_g(colg, X, g, z, beta, Wg); // updating beta for the group g
    update_phi_g(arma::sum(colg), censored_indexes, X, colg, y, z, sd, beta, var, g, n, phi, rng_device, alpha, quant);
  }
}

// Compute model's log-likelihood to select the EM initial values
double loglik_em(const arma::vec& eta, const arma::vec& sd, const arma::mat& W, const arma::vec& z, const int& G, const int& N, const arma::mat& mean, const arma::uvec& censored_indexes) {
  double loglik = 0.0;
  
  for(int i = 0; i < N; i++) {
    if(arma::any(censored_indexes == i)) {
      for (int g = 0; g < G; g++) {
        if (eta(g) * R::pnorm((z(i) - mean(i, g))/sd(g), 0.0, 1.0, false, false) == 0.0) {
          loglik += W(i, g) * log(0.00001);
        } else {
          loglik += W(i, g) * log(eta(g) * R::pnorm((z(i) - mean(i, g))/sd(g), 0.0, 1.0, false, false));
        }
      }
    } else {
      for(int g = 0; g < G; g++) {
        if (eta(g) * R::dnorm(z(i), arma::as_scalar(mean(i, g)), sd(g), false) == 0.0) {
          loglik += W(i, g) * log(0.00001);
        } else {
          loglik += W(i, g) * log(eta(g) * R::dnorm(z(i), arma::as_scalar(mean(i, g)), sd(g), false));
        }
      }
    }
  }
  
  return loglik;
}

// EM for the lognormal mixture model.
arma::field<arma::mat> lognormal_mixture_em(const int& Niter, const int& G, const arma::vec& t, const arma::ivec& delta, const arma::mat& X,
                                            const bool& better_initial_values, const int& N_em,
                                            const int& Niter_em, const bool& internal, const bool& show_output, gsl_rng* rng_device) {
  
  int n = X.n_rows;
  int k = X.n_cols;
  double quant, denom, alpha;

  // initializing objects used on EM algorithm
  arma::vec y = log(t);
  arma::vec eta(G);
  arma::vec phi(G);
  arma::vec sd(G);
  arma::vec z(n);
  arma::mat W(n, G);
  arma::mat beta(G, k);
  arma::mat mean(n, k);
  arma::mat out(Niter, G * k + (G * 2));
  arma::uvec censored_indexes = arma::find(delta == 0); // finding which observations are censored
  arma::vec colg(n);
  arma::sp_mat Wg;
  arma::field<arma::mat> out_internal_true(6);
  arma::field<arma::mat> out_internal_false(2);
  arma::field<arma::mat> em_params(6);
  arma::field<arma::mat> best_em(6);
  arma::mat mat_denom(n, G);
  arma::rowvec repl_vec = repl(1.0 / G, G).t();
  
  for(int iter = 0; iter < Niter; iter++) {
    if(iter == 0) { // sample starting values
      
      if(better_initial_values) {
        for (int init = 0; init < N_em; init ++) {
          em_params = lognormal_mixture_em(Niter_em, G, t, delta, X, false, 0, 0, true, false, rng_device);
          
          if(init == 0) {
            best_em = em_params;
            if(show_output) {
              Rcout << "Initial LogLik: " << arma::as_scalar(best_em(5)) << "\n";
            }
          } else {
            if(arma::as_scalar(em_params(5)) > arma::as_scalar(best_em(5))) { // comparing logliks
              if(show_output) {
                Rcout << "Previous maximum: " << arma::as_scalar(best_em(5)) << " | New maximum: " << arma::as_scalar(em_params(5))  << "\n";
              }
              best_em = em_params;
            }
          }
        }
        
        eta = best_em(0);
        beta = best_em(1);
        phi = best_em(2);
        W = best_em(3);
        if(show_output) {
          Rcout << "Starting EM with better initial values" << "\n";
        }
      } else {
        sample_initial_values_em(eta, phi, beta, sd, G, k, rng_device);
        W = compute_W(y, X, eta, beta, sd, G, n, denom, mat_denom, repl_vec);
      }
      
    } else {
      mean = X * beta.t();
      sd = 1.0 / sqrt(phi);
      z = augment_em(y, censored_indexes, X, beta, sd, W, G, mean, n);
      W = compute_W(z, X, eta, beta, sd, G, n, denom, mat_denom, repl_vec);
      update_em_parameters(n, G, eta, beta, phi, W, X, y, z, censored_indexes, sd, rng_device, quant, denom, alpha, Wg, colg);

      if(show_output) {
        if((iter + 1) % 20 == 0) {
          Rcout << "EM Iter: " << (iter + 1) << " | " << Niter << "\n";
        }
      }
    }
    
    // Fill the out matrix
    arma::rowvec newRow = 
      arma::join_rows(eta.row(0), 
                      beta.row(0),
                      phi.row(0));
    
    for (int g = 1; g < G; g++) {
      newRow = 
        arma::join_rows(newRow, 
                        eta.row(g), 
                        beta.row(g),
                        phi.row(g));
    }
    
    out.row(iter) = newRow;
  }
  
  mean = X * beta.t();
  
  if(internal) {
    out_internal_true(0) = eta;
    out_internal_true(1) = beta;
    out_internal_true(2) = phi;
    out_internal_true(3) = W;
    out_internal_true(4) = augment_em(y, censored_indexes, X, beta, 1.0 / sqrt(phi), W, G, mean, n);
    out_internal_true(5) = loglik_em(eta, 1.0 / sqrt(phi), compute_W(y, X, eta, beta, 1.0 / sqrt(phi), G, n, denom, mat_denom, repl_vec), y, G, n, mean, censored_indexes);
    
    return out_internal_true;
  } else {
    out_internal_false(0) = out;
    out_internal_false(1) = loglik_em(eta, 1.0 / sqrt(phi), compute_W(y, X, eta, beta, 1.0 / sqrt(phi), G, n, denom, mat_denom, repl_vec), y, G, n, mean, censored_indexes);
    
    return out_internal_false;
  }
  
  return out_internal_false; // should never be reached
}

// Setting parameter's values for the first Gibbs iteration
void first_iter_gibbs(const arma::field<arma::mat>& em_params, arma::vec& eta, arma::mat& beta, arma::vec& phi, const int& em_iter, const int& G, const arma::vec& y,
                      arma::vec& sd, arma::ivec& groups, const arma::mat& X, const bool& use_W, const arma::ivec& delta, gsl_rng* rng_device, const bool& fast_groups) {
  arma::ivec groups_start(y.n_rows);
  int p = X.n_cols;
  if (em_iter != 0) {
    // we are going to start the values using the last EM iteration
    eta = em_params(0);
    beta = em_params(1);
    phi = em_params(2);
    sd = 1.0 / sqrt(phi);
    groups_start = sample_groups_from_W(em_params(3), y.n_rows);
  } else {
    eta = rdirichlet(repl(1, G), rng_device);
    
    for (int g = 0; g < G; g++) {
      phi(g) = rgamma_(0.5, 0.5, rng_device);
      beta.row(g) = rmvnorm(repl(0.0, p),
               arma::diagmat(repl(20.0 * 20.0, p)),
               rng_device).t();
    }
    
    sd = 1.0 / sqrt(phi);
    // Sampling classes for the observations
    groups_start = sample_groups_start(G, y, eta, rng_device);
  }
  
  if(use_W == false) {
    groups = sample_groups(G, augment(G, y, groups_start, delta, sd, beta, X, rng_device), eta, sd, beta, X, rng_device, groups_start, fast_groups);
  } else {
    groups = groups_start;
  }
}

// Function to update groups
void update_groups_gibbs(const int& iter, const bool& use_W, const arma::field<arma::mat>& em_params, const int& G, const arma::vec& y_aug, 
                         const arma::vec& eta, const arma::mat& beta, const arma::vec& phi, const arma::mat& X, arma::ivec& groups, gsl_rng* rng_device,
                         const bool& fast_groups) {

  arma::ivec groups_old = groups;
  if(use_W) {
    groups = sample_groups_from_W(em_params(3), y_aug.n_rows);
  } else {
    groups = sample_groups(G, y_aug, eta, phi, beta, X, rng_device, groups_old, fast_groups);
  }
}

// Avoiding groups with zero number of observations in it (causes numerical issues)
void avoid_group_with_zero_allocation(arma::ivec& n_groups, arma::ivec& groups, const int& G, const int& N, gsl_rng* rng_device) {
  int idx = 0;
  int m;
  
  for(int g = 0; g < G; g++) {
    if(n_groups(g) == 0) {
      m = 0;
      while(m < 5) {
        idx = numeric_sample(seq(0, N),
                             repl(1.0 / N, N),
                             rng_device);
        
        if(n_groups(groups(idx)) > 5) {
          groups(idx) = g;
          m += 1;
        } 
      }
      
      // recalculating the number of groups
      n_groups = groups_table(G, groups);
    }
  }
}

double update_phi_g_gibbs(const int& n_groups_g, const arma::vec& linearComb, gsl_rng* rng_device) {
  return rgamma_(static_cast<double>(n_groups_g)  / 2.0 + 0.01, (1.0 / 2.0) * arma::as_scalar(linearComb.t() * linearComb) + 0.01, rng_device);
}

arma::rowvec update_beta_g_gibbs(const double& phi_g, const arma::mat& Xg, const arma::mat& Xgt, const arma::vec& yg, gsl_rng* rng_device) {
  arma::rowvec out;
  arma::mat comb = phi_g * Xgt * Xg + arma::diagmat(repl(1.0 / 1000.0, Xg.n_cols));
  arma::mat Sg;
  arma::vec mg;
  
  if(arma::det(comb) != 0) {
    if(arma::det(makeSymmetric(comb)) < 1e-10) { // regularization if matrix is poorly conditioned
      comb += 1e-8 * arma::eye(Xg.n_cols, Xg.n_cols);
    }
    
    Sg = arma::solve(makeSymmetric(comb),
                         arma::eye(Xg.n_cols, Xg.n_cols),
                         arma::solve_opts::likely_sympd);
    mg = phi_g * (Sg * Xgt * yg);
    out = rmvnorm(mg, Sg, rng_device).t();
  }
  
  return out;
}

// update all the Gibbs parameters
void update_gibbs_parameters(const int& G, const arma::mat& X, const arma::vec& y_aug, const arma::ivec& n_groups, const arma::ivec& groups, 
                             arma::vec& eta, arma::mat& beta, arma::vec& phi, gsl_rng* rng_device) {
  
  arma::mat Xg;
  arma::mat Xgt;
  arma::vec yg;
  arma::vec linearComb;
  arma::uvec indexg;
  
  // updating eta
  eta = rdirichlet(arma::conv_to<arma::Col<double>>::from(n_groups) + 150.0, 
                   rng_device);
  
  // For each g, sample new phi[g] and beta[g, _]
  for (int g = 0; g < G; g++) {
    indexg = arma::find(groups == g);
    Xg = X.rows(indexg);
    Xgt = Xg.t();
    yg = y_aug(indexg);
    linearComb = yg - Xg * beta.row(g).t();
    
    // updating phi(g)
    // the priori used was Gamma(0.01, 0.01)
    phi(g) = update_phi_g_gibbs(n_groups(g), linearComb, rng_device);
   
    // updating beta.row(g)
    // the priori used was MNV(vec 0, diag 1000)
    beta.row(g) = update_beta_g_gibbs(phi(g), Xg, Xgt, yg, rng_device);
  }
}

// Internal implementation of the lognormal mixture model via Gibbs sampler
arma::mat lognormal_mixture_gibbs_implementation(const int& Niter, const int& em_iter, const int& G, 
                                                 const arma::vec& t, const arma::ivec& delta, 
                                                 const arma::mat& X,
                                                 long long int starting_seed,
                                                 const bool& show_output, const int& chain_num,
                                                 const bool& use_W, const bool& better_initial_values,
                                                 const int& Niter_em, const int& N_em, const bool& fast_groups) {
  
  gsl_rng* global_rng = gsl_rng_alloc(gsl_rng_default);
  
  // setting global seed to start the sampler
  setSeed(starting_seed, global_rng);
  
  // add verifications for robustiness. Skipping for the sake of simplicity.
  
  // Calculating number of columns of the output matrix:
  // Each group has p (#cols X) covariates, 1 mixture component and
  // 1 precision. This implies:
  int p = X.n_cols;
  int nColsOutput = (p + 2) * G;
  int N = X.n_rows;
  
  arma::vec y = log(t);
  
  // The output matrix should have Niter rows (1 row for each iteration) and
  // nColsOutput columns (1 column for each element).
  arma::mat out(Niter, nColsOutput);
  
  // The order of filling the output matrix matters a lot, since we can
  // make label switching accidentally. Latter this is going to be defined
  // so we can always fill the matrix in the correct order (by columns, always).
  arma::mat Xt = X.t();
  arma::vec y_aug(N);
  arma::ivec n_groups(G);
  arma::mat means(N, G);
  arma::vec sd(G);
  
  // Starting other new values for MCMC algorithms
  arma::vec eta(G);
  arma::vec phi(G);
  arma::mat beta(G, p);
  arma::ivec groups(N);
  arma::vec log_eta_new(G);
  
  arma::rowvec newRow;
  arma::field<arma::mat> em_params(6);
  
  if(em_iter > 0) {
    // starting EM algorithm to find values close to the MLE
    em_params = lognormal_mixture_em(em_iter, G, t, delta, X, better_initial_values, N_em, Niter_em, true, false, global_rng);
  } else if(show_output) {
    Rcout << "Skipping EM Algorithm" << "\n";
  }
  
  for (int iter = 0; iter < Niter; iter++) {
    // Starting empty objects for Gibbs Sampler
    if (iter == 0) {
      first_iter_gibbs(em_params, eta, beta, phi, em_iter, G, y, sd, groups, X, use_W, delta, global_rng, fast_groups);
    }
    
    sd = 1.0 / sqrt(phi);
    
    // Data augmentation
    y_aug = augment(G, y, groups, delta, sd, beta, X, global_rng); 
    update_groups_gibbs(iter, use_W, em_params, G, y_aug, eta, beta, phi, X, groups, global_rng, fast_groups);
    
    // Computing number of observations allocated at each class
    n_groups = groups_table(G, groups);
    
    // ensuring that every class have, at least, 5 observations
    avoid_group_with_zero_allocation(n_groups, groups, G, N, global_rng);
    
    // updating all parameters
    update_gibbs_parameters(G, X, y_aug, n_groups, groups, eta, beta, phi, global_rng);
    
    // filling the ith iteration row of the output matrix
    // the order of filling will always be the following:
    
    // First Mixture: proportion, betas, phi
    // Second Mixture: proportion, betas, phi
    // ...
    // Last Mixture: proportion, betas, phi
    
    // arma::uvec sorteta = arma::sort_index(eta, "descend");
    // beta = beta.rows(sorteta);
    // phi = phi.rows(sorteta);
    // eta = eta.rows(sorteta);
    
    newRow = arma::join_rows(beta.row(0),
                             phi.row(0),
                             eta.row(0));
    for (int g = 1; g < G; g++) {
      newRow = arma::join_rows(newRow, beta.row(g),
                               phi.row(g),
                               eta.row(g));
    }
    
    out.row(iter) = newRow;
    
    if((iter % 500 == 0) && show_output) {
      Rcout << "(Chain " << chain_num << ") MCMC Iter: " << iter << "/" << Niter << "\n";
    }
  }
  
  if(show_output) {
    Rcout << "Chain " << chain_num << " finished sampling." << "\n";
  }
  
  return out;
}

struct GibbsWorker : public RcppParallel::Worker {
  const arma::vec& seeds; // starting seeds for each chain
  arma::cube& out; // store matrix iterations for each chain
  
  // other parameters used to fit the model
  const int& Niter;
  const int& em_iter;
  const int& G;
  const arma::vec& t;
  const arma::ivec& delta;
  const arma::mat& X;
  const bool& show_output;
  const bool& use_W;
  const bool& better_initial_values;
  const int& N_em;
  const int& Niter_em;
  const bool& fast_groups;
  
  // Creating Worker
  GibbsWorker(const arma::vec& seeds, arma::cube& out, const int& Niter, const int& em_iter, const int& G, const arma::vec& t,
              const arma::ivec& delta, const arma::mat& X, const bool& show_output, const bool& use_W, const bool& better_initial_values,
              const int& N_em, const int& Niter_em, const bool& fast_groups) :
    seeds(seeds), out(out), Niter(Niter), em_iter(em_iter), G(G), t(t), delta(delta), X(X), show_output(show_output), use_W(use_W), better_initial_values(better_initial_values), N_em(N_em), Niter_em(Niter_em), fast_groups(fast_groups) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      usleep(5000 * i); // avoid racing conditions
      out.slice(i) = lognormal_mixture_gibbs_implementation(Niter, em_iter, G, t, delta, X, seeds(i), show_output, i + 1, use_W, better_initial_values, Niter_em, N_em, fast_groups);
    }
  }
};

// Function to call lognormal_mixture_gibbs_implementation with parallellization
// [[Rcpp::export]]
arma::cube lognormal_mixture_gibbs(const int& Niter, const int& em_iter, const int& G,
                                   const arma::vec& t, const arma::ivec& delta, 
                                   const arma::mat& X,
                                   const arma::vec& starting_seed, const bool& show_output,
                                   const int& n_chains, const bool& use_W,
                                   const bool& better_initial_values, const int& N_em, const int& Niter_em, const bool& fast_groups) {
  arma::cube out(Niter, (X.n_cols + 2) * G, n_chains); // initializing output object
  
  // Fitting in parallel
  GibbsWorker worker(starting_seed, out, Niter, em_iter, G, t, delta, X, show_output, use_W, better_initial_values, N_em, Niter_em, fast_groups);
  RcppParallel::parallelFor(0, n_chains, worker);
  
  return out;
}

//[[Rcpp::export]]
arma::field<arma::mat> lognormal_mixture_em_implementation(const int& Niter, const int& G, const arma::vec& t,
                                              const arma::ivec& delta, const arma::mat& X, 
                                              long long int starting_seed,
                                              const bool& better_initial_values, const int& N_em,
                                              const int& Niter_em, const bool& show_output) {
  
  gsl_rng* global_rng = gsl_rng_alloc(gsl_rng_default);
  
  // setting global seed to start the sampler
  setSeed(starting_seed, global_rng);
  
  arma::field<arma::mat> out = lognormal_mixture_em(Niter, G, t, delta, X, better_initial_values, N_em, Niter_em, false, show_output, global_rng);
  
  return out;
}
