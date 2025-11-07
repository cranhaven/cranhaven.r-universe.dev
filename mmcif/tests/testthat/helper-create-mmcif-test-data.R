# assign model parameters
n_causes <- 2L
delta <- 2

# set the betas
coef_risk <- matrix(c(.67, 1, .1, -.4, .25, .3), ncol = n_causes)

# set the gammas
coef_traject <- matrix(c(-.8, -.45, .8, .4, -1.2, .15, .25, -.2), ncol = n_causes)

# set the covariance matrix
Sigma <-
  matrix(c(0.306, 0.008, -0.138, 0.197, 0.008, 0.759, 0.251,
           -0.25, -0.138, 0.251, 0.756, -0.319, 0.197, -0.25, -0.319, 0.903),
         2L * n_causes)

test_data_file <- "mmcif-test-data.RDS"
if(!file.exists(test_data_file)){
  library(mvtnorm)
  sim_dat <- function(n_clusters, max_cluster_size){
    stopifnot(max_cluster_size > 0,
              n_clusters > 0)

    cluster_id <- 0L
    out <- replicate(n_clusters, simplify = FALSE, {
      n_obs <- sample.int(max_cluster_size, 1L)
      cluster_id <<- cluster_id + 1L

      # draw the covariates and the left truncation time
      covs <- cbind(a = rnorm(n_obs), b = runif(n_obs, -1))
      Z <- cbind(1, covs)

      delayed_entry <- pmax(runif(n_obs, -1), 0)
      cens <- rep(-Inf, n_obs)
      while(all(cens <= delayed_entry))
        cens <- runif(n_obs, max = 3 * delta)

      successful_sample <- FALSE
      while(!successful_sample){
        rng_effects <- drop(rmvnorm(1, sigma = Sigma))
        U <- head(rng_effects, n_causes)
        eta <- tail(rng_effects, n_causes)

        # draw the cause
        cond_logits_exp <-
          cbind(exp(Z %*% coef_risk + rep(U, each = n_obs)), 1)
        cond_probs <- cond_logits_exp / rowSums(cond_logits_exp)
        cause <- apply(cond_probs, 1,
                       function(prob)
                         sample.int(n_causes + 1L, 1L, prob = prob))

        # compute the observed time if needed
        obs_time <- mapply(function(cause, idx){
          if(cause > n_causes)
            return(delta)

          # can likely be done smarter but this is more general
          coefs <- coef_traject[, cause]
          offset <- sum(Z[idx, ] * coefs[-1]) + eta[cause]
          rng <- runif(1)
          eps <- .Machine$double.eps
          root <- uniroot(
            function(x) rng - pnorm(
              -coefs[1] * atanh((x - delta / 2) / (delta / 2)) - offset),
            c(eps^2, delta * (1 - eps)), tol = 1e-12)$root
        }, cause, 1:n_obs)

        keep <- which(pmin(obs_time, cens) > delayed_entry)
        successful_sample <- length(keep) > 0
        if(!successful_sample)
          next

        has_finite_trajectory_prob <- cause <= n_causes
        is_censored <- which(!has_finite_trajectory_prob | cens < obs_time)

        if(length(is_censored) > 0){
          obs_time[is_censored] <- pmin(delta, cens[is_censored])
          cause[is_censored] <- n_causes + 1L
        }
      }

      data.frame(covs, cause, time = obs_time, cluster_id, delayed_entry)[keep, ]
    })
    do.call(rbind, out)
  }

  set.seed(1)
  saveRDS(sim_dat(100, 5), test_data_file)
}

dat <- readRDS(test_data_file)
