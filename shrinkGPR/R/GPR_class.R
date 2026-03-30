# Create nn_module subclass that implements forward methods for GPR
GPR_class <- nn_module(
  classname = "GPR",
  initialize = function(y,
                        x,
                        x_mean,
                        a = 0.5,
                        c = 0.5,
                        a_mean = 0.5,
                        c_mean = 0.5,
                        sigma2_rate = 10,
                        n_layers,
                        flow_func,
                        flow_args,
                        kernel_func = shrinkGPR::kernel_se,
                        device) {

    # Add dimension attribute
    # size of x +1 for the variance term + 1 for global shrinkage parameter
    # + size of x_mean, if provided
    self$d <- ncol(x) + 2
    self$mean_zero <- TRUE
    if (!missing(x_mean) & !is.null(x_mean)) {
      # +1 for global shrinkage parameter
      self$d <- self$d + ncol(x_mean) + 1
      self$mean_zero <- FALSE
    }

    self$N <- nrow(x)

    # Add kernel attribute
    self$kernel_func <- kernel_func

    # Add device attribute, set to GPU if available
    if (missing(device)) {
      if (cuda_is_available()) {
        self$device <- torch_device("cuda")
      } else {
        self$device <- torch_device("cpu")
      }
    } else {
      self$device <- device
    }

    # Add softplus function for positive parameters
    self$beta_sp <- 0.7
    self$softplus <- nn_softplus(beta = self$beta_sp, threshold = 20)

    flow_args <- c(d = self$d, flow_args)

    # Add flow parameters
    self$n_layers <- n_layers
    self$layers <- nn_module_list()

    for (i in 1:n_layers) {
      self$layers$append(do.call(flow_func, flow_args))
    }

    self$layers$to(device = self$device)

    # Create forward method
    self$model <- nn_sequential(self$layers)

    # Add data to the model
    # Unsqueezing y to add a dimension - this enables broadcasting
    self$y <- y$to(device = self$device)$unsqueeze(2)
    self$x <- x$to(device = self$device)
    if (!self$mean_zero) {
      self$x_mean <- x_mean$to(device = self$device)
    } else {
      self$x_mean <- NULL
    }

    #create holders for prior a, c, lam and rate
    self$prior_a <- torch_tensor(a, device = self$device, requires_grad = FALSE)
    self$prior_c <- torch_tensor(c, device = self$device, requires_grad = FALSE)
    self$prior_a_mean <- torch_tensor(a_mean, device = self$device, requires_grad = FALSE)
    self$prior_c_mean <- torch_tensor(c_mean, device = self$device, requires_grad = FALSE)
    self$prior_rate <- torch_tensor(sigma2_rate, device = self$device, requires_grad = FALSE)
  },

  # Unnormalised log likelihood for Gaussian Process
  ldnorm = function(K, sigma2, beta) {
    log_lik <- .shrinkGPR_internal$jit_funcs$ldnorm(
      K = K,
      sigma2 = sigma2,
      y = self$y,
      x_mean = self$x_mean,
      beta = beta
    )
    return(log_lik)
  },

  # Unnormalised log density of triple gamma prior
  ltg = function(x, a, c, lam) {
    res <-  - 0.5 * torch_log(lam$unsqueeze(2)) -
      0.5 * torch_log(x) +
      log_hyperu(c + 0.5, 1.5 - a, a*x/(c * lam$unsqueeze(2)))

    return(res)
  },

  # Unnormalised log density of normal-gamma-gamma prior
  ngg = function(x, a, c, lam) {
    res <- - 0.5 * torch_log(lam$unsqueeze(2)) +
      log_hyperu(c + 0.5, 1.5 - a,  a * x^2/(c * lam$unsqueeze(2)))

    return(res)
  },

  # Unnormalised log density of exponential distribution
  lexp = function(x, rate) {
    return(torch_log(rate) - rate * x)
  },

  # Unnormalised log density of F distribution
  ldf = function(x, d1, d2) {
    res <- (d1 * 0.5 - 1.0) * torch_log(x) - (d1 + d2) * 0.5 *
      torch_log1p(d1 / d2 * x)

    return(res)
  },

  # Forward method for GPR
  forward = function(zk) {
    log_det_J <- 0

    for (layer in 1:self$n_layers) {
      layer_out <- self$layers[[layer]]$forward(zk)
      log_det_J <- log_det_J + layer_out$log_diag_j
      zk <- layer_out$zk
    }

    # logdet jacobian of softplus transformation
    if (self$mean_zero) {
      log_det_J <- log_det_J + self$beta_sp * torch_sum(zk - self$softplus(zk), dim = 2)
      zk <- self$softplus(zk)
    } else {
      log_det_J <- log_det_J + self$beta_sp * torch_sum(zk[, 1:(self$x$shape[2] + 2)] - self$softplus(zk[, 1:(self$x$shape[2] + 2)]), dim = 2)
      l2_sigma_lam <- self$softplus(zk[, 1:(self$x$shape[2] + 2)])

      log_det_J <- log_det_J + self$beta_sp * (zk[, -1] - self$softplus(zk[, -1]))
      lam_mean <- self$softplus(zk[, -1])

      zk <- torch_cat(list(l2_sigma_lam,
                           zk[, (self$x$shape[2] + 3):(self$x$shape[2] + self$x_mean$shape[2] + 2)],
                           lam_mean$unsqueeze(2)),
                      dim = 2)
    }

    return(list(zk = zk, log_det_J = log_det_J))
  },

  gen_batch = function(n_latent) {
    # Generate a batch of samples from the model
    z <- torch_randn(n_latent, self$d, device = self$device)
    return(z)
  },

  elbo = function(zk_pos, log_det_J) {
    # Extract the components of the variational distribution
    # Convention:
    # First x$shape[2] components are the theta parameters
    # Next component is the sigma parameter
    # Next component is the lambda parameter
    # Next x_mean$shape[2] components are the mean parameters
    # Last component is the lambda parameter for the mean
    l2_zk <- zk_pos[, 1:self$x$shape[2]]
    sigma_zk <- zk_pos[, (self$x$shape[2] + 1)]
    lam_zk <- zk_pos[, (self$x$shape[2] + 2)]

    if (!self$mean_zero) {
      beta <- zk_pos[, (self$x$shape[2] + 3):(self$x$shape[2] + 2 + self$x_mean$shape[2])]
      lam_mean <- zk_pos[, -1]
    } else {
      beta <- NULL
    }

    # Calculate covariance matrix
    K <- self$kernel_func(l2_zk, lam_zk, self$x)

    # Calculate the components of the ELBO
    likelihood <- self$ldnorm(K, sigma_zk, beta)$mean()

    prior <- self$ltg(l2_zk, self$prior_a, self$prior_c, lam_zk)$sum(dim = 2)$mean() +
      self$ldf(lam_zk, 2*self$prior_c, 2*self$prior_a)$mean() +
      self$lexp(sigma_zk, self$prior_rate)$mean()

    if (!self$mean_zero) {
      prior <- prior + self$ngg(beta, self$prior_a_mean, self$prior_c_mean, lam_mean)$sum(dim = 2)$mean() +
        self$ldf(lam_mean, 2*self$prior_c_mean, 2*self$prior_a_mean)$mean()
    }

    var_dens <- log_det_J$mean()

    # Compute ELBO
    elbo <- likelihood + prior + var_dens

    if (torch_isnan(elbo)$item()) {
      stop("ELBO is NaN")
    }

    return(elbo)
  },

  # Method to calculate moments of predictive distribution
  calc_pred_moments = function(x_new, nsamp, x_mean_new) {

    with_no_grad({
      N_new = x_new$shape[1]

      # First, generate posterior draws by drawing random samples from the variational distribution.
      z <- self$gen_batch(nsamp)
      zk_pos <- self$forward(z)$zk
      zk_pos <- res_protector_autograd(zk_pos)

      l2_zk <- zk_pos[, 1:self$x$shape[2]]
      sigma_zk <- zk_pos[, (self$x$shape[2] + 1)]
      lam_zk <- zk_pos[, (self$x$shape[2] + 2)]

      if (!self$mean_zero) {
        beta <- zk_pos[, (self$x$shape[2] + 3):(self$x$shape[2] + 2 + self$x_mean$shape[2])]
      } else {
        beta <- NULL
      }

      # Calculate covariance matrix K and transform into L and alpha
      # L is the cholseky decomposition of K + sigma^2I, i.e. the covariance matrix of the GP
      # alpha is the solution to L L^T alpha = y, i.e. (K + sigma^2I)^{-1}y

      K <- self$kernel_func(l2_zk, lam_zk, self$x)
      single_eye <- torch_eye(self$N, device = self$device)
      batch_sigma2 <- single_eye$`repeat`(c(nsamp, 1, 1)) *
        sigma_zk$unsqueeze(2)$unsqueeze(2)
      L <- robust_chol(K + batch_sigma2, upper = FALSE)

      if (self$mean_zero) {
        alpha <- torch_cholesky_solve(self$y, L, upper = FALSE)
      } else {
        y_demean <- (self$y - torch_matmul(self$x_mean, beta$t()))$t()$unsqueeze(3)
        alpha <- torch_cholesky_solve(y_demean, L, upper = FALSE)
      }

      # Calculate K_star_star, the covariance between the test data
      K_star_star <- self$kernel_func(l2_zk, lam_zk, x_new)

      # Calculate K_star, the covariance between the training and test data
      K_star_t <- self$kernel_func(l2_zk, lam_zk, self$x, x_new)

      # Calculate the predictive mean and variance
      if (self$mean_zero) {
        pred_mean <- torch_bmm(K_star_t, alpha)$squeeze()
      } else {
        pred_mean <- torch_bmm(K_star_t, alpha)$squeeze() +
          torch_matmul(x_mean_new, beta$t())$t()$squeeze()
      }

      single_eye_new <- torch_eye(N_new, device = self$device)
      batch_sigma2_new <- single_eye_new$`repeat`(c(nsamp, 1, 1)) *
        sigma_zk$unsqueeze(2)$unsqueeze(2)
      v <- linalg_solve_triangular(L, K_star_t$permute(c(1, 3, 2)), upper = FALSE)
      pred_var <- K_star_star - torch_matmul(v$permute(c(1, 3, 2)), v) + batch_sigma2_new

      return(list(pred_mean = pred_mean, pred_var = pred_var))
    })
  },

  predict = function(x_new, nsamp, x_mean_new) {

    with_no_grad({
      N_new <- x_new$shape[1]

      # Calculate the moments of the predictive distribution
      pred_moments <- self$calc_pred_moments(x_new, nsamp, x_mean_new)
      pred_mean <- pred_moments$pred_mean
      pred_var <- pred_moments$pred_var

      pred_var_chol <- robust_chol(pred_var, upper = FALSE)
      eps <- torch_randn(nsamp, N_new, 1, device = self$device)

      pred_samples <- pred_mean$unsqueeze(1) + torch_bmm(pred_var_chol, eps)$squeeze()

      return(pred_samples$squeeze())
    })

  },

  # Method to evaluate predictive density
  eval_pred_dens = function(y_new, x_new, nsamp, x_mean_new = NULL, log = FALSE) {

    with_no_grad({
      pred_moments <- self$calc_pred_moments(x_new, nsamp, x_mean_new)
      pred_mean <- pred_moments$pred_mean
      pred_var <- pred_moments$pred_var$squeeze()

      ldnorm <- distr_normal(pred_mean, torch_sqrt(pred_var))

      log_dens <- ldnorm$log_prob(y_new$unsqueeze(2))$t()
      max <- torch_max(log_dens, dim = 1)
      res <- -torch_log(torch_tensor(nsamp, device = self$device)) + max[[1]] + torch_log(torch_sum(torch_exp(log_dens - max[[1]]$unsqueeze(1)), dim = 1))

      if (!log) {
        res <- torch_exp(res)
      }

      return(res)
    })

  },

  # Method to calculate LPDS
  LPDS = function(x_new, y_new, nsamp, x_mean_new = NULL) {
    res <- self$eval_pred_dens(x_new, y_new, nsamp, x_mean_new, log = TRUE)
    return(res)
  }

)
