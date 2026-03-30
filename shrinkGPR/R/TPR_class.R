# Create nn_module subclass that implements forward methods for TPR
TPR_class <- nn_module(
  classname = "TPR",
  initialize = function(y,
                        x,
                        x_mean,
                        a = 0.5,
                        c = 0.5,
                        a_mean = 0.5,
                        c_mean = 0.5,
                        sigma2_rate = 10,
                        nu_alpha = 0.5,
                        nu_beta = 2,
                        n_layers,
                        flow_func,
                        flow_args,
                        kernel_func = shrinkGPR::kernel_se,
                        device) {

    # Add dimension attribute
    # size of x +1 for the variance term + 1 for global shrinkage parameter
    # NEW for t dist: +1 for nu
    # + size of x_mean, if provided
    self$d <- ncol(x) + 3
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

    # For prior on nu
    self$nu_alpha <- torch_tensor(nu_alpha, device = self$device, requires_grad = FALSE)
    self$nu_beta <- torch_tensor(nu_beta, device = self$device, requires_grad = FALSE)
  },

  # Unnormalised log likelihood for Student-t Process
  ldt = function(K, sigma2, beta, nu) {
    log_lik <- .shrinkGPR_internal$jit_funcs$ldt(
      K = K,
      sigma2 = sigma2,
      y = self$y,
      x_mean = self$x_mean,
      beta = beta,
      nu = nu)
    return(log_lik)
  },

  # Unnormalised log density of triple gamma prior
  ltg = function(x, a, c, lam) {
    res <-  0.5 * torch_log(lam$unsqueeze(2)) -
      0.5 * torch_log(x) +
      log_hyperu(c + 0.5, 1.5 - a, a* x/(4.0 * c) * lam$unsqueeze(2))

    return(res)
  },

  # Unnormalised log density of normal-gamma-gamma prior
  ngg = function(x, a, c, lam) {
    res <- 0.5 * torch_log(lam$unsqueeze(2)) +
      log_hyperu(c + 0.5, 1.5 - a,  a * x^2/(4.0 * c) * lam$unsqueeze(2))

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

  # Unnormalised log density of gamma distribution
  ldg = function(x, alpha, beta) {
    res <- (alpha - 1.0) * torch_log(x) - beta * x
    return(res)
  },

  # Forward method for TPR
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

      log_det_J <- log_det_J + self$beta_sp * (zk[, -2] - self$softplus(zk[, -2]))
      lam_mean <- self$softplus(zk[, -2])

      # For nu
      log_det_J <- log_det_J + self$beta_sp * (zk[, -1] - self$softplus(zk[, -1]))
      nu <- self$softplus(zk[, -1])

      zk <- torch_cat(list(l2_sigma_lam,
                           zk[, (self$x$shape[2] + 3):(self$x$shape[2] + self$x_mean$shape[2] + 2)],
                           lam_mean$unsqueeze(2),
                           nu$unsqueeze(2)),
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
    # Next component is the lambda parameter for the mean
    # Last component is the nu parameter (degrees of freedom for t dist)
    l2_zk <- zk_pos[, 1:self$x$shape[2]]
    sigma_zk <- zk_pos[, (self$x$shape[2] + 1)]
    lam_zk <- zk_pos[, (self$x$shape[2] + 2)]
    nu_zk <- zk_pos[, -1] + 2 # +2 to ensure nu > 2

    if (!self$mean_zero) {
      beta <- zk_pos[, (self$x$shape[2] + 3):(self$x$shape[2] + 2 + self$x_mean$shape[2])]
      lam_mean <- zk_pos[, -2]
    } else {
      beta <- NULL
    }

    # Calculate covariance matrix
    K <- self$kernel_func(l2_zk, lam_zk, self$x)

    # Calculate the components of the ELBO
    likelihood <- self$ldt(K, sigma_zk, beta, nu_zk)$mean()

    prior <- self$ltg(l2_zk, self$prior_a, self$prior_c, lam_zk)$sum(dim = 2)$mean() +
      self$ldf(lam_zk/2, 2*self$prior_a, 2*self$prior_c)$mean() +
      self$lexp(sigma_zk, self$prior_rate)$mean() +
      self$ldg(nu_zk - 2, self$nu_alpha, self$nu_beta)$mean()

    if (!self$mean_zero) {
      prior <- prior + self$ngg(beta, self$prior_a_mean, self$prior_c_mean, lam_zk)$sum(dim = 2)$mean() +
        self$ldf(lam_mean/2, 2*self$prior_a_mean, 2*self$prior_c_mean)$mean()
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
      nu_zk <- zk_pos[, -1] + 2 # +2 to ensure nu > 2

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
        y_demean <- self$y
      } else {
        y_demean <- (self$y - torch_matmul(self$x_mean, beta$t()))$t()$unsqueeze(3)
        alpha <- torch_cholesky_solve(y_demean, L, upper = FALSE)
      }

      # Calculate K_star_star, the covariance between the test data
      single_eye <- torch_eye(N_new, device = self$device)
      batch_sigma2 <- single_eye$`repeat`(c(nsamp, 1, 1)) *
        sigma_zk$unsqueeze(2)$unsqueeze(2)
      K_star_star <- self$kernel_func(l2_zk, lam_zk, x_new) + batch_sigma2

      # Calculate K_star, the covariance between the training and test data
      K_star_t <- self$kernel_func(l2_zk, lam_zk, self$x, x_new)

      # Calculate the predictive mean and scale
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
      if (self$mean_zero) {
        pred_scale <- (K_star_star - torch_matmul(v$permute(c(1, 3, 2)), v)) *
          ((nu_zk$unsqueeze(2) + torch_mm(alpha$squeeze(3), y_demean) - 2 ) / (nu_zk + self$N - 2)$unsqueeze(2))$unsqueeze(3)

      } else {
        pred_scale <- (K_star_star - torch_matmul(v$permute(c(1, 3, 2)), v)) *
          ((nu_zk$unsqueeze(2) + torch_bmm(alpha$permute(c(1, 3, 2)), y_demean)$squeeze(3) - 2 ) /
             (nu_zk + self$N - 2)$unsqueeze(2))$unsqueeze(3)
      }

      pred_nu <- nu_zk$unsqueeze(2)$unsqueeze(3) + self$N

      return(list(pred_mean = pred_mean, pred_scale = pred_scale, pred_nu = pred_nu$squeeze()))
    })
  },

  predict = function(x_new, nsamp, x_mean_new) {

    with_no_grad({
      N_new <- x_new$shape[1]

      # Calculate the moments of the predictive distribution
      pred_moments <- self$calc_pred_moments(x_new, nsamp, x_mean_new)
      pred_mean <- pred_moments$pred_mean
      pred_scale <- pred_moments$pred_scale
      pred_nu <- pred_moments$pred_nu

      pred_scale_chol <- robust_chol(pred_scale, upper = FALSE)
      eps <- torch_randn(nsamp, N_new, 1, device = self$device)

      r <- torch_tensor(1/rgamma(nsamp, as.matrix(pred_moments$pred_nu) / 2, 1 / 2),
                        device = self$device)$view(c(nsamp, 1))

      pred_samples <- pred_mean + sqrt(r * (pred_nu$view(c(nsamp, 1)) - 2)) * torch_bmm(pred_scale_chol, eps)$squeeze()

      return(pred_samples$squeeze())
    })

  },

  # Density function of the predictive distribution
  # Note this is only univariate
  dt = function(y_test, pred_mean, pred_scale, pred_nu) {

    lpi_tens <- torch_log(torch_tensor(pi, device = self$device))

    torch_lgamma((pred_nu + 1) * 0.5) -
      0.5 * (torch_log(pred_scale) + torch_log(pred_nu - 2) + lpi_tens) -
      torch_lgamma(pred_nu * 0.5) -
      ((pred_nu + 1) * 0.5) * torch_log(1 + (y_test - pred_mean) ^ 2 / ((pred_nu - 2) * pred_scale))
  },

  # Method to evaluate predictive density
  eval_pred_dens = function(y_new, x_new, nsamp, x_mean_new = NULL, log = FALSE) {

    with_no_grad({
      pred_moments <- self$calc_pred_moments(x_new, nsamp, x_mean_new)
      pred_mean <- pred_moments$pred_mean
      pred_scale <- pred_moments$pred_scale$squeeze()
      pred_nu <- pred_moments$pred_nu

      log_dens <- self$dt(y_new$unsqueeze(2), pred_mean, pred_scale, pred_nu)$t()
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
