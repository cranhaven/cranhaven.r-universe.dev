# compute OLS prediction from resp ~ pred - 1
project_ols <- function(resp, pred) {
  as.matrix(lm(resp ~ pred - 1)$fitted.values)
}

# compute OLS coefficients from resp ~ pred - 1
get_ols_coefs <- function(resp, pred) {
  as.matrix(lm(resp ~ pred - 1)$coefficients)
}

# returns the functions used to compute the conditional expectations
# and specifies if several of them are splines. If several are splines,
# the computation of the conditional expectations can be performed in
# a faster way.
get_condexp_funcs <- function(cond_method, params) {
  # IF some of the nuisance parameters are estimated with splines and
  # have the same additional parameters stored in params, THEN estimation can
  # be performed in a faster way.
  all_condexp_spline <- (cond_method == "spline")
  params_identical <- FALSE
  if (sum(all_condexp_spline) == 3) {
    params_identical <-
      identical(params[[1]], params[[2]]) &&
      identical(params[[1]], params[[3]]) &&
      identical(params[[2]], params[[3]])
  } else if ((sum(all_condexp_spline) >= 2) && !params_identical) {
    params_identical <- rep(FALSE, 3)
    names(params_identical) <- c("1-2", "1-3", "2-3")
    for (i in seq_len(3)) {
      test_params <- as.numeric(strsplit(names(params_identical)[i], "-")[[1]])
      if (!is.atomic(cond_method[[test_params[1]]]) ||
          !is.atomic(cond_method[[test_params[2]]])) {
        next
      }
      params_identical[i] <-
        identical(params[[test_params[1]]], params[[test_params[2]]]) &&
        (cond_method[[test_params[1]]] == "spline") &&
        (cond_method[[test_params[2]]] == "spline")
    }
  }

  # get functions to estimate conditional expectations
  if (is.atomic(cond_method)) {
    condexp_aa <- get(paste("condexp_", cond_method[1], sep = ""))
    condexp_xx <- get(paste("condexp_", cond_method[2], sep = ""))
    condexp_yy <- get(paste("condexp_", cond_method[3], sep = ""))
  } else {
    condexp_aa <- cond_method[[1]]
    if (is.atomic(condexp_aa)) {
      condexp_aa <- get(paste("condexp_", condexp_aa, sep = ""))
    }
    condexp_xx <- cond_method[[2]]
    if (is.atomic(condexp_xx)) {
      condexp_xx <- get(paste("condexp_", condexp_xx, sep = ""))
    }
    condexp_yy <- cond_method[[3]]
    if (is.atomic(condexp_yy)) {
      condexp_yy <- get(paste("condexp_", condexp_yy, sep = ""))
    }
  }

  list(params_identical = params_identical,
       all_condexp_spline = all_condexp_spline,
       condexp_aa = condexp_aa,
       condexp_xx = condexp_xx,
       condexp_yy = condexp_yy)
}

# computes the residuals on index set I with conditional
# expectations fitted on index set Ic
residuals_samplesplit <- function(aa, ww, xx, yy, I, Ic,
                                  cond_func_all, params) {

  # split data into two folds
  ww_fit <- ww[Ic, , drop = FALSE]
  aa_fit <- aa[Ic, , drop = FALSE]
  xx_fit <- xx[Ic, , drop = FALSE]
  yy_fit <- yy[Ic, , drop = FALSE]
  ww_predict <- ww[I, , drop = FALSE]
  aa_predict <- aa[I, , drop = FALSE]
  xx_predict <- xx[I, , drop = FALSE]
  yy_predict <- yy[I, , drop = FALSE]
  dim_X <- dim(xx_fit)
  nn <- dim_X[1]
  d <- dim_X[2]

  # compute conditional expectations. If all 3 conditional expectations
  # are fitted with splines, a shortcut is possible. This is encoded by
  # the argument cond_func_all$all_condexp_spline. Additionally,
  # it needs to be checked if the additional sets of parameters are equal,
  # which is done via the variable cond_func_all$params_identical.
  if ((sum(cond_func_all$all_condexp_spline) == 3) &&
      (length(cond_func_all$params_identical) == 1) &&
      cond_func_all$params_identical[1]) {
    egW_hat_all <- condexp_spline(aa_fit = aa_fit,
                                  xx_fit = xx_fit,
                                  yy_fit = yy_fit,
                                  ww_fit = ww_fit,
                                  ww_predict = ww_predict,
                                  params = params[[1]])
    eAgW_hat <- egW_hat_all$eAgW_hat
    eXgW_hat <- egW_hat_all$eXgW_hat
    eYgW_hat <- egW_hat_all$eYgW_hat
  } else if (sum(cond_func_all$all_condexp_spline) >= 2 &&
             sum(cond_func_all$params_identical) == 1) {
    if (cond_func_all$params_identical[1]) {
      egW_hat_all <- condexp_spline(aa_fit = aa_fit,
                                    xx_fit = xx_fit,
                                    ww_fit = ww_fit,
                                    ww_predict = ww_predict,
                                    params = params[[1]])
      eAgW_hat <- egW_hat_all$eAgW_hat
      eXgW_hat <- egW_hat_all$eXgW_hat
      eYgW_hat <- cond_func_all$condexp_yy(yy_fit = yy_fit, ww_fit = ww_fit,
                                           ww_predict = ww_predict,
                                           params = params[[3]])
    } else if (cond_func_all$params_identical[2]) {
      egW_hat_all <- condexp_spline(aa_fit = aa_fit,
                                    yy_fit = yy_fit,
                                    ww_fit = ww_fit,
                                    ww_predict = ww_predict,
                                    params = params[[1]])
      eAgW_hat <- egW_hat_all$eAgW_hat
      eYgW_hat <- egW_hat_all$eYgW_hat
      eXgW_hat <- cond_func_all$condexp_xx(yy_fit = xx_fit,
                                           ww_fit = ww_fit,
                                           ww_predict = ww_predict,
                                           params = params[[2]])
    } else if (cond_func_all$params_identical[3]) {
      egW_hat_all <- condexp_spline(xx_fit = xx_fit,
                                    yy_fit = yy_fit,
                                    ww_fit = ww_fit,
                                    ww_predict = ww_predict,
                                    params = params[[2]])
      eXgW_hat <- egW_hat_all$eXgW_hat
      eYgW_hat <- egW_hat_all$eYgW_hat
      eAgW_hat <- cond_func_all$condexp_aa(yy_fit = aa_fit,
                                           ww_fit = ww_fit,
                                           ww_predict = ww_predict,
                                           params = params[[1]])
    }
  } else {
    eAgW_hat <- cond_func_all$condexp_aa(yy_fit = aa_fit, ww_fit = ww_fit,
                                         ww_predict = ww_predict,
                                         params = params[[1]])
    eXgW_hat <- cond_func_all$condexp_xx(yy_fit = xx_fit, ww_fit = ww_fit,
                                         ww_predict = ww_predict,
                                         params = params[[2]])
    eYgW_hat <- cond_func_all$condexp_yy(yy_fit = yy_fit, ww_fit = ww_fit,
                                         ww_predict = ww_predict,
                                         params = params[[3]])
  }

  # compute residuals
  rA <- aa_predict - eAgW_hat
  rX <- xx_predict - eXgW_hat
  rY <- yy_predict - eYgW_hat

  # compute projected residuals
  # rX_gW = OLS rX ~ rA; rY_gW = OLS rY ~ rA
  list(rX = rX,
       rY = rY,
       rA = rA,
       rX_gW = tryCatch_WEM(project_ols(resp = rX, pred = rA),
                            matrix(NA, ncol = d, nrow = nn))$value,
       rY_gW = tryCatch_WEM(project_ols(resp = rY, pred = rA),
                            matrix(NA, ncol = 1, nrow = nn))$value)
}

# computes all residual terms needed for DML and the regularization methods.
# This function performs the sample splitting step, where the observations
# are splitted into K sets of equal size if possible.
beta_get_matrices <- function(aa, ww, xx, yy, K, gamma, DML, do_DML, do_regDML,
                              cond_func_all, params) {
  # dimensions of the data
  n <- length(yy)

  # the residual quantities Ra, Rx, and Ry need to be stored to estimate the
  # asymptotic variance-covariance matrices afterwards
  all_residuals <- all_residuals_gW <- vector(mode = "list", length = K)
  n_reorder <- sample(seq_len(n), n, replace = FALSE)
  folds <- if (K >= 2) {
    cut(n_reorder, breaks = K, labels = FALSE)
  } else {
    # If K == 1, have same train and test set.
    rep(1, n)
  } # end folds

  for (kk in seq_len(K)) {
    I <- seq_len(n)[folds == kk] # test set: evaluate conditional expectations
    Ic <- if (K >= 2) { # training set: estimate conditional expectations
      setdiff(seq_len(n), I)
    } else {
      seq_len(n) # if K == 1, have same train and test set
    } # end Ic

    # train conditional expectations with Ic, and evaluate residuals on I.
    # cond_func_all = learning method for conditional expectations
    # params = additional parameters to estimate conditional expectations
    residuals_samplesplit_hat <-
      residuals_samplesplit(aa = aa, ww = ww,
                            xx = xx, yy = yy,
                            I = I, Ic = Ic,
                            cond_func_all = cond_func_all,
                            params = params)

    all_residuals[[kk]] <- residuals_samplesplit_hat[c("rA", "rX", "rY")]
    all_residuals_gW[[kk]] <- residuals_samplesplit_hat[c("rX_gW", "rY_gW")]
  } # end for (kk in seq_len(K))

  # return results
  list(all_residuals = all_residuals,
       all_residuals_gW = all_residuals_gW)
}

# estimates linear coefficient beta_0 with DML1: first estimate K individual
# estimators, then average them.
get_beta_DML1 <- function(all_residuals_gW) {
  K <- length(all_residuals_gW)
  d <- dim(all_residuals_gW[[1]]$rX_gW)[2]

  func_tmp <- function(k, all_residuals_gW, d) {
    tryCatch_WEM(get_ols_coefs(pred = all_residuals_gW[[k]]$rX_gW,
                               resp = all_residuals_gW[[k]]$rY_gW),
                 matrix(NA, nrow = d, ncol = 1))$value
  }
  cbind(apply(do.call(cbind,
                      lapply(seq_len(K),
                             func_tmp,
                             all_residuals_gW = all_residuals_gW,
                             d = d)),
              1, mean))
}

# estimates beta_0 with DML2: average quantities first, then compute
# one final estimator with the aggregated quantities
get_beta_DML2 <- function(all_residuals_gW) {
  K <- length(all_residuals_gW)
  dim_rX <- dim(all_residuals_gW[[1]]$rX_gW)
  d <- dim_rX[2]
  n <- dim_rX[1]
  mat <- matrix(0, nrow = d, ncol = d)
  vec <- matrix(0, nrow = d, ncol = 1)

  for (k in seq_len(K)) {
    mat <- mat + crossprod(all_residuals_gW[[k]]$rX_gW, all_residuals_gW[[k]]$rX_gW) / n
    vec <- vec + crossprod(all_residuals_gW[[k]]$rX_gW, all_residuals_gW[[k]]$rY_gW) / n
  }

  tryCatch_WEM(qr.solve(mat, vec), matrix(NA, nrow = d, ncol = 1))$value
}

# helper function to estimate beta_0 with regularized DML1 method:
# for some given gamma value, compute the DML1-version of the regularized
# parameter. That is first compute K individual estimators, and then aggregate.
# This function only computes one of the K individual estimators for
# one given value of gamma. The K corresponds to the number of sample splits.
combine_regDML1_regular_ols <- function(gamma, rX, rX_gW, rY, rY_gW, n) {
  rX_gamma <- rX + (sqrt(gamma) - 1) * rX_gW
  tryCatch_WEM(get_ols_coefs(pred = rX_gamma,
                             resp = rY + (sqrt(gamma) - 1) * rY_gW),
               matrix(NA, ncol = 1, nrow = ncol(as.matrix(rX_gamma))))$value
}

# estimate beta_0 with regularized DML1 method:
# for some given gamma value, compute the DML1-version of the regularized
# parameter. That is first compute K individual estimators, and then aggregate.
get_beta_regDML1 <- function(all_residuals, all_residuals_gW, gamma) {
  K <- length(all_residuals_gW)
  dim_rX <- dim(all_residuals_gW[[1]]$rX_gW)
  d <- dim_rX[2]
  n <- dim_rX[1]
  gammalen <- length(gamma)
  beta_regDML1 <- matrix(0, nrow = d, ncol = gammalen)

  for (k in seq_len(K)) {
    beta_regDML1 <-
      beta_regDML1 +
      tryCatch_WEM(do.call(cbind,
                           lapply(gamma,
                                  combine_regDML1_regular_ols,
                                  rX = all_residuals[[k]]$rX,
                                  rX_gW = all_residuals_gW[[k]]$rX_gW,
                                  rY = all_residuals[[k]]$rY,
                                  rY_gW = all_residuals_gW[[k]]$rY_gW,
                                  n = n)),
                   matrix(NA, nrow = d, ncol = gammalen))$value
  }
  beta_regDML1 / K
}

# helper function to estimate beta_0 with regularized DML2 method:
# for some given value of gamma, compute the quantities of one of the K
# aggregation steps. These quantities are aggregated in the main function
# to form the final regularized estimator
get_mat_vec_regDML2 <- function(gamma, rX, rX_gW, rY, rY_gW, n) {
  rX_gamma <- rX + (sqrt(gamma) - 1) * rX_gW

  list(mat =  crossprod(rX_gamma, rX_gamma) / n,
       vec =  crossprod(rX_gamma, rY + (sqrt(gamma) - 1) * rY_gW) / n)
}

# estimate beta_0 with regularized DML2 method:
# Compute the regularized estimators for all gamma values with the DML2 method.
# That is, first aggregate the K individual quantities, and then compute
# the final estimator. A final estimator is computed for all values of gamma.
get_beta_regDML2 <- function(all_residuals, all_residuals_gW, gamma) {
  K <- length(all_residuals_gW)
  dim_rX <- dim(all_residuals_gW[[1]]$rX_gW)
  d <- dim_rX[2]
  n <- dim_rX[1]
  gammalen <- length(gamma)
  # the dimensions 1:d in the second entry stores the matrix; the dimension
  # d+1 in the second entry stores the vector
  mat_vec_gamma <- array(0, dim = c(d, d + 1, gammalen))

  for (k in seq_len(K)) {
    mat_vec <-
      sapply(gamma,
             get_mat_vec_regDML2,
             rX = all_residuals[[k]]$rX,
             rX_gW = all_residuals_gW[[k]]$rX_gW,
             rY = all_residuals[[k]]$rY,
             rY_gW = all_residuals_gW[[k]]$rY_gW,
             n = n)
    mat_vec_gamma[, seq_len(d), ] <-
      mat_vec_gamma[, seq_len(d), , drop = FALSE] +
      array(unlist(mat_vec[1, ]), dim = c(d, d, gammalen))
    mat_vec_gamma[, d + 1, ] <-
      mat_vec_gamma[, d + 1, , drop = FALSE] +
      array(unlist(mat_vec[2, ]), dim = c(d, 1, gammalen))
  }

  func_tmp <- function(x) {
    qr.solve(x[, seq_len(d), drop = FALSE], x[, d + 1, drop = FALSE])
  }
  tryCatch_WEM(rbind(apply(mat_vec_gamma, 3, func_tmp)),
               matrix(NA, nrow = d, ncol = gammalen))$value
}

# returns all point estimates and variances for DML and the whole gamma grid
beta_crossfit <- function(aa, ww, xx, yy, K, gamma, DML, do_DML, do_regDML,
                          cond_func_all, params) {
  # issue a warning if sample splitting is omitted, that is, K = 1
  if (K == 1) {
    warning("no sample splitting performed due to K = 1")
  }

  # dimensions of the data
  d <- ncol(xx)
  gammalen <- length(gamma)

  # get matrices and vectors with which the coefficient estimate
  # is computed
  all_matrices <- beta_get_matrices(aa = aa, ww = ww, xx = xx, yy = yy,
                                    K = K, gamma = gamma, DML = DML,
                                    do_DML = do_DML, do_regDML = do_regDML,
                                    cond_func_all = cond_func_all,
                                    params = params)

  # compute estimators, variance-covariance matrices,
  # and prepare results to return
  to_return <- list()
  # DML method (non-regularized)
  if (do_DML) {
    # point estimator
    beta_DML <- if (DML == "DML1") { # DML1
      get_beta_DML1(all_residuals_gW = all_matrices$all_residuals_gW)
    } else { # DML2
      get_beta_DML2(all_residuals_gW = all_matrices$all_residuals_gW)
    } # end beta_DML

    # get asymptotic variance of the DML point estimator.
    # If necessary, compute a more stable version of it.
    as_var_DML <-
      tryCatch_WEM(sigma2_DML(all_residuals = all_matrices$all_residuals,
                              betahat = beta_DML),
                   matrix(NA, nrow = d, ncol = d))
    # return NAs above if matrix inversion in sigma2_DML was infeasible

    # if matrix inverion in the function call of sigma2_DML was not feasible,
    # try if a more stable version of the asymptotic variance matrix can be
    # computed.
    if (!is.null(as_var_DML$error)) {
      as_var_DML <-
        tryCatch_WEM(sigma2_DML_stable(all_residuals = all_matrices$all_residuals,
                                       betahat = beta_DML),
                     matrix(NA, nrow = d, ncol = d))
      if (is.null(as_var_DML$error) & !is.null(as_var_DML$warning)) {
        warning(as_var_DML$warning)
      }
    }
    as_var_DML <- as_var_DML$value

    # save results to return
    to_return <- c(to_return,
                   list(beta_DML = as.matrix(beta_DML),
                        as_var_DML = as_var_DML))
  }

  # regDML: here, this is used for regsDML, regDMLopt and regDML for all
  # gamma values
  if (do_regDML) {
    # point estimator for all gamma values
    beta_gamma <- if (DML == "DML1") { # DML1
      get_beta_regDML1(all_residuals = all_matrices$all_residuals,
                       all_residuals_gW = all_matrices$all_residuals_gW,
                       gamma = gamma)
    } else { # DML2
      get_beta_regDML2(all_residuals = all_matrices$all_residuals,
                       all_residuals_gW = all_matrices$all_residuals_gW,
                       gamma = gamma)
    } # end beta_gamma

    # asymptotic variance for all gamma values
    as_var_gamma <-
      array(unlist(lapply(seq_len(gammalen),
                          function(i) sigma2_gamma(all_residuals = all_matrices$all_residuals,
                                                   betahat = beta_gamma[, i, drop = FALSE],
                                                   gamma = gamma[i]))),
            dim = c(d, d, gammalen))

    # update return object
    to_return <- c(to_return,
                   list(beta_gamma = beta_gamma, as_var_gamma = as_var_gamma))
  }
  to_return
}
