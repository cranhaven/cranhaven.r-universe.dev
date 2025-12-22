# pre-process information about parallel computing and output an error if
# the computing platform does not support the parallelization method
initial_setup <- function(parallel, ncpus, cl) {
  do_parallel <- ((parallel != "no" && ncpus > 1L) ||
                    (parallel == "snow" && !is.null(cl)))
  if (do_parallel &&
      parallel == "multicore" &&
      .Platform$OS.type == "windows") {
    stop("The argument parallel = 'multicore' is not available for windows.
Use parallel = 'snow' for parallel execution or
parallel = 'no' for serial execution of the code.")
  }

  do_parallel
}

# return an object consisting of NA-entries only but of the same dimension as
# the return object of the function beta_crossfit
beta_crossfit_NA_return <- function(d, gammalen) {
  list(beta_DML = matrix(NA, nrow = d, ncol = 1),
       as_var_DML = matrix(NA, nrow = d, ncol = d),
       beta_gamma = matrix(NA, nrow = d, ncol = gammalen),
       as_var_gamma = array(NA, dim = c(d, d, gammalen)))
}

# fitting including a correction step for random batch creation:
# repeat the whole procedure S times.
batch_correction <- function(aa, ww, xx, yy, K, gamma, DML,
                             do_DML, do_regsDML, do_regDML, do_regDML_all_gamma,
                             safety,
                             cond_method,
                             params = NULL,
                             do_parallel, parallel, S, ncpus, cl) {
  # batch correction step:
  # The concept of how to elegantly parallelize a function call (and save
  # all warnings, errors, and messages) is taken from the package boot or
  # lme4. Both packages implement almost identical solutions.
  # See the source code of the package boot: R/bootfuns.R in the function
  # boot().
  # See the source code of the package lme4: R/bootMer.R in the function
  # bootMer().
  # An implementation of this parallelized code can alternatively be found
  # in the package hierinf.

  # Using a closure, the function below can access all the variables of the
  # environment in which it was created. This makes parallel computation
  # cleaner or simpler. There are less arguments and we do not have to
  # export objects to the workers in the PSOCKcluster case.
  cond_func_all <- get_condexp_funcs(cond_method = cond_method,
                                     params = params)
  beta_crossfit_parallel <- local({
    aa
    ww
    xx
    yy
    K
    gamma
    DML
    do_DML
    do_regsDML
    do_regDML
    do_regDML_all_gamma
    safety
    cond_func_all
    params
    function(colnames.cluster) {
      tryCatch_WEM(beta_crossfit(aa = aa, ww = ww, xx = xx, yy = yy,
                                 K = K,
                                 gamma = gamma,
                                 DML = DML,
                                 do_DML = do_DML,
                                 do_regDML = (do_regDML || do_regDML_all_gamma || do_regsDML || safety),
                                 cond_func_all = cond_func_all,
                                 params = params),
                   beta_crossfit_NA_return(d = ncol(xx), gammalen = length(gamma)))
    }})

  if (do_parallel) {
    if (parallel == "multicore") {
      parallel::mclapply(seq_len(S), beta_crossfit_parallel, mc.cores = ncpus)
    } else if (parallel == "snow") {
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
        # export the namespace of dmlalg in order for the use the functions
        # of the package dmlalg on the workers
        parallel::clusterExport(cl, varlist = getNamespaceExports("dmlalg"))
        if (RNGkind()[1L] == "L'Ecuyer-CMRG")
          parallel::clusterSetRNGStream(cl)
        res <- parallel::parLapply(cl, seq_len(S), beta_crossfit_parallel)
        parallel::stopCluster(cl)
        cl <- NULL # overwrite object which is responsible for the connection
        res
      } else parallel::parLapply(cl, seq_len(S), beta_crossfit_parallel)
    }
  } else lapply(seq_len(S), beta_crossfit_parallel)
}

# compute confidence interval
get_CI_DML <- function(beta, sd, alpha) {
  deviation <- qnorm(1 - alpha / 2, 0, 1) * sd
  CI <- cbind(beta - deviation, beta + deviation)
  rownames(CI) <- rownames(beta)
  colnames(CI) <- c(paste((alpha / 2) * 100, " %", sep = ""),
                    paste((1 - alpha / 2) * 100, " %", sep = ""))
  CI
}

# return results in the form of a table
return_results <- function(beta, var, xx_colnames, method, alpha) {
  beta_return <- cbind(beta, deparse.level = 0)
  rownames(beta_return) <- xx_colnames
  colnames(beta_return) <- paste("beta_", method, sep = "")

  var_return <- rbind(var, deparse.level = 0)
  colnames(var_return) <- rownames(var_return) <- xx_colnames

  sd_return <- cbind(sqrt(diag(var_return)), deparse.level = 0)
  rownames(sd_return) <- xx_colnames
  colnames(sd_return) <- paste("sd_", method, sep = "")

  pval_return <- cbind(2 * pnorm(abs(beta_return / sd_return),
                                 lower.tail = FALSE),
                       deparse.level = 0)
  rownames(pval_return) <- xx_colnames
  colnames(pval_return) <- paste("pval_", method, sep = "")

  CI_return <- get_CI_DML(beta = beta_return,
                          sd = sd_return,
                          alpha = alpha)

  list_names <- sapply(c("beta", "sd", "var", "pval", "CI"),
                       FUN = function(x) paste(x, "_", method, sep = ""))
  return_statistic <- list(beta_return,
                           sd_return,
                           var_return,
                           pval_return,
                           CI_return)
  names(return_statistic) <- list_names
  return_statistic
}

# symmetrize matrix mat
symmetrize <- function(mat) {
  mat[lower.tri(mat, diag = FALSE)] <- t(mat)[lower.tri(mat, diag = FALSE)]
  mat
}

# perform batch corrections on point estimator and variances for DML
get_DML_batch_corrected <- function(beta_all_unlist) {
  d <- nrow(beta_all_unlist[1, "beta_DML"][[1]])
  S <- nrow(beta_all_unlist)

  beta_DML_S <- do.call(cbind, beta_all_unlist[, "beta_DML"])
  as_var_DML_S <- array(unlist(beta_all_unlist[, "as_var_DML"]),
                        dim = c(d, d, S))

  beta_DML <- apply(beta_DML_S, 1, median, na.rm = TRUE)
  func_tmp <- function(x) {
    outer(x, x)
  }
  DML_correction_var <-
    array(apply(sweep(beta_DML_S, 1, beta_DML, FUN = "-"), 2, func_tmp),
          dim = c(d, d, S))
  # preferably, assemble the variances by using the median.
  # If the resulting matrix is not positive definite, assemble using the mean.
  # This problem can only occur id d > 1.
  as_var_DML <- apply(as_var_DML_S + DML_correction_var, c(1, 2), median, na.rm = TRUE)
  if (d > 1) {
    # Due to rounding issues, it can happen that as_var_DML is not symmetric
    # up to machine precision. Account for this
    as_var_DML <- symmetrize(as_var_DML)
    if (!matrixcalc::is.positive.definite(as_var_DML)) {
      as_var_DML <- apply(as_var_DML_S + DML_correction_var, c(1, 2), mean, na.rm = TRUE)
      # make sure returned matrix is symmetric
      as_var_DML <- symmetrize(as_var_DML)
    }
  }

  # return batch-corrected results
  list(beta_DML = beta_DML,
       as_var_DML = as_var_DML)
}

# perform batch corrections on point estimator and variances for regularization
# schemes
get_regDML_all_gamma_batch_corrected <- function(beta_all_unlist) {
  d <- nrow(beta_all_unlist[1, "beta_gamma"][[1]])
  gammalen <- ncol(beta_all_unlist[1, "beta_gamma"][[1]])
  S <- nrow(beta_all_unlist)

  beta_gamma_S <- array(unlist(beta_all_unlist[, "beta_gamma"]),
                        dim = c(d, gammalen, S))
  as_var_gamma_S <- array(unlist(beta_all_unlist[, "as_var_gamma"]),
                          dim = c(d, d, gammalen, S))

  beta_gamma <- apply(beta_gamma_S, c(1, 2), median, na.rm = TRUE)
  # preferably, assemble the variances by using the median.
  # If the resulting matrix is not positive definite, assemble using the mean.
  # This can only happen if d > 1.
  func_tmp <- function(x) {
    array(apply(x, 2, function(x) outer(x, x)), dim = c(d, d, gammalen))
  }
  as_var_gamma <-
    apply(as_var_gamma_S + array(apply(sweep(beta_gamma_S, c(1, 2), beta_gamma, FUN = "-"),
                                       3, func_tmp),
                                 dim = c(d, d, gammalen, S)),
          c(1, 2, 3), median, na.rm = TRUE)

  if (d > 1) {
    as_var_gamma_sum <-
      apply(as_var_gamma_S + array(apply(sweep(beta_gamma_S, c(1, 2), beta_gamma, FUN = "-"),
                                         3, func_tmp),
                                   dim = c(d, d, gammalen, S)),
            c(1, 2, 3), mean, na.rm = TRUE)

    for (i in seq_len(gammalen)) {
      # make sure the matrix is symmetric. Due to rounding errors,
      # it can happen that the matrix is not symmetric up to machine precision.
      as_var_gamma[, , i] <- symmetrize(as_var_gamma[, , i])
      if (!matrixcalc::is.positive.definite(as_var_gamma[, , i])) {
        as_var_gamma[, , i] <- as_var_gamma_sum[, , i]
        # make sure matrix is symmetric
        as_var_gamma[, , i] <- symmetrize(as_var_gamma[, , i])
      }
    }
  }

  # return batch-corrected results
  list(beta_gamma = beta_gamma,
       as_var_gamma = as_var_gamma)
}

# find names of columns where a row of the S repetitions contains NA return
# values
find_na_rows <- function(beta_all_unlist_row) {
  present_names <- names(beta_all_unlist_row)
  count <- 0
  problematic_names <- NULL
  for (name in present_names) {
    if (sum(is.na(beta_all_unlist_row[[name]])) > 0) {
      count <- count + 1
      problematic_names <- c(problematic_names, name)
    }
  }
  list(count = count,
       problematic_names = problematic_names)
}

# customize warning and error messages
DML_reg_error_message <- function(is_DML_prob, is_reg_prob) {
  problematic_schemes <-
    paste(c("DML", ", ", "a regularization scheme")[c(is_DML_prob, is_DML_prob && is_reg_prob, is_reg_prob)],
          collapse = "")
  paste("Essentially perfect fit, not enough non-NA results among the S repetitions.",
        "\nSingularity occured in: ",
        problematic_schemes,
        "\nPlease rerun regsdml without it or try using 'DML = DML2'.",
        sep = "")
}

# If some NA-rows are present in beta_all_unlist, try to replace them
return_W_E_res_fun <- function(beta_all, beta_all_unlist,
                               aa, ww, xx, yy,
                               K, gamma,
                               DML, do_DML, do_regsDML, do_regDML,
                               do_regDML_all_gamma,
                               safety, cond_method, params, do_parallel,
                               parallel, S, ncpus, cl, beta_all_new = NULL) {
  # identify NA-rows of beta_all_unlist and delete respective warning and
  # error messages
  na_rows_all <- do.call(rbind, apply(beta_all_unlist, 1, find_na_rows))
  na_rows <- unlist(na_rows_all[, 1])
  problematic_names <- do.call(rbind, na_rows_all[, 2])
  if (!is.null(problematic_names)) {
    problematic_names <- apply(problematic_names, 2, unique)
  }
  # ith entry is TRUE if ith repetition among the S returns NA
  na_rows <- (na_rows >= 1)
  non_na_rows <- !na_rows

  # adapt error and warning messages accordingly (treat messages as warnings)
  errors <- unique(do.call(c, beta_all[, "error"][non_na_rows]))
  warningMsgs <- unique(c(do.call(c, beta_all[, "warning"][non_na_rows]),
                          do.call(c, beta_all[, "message"][non_na_rows])))
  beta_all <- NULL

  # check if design is regular enough to give enough non-NA returns among
  # the S repetitions
  # if more than half of the S repetitions resulted inNAs, stop
  stopping_criterion <- (sum(na_rows) > (0.5 * S))
  if (stopping_criterion) {
    DML_problematic <- sum(grepl("DML", problematic_names)) >= 1
    reg_problematic <- sum(grepl("gamma", problematic_names)) >= 1
    # one should only be in here if some NA-values were observed
    stopifnot(DML_problematic + reg_problematic >= 1)
    new_error_message <- DML_reg_error_message(is_DML_prob = DML_problematic,
                                               is_reg_prob = reg_problematic)

    errors <- c(errors, new_error_message)
  } else if (sum(na_rows) >= 1) { # if some NAs were returned,
    # try to replace them
    warningMsgs <- c(warningMsgs, "Essentially perfect fit: do S more repetitions.")
    # If beta_all_new is not computed yet, compute it
    beta_all_new <- if (is.null(beta_all_new)){
      batch_correction(aa = aa, ww = ww, xx = xx, yy = yy,
                       K = K,
                       gamma = gamma,
                       DML = DML,
                       do_DML = do_DML,
                       do_regsDML = do_regsDML,
                       do_regDML = do_regDML,
                       do_regDML_all_gamma = do_regDML_all_gamma,
                       safety = safety,
                       cond_method = cond_method,
                       params = params,
                       do_parallel = do_parallel,
                       parallel = parallel,
                       S = S, ncpus = ncpus, cl = cl)
    } else {
      beta_all_new
    }

    beta_all_new <- do.call(rbind, beta_all_new)
    beta_all_unlist_new <- do.call(rbind, beta_all_new[, "value"])

    # identify NA-rows of beta_all_unlist_new and delete respective warning and
    # error messages
    na_rows_all_new <- do.call(rbind, apply(beta_all_unlist_new, 1, find_na_rows))
    na_rows_new <- unlist(na_rows_all_new[, 1])
    problematic_names_new <- do.call(rbind, na_rows_all_new[, 2])
    if (!is.null(problematic_names_new)) {
      problematic_names_new <- apply(problematic_names_new, 2, unique)
    }
    # ith entry is TRUE if ith repetition among the S returns NA
    na_rows_new <- (na_rows_new >= 1)
    non_na_rows_new <- !na_rows_new

    sum_na_rows <- sum(na_rows)
    if (sum(non_na_rows_new) < sum_na_rows) { # there are not enough new
      # non-NA cases
      # adapt error and warning messages accordingly
      errors <-
        unique(c(errors,
                 unique(do.call(c, beta_all_new[, "error"][non_na_rows_new]))))
      warningMsgs <-
        unique(c(warningMsgs,
                 unique(c(do.call(c, beta_all_new[, "warning"][non_na_rows_new]),
                          do.call(c, beta_all_new[, "message"][non_na_rows_new])))))
      beta_all_new <- NULL

      DML_problematic_new <- (sum(grepl("DML", problematic_names_new)) >= 1) ||
        (sum(grepl("DML", problematic_names)) >= 1)
      reg_problematic_new <- (sum(grepl("gamma", problematic_names_new)) >= 1) ||
        (sum(grepl("gamma", problematic_names)) >= 1)
      stopifnot(DML_problematic_new + reg_problematic_new >= 1)
      new_error_message_new <- DML_reg_error_message(is_DML_prob = DML_problematic_new,
                                                     is_reg_prob = reg_problematic_new)
      errors <- unique(c(errors, new_error_message_new))

    } else { # there are enough new non-NA cases
      # adapt error and warning messages accordingly
      errors <-
        unique(c(errors,
                 unique(do.call(c, beta_all_new[, "error"][non_na_rows_new][seq_len(sum_na_rows)]))))
      warningMsgs <-
        unique(c(warningMsgs,
                 unique(c(do.call(c, beta_all_new[, "warning"][non_na_rows_new][seq_len(sum_na_rows)]),
                          do.call(c, beta_all_new[, "message"][non_na_rows_new][seq_len(sum_na_rows)])))))
      beta_all_new <- NULL

      beta_all_unlist <-
        rbind(beta_all_unlist[non_na_rows, ],
              beta_all_unlist_new[non_na_rows_new, ][seq_len(sum_na_rows), ])
      beta_all_unlist_new <- NULL
    }
  }

  # return warnings, errors and results of the algorithm that can
  # be further processed to be finally returned
  list(beta_all_unlist = beta_all_unlist,
       errors = errors,
       warningMsgs = warningMsgs)
}

# return errors and warnings encountered estimating the
# conditional expectations
print_W_E_fun <- function(errors, warningMsgs) {
  if (!is.null(errors)) { # there are errors
    print_W_E <- if (!is.null(warningMsgs)) { # there are errors and warnings
      paste("\nError messages:\n",
            paste(errors, collapse = "\n"),
            "\n\n",
            "Warning messages:\n",
            paste(warningMsgs, collapse = "\n"),
            sep = "")
    } else { # there are errors but no warnings
      paste("\nError messages:",
            paste(errors, collapse = "\n"))
    } # end print_W_E
    stop(print_W_E)
  } else if (!is.null(warningMsgs)) { # there are warnings but no errors
    warning(paste("\nWarning messages:",
                  paste(warningMsgs, collapse = "\n"),
                  sep = "\n"))
  }
}

# estimating linear coefficients with double machine learning (DML) and
# regularization
regsdml <- function(a, w, x, y, data = NULL,
                    DML = c("DML2", "DML1"),
                    K = 2L,
                    gamma = exp(seq(-4, 10, length.out = 100)),
                    aN = NULL,
                    do_regsDML = TRUE,
                    do_safety = FALSE,
                    do_DML = do_regDML || do_regsDML || do_safety,
                    do_regDML = FALSE,
                    do_regDML_all_gamma = FALSE,
                    safety_factor = 0.7,
                    cond_method = rep("spline", 3),
                    params = NULL,
                    level = 0.95,
                    S = 100L,
                    parallel = c("no", "multicore", "snow"),
                    ncpus = 1L,
                    cl = NULL) {

  safety <- do_safety
  if ((do_regDML || do_regsDML || safety) && !do_DML) {
    stop("do_DML must be TRUE if either one of
do_regsDML, do_regDML, or safety is TRUE")
  }

  # preprocess input arguments
  alpha <- 1 - level
  DML <- match.arg(DML)
  parallel <- match.arg(parallel)
  do_parallel <- initial_setup(parallel = parallel,
                               ncpus = ncpus, cl = cl)

  # preprocess data if it is given as a data frame
  if (!is.null(data)) {
    a <- data[, a]
    x <- data[, x]
    y <- data[, y]
    w <- data[, w]
    data <- NULL
  }

  # preprocess data to make sure it is of the right format
  mat_data <- check_data(aa = a, ww = w, xx = x, yy = y)
  aa <- mat_data$aa
  xx <- mat_data$xx
  yy <- mat_data$yy
  ww <- mat_data$ww
  mat_data <- NULL

  # read size of data
  N <- length(yy)
  d <- ncol(xx)
  gammalen <- length(gamma)
  # read column names that are used as names for the betas
  xx_colnames <- colnames(xx)
  if (is.null(xx_colnames)) {
    xx_colnames <- apply(rbind(rep("b", d), seq_len(d)), 2,
                         function(x) paste(x[1], x[2], sep = ""))
  }

  # fitting including a correction step for random batch creation
  beta_all <- batch_correction(aa = aa, ww = ww, xx = xx, yy = yy,
                               K = K,
                               gamma = gamma,
                               DML = DML,
                               do_DML = do_DML,
                               do_regsDML = do_regsDML,
                               do_regDML = do_regDML,
                               do_regDML_all_gamma = do_regDML_all_gamma,
                               safety = safety,
                               cond_method = cond_method,
                               params = params,
                               do_parallel = do_parallel,
                               parallel = parallel,
                               S = S, ncpus = ncpus, cl = cl)
  beta_all <- do.call(rbind, beta_all)
  beta_all_unlist <- do.call(rbind, beta_all[, "value"])

  # If some NA-rows are present in beta_all_unlist, try to replace them
  return_W_E_res <-
    return_W_E_res_fun(beta_all = beta_all, beta_all_unlist = beta_all_unlist,
                       aa = aa, ww = ww, xx = xx, yy = yy,
                       K = K, gamma = gamma,
                       DML = DML, do_DML = do_DML, do_regsDML = do_regsDML,
                       do_regDML = do_regDML,
                       do_regDML_all_gamma = do_regDML_all_gamma,
                       safety = safety, cond_method = cond_method,
                       params = params, do_parallel = do_parallel,
                       parallel = parallel, S = S, ncpus = ncpus, cl = cl,
                       beta_all_new = NULL)
  beta_all_unlist <- return_W_E_res$beta_all_unlist
  errors <- return_W_E_res$errors
  warningMsgs <- return_W_E_res$warningMsgs
  return_W_E_res <- NULL

  # return errors and warnings encountered estimating the
  # conditional expectations
  print_W_E_fun(errors, warningMsgs)

  # build return object
  to_return <- list()
  # DML (unregularized estimator and associated variance)
  if (do_DML) {
    DML_batch_corrected <- get_DML_batch_corrected(beta_all_unlist)
    beta_DML <- DML_batch_corrected$beta_DML
    as_var_DML <- DML_batch_corrected$as_var_DML

    # variance (= mean squared error) of DML. If d > 1, take as variance measure
    # the trace of the variance-covariance matrix.
    mse_DML <- sum(diag(as_var_DML))
  }

  # regularized methods
  if (do_regDML_all_gamma || do_regDML || do_regsDML || safety) {
    regDML_all_gamma_batch_corrected <- get_regDML_all_gamma_batch_corrected(beta_all_unlist)
    beta_gamma <- regDML_all_gamma_batch_corrected$beta_gamma
    as_var_gamma <- regDML_all_gamma_batch_corrected$as_var_gamma
  }

  # regularized methods: choose optimal (w.r.t. MSE) gamma values
  if (do_regDML || do_regsDML || safety) {
    # bias, variance, and mean squared error of regDML.
    # If d > 1, take as variance measure the trace of the variance-covariance
    # matrix.
    bias2_gamma <- apply(sweep(beta_gamma, 1, beta_DML, FUN = "-"), 2,
                         function(x) sqrt(sum(x ^ 2)))
    var_gamma <- apply(as_var_gamma, 3, function(x) sum(diag(x)))
    mse_gamma <- bias2_gamma + var_gamma

    # choose value of gamma that optimizes the (estimated) asymptotic MSE
    ind_opt <- which.min(mse_gamma)
    gamma_opt <- gamma[ind_opt]
    if (is.null(aN)) {
      aN <- max(1, log(sqrt(N)))
    }
    ind_aN <- which(gamma >= aN * gamma_opt)[1]
    if (is.na(ind_aN)) {
      ind_aN <- gammalen
    }
    gamma_aN <- gamma[ind_aN]
    var_aN <- var_gamma[ind_aN]
  }

  # prepare and return regsDML statistics
  if (do_regsDML) {
    regsDML_statistics <- if (is.na(mse_DML) || var_aN < mse_DML) {
      c(return_results(beta = beta_gamma[, ind_aN],
                       var = as_var_gamma[, , ind_aN],
                       xx_colnames = xx_colnames,
                       method = "regsDML",
                       alpha = alpha),
        list(gamma_aN = gamma_aN, message_regsDML = "regDML selected"))
    } else {
      c(return_results(beta = beta_DML,
                       var = as_var_DML,
                       xx_colnames = xx_colnames,
                       method = "regsDML",
                       alpha = alpha),
        list(message_regsDML = "DML selected", gamma_aN = Inf))
    } # end regsDML_statistics
    to_return <-
      c(to_return, list(regsDML_statistics = regsDML_statistics))
  }

  # safety device: if possible, do not allow for a smaller (norm of the)
  # variance of regDML than safety_factor times the variance (= mse_DML) of DML
  apply_safety_device <- FALSE
  if (safety) {
    ind_safety <- which(var_gamma >= safety_factor * mse_DML)[1]

    regDML_safety_statistics <- if (!is.na(ind_safety)) {
      apply_safety_device <- TRUE
      c(return_results(beta = beta_gamma[, ind_safety],
                       var = as_var_gamma[, , ind_safety],
                       xx_colnames = xx_colnames,
                       method = "safety",
                       alpha = alpha),
        list(gamma_safety = gamma[ind_safety],
             message_safety = "safety device applicable"))
    } else {
      list(message_safety = "safety device not applicable")
    } # end apply_safety_device
    to_return <-
      c(to_return, list(regDML_safety_statistics = regDML_safety_statistics))
  }

  # return DML statistics
  if (do_DML) {
    to_return <-
      c(to_return,
        list(DML_statistics = return_results(beta = beta_DML,
                                             var = as_var_DML,
                                             xx_colnames = xx_colnames,
                                             method = "DML",
                                             alpha = alpha)))
  }

  # prepare and return regDML statistics
  if (do_regDML) {
    to_return <-
      c(to_return,
        list(regDML_statistics = c(return_results(beta = beta_gamma[, ind_aN],
                                                  var = as_var_gamma[, , ind_aN],
                                                  xx_colnames = xx_colnames,
                                                  method = "regDML",
                                                  alpha = alpha),
                                   list(gamma_aN = gamma_aN))))
  }

  # return regDML_all_gamma statistic
  if (do_regDML_all_gamma) {
    to_return <-
      c(to_return,
        list(regDML_all_gamma_statistics = lapply(seq_len(gammalen),
                                                  function(j) return_results(beta = beta_gamma[, j],
                                                                             var = as_var_gamma[, , j],
                                                                             xx_colnames = xx_colnames,
                                                                             method = "regDML_all_gamma",
                                                                             alpha = alpha))))
  }

  # return results
  attr(to_return, "level") <- level
  attr(to_return, "d") <- d
  if (apply_safety_device) {
    attr(to_return, "safety_factor") <- safety_factor
  }
  if (do_regDML_all_gamma) {
    attr(to_return, "gamma") <- gamma
  }

  class(to_return) <- c("regsdml", "list")
  to_return
}
