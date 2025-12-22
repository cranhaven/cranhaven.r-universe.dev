# perform batch corrections on point estimator and variances for mmDML
get_batch_correction_mmDML <- function(beta_all_unlist, xx_colnames,
                                       nr_random_eff, nr_res) {
  d <- length(beta_all_unlist[1, "beta"][[1]]) # = dim(beta)
  S <- nrow(beta_all_unlist) # = number of overall repetitions of procedure
  methTitle <- "Semiparametric mixed model fit by maximum likelihood"
  to_return <- beta_all_unlist[1, ] # build return object
  to_return$methTitle <- methTitle

  # aggregate beta
  beta_S <- do.call(cbind, beta_all_unlist[, "beta"])
  to_return$beta <- apply(beta_S, 1, median, na.rm = TRUE)
  names(to_return$beta) <- xx_colnames

  # aggregate variance covariance
  vcov_correction <-
    array(apply(sweep(beta_S, 1, to_return$beta, FUN = "-"), 2, function(x) outer(x, x)),
          dim = c(d, d, S))
  vcov_S <-
    unlist(beta_all_unlist[, "vcov"])
  corr_x_S <-
    array(unlist(lapply(seq(S), function(i) vcov_S[[i]]@factors$correlation@x)),
          dim = c(d, d, S))
  corr_sd <-
    do.call(cbind, lapply(seq(S), function(i) vcov_S[[i]]@factors$correlation@sd))

  vcov_S <-
    array(unlist(lapply(seq(S), function(i) vcov_S[[i]]@x)), dim = c(d, d, S))
  vcov <-
    apply(vcov_S + vcov_correction, c(1, 2), median, na.rm = TRUE)
  if (!matrixcalc::is.positive.definite(vcov)) {
    # if vcov is not positive definite with median, take mean
    vcov <- apply(vcov_S + vcov_correction,
                  c(1, 2), mean, na.rm = TRUE)
  }

  # correlation
  corr_x <- apply(corr_x_S, c(1, 2), median, na.rm = TRUE)

  # standard deviation
  corr_sd_med <- apply(corr_sd, 1, median)

  to_return$vcov <-
    new("dpoMatrix",
        x = as.numeric(vcov),
        Dim = to_return$vcov@Dim,
        Dimnames = list(xx_colnames, xx_colnames),
        uplo = to_return$vcov@uplo,
        factors = list(correlation =
                         new("corMatrix",
                             sd = corr_sd_med,
                             x = as.numeric(corr_x),
                             Dim = to_return$vcov@factors$correlation@Dim,
                             Dimnames = list(xx_colnames, xx_colnames),
                             uplo = to_return$vcov@factors$correlation@uplo)))

  # aggregate random effects
  rand_eff_S <- beta_all_unlist[, "random_eff"]
  rand_eff_names <- names(rand_eff_S[[1]])
  rand_eff <- vector(mode = "list", length = length(rand_eff_names))
  names(rand_eff) <- rand_eff_names
  for (nm in rand_eff_names) {
    ord <- order(rownames(rand_eff_S[[1]][[nm]]))
    v <-
      array(unlist(lapply(seq(S), function(i) as.matrix(rand_eff_S[[i]][[nm]])[ord, ])),
            dim = c(dim(rand_eff_S[[1]][[nm]]), S))
    rand_eff[[nm]] <- as.data.frame(apply(v, c(1, 2), mean))
    rownames(rand_eff[[nm]]) <- rownames(rand_eff_S[[1]][[nm]])[ord]
    colnames(rand_eff[[nm]]) <- colnames(rand_eff_S[[1]][[nm]])
  }
  to_return$random_eff <- rand_eff
  to_return$random_eff_all <- beta_all_unlist[1:nr_random_eff, "random_eff"]

  # aggregate residuals
  to_return$residuals <- beta_all_unlist[1:nr_res, "residuals"]

  # aggregate theta
  to_return$theta <- apply(do.call(rbind, beta_all_unlist[, "theta"]), 2, median)

  # aggregate sigma
  to_return$sigma <- median(unlist(beta_all_unlist[, "sigma"]))

  # aggregate varcor
  cnms <- beta_all_unlist[, "cnms"][[1]]
  nc <- beta_all_unlist[, "nc"][[1]]
  nms <- beta_all_unlist[, "nms"][[1]]
  useSc <- beta_all_unlist[, "useSc"][[1]]
  varcor <- structure(mkVarCorr(to_return$sigma, cnms = cnms, nc = nc,
                                theta = to_return$theta,
                                nms = nms),
                      useSc = useSc,
                      class = "VarCorr.merMod")
  to_return$varcor <- varcor

  # aggregate optinfo
  optinfo_S <- do.call(rbind, beta_all_unlist[, "optinfo"])
  optinfo <- beta_all_unlist[1, "optinfo"]$optinfo

  # optimizer
  optinfo$optimizer <- unique(unlist(optinfo_S[, "optimizer"]))
  v <- do.call(rbind, optinfo_S[, "control"])
  names_v <- colnames(v)
  list_v <- lapply(names_v, function(x) unique(unlist(v[, x])))
  names(list_v) <- names_v
  optinfo$control <- list_v
  optinfo$derivs$gradient <-
    apply(do.call(rbind, do.call(rbind, optinfo_S[, "derivs"])[, "gradient"]),
          2, median)
  len_grad <- length(optinfo$derivs$gradient)
  optinfo$derivs$Hessian <-
    apply(array(unlist(do.call(rbind, optinfo_S[, "derivs"])[, "Hessian"]),
                dim = c(len_grad, len_grad, S)),
          c(1, 2), median)

  # varia
  optinfo$conv$opt <-
    unique(unlist(do.call(rbind, optinfo_S[, "conv"])[, "opt"]))
  messages_code <-
    do.call(rbind, do.call(rbind, optinfo_S[, "conv"])[, "lme4"])
  optinfo$conv$lme4 <-
    list(messages = unique(do.call(c, messages_code[, "messages"])),
         code = unique(do.call(c, messages_code[, "code"])))
  optinfo$feval <-
    unique(unlist(optinfo_S[, "feval"]))
  optinfo$message <-
    unique(unlist(optinfo_S[, "message"]))
  optinfo$warnings <-
    do.call(c, optinfo_S[, "warnings"])
  optinfo$val <-
    apply(do.call(rbind, optinfo_S[, "val"]), 2, median)
  to_return$optinfo <-
    optinfo

  # aggregate fitMsgs
  fitMsgs <- unique(unlist(beta_all_unlist[, "fitMsgs"]))
  to_return$fitMsgs <- fitMsgs

  # return the object built
  class(to_return) <- c("mmdml", "list")
  to_return
}

# bring data into the right format
check_data_mm <- function(zz, ww, xx, yy) {
  if (is.atomic(zz) | is.matrix(zz)) {
    zz <- as.data.frame(zz)
  }
  if (is.atomic(ww) | is.matrix(ww)) {
    ww <- as.data.frame(ww)
  }
  if (is.atomic(xx) | is.data.frame(xx)) {
    xx <- as.matrix(xx)
  }
  if (is.atomic(yy) | is.data.frame(yy)) {
    yy <- as.matrix(yy)
  }

  list(zz = zz, ww = ww, xx = xx, yy = yy)
}

# customize warning and error messages
DML_reg_error_message_mm <- function() {
  "Essentially perfect fit. Not enough non-NA results among the S repetitions."
}

# return an object consisting of NA-entries only but of the same dimension as
# the return object of the function beta_crossfit
beta_crossfit_NA_return_mm <- function(d) {
  list(random_eff = NA, beta = matrix(NA, nrow = d, ncol = 1),
       theta = NA, sigma = NA, vcov = NA, residuals = NA, ngrps = NA,
       nobs = NA, fitMsgs = NA, cnms = NA, nc = NA, nms = NA,
       useSc = NA, optinfo = NA)
}

batch_correction_mm <- function(zz, ww, xx, yy, zz_formula, group, K,
                                cond_method, params,
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
  cond_func_all <-
    get_condexp_funcs_mm(cond_method = cond_method, params = params)
  beta_crossfit_parallel_mm <- local({
    zz
    zz_formula
    group
    ww
    xx
    yy
    K
    cond_func_all
    params
    function(colnames.cluster) {
      tryCatch_WEM(
        get_beta_mmdml(zz = zz, ww = ww, xx = xx, yy = yy,
                       zz_formula = zz_formula, group = group, K = K,
                       cond_func_all = cond_func_all, params = params),
        beta_crossfit_NA_return_mm(d = ncol(xx)))
    }})

  if (do_parallel) {
    if (parallel == "multicore") {
      parallel::mclapply(1:S, beta_crossfit_parallel_mm, mc.cores = ncpus)
    } else if (parallel == "snow") {
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
        # export the namespace of dmlalg in order for the use the functions
        # of the package dmlalg on the workers
        parallel::clusterExport(cl, varlist = getNamespaceExports("dmlalg"))
        if (RNGkind()[1L] == "L'Ecuyer-CMRG")
          parallel::clusterSetRNGStream(cl)
        res <- parallel::parLapply(cl, 1:S, beta_crossfit_parallel_mm)
        parallel::stopCluster(cl)
        cl <- NULL # overwrite object which is responsible for the connection
        res
      } else parallel::parLapply(cl, 1:S, beta_crossfit_parallel_mm)
    }
  } else lapply(1:S, beta_crossfit_parallel_mm)
}

# estimating linear coefficients with double machine learning (DML) and
# regularization
mmdml <- function(w, x, y, z, data = NULL,
                  z_formula = NULL,
                  group = "group",
                  K = 2L, S = 100L,
                  cond_method = rep("forest", 2),
                  params = NULL,
                  parallel = c("no", "multicore", "snow"),
                  ncpus = 1L,
                  cl = NULL,
                  nr_random_eff = if (S > 5) 1L else S,
                  nr_res = nr_random_eff) {

  # nr_random_eff and nr_res must not exceed S
  if (nr_random_eff >  S) {
    stop("choose nr_random_eff <= S")
  }
  if (nr_res > S) {
    stop("choose nr_res <= S")
  }

  # preprocess input arguments
  parallel <- match.arg(parallel)
  do_parallel <- initial_setup(parallel = parallel,
                               ncpus = ncpus, cl = cl)

  if (!is.null(data)) {
    z <- data[, z, drop = FALSE]
    x <- data[, x, drop = FALSE]
    y <- data[, y, drop = FALSE]
    w <- data[, w, drop = FALSE]
    data <- NULL
  }

  mat_data <- check_data_mm(zz = z, ww = w, xx = x, yy = y)
  zz <- mat_data$zz
  xx <- mat_data$xx
  yy <- mat_data$yy
  ww <- mat_data$ww
  zz_formula <- z_formula

  d <- ncol(xx)
  xx_colnames <- colnames(xx)
  if (is.null(xx_colnames)) {
    xx_colnames <-
      apply(rbind(rep("b", d), 1:d), 2, function(x) paste(x[1], x[2], sep = ""))
    colnames(xx) <- xx_colnames
  }

  # fitting including a correction step for random batch creation
  beta_all <-
    batch_correction_mm(zz = zz, ww = ww, xx = xx, yy = yy,
                        zz_formula = zz_formula, group = group, K = K,
                        cond_method = cond_method, params = params,
                        do_parallel = do_parallel, parallel = parallel,
                        S = S, ncpus = ncpus, cl = cl)
  beta_all <- do.call(rbind, beta_all)
  beta_all_unlist <- do.call(rbind, beta_all[, "value"])

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
  # if more than half of the S repetitions resulted in NAs, stop
  stopping_criterion <- (sum(na_rows) > (0.5 * S))
  if (stopping_criterion) {
    # more than half of the S repetitions resulted in NAs,
    # so append error messages by a respective message
    errors <- c(errors, DML_reg_error_message_mm())
  } else if (sum(na_rows) >= 1) {
    # if some NAs were returned, try to replace them
    warningMsgs <-
      c(warningMsgs, "Essentially perfect fit: do S more repetitions.")
    beta_all_new <-
      batch_correction_mm(zz = zz, ww = ww, xx = xx, yy = yy,
                          zz_formula = zz_formula, group = group, K = K,
                          cond_method = cond_method, params = params,
                          do_parallel = do_parallel, parallel = parallel,
                          S = S, ncpus = ncpus, cl = cl)
    beta_all_new <- do.call(rbind, beta_all_new)
    beta_all_unlist_new <- do.call(rbind, beta_all_new[, "value"])
    #beta_all_message_new <- unique(do.call(c, beta_all_new[, "message"]))

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
    if (sum(non_na_rows_new) < sum_na_rows) {
      # there are not enough new non-NA cases
      # adapt error and warning messages accordingly
      errors <-
        unique(c(errors,
                 unique(do.call(c, beta_all_new[, "error"][non_na_rows_new]))))
      # treat messages as warnings
      warningMsgs <-
        unique(c(warningMsgs,
                 unique(do.call(c, beta_all_new[, "warning"][non_na_rows_new])),
                 unique(do.call(c, beta_all_new[, "message"][non_na_rows_new]))))
      beta_all_new <- NULL

      errors <- unique(c(errors, DML_reg_error_message_mm()))

    } else {
      # there are enough new non-NA cases
      # adapt error and warning messages accordingly
      errors <-
        unique(c(errors,
                 unique(do.call(c, beta_all_new[, "error"][non_na_rows_new][1:sum_na_rows]))))
      warningMsgs <-
        unique(c(warningMsgs,
                 unique(do.call(c, beta_all_new[, "warning"][non_na_rows_new][1:sum_na_rows])),
                 unique(do.call(c, beta_all_new[, "message"][non_na_rows_new][1:sum_na_rows]))))
      beta_all_new <- NULL

      beta_all_unlist <-
        rbind(beta_all_unlist[non_na_rows, ],
              beta_all_unlist_new[non_na_rows_new, ][1:sum_na_rows, ])
      beta_all_unlist_new <- NULL
    }
  }

  # take out new lines "\n"
  warningMsgs <- sapply(seq_len(length(warningMsgs)),
                        function(x) sub(pattern = "\n", replacement = "", warningMsgs[x]))
  # prepare return object if do not need to stop early
  warningMsgs_all <- if (!stopping_criterion) {
    res_mmDML <-
      get_batch_correction_mmDML(beta_all_unlist = beta_all_unlist,
                                 xx_colnames = xx_colnames,
                                 nr_random_eff = nr_random_eff,
                                 nr_res = nr_res)
    res_mmDML$warnings_fit <- warningMsgs
    res_mmDML$errors_fit <- errors
    unique(c(warningMsgs, res_mmDML$optinfo$conv$lme4$message))
  } else {
    warningMsgs
  }

  # return errors and warnings encountered estimating the
  # conditional expectations
  if (!is.null(errors)) { # there are errors
    if (!is.null(warningMsgs_all)) { # there are errors and warnings
      print_W_E <- paste("\nError messages:\n",
                         paste(errors,
                               collapse = "\n"),
                         "\n\n",
                         "Warning messages:\n",
                         paste(warningMsgs_all,
                               collapse = "\n"),
                         sep = "")
    } else { # there are errors but no warnings
      print_W_E <- paste("\nError messages:",
                         paste(errors,
                               collapse = "\n"))
    }
    stop(print_W_E)
  } else if (!is.null(warningMsgs_all)) { # there are warnings but no errors
    warning(paste("\nWarning messages:",
                  paste(warningMsgs_all, collapse = "\n"),
                  sep = "\n"))
  }

  # return object
  attr(res_mmDML, "nr_res") <- nr_res
  res_mmDML
}
