# returns the functions used to compute the conditional expectations
# and specifies if several of them are splines. If several are splines,
# the computation of the conditional expectations can be performed in
# a faster way.
get_condexp_funcs_mm <- function(cond_method, params) {
  # if some of the nuisance parameters are estimated with splines and
  # have the same additional parameters stored in params, estimation can
  # be performed in a faster way.
  all_condexp_spline <- (cond_method == "spline")
  params_identical <- FALSE
  if (sum(all_condexp_spline) == 2) {
    params_identical <- identical(params[[1]], params[[2]])
  }

  # get functions to estimate conditional expectations
  if (is.atomic(cond_method)) {
    # a precoded machine learning method is chosen
    condexp_xx <- get(paste("condexp_", cond_method[1], sep = ""))
    condexp_yy <- get(paste("condexp_", cond_method[2], sep = ""))
  } else {
    # at least one custom machine learning methods is provided
    condexp_xx <- cond_method[[1]]
    if (is.atomic(condexp_xx)) {
      condexp_xx <- get(paste("condexp_", condexp_xx, sep = ""))
    }
    condexp_yy <- cond_method[[2]]
    if (is.atomic(condexp_yy)) {
      condexp_yy <- get(paste("condexp_", condexp_yy, sep = ""))
    }
  }

  list(params_identical = params_identical,
       all_condexp_spline = all_condexp_spline,
       condexp_xx = condexp_xx,
       condexp_yy = condexp_yy)
}

# computes the residuals on I with conditional expectations fitted from Ic
residuals_samplesplit_mm <- function(ww, xx, yy, I, Ic,
                                     cond_func_all, params) {

  # assign splitted data
  ww_fit <- ww[Ic, , drop = FALSE]
  xx_fit <- xx[Ic, , drop = FALSE]
  yy_fit <- yy[Ic, , drop = FALSE]
  ww_predict <- ww[I, , drop = FALSE]
  xx_predict <- xx[I, , drop = FALSE]
  yy_predict <- yy[I, , drop = FALSE]

  # compute conditional expectations E[X|W] and E[Y|W]
  if ((sum(cond_func_all$all_condexp_spline) == 2) &&
      cond_func_all$params_identical) {
    # both conditional expectations E[X|W] and E[Y|W] are fitted with splines
    # and the same set of extra parameters
    egW_hat_all <- condexp_spline(aa_fit = NULL,
                                  xx_fit = xx_fit,
                                  yy_fit = yy_fit,
                                  ww_fit = ww_fit,
                                  ww_predict = ww_predict,
                                  params = params[[1]])
    eXgW_hat <- egW_hat_all$eXgW_hat
    eYgW_hat <- egW_hat_all$eYgW_hat
  } else {
    # different estimation procedures for the two conditional expectations
    eXgW_hat <- cond_func_all$condexp_xx(yy_fit = xx_fit, ww_fit = ww_fit,
                                         ww_predict = ww_predict,
                                         params = params[[1]])
    eYgW_hat <- cond_func_all$condexp_yy(yy_fit = yy_fit, ww_fit = ww_fit,
                                         ww_predict = ww_predict,
                                         params = params[[2]])
  }

  # compute residuals
  list(rX = xx_predict - eXgW_hat,
       rY = yy_predict - eYgW_hat)
}

# computes all residual terms and lmer-parameter estimates
get_beta_mmdml <- function(zz, ww, xx, yy, zz_formula, group, K,
                           cond_func_all, params) {
  # issue a warning if sample splitting is omitted, that is, K = 1
  if (K == 1) {
    warning("no sample splitting performed due to K = 1")
  }

  # dimensions of the data
  N_levels <- unique(zz[[group]])
  N <- length(N_levels)
  Ntot <- length(yy)
  d <- ncol(xx)

  # build model formulas used to fit the partialled-out linear
  # mixed-effects model
  xx_colnames <- colnames(xx)
  colnames_rX <- sapply(xx_colnames, function(a) paste("rX", a, sep = ""))
  names(colnames_rX) <- NULL
  formula_rX <- paste(c(rbind(colnames_rX,
                              c(rep("+", length(colnames_rX) - 1), ""))),
                      sep = "", collapse = "")
  plus <- ifelse(!is.null(zz_formula), "+", "")
  lmer_formula <- as.formula(paste("rY ~ -1 + ", formula_rX, plus, zz_formula))

  # the residual quantities Rx and Ry need to be stored to estimate the
  # asymptotic variance-covariance matrices afterwards
  rX_hat <- matrix(0, nrow = Ntot, ncol = d)
  rY_hat <- matrix(0, nrow = Ntot, ncol = 1)
  colnames(rX_hat) <- colnames_rX
  colnames(rY_hat) <- "rY"

  # create folds used for sample splitting
  # this splits the "units" in folds
  N_reorder <- sample(1:N, N, replace = FALSE)
  folds <- if (K >= 2) {
    cut(N_reorder, breaks = K, labels = FALSE)
  } else {
    rep(1, N) # If K == 1, have same train and test set.
  } # end folds

  # perform the sample splitting (and cross-fitting)
  for (k in 1:K) {
    I <- N_levels[folds == k] # test set: evaluate conditional expectations
    Ic <- if (K >= 2) { # training set: estimate conditional expectations
      setdiff(N_levels, I)
    } else {
      N_levels # if K == 1, have same train and test set
    } # end Ic

    # gater the indices of all observations from units belonging to I or Ic
    I_full <- c(1:Ntot)[zz[[group]] %in% I]
    Ic_full <- c(1:Ntot)[zz[[group]] %in% Ic]

    # compute residuals
    residuals_hat <- residuals_samplesplit_mm(ww = ww, xx = xx, yy = yy,
                                              I = I_full, Ic = Ic_full,
                                              cond_func_all = cond_func_all,
                                              params = params)
    rX_hat[I_full, ] <- residuals_hat$rX
    rY_hat[I_full, ] <- residuals_hat$rY

    # fit linear mixed-effects model on partialled-out data
    lmer_data <- cbind(data.frame(residuals_hat$rY,
                                  residuals_hat$rX),
                       zz[I_full, , drop = FALSE])
    names(lmer_data) <- c("rY", colnames_rX, names(zz))
    fit_mm <- lmer(formula = lmer_formula, data = lmer_data, REML = FALSE)

    # In first iteration (k == 1), initialize the estimators
    if (k == 1) {
      sigma <- sigma(fit_mm)
      theta <- fit_mm@theta
      vcov_sum <- vcov.merMod(fit_mm)
      beta <- getME(fit_mm, "beta")
      ngrps_mm <- ngrps(fit_mm)
      cnms <- getME(fit_mm, "cnms")
      nobs <- fit_mm@devcomp$dims[["n"]]
      fitMsgs <- .merMod.msgs(fit_mm)
      optinfo <- fit_mm@optinfo
      optinfo$conv$lme4 <- list(messages = optinfo$conv$lme4$messages, code = optinfo$conv$lme4$code)
    } else {
      # in later iterations, update the estimators
      # (this is the cross-fitting step, which corresponds to
      # taking the mean of the K individual estimators)
      sigma <- sigma + sigma(fit_mm)
      theta <- theta + fit_mm@theta
      beta <- beta + getME(fit_mm, "beta")
      nobs <- nobs + fit_mm@devcomp$dims[["n"]]

      vcov_new <- vcov.merMod(fit_mm)
      vcov_sum@x <- vcov_sum@x + vcov_new@x
      vcov_sum@factors$correlation@sd <- vcov_sum@factors$correlation@sd + vcov_new@factors$correlation@sd
      vcov_sum@factors$correlation@x <- vcov_sum@factors$correlation@x + vcov_new@factors$correlation@x
      vcov_sum@factors$correlation@factors$Cholesky <- NULL

      ngrps_mm <- ngrps_mm + ngrps(fit_mm)
      fitMsgs <- c(fitMsgs, .merMod.msgs(fit_mm))
      optinfo_new <- fit_mm@optinfo
      optinfo$optimizer <- unique(optinfo$optimizer, optinfo_new$optimizer)

      v <- do.call(rbind, list(optinfo$control, optinfo_new$control))
      names_v <- names(optinfo$control)
      list_v <- lapply(names_v, function(x) unique(unlist(v[, x])))
      names(list_v) <- names_v
      optinfo$control <- list_v

      optinfo$derivs$gradient <- optinfo$derivs$gradient + optinfo_new$derivs$gradient
      optinfo$derivs$Hessian <- optinfo$derivs$Hessian + optinfo_new$derivs$Hessian
      optinfo$conv$opt <- unique(optinfo$conv$opt, optinfo_new$conv$opt)
      optinfo$conv$lme4 <-
        list(messages = rbind(optinfo$conv$lme4$messages, optinfo_new$conv$lme4$messages),
             code = rbind(optinfo$conv$lme4$code, optinfo_new$conv$lme4$code))

      optinfo$feval <- unique(optinfo$feval, optinfo_new$feval)
      optinfo$message <- unique(optinfo$message, optinfo_new$message)
      optinfo$warnings <- c(optinfo$warnings, optinfo_new$warnings)
      optinfo$val <- optinfo$val + optinfo_new$val

    } # if k > 1
  } # for (k in 1:K)

  # divide by K to finalize the cross-fitting step, which corresponds to
  # taking the mean of the K individual estimators
  sigma <- sigma / K
  theta <- theta / K
  beta <- beta / K
  vcov_sum@x <- vcov_sum@x / K ^ 2
  vcov_sum@factors$correlation@sd <- vcov_sum@factors$correlation@sd / K ^ (3/2)
  vcov_sum@factors$correlation@x <- vcov_sum@factors$correlation@x / K
  optinfo$derivs$gradient <- optinfo$derivs$gradient
  optinfo$derivs$Hessian <- optinfo$derivs$Hessian
  optinfo$val <- optinfo$val / K

  # compute random effects b_hat on full partialled-out data
  fit_full <- lmer(formula = lmer_formula, REML = FALSE,
                   data = cbind(data.frame(rY_hat), data.frame(rX_hat), zz))
  Lambda_full <- getME(fit_full, "Lambda")
  ind <- getME(fit_full, "Lind")
  Lambda_full@x <- theta[ind]
  Z_full <- getME(fit_full, "Z")
  res <- rY_hat - rX_hat %*% beta
  ZLambda <- as.matrix(Z_full %*% Lambda_full) # !!! andere Moeglichkeit?
  b_hat <- Lambda_full %*% qr.solve(crossprod(ZLambda, ZLambda) + diag(1, ncol(ZLambda)),
                                    crossprod(ZLambda, res))
  q_i <- getME(fit_full, "q_i")
  l_i <- getME(fit_full, "l_i")
  b_hat_reorder <- ranef(fit_full)
  b_hat_reorder <- lapply(seq_len(length(cnms)), function(x) {
    index <- 1 + sum(q_i[seq_len(x - 1)])
    rownms <- rownames(b_hat_reorder[[x]])
    colnms <- colnames(b_hat_reorder[[x]])
    a <- matrix(b_hat[index:(index - 1 + q_i[x])],
                ncol = length(cnms[[x]]), nrow = l_i[x],
                byrow = TRUE)
    rownames(a) <- rownms
    colnames(a) <- colnms
    a
  })
  names(b_hat_reorder) <- names(ranef(fit_full))

  # compute residuals
  res_res <- as.numeric((res - Z_full %*% b_hat) / sigma) # residuals(fit_full, type = "response", scale = TRUE)
  # !!! eventuell noch andere Residuen zurückgeben sowie Funktionen
  # zur Residuenberechnung,...

  ###### aus VarCorr.merMod
  cnms <- fit_mm@cnms
  nc <- lengths(cnms) # no. of columns per term
  nms <- {
    fl <- fit_mm@flist; names(fl)[attr(fl, "assign")]
  }
  useSc <- as.logical(fit_mm@devcomp$dims[["useSc"]])

  # return object
  list(random_eff = b_hat_reorder,
       beta = beta,
       theta = theta,
       sigma = sigma,
       vcov = vcov_sum,
       residuals = res_res,
       ngrps = ngrps_mm,
       nobs = nobs,
       fitMsgs = fitMsgs,
       cnms = cnms,
       nc = nc,
       nms = nms,
       useSc = useSc,
       optinfo = optinfo)
}
