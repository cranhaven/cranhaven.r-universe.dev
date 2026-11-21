
.mi_iirr_check <- function(data, vars) {
  d <- as.data.frame(data); bad <- setdiff(unique(vars), names(d))
  if (length(bad)) stop("Missing columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  d
}

.mi_iirr_seed <- function(seed, expr) {
  if (missing(seed) || length(seed) != 1L || !is.finite(seed))
    stop("`seed` must be one finite integer.", call. = FALSE)
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, .GlobalEnv)
    else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed)); force(expr)
}

.mi_iirr_imputation_random <- function(d, id, vars) {
  vars <- unique(vars)
  if (!length(vars)) return(list(used = character(), dropped = character()))
  varies <- vapply(vars, function(v) {
    zz <- split(d[[v]], as.character(d[[id]]), drop = TRUE)
    any(vapply(zz, function(x) length(unique(x[is.finite(x)])) > 1L,
               logical(1)))
  }, logical(1))
  list(used = vars[varies], dropped = vars[!varies])
}

.mi_iirr_impute <- function(d, id, time, outcome, obs_indicator, fixed_vars,
                        random_vars, m, maxit, seed) {
  if (!requireNamespace("mice", quietly = TRUE) || !requireNamespace("pan", quietly = TRUE))
    stop("MI-IIRR requires CRAN packages 'mice' and 'pan'.", call. = FALSE)
  d$.mi_iirr_row <- seq_len(nrow(d)); vis <- is.finite(d[[time]])
  a <- d[vis, , drop = FALSE]; z0 <- d[!vis, , drop = FALSE]
  if (!nrow(a)) stop("There are no visit rows.", call. = FALSE)
  marker_observed <- !is.na(a[[obs_indicator]]) & a[[obs_indicator]] == 1 &
    is.finite(a[[outcome]])
  a[[outcome]][!marker_observed] <- NA_real_
  random_info <- .mi_iirr_imputation_random(a, id, random_vars)
  random_vars <- random_info$used
  pred <- unique(c(fixed_vars, random_vars, random_info$dropped, time))
  nonnum <- pred[!vapply(a[pred], is.numeric, logical(1))]
  if (length(nonnum)) stop("2l.pan predictors must be numeric: ", paste(nonnum, collapse = ", "))
  miss <- !is.finite(a[[outcome]])
  if (!any(miss)) {
    out <- rbind(a, z0); out <- out[order(out$.mi_iirr_row), ]; out$.mi_iirr_row <- NULL
    ans <- rep(list(out), as.integer(m))
    attr(ans, "imputation_random_used") <- random_info$used
    attr(ans, "imputation_random_dropped") <- random_info$dropped
    return(ans)
  }
  cl <- ".mi_iirr_cluster"; while (cl %in% names(a)) cl <- paste0(cl, "_")
  a[[cl]] <- match(a[[id]], unique(a[[id]]))
  .mi_iirr_seed(seed, {
    ini <- mice::mice(a, maxit = 0, printFlag = FALSE)
    pm <- ini$predictorMatrix; pm[,] <- 0
    meth <- ini$method; meth[] <- ""; meth[outcome] <- "2l.pan"
    pm[outcome, cl] <- -2L
    for (v in pred) if (v != outcome && v %in% colnames(pm))
      pm[outcome, v] <- if (v %in% random_vars) 2L else 1L
    wh <- matrix(FALSE, nrow(a), ncol(a), dimnames = list(NULL, names(a)))
    wh[miss, outcome] <- TRUE
    imp <- mice::mice(a, m = as.integer(m), maxit = as.integer(maxit),
                      predictorMatrix = pm, method = meth, where = wh,
                      printFlag = FALSE)
    ans <- lapply(seq_len(m), function(k) {
      x <- mice::complete(imp, k); x[[cl]] <- NULL
      x <- rbind(x, z0); x <- x[order(x$.mi_iirr_row), ]; x$.mi_iirr_row <- NULL
      rownames(x) <- NULL; x
    })
    attr(ans, "imputation_random_used") <- random_info$used
    attr(ans, "imputation_random_dropped") <- random_info$dropped
    ans
  })
}

.mi_iirr_pinv <- function(A, tol = sqrt(.Machine$double.eps)) {
  A <- as.matrix(A); s <- svd(A)
  if (!length(s$d) || !is.finite(max(s$d)) || max(s$d) == 0)
    stop("Estimating-equation bread has rank zero.", call. = FALSE)
  keep <- s$d > tol * max(s$d)
  if (sum(keep) < min(dim(A)))
    stop("Estimating-equation bread is rank deficient; named coefficients are not identified.",
         call. = FALSE)
  s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep])
}

.mi_iirr_gamma_subject_score <- function(g, V, m) {
  eta <- as.vector(V %*% g); ew <- exp(eta - max(eta)); prob <- ew / sum(ew)
  vbar <- colSums(V * prob)
  sweep(sweep(V, 2L, vbar, "-"), 1L, m - sum(m) * prob, "*")
}

.mi_iirr_constant <- function(d, id, vars) {
  for (v in vars) {
    k <- tapply(d[[v]], d[[id]], function(x) length(unique(x[!is.na(x)])))
    if (any(k > 1L, na.rm = TRUE))
      stop("This IIRR implementation requires time-invariant visit covariate `", v, "`.", call. = FALSE)
  }
}

.mi_iirr_gamma <- function(d, id, time, visit_covs) {
  .mi_iirr_constant(d, id, visit_covs)
  ids <- unique(d[[id]]); si <- match(d[[id]], ids); first <- match(ids, d[[id]])
  V <- as.matrix(d[first, visit_covs, drop = FALSE]); storage.mode(V) <- "double"
  if (any(!is.finite(V))) stop("Baseline visit covariates must be finite.", call. = FALSE)
  m <- tabulate(si[is.finite(d[[time]])], nbins = length(ids)); M <- sum(m)
  if (!M) stop("No visit events.", call. = FALSE)
  Es <- colSums(V * m)
  fg <- function(g) {
    lp <- as.vector(V %*% g); a <- max(lp); ew <- exp(lp - a); sw <- sum(ew)
    list(value = M * (a + log(sw)) - sum(Es * g),
         gradient = M * colSums(V * ew) / sw - Es)
  }
  o <- stats::optim(rep(0, ncol(V)), function(g) fg(g)$value,
                    function(g) fg(g)$gradient, method = "BFGS",
                    control = list(reltol = 1e-10, maxit = 1000))
  g <- o$par; names(g) <- visit_covs
  list(gamma = g, ids = ids, si = si, first = first, V = V, m = m, M = M,
       converged = o$convergence == 0L, code = o$convergence,
       score_max = max(abs(fg(g)$gradient)))
}

.mi_iirr_iirr <- function(d, id, time, outcome, visit_covs, long_covs) {
  ga <- .mi_iirr_gamma(d, id, time, visit_covs)
  vars <- unique(c(time, outcome, visit_covs, long_covs))
  ok <- is.finite(d[[time]]) & is.finite(d[[outcome]]) &
        stats::complete.cases(d[, vars, drop = FALSE])
  dd <- d[ok, , drop = FALSE]; sid <- ga$si[ok]
  X <- stats::model.matrix(stats::reformulate(c(long_covs, time)), dd)
  y <- dd[[outcome]]
  Vr <- as.matrix(dd[, visit_covs, drop = FALSE]); storage.mode(Vr) <- "double"
  lp <- as.vector(Vr %*% ga$gamma)
  if (any(abs(lp) > 700)) stop("IIRR linear predictor overflow.", call. = FALSE)
  w <- exp(-lp)
  A <- crossprod(X, X * w); Ai <- .mi_iirr_pinv(A)
  beta <- as.vector(Ai %*% crossprod(X, w * y)); names(beta) <- colnames(X)

  q <- length(ga$gamma); p <- length(beta); n <- length(ga$ids)
  psi <- function(par) {
    g <- par[seq_len(q)]; b <- par[q + seq_len(p)]

    Ug <- .mi_iirr_gamma_subject_score(g, ga$V, ga$m)
    wr <- exp(-as.vector(Vr %*% g))
    rb <- y - as.vector(X %*% b)
    tmp <- rowsum(X * (wr * rb), sid, reorder = FALSE)
    Ub <- matrix(0, n, p); Ub[as.integer(rownames(tmp)), ] <- tmp
    cbind(Ug, Ub)
  }
  par <- c(ga$gamma, beta); P0 <- psi(par)
  total <- function(z) colSums(psi(z))
  J <- matrix(NA_real_, length(par), length(par))
  for (k in seq_along(par)) {
    h <- 1e-5 * (abs(par[k]) + 1); a <- par; b <- par
    a[k] <- a[k] + h; b[k] <- b[k] - h
    J[, k] <- (total(a) - total(b)) / (2 * h)
  }
  bread <- -J; Binv <- .mi_iirr_pinv(bread)
  Vfull <- Binv %*% crossprod(P0) %*% t(Binv)
  nm <- c(paste0("gamma:", visit_covs), names(beta)); dimnames(Vfull) <- list(nm, nm)
  Vbeta <- Vfull[q + seq_len(p), q + seq_len(p), drop = FALSE]
  list(coef = beta, vcov = Vbeta, vcov_full = Vfull, gamma = ga$gamma,
       converged = ga$converged, gamma_code = ga$code,
       gamma_score_max = ga$score_max, condition = kappa(bread),
       n_visit = ga$M, n_outcome = nrow(dd), n_subject = n,
       zero_visit = sum(ga$m == 0L))
}

.mi_iirr_pool <- function(fits) {
  nm <- Reduce(intersect, lapply(fits, function(x) names(x$coef)))
  Q <- do.call(rbind, lapply(fits, function(x) x$coef[nm]))
  W <- Reduce(`+`, lapply(fits, function(x) x$vcov[nm, nm, drop = FALSE])) / length(fits)
  B <- if (length(fits) > 1L) stats::cov(Q) else matrix(0, length(nm), length(nm))
  if (length(nm) == 1L) B <- matrix(B, 1, 1); dimnames(B) <- list(nm, nm)
  mm <- length(fits); T <- W + (1 + 1 / mm) * B
  u <- pmax(diag(W), 0); b <- pmax(diag(B), 0)
  riv <- ifelse(u > 0, (1 + 1 / mm) * b / u, ifelse(b > 0, Inf, 0))
  df <- if (mm <= 1L) rep(Inf, length(nm)) else
    ifelse(b > 0, (mm - 1) * (1 + 1 / pmax(riv, .Machine$double.eps))^2, Inf)
  fmi <- ifelse(is.finite(riv), (riv + 2 / (df + 3)) / (riv + 1), 1)
  names(riv) <- names(df) <- names(fmi) <- nm
  list(coef = colMeans(Q), vcov = T, W = W, B = B, rubin_df = df,
       relative_increase_variance = riv, fraction_missing_information = fmi)
}

.mi_iirr_compact_analysis_fit <- function(z) {
  beta <- z$coef
  Vbeta <- z$vcov
  se_beta <- stats::setNames(rep(NA_real_, length(beta)), names(beta))
  if (is.matrix(Vbeta) && nrow(Vbeta) == length(beta)) {
    se_beta[] <- sqrt(pmax(diag(Vbeta), 0))
  }
  gamma <- z$gamma
  se_gamma <- stats::setNames(rep(NA_real_, length(gamma)), names(gamma))
  Vfull <- z$vcov_full
  gamma_names <- paste0("gamma:", names(gamma))
  if (is.matrix(Vfull) && all(gamma_names %in% rownames(Vfull))) {
    se_gamma[] <- sqrt(pmax(diag(Vfull[gamma_names, gamma_names, drop = FALSE]), 0))
  }
  list(
    coefficients = beta,
    coef = beta,
    se = se_beta,
    vcov = Vbeta,
    gamma = gamma,
    gamma_se = se_gamma,
    vcov_full = Vfull,
    converged = z$converged,
    gamma_code = z$gamma_code,
    gamma_score_max = z$gamma_score_max,
    condition = z$condition,
    n_visit = z$n_visit,
    n_outcome = z$n_outcome,
    n_subject = z$n_subject,
    zero_visit = z$zero_visit
  )
}

.fit_mi_iirr <- function(
    data, id = "id", time = "time", outcome = "Y", obs_indicator = "R",
    visit_covs = c("Z", "X"), long_covs = c("Z", "X"), random_covs = "Z",
    m = 5L, maxit = 5L, seed = 1L, censor_col = if ("C" %in% names(data)) "C" else NULL,
    keep_completed = FALSE) {
  if (length(m) != 1L || !is.finite(m) || m < 1 || m != as.integer(m) ||
      length(maxit) != 1L || !is.finite(maxit) || maxit < 1 || maxit != as.integer(maxit))
    stop("`m` and `maxit` must be positive integers.", call. = FALSE)
  d <- .mi_iirr_check(data, c(id, time, outcome, obs_indicator, visit_covs, long_covs, random_covs,
                          if (!is.null(censor_col)) censor_col))
  if (!is.null(censor_col)) {
    ci <- as.numeric(d[match(unique(d[[id]]), d[[id]]), censor_col])
    if (any(!is.finite(ci)))
      stop("Every subject needs a finite censoring time.", call. = FALSE)
    if (diff(range(ci)) > 1e-10)
      stop("MI-IIRR: common-risk IIRR does not support varying censoring times.", call. = FALSE)
  }
  completed <- .mi_iirr_impute(d, id, time, outcome, obs_indicator,
                           unique(c(visit_covs, long_covs)), random_covs,
                           m, maxit, seed)
  imp_random_used <- attr(completed, "imputation_random_used")
  imp_random_dropped <- attr(completed, "imputation_random_dropped")
  fits <- lapply(completed, .mi_iirr_iirr, id = id, time = time, outcome = outcome,
                 visit_covs = visit_covs, long_covs = long_covs)
  pool <- .mi_iirr_pool(fits)
  sig <- vapply(completed, function(z) {
    yy <- z[[outcome]]; sum((seq_along(yy) %% 1009L) * ifelse(is.finite(yy), yy, 0))
  }, 0.0)
  conv <- all(vapply(fits, `[[`, logical(1), "converged"))
  ans <- list(method_key = "mi_iirr", method = "MI+IIRR",
              estimand = "Visit-process IIRR-weighted longitudinal coefficient pooled over MAR multiple imputations",
              coefficients = pool$coef, vcov = pool$vcov,
              se = sqrt(pmax(diag(pool$vcov), 0)),
              vcov_type = "Rubin using within-imputation stacked IIRR sandwich",
              nuisance = list(gamma = fits[[1]]$gamma,
                              within_vcov = pool$W, between_vcov = pool$B),
              converged = conv,
              diagnostics = list(ok = isTRUE(conv && all(is.finite(pool$coef)) &&
                                               all(is.finite(pool$vcov))),
                                 m_requested = as.integer(m), m_success = length(fits),
                                 rubin_df = pool$rubin_df,
                                 relative_increase_variance = pool$relative_increase_variance,
                                 fraction_missing_information = pool$fraction_missing_information,
                                 seed = as.integer(seed), n_subjects = fits[[1]]$n_subject,
                                 n_visits = fits[[1]]$n_visit,
                                 n_observed_before_imputation = sum(is.finite(d[[time]]) &
                                   !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
                                   is.finite(d[[outcome]])),
                                 n_imputed_per_dataset = vapply(completed, function(z)
                                   sum(is.finite(d[[time]]) &
                                         (is.na(d[[obs_indicator]]) | d[[obs_indicator]] != 1 |
                                            !is.finite(d[[outcome]])) &
                                         is.finite(z[[outcome]])), 0L),
                                 original_R_preserved = all(vapply(completed, function(z)
                                   identical(z[[obs_indicator]], d[[obs_indicator]]), logical(1))),
                                 imputation_random_used = imp_random_used,
                                 imputation_random_dropped_as_aliased = imp_random_dropped,
                                 downstream_process = "all actual visits N(t); imputed rows are outcomes, not new events",
                                 zero_visit_subjects = fits[[1]]$zero_visit,
                                 gamma_score_max = fits[[1]]$gamma_score_max,
                                 condition_number = vapply(fits, `[[`, 0.0, "condition"),
                                 imputation_signature = sig),
              assumptions = c("Congenial 2l.pan MAR imputation; not latent-variable MNAR",
                              "Common follow-up and time-invariant visit covariates",
                              "IIRR visiting-at-random/separability assumptions"),
              call = match.call())
  storage_started <- proc.time()[["elapsed"]]
  ans$analysis_fits <- lapply(fits, .mi_iirr_compact_analysis_fit)
  ans$diagnostics$analysis_fit_storage_seconds <-
    proc.time()[["elapsed"]] - storage_started
  ans$diagnostics$analysis_fit_storage_bytes <-
    as.numeric(object.size(ans$analysis_fits))
  if (keep_completed) ans$completed_data <- completed
  class(ans) <- c("mi_iirr")
  ans
}
