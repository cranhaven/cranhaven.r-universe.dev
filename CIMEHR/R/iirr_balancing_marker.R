.iirr_balancing_marker_stable_risk <- function(log_weight, values, times, entry, censor) {
  values <- as.matrix(values)
  n <- nrow(values)
  stopifnot(length(log_weight) == n, length(entry) == n, length(censor) == n)
  if (anyNA(log_weight) || any(log_weight == Inf) || any(!is.finite(values)))
    stop("Non-finite risk-set inputs.", call. = FALSE)
  ly_stable_risk_cpp(log_weight, values, times, entry, censor)
}

.iirr_balancing_marker_stable_interval <- function(times, log_weight, values, entry, censor) {
  values <- as.matrix(values)
  if (anyNA(log_weight) || any(log_weight == Inf) || any(!is.finite(values)))
    stop("Non-finite interval-sum inputs.", call. = FALSE)
  ly_stable_interval_cpp(times, log_weight, values, entry, censor)
}

.iirr_balancing_marker_stable_compensator <- function(eta, V, tu, dn, logs0, vbar, entry, censor) {
  z <- .iirr_balancing_marker_stable_interval(tu, log(dn) - logs0, vbar, entry, censor)
  scale <- exp(eta + z$log_sum)
  (V - z$mean) * scale
}

.iirr_balancing_marker_check <- function(data, vars) {
  d <- as.data.frame(data)
  miss <- setdiff(unique(vars), names(d))
  if (length(miss))
    stop("Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]]))
    stop("Subject id cannot be missing.", call. = FALSE)
  d
}

.iirr_balancing_marker_inv <- function(A, tol = sqrt(.Machine$double.eps)) {
  z <- svd(as.matrix(A))
  if (!length(z$d) || max(z$d) == 0)
    stop("Singular derivative.", call. = FALSE)
  k <- z$d > tol * max(z$d)
  if (!all(k))
    stop("Balancing/estimating equations are rank deficient.", call. = FALSE)
  z$v %*% (t(z$u)/z$d)
}

.iirr_balancing_marker_jac <- function(fun, x, fx = fun(x)) {
  J <- matrix(NA_real_, length(fx), length(x))
  for (j in seq_along(x)) {
    h <- 1e-05 * (1 + abs(x[j]))
    a <- b <- x
    a[j] <- a[j] + h
    b[j] <- b[j] - h
    J[, j] <- (fun(a) - fun(b))/(2 * h)
  }
  J
}

.iirr_balancing_marker_prep <- function(d, id, time, outcome, obs_indicator, visit_covs, entry_col, censor_col, process) {
  ids <- unique(d[[id]])
  si <- match(d[[id]], ids)
  first <- match(ids, d[[id]])
  V <- as.matrix(d[first, visit_covs, drop = FALSE])
  storage.mode(V) <- "double"
  if (any(!is.finite(V)))
    stop("Baseline visit covariates must be finite.", call. = FALSE)
  for (v in visit_covs) if (any(is.finite(d[[v]]) & abs(d[[v]] - d[[v]][first][si]) > 1e-10, na.rm = TRUE))
    stop("Time-varying visit covariates are not supported by this risk-set calibration: ", v, call. = FALSE)
  ft <- is.finite(d[[time]])
  if (!any(ft))
    stop("No visits.", call. = FALSE)
  entry <- if (is.null(entry_col))
    rep(-Inf, length(ids))
  else as.numeric(d[[entry_col]][first])
  if (is.null(censor_col)) {
    censor <- rep(max(d[[time]][ft]), length(ids))
    csrc <- "common maximum observed time"
  }
  else {
    censor <- as.numeric(d[[censor_col]][first])
    csrc <- censor_col
  }
  if ((!is.null(entry_col) && any(!is.finite(entry))) || any(!is.finite(censor)))
    stop("Invalid entry/censor times.", call. = FALSE)
  if (any(entry > censor))
    stop("Entry time cannot exceed censoring time.", call. = FALSE)
  visit <- ft & d[[time]] > entry[si]
  marker <- visit & !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1
  ev <- if (process == "marker")
    marker
  else visit
  oy <- marker & is.finite(d[[outcome]])
  list(ids = ids, si = si, V = V, entry = entry, censor = censor, event = which(ev), outcome = which(oy), visit = which(visit),
       marker = which(marker), censor_source = csrc)
}

.iirr_balancing_marker_gamma <- function(g, V, ei, et, entry, censor, individual = FALSE, tu = sort(unique(et)), em = match(et,
                                                                                                                            tu), dn = tabulate(em, nbins = length(tu))) {
  eta <- drop(V %*% g)
  risk <- .iirr_balancing_marker_stable_risk(eta, V, tu, entry, censor)
  if (any(!is.finite(risk$log_sum)))
    stop("Empty risk set at an event time.", call. = FALSE)
  vb <- risk$mean
  ls0 <- risk$log_sum
  parts <- iib_gamma_parts(V, vb, risk$covariance, dn, ei, em)
  U <- parts$U
  if (individual)
    U <- U - .iirr_balancing_marker_stable_compensator(eta, V, tu, dn, ls0, vb, entry, censor)
  list(objective = sum(dn * ls0) - sum(eta[ei]), score = colSums(U), individual = U, information = parts$information,
       event_times = tu, risk_mean = vb, eta = eta, log_sum = ls0, n_risk = risk$n_risk)
}

.iirr_balancing_marker_basis_raw <- function(t, V, t_center, t_scale) {
  ts <- (t - t_center)/t_scale
  H <- cbind(`(mass)` = 1, time = ts, V, `time:V` = V * ts)
  colnames(H) <- c("(mass)", "balance_time", paste0("balance_", colnames(V)), paste0("balance_time:", colnames(V)))
  H
}

.iirr_balancing_marker_logmeanexp <- function(risk) {
  if (any(!is.finite(risk$log_sum)) || any(risk$n_risk <= 0L))
    stop("Empty stabilization risk set.", call. = FALSE)
  risk$log_sum - log(risk$n_risk)
}

.iirr_balancing_marker_interval_sum <- function(tu, value, entry, censor) {
  value <- as.matrix(value)
  h <- ncol(value)
  cs <- rbind(rep(0, h), apply(value, 2L, cumsum))
  lo <- findInterval(entry, tu)
  hi <- findInterval(censor, tu)
  cs[hi + 1L, , drop = FALSE] - cs[lo + 1L, , drop = FALSE]
}

.iirr_balancing_marker_planned_interval <- function(plan, times, log_weight, values, entry, censor) {
  values <- as.matrix(values)
  if (anyNA(log_weight) || any(log_weight == Inf) || !iib_all_finite(values))
    stop("Non-finite interval-sum inputs.", call. = FALSE)
  if (!identical(log_weight, plan$log_weight) || !identical(nrow(values), plan$rows)) {
    plan$steps <- iib_interval_plan(times, log_weight, nrow(values), entry, censor)
    plan$log_weight <- log_weight
    plan$rows <- nrow(values)
  }
  iib_interval_apply(plan$steps, values)
}

.iirr_balancing_marker_lmean_case_effect <- function(eta, log_sum, tu, event_total, neg_log_n_risk, entry, censor, plans) {
  a <- .iirr_balancing_marker_planned_interval(plans$a, tu, -log_sum, event_total, entry, censor)
  b <- .iirr_balancing_marker_planned_interval(plans$b, tu, neg_log_n_risk, event_total, entry, censor)
  a$mean * exp(eta + a$log_sum) - b$mean * exp(b$log_sum)
}

.fit_iirr_balancing_marker <- function(data, id = "id", time = "time", outcome = "Y", obs_indicator = "R", visit_covs = c("Z",
                                                                                                                          "X"), long_covs = c("Z", "X"), random_covs = NULL, process = c("marker", "visit"), entry_col = NULL, censor_col = NULL,
                                       include_time = TRUE, balance_time_interactions = TRUE, calibration_tol = 1e-08, optimizer_control = list(maxit = 2000L),
                                       return_subject_scores = FALSE) {
  .rate_cache <- new.env(parent = emptyenv())
  .rate_parts <- function(g, individual = FALSE) {
    if (!identical(g, .rate_cache$gamma)) {
      .rate_cache$gamma <- g
      .rate_cache$ordinary <- NULL
      .rate_cache$individual <- NULL
    }
    key <- if (isTRUE(individual))
      "individual"
    else "ordinary"
    if (is.null(.rate_cache[[key]]))
      .rate_cache[[key]] <- .iirr_balancing_marker_gamma(g, S$V, ei, et, S$entry, S$censor, individual = individual, tu = tu,
                                                         em = em, dn = dn)
    .rate_cache[[key]]
  }
  process <- match.arg(process)
  d <- .iirr_balancing_marker_check(data, c(id, time, outcome, obs_indicator, visit_covs, long_covs, entry_col, censor_col))
  S <- .iirr_balancing_marker_prep(d, id, time, outcome, obs_indicator, visit_covs, entry_col, censor_col, process)
  if (!length(S$event) || !length(S$outcome))
    stop("No process events or recorded outcomes.", call. = FALSE)
  ei <- S$si[S$event]
  et <- d[[time]][S$event]
  tu <- sort(unique(et))
  em <- match(et, tu)
  dn <- tabulate(em, nbins = length(tu))
  og <- stats::optim(rep(0, ncol(S$V)), function(g) .rate_parts(g)$objective, gr = function(g) -.rate_parts(g)$score,
                     method = "BFGS", control = optimizer_control, hessian = TRUE)
  gpol <- og$par
  for (it in seq_len(25L)) {
    pp <- .rate_parts(gpol)
    if (max(abs(pp$score)) <= 1e-09 * (1 + length(et)))
      break
    step <- drop(.iirr_balancing_marker_inv(pp$information) %*% pp$score)
    fac <- 1
    accepted <- FALSE
    while (fac >= 2^-20) {
      cand <- gpol + fac * step
      nv <- .rate_parts(cand)$objective
      if (is.finite(nv) && nv <= pp$objective + 1e-12 * (1 + abs(pp$objective))) {
        gpol <- cand
        accepted <- TRUE
        break
      }
      fac <- fac/2
    }
    if (!accepted)
      break
  }
  og$par <- gpol
  gs <- .rate_parts(og$par)
  og$value <- gs$objective
  og$hessian <- gs$information
  if (max(abs(gs$score)) > 1e-07 * (1 + length(et)))
    stop("Rate-model fit failed.", call. = FALSE)
  tc <- mean(et)
  ts <- stats::sd(et)
  if (!is.finite(ts) || ts <= 0)
    ts <- max(diff(range(et)), 1)
  He <- .iirr_balancing_marker_basis_raw(et, S$V[ei, , drop = FALSE], tc, ts)
  if (!balance_time_interactions)
    He <- He[, seq_len(2 + ncol(S$V)), drop = FALSE]
  bal <- iib_balance_sweep(S$V, tu, S$entry, S$censor, order(S$entry), order(S$censor))
  if (any(bal$n_risk <= 0L))
    stop("Empty balancing risk set.", call. = FALSE)
  nRisk <- bal$n_risk
  meanV <- matrix(bal$mean, length(tu), ncol(S$V), dimnames = list(NULL, colnames(S$V)))
  Hbar <- .iirr_balancing_marker_basis_raw(tu, meanV, tc, ts)
  if (!balance_time_interactions)
    Hbar <- Hbar[, seq_len(ncol(He)), drop = FALSE]
  target <- colSums(Hbar * dn)
  ak <- dn/nRisk
  bk <- ak * ((tu - tc)/ts)
  ca <- c(0, cumsum(ak))
  cb <- c(0, cumsum(bk))
  lo <- findInterval(S$entry, tu) + 1L
  hi <- findInterval(S$censor, tu)
  at <- hi >= lo
  sa <- sb <- numeric(length(S$ids))
  sa[at] <- ca[hi[at] + 1L] - ca[lo[at]]
  sb[at] <- cb[hi[at] + 1L] - cb[lo[at]]
  target_by_subject <- unname(cbind(sa, sb, S$V * sa, S$V * sb))
  if (!balance_time_interactions)
    target_by_subject <- target_by_subject[, seq_len(ncol(He)), drop = FALSE]
  event_target_direct <- cimehr_group_sum(Hbar[em, , drop = FALSE], ei, length(S$ids))
  target_risk_bar <- .iirr_balancing_marker_interval_sum(tu, Hbar * (dn/nRisk), S$entry, S$censor)
  target_case_derivative <- event_target_direct + target_by_subject - target_risk_bar
  hrank <- qr(He)$rank
  if (hrank < ncol(He))
    stop("Balancing basis is not identified by process events.", call. = FALSE)
  VE <- S$V[ei, , drop = FALSE]
  bw <- exp(-drop(VE %*% og$par) + .iirr_balancing_marker_logmeanexp(gs)[em])
  .dual_cache <- new.env(parent = emptyenv())
  dual_weights <- function(a) {
    if (!identical(a, .dual_cache$a)) {
      .dual_cache$a <- a
      .dual_cache$z <- drop(He %*% a)
      .dual_cache$w <- bw * exp(.dual_cache$z)
    }
    .dual_cache
  }
  dual <- function(a) {
    dw <- dual_weights(a)
    if (max(dw$z) > 700)
      return(.Machine$double.xmax/100)
    sum(dw$w) - sum(target * a)
  }
  grad <- function(a) colSums(He * dual_weights(a)$w) - target
  oa <- stats::optim(rep(0, ncol(He)), dual, gr = grad, method = "BFGS", control = optimizer_control, hessian = TRUE)
  aa <- oa$par
  for (it in seq_len(30L)) {
    gg <- grad(aa)
    if (max(abs(gg))/(1 + max(abs(target))) <= calibration_tol/10)
      break
    ww <- dual_weights(aa)$w
    HH <- crossprod(He, He * ww)
    step <- drop(.iirr_balancing_marker_inv(HH) %*% gg)
    old <- dual(aa)
    fac <- 1
    repeat {
      cand <- aa - fac * step
      nv <- dual(cand)
      if (is.finite(nv) && nv <= old || fac < 2^-20)
        break
      fac <- fac/2
    }
    aa <- cand
  }
  oa$par <- aa
  gres <- grad(oa$par)
  relres <- max(abs(gres))/(1 + max(abs(target)))
  if (is.finite(relres) && relres > calibration_tol) {
    for (refine in seq_len(10L)) {
      gg <- grad(aa)
      if (max(abs(gg))/(1 + max(abs(target))) <= calibration_tol/10)
        break
      ww <- dual_weights(aa)$w
      HH <- crossprod(He, He * ww)
      step <- drop(.iirr_balancing_marker_inv(HH) %*% gg)
      fac <- 1
      accepted <- FALSE
      while (fac >= 2^-20) {
        change <- drop(He %*% (-fac * step))
        difference <- sum(ww * (expm1(change) - change)) - fac * sum(gg * step)
        if (is.finite(difference) && difference <= 0) {
          aa <- aa - fac * step
          accepted <- TRUE
          break
        }
        fac <- fac/2
      }
      if (!accepted)
        break
    }
    oa$par <- aa
    gres <- grad(oa$par)
    relres <- max(abs(gres))/(1 + max(abs(target)))
  }
  if (oa$convergence != 0L || !is.finite(relres) || relres > calibration_tol)
    stop(sprintf("Risk-set calibration failed (relative imbalance %.3g).", relres), call. = FALSE)
  orow <- S$outcome
  oi <- S$si[orow]
  ot <- d[[time]][orow]
  otu <- sort(unique(ot))
  om <- match(ot, otu)
  otk <- match(ot, tu)
  otuk <- match(otu, tu)
  nRiskO <- nRisk[otuk]
  nlRisk <- -log(nRisk)
  nlRiskO <- -log(nRiskO)
  Ho <- .iirr_balancing_marker_basis_raw(ot, S$V[oi, , drop = FALSE], tc, ts)
  if (!balance_time_interactions)
    Ho <- Ho[, seq_len(ncol(He)), drop = FALSE]
  VO <- S$V[oi, , drop = FALSE]
  wf <- exp(-drop(VO %*% og$par) + .iirr_balancing_marker_logmeanexp(gs)[otk] + drop(Ho %*% oa$par))
  wf <- wf/mean(wf)
  rhs <- unique(c(long_covs, if (include_time) time))
  mf <- stats::model.frame(stats::reformulate(rhs, response = outcome), d[orow, , drop = FALSE], na.action = stats::na.fail)
  X <- stats::model.matrix(stats::terms(mf), mf)
  y <- stats::model.response(mf)
  beta <- drop(.iirr_balancing_marker_inv(crossprod(X, X * wf)) %*% crossprod(X, wf * y))
  names(beta) <- colnames(X)
  q <- length(og$par)
  aN <- length(oa$par)
  p <- length(beta)
  n <- length(S$ids)
  .ua_cache <- new.env(parent = emptyenv())
  tu_plans <- list(a = new.env(parent = emptyenv()), b = new.env(parent = emptyenv()))
  otu_plans <- list(a = new.env(parent = emptyenv()), b = new.env(parent = emptyenv()))
  scores <- function(par) {
    g <- par[seq_len(q)]
    a <- par[q + seq_len(aN)]
    b <- par[q + aN + seq_len(p)]
    G <- .rate_parts(g, TRUE)
    lme <- .iirr_balancing_marker_logmeanexp(G)
    if (!identical(par[seq_len(q + aN)], .ua_cache$par)) {
      Ea <- iib_scatter2(He, exp(-drop(VE %*% g) + lme[em] + drop(He %*% a)), NULL, ei, n, em, length(tu))
      .ua_cache$Ua <- Ea[[1L]] + .iirr_balancing_marker_lmean_case_effect(G$eta, G$log_sum, tu, Ea[[2L]], nlRisk, S$entry,
                                                                          S$censor, tu_plans) - target_case_derivative
      .ua_cache$par <- par[seq_len(q + aN)]
    }
    Eb <- iib_scatter2(X, exp(-drop(VO %*% g) + lme[otk] + drop(Ho %*% a)), y - drop(X %*% b), oi, n, om, length(otu))
    Ub <- Eb[[1L]] + .iirr_balancing_marker_lmean_case_effect(G$eta, G$log_sum[otuk], otu, Eb[[2L]], nlRiskO, S$entry, S$censor,
                                                              otu_plans)
    cbind(G$individual, .ua_cache$Ua, Ub)
  }
  par <- c(og$par, oa$par, beta)
  psi <- scores(par)
  A <- .iirr_balancing_marker_jac(function(z) colSums(scores(z)), par, colSums(psi))
  meat <- crossprod(psi)
  Ai <- .iirr_balancing_marker_inv(A)
  Vall <- Ai %*% meat %*% t(Ai)
  ix <- q + aN + seq_len(p)
  Vb <- Vall[ix, ix, drop = FALSE]
  dimnames(Vb) <- list(names(beta), names(beta))
  pn <- c(paste0("gamma:", visit_covs), paste0("calibration:", colnames(He)), paste0("beta:", names(beta)))
  dimnames(A) <- dimnames(meat) <- dimnames(Vall) <- list(pn, pn)
  ans <- list(method_key = "iirr_balancing_marker", method = "IIRR risk-set entropy balancing", estimand = "marginal longitudinal mean coefficient",
              coefficients = beta, vcov = Vb, se = setNames(sqrt(pmax(diag(Vb), 0)), names(beta)), vcov_type = "case-derivative stacked subject sandwich (Breslow-profiled rate, empirical risk-set calibration, outcome)",
              nuisance = list(gamma = setNames(og$par, visit_covs), calibration = setNames(oa$par, colnames(He)), balance_target = target,
                              breslow_event_times = gs$event_times, breslow_increment = dn * exp(-gs$log_sum)), inference = list(bread = A,
                                                                                                                                 meat = meat, vcov_all = Vall, subject_scores = if (isTRUE(return_subject_scores)) {
                                                                                                                                   dimnames(psi) <- list(as.character(S$ids), pn)
                                                                                                                                   psi
                                                                                                                                 } else NULL), converged = TRUE, diagnostics = list(ok = TRUE, process = process, n_subjects = length(S$ids),
                                                                                                                                                                                    n_visit_events = length(S$visit), n_marker_events = length(S$marker), n_process_events = length(S$event),
                                                                                                                                                                                    n_outcomes = length(orow), zero_event_subjects = sum(tabulate(ei, nbins = n) == 0), censor_source = S$censor_source,
                                                                                                                                                                                    balance_rank = hrank, relative_imbalance = relres, effective_sample_size = sum(wf)^2/sum(wf^2),
                                                                                                                                                                                    weight_range = range(wf), stacked_condition = kappa(A), stacked_score_sum_max = max(abs(colSums(psi)))),
              assumptions = c("Entropy calibration basis is prespecified and feasible", "Risk-set targets use every represented subject, including zero-event subjects",
                              "The selected process, marker by default, is the process governing outcome observability"), call = match.call())
  class(ans) <- c("iirr_balancing_marker")
  ans
}


IIRR_balancing <- function(data,process=c('marker','visit'),...) {
  process<-match.arg(process);fit_method(method=if(process=='visit')'iirr_balancing_visit' else 'iirr_balancing_marker',data=data,...)
}

Inverse_intensity_rate_ratio_balancing <- IIRR_balancing
