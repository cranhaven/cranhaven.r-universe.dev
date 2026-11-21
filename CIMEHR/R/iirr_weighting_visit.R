
.iirr_weighting_visit_stable_risk <- function(log_weight, values, times, entry, censor) {
  values <- as.matrix(values); n <- nrow(values)
  stopifnot(length(log_weight) == n, length(entry) == n, length(censor) == n)
  if (anyNA(log_weight) || any(log_weight == Inf) || any(!is.finite(values)))
    stop("Non-finite risk-set inputs.", call. = FALSE)
  ly_stable_risk_cpp(log_weight, values, times, entry, censor)
}

.iirr_weighting_visit_stable_interval <- function(times, log_weight, values, entry, censor) {
  values <- as.matrix(values)
  if (anyNA(log_weight)||any(log_weight==Inf)||any(!is.finite(values)))
    stop("Non-finite interval-sum inputs.",call.=FALSE)
  ly_stable_interval_cpp(times, log_weight, values, entry, censor)
}

.iirr_weighting_visit_stable_compensator <- function(eta,V,tu,dn,logs0,vbar,entry,censor){
  z<-.iirr_weighting_visit_stable_interval(tu,log(dn)-logs0,vbar,entry,censor)
  scale<-exp(eta+z$log_sum)
  (V-z$mean)*scale
}

.iirr_weighting_visit_check <- function(data, vars) {
  d <- as.data.frame(data)
  miss <- setdiff(unique(vars), names(d))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  d
}

.iirr_weighting_visit_inv <- function(A, tol = sqrt(.Machine$double.eps)) {
  A <- as.matrix(A); z <- svd(A)
  if (!length(z$d) || max(z$d) == 0) stop("Singular estimating-equation derivative.", call. = FALSE)
  keep <- z$d > tol * max(z$d)
  if (!all(keep)) stop("Estimating equations are rank deficient; inference is unidentified.", call. = FALSE)
  z$v %*% (t(z$u) / z$d)
}

.iirr_weighting_visit_subject_data <- function(d, id, time, outcome, obs_indicator, visit_covs,
                               entry_col, censor_col) {
  ids <- unique(d[[id]]); si <- match(d[[id]], ids); first <- match(ids, d[[id]])
  V <- as.matrix(d[first, visit_covs, drop = FALSE]); storage.mode(V) <- "double"
  if (any(!is.finite(V))) stop("Visit-process covariates must be finite at baseline.", call. = FALSE)
  for (v in visit_covs) {
    z <- d[[v]]; ref <- d[[v]][first][si]
    if (any(is.finite(z) & abs(z - ref) > 1e-10, na.rm = TRUE))
      stop("IIRR-Weighting (visit) currently requires time-invariant visit covariates; column: ", v, call. = FALSE)
  }
  finite_t <- is.finite(d[[time]])
  if (!any(finite_t)) stop("No finite visit times.", call. = FALSE)
  entry <- if (is.null(entry_col)) rep(-Inf, length(ids)) else as.numeric(d[[entry_col]][first])
  if (is.null(censor_col)) {
    censor <- rep(max(d[[time]][finite_t]), length(ids))
    censor_source <- "common maximum observed time"
  } else {
    censor <- as.numeric(d[[censor_col]][first]); censor_source <- censor_col
  }
  if (anyNA(entry) || any(entry == Inf) || any(!is.finite(censor)) || any(censor < entry))
    stop("Every subject needs a finite censoring time and an entry time with entry <= censor.", call. = FALSE)
  tt <- d[[time]]
  in_window <- finite_t & tt > entry[si] & tt <= censor[si]
  marker <- in_window & !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1
  visit <- in_window
  event <- visit
  outcome_rows <- marker & is.finite(d[[outcome]])
  list(ids = ids, si = si, first = first, V = V, entry = entry, censor = censor,
       event_rows = which(event), outcome_rows = which(outcome_rows),
       visit_rows = which(visit), marker_rows = which(marker),
       censor_source = censor_source)
}

# With prepared, log_sum is indexed by risk state and event_times is NULL.
.iirr_weighting_visit_gamma_parts_reference <- function(gamma, V, event_id, event_t, entry, censor, subject_scores = FALSE, prepared = NULL) {
  g<-gamma; ei<-event_id; et<-event_t; individual<-subject_scores
  n<-nrow(V);q<-ncol(V)
  if(is.null(prepared)){tu<-sort(unique(et));em<-match(et,tu);dn<-tabulate(em,nbins=length(tu));rt<-tu;st<-seq_along(tu)}
  else{tu<-prepared$tu;em<-prepared$em;dn<-prepared$dn;rt<-prepared$risk_times;st<-prepared$risk_state}
  eta<-drop(V%*%g)
  risk<-.iirr_weighting_visit_stable_risk(eta,V,rt,entry,censor)
  if(any(!is.finite(risk$log_sum)))stop("Empty risk set at an event time.",call.=FALSE)
  information<-lw_running_cov_sum(risk$covariance,dn,st,q)
  U<-lw_event_score_loop(V,risk$mean,ei,st[em])
  if(individual)U<-U-.iirr_weighting_visit_stable_compensator(eta,V,tu,dn,risk$log_sum[st],risk$mean[st,,drop=FALSE],entry,censor)
  list(objective=lw_indexed_dot(dn,risk$log_sum,st)-lw_indexed_sum(eta,ei),score=colSums(U),subject_score=U,
       information=information,event_times=if(is.null(prepared))tu,log_sum=risk$log_sum,risk_state=st)
}

.iirr_weighting_visit_gamma_geometry <- function(V, event_id, event_t, entry, censor) {
  tu <- sort(unique(event_t)); em <- match(event_t, tu)
  dn <- tabulate(em, nbins = length(tu))

  entered <- findInterval(tu, sort(entry), left.open = TRUE)
  exited <- findInterval(tu, sort(censor), left.open = TRUE)
  first <- c(TRUE, diff(entered) != 0L | diff(exited) != 0L)
  state <- cumsum(first)
  list(tu = tu, em = em, dn = dn, risk_times = tu[first], risk_state = state,
       state_dn = tabulate(state[em], nbins = sum(first)), event_state = state[em],
       event_values = V[event_id, , drop = FALSE],
       balanced_constant_risk = sum(first) == 1L &&
         length(unique(tabulate(event_id, nbins = nrow(V)))) == 1L)
}

.iirr_weighting_visit_gamma_parts <- function(gamma, V, event_id, event_t, entry, censor,
                                subject_scores = FALSE, prepared = NULL) {
  eta <- drop(V %*% gamma)

  if (any(!is.finite(eta)) || any(!is.finite(V)) ||
      !length(event_t) || anyNA(entry) || any(entry == Inf) || any(!is.finite(censor))) {
    return(.iirr_weighting_visit_gamma_parts_reference(gamma, V, event_id, event_t,
                                         entry, censor, subject_scores))
  }
  if (is.null(prepared))
    prepared <- .iirr_weighting_visit_gamma_geometry(V, event_id, event_t, entry, censor)
  G <- prepared; n <- nrow(V); q <- ncol(V)

  if (any(abs(eta) > 500) || any(abs(V) > 1e6) || isTRUE(G$balanced_constant_risk))
    return(.iirr_weighting_visit_gamma_parts_reference(gamma, V, event_id, event_t,
      entry, censor, subject_scores, G))
  risk <- .iirr_weighting_visit_stable_risk(eta, V, G$risk_times, entry, censor)
  if (any(!is.finite(risk$log_sum)))
    stop("Empty risk set at an event time.", call. = FALSE)
  information <- lw_running_cov_sum(risk$covariance, G$state_dn, seq_along(G$state_dn), q)
  U <- lw_centered_group_sum(G$event_values, risk$mean, G$event_state, event_id, n)
  if (isTRUE(subject_scores)) {

    U <- U - .iirr_weighting_visit_stable_compensator(eta, V, G$risk_times, G$state_dn,
      risk$log_sum, risk$mean, entry, censor)
  }
  list(objective = lw_indexed_dot(G$dn, risk$log_sum, G$risk_state) - lw_indexed_sum(eta, event_id),
       score = colSums(U), subject_score = U, information = information,
       event_times = G$tu, log_sum = risk$log_sum, risk_state = G$risk_state)
}

.fit_iirr_weighting_visit <- function(
    data, id = "id", time = "time", outcome = "Y", obs_indicator = "R",
    visit_covs = c("Z", "X"), long_covs = c("Z", "X"), random_covs = NULL,
    entry_col = NULL, censor_col = NULL,
    include_time = TRUE, optimizer_control = list(maxit = 1000L),
    return_subject_scores = FALSE) {
  d <- .iirr_weighting_visit_check(data, c(id, time, outcome, obs_indicator, visit_covs, long_covs,
                          entry_col, censor_col))
  S <- .iirr_weighting_visit_subject_data(d, id, time, outcome, obs_indicator, visit_covs,
                         entry_col, censor_col)
  if (!length(S$event_rows)) stop("The clinic-visit process has no events.", call. = FALSE)
  if (!length(S$outcome_rows)) stop("No recorded outcomes are available.", call. = FALSE)
  eid <- S$si[S$event_rows]; et <- d[[time]][S$event_rows]
  geometry <- .iirr_weighting_visit_gamma_geometry(S$V, eid, et, S$entry, S$censor)

  memo <- new.env(parent = emptyenv())
  memo$gamma <- NULL; memo$ordinary <- NULL; memo$individual <- NULL
  parts <- function(g, individual = FALSE) {
    if (!identical(g, memo$gamma)) {
      memo$gamma <- g; memo$ordinary <- NULL; memo$individual <- NULL
    }
    key <- if (isTRUE(individual)) "individual" else "ordinary"
    if (is.null(memo[[key]])) memo[[key]] <- .iirr_weighting_visit_gamma_parts(
      g, S$V, eid, et, S$entry, S$censor, individual, prepared = geometry)
    memo[[key]]
  }
  og <- stats::optim(rep(0, ncol(S$V)), function(g) parts(g)$objective,
    gr = function(g) -parts(g)$score,
    method = "BFGS", control = optimizer_control, hessian = TRUE)

  gpol <- og$par
  polish_iterations <- 0L
  for (it in seq_len(25L)) {
    pp <- parts(gpol)
    if (max(abs(pp$score)) <= 1e-9 * (1 + length(et))) break
    step <- drop(.iirr_weighting_visit_inv(pp$information) %*% pp$score)
    fac <- 1
    accepted <- FALSE
    while (fac >= 2^-20) {
      candidate <- gpol + fac * step
      candidate_objective <- parts(candidate)$objective
      if (is.finite(candidate_objective) &&
          candidate_objective <= pp$objective + 1e-12 * (1 + abs(pp$objective))) {
        gpol <- candidate
        accepted <- TRUE
        break
      }
      fac <- fac / 2
    }
    if (!accepted) break
    polish_iterations <- it
  }
  og$par <- gpol
  gp <- parts(og$par)
  og$value <- gp$objective
  og$hessian <- gp$information
  if (max(abs(gp$score)) > 1e-7 * (1 + length(et)))
    stop("The proportional-rate nuisance fit did not converge.", call. = FALSE)

  orow <- S$outcome_rows
  rhs <- unique(c(long_covs, if (include_time) time))
  mf <- stats::model.frame(stats::reformulate(rhs, response = outcome),
                           data = d[orow, , drop = FALSE], na.action = stats::na.fail)
  X <- stats::model.matrix(stats::terms(mf), mf); y <- stats::model.response(mf)
  oi <- S$si[orow]
  w <- exp(-drop(S$V[oi, , drop = FALSE] %*% og$par)); w <- w / mean(w)
  H <- crossprod(X, X * w)
  beta <- drop(.iirr_weighting_visit_inv(H) %*% crossprod(X, w * y)); names(beta) <- colnames(X)

  q <- length(og$par); p <- length(beta); n <- length(S$ids)
  subject_scores <- function(par) {
    g <- par[seq_len(q)]; b <- par[q + seq_len(p)]
    Ug <- parts(g, TRUE)$subject_score
    wb <- exp(-drop(S$V[oi, , drop = FALSE] %*% g)); wb <- wb / mean(wb)
    rr <- y - drop(X %*% b); Ub <- lw_group_sum(X * wb * rr, oi, n)
    cbind(Ug, Ub)
  }
  par <- c(og$par, beta)
  psi <- subject_scores(par)

  Vout <- S$V[oi, , drop = FALSE]
  A_bg <- -crossprod(X * (w * (y - drop(X %*% beta))), Vout)
  A <- rbind(
    cbind(-gp$information, matrix(0, q, p)),
    cbind(A_bg, -H)
  )
  meat <- crossprod(psi)
  Ai <- .iirr_weighting_visit_inv(A); Vall <- Ai %*% meat %*% t(Ai)
  theta_names <- c(paste0("gamma:", visit_covs), paste0("beta:", names(beta)))
  dimnames(A) <- dimnames(meat) <- dimnames(Vall) <- list(theta_names, theta_names)
  Vbeta <- Vall[q + seq_len(p), q + seq_len(p), drop = FALSE]
  dimnames(Vbeta) <- list(names(beta), names(beta))
  Vgamma <- Vall[seq_len(q), seq_len(q), drop = FALSE]
  dimnames(Vgamma) <- list(visit_covs, visit_covs)

  ans <- list(
    method_key = "iirr_weighting_visit", method = "IIRR weighting (risk-set corrected)", estimand = "marginal longitudinal mean coefficient",
    coefficients = beta, vcov = Vbeta, se = setNames(sqrt(pmax(diag(Vbeta), 0)), names(beta)),
    vcov_type = paste0("stacked subject sandwich (Breslow-profiled proportional-rate ",
      "nuisance + inverse-rate weighted outcome equation)"),
    nuisance = list(gamma = setNames(og$par, visit_covs),
      gamma_vcov = Vgamma,
      gamma_se = setNames(sqrt(pmax(diag(Vgamma), 0)), visit_covs),
      gamma_information = gp$information, gamma_hessian = og$hessian,
      breslow_event_times = gp$event_times %||% sort(unique(et)),
      breslow_increment = geometry$dn * exp(-gp$log_sum[gp$risk_state])),
    inference = list(bread = A, meat = meat, vcov_all = Vall,
      subject_scores = if (isTRUE(return_subject_scores)) {
        dimnames(psi) <- list(as.character(S$ids), theta_names); psi
      } else NULL),
    converged = TRUE,
    diagnostics = list(ok = TRUE, process = "visit", n_subjects = length(S$ids),
      n_visit_events = length(S$visit_rows), n_marker_events = length(S$marker_rows),
      n_process_events = length(S$event_rows), n_outcomes = length(orow),
      zero_event_subjects = sum(tabulate(eid, nbins = length(S$ids)) == 0L),
      censor_source = S$censor_source, gamma_score_max = max(abs(gp$score)),
      gamma_optimizer_code = og$convergence,
      gamma_newton_polish_iterations = polish_iterations,
      stacked_condition = kappa(A), outcome_rank = qr(X)$rank,
      gamma_subject_score_sum_max = max(abs(colSums(psi[, seq_len(q), drop = FALSE]))),
      outcome_subject_score_sum_max = max(abs(colSums(psi[, q + seq_len(p), drop = FALSE])))),
    assumptions = c("The selected event process follows a proportional-rate model conditional on visit covariates",
      "The event process is fixed to all clinic visits; recorded-marker events define the separate IIRR-Weighting (marker) comparator",
      "The longitudinal target is defined by the stated linear mean model; inverse-rate weighting removes the fitted rate ratio, not the unspecified baseline time rate",
      "Censoring/entry are conditionally independent and all subjects, including zero-event subjects, are retained"),
    call = match.call())
  class(ans) <- c("iirr_weighting_visit")
  ans
}
