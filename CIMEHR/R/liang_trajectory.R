.liang_trajectory_increment_summary <- function(time, residual, log_risk,
    grid = sort(unique(time))) {
  if (length(log_risk) == 1L) log_risk <- rep(log_risk, length(grid))
  index <- match(time, grid)
  number <- tabulate(index, nbins = length(grid))
  numerator <- numeric(length(grid))
  sums <- rowsum(as.numeric(residual), index, reorder = FALSE)
  numerator[as.integer(rownames(sums))] <- sums[, 1L]
  offset <- stats::median(log_risk[is.finite(log_risk)])
  inverse_risk <- exp(offset - log_risk)
  list(type = "increments",
    increments = data.frame(time = grid, dA = numerator * inverse_risk,
      dLambda = number * inverse_risk),
    risk_log_scale = offset,
    normalization = paste("Empirical outcome-event measure with at-risk m_i/Lambda_i weights;",
      "dLambda is not a clinic-visiting baseline when only recorded outcomes are analyzed"))
}

.liang_trajectory_parametric_summary <- function(terms, coefficients) {
  list(type = "parametric", terms = terms, coefficients = coefficients)
}

.liang_trajectory_window <- function(d, time, censor_col=NULL, entry_col=NULL) {
  actual <- d[[time]][is.finite(d[[time]])]
  lower <- if(is.null(entry_col)) 0 else {
    entry <- d[[entry_col]][is.finite(d[[entry_col]])]
    if(length(entry))min(entry)else 0
  }
  upper <- if(is.null(censor_col))max(actual)else max(d[[censor_col]],na.rm=TRUE)
  c(lower,upper)
}

.liang_trajectory_uses_smoothing <- function(object) {
  check <- function(b) {
    if(identical(b$type,"parametric"))return(FALSE)
    if(identical(b$type,"mi"))return(any(vapply(b$components,check,logical(1))))
    TRUE
  }
  check(object$prediction$baseline)
}

.liang_trajectory_spec <- function(baseline, d, id, time, long_covs,
    coefficients, time_range = .liang_trajectory_window(d,time),
    population = "analysis subjects") {
  first <- match(unique(d[[id]]), d[[id]])
  list(baseline = baseline, reference = d[first, long_covs, drop = FALSE],
    long_covs = long_covs, coefficients = coefficients[long_covs],
    time_col = time, time_range = time_range, population = population)
}

.liang_trajectory_evaluate_baseline <- function(spec, time_col, times, bandwidth) {
  if (identical(spec$type, "increments")) {
    z <- spec$increments
    return(.trajectory_local_linear(z$time, z$dA, z$dLambda, times, bandwidth))
  }
  if (identical(spec$type, "parametric")) {
    nd <- stats::setNames(data.frame(as.numeric(times)), time_col)
    mf <- stats::model.frame(spec$terms, data = nd, na.action = stats::na.pass)
    X <- stats::model.matrix(spec$terms, data = mf)
    nm <- colnames(X); nm[nm != "(Intercept)"] <- paste0("baseline:", nm[nm != "(Intercept)"])
    if (any(!nm %in% names(spec$coefficients)))
      stop("The stored Liang baseline basis and coefficients do not align.", call. = FALSE)
    return(drop(X %*% spec$coefficients[nm]))
  }
  if (identical(spec$type, "mi")) {
    curves <- lapply(spec$components, .liang_trajectory_evaluate_baseline,
      time_col = time_col, times = times, bandwidth = bandwidth)
    return(Reduce(`+`, curves) / length(curves))
  }
  stop("The fitted object does not contain a supported Liang baseline summary.", call. = FALSE)
}

.liang_trajectory_baseline <- function(object, times = NULL, bandwidth = NULL) {
  p <- object$prediction
  if (!is.list(p) || is.null(p$baseline))
    stop("The fitted object does not retain a Liang baseline; refit with the current package.", call. = FALSE)
  if (is.null(times)) times <- seq(p$time_range[1L], p$time_range[2L], length.out = 101L)
  if (!is.numeric(times) || any(!is.finite(times)))
    stop("times must be a finite numeric vector.", call. = FALSE)
  data.frame(time = times, fit = .liang_trajectory_evaluate_baseline(
    p$baseline, p$time_col, times, bandwidth))
}

.liang_trajectory_mean <- function(object, newdata = NULL, times = NULL,
    bandwidth = NULL) {
  p <- object$prediction
  if (is.null(newdata)) newdata <- p$reference
  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  missing <- setdiff(p$long_covs, names(newdata))
  if (length(missing)) stop("Missing prediction covariates: ", paste(missing, collapse = ", "), call. = FALSE)
  X <- as.matrix(newdata[, p$long_covs, drop = FALSE]); storage.mode(X) <- "double"
  if (any(!is.finite(X))) stop("Prediction covariates must be finite.", call. = FALSE)
  baseline <- .liang_trajectory_baseline(object, times, bandwidth)
  shifts <- drop(X %*% p$coefficients)
  n <- nrow(newdata); k <- nrow(baseline)
  data.frame(profile = rep(seq_len(n), each = k), time = rep(baseline$time, n),
    fit = rep(baseline$fit, n) + rep(shifts, each = k))
}
