.li_select_subjects <- function(d, id, time, outcome, obs_indicator,
                                unobserved_subjects, method) {
  ids <- unique(d[[id]]); si <- match(d[[id]], ids)
  actual <- is.finite(d[[time]])
  observed <- actual & !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
    is.finite(d[[outcome]])
  counts <- tabulate(si[actual], nbins = length(ids))
  measured <- tabulate(si[observed], nbins = length(ids))
  unobserved <- counts > 0L & measured == 0L
  if (unobserved_subjects == "error" && any(unobserved))
    stop(method, ": subjects with visits but no observed outcome cannot be reconstructed; ",
         "unobserved_subjects='error'.", call. = FALSE)
  exclude <- if (unobserved_subjects == "exclude") unobserved else
    rep(FALSE, length(ids))
  keep <- !exclude[si]
  if (any(exclude) && !any(actual[keep]))
    stop("No actual visits remain after excluding subjects without any observed outcome.",
         call. = FALSE)
  list(data = if (any(exclude)) d[keep, , drop = FALSE] else d,
       n_subjects_input = length(ids), n_visits_input = sum(actual),
       n_subjects_excluded = sum(exclude), n_visits_excluded = sum(counts[exclude]))
}

.li_require_completed <- function(d, id, time, outcome, method) {
  actual <- is.finite(d[[time]])
  missing <- actual & !is.finite(d[[outcome]])
  if (any(missing)) {
    message <- paste0(method,
      " requires a finite observed or LI-reconstructed outcome at every actual visit. ",
      sum(missing), " visit(s) in ", length(unique(d[[id]][missing])),
      " subject(s) remain unreconstructed or non-finite. ",
      "Forward/reverse LI requires an observed anchor, positive time gaps and finite reconstruction. ",
      "The fit has stopped without dropping additional visits from the retained analysis cohort. ",
      "With leading_imputation='error', leading missing outcomes are deliberately left unresolved.")
    stop(structure(list(message = message, call = NULL,
                        n_unreconstructed = sum(missing),
                        n_subjects_affected = length(unique(d[[id]][missing])),
                        n_actual_visits = sum(actual)),
                   class = c("li_unreconstructed_error", "error", "condition")))
  }
  invisible(sum(actual))
}

.li_require_design <- function(d, id, vars, method, rows = rep(TRUE, nrow(d))) {
  vars <- unique(vars)
  bad_rows <- rep(FALSE, nrow(d)); bad_vars <- character()
  for (v in vars) {
    x <- d[[v]]
    bad <- if (!is.numeric(x) || !is.null(dim(x))) rep(TRUE, nrow(d)) else !is.finite(x)
    bad <- bad & rows
    if (any(bad)) { bad_rows <- bad_rows | bad; bad_vars <- c(bad_vars, v) }
  }
  if (any(bad_rows))
    stop(structure(list(
      message = paste0(method, " requires finite numeric model covariates on all required rows; ",
        sum(bad_rows), " row(s) in ", length(unique(d[[id]][bad_rows])),
        " subject(s) have unavailable covariates: ", paste(bad_vars, collapse = ", "),
        ". Actual visits cannot be silently omitted from the completed outcome analysis."),
      call = NULL, n_rows = sum(bad_rows),
      n_subjects_affected = length(unique(d[[id]][bad_rows])), covariates = bad_vars),
      class = c("li_incomplete_design_error", "error", "condition")))
  invisible(TRUE)
}

.li_restore_rows <- function(z, o, helper) {
  cl <- oldClass(z); class(z) <- NULL
  z[[helper]] <- NULL
  back <- NULL
  for (j in seq_along(z)) {
    x <- z[[j]]; at <- names(attributes(x))
    if (length(at) && !identical(at, "names")) {
      if (is.null(back)) back <- order(o)
      z[[j]] <- if (length(dim(x)) != 2L) x[o][back] else
        x[o, , drop = FALSE][back, , drop = FALSE]
    }
  }
  class(z) <- cl
  rownames(z) <- NULL
  z
}

# Forward inputs: (id, time)-ordered visits, observed adjacent pairs,
# and outcomes masked at unobserved visits.
.li_sorted_increments <- function(z, id, time, outcome, obs_indicator, li_covs, helper) {
  o <- order(z[[id]], z[[time]], na.last = TRUE)
  data <- .li_restore_rows(z, o, helper)
  actual <- is.finite(z[[time]])
  observed <- actual & !is.na(z[[obs_indicator]]) & z[[obs_indicator]] == 1 &
    is.finite(z[[outcome]])
  y <- z[[outcome]][seq_len(nrow(z))]; y[actual & !observed] <- NA_real_
  ids <- unique(z[[id]][o]); si <- match(z[[id]], ids)
  x <- as.matrix(z[, li_covs, drop = FALSE], rownames.force = FALSE)
  finite_x <- rowSums(!is.finite(x)) == 0L
  rows <- o[actual[o]]
  a <- utils::head(rows, -1L); b <- utils::tail(rows, -1L)
  gap <- z[[time]][b] - z[[time]][a]
  take <- si[a] == si[b] & observed[a] & observed[b] &
    is.finite(gap) & gap > 0 & finite_x[a]
  list(data = data, rows = rows, ids = ids, si = si, unobserved = actual & !observed,
       observed = observed, y = y, x = x, finite_x = finite_x,
       a = a[take], b = b[take], gap = gap[take])
}

.li_plain_numeric <- function(x) is.numeric(x) && !is.object(x) && is.null(dim(x))

.li_forward_recursion <- function(s, z, time, eta) {
  res <- li_forward_cpp(s$rows, s$si, z[[time]], s$y, s$observed, s$x, s$finite_x, eta,
                        .Machine$sizeof.longdouble > 0L)
  if (!is.null(res)) return(res)
  ix <- s$rows; si <- s$si; observed <- s$observed; y <- s$y
  rec <- rep(FALSE, length(y)); n_actual <- length(ix)
  pos <- which(observed[ix])
  pos <- pos[pos < n_actual]
  pos <- pos[si[ix[pos]] == si[ix[pos + 1L]] & !observed[ix[pos + 1L]]]
  yp <- y[ix[pos]]
  pos <- pos + 1L
  while (length(pos)) {
    prev <- ix[pos - 1L]; r <- ix[pos]
    gap <- z[[time]][r] - z[[time]][prev]
    good <- which(is.finite(gap) & gap > 0 & s$finite_x[prev])
    if (!length(good)) break
    pos <- pos[good]; prev <- prev[good]; r <- r[good]; gap <- gap[good]
    yp <- yp[good]
    wv <- cbind(1, s$x[prev, , drop = FALSE], yp)
    yn <- yp + gap * rowSums(sweep(wv, 2L, eta, "*"))
    y[r] <- yn; rec[r] <- TRUE
    next_pos <- pos + 1L
    keep <- next_pos <= n_actual
    keep[keep] <- si[ix[next_pos[keep]]] == si[r[keep]] & !observed[ix[next_pos[keep]]]
    pos <- next_pos[keep]; yp <- yn[keep]
  }
  list(y = y, rec = rec)
}

# TRUE if a subject has actual unobserved visits before its first observed visit.
.li_has_leading_run <- function(rows, si, n_subjects, observed) {
  sr <- si[rows]; ob <- observed[rows]
  anchored <- tabulate(sr[ob], nbins = n_subjects) > 0L
  any(anchored[sr[!duplicated(sr) & !ob]])
}

.li_reverse_recursion <- function(d, time, outcome, y, rows, si, n_subjects, observed, C, eta) {
  res <- if (is.double(y) && .li_plain_numeric(y) && .li_plain_numeric(d[[time]]))
    li_reverse_cpp(rows, si, n_subjects, d[[time]], y, observed, C, eta)
  if (!is.null(res)) return(res)
  groups <- split(rows, factor(si[rows], levels = seq_len(n_subjects)))
  first_observed <- vapply(groups, function(ix) {
    k <- which(observed[ix]); if (length(k)) k[1L] else 0L
  }, integer(1))
  selected <- which(first_observed > 1L)
  runs <- lapply(selected, function(i) rev(groups[[i]][seq_len(first_observed[i] - 1L)]))
  rr <- unlist(runs, use.names = FALSE)
  run <- rep.int(seq_along(runs), lengths(runs))
  depth <- sequence(lengths(runs))
  anchor <- vapply(selected, function(i) groups[[i]][first_observed[i]], integer(1))
  prev <- anchor; yp <- d[[outcome]][anchor]; alive <- rep(TRUE, length(runs))
  rec <- rep(FALSE, length(y))
  for (at in split(seq_along(rr), depth)) {
    g <- run[at]; r <- rr[at]
    keep <- alive[g]; g <- g[keep]; r <- r[keep]
    if (!length(r)) next
    dt <- d[[time]][prev[g]] - d[[time]][r]
    good <- is.finite(dt) & dt > 0
    alive[g[!good]] <- FALSE
    g <- g[good]; r <- r[good]; dt <- dt[good]
    if (!length(r)) next
    w <- cbind(1, C[r, , drop = FALSE], yp[g])
    yn <- yp[g] + dt * drop(w %*% eta)
    good <- is.finite(yn); alive[g[!good]] <- FALSE
    y[r[good]] <- yn[good]; rec[r[good]] <- TRUE
    prev[g[good]] <- r[good]; yp[g[good]] <- yn[good]
  }
  list(y = y, rec = rec)
}

.li_reverse_reconstruct <- function(li, d, id, time, outcome, obs_indicator,
                                    reverse_covs, covariance, leading_imputation, method,
                                    full = TRUE) {
  leading_imputation <- match.arg(leading_imputation, c("reverse_li", "error"))
  li$n_imputed_forward <- li$n_imputed
  li$n_imputed_backward <- 0L
  li$backward <- NULL
  li$reconstructed_backward <- rep(FALSE, nrow(d))
  if (leading_imputation == "error" || li$n_unreconstructed == 0L) return(li)

  actual <- is.finite(d[[time]])
  observed <- actual & !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
    is.finite(d[[outcome]])
  ord <- order(d[[id]], d[[time]], na.last = TRUE)
  rows <- ord[actual[ord]]
  ids <- unique(d[[id]]); si <- match(d[[id]], ids)
  if (!.li_has_leading_run(rows, si, length(ids), observed)) return(li)

  reverse_covs <- unique(reverse_covs)
  .li_require_design(d, id, reverse_covs, method, rows = actual)
  C <- as.matrix(d[, reverse_covs, drop = FALSE]); storage.mode(C) <- "double"
  a <- utils::head(rows, -1L); b <- utils::tail(rows, -1L)
  gap <- d[[time]][b] - d[[time]][a]
  use <- si[a] == si[b] & observed[a] & observed[b] & is.finite(gap) & gap > 0
  a <- a[use]; b <- b[use]; gap <- gap[use]
  W <- cbind(`(Intercept)` = 1, C[a, , drop = FALSE], d[[outcome]][b])
  colnames(W)[ncol(W)] <- paste0("next_", outcome)
  k <- length(a); p <- ncol(W)
  if (k < max(10L, p + 2L))
    stop(method, ": too few observed adjacent increments for reverse LI.", call. = FALSE)
  if (qr(W)$rank < p)
    stop(method, ": reverse linear-increment design is rank deficient; leading outcomes are not identified.",
         call. = FALSE)
  rate <- (d[[outcome]][a] - d[[outcome]][b]) / gap
  ww <- if (identical(covariance, "randomwalk")) gap else rep(1, k)
  A <- crossprod(W, W * ww)
  s <- svd(A)
  identified <- s$d > sqrt(.Machine$double.eps) * max(s$d)
  if (sum(identified) < p)
    stop(method, ": reverse linear-increment design is numerically rank deficient.", call. = FALSE)
  Ai <- s$v %*% (t(s$u) / s$d)
  eta <- drop(Ai %*% crossprod(W, ww * rate)); names(eta) <- colnames(W)

  rv <- .li_reverse_recursion(d, time, outcome, li$data[[outcome]], rows, si, length(ids),
                              observed, C, eta)
  y <- rv$y; rec <- rv$rec
  li$data[[outcome]] <- y
  li$n_imputed_backward <- sum(rec)
  li$n_imputed <- li$n_imputed_forward + li$n_imputed_backward
  li$n_unreconstructed <- sum(actual & !is.finite(y))
  li$reconstructed <- li$reconstructed | rec
  li$reconstructed_backward <- rec
  if (!full) return(li)
  scores <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * (rate - drop(W %*% eta))), si[a], reorder = FALSE)
  scores[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(scores) %*% t(Ai)
  dimnames(Ve) <- list(names(eta), names(eta))
  li$backward <- list(coefficients = eta, vcov = Ve, n_observed_increments = k,
    rank = qr(W)$rank, columns = p, condition = kappa(A), predictors = colnames(C),
    formula = paste0("(previous_", outcome, " - next_", outcome, ")/dt ~ ",
      paste(c(colnames(C), paste0("next_", outcome)), collapse = " + ")),
    weighting = li$weighting)
  li
}

.li_resampler <- function(d, id) {
  ids <- unique(d[[id]])
  if (!length(ids) || any(vapply(d, function(x) !is.null(dim(x)), logical(1))))
    return(function() {
      take <- sample.int(length(ids), length(ids), replace = TRUE)
      out <- lapply(seq_along(take), function(k) {
        x <- d[d[[id]] == ids[take[k]], , drop = FALSE]; x[[id]] <- k; x
      })
      z <- do.call(rbind, out); rownames(z) <- NULL; z
    })
  si <- match(d[[id]], ids); o <- order(si)
  len <- tabulate(si, nbins = length(ids)); start <- cumsum(c(0L, len))[seq_along(ids)]
  at <- attributes(d)
  function() {
    take <- sample.int(length(ids), length(ids), replace = TRUE)
    n <- len[take]; rows <- o[sequence(n, from = start[take] + 1L)]
    z <- lapply(d, `[`, rows)
    z[[id]] <- rep.int(seq_along(take), n)
    at$row.names <- .set_row_names(length(rows))
    attributes(z) <- at
    z
  }
}
