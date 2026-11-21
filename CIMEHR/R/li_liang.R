.li_liang_baseline_design <- function(time_values,time_col,estimate_intercept,baseline_formula,
    prediction_terms=NULL) {
  if(!is.logical(estimate_intercept)||length(estimate_intercept)!=1L||is.na(estimate_intercept))
    stop("estimate_intercept must be TRUE or FALSE.",call.=FALSE)
  if(!inherits(baseline_formula,"formula")||length(baseline_formula)!=2L)
    stop("baseline_formula must be a one-sided formula.",call.=FALSE)
  if(any(!all.vars(baseline_formula)%in%time_col))
    stop("baseline_formula may use only the specified time column: ",time_col,call.=FALSE)
  allowed<-c("+","-","*","/","^",":","(","I","c","poly","ns","bs",
    "log","log1p","exp","expm1","sqrt","sin","cos","tan","abs",
    "pmin","pmax")
  walk<-function(x,inside_transform=FALSE){
    if(!is.call(x))return(invisible(NULL))
    h<-x[[1L]]
    valid<-is.symbol(h)&&as.character(h)%in%allowed
    fn<-if(is.symbol(h))as.character(h) else ""
    if(is.call(h)&&identical(h[[1L]],as.name("::"))&&length(h)==3L){
      ns<-as.character(h[[2L]]);fn<-as.character(h[[3L]])
      valid<-(ns=="stats"&&fn=="poly")||(ns=="splines"&&fn%in%c("ns","bs"))
    }
    if(!valid)stop("baseline_formula supports deterministic time transformations only; unsupported function call.",call.=FALSE)
    if(fn%in%c("poly","ns","bs")&&inside_transform)
      stop("Data-adaptive poly/ns/bs bases must be direct formula terms so their prediction basis can be preserved.",call.=FALSE)
    transformed<-inside_transform||!fn%in%c("+","-","*","/","^",":","(")
    for(a in as.list(x)[-1L])walk(a,transformed)
    invisible(NULL)
  }
  walk(baseline_formula[[2L]])
  safe<-new.env(parent=baseenv())
  safe$poly<-stats::poly;safe$ns<-splines::ns;safe$bs<-splines::bs
  formula_use<-baseline_formula;environment(formula_use)<-safe
  tt<-stats::terms(formula_use)
  if(attr(tt,"intercept")!=1L)
    stop("baseline_formula must include an intercept.",call.=FALSE)
  text<-paste(deparse(formula_use,width.cutoff=500L),collapse=" ")
  if(!estimate_intercept){
    if(length(attr(tt,"term.labels"))||length(all.vars(formula_use)))
      stop("A nonconstant baseline_formula requires estimate_intercept=TRUE; it is not used in profile mode.",call.=FALSE)
    return(list(design=matrix(numeric(),length(time_values),0L),formula=text,terms=NULL,
      assumption="The outcome time baseline is unspecified and removed by the centered profile estimating equation"))
  }
  dd<-setNames(data.frame(as.numeric(time_values)),time_col)
  mf<-stats::model.frame(if(is.null(prediction_terms))formula_use else prediction_terms,
    data=dd,na.action=stats::na.pass)
  fitted_terms<-attr(mf,"terms")
  F<-stats::model.matrix(fitted_terms,data=mf)
  if(nrow(F)!=length(time_values)||any(!is.finite(F)))
    stop("baseline_formula must produce finite rows for every analysis outcome.",call.=FALSE)
  if(!"(Intercept)"%in%colnames(F))stop("baseline_formula must include an intercept.",call.=FALSE)
  nn<-colnames(F);nn[nn!="(Intercept)"]<-paste0("baseline:",nn[nn!="(Intercept)"])
  colnames(F)<-nn
  list(design=F,formula=text,terms=fitted_terms,assumption=if(ncol(F)==1L)
    "Liang-time assumes a constant outcome baseline alpha(t)" else
    paste0("Liang-time assumes alpha(t) lies in the supplied deterministic time basis: ",text))
}

.li_liang_check <- function(data, vars) {
  d <- as.data.frame(data); bad <- setdiff(unique(vars), names(d))
  if (length(bad)) stop("Missing columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  d
}

# Zero-visit placeholders remain in the risk set; anchors require an observation indicator of 1.
.li_liang_select_subjects <- function(d, id, time, outcome, obs_indicator,
                                     unobserved_subjects) {
  ids <- unique(d[[id]]); si <- match(d[[id]], ids)
  actual <- is.finite(d[[time]])
  observed <- actual & !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
    is.finite(d[[outcome]])
  counts <- tabulate(si[actual], nbins = length(ids))
  measured <- tabulate(si[observed], nbins = length(ids))
  exclude <- if (unobserved_subjects == "exclude") counts > 0L & measured == 0L else
    rep(FALSE, length(ids))
  keep <- !exclude[si]
  selected <- if (any(exclude)) d[keep, , drop = FALSE] else d
  if (any(exclude) && !any(actual[keep]))
    stop("No actual visits remain after excluding subjects without any observed outcome.",
         call. = FALSE)
  list(data = selected, n_subjects_input = length(ids), n_visits_input = sum(actual),
       n_subjects_excluded = sum(exclude), n_visits_excluded = sum(counts[exclude]))
}

# Actual visits require finite observed or reconstructed outcomes; time=NA rows are exempt.
.li_liang_require_completed <- function(d, id, time, outcome) {
  actual <- is.finite(d[[time]])
  missing <- actual & !is.finite(d[[outcome]])
  if (any(missing)) {
    n_missing <- sum(missing)
    n_affected <- length(unique(d[[id]][missing]))
    message <- paste0(
      "LI+Liang requires a finite observed or LI-reconstructed outcome at every actual visit. ",
      n_missing, " visit(s) in ", n_affected,
      " subject(s) remain unreconstructed or non-finite. ",
      "Forward/reverse LI requires an observed anchor, positive time gaps and finite reconstruction. ",
      "Subjects with no observed outcome cannot be reconstructed by these increment models. ",
      "The fit has stopped without dropping additional visits from the retained analysis cohort. ",
      "With leading_imputation='error', leading missing outcomes are deliberately left unresolved.")
    stop(structure(list(message = message, call = NULL,
                        n_unreconstructed = n_missing,
                        n_subjects_affected = n_affected,
                        n_actual_visits = sum(actual)),
                   class = c("li_liang_unreconstructed_error", "error", "condition")))
  }
  invisible(sum(actual))
}

# Risk-set covariates are required on zero-visit placeholder rows.
.li_liang_require_design <- function(d, id, vars) {
  vars <- unique(vars)
  bad_rows <- rep(FALSE, nrow(d)); bad_vars <- character()
  for (v in vars) {
    x <- d[[v]]
    bad <- if (!is.numeric(x) || !is.null(dim(x))) rep(TRUE, nrow(d)) else !is.finite(x)
    if (any(bad)) { bad_rows <- bad_rows | bad; bad_vars <- c(bad_vars, v) }
  }
  if (any(bad_rows)) {
    stop(structure(list(
      message = paste0("LI+Liang requires finite numeric model covariates on every subject row; ",
                       sum(bad_rows), " row(s) in ", length(unique(d[[id]][bad_rows])),
                       " subject(s) have unavailable covariates: ", paste(bad_vars, collapse = ", "),
                       ". Rows cannot be silently omitted from the all-visit outcome equation."),
      call = NULL, n_rows = sum(bad_rows),
      n_subjects_affected = length(unique(d[[id]][bad_rows])), covariates = bad_vars),
      class = c("li_liang_incomplete_design_error", "error", "condition")))
  }
  invisible(TRUE)
}

.li_liang_pinv <- function(A, tol = sqrt(.Machine$double.eps)) {
  A <- as.matrix(A); s <- svd(A)
  if (!length(s$d) || !is.finite(max(s$d)) || max(s$d) == 0)
    stop("Rank-zero estimating equation.", call. = FALSE)
  keep <- s$d > tol * max(s$d)
  if (sum(keep) == min(dim(A)))
    return(s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep]))
  fail <- function() stop("Estimating equation is rank deficient; named coefficients are not identified.", call. = FALSE)
  if (nrow(A) != ncol(A)) fail()
  column_scale <- apply(abs(A), 2L, max)
  if (any(!is.finite(column_scale) | column_scale <= 0)) fail()
  scaled <- sweep(A, 2L, column_scale, "/")
  row_scale <- apply(abs(scaled), 1L, max)
  if (any(!is.finite(row_scale) | row_scale <= 0)) fail()
  scaled <- scaled / row_scale
  balanced <- svd(scaled)
  keep <- balanced$d > tol * max(balanced$d)
  if (sum(keep) < ncol(A)) fail()
  inverse <- balanced$v[, keep, drop = FALSE] %*%
    (t(balanced$u[, keep, drop = FALSE]) / balanced$d[keep])
  sweep(sweep(inverse, 1L, column_scale, "/"), 2L, row_scale, "/")
}
.li_liang_const <- function(d, id, vars) {
  subject <- d[[id]]
  plain_id <- is.atomic(subject) && !is.object(subject) && is.null(dim(subject)) && !anyNA(subject)
  grouping <- if (!plain_id) NULL else if (is.integer(subject) || is.character(subject))
    match(subject, subject) else as.integer(factor(subject))
  for (v in vars) {
    x <- d[[v]]
    if (plain_id && is.numeric(x) && !is.object(x) && is.null(dim(x))) {
      keep <- !is.na(x); values <- x[keep]; group <- grouping[keep]
      different <- any(values != values[match(group, group)])
    } else {
      z <- tapply(x, subject, function(y) length(unique(y[!is.na(y)])))
      different <- any(z > 1L, na.rm = TRUE)
    }
    if (different) stop("Liang requires time-invariant covariate `", v, "`.", call. = FALSE)
  }
}

.li_liang_li <- function(d, id, time, outcome, obs_indicator, li_covs, covariance) {
  d$.li_liang_row <- seq_len(nrow(d)); o <- order(d[[id]], d[[time]], na.last = TRUE)
  z <- d[o, , drop = FALSE]; rownames(z) <- NULL
  actual <- is.finite(z[[time]])
  observed <- actual & !is.na(z[[obs_indicator]]) & z[[obs_indicator]] == 1 &
    is.finite(z[[outcome]])
  z[[outcome]][actual & !observed] <- NA_real_
  ids <- unique(z[[id]]); si <- match(z[[id]], ids); p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  actual_rows <- split(which(actual), factor(si[actual], levels = seq_along(ids)))
  cache_covariates <- !outcome %in% li_covs &&
    all(vapply(z[, li_covs, drop = FALSE], function(x)
      is.numeric(x) && !is.object(x) && is.null(dim(x)), logical(1)))
  if (cache_covariates) {
    li_values <- as.matrix(z[, li_covs, drop = FALSE])
    li_row <- function(r) as.numeric(li_values[r, , drop = TRUE])
  } else {
    li_row <- function(r) as.numeric(z[r, li_covs, drop = FALSE])
  }
  WL <- list(); rate <- gapv <- numeric(); pid <- integer(); k <- 0L
  for (ii in seq_along(ids)) {
    ix <- actual_rows[[ii]]; if (length(ix) < 2L) next
    for (j in 2:length(ix)) {
      a <- ix[j - 1L]; b <- ix[j]; gap <- z[[time]][b] - z[[time]][a]
      if (!observed[a] || !observed[b] || !is.finite(gap) || gap <= 0 ||
          !all(is.finite(li_row(a)))) next
      k <- k + 1L; WL[[k]] <- c(1, li_row(a),
                                 z[[outcome]][a])
      rate[k] <- (z[[outcome]][b] - z[[outcome]][a]) / gap
      gapv[k] <- gap; pid[k] <- ii
    }
  }
  if (k < max(10L, p + 2L)) stop("Too few observed adjacent increments for LI.", call. = FALSE)
  W <- do.call(rbind, WL); colnames(W) <- nm
  if (qr(W)$rank < ncol(W)) stop("Linear-increment design is rank deficient.", call. = FALSE)
  ww <- if (identical(covariance, "randomwalk")) gapv else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_liang_pinv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * rate)); names(eta) <- nm
  er <- rate - as.vector(W %*% eta); us <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * er), pid, reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Ve) <- list(nm, nm)
  grad <- matrix(NA_real_, nrow(z), p, dimnames = list(NULL, nm))
  rec <- unr <- rep(FALSE, nrow(z)); ey <- eta[p]
  for (ii in seq_along(ids)) {
    ix <- actual_rows[[ii]]; anchored <- FALSE; prev <- NA_integer_
    yp <- NA_real_; gp <- rep(NA_real_, p)
    for (r in ix) {
      if (observed[r]) { yp <- z[[outcome]][r]; gp <- rep(0, p); grad[r, ] <- gp
        anchored <- TRUE; prev <- r; next }
      if (!anchored || is.na(prev)) { unr[r] <- TRUE; prev <- r; next }
      gap <- z[[time]][r] - z[[time]][prev]
      xv <- li_row(prev)
      if (!is.finite(gap) || gap <= 0 || !all(is.finite(xv))) {
        unr[r] <- TRUE; anchored <- FALSE; prev <- r; next }
      wv <- c(1, xv, yp); yn <- yp + gap * sum(wv * eta)
      gn <- (1 + gap * ey) * gp + gap * wv
      z[[outcome]][r] <- yn; grad[r, ] <- gn; rec[r] <- TRUE
      yp <- yn; gp <- gn; prev <- r
    }
  }
  back <- order(z$.li_liang_row); out <- z[back, , drop = FALSE]; out$.li_liang_row <- NULL
  rownames(out) <- NULL
  list(data = out, n_imputed = sum(rec), n_unreconstructed = sum(unr),
       reconstructed = rec[back], gradient = grad[back, , drop = FALSE],
       eta = eta, vcov_eta = Ve, eta_scores = us, eta_bread = A,
       n_observed_increments = k, increment_design_rank = qr(W)$rank,
       increment_design_columns = ncol(W), increment_condition = kappa(A),
       formula = paste0("d", outcome, "/dt ~ ",
                        paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
       weighting = if (identical(covariance, "randomwalk"))
         "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates")
}

.li_liang_li_loop <- .li_liang_li
.li_liang_li <- function(d, id, time, outcome, obs_indicator, li_covs, covariance) {
  d$.li_liang_row <- seq_len(nrow(d)); o <- order(d[[id]], d[[time]], na.last = TRUE)
  z <- d[o, , drop = FALSE]; rownames(z) <- NULL
  actual <- is.finite(z[[time]])
  observed <- actual & !is.na(z[[obs_indicator]]) & z[[obs_indicator]] == 1 &
    is.finite(z[[outcome]])
  z[[outcome]][actual & !observed] <- NA_real_
  ids <- unique(z[[id]]); si <- match(z[[id]], ids); p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  cache_covariates <- !outcome %in% li_covs &&
    all(vapply(z[, li_covs, drop = FALSE], function(x)
      is.numeric(x) && !is.object(x) && is.null(dim(x)), logical(1)))
  if (!cache_covariates)
    return(.li_liang_li_loop(d, id, time, outcome, obs_indicator, li_covs, covariance))
  li_values <- as.matrix(z[, li_covs, drop = FALSE])
  finite_x <- rowSums(!is.finite(li_values)) == 0L
  tt <- z[[time]]; y <- z[[outcome]]
  Ar <- which(actual); nA <- length(Ar); sA <- si[Ar]; oA <- observed[Ar]
  a <- Ar[-nA]; b <- Ar[-1L]; gap <- tt[b] - tt[a]
  keep <- sA[-nA] == sA[-1L] & observed[a] & observed[b] & is.finite(gap) & gap > 0 &
    finite_x[a]
  a <- a[keep]; b <- b[keep]; gapv <- gap[keep]; k <- length(a)
  if (k < max(10L, p + 2L)) stop("Too few observed adjacent increments for LI.", call. = FALSE)
  W <- cbind(1, li_values[a, , drop = FALSE], y[a]); dimnames(W) <- list(NULL, nm)
  rate <- (y[b] - y[a]) / gapv; pid <- si[a]
  if (qr(W)$rank < ncol(W)) stop("Linear-increment design is rank deficient.", call. = FALSE)
  ww <- if (identical(covariance, "randomwalk")) gapv else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_liang_pinv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * rate)); names(eta) <- nm
  er <- rate - as.vector(W %*% eta); us <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * er), pid, reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Ve) <- list(nm, nm)
  grad <- matrix(NA_real_, nrow(z), p, dimnames = list(NULL, nm))
  rec <- unr <- rep(FALSE, nrow(z)); ey <- eta[p]
  grad[Ar[oA], ] <- 0
  first <- c(TRUE, sA[-1L] != sA[-nA])
  lead <- !oA & (first | c(TRUE, oA[-nA]))
  run <- cumsum(lead); st <- which(lead); pos <- seq_len(nA) - st[pmax(run, 1L)] + 1L
  alive <- !first[st]; prev <- rep(NA_integer_, length(st))
  prev[alive] <- Ar[st[alive] - 1L]
  yp <- rep(NA_real_, length(st)); yp[alive] <- y[prev[alive]]
  gp <- matrix(NA_real_, length(st), p); gp[alive, ] <- 0
  for (at in split(which(!oA), pos[!oA])) {
    rr <- run[at]; r <- Ar[at]
    unr[r[!alive[rr]]] <- TRUE; r <- r[alive[rr]]; rr <- rr[alive[rr]]
    if (!length(r)) next
    pv <- prev[rr]; g <- tt[r] - tt[pv]
    ok <- is.finite(g) & g > 0 & finite_x[pv]
    unr[r[!ok]] <- TRUE; alive[rr[!ok]] <- FALSE
    r <- r[ok]; rr <- rr[ok]; pv <- pv[ok]; g <- g[ok]
    if (!length(r)) next
    wv <- cbind(1, li_values[pv, , drop = FALSE], yp[rr])
    yn <- yp[rr] + g * rowSums(wv * rep(eta, each = length(r)))
    gn <- (1 + g * ey) * gp[rr, , drop = FALSE] + g * wv
    y[r] <- yn; grad[r, ] <- gn; rec[r] <- TRUE
    prev[rr] <- r; yp[rr] <- yn; gp[rr, ] <- gn
  }
  z[[outcome]] <- y
  back <- order(z$.li_liang_row); out <- z[back, , drop = FALSE]; out$.li_liang_row <- NULL
  rownames(out) <- NULL
  list(data = out, n_imputed = sum(rec), n_unreconstructed = sum(unr),
       reconstructed = rec[back], gradient = grad[back, , drop = FALSE],
       eta = eta, vcov_eta = Ve, eta_scores = us, eta_bread = A,
       n_observed_increments = k, increment_design_rank = qr(W)$rank,
       increment_design_columns = ncol(W), increment_condition = kappa(A),
       formula = paste0("d", outcome, "/dt ~ ",
                        paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
       weighting = if (identical(covariance, "randomwalk"))
         "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates")
}

.li_liang_li_vector <- .li_liang_li
.li_liang_li <- function(d, id, time, outcome, obs_indicator, li_covs, covariance,
                         full = TRUE) {
  plain <- !outcome %in% li_covs && is.double(d[[time]]) && .li_plain_numeric(d[[time]]) &&
    .li_plain_numeric(d[[outcome]]) &&
    all(vapply(d[, li_covs, drop = FALSE], .li_plain_numeric, logical(1)))
  if (!plain)
    return(.li_liang_li_vector(d, id, time, outcome, obs_indicator, li_covs, covariance))
  s <- .li_sorted_increments(d, id, time, outcome, obs_indicator, li_covs, ".li_liang_row")
  p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  a <- s$a; b <- s$b; gapv <- s$gap; k <- length(a)
  if (k < max(10L, p + 2L)) stop("Too few observed adjacent increments for LI.", call. = FALSE)
  W <- cbind(1, s$x[a, , drop = FALSE], s$y[a]); dimnames(W) <- list(NULL, nm)
  rate <- (s$y[b] - s$y[a]) / gapv; pid <- s$si[a]
  if (qr(W)$rank < ncol(W)) stop("Linear-increment design is rank deficient.", call. = FALSE)
  ww <- if (identical(covariance, "randomwalk")) gapv else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_liang_pinv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * rate)); names(eta) <- nm
  fw <- .li_forward_recursion(s, d, time, eta)
  out <- s$data; out[[outcome]] <- fw$y
  li <- list(data = out, n_imputed = sum(fw$rec), n_unreconstructed = sum(s$unobserved & !fw$rec),
             reconstructed = fw$rec, eta = eta)
  if (!full) return(li)
  er <- rate - as.vector(W %*% eta); us <- matrix(0, length(s$ids), p)
  tmp <- rowsum(W * (ww * er), pid, reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Ve) <- list(nm, nm)
  c(li, list(vcov_eta = Ve, eta_scores = us, eta_bread = A,
       n_observed_increments = k, increment_design_rank = qr(W)$rank,
       increment_design_columns = ncol(W), increment_condition = kappa(A),
       formula = paste0("d", outcome, "/dt ~ ",
                        paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
       weighting = if (identical(covariance, "randomwalk"))
         "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates"))
}

.li_liang_gamma <- function(d, id, time, visit_covs, check = TRUE) {
  if (check) .li_liang_const(d, id, visit_covs)
  ids <- unique(d[[id]]); si <- match(d[[id]], ids); first <- match(ids, d[[id]])
  V <- as.matrix(d[first, visit_covs, drop = FALSE]); storage.mode(V) <- "double"
  if (any(!is.finite(V))) stop("Baseline visit covariates must be finite.")
  m <- tabulate(si[is.finite(d[[time]])], nbins = length(ids)); M <- sum(m)
  if (!M) stop("No visit events.")
  es <- colSums(V * m)
  cached_g <- cached_fg <- NULL
  fg <- function(g) {
    if (!is.null(cached_g) && identical(g, cached_g)) return(cached_fg)
    lp <- as.vector(V %*% g); a <- max(lp); w <- exp(lp - a); sw <- sum(w)
    value <- list(v = M * (a + log(sw)) - sum(es * g),
         gr = M * colSums(V * w) / sw - es)
    cached_g <<- g; cached_fg <<- value
    value
  }
  o <- stats::optim(rep(0, ncol(V)), function(g) fg(g)$v, function(g) fg(g)$gr,
                    method = "BFGS", control = list(reltol = 1e-10, maxit = 1000))
  g <- o$par; names(g) <- visit_covs
  list(gamma = g, ids = ids, si = si, first = first, V = V, m = m, M = M,
       exp_gamma = exp(as.vector(V %*% g)), converged = o$convergence == 0L,
       code = o$convergence, score_max = max(abs(fg(g)$gr)))
}

.li_liang_visit_stage <- function(d, id, time, visit_covs, sigma_floor, censor_col=NULL,
                                  check = TRUE) {
  if (.liang_completed_varying_followup(d, id, censor_col))
    return(.liang_completed_visit_stage(d, id, time, visit_covs, censor_col, sigma_floor))
  ga <- .li_liang_gamma(d, id, time, visit_covs, check)
  Lam <- ga$M / sum(ga$exp_gamma); Li <- ga$exp_gamma * Lam
  raw <- sum(ga$m^2 - ga$m - Li^2) / sum(Li^2); sig <- max(raw, 0)
  use <- sig > sigma_floor
  Bi <- if (use) (1 + ga$m * sig) / (1 + Li * sig) - 1 else rep(0, length(ga$ids))
  list(ga = ga, Lambda = Lam, sigma2_raw = raw, sigma2 = sig, use = use, B = Bi)
}

# Reverse-increment assumptions: ?Linear_increment_IP.
# full = FALSE omits increment covariances; check = FALSE requires subject-constant visit covariates.
.li_liang_reconstruct <- function(d, id, time, outcome, obs_indicator,
    visit_covs, long_covs, random_covs, covariance, sigma_floor,
    leading_imputation = "reverse_li", censor_col=NULL, full = TRUE, check = TRUE) {
  leading_imputation <- match.arg(leading_imputation, c("reverse_li", "error"))
  li <- .li_liang_li(d, id, time, outcome, obs_indicator, visit_covs, covariance, full)
  li$n_imputed_forward <- li$n_imputed; li$n_imputed_backward <- 0L
  li$backward <- NULL; li$visit_stage <- NULL
  li$reconstructed_backward <- rep(FALSE, nrow(d))
  if (leading_imputation == "error" || li$n_unreconstructed == 0L) return(li)
  actual <- is.finite(d[[time]])
  observed <- actual & !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
    is.finite(d[[outcome]])
  # Returned rows retain input order.
  ord <- order(d[[id]], d[[time]], na.last = TRUE)
  rows <- ord[actual[ord]]
  ids <- unique(d[[id]]); si <- match(d[[id]], ids)
  if (!.li_has_leading_run(rows, si, length(ids), observed)) return(li)
  covs <- unique(c(visit_covs, long_covs, random_covs))
  .li_liang_require_design(d, id, covs)
  if (check) .li_liang_const(d, id, covs)
  stage <- .li_liang_visit_stage(d, id, time, visit_covs, sigma_floor, censor_col, check)
  ga <- stage$ga
  C <- as.matrix(d[, covs, drop = FALSE]); storage.mode(C) <- "double"
  if (stage$use) {
    Q <- cbind(`(Intercept)` = 1, as.matrix(d[, random_covs, drop = FALSE]))
    colnames(Q) <- paste0("visit_frailty:", colnames(Q))
    C <- cbind(C, Q * stage$B[ga$si])
  }
  if (any(!is.finite(C))) stop("Non-finite reverse LI covariate design.", call. = FALSE)
  a <- rows[-length(rows)]; b <- rows[-1L]
  gap <- d[[time]][b] - d[[time]][a]
  use <- si[a] == si[b] & observed[a] & observed[b] & is.finite(gap) & gap > 0
  a <- a[use]; b <- b[use]; gap <- gap[use]
  W <- cbind(`(Intercept)` = 1, C[a, , drop = FALSE], d[[outcome]][b])
  colnames(W)[ncol(W)] <- paste0("next_", outcome)
  k <- length(a); p <- ncol(W)
  if (k < max(10L, p + 2L))
    stop("Too few observed adjacent increments for reverse LI.", call. = FALSE)
  if (qr(W)$rank < p)
    stop("Reverse linear-increment design is rank deficient; leading outcomes are not identified.",
         call. = FALSE)
  rate <- (d[[outcome]][a] - d[[outcome]][b]) / gap
  ww <- if (identical(covariance, "randomwalk")) gap else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_liang_pinv(A)
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
  li$visit_stage <- stage
  if (!full) return(li)
  scores <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * (rate - drop(W %*% eta))), si[a], reorder = FALSE)
  scores[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(scores) %*% t(Ai)
  dimnames(Ve) <- list(names(eta), names(eta))
  li$backward <- list(coefficients = eta, vcov = Ve, n_observed_increments = k,
    rank = qr(W)$rank, columns = p, condition = kappa(A),
    predictors = colnames(C),
    formula = paste0("(previous_", outcome, " - next_", outcome, ")/dt ~ ",
      paste(c(colnames(C), paste0("next_", outcome)), collapse = " + ")),
    weighting = li$weighting)
  li
}

.li_liang_profile_scores <- function(Dsub,center,D,H,coef,y,subject,counts) {

  residual<-y-drop(D%*%coef)
  score<-matrix(0,nrow(Dsub),ncol(Dsub))
  direct<-rowsum(H*residual,subject,reorder=FALSE)
  score[as.integer(rownames(direct)),]<-direct
  center_derivative<-sweep(Dsub,2,center,"-")*(counts/sum(counts))
  score-center_derivative*sum(residual)
}

.li_liang_fit <- function(d, id, time, outcome, visit_covs, long_covs, random_covs,
                     sigma_floor, estimate_intercept=FALSE, baseline_formula=~1,
                       baseline_terms=NULL, visit_stage=NULL, censor_col=NULL,
                       full=TRUE, check=TRUE) {
  if (.liang_completed_varying_followup(d, id, censor_col))
    return(.liang_completed_followup_fit(d, id, time, outcome, visit_covs,
      long_covs, random_covs, sigma_floor, censor_col, estimate_intercept,
      baseline_formula, baseline_terms))
  .li_liang_require_completed(d, id, time, outcome)
  .li_liang_require_design(d, id, unique(c(visit_covs, long_covs, random_covs)))
  if (check) .li_liang_const(d, id, unique(c(long_covs, random_covs)))
  stage <- if (is.null(visit_stage))
    .li_liang_visit_stage(d, id, time, visit_covs, sigma_floor, check = check) else visit_stage
  ga <- stage$ga; Lam <- stage$Lambda; raw <- stage$sigma2_raw; sig <- stage$sigma2
  use <- stage$use; Bi <- stage$B
  X <- as.matrix(d[, long_covs, drop = FALSE]); storage.mode(X) <- "double"
  Xb <- X[ga$first, , drop = FALSE]
  Q <- as.matrix(d[, random_covs, drop = FALSE]); storage.mode(Q) <- "double"
  Q0 <- cbind(`(Intercept)` = 1, Q); Qb <- Q0[ga$first, , drop = FALSE]
  if (use) {
    Bb <- Qb * Bi; D <- cbind(X, Q0 * Bi[ga$si])
    nm <- c(long_covs, paste0("theta:", colnames(Q0)))
  } else { D <- X; Bb <- NULL; nm <- long_covs }
  colnames(D) <- nm
  if (any(!is.finite(D)))
    stop("LI+Liang produced a non-finite augmented outcome design; the all-visit fit cannot proceed.",
         call. = FALSE)
  ctr <- colSums(Xb * ga$m) / sum(ga$m)
  if (use) ctr <- c(ctr, colSums(Bb * ga$m) / sum(ga$m))
  H <- sweep(D, 2, ctr, "-")
  ok <- is.finite(d[[time]])
  baseline<-.li_liang_baseline_design(d[[time]][ok],time,estimate_intercept,
                                   baseline_formula,prediction_terms=baseline_terms)
  F<-baseline$design;nb<-ncol(F);Dobs<-D[ok,,drop=FALSE]
  if(estimate_intercept) {

    W<-cbind(F,Dobs);an<-colnames(W)
    if(anyDuplicated(an))stop("Baseline and outcome coefficient names must be distinct.",call.=FALSE)
    A<-crossprod(W);Ai<-.li_liang_pinv(A)
    ca<-drop(Ai%*%crossprod(W,d[[outcome]][ok]));names(ca)<-an
  } else {
    Hobs<-H[ok,,drop=FALSE];an<-colnames(D)
    A<-crossprod(Hobs,Dobs);Ai<-.li_liang_pinv(A)
    ca<-drop(Ai%*%crossprod(Hobs,d[[outcome]][ok]));names(ca)<-an
  }
  primary<-seq_len(nb+length(long_covs))
  if(!full)return(list(coef=ca[primary],converged=ga$converged))
  if(estimate_intercept) {
    rr<-d[[outcome]][ok]-drop(W%*%ca)
    sc<-matrix(0,length(ga$ids),ncol(W))
    tmp<-rowsum(W*rr,ga$si[ok],reorder=FALSE)
    sc[as.integer(rownames(tmp)),]<-tmp
  } else {
    sc<-.li_liang_profile_scores(D[ga$first,,drop=FALSE],ctr,Dobs,Hobs,
                             ca,d[[outcome]][ok],ga$si[ok],ga$m)
  }
  VA<-Ai%*%crossprod(sc)%*%t(Ai);dimnames(VA)<-list(an,an)
  theta_index<-setdiff(seq_along(ca),primary)
  model_scope<-if(estimate_intercept)"Liang-time with an explicit deterministic outcome baseline" else
    "Liang profile estimating equation with unspecified outcome baseline"
  equation<-if(estimate_intercept)"W'W c=W'Y, W=[F(time),D]; 2023 Section 2.5.2 Eq. (5)" else
    "H'D b=H'Y, H=D-visit-count center; 2023 Section 2.5.1 Eq. (4)"
  list(coef=ca[primary],vcov=VA[primary,primary,drop=FALSE],
       coef_all=ca,vcov_all=VA,vcov_full=VA,
       theta=if(use)ca[theta_index] else
         setNames(rep(NA_real_,ncol(Q0)),colnames(Q0)),
       estimate_intercept=estimate_intercept,baseline_formula=baseline$formula,
       baseline_terms=baseline$terms,baseline_assumption=baseline$assumption,
       model_scope=model_scope,outcome_equation=equation,
       gamma=ga$gamma,Lambda=Lam,sigma2=sig,sigma2_raw=raw,use_B=use,
       converged=ga$converged,code=ga$code,score_max=ga$score_max,
       condition=kappa(A),n_outcome=sum(ok),n_visit=ga$M,zero_visit=sum(ga$m==0L),
       baseline_prediction=if(estimate_intercept)
         .liang_trajectory_parametric_summary(baseline$terms,ca[seq_len(nb)]) else
         .liang_trajectory_increment_summary(d[[time]][ok],
           d[[outcome]][ok]-drop(Dobs%*%ca),log(sum(ga$m)/Lam)))
}

.li_liang_seed <- function(seed, expr) {
  if (length(seed) != 1L || !is.finite(seed)) stop("Invalid bootstrap seed.", call. = FALSE)
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, .GlobalEnv) else if
    (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(as.integer(seed)); force(expr)
}

.fit_li_liang <- function(
    data, id = "id", time = "time", outcome = "Y", obs_indicator = "R",
    visit_covs = c("Z", "X"), long_covs = c("Z", "X"), random_covs = "Z",
    li_covariance = c("randomwalk", "rate_independence"),
    sigma_floor = 1e-8, censor_col = if ("C" %in% names(data)) "C" else NULL,
    inference = c("conditional", "patient_bootstrap"), bootstrap_reps = 200L,
    bootstrap_seed = 1801L, keep_completed = FALSE, estimate_intercept = FALSE, baseline_formula = ~1,
    leading_imputation = c("reverse_li", "error"),
    unobserved_subjects = c("exclude", "error")) {
  li_covariance <- match.arg(li_covariance)
  inference <- match.arg(inference)
  leading_imputation <- match.arg(leading_imputation)
  unobserved_subjects <- match.arg(unobserved_subjects)
  d <- .li_liang_check(data, c(id, time, outcome, obs_indicator, visit_covs, long_covs,
                          random_covs, if (!is.null(censor_col)) censor_col))
  selection <- .li_liang_select_subjects(d, id, time, outcome, obs_indicator, unobserved_subjects)
  d <- selection$data
  .li_liang_require_design(d, id, unique(c(visit_covs, long_covs, random_covs)))
  baseline_request<-.li_liang_baseline_design(d[[time]][is.finite(d[[time]])],
                                           time,estimate_intercept,baseline_formula)
  if (!is.null(censor_col)) {
    cc <- as.numeric(d[match(unique(d[[id]]), d[[id]]), censor_col])
    if (any(!is.finite(cc)))
      stop("Every subject needs a finite censoring time.", call. = FALSE)
  }
  varying_followup <- .liang_completed_varying_followup(d, id, censor_col)
  li <- .li_liang_reconstruct(d, id, time, outcome, obs_indicator,
    visit_covs, long_covs, random_covs, li_covariance, sigma_floor, leading_imputation, censor_col)
  f <- .li_liang_fit(li$data, id, time, outcome, visit_covs, long_covs, random_covs, sigma_floor,
                 estimate_intercept,baseline_formula, visit_stage = li$visit_stage, censor_col=censor_col)
  Vout <- f$vcov; boot_success <- 0L; boot_failure <- character()
  if (inference == "patient_bootstrap") {
    if (length(bootstrap_reps) != 1L || !is.finite(bootstrap_reps) ||
        bootstrap_reps < 2L || bootstrap_reps != as.integer(bootstrap_reps))
      stop("`bootstrap_reps` must be an integer >= 2.", call. = FALSE)
    check <- varying_followup || outcome %in% c(visit_covs, long_covs, random_covs)
    resample <- .li_resampler(d, id)
    bb <- .li_liang_seed(bootstrap_seed, lapply(seq_len(bootstrap_reps), function(b) {
      tryCatch({
        db <- resample()
        lb <- .li_liang_reconstruct(db, id, time, outcome, obs_indicator,
          visit_covs, long_covs, random_covs, li_covariance, sigma_floor, leading_imputation, censor_col,
          full = FALSE, check = check)
        zb <- .li_liang_fit(lb$data, id, time, outcome, visit_covs, long_covs,
                       random_covs, sigma_floor,estimate_intercept,baseline_formula,
                       baseline_terms=f$baseline_terms, visit_stage = lb$visit_stage, censor_col=censor_col,
                       full = FALSE, check = check)
        if (!isTRUE(zb$converged) || !all(is.finite(zb$coef)))
          stop("Nonconverged LI+Liang bootstrap fit.", call. = FALSE)
        zb$coef
      }, error = function(e) structure(list(message = conditionMessage(e)), class = "li_liang_failure"))
    }))
    nm <- names(f$coef)
    good <- vapply(bb, function(x) is.numeric(x) && length(x) == length(nm) &&
                     !anyDuplicated(names(x)) && setequal(names(x), nm) &&
                     all(is.finite(x[nm])), logical(1)); boot_success <- sum(good)
    boot_failure <- vapply(bb[!good], function(x) if (inherits(x, "li_liang_failure"))
      x$message else "coefficient schema mismatch or non-finite coefficient", "")
    if (boot_success < ceiling(0.95 * bootstrap_reps))
      stop("Fewer than 95% successful patient bootstrap fits: ", boot_success, "/", bootstrap_reps,
           call. = FALSE)
    Q <- do.call(rbind, lapply(bb[good], `[`, nm))
    Vout <- stats::cov(Q); dimnames(Vout) <- list(nm, nm)
  }
  ans <- list(method_key = "li_liang", method = if(estimate_intercept)"LI+Liang-time" else "LI+Liang",
              estimand = paste0("Liang marginal longitudinal coefficient after recursive LI reconstruction",
                if (selection$n_subjects_excluded > 0L)
                  " in the retained cohort after excluding subjects without any observed outcome" else ""),
              coefficients = f$coef, vcov = Vout,
              se = sqrt(pmax(diag(Vout), 0)),
              vcov_type = if (inference == "patient_bootstrap")
                "patient-cluster bootstrap of the full LI+Liang procedure" else
                if(estimate_intercept)
                  "variance (plug-in): Liang-time subject sandwich for baseline/outcome effects conditional on LI reconstruction and Stage 1" else
                  "variance (plug-in): Liang profile subject sandwich with empirical-centering influence, conditional on LI reconstruction and Stage 1",
              nuisance = list(gamma = f$gamma, Lambda = f$Lambda, sigma2 = f$sigma2,
                              sigma2_raw = f$sigma2_raw, theta = f$theta,
                              baseline_terms=f$baseline_terms,
                              increment_coefficients = li$eta,
                              increment_vcov = li$vcov_eta,
                              backward_increment_coefficients = li$backward$coefficients,
                              backward_increment_vcov = li$backward$vcov,
                              conditional_vcov_type = "variance (plug-in): conditional on LI reconstruction and Liang Stage 1",
                              conditional_vcov = f$vcov,
                              coefficients_all=f$coef_all,conditional_vcov_all=f$vcov_all),
              converged = f$converged,
              diagnostics = list(ok = isTRUE(f$converged && all(is.finite(f$coef)) &&
                                               all(is.finite(f$vcov))),
                                 estimate_intercept=estimate_intercept,
                                 baseline_formula=f$baseline_formula,model_scope=f$model_scope,
                                 baseline_assumption=f$baseline_assumption,
                                 unobserved_subjects = unobserved_subjects,
                                 n_subjects_input = selection$n_subjects_input,
                                 n_visits_input = selection$n_visits_input,
                                 n_subjects_excluded_unobserved = selection$n_subjects_excluded,
                                 n_visits_excluded_unobserved = selection$n_visits_excluded,
                                 analysis_population = if (selection$n_subjects_excluded > 0L)
                                   "retained cohort after excluding subjects without any observed outcome" else
                                   "input cohort; no subjects excluded",
                                 bootstrap_population = "retained eligible subjects; conditional on cohort eligibility",
                                 n_subjects = length(unique(d[[id]])), n_visits = f$n_visit,
                                 zero_visit_subjects = f$zero_visit,
                                 n_observed_before_imputation = sum(is.finite(d[[time]]) &
                                   !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
                                   is.finite(d[[outcome]])),
                                 n_imputed = li$n_imputed, n_analysis = f$n_outcome,
                                 n_imputed_forward = li$n_imputed_forward,
                                 n_imputed_backward = li$n_imputed_backward,
                                 backward_model_used = !is.null(li$backward),
                                 leading_imputation = leading_imputation,
                                 backward_model = li$backward[c("formula", "predictors", "weighting",
                                   "n_observed_increments", "rank", "columns", "condition")],
                                 n_unreconstructed = li$n_unreconstructed,
                                 all_visit_outcome_coverage = f$n_outcome == f$n_visit,
                                 unreconstructed_policy = "error; preserve all actual visits of retained subjects",
                                 n_observed_increments = li$n_observed_increments,
                                 increment_design_rank = li$increment_design_rank,
                                 increment_design_columns = li$increment_design_columns,
                                 increment_condition_number = li$increment_condition,
                                 increment_weighting = li$weighting,
                                 original_R_preserved = identical(li$data[[obs_indicator]],
                                                                  d[[obs_indicator]]),
                                 downstream_process = "all actual visits N(t) of retained subjects; reconstructed outcomes are not new process events",
                                 outcome_equation = f$outcome_equation,
                                 li_uncertainty_included = inference == "patient_bootstrap",
                                 inference = inference,
                                 bootstrap_requested = if (inference == "patient_bootstrap")
                                   as.integer(bootstrap_reps) else 0L,
                                 bootstrap_success = boot_success,
                                 bootstrap_failures = boot_failure,
                                 gamma_score_max = f$score_max,
                                 condition_number = f$condition, frailty_boundary = !f$use_B),
              assumptions = c("Pooled linear increment-rate model and discrete-time independent censoring (DTIC)",
                              "Observed adjacent R=1 pairs train increments; forward recursion re-anchors at returned observations",
                              "Re-anchoring after nonmonotone missingness requires a compatible return-to-observation mechanism",
                              "Every actual visit has a finite observed or LI-reconstructed outcome; at least one observed anchor is needed per retained subject with visits",
                              if (selection$n_subjects_excluded > 0L)
                                "Inference concerns the retained cohort; visit/recording-based exclusion can change the visit and outcome models and does not establish unbiasedness for the original cohort",
                              if (!is.null(li$backward)) c(
                                "Leading outcomes use a separately fitted reverse-time increment-rate model, not inversion of forward coefficients",
                                "Reverse conditional means given later outcomes, covariates and the visit process follow the pooled linear model; observed adjacent pairs represent leading missing runs",
                                "Reverse reconstruction errors have zero conditional mean given the visit process, preserving Liang's visit-weighted score; forward DTIC alone does not imply this assumption"),
                              "Common follow-up and time-invariant Liang covariates",
                              f$baseline_assumption,
                              if (inference == "conditional")
                                "Covariance conditions on LI and frailty nuisance estimates" else
                                "Subjects are the independent resampling units"),
              call = match.call())
  if (varying_followup) {
    conditional_scope <- "Ordinary Liang-V all-stage patient stacked covariance conditional on LI reconstruction"
    if (inference == "conditional") ans$vcov_type <- conditional_scope
    ans$nuisance$conditional_vcov_type <- conditional_scope
    ans$nuisance$followup <- f$followup_nuisance
    ans$diagnostics$subject_specific_followup <- TRUE
    ans$diagnostics$nuisance_estimation_propagated <- TRUE
    ans$assumptions[ans$assumptions == "Common follow-up and time-invariant Liang covariates"] <-
      "Subject-specific noninformative follow-up and time-invariant Liang covariates"
    ans$assumptions[ans$assumptions == "Covariance conditions on LI and frailty nuisance estimates"] <-
      "Covariance conditions on LI reconstruction and propagates visiting rate, profiled baseline, frailty moment and outcome-centering uncertainty"
  }
  ans$prediction <- .liang_trajectory_spec(f$baseline_prediction,d,id,time,long_covs,f$coef,
    time_range=.liang_trajectory_window(d,time,censor_col),
    population=ans$diagnostics$analysis_population)
  if (keep_completed) ans$completed_data <- li$data
  class(ans) <- c("li_liang"); ans
}
