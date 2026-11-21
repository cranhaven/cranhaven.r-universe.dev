library(CIMEHR)

ns <- asNamespace("CIMEHR")
expect_identification_error <- function(expr, reason = NULL, stage = "outcome") {
  err <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(err, "cimehr_zero_frailty_identification_error"),
    identical(err$status, "not_identified"), identical(err$stage, stage),
    identical(err$sigma2_zeta, 0), startsWith(conditionMessage(err),
      "This zero-frailty design cannot be uniquely estimated by the current three-stage procedure"))
  if (!is.null(reason)) stopifnot(identical(err$reason, reason))
  invisible(err)
}

for (variant in c("current", "higher_order_gaussian_moments")) {
  engine <- get(".cimehr_build_engine", ns)(variant)
  up <- engine$.cimehr_updated

  # Zero-information posterior variance.
  post <- up$.lu_laplace(c(0, 2, 5), c(0, 1.3, 4), 0)
  stopifnot(all(post$mu == 0), all(post$s2 == 1), all(post$mode == 0),
    all(post$curvature == 1), identical(post$sigma2_zeta, 0))
  if (is.function(engine$.cimehr_spline$.lu_laplace)) {
    spline_post <- engine$.cimehr_spline$.lu_laplace(c(0, 2, 5), c(0, 1.3, 4), 0)
    stopifnot(identical(post$mu, spline_post$mu), identical(post$s2, spline_post$s2))
  }

  # Scale-free compensation multiplier.
  Z <- c(-1, 0, 1)
  X <- c(-0.5, 0, 0.5)
  XO <- cbind("(Intercept)" = 1, X = X, Z = Z)
  ZO <- cbind("(Intercept)" = 1, Z = Z)
  par <- c("alpha.(Intercept)" = 0.2, "alpha.X" = 0.5, "alpha.Z" = 0.1,
    "delta.(Intercept)" = 0.7, "delta.Z" = -0.3,
    "sigma2_q.(Intercept)" = 0.4, "sigma2_q.Z" = 0.2)
  f <- up$.lu_functionals(par, XO, ZO, post, include_delta = TRUE)
  d <- 0.7 - 0.3 * Z
  total <- 1 + 0.4 + 0.2 * Z^2 + d^2
  k <- (0.2 + 0.5 * X + 0.1 * Z)/sqrt(total)
  h <- stats::dnorm(k)/stats::pnorm(k)/sqrt(total)
  stopifnot(max(abs(f$omega - stats::pnorm(k))) < 1e-14,
    max(abs(f$kappa - d*h)) < 1e-14, all(abs(f$kappa) > 0))
  attr(par, "zero_compensation") <- TRUE
  quotient <- up$.lu_functionals(par, XO, ZO, post, include_delta = TRUE)
  stopifnot(max(abs(quotient$kappa - h)) < 1e-14)

  n <- 48L
  z <- seq(-1.5, 1.5, length.out = n)
  x <- sin(seq_len(n))
  post_n <- up$.lu_laplace(rep(4, n), rep(4, n), 0)
  make_spec <- function(ZO, family, ...) {
    c(list(kind = "quotient", posterior = post_n, original_ZO = ZO,
      tolerance = 1e-7, family = family), list(...))
  }
  same_space <- function(A, B) {
    stopifnot(up$.lu_zero_rank(A) == up$.lu_zero_rank(B),
      up$.lu_zero_rank(cbind(A, B)) == up$.lu_zero_rank(A))
  }
  check_reuse <- function(result, W0, W1, ZO) {
    again <- up$.lu_zero_compensation(result$spec, W0, W1, ZO)
    stopifnot(identical(result$W0, again$W0), identical(result$W1, again$W1))
  }

  # One loading with an affine outcome random design.
  zi <- matrix(1, n, 1L)
  W0 <- cbind(1, x, 0)
  W1 <- cbind(rep(0,n), rep(0,n), rep(1,n))
  scalar <- make_spec(zi, "groups", profiles = matrix(1, 1L, 1L),
    group_variance = 0.8)
  ans <- up$.lu_zero_compensation(scalar, W0, W1)
  same_space(rbind(ans$W0, ans$W1), rbind(W0, W1))
  for (loading in c(-2, -0.3, 0.4, 3))
    same_space(rbind(ans$W0, ans$W1), loading*rbind(W0, W1))
  check_reuse(ans, W0, W1, zi)
  scalar$group_variance <- 0
  zero <- up$.lu_zero_compensation(scalar, W0, W1)
  stopifnot(ncol(zero$W0) == 0L, ncol(zero$W1) == 0L)

  # Saturated outcome group coefficients.
  g <- rep(0:1, length.out = n)
  zg <- cbind(1, g)
  Wg <- zg
  Wg1 <- matrix(0, n, ncol(Wg))
  groups <- make_spec(zg, "groups", profiles = rbind(c(1,0), c(1,1)),
    group_variance = c(0.6, 1.2))
  ans <- up$.lu_zero_compensation(groups, Wg, Wg1)
  same_space(ans$W0, zg)
  for (loading in list(c(-0.4,0.8), c(0.3,0.2), c(0.6,-1)))
    same_space(ans$W0, Wg*loading[g+1L])
  check_reuse(ans, Wg, Wg1, zg)

  # Common outcome random intercept with two recording loadings.
  expect_identification_error(up$.lu_zero_compensation(groups,
    matrix(1,n,1L), matrix(0,n,1L)),
    "compensation_direction_depends_on_unidentified_decomposition")

  # One zero-variance group and compensation c*(1-Z).
  one_group <- groups
  one_group$group_variance <- c(0.6, 0)
  a <- up$.lu_zero_compensation(one_group, matrix(1,n,1L), matrix(0,n,1L))
  np <- up$.lu_zero_compensation(a$spec, matrix(1,1L,1L), matrix(0,1L,1L),
    matrix(c(1,0.25),1L,2L))
  stopifnot(abs(np$W0[1L,1L]-0.75)<1e-12, np$W1[1L,1L]==0)

  # Continuous Z with a two-loading covariance ridge.
  zc <- cbind(1, z)
  edge <- make_spec(zc, "edge", axes = 1:2,
    reference_direction = c(0.6,-0.4))
  expect_identification_error(up$.lu_zero_compensation(edge, zc,
    matrix(0,n,2L)),
    "compensation_direction_depends_on_unidentified_decomposition")

  # Diagonal covariance with one or multiple positive axes.
  axes <- make_spec(zc, "axes", axes = 1L)
  ans <- up$.lu_zero_compensation(axes, matrix(1,n,1L), matrix(0,n,1L))
  same_space(ans$W0, matrix(1,n,1L))
  # Prediction at new continuous Z_O rows.
  new_w <- matrix(1, 1L, 1L)
  pred <- up$.lu_zero_compensation(ans$spec, new_w, 0*new_w,
    matrix(c(1, 0.125), 1L, 2L))
  stopifnot(max(abs(unname(pred$W0)-unname(new_w)))<1e-12)
  axes$axes <- 1:2
  expect_identification_error(up$.lu_zero_compensation(axes,
    matrix(1,n,1L), matrix(0,n,1L)),
    "compensation_direction_depends_on_unidentified_decomposition")
  # Nested compensation spaces for x3 in {-1, 0, 1}.
  x3 <- rep(c(-1, 0, 1), length.out = n)
  z3 <- cbind(1, x3, deparse.level = 0)
  W3 <- cbind(1, x3, x3^2, deparse.level = 0)
  nested <- up$.lu_zero_compensation(make_spec(z3, "axes", axes = 1:2), W3, 0*W3)
  same_space(nested$W0, W3)

  # Locally unique loading direction.
  raw <- make_spec(zc, "unique_up_to_common_sign")
  raw$kind <- "raw_unique"
  r <- up$.lu_zero_compensation(raw, zc, 0*zc)
  stopifnot(identical(r$W0, zc), identical(r$W1, 0*zc), identical(r$prefix, "theta."))

  # Prediction within and beyond the fitted design span.
  edge3 <- make_spec(z3, "edge", axes = 1:2, reference_direction = c(0.7, 0.3))
  ans <- up$.lu_zero_compensation(edge3, W3, 0*W3)
  same_space(ans$W0, W3)
  check_reuse(ans, W3, 0*W3, z3)
  again <- up$.lu_zero_compensation(ans$spec, W3[3:1, ], 0*W3[3:1, ], z3[3:1, ])
  stopifnot(identical(again$W0, ans$W0[3:1, , drop = FALSE]))
  for (x in c(0.5, 2))
    expect_identification_error(up$.lu_zero_compensation(ans$spec,
      matrix(c(1, x, x^2), 1L), matrix(0, 1L, 3L), matrix(c(1, x), 1L)),
      NULL)
  expect_identification_error(up$.lu_zero_compensation(ans$spec,
    matrix(c(1, 1, 0), 1L), matrix(0, 1L, 3L), matrix(c(1, 1), 1L)),
    NULL)

  # Three saturated patterns with exact indicator columns.
  level <- rep(1:3, length.out = n)
  zf <- cbind("(Intercept)" = 1, FA = +(level == 1), FB = +(level == 2))
  prof <- unique(zf); rownames(prof) <- NULL
  lab <- up$.lu_zero_pattern_labels(prof)
  Wf <- zf; colnames(Wf)[1L] <- "Intercept"
  three <- make_spec(zf, "groups", profiles = prof, labels = lab,
    group = match(do.call(paste, as.data.frame(zf)), do.call(paste, as.data.frame(prof))),
    group_variance = c(0.5, 0.7, 0.9))
  ans <- up$.lu_zero_compensation(three, Wf, 0*Wf)
  stopifnot(identical(lab, c("FA=1,FB=0", "FA=0,FB=1", "FA=0,FB=0")),
    identical(colnames(ans$W0), paste0("Intercept|", lab)),
    all(ans$W0 %in% c(0, 1)), all(colSums(ans$W0) > 0))
  for (loading in list(c(1, 1, 1), c(-0.4, 0.8, 2), c(0.3, -0.2, 1)))
    same_space(ans$W0, Wf*loading[level])
  check_reuse(ans, Wf, 0*Wf, zf)
  expect_identification_error(up$.lu_zero_compensation(ans$spec,
    Wf[1:2, ], 0*Wf[1:2, ], rbind(zf[1, ], c(1, 1, 1))),
    NULL)

  # Baseline profiling with constant compensation.
  id <- rep(seq_len(n), each = 4L)
  dat <- data.frame(id=id, time=rep(c(0.1,0.3,0.6,0.9),n),
    R=1, C=1, Z=rep(z,each=4L))
  dat$Y <- 0.2 + 0.3*dat$Z + dat$time + sin(id)/10
  check <- up$.lu_validate_data(dat, "id", "time", "Y", "R", "C", "Z")
  XV <- up$.lu_design(dat, check$sf, "Z")
  stage1 <- up$.cimehr_shared_engine$.cimehr_fit_visiting(dat$time,
    check$visit, check$subject, dat$C[check$sf], XV,
    time_precision=0, score_tol=1e-10, Ei_subj=check$Ei)
  scalar$group_variance <- 0.8
  expect_identification_error(up$.lu_stage3(dat, check$sf, check$subject,
    dat$time, dat$Y, dat$R, check$visit, stage1, "Z", character(), "time",
    omega=rep(0.5,n), kappa=rep(0.4,n), include_theta=TRUE,
    zero_spec=scalar), "compensation_not_separable_from_outcome")
}

# Fitted zero-frailty designs.
sim_zero <- function(n, seed, sd_logU = 0, w_effect = 0) {
  set.seed(seed + 1000); W <- rnorm(n)
  set.seed(seed)
  Z <- rep(c(0, 1), length.out = n); X <- rnorm(n)
  logU <- if (sd_logU > 0) rnorm(n, -.5 * sd_logU^2, sd_logU) else rep(0, n)
  q <- rnorm(n, 0, .9)
  b <- .35 * logU + .4 * q + rnorm(n, 0, .6)
  m <- rpois(n, 10 * exp(.25 * Z - .15 * X) * exp(logU))
  id <- rep(seq_len(n), pmax(m, 1L)); has <- m[id] > 0
  N <- length(id); tt <- rep(NA_real_, N)
  tt[has] <- runif(sum(has), .025, 4.975)
  o <- order(id, tt); id <- id[o]; tt <- tt[o]; has <- has[o]
  rr <- rep(NA_integer_, N); yy <- rep(NA_real_, N)
  k <- sum(has); ii <- id[has]
  rr[has] <- as.integer(rnorm(k) < .25 + .2 * Z[ii] - .1 * X[ii] + .55 * logU[ii] + q[ii] + w_effect * W[ii])
  yy[has] <- 1 + .8 * Z[ii] - .4 * X[ii] + .2 * tt[has] + b[ii] + rnorm(k, 0, .7)
  yy[has & rr == 0L] <- NA_real_
  d <- data.frame(id = id, time = tt, Y = yy, R = rr, Z = Z[id], X = X[id], C = 5)
  d$W <- W[d$id]
  d
}
# Numerically identical covariates with different names.
dz <- sim_zero(300, 12)
dz$Xalias <- dz$X
set.seed(20261013); level <- sample(1:3, 300L, TRUE)
dz$FA <- +(level[dz$id] == 1); dz$FB <- +(level[dz$id] == 2); dz$FC <- +(level[dz$id] == 3)
# Recording influenced by the exclusion variable W.
dw <- sim_zero(300, 12, w_effect = 1)
dw[c('FA', 'FB', 'FC')] <- dz[c('FA', 'FB', 'FC')]
methods <- c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')
zero_fit <- function(method, data = dz, ...) do.call(fit_method, c(list(method = method,
  data = data), utils::modifyList(list(visit_covs = c('Z', 'X'), long_covs = c('Z', 'X'),
  censor_col = 'C', seed = 1, controls = list(pairwise_control = list(score_tol = 1e-7))),
  list(...))))
frailty_variance <- function(f) if (grepl('spline', f$method_key))
  f$fit$nuisance_summary$visiting_process$sigma2_zeta else unname(coef(f)['sigma2_zeta'])
intercept_name <- function(method) if (grepl('spline', method)) '(Intercept)' else 'Intercept'

for (method in methods) {
  # Equal outcome and recording fixed designs.
  f <- suppressWarnings(zero_fit(method))
  alias <- suppressWarnings(zero_fit(method, obs_fixed_covs = c('Z', 'Xalias')))
  stopifnot(identical(frailty_variance(f), 0))
  comp <- paste0('compensation.', intercept_name(method))
  stopifnot(!comp %in% names(coef(f)), !any(grepl('group|[.]c[0-9]', names(coef(f)))),
    identical(f$diagnostics$zero_frailty_compensation$status, 'omitted_no_relevant_exclusion'),
    identical(alias$diagnostics$zero_frailty_compensation, f$diagnostics$zero_frailty_compensation))
  out <- summary_outcome(f)
  out_alias <- summary_outcome(alias)
  stopifnot(all(out$point_valid), all(out$se_available),
    all(is.finite(out$lower)), all(is.finite(out$upper)),
    identical(out$estimate, out_alias$estimate), identical(out$se, out_alias$se),
    identical(out$lower, out_alias$lower), identical(out$upper, out_alias$upper),
    identical(out$ci_available, out_alias$ci_available),
    all(is.finite(confint(f))))
  if (grepl('spline', method)) stopifnot(any(startsWith(out$parameter, 'baseline.')))
  obs <- summary_observation(f)
  stopifnot(identical(obs$parameter, c('alpha.(Intercept)', 'alpha.Z', 'alpha.X', 'tau2.(Intercept)')),
    all(is.finite(population_trajectory(f, times = c(1, 2))$fit)),
    all(is.finite(baseline_mean(f, times = c(1, 2))$fit)))
  if (grepl('spline', method)) {
    stopifnot(all(is.finite(population_trajectory(f, times = c(1, 2), se.fit = TRUE)$se)),
      all(is.finite(baseline_mean(f, times = c(1, 2), se.fit = TRUE)$se)))
  }

  # Binary Z across exclusion-restriction settings.
  f <- zero_fit(method, data = dw, obs_fixed_covs = c('Z', 'X', 'W'),
    obs_random_covs = 'Z', outcome_random_covs = 'Z')
  int <- intercept_name(method)
  stopifnot(identical(f$diagnostics$zero_frailty_compensation$status, 'estimated'),
    identical(summary_observation(f)$parameter[5:6], c('tau2.Z=0', 'tau2.Z=1')),
    identical(grep('^compensation[.]', names(coef(f)), value = TRUE),
      paste0('compensation.', int, '|Z=', 0:1)),
    all(is.finite(summary_outcome(f)$lower)), all(is.finite(summary_outcome(f)$upper)))
  f <- suppressWarnings(zero_fit(method, obs_random_covs = 'Z', outcome_random_covs = 'Z'))
  stopifnot(identical(f$diagnostics$zero_frailty_compensation$status, 'omitted_no_relevant_exclusion'),
    identical(summary_observation(f)$parameter[4:5], c('tau2.Z=0', 'tau2.Z=1')),
    !any(grepl('^compensation[.]', names(coef(f)))),
    all(is.finite(summary_outcome(f)$lower)), all(is.finite(summary_outcome(f)$upper)))

  # Factor levels with redundant dummy columns.
  f <- zero_fit(method, data = dw, obs_fixed_covs = c('Z', 'X', 'W'),
    obs_random_covs = c('FA', 'FB'), outcome_random_covs = c('FA', 'FB'))
  g <- zero_fit(method, data = dw, obs_fixed_covs = c('Z', 'X', 'W'),
    obs_random_covs = c('FA', 'FB', 'FC'), outcome_random_covs = c('FA', 'FB'))
  comp <- grep('^compensation[.]', names(coef(f)), value = TRUE)
  stopifnot(identical(comp, paste0('compensation.', int, '|',
      c('FA=0,FB=0', 'FA=1,FB=0', 'FA=0,FB=1'))),
    identical(grep('^tau2[.]', summary_observation(f)$parameter, value = TRUE),
      c('tau2.FA=0,FB=0', 'tau2.FA=1,FB=0', 'tau2.FA=0,FB=1')),
    identical(unname(coef(f)[c('beta.Z', 'beta.X', comp)]),
      unname(coef(g)[grepl('^(beta|compensation)[.]', names(coef(g)))])),
    identical(summary_outcome(f)$se, summary_outcome(g)$se))
}

# Spline nuisance reuse and conditional prediction.
args <- list(dw, covars_visit_XV = c('Z', 'X'), covars_outcome_fixed_XY = c('Z', 'X'),
  covars_obs_fixed_XO = c('Z', 'X', 'W'), covars_obs_random_link_ZO = 'Z',
  covars_outcome_random_link_ZY = 'Z', pairwise_control = list(score_tol = 1e-7))
f <- do.call(CIMEHR_spline, c(args, list(retain_nuisance_fit = TRUE)))
g <- do.call(CIMEHR_spline, c(args, list(nuisance_fit = f$fit$nuisance_fit)))
stopifnot(identical(coef(f), coef(g)), identical(f$se, g$se))
nd <- dw[!duplicated(dw$id), ][1:3, ]
p <- predict(f, newdata = nd, times = c(1, 2), type = 'observed',
  kappa = f$fit$outcome_functionals$compensation_multiplier[1:3], se.fit = TRUE)
stopifnot(all(is.finite(p$fit)), all(is.finite(p$se)))
# Prediction at nonbinary Z after fitting binary Z.
for (z in c(0.5, 2)) {
  nz <- nd; nz$Z[2] <- z
  expect_identification_error(predict(f, newdata = nz, times = c(1, 2), type = 'observed',
    kappa = f$fit$outcome_functionals$compensation_multiplier[1:3], se.fit = TRUE))
}
old <- f$fit$nuisance_fit; old$zero_spec$schema <- 1L
expect_identification_error(do.call(CIMEHR_spline, c(args, list(nuisance_fit = old))),
  'incompatible_zero_frailty_nuisance_refit_required', 'observation')

# (6) Positive visiting frailty.
dp <- sim_zero(300, 12, sd_logU = .7)
reference <- list(
  cimehr = c(0.939513471052, -0.489763758753, 0.119835646997, 0.0631692880191),
  cimehr_hl = c(0.938272815697, -0.48829256448, 0.119537150101, 0.0629744812549),
  cimehr_spline = c(0.945809093841, -0.494406841505, 0.119789596848, 0.0628852641996),
  cimehr_spline_hl = c(0.944567735783, -0.492946310446, 0.119498908097, 0.0626989555412))
for (method in methods) {
  f <- fit_method(method, dp, visit_covs = c('Z', 'X'), long_covs = c('Z', 'X'),
    censor_col = 'C', seed = 1, controls = list(pairwise_control = list(score_tol = 1e-7)))
  est <- unname(c(coef(f)[c('beta.Z', 'beta.X')], f$se[c('beta.Z', 'beta.X')]))
  stopifnot(frailty_variance(f) > 0,
    max(abs(est - reference[[method]])) < 1e-6,
    paste0('theta.', intercept_name(method)) %in% names(coef(f)),
    !any(grepl('^(compensation|tau2)[.]', names(coef(f)))),
    !any(grepl('^exclusion_restriction', names(f$diagnostics))),
    is.null(f$fit$zero_frailty_compensation), is.null(f$fit$spline_spec$zero_compensation),
    all(is.finite(population_trajectory(f, times = c(1, 2), se.fit = grepl('spline', method))$fit)))
  if (!grepl('spline', method))
    stopifnot(all(c('delta.(Intercept)', 'sigma2_q.(Intercept)') %in% names(coef(f))))
}
