library(CIMEHR)

`%||%` <- function(x, y) if (is.null(x)) y else x

make_dat<-function(seed=813,n=600L,loading=c(.8,0,0),excluded=0,visiting_sd=0) {
 set.seed(seed);m<-6L;z<-rbinom(n,1,.5);x<-rnorm(n);w<-rnorm(n);u<-rnorm(n)
 q<-rnorm(n,sd=.3);rx<-rnorm(n,sd=.3);rw<-rnorm(n,sd=.3)
 counts<-if(visiting_sd==0)rep(m,n) else pmax(1L,rpois(n,8*exp(visiting_sd*u-.5*visiting_sd^2)))
 id<-rep(seq_len(n),counts)
 d<-data.frame(id=id,time=unlist(lapply(counts,function(m)seq(.05,.95,length.out=m))),C=1,Z=z[id],X=x[id],W=w[id])
 obs<-((loading[1]+loading[2]*x+loading[3]*w)*u+q+rx*x+rw*w)[id]
 d$R<-as.integer(-.2+.8*d$X+.4*d$Z+excluded*d$W+obs+rnorm(length(id))>0)
 d$Y<-.2+.15*d$time+.8*d$Z-.4*d$X+(.6*u+rnorm(n,sd=.4))[id]+rnorm(length(id),sd=.4)
 d$Y[d$R==0]<-NA_real_;d
}
collect <- function(expr) {
  conds <- list()
  value <- withCallingHandlers(expr, warning = function(w) {
    conds[[length(conds) + 1L]] <<- w
    invokeRestart('muffleWarning')
  })
  list(value = value, warnings = conds)
}
id_class <- c('cimehr_identification_warning', 'warning', 'condition')
is_id <- function(w) inherits(w, 'cimehr_identification_warning')
cases <- list(
  no_exclusion = list(data = make_dat(), XO = c('Z', 'X'), ZO = NULL),
  noise_loading = list(data = make_dat(seed = 810, n = 1000, loading = c(.8, 0, 0),
    excluded = .5), XO = c('Z', 'X', 'W'), ZO = c('X', 'W')),
  supported = list(data = make_dat(excluded = 1.2), XO = c('Z', 'X', 'W'), ZO = NULL),
  positive = list(data = make_dat(visiting_sd = .7), XO = c('Z', 'X'), ZO = NULL))
fit_with <- function(how, case) {
  cs <- cases[[case]]
  spline <- grepl('spline', how, ignore.case = TRUE)
  if (how %in% c('CIMEHR', 'CIMEHR_HL', 'CIMEHR_spline', 'CIMEHR_spline_HL'))
    return(do.call(how, c(list(cs$data, covars_visit_XV = c('Z', 'X'),
      covars_outcome_fixed_XY = c('Z', 'X'), covars_obs_fixed_XO = cs$XO,
      covars_obs_random_link_ZO = cs$ZO, censor_col = 'C'),
      if (spline) list(baseline_df = 3))))
  fit_method(how, cs$data, visit_covs = c('Z', 'X'), long_covs = c('Z', 'X'),
    obs_fixed_covs = cs$XO, obs_random_covs = if (is.null(cs$ZO)) character() else cs$ZO,
    censor_col = 'C', controls = if (spline) list(baseline_df = 3) else list())
}
hows <- c('CIMEHR', 'CIMEHR_HL', 'CIMEHR_spline', 'CIMEHR_spline_HL',
  'cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')
id_line <- function(f) paste0('Identification warnings: ',
  paste(f$diagnostics$identification_diagnostics$warning_codes, collapse = ', '),
  '; see diagnostics$identification_warning')
# Printed identification line and summary structure.
outcome_table <- get('.outcome_table', asNamespace('CIMEHR'))
print_520 <- function(f) c(capture.output(cat(f$method, '\n')),
  capture.output(print(outcome_table(f), row.names = FALSE)))
check_summary <- function(f) {
  s <- summary(f)
  stopifnot(identical(s, list(method = f$method, coefficients = outcome_table(f),
      diagnostics = f$diagnostics, variance_label = f$variance_label, assumptions = f$assumptions)),
    is.null(attr(s, 'class')), inherits(s, 'list'),
    !any(startsWith(capture.output(print(s)), 'Identification warnings')))
}
stopifnot(is.null(getS3method('print', 'summary.cimehr_fit', optional = TRUE)))

for (case in c('no_exclusion', 'noise_loading')) for (how in hows) {
  g <- collect(fit_with(how, case)); f <- g$value; dg <- f$diagnostics
  stopifnot(length(g$warnings) == 1L, identical(class(g$warnings[[1L]]), id_class),
    length(dg$identification_warning) > 0L,
    identical(dg$identification_warning, dg$identification_diagnostics$warnings))
  w <- g$warnings[[1L]]
  msg <- paste0('Zero-frailty identification diagnostics: ',
    paste(dg$identification_warning, collapse = ' '))
  # Stored engine warning.
  stopifnot(identical(conditionMessage(w), msg), sum(dg$warnings == msg) == 1L,
    is.null(conditionCall(w)), identical(w$diagnostics, dg$identification_diagnostics))
  caught <- tryCatch({ fit_with(how, case); 'none' },
    cimehr_identification_warning = function(w) 'caught')
  stopifnot(identical(caught, 'caught'))
  printed <- capture.output(print(f))
  stopifnot(sum(startsWith(printed, 'Identification warnings: ')) == 1L,
    identical(printed, c(print_520(f), id_line(f))))
  check_summary(f)
}

# Supported zero-frailty and positive-frailty fits.
for (case in c('supported', 'positive')) for (how in hows) {
  g <- collect(fit_with(how, case)); f <- g$value
  stopifnot(length(g$warnings) == 0L, !length(f$diagnostics$identification_warning),
    !any(startsWith(capture.output(print(f)), 'Identification warnings')),
    identical(capture.output(print(f)), print_520(f)))
  check_summary(f)
}
stopifnot(f$fit$nuisance_summary$visiting_process$sigma2_zeta > 0)

data('cimehr_example', package = 'CIMEHR')
tutorial <- merge(cimehr_example$participants, cimehr_example$visits, by = 'id', all.x = TRUE)
tutorial <- tutorial[order(tutorial$id, tutorial$time, na.last = TRUE), ]
for (key in c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')) {
  g <- collect(fit_method(key, tutorial, censor_col = 'C', visit_covs = c('Z', 'X'),
    cimehr_visit_covs = c('Z', 'X'), obs_fixed_covs = c('Z', 'X'), long_covs = c('Z', 'X'),
    outcome_random_covs = 'Z', obs_random_covs = 'Z'))
  stopifnot(length(g$warnings) == 0L, !length(g$value$diagnostics$identification_warning))
}

# Warning collection for internal refits.
for (how in c('CIMEHR', 'CIMEHR_spline')) {
  f <- suppressWarnings(fit_with(how, 'no_exclusion'))
  g <- collect(meanfit(f, times = c(.23, .59), bootstrap = TRUE, B = 3L, seed = 11L))
  stopifnot(!any(vapply(g$warnings, is_id, logical(1))), length(g$warnings) <= 1L,
    all(vapply(g$warnings, function(w) startsWith(conditionMessage(w),
      'Some bootstrap fits or curve points were unavailable'), logical(1))),
    nrow(g$value$bootstrap$warnings) == 3L,
    all(grepl('Zero-frailty identification diagnostics', g$value$bootstrap$warnings$message, fixed = TRUE)))
}
g <- collect(method_comparisons(cases$no_exclusion$data, methods = c('cimehr', 'cimehr_spline'),
  visit_covs = c('Z', 'X'), long_covs = c('Z', 'X'), obs_fixed_covs = c('Z', 'X'), censor_col = 'C',
  controls = list(cimehr_spline = list(baseline_df = 3))))
stopifnot(length(g$warnings) == 0L,
  all(vapply(g$value, function(f) length(f$diagnostics$identification_warning) > 0L, logical(1))))
g <- collect(fit_with('CIMEHR', 'no_exclusion'))
stopifnot(length(g$warnings) == 1L, is_id(g$warnings[[1L]]))

# Public fits evaluated inside arguments.
d0 <- cases$no_exclusion$data
inner <- function(value) { fit_with('CIMEHR', 'no_exclusion'); value }
for (arg in c('data', 'methods', 'controls', 'dots')) {
  g <- collect(method_comparisons(data = if (arg == 'data') inner(d0) else d0,
    methods = if (arg == 'methods') inner('cimehr') else 'cimehr',
    controls = if (arg == 'controls') inner(list()) else list(),
    visit_covs = if (arg == 'dots') inner(c('Z', 'X')) else c('Z', 'X'),
    long_covs = c('Z', 'X'), obs_fixed_covs = c('Z', 'X'), censor_col = 'C'))
  stopifnot(length(g$warnings) == 1L, is_id(g$warnings[[1L]]), inherits(g$value[[1L]], 'cimehr_fit'),
    length(g$value[[1L]]$diagnostics$identification_warning) > 0L)
}
f <- suppressWarnings(fit_with('CIMEHR', 'no_exclusion'))
g <- collect(meanfit(f, times = inner(c(.23, .59)), bootstrap = TRUE, B = 2L, seed = 11L))
stopifnot(sum(vapply(g$warnings, is_id, logical(1))) == 1L)
# Argument evaluation failure.
g <- collect(method_comparisons(methods = c('lmm', 'cimehr')))
stopifnot(length(g$warnings) == 0L, all(vapply(g$value, function(x) inherits(x, 'cimehr_failure') &&
  grepl('"data" is missing', x$error, fixed = TRUE), logical(1))))

# Prediction covariance and assessment scopes.
assessment <- get('.cimehr_identification_assessment', asNamespace('CIMEHR'))
for (case in c('no_exclusion', 'positive')) {
  f <- suppressWarnings(fit_with('CIMEHR_spline', case))
  d <- cases[[case]]$data
  ids <- unique(d$id)[1:2]
  k <- f$fit$outcome_functionals$kappa %||% f$fit$outcome_functionals$compensation_multiplier
  p <- predict(f, newdata = d[match(ids, d$id), c('Z', 'X')], times = c(.2, .6),
    type = 'observed', kappa = unname(k[as.character(ids)]))
  stopifnot(identical(attr(p, 'inference_scope'), f$diagnostics$inference_scope))
  if (case == 'no_exclusion') stopifnot(identical(attr(p, 'inference_assessment_scope'),
    assessment(f$diagnostics$identification_diagnostics)$scope))
  else stopifnot(is.null(attr(p, 'inference_assessment_scope')))
}

# Bootstrap reason punctuation.
curve_report <- get('.report_curve_inference_metadata', asNamespace('CIMEHR'))
for (boundary in c(FALSE, TRUE)) {
  b <- c(beta.X = -0.5, theta.X = 0.2)
  fit <- structure(list(method_key = 'cimehr', coefficients = b, se = c(beta.X = .06, theta.X = .09),
    diagnostics = list(ok = TRUE, converged = TRUE, identified = TRUE, regular = !boundary,
      conditional_inference_valid = boundary, frailty_boundary = boundary,
      variance_boundary = boundary, covariance_failed = TRUE)), class = 'cimehr_fit')
  boot <- data.frame(time = c(.2, .7), fit = c(1, 1.1), lower = c(.8, .9),
    upper = c(1.2, 1.3), n_boot = c(2L, 2L))
  reason <- attr(curve_report(boot, fit), 'inference_reason')
  stopifnot(grepl('[.] The parent analytic covariance failed[.]', reason), !grepl('..', reason, fixed = TRUE))
  if (!boundary) stopifnot(grepl('not a guarantee of coverage. The parent', reason, fixed = TRUE))
}
