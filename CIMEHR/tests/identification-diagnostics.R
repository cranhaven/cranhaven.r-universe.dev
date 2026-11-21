library(CIMEHR)

make_dat<-function(seed=813,n=600L,loading=c(.8,0,0),excluded=0,visiting_sd=0) {
 set.seed(seed);m<-6L;z<-rbinom(n,1,.5);x<-rnorm(n);w<-rnorm(n);u<-rnorm(n)
 q<-rnorm(n,sd=.3);rx<-rnorm(n,sd=.3);rw<-rnorm(n,sd=.3)
 counts<-if(visiting_sd==0)rep(m,n) else pmax(1L,rpois(n,8*exp(visiting_sd*u-.5*visiting_sd^2)))
 id<-rep(seq_len(n),counts)
 d<-data.frame(id=id,time=unlist(lapply(counts,function(m)seq(.05,.95,length.out=m))),C=1,Z=z[id],X=x[id],W=w[id])
 obs<-((loading[1]+loading[2]*x+loading[3]*w)*u+q+rx*x+rw*w)[id]
 d$R<-as.integer(-.2+.8*d$X+.4*d$Z+excluded*d$W+obs+rnorm(length(id))>0)
 d$Y<-.2+.15*d$time+.8*d$Z-.4*d$X+(.6*u+rnorm(n,sd=.4))[id]+rnorm(length(id),sd=.4)
 d$Y[d$R==0]<-NA_real_;d$Xalias<-d$X;d
}

methods <- c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')
d <- make_dat()
fit_case <- function(method, data = d, ...) {
  args <- list(method = method, data = data, visit_covs = c('Z', 'X'),
    long_covs = c('Z', 'X'), censor_col = 'C')
  if (grepl('spline', method)) args$controls <- list(baseline_df = 3)
  suppressWarnings(do.call(fit_method, utils::modifyList(args, list(...))))
}
for (method in methods) {
  f <- fit_case(method)
  g <- fit_case(method, obs_fixed_covs = c('Z', 'Xalias'))
  a <- f$diagnostics$identification_diagnostics
  b <- g$diagnostics$identification_diagnostics
  stopifnot(is.list(a), isTRUE(a$zero_frailty),
    all(c('exclusion', 'relevance', 'compensation', 'loading') %in% names(a)),
    is.character(f$diagnostics$identification_warning),
    length(f$diagnostics$identification_warning) > 0L,
    identical(a$warning_codes, 'compensation_omitted'),
    identical(f$diagnostics$zero_frailty_compensation$status, 'omitted_no_relevant_exclusion'),
    identical(f$diagnostics$zero_frailty_compensation$reason, 'no_excluded_direction'),
    identical(a$exclusion$extra_rank, 0L), identical(b$exclusion$extra_rank, 0L),
    identical(a$exclusion$outcome_rank, b$exclusion$outcome_rank),
    identical(a$exclusion$observation_rank, b$exclusion$observation_rank))
  s <- summary_outcome(f); t <- summary_outcome(g)
  stopifnot(identical(s$estimate, t$estimate), identical(s$se, t$se),
    identical(s$lower, t$lower), identical(s$upper, t$upper),
    all(is.finite(s$lower)), all(is.finite(s$upper)),
    all(s$ci_available), all(s$conditional_inference_available),
    !any(s$regular_inference_valid))
  # Conditional covariance with weak diagnostic signals.
  stopifnot(all(s$inference_status == 'conditional_supported'),
    identical(s$inference_status, t$inference_status),
    identical(s$inference_valid, t$inference_valid), all(s$inference_valid))
  stopifnot(isTRUE(all.equal(a$compensation$minimum_residual_fraction,
    b$compensation$minimum_residual_fraction, tolerance = 1e-12)),
    isTRUE(all.equal(a$compensation$condition_number,
      b$compensation$condition_number, tolerance = 1e-12)))
  stopifnot(all(is.finite(population_trajectory(f, times=c(.2,.7), bandwidth=.6)$fit)),
    all(is.finite(baseline_mean(f, times=c(.2,.7), bandwidth=.6)$fit)))
}

# Recording-covariate span and relevance.
f <- fit_case('cimehr', make_dat(excluded=1.2), obs_fixed_covs=c('Z','X','W'))
g <- fit_case('cimehr', make_dat(excluded=0), obs_fixed_covs=c('Z','X','W'))
a <- f$diagnostics$identification_diagnostics
b <- g$diagnostics$identification_diagnostics
stopifnot(a$exclusion$extra_rank == 1L, b$exclusion$extra_rank == 1L,
  a$relevance$dimension == 1L, b$relevance$dimension == 1L,
  a$relevance$statistic > a$relevance$threshold,
  b$relevance$statistic < b$relevance$threshold,
  identical(f$diagnostics$zero_frailty_compensation$status, 'estimated'),
  identical(g$diagnostics$zero_frailty_compensation$status, 'omitted_no_relevant_exclusion'),
  identical(f$diagnostics$zero_frailty_compensation$reason, NA_character_),
  identical(g$diagnostics$zero_frailty_compensation$reason, 'weak_relevance'),
  identical(f$diagnostics$zero_frailty_compensation$relevance_statistic, a$relevance$statistic),
  identical(g$diagnostics$zero_frailty_compensation$relevance_statistic, b$relevance$statistic),
  all(is.finite(summary_outcome(f)$lower)), all(is.finite(summary_outcome(g)$lower)))
stopifnot(all(summary_outcome(f)$inference_status == 'conditional_supported'),
  all(summary_outcome(f)$inference_valid),
  all(summary_outcome(g)$inference_status == 'conditional_supported'),
  all(summary_outcome(g)$inference_valid))
# One-dimensional Wald signal.
stopifnot(abs(a$relevance$statistic - abs(coef(f)['alpha.W']/f$se['alpha.W'])) < 1e-8,
  abs(b$relevance$statistic - abs(coef(g)['alpha.W']/g$se['alpha.W'])) < 1e-8)

# Legacy fit with missing diagnostic screens.
legacy <- f
legacy$diagnostics$identification_diagnostics <- NULL
if (is.list(legacy$fit$diagnostics))
  legacy$fit$diagnostics$identification_diagnostics <- NULL
actual <- summary_outcome(f); unknown <- summary_outcome(legacy)
stopifnot(all(unknown$inference_status == 'conditional_supported'),
  all(unknown$inference_valid), all(unknown$ci_available),
  identical(actual$estimate, unknown$estimate), identical(actual$se, unknown$se),
  identical(actual$lower, unknown$lower), identical(actual$upper, unknown$upper))

# Equal rank with different loading strengths.
f <- fit_case('cimehr', make_dat(seed=810,n=1000,loading=c(.8,0,0),excluded=.5),
  obs_fixed_covs=c('Z','X','W'), obs_random_covs=c('X','W'))
g <- fit_case('cimehr', make_dat(seed=810,n=1000,loading=c(.7,.4,-.4),excluded=.5),
  obs_fixed_covs=c('Z','X','W'), obs_random_covs=c('X','W'))
a <- f$diagnostics$identification_diagnostics
b <- g$diagnostics$identification_diagnostics
stopifnot(a$loading$third_largest_signal < a$loading$threshold,
  b$loading$third_largest_signal > b$loading$threshold,
  all(is.finite(summary_outcome(f)$lower)), all(is.finite(summary_outcome(g)$lower)))
stopifnot(all(summary_outcome(f)$inference_status == 'conditional_supported'),
  all(summary_outcome(f)$inference_valid),
  all(summary_outcome(g)$inference_status == 'conditional_supported'),
  all(summary_outcome(g)$inference_valid),
  !any(summary_outcome(g)$regular_inference_valid))
obs <- summary_observation(f)
alpha <- obs[startsWith(obs$parameter, 'alpha.'), , drop = FALSE]
delta <- obs[startsWith(obs$parameter, 'delta.'), , drop = FALSE]
stopifnot(all(alpha$inference_status == 'conditional_supported'),
  all(alpha$inference_valid), all(alpha$ci_available),
  all(delta$inference_status == 'conditional_supported'),
  all(delta$inference_valid), all(delta$ci_available),
  'weak_loading' %in% a$warning_codes)
