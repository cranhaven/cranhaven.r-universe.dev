library(CIMEHR)

ehrjoint_fixture <- function() {
  set.seed(673)
  n <- 220L
  z <- rbinom(n, 1, .5); x <- rnorm(n); w <- rnorm(n); v <- rnorm(n)
  C <- runif(n, 2, 5); u <- rgamma(n, 1, 1)
  m <- rpois(n, 3*C*exp(.2*z-.15*x)*u); m[1L] <- 0L
  do.call(rbind, lapply(seq_len(n), function(i) {
    t <- if (m[i]) sort(runif(m[i], .01, C[i])) else NA_real_
    r <- if (m[i]) rbinom(m[i], 1, plogis(.3+.2*z[i]+.4*w[i]-.3*v[i])) else NA_integer_
    y <- 1+.1*t+.4*z[i]-.25*x[i]+.4*(u[i]-1)+rnorm(length(t))
    y[is.na(r) | r != 1] <- NA_real_
    data.frame(id=i, time=t, Y=y, R=r, C=C[i], Z=z[i], X=x[i], W=w[i], V=v[i])
  }))
}

local({
  d <- ehrjoint_fixture()
  args <- list(method='ehrjoint', data=d, visit_covs=c('Z','X'),
    long_covs=c('Z','X'), outcome_random_covs='Z', censor_col='C',
    controls=list(retain_influence=TRUE))
  default <- do.call(fit_method, args)
  explicit <- do.call(fit_method, c(args, list(obs_fixed_covs=c('Z','X'))))
  for (field in c('coefficients','se','vcov'))
    stopifnot(identical(default[[field]], explicit[[field]]))

  for (link in c('logit','probit')) {
    selected <- args
    selected$obs_fixed_covs <- c('Z','W','V')
    selected$controls$obs_link <- link
    fit <- do.call(fit_method, selected)
    visited <- d[is.finite(d$time),]
    reference <- glm(R ~ Z + W + V, visited, family=binomial(link=link))
    stopifnot(identical(names(fit$nuisance$alpha),names(coef(reference))),
      isTRUE(all.equal(unname(fit$nuisance$alpha),unname(coef(reference)),tolerance=1e-12)),
      identical(names(fit$nuisance$gamma),c('Z','X')),
      all(c('alpha:W','alpha:V') %in% colnames(fit$stacked_sandwich$bread)),
      !any(c('gamma:W','gamma:V') %in% colnames(fit$stacked_sandwich$bread)),
      identical(fit$refit$arguments$obs_fixed_covs,c('Z','W','V')),
      isTRUE(all.equal(crossprod(fit$stacked_sandwich$influence),
        fit$stacked_sandwich$vcov_joint,tolerance=1e-12)))
    idx <- paste0('outcome:',names(fit$coefficients))
    stopifnot(isTRUE(all.equal(unname(fit$vcov),
      unname(fit$stacked_sandwich$vcov_joint[idx,idx,drop=FALSE]),tolerance=1e-12)))
  }
  intercept <- do.call(fit_method,c(args,list(obs_fixed_covs=character())))
  stopifnot(identical(names(intercept$nuisance$alpha),'(Intercept)'),
    nrow(intercept$nuisance$vcov_alpha)==1L)
  changing <- d
  changing$W <- ifelse(is.finite(changing$time),changing$W+changing$time,changing$W)
  args$data <- changing
  bad <- try(do.call(fit_method,c(args,list(obs_fixed_covs=c('Z','W')))),silent=TRUE)
  stopifnot(inherits(bad,'try-error'),grepl('time-invariant',as.character(bad),fixed=TRUE))
})

local({
  # Recording-covariate defaults and overrides.
  d <- ehrjoint_fixture()
  args <- list(method='ehrjoint', data=d, visit_covs=c('Z','X'),
    long_covs=c('Z','X'), outcome_random_covs='Z', censor_col='C')
  same <- function(a, b) for (field in c('coefficients','se','vcov','nuisance'))
    stopifnot(identical(a[[field]], b[[field]]))
  overridden <- do.call(fit_method, c(args, list(controls=list(visit_covs='Z'))))
  reference <- do.call(fit_method, modifyList(args, list(visit_covs='Z')))
  same(overridden, reference)
  stopifnot(identical(names(overridden$nuisance$alpha), c('(Intercept)','Z')),
    identical(names(overridden$nuisance$gamma), 'Z'),
    identical(overridden$refit$arguments$obs_fixed_covs, 'Z'))
  same(do.call(EHRJoint, c(args[-1L], list(controls=list(visit_covs='Z')))), overridden)
  same(CIMEHR:::.curve_bootstrap_refit(overridden, seq_along(unique(d$id)), 1L), overridden)

  explicit <- do.call(fit_method, c(args, list(obs_fixed_covs=c('Z','X'),
    controls=list(visit_covs='Z'))))
  stopifnot(identical(names(explicit$nuisance$alpha), c('(Intercept)','Z','X')),
    identical(names(explicit$nuisance$gamma), 'Z'),
    !identical(explicit$coefficients, overridden$coefficients))
  same(do.call(fit_method, c(args, list(controls=list(visit_covs='Z',
    obs_fixed_covs=c('Z','X'))))), explicit)
  intercept <- do.call(fit_method, c(args, list(obs_fixed_covs=character(),
    controls=list(visit_covs='Z'))))
  stopifnot(identical(names(intercept$nuisance$alpha), '(Intercept)'))
})
