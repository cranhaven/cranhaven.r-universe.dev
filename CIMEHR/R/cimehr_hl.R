.cimehr_approximation_variant <- function(method) {
  if(method%in%c('cimehr_hl','cimehr_spline_hl'))'higher_order_gaussian_moments' else 'current'
}

.cimehr_assert_nuisance_variant <- function(nuisance, expected) {
  if(is.null(nuisance))return(NULL)
  if(!inherits(nuisance,'cimehr_laplace_nuisance'))
    stop('nuisance_fit must be a retained spline nuisance object.',call.=FALSE)
  if(!is.null(nuisance$approximation_variant)&&!is.null(nuisance$model_info$approximation_variant)&&
      !identical(nuisance$approximation_variant,nuisance$model_info$approximation_variant))
    stop('nuisance_fit has conflicting approximation tags.',call.=FALSE)
  tag<-nuisance$approximation_variant %||% nuisance$model_info$approximation_variant
  description<-nuisance$posterior$approximation
  legacy_current<-identical(description,'Gaussian/Laplace mode and inverse curvature')||
    identical(description,'Gaussian/Laplace; zero shared-frailty branch')
  if(is.null(tag)) {
    if(expected!='current'||!legacy_current)
      stop('nuisance_fit has no compatible approximation tag. Refit nuisance with the requested ordinary/HL method.',call.=FALSE)
    tag<-'current'
  }
  if(!is.character(tag)||length(tag)!=1L||is.na(tag)||!identical(tag,expected))
    stop('nuisance_fit approximation mismatch: expected ',expected,', received ',paste(tag,collapse=', '),'. Refit nuisance with the same method variant.',call.=FALSE)
  if(expected=='current'&&!legacy_current)
    stop('nuisance_fit current tag conflicts with its posterior approximation.',call.=FALSE)
  if(expected=='higher_order_gaussian_moments'&&!(is.character(description)&&length(description)==1L&&description%in%c(
      'Research Taylor correction: third/fourth local log-density derivatives; Gaussian moment closure',
      'Gaussian/Laplace; zero shared-frailty branch')))
    stop('nuisance_fit HL tag conflicts with its posterior approximation.',call.=FALSE)
  .cimehr_tag_nuisance(nuisance,expected)
}

.cimehr_tag_nuisance <- function(nuisance, variant) {
  nuisance$approximation_variant<-variant
  nuisance$model_info$approximation_variant<-variant
  nuisance
}

.cimehr_prepare_engine <- function(engine, variant) {
  sp<-engine$.cimehr_spline
  sp$.cimehr_assert_nuisance_variant<-.cimehr_assert_nuisance_variant
  sp$.cimehr_tag_nuisance<-.cimehr_tag_nuisance
  native<-sp$.cimehr_spline_native
  statements<-as.list(body(native))
  guard<-substitute(nuisance_fit<-.cimehr_assert_nuisance_variant(nuisance_fit,V),list(V=variant))
  body(native)<-as.call(c(statements[1L],list(guard),statements[-1L]))
  sp$.cimehr_spline_native<-native
  nuisance<-sp$.cimehr_spline_nuisance_fit
  statements<-as.list(body(nuisance))
  statements[[length(statements)]]<-substitute(.cimehr_tag_nuisance(out,V),list(V=variant))
  body(nuisance)<-as.call(statements)
  sp$.cimehr_spline_nuisance_fit<-nuisance
  engine$.cimehr_approximation_variant<-variant
  invisible(engine)
}

.cimehr_hl_engine <- function() {
  if(!exists('cimehr_hl',.engine_cache,inherits=FALSE))
    .engine_cache$cimehr_hl<-.cimehr_build_engine('higher_order_gaussian_moments')
  .engine_cache$cimehr_hl
}

.cimehr_object_engine <- function(object) {
  expected<-.cimehr_approximation_variant(object$method_key)
  if(!is.null(object$variant)&&!is.null(object$approximation_variant)&&
      !identical(object$variant,object$approximation_variant))
    stop('Fitted object has conflicting approximation tags.',call.=FALSE)
  variant<-object$variant %||% object$approximation_variant %||% 'current'
  if(!identical(variant,expected))stop('Fitted object method and approximation variant disagree.',call.=FALSE)
  if(expected=='higher_order_gaussian_moments').cimehr_hl_engine() else .cimehr_engine()
}

.cimehr_tag_fit <- function(ans,method) {
  if(!method%in%c('cimehr_spline','cimehr','cimehr_hl','cimehr_spline_hl'))return(ans)
  variant<-.cimehr_approximation_variant(method)
  base<-if(method=='cimehr_hl')'cimehr' else if(method=='cimehr_spline_hl')'cimehr_spline' else method
  ans$variant<-variant;ans$approximation_variant<-variant
  ans$native_base_method<-base
  ans$fit$approximation_variant<-variant
  if(!is.null(ans$fit$model_info))ans$fit$model_info$approximation_variant<-variant
  ans$diagnostics$approximation_variant<-variant
  if(method%in%c('cimehr_hl','cimehr_spline_hl')) {
    ans$method_key<-method
    ans$method<-if(method=='cimehr_hl')'CIMEHR-HL' else 'Spline CIMEHR-HL'
    ans$estimand<-'Higher-order corrected Gaussian posterior moment estimating-equation target'
    ans$diagnostics$approximation_target<-ans$estimand
    ans$diagnostics$message<-'Higher-order Gaussian posterior moments use a local approximation; sandwich inference does not remove approximation bias.'
    ans$assumptions<-c('MOM visiting frailty and analytic third/fourth derivative correction of Gaussian posterior moments',
      'Unchanged within-patient pairwise probit observation likelihood and Gaussian moment functionals',
      paste0('Native outcome equations; covariance: ',ans$vcov_type %||% 'see inference setting'),
      'Higher-order posterior moments remain approximate; no guarantee of unbiasedness or specified numerical bias')
  }
  ans
}

.cimehr_variant_table <- function(table,object) {
  if(object$method_key%in%c('cimehr_hl','cimehr_spline_hl')&&is.data.frame(table)) {
    n<-nrow(table)
    table$method_key<-rep(object$method_key,n)
    table$variant<-rep('higher_order_gaussian_moments',n)
    table$native_base_method<-rep(if(object$method_key=='cimehr_hl')'cimehr' else 'cimehr_spline',n)
  }
  table
}

CIMEHR_HL <- function(
    data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    covars_outcome_random_link_ZY = NULL, covars_obs_fixed_XO = NULL,
    covars_obs_random_link_ZO = NULL, time_precision = 0,
    outcome_fixed_structure = NULL, outcome_random_structure = NULL,
    outcome_random_intercept = TRUE, pairwise_control = list(),
    inference = c("stacked", "none"), derivative_step = 2e-5,
    retain_details = FALSE, entry_col = NULL) {
  inference <- match.arg(inference)
  data <- as.data.frame(data)
  .cimehr_check_numeric_columns(data, c(covars_visit_XV, covars_outcome_fixed_XY,
    covars_outcome_random_link_ZY, covars_obs_fixed_XO, covars_obs_random_link_ZO))
  ans <- fit_method('cimehr_hl', data = data,
    id = id_col, time = time_col, outcome = y_col, obs_indicator = r_col,
    censor_col = censor_col, entry_col = entry_col, visit_covs = covars_visit_XV,
    cimehr_visit_covs = covars_visit_XV,
    obs_fixed_covs = if (is.null(covars_obs_fixed_XO)) character() else covars_obs_fixed_XO,
    long_covs = covars_outcome_fixed_XY,
    outcome_random_covs = if (is.null(covars_outcome_random_link_ZY)) character() else covars_outcome_random_link_ZY,
    obs_random_covs = if (is.null(covars_obs_random_link_ZO)) character() else covars_obs_random_link_ZO,
    outcome_fixed_structure = outcome_fixed_structure,
    outcome_random_structure = outcome_random_structure,
    outcome_random_intercept = outcome_random_intercept,
    controls = list(time_precision = time_precision, pairwise_control = pairwise_control,
      inference = inference, derivative_step = derivative_step, retain_details = retain_details))
  ans$call <- match.call()
  class(ans) <- c("cimehr_hl_fit", "cimehr_fit", "list")
  ans
}
