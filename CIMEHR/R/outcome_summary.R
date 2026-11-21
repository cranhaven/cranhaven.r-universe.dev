.inference_boundary_reason <- function(diagnostics) {
  reason <- c(if (isTRUE(diagnostics$frailty_boundary)) 'frailty variance is on the boundary',
    if (isTRUE(diagnostics$observation_variance_boundary)) 'observation variance is on the boundary')
  scope <- if (!length(reason)) 'full-estimator covariance regularity is not established' else
    paste0(paste(reason, collapse = '; '), '; reported SE holds the selected branch or face fixed and omits selection uncertainty')
  if (isTRUE(diagnostics$frailty_weak_identification)) scope <- paste0(scope,
    '; the positive frailty variance has a weak standardized signal relative to zero (descriptive diagnostic); this does not suppress computed covariance, SEs or intervals')
  scope
}

.report_covariance_se <- function(V) {
  v <- diag(V)
  good <- is.finite(v) & v >= 0
  ans <- rep(NA_real_, length(v)); names(ans) <- names(v)
  ans[good] <- sqrt(v[good])
  attr(ans, 'numerical_failure') <- is.nan(v) | (!is.na(v) & (!is.finite(v) | v < 0))
  ans
}

.report_metadata_list <- function(x) if (is.list(x)) x else list()

.report_confirmed_failure <- function(object) {
  dg <- .report_metadata_list(object$diagnostics)
  native <- .report_metadata_list(object$fit)
  ndg <- .report_metadata_list(native$diagnostics)
  flag <- function(name) dg[[name]] %||% ndg[[name]]
  point_failed <- identical(flag('ok'),FALSE) || identical(flag('converged'),FALSE) ||
    identical(flag('identified'),FALSE)
  list(point_failed=point_failed,covariance_failed=isTRUE(flag('covariance_failed')))
}

.report_identification_info <- function(object) {
  native <- .report_metadata_list(object$fit)
  dg <- .report_metadata_list(object$diagnostics)
  ndg <- .report_metadata_list(native$diagnostics)
  details <- dg$identification_diagnostics %||% ndg$identification_diagnostics
  status <- dg$identification_status %||% ndg$identification_status %||% 'not_assessed'
  warnings <- dg$identification_warning %||% ndg$identification_warning %||% character()
  warnings <- unique(as.character(warnings)); warnings <- warnings[!is.na(warnings) & nzchar(warnings)]
  cimehr <- isTRUE(object$method_key %in% c('cimehr','cimehr_hl','cimehr_spline','cimehr_spline_hl'))
  zero <- cimehr && (isTRUE(details$zero_frailty) || isTRUE(dg$frailty_boundary) ||
    isTRUE(native$visiting_process$sigma2_zeta == 0) ||
    isTRUE(native$nuisance_summary$visiting_process$sigma2_zeta == 0))
  list(cimehr = cimehr, zero_frailty = zero, status = as.character(status)[1L],
    warning = warnings, diagnostics = details)
}

.report_curve_inference_metadata <- function(ans, object) {
  info <- .report_identification_info(object)
  failure <- .report_confirmed_failure(object)
  if (!info$cimehr || (!info$zero_frailty && is.null(info$diagnostics) &&
      !failure$point_failed && !failure$covariance_failed)) return(ans)
  n <- nrow(ans)
  bootstrap <- 'n_boot' %in% names(ans)
  available <- if(all(c('lower','upper') %in% names(ans)))
    is.finite(ans$lower) & is.finite(ans$upper) else rep(FALSE,n)
  dg <- .report_metadata_list(object$diagnostics)
  regular <- isTRUE(dg$regular)
  conditional <- isTRUE(dg$conditional_inference_available %||% dg$conditional_inference_valid)
  valid <- available & (regular | conditional)
  status <- ifelse(available, if(regular)'regular' else if(conditional)'conditional' else 'not_established',
    if(all(c('lower','upper') %in% names(ans)))'unavailable' else 'not_requested')
  if(info$zero_frailty) {
    assessment <- .cimehr_identification_assessment(info$diagnostics,stage='outcome')
    fit_ok <- !failure$point_failed
    eligible <- available & fit_ok & !failure$covariance_failed & (regular | conditional)
    valid[available] <- FALSE
    status[available] <- if(!fit_ok)'fit_invalid' else 'not_assessed'
    valid[eligible] <- assessment$valid
    status[eligible] <- assessment$status
    attr(ans,'inference_reason') <- assessment$reason
    attr(ans,'inference_scope') <- assessment$scope
    if(!fit_ok) attr(ans,'inference_reason') <-
      'the parent point fit failed its convergence or identification checks; numerical curve limits do not establish inference'
    else if(failure$covariance_failed || !(regular | conditional))
      attr(ans,'inference_reason') <-
        'the parent fit does not provide an eligible regular or conditional covariance; numerical curve limits alone do not establish inference'
    if(bootstrap) {
      bootstrap_ok <- available & fit_ok & is.finite(ans$n_boot) & ans$n_boot >= 2L
      valid[available] <- FALSE
      status[available] <- if(!fit_ok)'fit_invalid' else 'not_assessed'
      valid[bootstrap_ok] <- assessment$valid
      status[bootstrap_ok] <- assessment$status
      attr(ans,'parent_identification_assessment') <- assessment
      if(fit_ok) attr(ans,'inference_reason') <- paste(
        'Full-refit bootstrap inference is retained where the parent point fit and compensation parameterization pass their existing checks and at least two finite replicate values support the interval;',
        'support is conditional on the model and bootstrap assumptions, not a calibration of bootstrap coverage or a requirement for the parent analytic covariance.')
      attr(ans,'inference_scope') <- paste(
        'Participant bootstrap refits all stages. Reported diagnostic support is inherited from the parent fit;',
        'it is not calibration of bootstrap coverage or a selected-face analytic covariance.')
    }
  }
  if(bootstrap && !info$zero_frailty) {
    bootstrap_ok <- available & !failure$point_failed & is.finite(ans$n_boot) & ans$n_boot >= 2L
    valid[available] <- FALSE
    status[available] <- if(failure$point_failed)'fit_invalid' else 'bootstrap_unavailable'
    valid[bootstrap_ok] <- TRUE
    status[bootstrap_ok] <- 'bootstrap_supported'
    attr(ans,'inference_reason') <- paste(
      'full-refit bootstrap inference is retained where the point fit is valid and at least two finite replicate values support the interval;',
      'this is a computability check under the bootstrap assumptions, not a guarantee of coverage')
    attr(ans,'inference_scope') <-
      'Participant bootstrap refits all stages and uses replicate variability, not the parent analytic covariance.'
  }
  if(bootstrap && failure$covariance_failed) {
    attr(ans,'parent_covariance_failed') <- TRUE
    attr(ans,'parent_covariance_warning') <- paste(
      'The parent analytic covariance failed. These full-refit bootstrap intervals use replicate variability instead;',
      'the analytic failure does not by itself invalidate the bootstrap intervals.')
    reason <- attr(ans,'inference_reason')
    if(length(reason) && !grepl('[.]$',reason)) reason <- paste0(reason,'.')
    attr(ans,'inference_reason') <- paste(reason,attr(ans,'parent_covariance_warning'))
  }
  if(failure$point_failed || (failure$covariance_failed && !bootstrap)) {
    valid[available] <- FALSE
    status[available] <- if(failure$point_failed)'fit_invalid' else 'covariance_failure'
    attr(ans,'inference_reason') <- if(failure$point_failed)
      'the parent point fit failed its convergence or identification checks; stored numerical curve limits are retained but do not establish inference' else
      'the parent fit reports a covariance calculation failure; stored numerical curve limits are retained but are not supported inference'
    attr(ans,'inference_scope') <- if(bootstrap)
      'Participant bootstrap refits all stages; retained numerical limits do not override the confirmed parent-fit failure.' else
      'Retained numerical curve limits only; a confirmed parent-fit failure prevents supported inference.'
  }
  attr(ans,'identification_status') <- info$status
  attr(ans,'identification_warning') <- info$warning
  attr(ans,'identification_diagnostics') <- info$diagnostics
  attr(ans,'ci_available') <- available
  attr(ans,'inference_valid') <- valid
  attr(ans,'inference_status') <- status
  ans
}

.report_call_inference <- function(x) {
  if (is.call(x) || is.list(x)) x[['inference']] else NULL
}

.report_inference_mode <- function(object) {
  if (is.null(object)) return(NULL)
  native <- .report_metadata_list(object$fit)
  native_model <- .report_metadata_list(native$model_info)
  object_model <- .report_metadata_list(object$model_info)
  dg <- .report_metadata_list(object$diagnostics)
  native_dg <- .report_metadata_list(native$diagnostics)
  candidates <- list(native_model$inference, object_model$inference,
    dg$inference, .report_call_inference(native$call), .report_call_inference(object$call))
  for (x in candidates) {
    if (is.character(x) && length(x) == 1L && x %in% c('none', 'stacked', 'conditional')) return(x)
  }
  if (isTRUE(object$method_key %in% c('cimehr', 'cimehr_hl')) &&
      identical(native_dg$inference_scope, 'not computed') &&
      is.null(native$inference)) return('none')
  NULL
}

.report_se_status <- function(table, object = NULL) {
  n <- nrow(table)
  status <- rep('unknown', n)
  dg <- .report_metadata_list(object$diagnostics)
  native <- .report_metadata_list(object$fit)
  native_inf <- .report_metadata_list(native$inference)
  native_stacked <- .report_metadata_list(native$stacked_sandwich)
  object_inf <- .report_metadata_list(object$inference)
  mode <- .report_inference_mode(object)
  if (identical(mode, 'none')) status[] <- 'disabled'
  else if (identical(dg$inference_scope, 'not computed') ||
      identical(object$vcov_type, 'not returned')) status[] <- 'not_computed'
  joint <- object$vcov_all %||% native_inf$vcov_joint %||%
    native_stacked$vcov_joint %||% object_inf$vcov_all
  selected_face <- isTRUE(native_inf$conditional_active_face_inference_available) ||
    isTRUE(native_stacked$conditional_active_face_inference_available) ||
    isTRUE(dg$conditional_inference_valid) || isTRUE(dg$conditional_inference_available)
  if (!identical(mode, 'none') && is.matrix(joint) && selected_face &&
      isTRUE(object$method_key %in% c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl'))) {
    status[table$parameter_boundary] <- 'boundary_not_computed'
  }
  if (!identical(mode, 'none')) {
    if (identical(dg$identified, FALSE)) status[] <- 'identifiability_failure'
    if (isTRUE(dg$covariance_failed)) {
      affected <- rep(TRUE,n)
      if(isTRUE(object$method_key %in% c('cimehr','cimehr_hl','cimehr_spline','cimehr_spline_hl')) &&
          'stage' %in% names(table)) affected <- table$stage!='visiting'
      status[affected] <- 'numerical_failure'
    }
  }
  invalid_number <- is.nan(table$se) | (!is.na(table$se) & (!is.finite(table$se) | table$se < 0))
  if ('.se_numerical_failure' %in% names(table))
    invalid_number <- invalid_number | table$.se_numerical_failure
  invalid_number[is.na(invalid_number)] <- FALSE
  status[invalid_number] <- 'numerical_failure'
  status[is.finite(table$se) & table$se >= 0] <- 'available'
  status
}

.inference_reporting_fields <- function(table, regular, conditional = FALSE,
    scope = 'reported covariance', reason = NULL, object = NULL) {
  n <- nrow(table)
  regular <- rep_len(as.logical(regular), n); regular[is.na(regular)] <- FALSE
  conditional <- rep_len(as.logical(conditional), n); conditional[is.na(conditional)] <- FALSE
  available <- is.finite(table$se) & table$se >= 0
  usable <- table$point_valid & available & !is.na(table$df) & table$df > 0
  usable[is.na(usable)] <- FALSE
  valid <- usable & regular
  conditional <- usable & (regular | conditional)
  q <- suppressWarnings(stats::qt(.975, table$df))
  dg <- object$diagnostics %||% list()
  if (!'parameter_boundary' %in% names(table)) table$parameter_boundary <-
    grepl('^(sigma2_q[.]|sigma2_zeta$|frailty_variance$)', table$parameter) &
      is.finite(table$estimate) & table$estimate == 0
  if (!'fit_boundary' %in% names(table)) {
    coefficient_boundary <- FALSE
    if (is.numeric(object$coefficients)) {
      variance <- grepl('^(sigma2_q[.]|sigma2_zeta$|frailty_variance$)', names(object$coefficients))
      coefficient_boundary <- any(variance & is.finite(object$coefficients) & object$coefficients == 0)
    }
    table$fit_boundary <- rep(isTRUE(dg$variance_boundary) || isTRUE(dg$frailty_boundary) ||
      isTRUE(dg$observation_variance_boundary) || coefficient_boundary ||
      any(table$parameter_boundary) || any(table$boundary %in% TRUE), n)
  }
  table$se_available <- available
  table$se_status <- .report_se_status(table, object)
  table$regular_inference_valid <- valid
  table$ci_available <- valid | conditional
  table$inference_valid <- valid | conditional
  table$conditional_inference_available <- conditional
  table$lower <- ifelse(table$ci_available, table$estimate - q * table$se, NA_real_)
  table$upper <- ifelse(table$ci_available, table$estimate + q * table$se, NA_real_)
  table$regular_lower <- ifelse(valid, table$estimate - q * table$se, NA_real_)
  table$regular_upper <- ifelse(valid, table$estimate + q * table$se, NA_real_)
  table$conditional_lower <- ifelse(conditional, table$estimate - q * table$se, NA_real_)
  table$conditional_upper <- ifelse(conditional, table$estimate + q * table$se, NA_real_)
  table$inference_scope <- rep_len(as.character(scope), n)
  if (is.null(reason)) reason <- 'full-estimator covariance regularity is not established'
  reason <- rep_len(as.character(reason), n)
  se_reason <- c(available = 'standard error is available',
    disabled = 'covariance calculation was disabled',
    not_computed = 'covariance was not computed or returned',
    boundary_not_computed = 'the selected-face covariance does not estimate uncertainty for this boundary variance',
    numerical_failure = 'a covariance calculation or reported variance/standard error is numerically invalid',
    identifiability_failure = 'the fit reports an identification failure',
    unknown = 'standard error is unavailable; its cause is not established by the saved fit')
  table$ci_status <- ifelse(!table$point_valid | is.na(table$point_valid), 'fit_invalid',
    ifelse(!available, 'se_unavailable',
      ifelse(is.na(table$df) | table$df <= 0, 'reference_unavailable',
        ifelse(valid, 'regular', ifelse(conditional, 'conditional',
          ifelse(table$parameter_boundary, 'boundary_uncalibrated', 'inference_unavailable'))))))
  table$inference_reason <- ifelse(table$ci_status == 'fit_invalid', 'point estimate failed the fit diagnostics',
    ifelse(!available, unname(se_reason[table$se_status]),
      ifelse(table$ci_status == 'reference_unavailable', 'valid reference degrees of freedom are not available',
        ifelse(valid, 'regular inference available',
          ifelse(conditional, paste0(reason, '; only inference within the stated conditional scope is available'),
            ifelse(table$parameter_boundary,
              'a standard error is available, but regular or conditional boundary coverage is not established', reason))))))
  identification <- .report_identification_info(object)
  table$identification_status <- rep(identification$status,n)
  table$identification_warning <- rep(paste(identification$warning,collapse='; '),n)
  table$inference_status <- ifelse(!table$point_valid | is.na(table$point_valid),'fit_invalid',
    ifelse(valid,'regular',ifelse(conditional,'conditional','unavailable')))
  assess <- which(identification$zero_frailty & conditional & !valid)
  if(length(assess)) {
    stages <- if('stage' %in% names(table))as.character(table$stage) else rep('outcome',n)
    stages[stages=='observation' & startsWith(table$parameter,'alpha.')] <- 'observation_mean'
    for(stage in unique(stages[assess])) {
      rows <- assess[stages[assess]==stage]
      assessment <- .cimehr_identification_assessment(identification$diagnostics,stage=stage)
      table$inference_valid[rows] <- assessment$valid
      table$inference_status[rows] <- assessment$status
      table$inference_reason[rows] <- paste(assessment$reason,assessment$scope,sep='; ')
    }
  }
  failure <- .report_confirmed_failure(object)
  if(identification$cimehr && failure$covariance_failed) {
    affected <- if('stage' %in% names(table))table$stage!='visiting' else rep(TRUE,n)
    table$inference_valid[affected] <- FALSE
    table$regular_inference_valid[affected] <- FALSE
    table$inference_status[affected] <- 'covariance_failure'
    table$ci_status[affected] <- ifelse(table$ci_available[affected],'numerical_only','covariance_failure')
    if('conditional_inference_valid' %in% names(table))table$conditional_inference_valid[affected] <- FALSE
    table$se_status[affected] <- 'numerical_failure'
    table$regular_lower[affected] <- NA_real_
    table$regular_upper[affected] <- NA_real_
    table$inference_reason[affected] <-
      'the fit reports a covariance calculation failure; stored numerical SEs and limits are retained but are not supported inference'
    table$inference_scope[affected] <- paste0(table$inference_scope[affected],
      '; retained numerical limits only because the fit reports a covariance failure')
  }
  if(identification$cimehr && failure$point_failed) {
    table$point_valid[] <- FALSE
    table$inference_valid[] <- FALSE
    table$regular_inference_valid[] <- FALSE
    table$inference_status[] <- 'fit_invalid'
    table$ci_status[] <- 'fit_invalid'
    if('conditional_inference_valid' %in% names(table))table$conditional_inference_valid[] <- FALSE
    table$regular_lower[] <- NA_real_
    table$regular_upper[] <- NA_real_
    table$inference_reason[] <-
      'the point fit reports a convergence or identification failure; any retained numerical SEs and limits do not establish inference'
  }
  table$.se_numerical_failure <- NULL
  table
}

.report_numerical_wald <- function(table, numerical_wald = FALSE) {
  if (!is.logical(numerical_wald) || length(numerical_wald) != 1L || is.na(numerical_wald))
    stop('numerical_wald must be TRUE or FALSE.', call. = FALSE)
  if (!numerical_wald) return(table)
  if (!nrow(table)) {
    table$numerical_lower <- table$numerical_upper <- numeric()
    table$numerical_wald_available <- logical()
    table$numerical_wald_scope <- character()
    return(table)
  }
  usable <- table$point_valid & table$se_available & !is.na(table$df) & table$df > 0
  usable[is.na(usable)] <- FALSE
  q <- suppressWarnings(stats::qt(.975, table$df))
  table$numerical_lower <- ifelse(usable, table$estimate - q * table$se, NA_real_)
  table$numerical_upper <- ifelse(usable, table$estimate + q * table$se, NA_real_)
  table$numerical_wald_available <- usable
  table$numerical_wald_scope <- ifelse(!usable, 'unavailable',
    ifelse(table$ci_available, table$inference_scope,
      'explicit numerical Wald calculation only; reference-distribution coverage is not established'))
  if (any(usable & !table$ci_available)) warning(
    'Requested numerical Wald limits include parameters without established regular or conditional coverage.',
    call. = FALSE)
  table
}

.outcome_table <- function(x) {
  b<-x$coefficients
  if(isTRUE(x$method_key%in%c('cimehr','cimehr_hl')))b<-b[grepl('^(beta|theta|compensation)[.]',names(b))]
  nm<-names(b);if(!length(b))return(data.frame())
  se<-x$se
  bad_cov<-NULL
  if(is.null(se)&&is.matrix(x$vcov)) {
    se<-.report_covariance_se(x$vcov)
    bad_cov<-attr(se,'numerical_failure')
    names(bad_cov)<-names(se)
  }
  if (isTRUE(x$method_key %in% c('cimehr','cimehr_hl','cimehr_spline','cimehr_spline_hl')) &&
      is.matrix(x$vcov)) {
    vv <- diag(x$vcov)
    invalid <- is.nan(vv) | (!is.na(vv) & (!is.finite(vv) | vv < 0))
    if (any(invalid)) {
      if (is.null(names(vv))) names(vv) <- rownames(x$vcov)
      bad_cov <- invalid; names(bad_cov) <- names(vv)
      se[names(vv)[invalid]] <- NA_real_
    }
  }
  if(is.null(names(se))&&length(se)==length(x$coefficients))names(se)<-names(x$coefficients)
  if(!is.null(bad_cov)&&is.null(names(bad_cov))&&length(bad_cov)==length(x$coefficients))names(bad_cov)<-names(x$coefficients)
  ss<-as.numeric(se[nm]);if(length(ss)!=length(b))ss<-rep(NA_real_,length(b))
  df<-x$df
  if(is.null(df)||!length(df))df<-rep(Inf,length(b))
  else if(length(df)==1L)df<-rep(df,length(b))
  else if(!is.null(names(df)))df<-df[nm]
  else if(length(df)==length(x$coefficients))df<-df[match(nm,names(x$coefficients))]
  else df<-rep(NA_real_,length(b))
  dg<-x$diagnostics %||% list()
  converged<-isTRUE(dg$converged %||% dg$ok %||% all(is.finite(x$coefficients)))
  identified<-isTRUE(dg$identified %||% TRUE)
  ok<-isTRUE(dg$ok %||% all(is.finite(x$coefficients)))
  point<-is.finite(b)&converged&identified&ok
  regular<-if(!is.null(dg$outcome_inference_available)) isTRUE(dg$outcome_inference_available) else
    isTRUE(dg$regular %||% dg$covariance_regular %||% TRUE)
  conditional<-isTRUE(dg$conditional_inference_available %||% dg$conditional_inference_valid)
  scope<-dg$outcome_inference_scope %||% dg$inference_scope %||% x$vcov_type %||% x$variance_label %||% 'reported outcome covariance'
  reason<-dg$outcome_inference_reason %||% .inference_boundary_reason(dg)
  table<-data.frame(parameter=nm,stage='outcome',estimate=as.numeric(b),se=ss,df=as.numeric(df),
    lower=NA_real_,upper=NA_real_,point_valid=point,inference_valid=FALSE,
    boundary=isTRUE(dg$variance_boundary)||isTRUE(dg$frailty_boundary),
    variance_type=x$variance_type %||% 'reported',row.names=NULL)
  if(!is.null(bad_cov))table$.se_numerical_failure<-as.logical(bad_cov[nm])
  table<-.inference_reporting_fields(table,regular,conditional,scope,reason,object=x)
  if(isTRUE(x$method_key%in%c('cimehr_hl','cimehr_spline_hl'))) .cimehr_variant_table(table,x) else table
}

coef.cimehr_fit <- function(object,...) object$coefficients

vcov.cimehr_fit <- function(object,all=FALSE,...) if(isTRUE(all))object$vcov_all %||% object$inference$vcov_all %||% object$stacked_sandwich$vcov_joint else object$vcov

confint.cimehr_fit <- function(object,parm,level=.95,type=c('available','regular','conditional'),...) {
  if(!is.numeric(level)||length(level)!=1L||!is.finite(level)||level<=0||level>=1)stop('level must lie between 0 and 1.')
  type<-match.arg(type)
  tab<-.outcome_table(object);if(!missing(parm))tab<-tab[if(is.numeric(parm))parm else match(parm,tab$parameter),,drop=FALSE]
  q<-stats::qt((1+level)/2,tab$df)
  valid<-switch(type,available=tab$ci_available,regular=tab$regular_inference_valid,conditional=tab$conditional_inference_available)
  a<-cbind(tab$estimate-q*tab$se,tab$estimate+q*tab$se);a[is.na(valid)|!valid,]<-NA_real_
  dimnames(a)<-list(tab$parameter,c('lower','upper'))
  if(type=='conditional'||(type=='available'&&any(tab$ci_available&!tab$regular_inference_valid,na.rm=TRUE))) {
    attr(a,'inference_type')<-type
    attr(a,'interval_scope')<-stats::setNames(ifelse(tab$inference_status %in% c('covariance_failure','fit_invalid'),'numerical_only',ifelse(tab$regular_inference_valid,'regular',
      ifelse(tab$conditional_inference_available,'conditional','unavailable'))),tab$parameter)
    attr(a,'inference_scope')<-stats::setNames(tab$inference_scope,tab$parameter)
    attr(a,'inference_reason')<-stats::setNames(tab$inference_reason,tab$parameter)
  }
  info <- .report_identification_info(object)
  if(info$cimehr && (info$zero_frailty || any(tab$inference_status %in% c('fit_invalid','covariance_failure')))) {
    attr(a,'inference_valid') <- stats::setNames(tab$inference_valid,tab$parameter)
    attr(a,'inference_status') <- stats::setNames(tab$inference_status,tab$parameter)
    attr(a,'identification_status') <- stats::setNames(tab$identification_status,tab$parameter)
    attr(a,'identification_warning') <- stats::setNames(tab$identification_warning,tab$parameter)
    attr(a,'inference_reason') <- stats::setNames(tab$inference_reason,tab$parameter)
    attr(a,'inference_scope') <- stats::setNames(tab$inference_scope,tab$parameter)
  }
  a
}

summary.cimehr_fit <- function(object,...) {
  ans <- list(method=object$method,coefficients=.outcome_table(object),diagnostics=object$diagnostics,
       variance_label=object$variance_label,assumptions=object$assumptions)
  if(length(object$diagnostics$numerical_warning))
    ans$numerical_warning <- object$diagnostics$numerical_warning
  ans
}

print.cimehr_fit <- function(x,...) {
  cat(x$method,'\n');print(.outcome_table(x),row.names=FALSE)
  line<-.cimehr_identification_line(x$diagnostics)
  if(!is.null(line))cat(line,'\n',sep='')
  line<-.cimehr_numerical_line(x$diagnostics)
  if(!is.null(line))cat(line,'\n',sep='')
  invisible(x)
}
