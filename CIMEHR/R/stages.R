.stage_table <- function(object, stage=c('outcome','visiting','observation')) {
  stage<-match.arg(stage)
  if(isTRUE(object$method_key%in%c('cimehr_spline','cimehr_spline_hl'))&&stage!='outcome')return(.cimehr_variant_table(.spline_stage_table(object,stage),object))
  if(stage=='outcome')return(.outcome_table(object))
  out<-list()
  append_block<-function(value,prefix,se=NULL,V=NULL,type='reported') {
    if(is.null(value)||!is.numeric(value)||is.matrix(value)||!length(value))return(invisible(NULL))
    nm<-names(value) %||% as.character(seq_along(value))
    bad_cov<-rep(FALSE,length(value))
    if(is.null(se)&&is.matrix(V)&&nrow(V)==length(value)) {
      se<-.report_covariance_se(V)
      bad_cov<-attr(se,'numerical_failure')
    }
    if(is.null(se))se<-rep(NA_real_,length(value))
    if(!is.null(names(se))&&all(nm%in%names(se))) {
      names(bad_cov)<-names(se)
      se<-se[nm];bad_cov<-bad_cov[nm]
    }
    if(length(se)!=length(value))se<-rep(NA_real_,length(value))
    out[[length(out)+1L]]<<-data.frame(parameter=paste0(prefix,nm),stage=stage,
      estimate=as.numeric(value),se=as.numeric(se),variance_type=type,
      .se_numerical_failure=as.logical(bad_cov))
  }
  if(isTRUE(object$method_key%in%c('cimehr','cimehr_hl'))) {
    pattern<-if(stage=='visiting')'^(gamma[.]|sigma2_zeta$)' else '^(alpha[.]|delta[.]|sigma2_q[.]|tau2[.])'
    nm<-grep(pattern,names(object$coefficients),value=TRUE)
    append_block(object$coefficients[nm],'',object$se[nm],type=object$variance_type %||% 'reported')
  } else {
    n<-object$nuisance %||% list();inf<-object$inference %||% list()
    if(stage=='visiting') {
      gamma_vcov<-n$gamma_vcov %||% n$vcov_gamma
      gamma_type<-if(!is.null(n$gamma_se)||!is.null(gamma_vcov))
        n$gamma_variance_type %||% object$variance_type %||% 'reported'
        else if(!is.null(inf$vcov_gamma_model_based))'model_based' else 'reported'
      append_block(n$gamma,'gamma.',se=n$gamma_se,
        V=gamma_vcov %||% inf$vcov_gamma_model_based,type=gamma_type)
      v<-n$frailty_variance %||% n$sigma2
      if(!is.null(v))append_block(stats::setNames(v,'frailty_variance'),'',se=n$se_sigma2,
        type=if(!is.null(n$se_sigma2))object$variance_type %||% 'reported' else 'reported')
      if(is.numeric(n$Lambda)&&length(n$Lambda)==1L)
        append_block(stats::setNames(n$Lambda,'Lambda'),'',se=n$se_Lambda)
    } else append_block(n$alpha,'alpha.',V=n$vcov_alpha)
  }
  if(!length(out))return(data.frame())
  tab<-do.call(rbind,out);dg<-object$diagnostics %||% list()
  tab$point_valid<-is.finite(tab$estimate)&isTRUE(dg$ok %||% TRUE)&isTRUE(dg$converged %||% TRUE)&isTRUE(dg$identified %||% TRUE)
  regular<-rep(isTRUE(dg$regular %||% dg$covariance_regular %||% TRUE),nrow(tab))
  conditional<-rep(isTRUE(dg$conditional_inference_available %||% dg$conditional_inference_valid),nrow(tab))
  scope<-rep(dg$inference_scope %||% object$vcov_type %||% 'reported stage covariance',nrow(tab))
  reason<-rep(.inference_boundary_reason(dg),nrow(tab))
  if(stage=='visiting') {
    gamma<-startsWith(tab$parameter,'gamma.')
    visiting_ok<-isTRUE(dg$gamma_converged %||% dg$visiting$converged %||% TRUE)
    regular[gamma]<-visiting_ok
    scope[gamma]<-'Stage-1 visiting-regression covariance; independent of downstream variance-face selection'
    reason[gamma]<-'visiting-regression inference is unavailable'
    variance<-tab$parameter%in%c('sigma2_zeta','frailty_variance')
    regular[variance]<-visiting_ok&is.finite(tab$estimate[variance])&tab$estimate[variance]>0&!isTRUE(dg$frailty_boundary)
    conditional[variance]<-FALSE
    scope[variance]<-'Stage-1 frailty-variance covariance'
    reason[variance]<-'frailty-variance Wald inference is unavailable at a zero variance or unverified frailty branch'
  }
  zero_variance<-grepl('^(sigma2_q[.]|tau2[.]|sigma2_zeta$|frailty_variance$)',tab$parameter)&is.finite(tab$estimate)&tab$estimate==0
  regular[zero_variance]<-FALSE;conditional[zero_variance]<-FALSE
  tab$df<-Inf;tab$boundary<-isTRUE(dg$variance_boundary)||isTRUE(dg$frailty_boundary)
  tab<-.inference_reporting_fields(tab,regular,conditional,scope,reason,object=object)
  if(isTRUE(object$method_key%in%c('cimehr_hl','cimehr_spline_hl'))) .cimehr_variant_table(tab,object) else tab
}

coef_stage <- function(object,stage=c('outcome','visiting','observation')) {
  z<-.stage_table(object,match.arg(stage));if(!nrow(z))return(numeric())
  stats::setNames(z$estimate,z$parameter)
}
summary_outcome <- function(object,...) .stage_table(object,'outcome')
summary_visiting <- function(object,numerical_wald=FALSE,...)
  .report_numerical_wald(.stage_table(object,'visiting'),numerical_wald)
summary_observation <- function(object,numerical_wald=FALSE,...)
  .report_numerical_wald(.stage_table(object,'observation'),numerical_wald)
extract_coefficient <- function(object,term,stage=c('outcome','visiting','observation')) {
  z<-.stage_table(object,match.arg(stage))
  nm<-if(term%in%z$parameter)term else paste0('beta.',term)
  if(!nm%in%z$parameter){
    if(identical(object$diagnostics$ok,FALSE)&&
       length(object$diagnostics$message)==1L&&
       !is.na(object$diagnostics$message)&&nzchar(object$diagnostics$message))
      stop('Method fit failed: ',object$diagnostics$message,call.=FALSE)
    stop('Coefficient not found in the requested stage: ',term,call.=FALSE)
  }
  z[match(nm,z$parameter),,drop=FALSE]
}
