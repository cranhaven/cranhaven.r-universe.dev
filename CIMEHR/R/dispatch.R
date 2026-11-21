`%||%` <- function(x, y) if (is.null(x)) y else x

.engine_cache <- new.env(parent = emptyenv())

.cimehr_build_engine <- function(variant) {
  ns <- environment(.cimehr_build_engine)
  e <- new.env(parent = ns)
  old <- list(ns$.cimehr_common, ns$.cimehr_updated, ns$.cimehr_spline)
  common <- new.env(parent = e)
  new <- list(common, new.env(parent = common), new.env(parent = common))
  relink <- function(x) {
    if (is.environment(x)) {
      at <- which(vapply(old, identical, logical(1), x))
      return(if (length(at)) new[[at]] else x)
    }
    if (is.function(x) && !is.primitive(x)) {
      at <- which(vapply(old, identical, logical(1), environment(x)))
      if (length(at)) environment(x) <- new[[at]]
    }
    x
  }
  for (k in seq_along(old))
    for (name in ls(old[[k]], all.names = TRUE)) assign(name, relink(get(name, old[[k]])), envir = new[[k]])
  e$.cimehr_common <- new[[1L]]
  e$.cimehr_updated <- new[[2L]]
  e$.cimehr_spline <- new[[3L]]
  for (name in c('.cimehr_updated_fit', '.cimehr_spline_fit', '.cimehr_predict_spline', '.cimehr_spline_adapter')) {
    f <- get(name, ns)
    environment(f) <- e
    assign(name, f, envir = e)
  }
  if (identical(variant, 'higher_order_gaussian_moments')) {
    corrected <- .cimehr_hl_laplace(e$.cimehr_common)
    e$.cimehr_updated$.lu_laplace <- corrected
    e$.cimehr_spline$.lu_laplace <- corrected
  }
  .cimehr_prepare_engine(e, variant)
  e
}

.cimehr_engine <- function() {
  if (!exists('cimehr', .engine_cache, inherits = FALSE))
    .engine_cache$cimehr <- .cimehr_build_engine('current')
  .engine_cache$cimehr
}

available_comparison_methods <- function() .cimehr_method_registry

.resolve_method <- function(method) {
  reg <- available_comparison_methods()
  aliases <- c('IIRR-V'='iirr_weighting_visit', 'IIRR-M'='iirr_weighting_marker', 'Liang-V'='liang_visit', 'Liang-M'='liang_marker',
               'LI+IIRR'='li_iirr', 'LI+Liang'='li_liang', 'CIMEHR (updated)'='cimehr', 'CIMEHR_spline'='cimehr_spline', 'CIMEHR_HL'='cimehr_hl', 'CIMEHR_spline_HL'='cimehr_spline_hl')
  if (length(method) != 1L || is.na(method)) stop('Supply one method key or label.', call.=FALSE)
  if (!is.character(method)) stop('Methods are identified by key or label, not by number; see available_comparison_methods().', call.=FALSE)
  m <- as.character(method)
  if (m %in% reg$method) return(m)
  if (m %in% names(aliases)) return(unname(aliases[m]))
  if (m %in% reg$label) return(reg$method[match(m,reg$label)])
  stop('Unknown method; see available_comparison_methods().',call.=FALSE)
}

.method_engine <- function(method) {
  if (method == 'cimehr_hl') return(.cimehr_hl_engine()$.cimehr_updated_fit)
  if (method == 'cimehr_spline_hl') return(.cimehr_hl_engine()$.cimehr_spline_adapter)
  if (method == 'cimehr_spline') return(.cimehr_engine()$.cimehr_spline_adapter)
  if (method == 'cimehr') return(.cimehr_engine()$.cimehr_updated_fit)
  ns <- environment(.method_engine)
  get(paste0('.fit_',method),envir=ns,inherits=FALSE)
}

fit_method <- function(method,data,id='id',time='time',outcome='Y',obs_indicator='R',
                       visit_covs=NULL,long_covs=NULL,outcome_random_covs=NULL,obs_random_covs=outcome_random_covs,
                       censor_col=NULL,entry_col=NULL,obs_fixed_covs=visit_covs,cimehr_visit_covs=visit_covs,
                       outcome_fixed_structure=NULL,outcome_random_structure=NULL,outcome_random_intercept=TRUE,
                       focal=if(length(long_covs))long_covs[1L] else NULL,seed=1L,controls=list(),...) {
  method<-.resolve_method(method)
  controls<-c(controls,list(...))
  if(length(controls) && (is.null(names(controls))||any(!nzchar(names(controls)))||anyDuplicated(names(controls))))
    stop('Controls must have unique nonempty names.',call.=FALSE)
  if(any(names(controls)%in%c('data','d','id','id_col','time','time_col','outcome','y_col','obs_indicator','r_col','entry_col','censor_col')))
    stop('Specify data and structural columns through fit_method arguments, not controls.',call.=FALSE)
  merged_covs<-unique(c(visit_covs,long_covs,outcome_random_covs,obs_random_covs,obs_fixed_covs,cimehr_visit_covs,
                        unlist(controls[intersect(names(controls),c('visit_covs','long_covs','random_covs','outcome_random_covs','obs_random_covs','obs_fixed_covs'))],use.names=FALSE)))
  .validate_data(data,id,time,outcome,obs_indicator,merged_covs,entry_col,censor_col)
  retain_curve_refit<-method%in%.curve_refit_methods()
  if(retain_curve_refit)curve_refit_data<-data
  masked<-.mask_entry_visits(data,id,time,outcome,obs_indicator,entry_col);data<-masked$data
  fixed<-c(lin_ying_marker='marker',liang_marker='marker',jmvl_g_marker='marker',iirr_weighting_marker='marker',iirr_balancing_marker='marker',
           iirr_weighting_visit='visit',lin_ying_visit='visit',liang_visit='visit',jmvl_g_visit='visit',iirr_balancing_visit='visit')
  if(method%in%names(fixed)&&!is.null(controls$process)&&controls$process!=fixed[method])
    stop('Choose the corresponding marker/visit method instead of overriding process.',call.=FALSE)
  native<-.method_engine(method)
  if(!is.null(entry_col)&&!'entry_col'%in%names(formals(native))&&any(data[[entry_col]]!=0))
    stop('This method does not support delayed entry; choose a method with explicit entry-time risk sets.',call.=FALSE)
  # EHRJoint defaults obs_fixed_covs to the effective visiting covariates.
  if(method=='ehrjoint'&&missing(obs_fixed_covs)&&!'obs_fixed_covs'%in%names(controls)&&
     'visit_covs'%in%names(controls))
    obs_fixed_covs<-controls[['visit_covs']]
  args<-.engine_call_args(native,data,id,time,outcome,obs_indicator,visit_covs,cimehr_visit_covs,
                          obs_fixed_covs,long_covs,outcome_random_covs,obs_random_covs,focal,seed,entry_col,censor_col,controls,
                          outcome_random_structure,outcome_random_intercept,outcome_fixed_structure)
  warnings<-character();started<-proc.time()[['elapsed']]
  ans<-withCallingHandlers(do.call(native,args),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
  elapsed<-proc.time()[['elapsed']]-started
  ans<-.cimehr_tag_fit(ans,method)
  ans<-.normalize_fit(ans,method,elapsed,warnings)
  ans$diagnostics$n_visits_at_entry_excluded<-masked$n
  if(method=='cimehr')ans$method<-'CIMEHR'
  ans$call<-match.call();ans$parameter_table<-.outcome_table(ans)
  class(ans)<-unique(c(if(method%in%c('cimehr_hl','cimehr_spline_hl'))'cimehr_hl_fit',if(method%in%c('cimehr_spline','cimehr_spline_hl'))'cimehr_spline_fit', 'cimehr_fit',class(ans)))
  if(retain_curve_refit) {
    ans$refit<-.curve_capture_refit(curve_refit_data,list(
      method=method,id=id,time=time,outcome=outcome,obs_indicator=obs_indicator,
      visit_covs=visit_covs,long_covs=long_covs,outcome_random_covs=outcome_random_covs,
      obs_random_covs=obs_random_covs,censor_col=censor_col,entry_col=entry_col,
      obs_fixed_covs=obs_fixed_covs,cimehr_visit_covs=cimehr_visit_covs,
      outcome_fixed_structure=outcome_fixed_structure,
      outcome_random_structure=outcome_random_structure,
      outcome_random_intercept=outcome_random_intercept,focal=focal,seed=seed,controls=controls))
    ans$diagnostics$object_bytes<-as.numeric(object.size(ans))
  }
  .cimehr_signal_identification_warning(ans)
  ans
}

method_comparisons <- function(data,methods=cimehr_methods('selected'),controls=list(),...) {
  if (!length(methods)) return(stats::setNames(list(), character()))
  argument_error <- tryCatch({
    force(controls)
    for (i in seq_len(...length())) force(...elt(i))
    force(data)
    NULL
  }, error = function(e) e)
  out<-.cimehr_without_identification_signal(lapply(methods,function(m)tryCatch({
    if (!is.null(argument_error)) stop(argument_error)
    fit_method(method=m,data=data,controls=controls[[as.character(m)]] %||%
                 controls[[.resolve_method(m)]] %||% list(),...)
  },error=function(e)
    structure(list(method=m,error=conditionMessage(e)),class='cimehr_failure'))))
  names(out)<-vapply(methods,function(m)available_comparison_methods()$label[match(.resolve_method(m),available_comparison_methods()$method)],character(1))
  out
}
