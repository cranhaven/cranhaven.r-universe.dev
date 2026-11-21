.trajectory_cimehr_followup_range <- function(visit_times, censor_times,
    entry_times=NULL, basis_range=NULL) {
  visit_times<-visit_times[is.finite(visit_times)]
  censor_times<-censor_times[is.finite(censor_times)]
  if(!length(visit_times)||!length(censor_times))
    stop('Prediction requires finite visit and censoring times.',call.=FALSE)
  lower<-if(length(entry_times)&&all(is.finite(entry_times)))min(entry_times) else
    min(c(0,visit_times,entry_times[is.finite(entry_times)]))
  ans<-c(lower,max(censor_times))
  if(!is.null(basis_range)) {
    if(length(basis_range)!=2L||any(!is.finite(basis_range))||diff(basis_range)<=0)
      stop('Stored spline boundary knots are invalid.',call.=FALSE)
    ans<-c(max(ans[1L],basis_range[1L]),min(ans[2L],basis_range[2L]))
  }
  if(any(!is.finite(ans))||diff(ans)<=0)
    stop('The fitted follow-up and spline domains have no positive-width overlap.',call.=FALSE)
  ans
}

.trajectory_cimehr_prediction_spec <- function(data, subject_first, fixed_covars,
    fixed_structure, time_col, times, additional_covars=character(), subject_index=NULL,
    checked=character()) {
  modifiers <- unique(c(unlist(lapply(fixed_covars,function(v) {
    terms <- if(is.null(fixed_structure))NULL else fixed_structure$columns[[v]]
    if(is.null(terms))v else as.character(terms$modifier)
  }),use.names=FALSE),additional_covars))
  modifiers <- setdiff(modifiers,c('(Intercept)','Intercept'))
  if(length(setdiff(modifiers,names(data))))
    stop('Prediction modifiers are absent from the fitted data.',call.=FALSE)
  profiles <- data[subject_first,modifiers,drop=FALSE]
  rownames(profiles) <- NULL
  unavailable <- ''
  if(!is.null(subject_index)&&length(modifiers)) {
    varying <- modifiers[vapply(modifiers,function(v) {
      if(v%in%checked)return(FALSE)
      x<-data[[v]];reference<-profiles[[v]][subject_index]
      !is.numeric(x)||any(!is.finite(x))||
        any(abs(x-reference)>1e-10*pmax(1,abs(x)))
    },logical(1))]
    if(length(varying)) {
      profiles<-NULL
      unavailable<-paste('Explicit time-dependent newdata is required for:',paste(varying,collapse=', '))
    }
  }
  list(fixed_covars=as.character(fixed_covars),fixed_structure=fixed_structure,
       time_col=time_col,time_range=range(times[is.finite(times)]),
       modifiers=modifiers,profiles=profiles,profile_unavailable_reason=unavailable,
       population='original eligible subject cohort; one equally weighted profile per subject')
}

.trajectory_cimehr_core <- function(object) {
  if(is.list(object$longitudinal_outcome)||is.list(object$spline_spec))object else {
    if(!is.list(object$fit))stop('CIMEHR fitted components are unavailable.',call.=FALSE)
    object$fit
  }
}

.trajectory_cimehr_engine <- function(object) {
  .cimehr_object_engine(object)$.cimehr_spline
}

.trajectory_cimehr_baseline <- function(object,times,bandwidth=NULL) {
  core <- .trajectory_cimehr_core(object)
  if(!is.null(core$spline_spec)) {
    if(is.null(times))times<-seq(core$spline_spec$boundary_knots[1L],
      core$spline_spec$boundary_knots[2L],length.out=101L)
    if(!is.numeric(times)||!length(times)||any(!is.finite(times)))
      stop('times must contain finite numeric values.',call.=FALSE)
    engine <- .trajectory_cimehr_engine(object)
    B <- engine$.cimehr_spline_eval(times,core$spline_spec$baseline)
    if(ncol(B)!=length(core$baseline_spline))
      stop('Stored spline baseline coefficients do not match their basis.',call.=FALSE)
    ans <- data.frame(time=times,fit=.cimehr_stable_matvec(B,core$baseline_spline))
    D <- matrix(0,length(times),length(core$coefficients),
      dimnames=list(NULL,names(core$coefficients)))
    D[,names(core$baseline_spline)] <- B
    attr(ans,'design') <- D
    attr(ans,'vcov') <- core$vcov
    attr(ans,'covariance_factor') <- core$covariance_factor
    attr(ans,'baseline_model') <- 'fixed-dimensional regression spline'
    return(ans)
  }
  b <- core$longitudinal_outcome$baseline
  dl <- core$visiting_process$Lambda0_hat
  if(!is.data.frame(b)||!all(c('time','dA')%in%names(b))||length(dl)!=nrow(b))
    stop('CIMEHR baseline and visiting increments are unavailable or misaligned.',call.=FALSE)
  if(!is.null(names(dl))) {
    named_time <- suppressWarnings(as.numeric(names(dl)))
    if(any(!is.finite(named_time))||any(abs(named_time-b$time)>1e-8*pmax(1,abs(b$time))))
      stop('CIMEHR baseline and visiting increment times do not match.',call.=FALSE)
  }
  if(is.null(times))times<-sort(unique(b$time))
  ans <- data.frame(time=times,fit=.trajectory_local_linear(
    b$time,b$dA,as.numeric(dl),times,bandwidth))
  attr(ans,'baseline_model') <- 'local-linear smoothing of cumulative baseline numerator against visiting baseline'
  ans
}

.trajectory_cimehr_fixed_design <- function(spec,newdata,times) {
  if(!is.data.frame(newdata)||nrow(newdata)<1L)
    stop('newdata must contain one or more subject covariate profiles.',call.=FALSE)
  if(!is.numeric(times)||!length(times)||any(!is.finite(times)))
    stop('times must contain finite numeric values.',call.=FALSE)
  profile <- rep(seq_len(nrow(newdata)),each=length(times))
  tt <- rep(times,times=nrow(newdata))
  fixed <- spec$fixed_covars
  D <- matrix(0,length(tt),length(fixed),dimnames=list(NULL,fixed))
  structure <- spec$fixed_structure
  origin <- if(is.null(structure$time_origin))0 else structure$time_origin
  scale <- if(is.null(structure$time_scale))1 else structure$time_scale
  if(length(origin)!=1L||!is.finite(origin)||length(scale)!=1L||!is.finite(scale)||scale<=0)
    stop('Stored fixed-path time origin/scale is invalid.',call.=FALSE)
  for(j in seq_along(fixed)) {
    v <- fixed[j]
    terms <- if(is.null(structure))NULL else structure$columns[[v]]
    if(is.null(terms))terms<-data.frame(modifier=v,power=0)
    if(!is.data.frame(terms)||!all(c('modifier','power')%in%names(terms))||
       !nrow(terms)||anyNA(terms[,c('modifier','power')])||any(!terms$power%in%c(0,1)))
      stop('Stored fixed-path terms are invalid for ',v,'.',call.=FALSE)
    for(k in seq_len(nrow(terms))) {
      modifier<-as.character(terms$modifier[k])
      if(modifier%in%c('(Intercept)','Intercept'))x<-rep(1,nrow(newdata)) else {
        if(!modifier%in%names(newdata)||!is.numeric(newdata[[modifier]])||
           any(!is.finite(newdata[[modifier]])))
          stop('newdata requires a finite numeric modifier: ',modifier,call.=FALSE)
        x<-newdata[[modifier]]
      }
      D[,j]<-D[,j]+x[profile]*if(terms$power[k]==0)1 else (tt-origin)/scale
    }
  }
  list(D=D,profile=profile,time=tt)
}

.trajectory_cimehr_mean <- function(object,newdata=NULL,times,bandwidth=NULL) {
  core <- .trajectory_cimehr_core(object)
  spec <- core$prediction_spec
  if(is.null(newdata)) {
    if(is.null(spec$profiles)) {
      reason<-spec$profile_unavailable_reason
      if(!is.null(reason)&&nzchar(reason))stop(reason,call.=FALSE)
      stop('This older fit has no stored population profiles; supply newdata or refit with prediction metadata.',call.=FALSE)
    }
    newdata<-spec$profiles
  }
  if(!is.data.frame(newdata)||nrow(newdata)<1L)
    stop('newdata must contain one or more subject covariate profiles.',call.=FALSE)
  if(!is.null(core$spline_spec)) {
    engine <- .trajectory_cimehr_engine(object)
    z <- engine$.cimehr_spline_newdata_design(core,newdata,times,type='population')
    fit <- .cimehr_stable_matvec(z$D,core$coefficients)
    if(length(z$profile)>nrow(newdata))names(fit) <- NULL
    ans <- data.frame(profile=z$profile,time=z$time,fit=fit)
    attr(ans,'design') <- z$D
    attr(ans,'vcov') <- core$vcov
    attr(ans,'covariance_factor') <- core$covariance_factor
    attr(ans,'prediction_model') <- 'spline'
    return(ans)
  }
  if(is.null(spec))
    stop('This older fit lacks the fixed-path specification; refit to predict covariate trajectories.',call.=FALSE)
  z <- .trajectory_cimehr_fixed_design(spec,newdata,times)
  beta <- core$longitudinal_outcome$beta_Y
  if(!identical(colnames(z$D),names(beta)))
    stop('Prediction fixed-design columns do not match the fitted coefficients.',call.=FALSE)
  baseline <- .trajectory_cimehr_baseline(object,times,bandwidth)
  ans <- data.frame(profile=z$profile,time=z$time,
    fit=rep(baseline$fit,times=nrow(newdata))+.cimehr_stable_matvec(z$D,beta))
  attr(ans,'prediction_model') <- 'unspecified baseline'
  ans
}
