.trajectory_local_linear <- function(t,dA,dL,times,bandwidth) {
  if(length(t)!=length(dA)||length(t)!=length(dL))
    stop('Baseline increments must have matching lengths.',call.=FALSE)
  if(length(bandwidth)!=1L||!is.finite(bandwidth)||bandwidth<=0)
    stop('bandwidth must be one positive finite number, in the fitted time units.',call.=FALSE)
  ok<-is.finite(t)&is.finite(dA)&is.finite(dL)&dL>0
  t<-t[ok];dA<-dA[ok];dL<-dL[ok]
  one<-function(g) {
    u<-(t-g)/bandwidth;ii<-which(abs(u)<1)
    if(length(unique(t[ii]))<3L)return(NA_real_)
    X<-cbind(1,u[ii]);K<-.75*(1-u[ii]^2)
    scale<-sum(K*dL[ii])
    if(!is.finite(scale)||scale<=0)return(NA_real_)
    M<-crossprod(X,X*(K*dL[ii]/scale))
    if(any(!is.finite(M))||rcond(M)<1e-10)return(NA_real_)
    tryCatch(unname(solve(M,crossprod(X,K*dA[ii]/scale))[1L]),
             error=function(e)NA_real_)
  }
  if(is.numeric(times)&&is.numeric(t)&&is.numeric(dA)&&is.numeric(dL)&&is.numeric(bandwidth)&&
     .trajectory_reference_linalg())
    for(fused in c(TRUE,FALSE)) {
      z<-cimehr_sp_local_linear(as.double(t),as.double(dA),as.double(dL),as.double(times),
        as.double(bandwidth),fused)
      ans<-z$fit;names(ans)<-names(times)
      for(k in which(z$defer))ans[k]<-one(times[[k]])
      probe<-which(!z$defer&!is.na(ans))
      if(length(probe))probe<-probe[unique(round(seq(1,length(probe),length.out=min(length(probe),5L))))]
      if(identical(unname(ans[probe]),vapply(times[probe],one,numeric(1),USE.NAMES=FALSE)))return(ans)
    }
  vapply(times,one,numeric(1))
}

.trajectory_reference_blas <- function()
  isTRUE(tryCatch(grepl('^(lib)?Rblas(\\.0)?\\.(so|dylib|dll)$',
    basename(normalizePath(extSoftVersion()[['BLAS']],mustWork=FALSE))),error=function(e)FALSE))

.trajectory_reference_linalg <- function()
  isTRUE(getOption('matprod')%in%c('default','blas'))&&.trajectory_reference_blas()&&
    isTRUE(tryCatch(grepl('^(lib)?Rlapack\\.(so|dylib|dll)$',
      basename(normalizePath(La_library(),mustWork=FALSE))),error=function(e)FALSE))

.trajectory_method <- function(object) {
  if(!inherits(object,'cimehr_fit'))stop('Supply a fitted CIMEHR package model.',call.=FALSE)
  m<-object$method_key
  supported<-c('cimehr','cimehr_hl','cimehr_spline','cimehr_spline_hl',
               'liang_visit','liang_marker','mi_liang','li_liang','ehrjoint')
  if(length(m)!=1L||!m%in%supported)
    stop('Mean-curve prediction is implemented for the CIMEHR, Liang, MI/LI+Liang and EHRJoint families.',call.=FALSE)
  if(!length(object$coefficients)||any(!is.finite(object$coefficients)))
    stop('The fitted model has unavailable or nonfinite outcome coefficients.',call.=FALSE)
  m
}

.trajectory_range <- function(object) {
  z<-object$fit$prediction_spec$time_range %||% object$prediction$time_range %||%
     object$fit$spline_spec$baseline$boundary_knots
  if(is.null(z)) {
    t<-object$fit$longitudinal_outcome$baseline$time
    if(length(t))z<-range(t,finite=TRUE)
  }
  if(length(z)!=2L||any(!is.finite(z))||diff(z)<=0)
    stop('The fit lacks its prediction time range; supply a fit from the updated package.',call.=FALSE)
  as.numeric(z)
}

.trajectory_times <- function(object,times) {
  range<-.trajectory_range(object)
  if(is.null(times))times<-seq(range[1L],range[2L],length.out=101L)
  if(!is.numeric(times)||!length(times)||any(!is.finite(times)))
    stop('times must be a nonempty finite numeric vector.',call.=FALSE)
  if(any(times<range[1L]-1e-10|times>range[2L]+1e-10))
    stop('Prediction times must lie within the fitted follow-up range.',call.=FALSE)
  sort(unique(as.numeric(times)))
}

.trajectory_bandwidth <- function(object,bandwidth) {
  if(is.null(bandwidth))bandwidth<-.2*diff(.trajectory_range(object))
  if(length(bandwidth)!=1L||!is.finite(bandwidth)||bandwidth<=0)
    stop('bandwidth must be one positive finite number, in the fitted time units.',call.=FALSE)
  as.numeric(bandwidth)
}

.trajectory_uses_smoothing <- function(object) {
  if(grepl('spline',object$method_key))return(FALSE)
  if(grepl('liang',object$method_key))return(.liang_trajectory_uses_smoothing(object))
  TRUE
}

.trajectory_reference_profiles <- function(object,newdata) {
  if(!is.null(newdata))return(newdata)
  if(grepl('^cimehr',object$method_key)) {
    s<-object$fit$prediction_spec
    if(is.null(s$profiles))stop(s$profile_unavailable_reason %||%
      'Supply explicit covariate profiles or refit with prediction metadata.',call.=FALSE)
    s$profiles
  } else object$prediction$reference
}

.trajectory_average_profiles <- function(object,newdata,weights) {
  if(!is.data.frame(newdata)||!nrow(newdata))stop('newdata must be a nonempty data.frame.',call.=FALSE)
  w<-if(is.null(weights))rep(1,nrow(newdata))else weights
  if(!is.numeric(w)||length(w)!=nrow(newdata)||any(!is.finite(w))||any(w<0)||sum(w)<=0)
    stop('weights must give one finite nonnegative weight per profile, with positive sum.',call.=FALSE)
  w<-w/sum(w)
  vars<-if(grepl('^cimehr',object$method_key))object$fit$prediction_spec$modifiers else object$prediction$long_covs
  if(is.null(vars))stop('The fit lacks its prediction design specification.',call.=FALSE)
  if(!all(vars%in%names(newdata)))stop('newdata is missing: ',paste(setdiff(vars,names(newdata)),collapse=', '),call.=FALSE)
  if(!length(vars))return(data.frame(.profile=1))
  z<-lapply(newdata[,vars,drop=FALSE],function(v) {
    if(!is.numeric(v)||any(!is.finite(v)))stop('Prediction modifiers must be finite numeric columns.',call.=FALSE)
    sum(v*w)
  })
  as.data.frame(z,check.names=FALSE)
}

.trajectory_add_se <- function(ans,D,V,se.fit,level,factor=NULL) {
  if(!is.logical(se.fit)||length(se.fit)!=1L||is.na(se.fit))
    stop('se.fit must be TRUE or FALSE.',call.=FALSE)
  if(length(level)!=1L||!is.finite(level)||level<=0||level>=1)
    stop('level must lie strictly between zero and one.',call.=FALSE)
  if(se.fit) {
    if(is.null(D)||(is.null(V)&&is.null(factor)))stop('This fit does not provide mean-curve uncertainty. Coefficient SEs alone do not include baseline uncertainty.',call.=FALSE)
    if(nrow(D)!=nrow(ans))stop('Prediction design and curve rows do not match.',call.=FALSE)
    if(is.null(factor)&&(ncol(D)!=nrow(V)||nrow(V)!=ncol(V)||any(!is.finite(V))))
      stop('A finite, aligned covariance matrix is required for mean-curve intervals.',call.=FALSE)
    if (!is.null(factor)) {
      vv <- .cimehr_factor_variance(D,factor)
      ans$se <- .cimehr_checked_se(vv)
    } else {
      DVD<-(D%*%V)*D;vv<-rowSums(DVD)
      if(any(vv< -1e-8*pmax(1,rowSums(abs(DVD)))))
        stop('The fitted covariance produces a negative curve variance.',call.=FALSE)
      ans$se<-sqrt(pmax(vv,0))
    }
    q<-stats::qnorm(1-(1-level)/2)
    ans$lower<-ans$fit-q*ans$se;ans$upper<-ans$fit+q*ans$se
  }
  ans
}

baseline_mean <- function(object,times=NULL,bandwidth=NULL,se.fit=FALSE,level=.95) {
  m<-.trajectory_method(object);times<-.trajectory_times(object,times)
  bandwidth<-.trajectory_bandwidth(object,bandwidth)
  ans<-if(grepl('^cimehr',m)) .trajectory_cimehr_baseline(object,times,bandwidth) else
    if(m=='ehrjoint') .trajectory_ehr_baseline(object,times,bandwidth) else
      .liang_trajectory_baseline(object,times,bandwidth)
  ans<-.trajectory_add_se(ans,attr(ans,'design'),attr(ans,'vcov'),se.fit,level,attr(ans,'covariance_factor'))
  attr(ans,'estimand')<-'Common baseline mean beta_0(t), on the modeled outcome scale'
  attr(ans,'bandwidth')<-if(.trajectory_uses_smoothing(object))bandwidth else NULL
  .report_curve_inference_metadata(ans,object)
}

.trajectory_profiles <- function(object,newdata,times,bandwidth) {
  m<-object$method_key
  if(grepl('^cimehr',m)) .trajectory_cimehr_mean(object,newdata,times,bandwidth) else
    if(m=='ehrjoint') .trajectory_ehr_mean(object,newdata,times,bandwidth) else
      .liang_trajectory_mean(object,newdata,times,bandwidth)
}

population_trajectory <- function(object,newdata=NULL,times=NULL,bandwidth=NULL,
    aggregate=TRUE,weights=NULL,se.fit=FALSE,level=.95) {
  .trajectory_method(object);times<-.trajectory_times(object,times)
  bandwidth<-.trajectory_bandwidth(object,bandwidth)
  if(!is.logical(aggregate)||length(aggregate)!=1L||is.na(aggregate))
    stop('aggregate must be TRUE or FALSE.',call.=FALSE)
  if(!aggregate&&!is.null(weights))stop('weights applies only to aggregate=TRUE.',call.=FALSE)
  profile_counts<-integer()
  if(is.function(newdata)) {
    row_ids<-NULL
    parts<-lapply(times,function(t) {
      d<-newdata(t)
      if(!is.data.frame(d)||!nrow(d))stop('newdata(time) must return a nonempty data.frame of profiles.',call.=FALSE)
      if(is.null(row_ids))row_ids<<-rownames(d) else if(!identical(row_ids,rownames(d)))
        stop('newdata(time) must preserve the number, row names and ordering of profiles across times.',call.=FALSE)
      profile_counts<<-c(profile_counts,nrow(d))
      if(aggregate)d<-.trajectory_average_profiles(object,d,weights)
      .trajectory_profiles(object,d,t,bandwidth)
    })
    D<-if(all(vapply(parts,function(x)!is.null(attr(x,'design')),logical(1))))
      do.call(rbind,lapply(parts,attr,'design')) else NULL
    V<-attr(parts[[1L]],'vcov');factor<-attr(parts[[1L]],'covariance_factor')
    ans<-do.call(rbind,parts);rownames(ans)<-NULL
  } else {
    if(aggregate) {
      newdata<-.trajectory_reference_profiles(object,newdata)
      profile_counts<-rep(nrow(newdata),length(times))
      newdata<-.trajectory_average_profiles(object,newdata,weights)
    }
    ans<-.trajectory_profiles(object,newdata,times,bandwidth)
    D<-attr(ans,'design');V<-attr(ans,'vcov');factor<-attr(ans,'covariance_factor')
  }
  if(!is.data.frame(ans)||!all(c('profile','time','fit')%in%names(ans)))
    stop('The fitted method returned an invalid prediction grid.',call.=FALSE)
  if(aggregate) {
    groups<-lapply(times,function(t)which(ans$time==t))
    wts<-lapply(groups,function(ii) {
      w<-rep(1,length(ii))
      if(!is.numeric(w)||length(w)!=length(ii)||any(!is.finite(w))||any(w<0)||sum(w)<=0)
        stop('weights must give one finite nonnegative weight per profile, with positive sum.',call.=FALSE)
      w/sum(w)
    })
    values<-vapply(seq_along(groups),function(k)sum(ans$fit[groups[[k]]]*wts[[k]]),numeric(1))
    if(!is.null(D))D<-do.call(rbind,lapply(seq_along(groups),function(k)
      colSums(D[groups[[k]],,drop=FALSE]*wts[[k]])))
    ans<-data.frame(time=times,fit=values)
    attr(ans,'n_profiles')<-profile_counts
  }
  ans<-.trajectory_add_se(ans,D,V,se.fit,level,factor)
  if(!is.null(factor)){attr(ans,'covariance_factor')<-factor;attr(ans,'design')<-D}
  attr(ans,'estimand')<-'Population mean on the modeled outcome scale, with random effects averaged to zero'
  attr(ans,'standardization')<-if(aggregate)'Weighted average over supplied profiles; equal weights by default' else 'Separate covariate profiles'
  attr(ans,'bandwidth')<-if(.trajectory_uses_smoothing(object))bandwidth else NULL
  attr(ans,'design')<-NULL;attr(ans,'vcov')<-NULL
  .report_curve_inference_metadata(ans,object)
}

predict.cimehr_fit <- function(object,newdata=NULL,times=NULL,type='population',
    aggregate=FALSE,weights=NULL,bandwidth=NULL,se.fit=FALSE,level=.95,...) {
  if(!identical(type,'population'))stop('Use type="population" for marginal mean trajectories.',call.=FALSE)
  if(length(list(...)))stop('Unknown prediction arguments.',call.=FALSE)
  population_trajectory(object,newdata,times,bandwidth,aggregate,weights,se.fit,level)
}

plot.cimehr_fit <- function(x,type=c('trajectory','baseline'),newdata=NULL,times=NULL,
    bandwidth=NULL,aggregate=is.null(newdata),weights=NULL,se.fit=FALSE,level=.95,
    xlab='Time',ylab=NULL,bootstrap=FALSE,B=50L,seed=NULL,workers=1L,...) {
  type<-match.arg(type)
  if(length(list(...)))stop('Unknown plot arguments.',call.=FALSE)
  curve<-meanfit(x,newdata=newdata,times=times,
    type=if(type=='baseline')'baseline'else'population',bandwidth=bandwidth,
    aggregate=aggregate,weights=weights,se.fit=se.fit,conf.int=level,
    bootstrap=bootstrap,B=B,seed=seed,workers=workers)
  plot(curve,xlab=xlab,ylab=ylab)
}
