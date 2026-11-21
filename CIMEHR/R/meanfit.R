# Column names used inside ggplot2::aes() in plot.cimehr_meanfit(); declared
# so R CMD check does not report 'no visible binding for global variable'.
utils::globalVariables(c("time", "fit", "profile", "method", "lower", "upper"))

meanfit <- function(object,...) UseMethod('meanfit')

meanfit.cimehr_fit <- function(object,newdata=NULL,times=NULL,
    type=c('population','baseline'),aggregate=is.null(newdata),weights=NULL,
    bandwidth=NULL,se.fit=FALSE,conf.int=.95,
    bootstrap=FALSE,B=50L,seed=NULL,workers=1L,...) {
  type<-match.arg(type)
  if(length(list(...)))stop('Unknown meanfit arguments.',call.=FALSE)
  if(!is.logical(bootstrap)||length(bootstrap)!=1L||is.na(bootstrap))
    stop('bootstrap must be TRUE or FALSE.',call.=FALSE)
  if(!is.logical(se.fit)||length(se.fit)!=1L||is.na(se.fit))
    stop('se.fit must be TRUE or FALSE.',call.=FALSE)
  if(type=='baseline') {
    if(!is.null(newdata)||!is.null(weights))stop('Baseline mean curves do not use newdata or profile weights.',call.=FALSE)
    d<-baseline_mean(object,times,bandwidth,se.fit&&!bootstrap,conf.int)
  } else d<-population_trajectory(object,newdata,times,bandwidth,aggregate,weights,se.fit&&!bootstrap,conf.int)
  boot<-NULL
  if(bootstrap) {
    z<-.curve_bootstrap(object,d,type,newdata,sort(unique(d$time)),aggregate,weights,
      attr(d,'bandwidth') %||% bandwidth,B,seed,workers,conf.int)
    d<-z$data;boot<-z$bootstrap
  }
  d<-.report_curve_inference_metadata(d,object)
  ans<-structure(list(data=d,time=sort(unique(d$time)),method=object$method %||% object$method_key,
    method_key=object$method_key,type=type,aggregate=if(type=='baseline')TRUE else aggregate,
    newdata=if(is.data.frame(newdata))newdata else NULL,
    standardization=if(type=='baseline')NULL else if(is.null(newdata)&&aggregate)
      'Empirical reference population retained by the fitted method' else if(aggregate)
      'Weighted mean over the supplied covariate profiles' else 'Separate supplied covariate profiles',
    bandwidth=attr(d,'bandwidth'),conf.int=if(se.fit||bootstrap)conf.int else NULL,
    bootstrap=boot,
    outcome_scale='Modeled outcome scale',call=match.call()),class='cimehr_meanfit')
  if(!is.null(attr(d,'identification_status'))) {
    ans$identification_status<-attr(d,'identification_status')
    ans$identification_warning<-attr(d,'identification_warning')
    ans$identification_diagnostics<-attr(d,'identification_diagnostics')
    ans$inference_status<-attr(d,'inference_status')
    ans$inference_valid<-attr(d,'inference_valid')
    ans$inference_reason<-attr(d,'inference_reason')
    ans$inference_scope<-attr(d,'inference_scope')
    ans$parent_identification_assessment<-attr(d,'parent_identification_assessment')
    ans$ci_available<-attr(d,'ci_available')
  }
  ans
}

as.data.frame.cimehr_meanfit <- function(x,row.names=NULL,optional=FALSE,...) {
  ans<-x$data
  if(!is.null(row.names))rownames(ans)<-row.names
  ans
}

summary.cimehr_meanfit <- function(object,...) {
  if(length(list(...)))stop('Specify prediction times in meanfit(..., times=...).',call.=FALSE)
  as.data.frame(object)
}

print.cimehr_meanfit <- function(x,...) {
  cat(x$method,': ',if(x$type=='baseline')'baseline mean' else 'population mean','\n',sep='')
  profiles<-if('profile'%in%names(x$data))length(unique(x$data$profile))else 1L
  cat(length(x$time),'time points;',profiles,'curve',if(profiles!=1)'s' else '',
      '; ',x$outcome_scale,'\n',sep=' ')
  if(!is.null(x$standardization))cat(x$standardization,'\n')
  if(!is.null(x$bandwidth))cat('Local-linear bandwidth:',x$bandwidth,'\n')
  if(!is.null(x$bootstrap))cat('Participant bootstrap:',x$bootstrap$successful_fits,'of',
    x$bootstrap$B,'fits succeeded;',round(100*x$conf.int,2),'% pointwise intervals\n')
  invisible(x)
}

plot.cimehr_meanfit <- function(x,conf.int=!is.null(x$conf.int),
    xlab='Time',ylab=NULL,...) {
  if(length(list(...)))stop('Unknown plot arguments.',call.=FALSE)
  if(!is.logical(conf.int)||length(conf.int)!=1L||is.na(conf.int))
    stop('conf.int must be TRUE or FALSE.',call.=FALSE)
  if(!requireNamespace('ggplot2',quietly=TRUE))stop('Install ggplot2 to draw fitted mean curves.',call.=FALSE)
  # Names evaluated inside ggplot2's data mask.
  time<-fit<-profile<-method<-lower<-upper<-NULL
  d<-as.data.frame(x)
  if(conf.int&&!all(c('lower','upper')%in%names(d)))
    stop('This curve object has no confidence intervals.',call.=FALSE)
  if(!'profile'%in%names(d))d$profile<-1L
  d$profile<-factor(d$profile);d$method<-x$method
  color<-if(grepl('liang',x$method_key)) {
    if(x$method_key%in%c('mi_liang','li_liang'))'#EF6C00' else '#1565C0'
  }else '#6A3D9A'
  p<-ggplot2::ggplot(d,ggplot2::aes(x=time,y=fit,group=profile,color=method))
  if(conf.int)p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=lower,ymax=upper,fill=method),alpha=.15,colour=NA)
  if(nlevels(d$profile)>1L)p<-p+ggplot2::geom_line(ggplot2::aes(linetype=profile),linewidth=.7)
  else p<-p+ggplot2::geom_line(linewidth=.7)
  if(nlevels(d$profile)>1L)p<-p+ggplot2::scale_linetype_manual(values=
    rep(c('solid','longdash','dotted','dotdash','twodash','dashed'),length.out=nlevels(d$profile)))
  p<-p+ggplot2::scale_color_manual(values=color)+
    ggplot2::labs(x=xlab,y=ylab %||% if(x$type=='baseline')'Baseline mean outcome' else 'Population-average outcome',
      color='Method')+
    ggplot2::theme_bw()+ggplot2::theme(legend.position='right',plot.title=ggplot2::element_blank())
  if(conf.int)p<-p+ggplot2::scale_fill_manual(values=color)+ggplot2::labs(fill='Method')
  if(nlevels(d$profile)>1L)p<-p+ggplot2::labs(linetype='Profile')
  print(p)
  invisible(p)
}
