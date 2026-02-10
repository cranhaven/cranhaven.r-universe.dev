#' Wrapper function for the implementaion of lagged WQS.
#'
#' @param data Data frame containing observations in long format.
#' @param timevar Enquoted variable name identifying the repeated measure / time variable
#' @param wqs_parms A list containing parameters to be passed to the WQS algorithm. See gWQS package for details.
#' @param outcome An enquoted variable name identifying the outcome measure
#' @param ID An enquoted variable name identifying the subject identifier
#' @param rDLM_parms (optional). A list containing parameters to be passed to the GAM algorithm. See gamm4 package for details. Parameters wqs, time, by, and id (see above) are created by the lwqs function and passed to the gamm4 function automatically.

#'@return The \code{lwqs} function returns a list containing final model output and time-specific model parameters.
#'
#'\item{parameters}{This list contains several objects summarizing different stages of the lagged ensemble model. The first
#'object, \code{res}, contains output from the gWQS algorithm applied to  each discreet repeated measure in the
#'overall model; see package gWQS for details. The second output, \code{wqstime}, provides the mixture index,
#'identified as "wqs", estimated for each subject at each discrete time point. The third item, \code{weightstime}, provides
#'the weights estimated for each predictor at each discrete time point.}
#'\item{plot}{This list contains two plots (as grobs) which summarize output of the lwqs algorithm.}
#'
#'@examples
#' \donttest{ # identify predictor variables used in mixture
#' mixvars=names(lwqs_data)[5:9]
#'
# # run model.
#' model=lwqs(data=lwqs_data,
#'            timevar="time",
#'            wqs_parms=list(formula=out ~ wqs,
#'               data = lwqs_data,
#'               mix_name=mixvars,
#'               b1_constr = TRUE,
#'               b1_pos=TRUE,
#'               b = 5,
#'               q = 5,
#'               validation = 0,
#'               family = "gaussian",
#'               seed = 1),
#'               outcome="out",
#'               ID="ID")}
#'
#' @import ggplot2
#' @import gWQS
#' @import plyr
#' @import gamm4
#' @import gridExtra
#' @import data.table
#'
#' @export lwqs

lwqs<-function(data,
               timevar,
               wqs_parms,
               outcome,
               ID,
               rDLM_parms=list(formula=wqs ~ s(time, by=y, bs="cr"),
                                    random = ~ (1 | id))){

wqs_parameters<-function(x){
    parms<-wqs_parms
    parms$solve_dir_issue="average"
    parms$data<-x
    model<-do.call(gwqs, parms)
    model$wqs=data.frame(cbind(model$wqs, subset(x, select=ID), subset(x, select=outcome)))
    names(model$wqs)<-c("wqs", "ID", "y")
    return(model)
  }

  res=plyr::dlply(data, timevar, wqs_parameters)
  wqstime<-plyr::ldply(res, function(x){rbind(x$wqs)})
  weightstime<-plyr::ldply(res, function(x){rbind(x$final_weights)})
  parameters=list(wqs=res, beta_t=wqstime, weight_t=weightstime)
  results=list(res=res, wqstime=wqstime, weightstime=weightstime)

  Beta <- lower <- upper <- time <- mean_weight <- mix_name <- NULL

  rDLM_parms$data<-wqstime
  rDLM_parms$data$id<-rDLM_parms$data$ID
  rDLM_parms$data$time<-rDLM_parms$data[,timevar]
  fit=do.call(gamm4, rDLM_parms)
  fit_se=plot(fit$gam)
  fplot=data.frame(cbind(unlist(fit_se[[1]][1]), unlist(fit_se[[1]][10]), unlist(fit_se[[1]][3])))
  names(fplot)<-c("time", "Beta", "SE")
  fplot$upper=fplot$Beta+(1.96*fplot$SE)
  fplot$lower=fplot$Beta-(1.96*fplot$SE)

  bplot<-ggplot(data=fplot, aes(x=time, y=Beta)) + geom_line(size=1.5) +
    theme_bw() + geom_hline(yintercept=0, size=0.6, lty=2) +
    ylim(-1,1) + xlab("Time") + ylab("Time Varying Effect of Mixture")

  bplot<-bplot+geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.35)

  weightstime$time=weightstime[,timevar]
  wplot=ggplot(weightstime, aes(x=time, y=mean_weight, group=mix_name, colour=mix_name)) +
    geom_smooth(se=F, method='loess', size=1.5) + theme_bw() + xlab("Time")
  plot=grid.arrange(bplot, wplot, nrow=1)
  results_fin=list(parameters=results, plot=plot)

  return(results_fin)

}

#'Function to extract time-varying mixture (wqs) index from lWQS object
#'@param lobj An object returned from lWQS function
#'@return Data frame containing the time index, wqs index estimated at each repeated measure, subject ID, and the outcome variable.
#'@examples
#'\donttest{
#' # identify predictor variables used in mixture
#' mixvars=names(lwqs_data)[5:9]
#'
#' # run model. Note for example run-time only 1 bootstrap (b=1) is used. Set b to be >50
#' model=lwqs(data=lwqs_data,
#'            timevar="time",
#'            wqs_parms=list(formula=out ~ wqs,
#'               data = lwqs_data,
#'               mix_name=mixvars,
#'               b1_constr = TRUE,
#'               b1_pos=FALSE,
#'               b = 5,
#'               q = 5,
#'               validation = 0,
#'               family = "gaussian",
#'               seed = 1),
#'               outcome="out",
#'               ID="ID")
#'
#' # use extract_mixture to access time-varying wqs index
#' mixtime=extract_mixture(model)}

#'@export extract_mixture
extract_mixture<-function(lobj){
  wqstime=data.frame(lobj$parameters$wqstime)
  return(wqstime)
}

#'Function to extract time-varying weights from lWQS object
#'@param lobj An object returned from lWQS function
#'@return A (long-form) data frame containing the time index and corresponding variable weights estimated in an lWQS
#'@examples
#'\donttest{
#' # identify predictor variables used in mixture
#' mixvars=names(lwqs_data)[5:9]
#'
#' # run model
#' model=lwqs(data=lwqs_data,
#'            timevar="time",
#'            wqs_parms=list(formula=out ~ wqs,
#'               data = lwqs_data,
#'               mix_name=mixvars,
#'               b1_constr = TRUE,
#'               b1_pos=TRUE,
#'               b = 5,
#'               q = 5,
#'               validation = 0,
#'               family = "gaussian",
#'               seed = 1),
#'               outcome="out",
#'               ID="ID")
#'
#' # use extract_weights to access time-varying predictor weights
#' timeweights=extract_weights(model)}

#'@export extract_weights
extract_weights<-function(lobj){
 weightstime=data.frame(lobj$parameters$weightstime)
 return(weightstime)
}


#'Simulated dataset for accompanying vignette
#'@name lwqs_data
#'@docType data
#'@return A data frame containing simulated data to explore the \code{lwqs} algorithm. Variables included are as follows:
#'\item{ID}{Variable identifying each simulated subject. Data reflect 30 successive measures per subject.}
#'\item{Sex}{A simulated binary covariate, either 1 or 0.}
#'\item{time}{Variable identifying the successive timing of each repeated measure}
#'\item{out}{Simulated outcome on standardized scale}
#'\item{pred1}{First simulated time-varying predictor. This has a large positive association with "out" from times 11-20.}
#'\item{pred2}{Second simulated time-varying predictor. This has a moderate positive association with "out" from times 11-20.}
#'\item{pred3}{Third simulated time-varying predictor. This has a moderate negative association with "out" from times 1-10.}
#'\item{pred4}{Fourth simulated time-varying predictor. This has a strong negative association with "out" from times 1-10.}
#'\item{pred5}{Fifth simulated time-varying predictor. This has no significant association with "out".}
#'@usage data(lwqs_data)
NULL


