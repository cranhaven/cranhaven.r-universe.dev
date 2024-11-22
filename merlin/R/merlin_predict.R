#' predict.merlin - post-estimation tools for merlin
#'
#' predictions following the fit of a merlin model
#'
#' @author Emma C. Martin, Alessandro Gasparini and Michael J. Crowther
#'
#' @param object merlin model object
#' @param stat specifies which prediction, which can be one of:
#'   \itemize{
#'     \item \code{eta} the expected value of the complex predictor
#'     \item \code{mu} the expected value of the response variable
#'     \item \code{hazard} the hazard function
#'     \item \code{chazard} the cumulative hazard function
#'     \item \code{logchazard} the log cumulative hazard function
#'     \item \code{survival} the survival function
#'     \item \code{cif} the cumulative incidence function
#'     \item \code{rmst} calculates the restricted mean survival time, which is the integral of the
#'     survival function within the interval (0,t], where t is the time at which predictions are made.
#'     If multiple survival models have been specified in your merlin model, then it will assume all
#'     of them are cause-specific competing risks models, and include them in the calculation. If
#'     this is not the case, you can override which models are included by using the \code{causes}
#'     option. \code{rmst = t - totaltimelost}.
#'     \item \code{timelost}  calculates the time lost due to a particular event occurring, within
#'     the interval (0,t].  In a single event survival model, this is the integral of the cif between
#'     (0,t].  If multiple survival models are specified in the merlin model then by default all are
#'     assumed to be cause-specific event time models contributing to the calculation. This can be
#'     overridden using the \code{causes} option.
#'     \item \code{totaltimelost} total time lost due to all competing events, within (0,t]. If multiple
#'     survival models are specified in the \code{merlin} model then by default all are assumed to
#'     be cause-specific event time models contributing to the calculation. This can be overridden
#'     using the \code{causes} option. \code{totaltimelost} is the sum of the \code{timelost} due to
#'     all causes.
#'     \item \code{cifdifference} calculates the difference in \code{cif} predictions between values 
#'     of a covariate specified using the \code{contrast} option.
#'     \item \code{hdifference} calculates the difference in \code{hazard} predictions between values 
#'     of a covariate specified using the \code{contrast} option. 
#'     \item \code{rmstdifference} calculates the difference in \code{rmst} predictions between values 
#'     of a covariate specified using the \code{contrast} option. 
#'     \item \code{mudifference} calculates the difference in \code{mu} predictions between values 
#'     of a covariate specified using the \code{contrast} option. 
#'     \item \code{etadifference} calculates the difference in \code{eta} predictions between values 
#'     of a covariate specified using the \code{contrast} option.  
#'   }
#' @param type the type of prediction, either:
#'   \itemize{
#'     \item \code{fixedonly} prediction calculated based only on the fixed effects; the default.
#'     \item \code{marginal} prediction calculated marginally with respect to the latent variables. the
#'     \code{stat} is calculated by integrating the prediction function with respect to all the latent
#'     variables over their entire support.
#'   }
#' @param predmodel specifies which model to obtain predictions from; default is \code{predmodel=1}
#' @param causes is for use when calculating predictions from a competing risks \code{merlin} model.
#' By default, \code{cif}, \code{rmst}, \code{timelost} and \code{totaltimelost} assume that all
#' survival models included in the merlin model are cause-specific hazard models contributing to
#' the calculation. If this is not the case, then you can specify which models (indexed using
#' the order they appear in your merlin model by using the \code{causes} option, e.g.
#' \code{causes=c(1,2)}.
#' @param at specify covariate values for prediction. Fixed values of covariates should be specified
#' in a list e.g. at = c("trt" = 1, "age" = 50).
#' @param contrast specifies the values of a covariate to be used when comparing statistics, 
#' such as when using the \code{cifdifference} option to compare cumulative incidence functions, 
#' e.g. \code{contrast = c("trt" = 0, "trt" = 1)}.
#' @param ... other options
#'
#' @seealso \code{\link{merlin}}
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed
#' effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development
#' in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects
#' regression of linear, non-linear and user-defined models.
#'
#' @examples
#' library(merlin)
#' data(pbc.merlin, package = "merlin")
#'
#' # Linear fixed-effects model
#' mod <-merlin(model = list(logb ~ year),
#'              family = "gaussian",
#'              data = pbc.merlin)
#' predict(mod,stat="eta",type="fixedonly")
#'

#' @export
predict.merlin<- function(object,
                          stat="eta",
                          type="fixedonly",
                          predmodel=1,
                          causes=NULL,
                          at=NULL,
                          contrast=NULL,
                          ...)
{
    if (class(object)!="merlin") stop("model object must be of class merlin")
    
    modelfit <- object # necessary if predict calls itself (for difference options)
  
    if (type!="fixedonly" & type!="marginal")
      stop("Invalid predictions type",call. = FALSE)
  
    b           <- object$par
    predtype    <- c(stat,type)
    # stats::update will update and (by default) re-fit a model
    pred        <- stats::update(object=object,
                                 predict=TRUE,
                                 from=b,
                                 predtype=predtype,
                                 predmodel=predmodel,
                                 causes=causes,
                                 at=at,
                                 contrast=contrast,
                                 modelfit=modelfit)
    class(pred) <- NULL
    pred
}

merlin_predict<- function(gml,predtype,at=NULL,contrast=NULL,modelfit=NULL) {
  
    stat <- predtype[1]
    type <- predtype[2]

    f = gml$family[gml$modelind]
    issurv = merlin_issurv(f)
    merlin_predict_error_check(gml,stat,issurv,f,at,contrast)
    
    #if at option specified change variable in data set
    if (is.null(at) == F) {
      #preserve version of variable in data, then replace column in gml$data
      saved_at <- matrix(nrow = length(gml$data[,1]), ncol = length(at))
      #loop through the vector of at options 
      for (a in 1:length(at)) {
        saved_at[,a] <- gml$data[,names(at[a])]
        gml$data[,names(at[a])] <- as.numeric(at[a])
      }
    }
    
    #fill up timevar for survival models with response
    if (issurv) gml$timevar[gml$modelind] <- gml$y[[gml$modelind]][1]

    #fill up VCV and nodes
    gml  <- merlin_xb(gml)
    
    if (stat != "cifdifference" & stat != "hdifference" & stat != "rmstdifference" & stat != "mudifference" & stat != "etadifference") {
      pf   <- merlin_p_getpf(gml,stat)
      res  <- merlin_predict_core(gml,pf,type)
    } else {
      if (stat == "hdifference")     diff = "hazard " 
      if (stat == "cifdifference")   diff = "cif" 
      if (stat == "rmstdifference")  diff = "rmst "
      if (stat == "mudifference")    diff = "mu"
      if (stat == "etadifference")   diff = "eta"
      
     res <-   predict.merlin(modelfit, 
                     stat=diff,
                     type=type,
                     predmodel=gml$modelind,
                     causes=gml$causes,
                     at=contrast[1]) -
              predict.merlin(modelfit, 
                     stat=diff,
                     type=type,
                     predmodel=gml$modelind,
                     causes=gml$causes,
                     at=contrast[2])
    }
    
    # restore data
    if (is.null(at) == F) {
      for (a in 1:length(at)) {
        gml$data[,names(at[a])] <- saved_at[,a] 
      }
    }
    return(res)
}

merlin_predict_error_check <- function(gml,stat,issurv,f,at,contrast)
{

    if (gml$modelind > gml$Nmodels) {
        stop("predmodel incorrect",call. = FALSE)
    }

    if (!issurv) {
        if (stat=="hazard" | stat=="survival" | stat=="chazard" | stat=="logchazard" | stat=="cif" | stat=="rmst" | stat=="timelost") {
            stop("stat not allowed with specified merlin family",call. = FALSE)
        }
    }

    if (stat=="mu") {
        if (f=="gompertz" | f=="lquantile" | f=="rcs" | f=="rp" | f=="user" | f=="ordinal" | f=="loghazard" | f=="user") {
            stop("mu not supported with specified merlin family",call. = FALSE)
        }
    }
  
    if (is.null(at) == F) {
      for (a in 1:length(at)) {
        if ((names(at[a]) %in% colnames(gml$data)) == F ) stop("variable specified in at option not available in dataset")
      }
    }
  
    if (stat== "cifdifference" | stat == "hdifference" | stat == "rmstdifference" | stat == "mudifference" | stat == "etadifference") {
      if (is.null(contrast) == T) stop("contrast option must be used with difference predictions")
      if (length(contrast) != 2) stop("two values must be specified in contrast for difference to be calculated")
      if (names(contrast[1]) != names(contrast[2])) stop("the contrast must be between two levels of the same covariate")
    }

}

merlin_p_getpf <- function(gml,stat)
{
    if 		  (stat=="eta")				      return("merlin_p_eta(")
    else if (stat=="mu") 				      return("merlin_p_mu(")
    else if (stat=="cif") 				    return("merlin_p_cif(")
    else if (stat=="rmst")				    return("merlin_p_rmst(")
    else if (stat=="timelost")			  return("merlin_p_timelost(")
    else if (stat=="totaltimelost")		return("merlin_p_totaltimelost(")
    else {
        f = gml$family[gml$modelind]
        if 		(f=="exponential") {
            if 		  (stat=="hazard")		 return("merlin_p_exp_h(")
            else if (stat=="survival")	 return("merlin_p_exp_s(")
            else if (stat=="chazard")		 return("merlin_p_exp_ch(")
            else if (stat=="logchazard") return("merlin_p_exp_logch(")
        }
        else if (f=="weibull") {
            if 		  (stat=="hazard")		  return("merlin_p_weibull_h(")
            else if (stat=="survival")		return("merlin_p_weibull_s(")
            else if (stat=="chazard")		  return("merlin_p_weibull_ch(")
            else if (stat=="logchazard")	return("merlin_p_weibull_logch(")
        }
        else if (f=="gompertz") {
            if 		  (stat=="hazard")		  return("merlin_p_gompertz_h(")
            else if (stat=="survival")		return("merlin_p_gompertz_s(")
            else if (stat=="chazard")		  return("merlin_p_gompertz_ch(")
            else if (stat=="logchazard")	return("merlin_p_gompertz_logch(")
        }
        else if (f=="loghazard") {
            if 		  (stat=="hazard")		  return("merlin_p_logh_h(")
            else if (stat=="survival")		return("merlin_p_logh_s(")
            else if (stat=="chazard")		  return("merlin_p_logh_ch(")
            else if (stat=="logchazard")	return("merlin_p_logh_logch(")
        }
        else if (f=="rp") {
            if 		  (stat=="hazard")		  return("merlin_p_rp_h(")
            else if (stat=="survival")		return("merlin_p_rp_s(")
            else if (stat=="chazard")		  return("merlin_p_rp_ch(")
            else if (stat=="logchazard")	return("merlin_p_rp_logch(")
        }
        # else if (f=="bernoulli") {
        #     if 		(stat=="mu") 		return("merlin_p_bernoulli_mu(")
        # }
        # else if (f=="beta") {
        #     if 		(stat=="mu") 		return("merlin_p_beta_mu(")
        # }
        # else if (f=="gamma") {
        #     if 		(stat=="mu") 		return("merlin_p_gamma_mu(")
        # }
        # else if (f=="gaussian") {
        #     if 		(stat=="mu") 		return("merlin_p_gaussian_mu(")
        # }
        # else if (f=="negbinomial") {
        #     if 		(stat=="mu") 		return("merlin_p_negbinomial_mu(")
        # }
    }
}

merlin_predict_core <- function(gml,pf,type)
{
    gml$fixedonly = FALSE
    if (type=="fixedonly") 	return(merlin_predict_fixedonly(gml,pf))
    else        						return(merlin_predict_marginal(1,gml,pf))
}

merlin_predict_fixedonly <- function(gml,pf)
{
    gml$fixedonly = TRUE
    return(eval(parse(text=paste0(pf,"gml)"))))
}


merlin_predict_marginal <- function(index,gml,pf)
{
  index2 <- index+1

  if (index<gml$Nlevels) {
      result <- matrix(0,gml$Nobs[[index]],gml$Ndim[index])
      for (q in 1:gml$Ndim[index]) {
          gml$qind[index2] <- q
          result[,q]       <- merlin_predict_marginal(index2,gml,pf)
      }
  }
  else result = eval(parse(text=paste0(pf,"gml)")))

  if (gml$intmethod[index]=="ghermite") return(result %*% gml$w[[index]])   	         #GHQ
  else 					                        return(base::rowSums(result)/gml$Ndim[index])	 #MCI
}

merlin_p_eta <- function(gml,t=NULL)
{
    t <- merlin_util_timevar(gml)
    return(merlin_util_xzb(gml,t))
}

merlin_p_mu <- function(gml,t=NULL)
{
  t <- merlin_util_timevar(gml)
  return(merlin_util_ev(gml,t))
}

merlin_p_cif <- function(gml,t=NULL)
{
    if (length(t)==0) t = merlin_util_timevar(gml)

    refmod 	= gml$modelind

    # get functions to cause-specific h and ch functions -> gml.model indexes this
    hf 		= merlin_p_getpf(gml,stat="hazard")
    chfs 	= merlin_p_getpf(gml,stat="chazard")

    modind 	= gml$model

    if (length(gml$causes)==0) {

        # assume all survival models contribute
        for (mod in 1:gml$Nmodels) {
            gml$model = mod
            f = gml$family[mod]
            issurv = merlin_issurv(f)
            if (issurv & mod!=refmod) {
                chfs 	  = cbind(chfs,merlin_p_getpf(gml, "chazard"))
                modind 	= cbind(modind,gml$model)
            }
        }
        Nsurvmodels = length(modind)
    }
    else {

        modind = as.matrix(gml$causes)
        Nsurvmodels = nrow(modind)
        for (mod in 1:Nsurvmodels) {
          gml$modelind = mod
          f = gml$family[mod]
          issurv = merlin_issurv(f)
          if (!issurv) stop("a model in cause() is not a survival model")
          if (mod!=refmod) {
            chfs 	= cbind(chfs,merlin_p_getpf(gml, "chazard"))
            modind 	= cbind(modind,gml$modelind)
          }
        }
    }

    gml$model 	= refmod
    N   = gml$Nobs[gml$modelind]
    gq 	= merlin_get_GK()
    qp	= sweep((t %*% (gq[[1]]/2)),1,(t/2),"+")
    if (gml$Nlevels & !gml$fixedonly) {
          result = matrix(0,nrow=N,ncol=gml$Ndim[gml$Nlevels])
    }
    else  result = matrix(0,nrow=N,ncol=1)
    for (q in 1:15) {
        gml$modelind  = refmod
        haz			      = eval(parse(text=paste0(hf,"gml,qp[,q])")))
        if (gml$Nlevels & !gml$fixedonly) {
          ochres = matrix(0,nrow=N,ncol=gml$Ndim[gml$Nlevels])
        }
        else  ochres = matrix(0,nrow=N,ncol=1)

        for (k in 1:Nsurvmodels) {
            gml$modelind = modind[k]
            ochres = ochres + eval(parse(text=paste0(chfs[k],"gml,qp[,q])")))
        }

        result = result + sweep(haz * exp(-ochres),1,gq[[2]][q] * t/2,"*")
    }
    gml$modeind = refmod
    return(result)
}


merlin_p_timelost <- function(gml,t=NULL)
{
    if (length(t)==0) t = merlin_util_timevar(gml)
    N   = gml$Nobs[gml$modelind]
    gq 	= merlin_get_GK()
    qp	= sweep((t %*% (gq[[1]]/2)),1,(t/2),"+")
    if (gml$Nlevels & !gml$fixedonly) {
      result = matrix(0,nrow=N,ncol=gml$Ndim[gml$Nlevels])
    }
    else  result = matrix(0,nrow=N,ncol=1)
    #integrate cif
    for (q in 1:15) {
        result = result + sweep(merlin_p_cif(gml,qp[,q]),1,gq[[2]][q] * t/2,"*")
    }
    return(result)
}

merlin_p_totaltimelost <- function(gml,t=NULL)
{
    if (length(t)==0) t = merlin_util_timevar(gml)

    refmod 	= gml$modelind
    modind 	= gml$model

    if (length(gml$causes)==0) {
        # assume all survival models contribute
        for (mod in 1:gml$Nmodels) {
            gml$model = mod
            f = gml$family[mod]
            issurv = merlin_issurv(f)
            if (issurv & mod!=refmod) {
                modind 	= cbind(modind,gml$model)
            }
        }
        Nsurvmodels = length(modind)
    }
    else {
        modind = as.matrix(gml$causes)
        Nsurvmodels = nrow(modind)
        for (mod in 1:Nsurvmodels) {
            gml$modelind = mod
            f = gml$family[mod]
            issurv = merlin_issurv(f)
            if (!issurv) stop("a model in cause() is not a survival model")
            if (mod!=refmod) {
              modind 	= cbind(modind,gml$modelind)
            }
        }
    }

    gml$model 	= refmod
    N   = gml$Nobs[gml$modelind]
    gq 	= merlin_get_GK()
    qp	= sweep((t %*% (gq[[1]]/2)),1,(t/2),"+")
    if (gml$Nlevels & !gml$fixedonly) {
        result = matrix(0,nrow=N,ncol=gml$Ndim[gml$Nlevels])
    }
    else  result = matrix(0,nrow=N,ncol=1)

    #total time lost
    for (q in 1:Nsurvmodels) {
        gml$model = modind[q]
        result 		= result + merlin_p_timelost(gml,t)
    }
    return(result)
}

merlin_p_rmst <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(t-merlin_p_totaltimelost(gml,t))
}

merlin_issurv <- function(f)
{
  return(f=="exponential" | f=="weibull" | f=="gompertz" | f=="rp" | f=="loghazard")
}
