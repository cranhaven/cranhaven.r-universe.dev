## #' Calculate ranks from right-censored survival data
## #'
## #' Faster version of coin::logrank_trafo, but does not support weights
## #' and does not handle ties
## #'
## #' @param time vector of survival times
## #' @param event vector of event indicators (0=censored, 1=event)
## #' @return vector of ranks
## logrank_trafo <- function(time, event) {
##     n <- length(time)

##     r <- rank(time, ties.method="min")
##     o <- order(time, event, na.last=TRUE, decreasing=FALSE)
##     or <- r[o]
##     uor <- unique.default(or)

##     ## number at risk, number of ties and events at the ordered unique times
##     n_risk <- n - uor + 1L
##     n_ties <- -diff.default(c(n_risk, 0L))
##     eo <- event[o]
##     n_event <- vapply(uor, function(i) sum(eo[i == or]), NA_real_)

##     ## index: expands ties and returns in original order
##     idx <- rep.int(seq_along(n_ties), n_ties)[r] # => uor[idx] == r

##     cumsum(n_event / n_risk)[idx] - event
## }

#' sampleFromKM
#'
#' Sample from distribution estimated by Kaplan-Meier estimator. Imputed values > tmax are right-censored.
#'
#' @param n sample size
#' @param fit Kaplan-Meier fit as returned by survfit
#' @param start if 0 sample from L(T), else sample from L(T, T > start)
#' @param tmax largest observation in pooled sample
#' @param dv 1 if imputing events, 0 if imputing censoring times
#' @return Random sample of survival times
sampleFromKM <- function(n, fit, start=0, tmax=NULL, dv=1) {
    if(is.null(tmax)) tmax <- fit$time[length(fit$time)]
    vapply(runif(n, start, 1), function(v) {
        if(v > max(1-fit$surv)) c(tmax, 0)
        else c(min(fit$time[fit$surv <= 1-v]), dv)
    }, c(NA_real_, NA_integer_))
}

#' sampleFromCondKM
#'
#' Sample from conditional distribution estimated by Kaplan-Meier estimator. Imputed values > tmax are right-censored.
#'
#' @param U vector of observed times
#' @param fit Kaplan-Meier fit as returned by survfit
#' @param tmax largest observation of the pooled sample
#' @param dv 1 if imputing events, 0 if imputing censoring times
#' @param f interpolated Kaplan-Meier estimate
#' @return Random sample of survival times drawn from conditional distribution of T given T > U
sampleFromCondKM <- function(U, fit, tmax=NULL, dv=1, f=NULL) {
    n <- length(U)
    if(is.null(f)) f <- approxfun(fit$time, fit$surv, method="constant", yleft=1, rule=2, f=0)
    sampleFromKM(n, fit, 1-f(U), tmax, dv)
}


#' Parse formula of survival model
#'
#' @param formula formula object
#' @param data data.frame (optional)
#' @return data.frame containing the parsed variables
parseFormula <- function(formula, data=parent.frame()) {
    Terms <- terms(formula, specials="strata")
    mf <- model.frame(Terms, data)
    
    ## extract Surv object
    surv <- model.response(mf)
    if(!is.Surv(surv)) stop("Model response must be 'Surv' object")
    
    n <- nrow(surv)
    
    if(ncol(surv) == 2) {
        Start <- rep.int(0, n)
        Stop <- surv[,1]
        status <- surv[,2]
    } else {
        Start <- surv[,1]
        Stop <- surv[,2]
        status <- surv[,3]
    }
    
    ## extract stratum indicator
    st <- attr(Terms, "specials")$strata
    if(is.null(st)) {
        W <- NULL
        st.labels <- NULL
    } else {
        st.labels <- dimnames(attr(Terms, "factors"))[[1]][st]
        W <- as.integer(mf[[st.labels]])
    }
    
    sdi <- setdiff(labels(Terms), st.labels)
    if(length(sdi) > 0) {
        trt <- as.factor(mf[[sdi]])
        if(nlevels(trt) != 2) stop("This package only supports two-sample methods!")
        ## ensure trt contains only 0/1
        trt <- as.integer(trt) - 1
    } else stop("No treatment groups specified!")
  
    if(!is.null(W)) data.frame(time=Stop, status=status, trt=trt, strata=W)
    else data.frame(time=Stop, status=status, trt=trt, strata=rep.int(1, length(trt)))
}
