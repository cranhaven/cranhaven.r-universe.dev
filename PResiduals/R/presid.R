###glm()
#' @export
#' @importFrom stats ppois dpois residuals pnorm naresid
presid.glm <- function(object, emp=FALSE, ...) {
  ##gaussian.emp = (2 * rank(residuals(object))-1-length(y))/length(y)  #need to figure out how to incorporate this in this function
  res <- switch(object$family$family ,
                poisson = 2 * ppois(object$y, object$fitted.values) - dpois(object$y, object$fitted.values) - 1,
                binomial = object$y - object$fitted.values,
                gaussian = if(emp) (2 * rank(residuals(object)) - 1 - length(object$y)) / length(object$y) else 2 * pnorm((object$y - object$fitted.values)/sqrt(summary(object)$dispersion)) - 1,
                stop("Unhandled family", object$family$family))
  res <- naresid(object$na.action, res)
  return(res)
}


###Glm()
#' @export
#' @importFrom stats ppois dpois pnorm residuals
presid.Glm <- function(object, emp=TRUE, ...) {
  res <- switch(object$family$family ,
                poisson = 2 * ppois(object$y, object$fitted.values) - dpois(object$y, object$fitted.values) - 1,
                binomial = object$y - object$fitted.values,
                gaussian = if(emp) (2 * rank(residuals(object)) - 1 - length(object$y)) / length(object$y) else 2 * pnorm((object$y - object$fitted.values)/(sum(object$residuals)/sqrt(object$df.residual))) - 1,
                stop("Unhandled family", object$family$family))

  res <- naresid(object$na.action, res)
  return(res)
}


###lm()
#' @export
#' @importFrom stats model.response residuals pnorm
presid.lm <- function(object, emp=FALSE, ...) {
  y <- model.response(object$model)
  if(emp) {
    res <- (2 * rank(residuals(object, type="working")) - 1 - length(y)) / length(y)
  } else {
    res <- 2 * pnorm((y - object$fitted.values)/summary(object)$sigma) - 1
  }
  res <- naresid(object$na.action, res)
  return(res)
}



###ols()
#' @export
#' @importFrom stats residuals
presid.ols <- function(object, emp=FALSE, ...) {
  if(is.null(object$y))
    stop("Need Y=TRUE in fitting function call")
  y <- object$y
  if(emp) {
    res <- (2 * rank(residuals(object, type="ordinary")) - 1 - length(y)) / length(y)
  } else {
    sigma <- sqrt(sum(object$residuals^2)/object$df.residual)
    res <- 2 * pnorm((y - object$fitted.values)/sigma) - 1
  }
  res <- naresid(object$na.action, res)
  return(res)
}


###negative binomial
#' @export
#' @importFrom stats pnbinom
presid.negbin <- function(object, ...) {
  res <- pnbinom(object$y-1, mu=object$fitted.values, size=object$theta ) + pnbinom(object$y, mu=object$fitted.values, size=object$theta) -1
  res <- naresid(object$na.action, res)
  return(res)
}


###polr
#' @export
#' @importFrom stats plogis pnorm pcauchy model.response
presid.polr <- function(object, ...) {
  pfun <- switch(object$method,
                 logistic = plogis,
                 probit = pnorm,
                 loglog = pgumbel,
                 cloglog = pGumbel,
                 cauchit = pcauchy)
  n <- length(object$lp)
  q <- length(object$zeta)
  cumpr <- cbind(0, matrix(pfun(matrix(object$zeta, n, q, byrow = TRUE) - object$lp),, q), 1)
  y <- as.integer(model.response(object$model))
  lo <- cumpr[cbind(seq_len(n), y)]
  hi <- 1 - cumpr[cbind(seq_len(n), y+1L)]
  res <- lo - hi
  res <- naresid(object$na.action, res)
  return(res)
}

###coxph()
#' @export
#' @importFrom stats residuals
presid.coxph <- function(object, type=c("standard", "Cox-Snell-like"), ...) {

  time <- naresid(object$na.action, object$y[,1])
  delta <- naresid(object$na.action, object$y[,2])

  resid <- residuals(object, type="martingale")
  if(type[1]=="standard"){
    result <- 1 - exp(resid - delta) - delta*exp(resid - delta)
    result <- 1 - exp(resid - delta) - delta*exp(resid - delta)
  }else if(type[1]=="Cox-Snell-like"){
    csresids<-delta - resid
    result<-2*(1-exp(-csresids))-1
  }else stop("type needs to be either standard or Cox-Snell-like")

  return(result)
}

###cph()
#' @export
#' @importFrom stats residuals

presid.cph <- function(object,type=c("standard", "Cox-Snell-like"), ...) {
  if(is.null(object$y))
    stop("X=TRUE must be set in fitting call")

  time <- naresid(object$na.action, object$y[,1])
  delta <- naresid(object$na.action, object$y[,2])
  resid <- residuals(object, type="martingale")
  if(type[1]=="standard"){
    result <- 1 - exp(resid - delta) - delta*exp(resid - delta)
  }else if(type[1]=="Cox-Snell-like"){
    csresids<-delta - resid
    result<-2*(1-exp(-csresids))-1
  }else stop("type needs to be either standard or Cox-Snell-like")
  return(result)
}

###survreg()
#' @export
#' @importFrom stats pweibull pexp pnorm plogis plnorm
presid.survreg <- function(object, type=c("standard", "Cox-Snell-like"), ...){
  time <- naresid(object$na.action, object$y[,1])
  delta <- naresid(object$na.action, object$y[,2])

  switch(object$dist,
         weibull = {
           prob <- pweibull(exp(time), shape=1/summary(object)$scale,
                            scale=naresid(object$na.action, exp(object$linear.predictors)),
                            lower.tail=TRUE, log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result

         },

         exponential = {
           prob <- pexp(time, rate=naresid(object$na.action, 1/exp(object$linear.predictors)),
                        lower.tail=TRUE, log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },

         gaussian = {
           prob <- pnorm(time, mean=naresid(object$na.action, object$linear.predictors),
                         sd=summary(object)$scale, lower.tail=TRUE,
                         log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },

         logistic = {
           prob <- plogis(time, location=naresid(object$na.action, object$linear.predictors),
                          scale=summary(object)$scale, lower.tail=TRUE,
                          log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },


         lognormal = {
           prob <- plnorm(time, meanlog=naresid(object$na.action, object$linear.predictors),
                          sdlog=summary(object)$scale, lower.tail=TRUE,
                          log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },
         stop("Unhandled dist", object$dist))

  return(result)
}


###psm()
#' @export
#' @importFrom stats pweibull pexp pnorm plogis plnorm
presid.psm <- function(object,type=c("standard", "Cox-Snell-like"), ...) {
  time <- naresid(object$na.action, object$y[,1])
  delta <- naresid(object$na.action, object$y[,2])

  switch(object$dist,
         weibull = {
           prob <- pweibull(exp(time), shape=1/object$scale,
                            scale=naresid(object$na.action, exp(object$linear.predictors)),
                            lower.tail=TRUE, log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },

         exponential = {
           prob <- pexp(time, rate=naresid(object$na.action, 1/exp(object$linear.predictors)),
                        lower.tail=TRUE, log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },

         gaussian = {
           prob <- pnorm(time, mean=naresid(object$na.action, object$linear.predictors),
                         sd=object$scale, lower.tail=TRUE,
                         log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },

         logistic = {
           prob <- plogis(time, location=naresid(object$na.action, object$linear.predictors),
                          scale=object$scale, lower.tail=TRUE,
                          log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },



         lognormal = {
           prob <- plnorm(time, meanlog=naresid(object$na.action, object$linear.predictors),
                          sdlog=object$scale, lower.tail=TRUE,
                          log.p=FALSE)
           if(type[1]=="standard"){
             result <-  prob + delta*(prob - 1)
           }else if(type[1]=="Cox-Snell-like"){
             result <- 2*prob-1
           }else stop("type needs to be either standard or Cox-Snell-like")
           result
         },
         stop("Unhandled dist", object$dist))

  return(result)
}


#' @export
#' @importFrom stats residuals
presid.lrm <- function(object, ...) {
  res <- residuals(object, type="li.shepherd")
  res <- naresid(object$na.action, res)
  return(res)
}

#' @export
#' @importFrom stats residuals
presid.orm <- function(object, ...) {
    res <- residuals(object, type="li.shepherd")
    res <- naresid(object$na.action, res)
    return(res)
}

#' @export
presid.default <- function(object, ...) {
    stop("Unhandled model type")
}


#' Probability-scale residual
#'
#' \code{presid} calculates the probability-scale residual for various model
#' function objects. Currently supported models include \code{\link{glm}}
#' (Poisson, binomial, and gaussian families), \code{\link{lm}} in the
#' \pkg{stats} library; \code{\link{survreg}} (Weibull, exponential, gaussian,
#' logistic, and lognormal distributions) and \code{\link{coxph}} in the
#' \pkg{survival} library; \code{\link{polr}} and \code{\link{glm.nb}} in
#' the \pkg{MASS} library; and \code{\link{ols}}, \code{\link{cph}},
#' \code{\link{lrm}}, \code{\link{orm}}, \code{\link{psm}}, and \code{\link{Glm}}
#' in the \pkg{rms} library.
#'
#' Probability-scale residual is \eqn{P(Y < y) - P(Y > y)} where \eqn{y} is the observed
#' outcome and \eqn{Y} is a random variable from the fitted distribution.
#'
#' @param object The model object for which the probability-scale residual is calculated
#' @param ... Additional arguements passed to methods
#' @return The probability-scale residual for the model
#' @references Shepherd BE, Li C, Liu Q (2016)
#' Probability-scale residuals for continuous, discrete, and censored data.
#' \emph{The Canadian Jouranl of Statistics}. \bold{44}:463--476.
#' @references Li C and Shepherd BE (2012)
#' A new residual for ordinal outcomes.
#' \emph{Biometrika}. \bold{99}: 473--480.
#' @export
#' @examples
#' library(survival)
#' library(stats)
#'
#' set.seed(100)
#' n <- 1000
#' x <- rnorm(n)
#' t <- rweibull(n, shape=1/3, scale=exp(x))
#' c <- rexp(n, 1/3)
#' y <- pmin(t, c)
#' d <- ifelse(t<=c, 1, 0)
#'
#' mod.survreg <- survreg(Surv(y, d) ~ x, dist="weibull")
#' summary(presid(mod.survreg))
#' plot(x, presid(mod.survreg))
#'
#' ##### example for proprotional hazards model
#' n <- 1000
#' x <- rnorm(n)
#' beta0 <- 1
#' beta1 <- 0.5
#' t <- rexp(n, rate = exp(beta0 + beta1*x))
#' c <- rexp(n, rate=1)
#' y <- ifelse(t<c, t, c)
#' delta <- as.integer(t<c)
#'
#' mod.coxph <- coxph(Surv(y, delta) ~ x)
#' presid <- presid(mod.coxph)
#' plot(x, presid, cex=0.4, col=delta+2)
#'
#' #### example for Negative Binomial regression
#' library(MASS)
#'
#' n <- 1000
#' beta0 <- 1
#' beta1 <- 0.5
#' x <- runif(n, min=-3, max=3)
#' y <- rnbinom(n, mu=exp(beta0 + beta1*x), size=3)
#'
#' mod.glm.nb <- glm.nb(y~x)
#' presid <- presid(mod.glm.nb)
#' summary(presid)
#' plot(x, presid, cex=0.4)
#'
#' ##### example for proportional odds model
#' library(MASS)
#'
#' n <- 1000
#' x  <- rnorm(n)
#' y  <- numeric(n)
#' alpha = c(-1, 0, 1, 2)
#' beta <- 1
#' py  <-  (1 + exp(- outer(alpha, beta*x, "+"))) ^ (-1)
#' aa = runif(n)
#' for(i in 1:n)
#'   y[i] = sum(aa[i] > py[,i])
#' y <-  as.factor(y)
#'
#'
#' mod.polr <- polr(y~x, method="logistic")
#' summary(mod.polr)
#' presid <- presid(mod.polr)
#' summary(presid)
#' plot(x, presid, cex=0.4)
presid <- function(object, ...) {
    UseMethod('presid', object)
}

