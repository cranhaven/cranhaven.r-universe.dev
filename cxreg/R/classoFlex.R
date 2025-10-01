#' fit a complex-valued lasso for a path of lambda values
#'
#' Fit a complex-valued lasso formulation for a path of lambda values.
#' \code{classo.path} solves the Lasso problem for a path of lambda values.
#'
#' @param x Complex-valued input matrix, of dimension nobs by nvar;
#' each row is an observation vector.
#' @param y Complex-valued response variable, nobs dimensional vector.
#' @param weights Observation weights. Default is 1 for each observation.
#' @param standardize Logical flag for x variable standardize beforehand; i.e. for n and p by nobs and nvar,
#' \deqn{\|X_j\|=\sqrt{n} \textrm{for all }j=1,\ldots,p}
#' is satisfied for the input x. Default is \code{FALSE}.
#' @param lambda A user supplied \code{lambda} sequence. Default is \code{NULL}.
#' @param nlambda The number of \code{lambda} values. Default is 100.
#' @param lambda.min.ratio If \code{nobs} < \code{nvars}, the default is 0.01.
#' @param intercept Should intercept be set to zero (default=FALSE) or fitted (FALSE)? This default is reversed from \code{glmnet} package.
#' @param thresh Convergence threshold for coordinate descent. Each inner
#' coordinate-descent loop continues until the maximum change in the objective after any
#' coefficient update is less than thresh times the null deviance. Default value is \code{1e-10}.
#' @param maxit Maximum number of iterations of outer loop. Default 10,000.
#' @param trace.it Controls how much information is printed to screen. Default is
#' \code{trace.it=0} (no information printed). If \code{trace.it=1}, a progress
#' bar is displayed. If \code{trace.it=2}, some information about the fitting
#' procedure is printed to the console as the model is being fitted.
#' @param \dots Other arguments that can be passed to \code{classo}
#'
#' @return An object with class "classofit" and "classo".
#' \item{a0}{Intercept sequence of length \code{length(lambda)}.}
#' \item{beta}{A \code{nvars x length(lambda)} matrix of coefficients, stored in
#' sparse matrix format.}
#' \item{df}{The number of nonzero coefficients for each value of lambda.}
#' \item{dim}{Dimension of coefficient matrix.}
#' \item{lambda}{The actual sequence of lambda values used. When alpha=0, the
#' largest lambda reported does not quite give the zero coefficients reported
#' (lambda=inf would in principle). Instead, the largest lambda for alpha=0.001
#' is used, and the sequence of lambda values is derived from this.}
#' \item{dev.ratio}{The fraction of (null) deviance explained. The deviance
#' calculations incorporate weights if present in the model. The deviance is
#' defined to be 2*(loglike_sat - loglike), where loglike_sat is the log-likelihood
#' for the saturated model (a model with a free parameter per observation).
#' Hence dev.ratio=1-dev/nulldev.}
#' \item{nulldev}{Null deviance (per observation). This is defined to be
#' 2*(loglike_sat -loglike(Null)). The null model refers to the intercept model.}
#' \item{npasses}{Total passes over the data summed over all lambda values.}
#' \item{jerr}{Error flag, for warnings and errors (largely for internal
#' debugging).}
#' \item{call}{The call that produced this object.}
#' \item{family}{Family used for the model.}
#' \item{nobs}{Number of observations.}
#'
#' @useDynLib cxreg classocd_warm classocd_warm_screen
classo.path <- function(x,y,
                        weights=NULL,
                        standardize=FALSE,
                        lambda=NULL,
                        nlambda=100,
                        lambda.min.ratio=ifelse(nobs<nvars,1e-2,1e-4),
                        intercept = FALSE,
                        thresh = 1e-10,
                        maxit = 100000,
                        trace.it = 0, ...){
  
  ####################################################################
  # Prepare all the generic arguments
  this.call <- match.call()
  control <- classo.control()
  
  np <- dim(x)
  if(is.null(np) || (np[2] <= 1)) {
    stop("x should be a matrix with 2 or more columns")
  }
  nobs <- np[1]
  
  # Unlike glmnet, we treat the intercept term by merging them into the design matrix
  # and treat it p+1-dimensional object.
  if (intercept){
    x <- cbind(rep(1,nobs),x)
    nvars <- np[2] + 1
  }else{
    nvars <- np[2]
  }
  
  ####################################################################
  # get feature variable names
  vnames <- colnames(x)
  # Unlike glmnet, we treat the intercept term by merging them into the design matrix
  # and treat it p+1-dimensional object.
  if (intercept){
    x <- cbind(rep(1,nobs),x)
    nvars <- np[2] + 1
  }else{
    nvars <- np[2]
  }
  
  if(is.null(vnames)){
    if (intercept){
      vnames <- c("intercept",paste("V",seq(nvars),sep=""))
    }else{
      vnames <- paste("V",seq(nvars),sep="") 
    }
  }
  
  ####################################################################
  # check weights
  if(is.null(weights)) {
    weights <- rep(1,nobs)
  }else if (length(weights) != nobs){
    stop(paste("Number of elements in weights (",length(weights),")
                not equal to the number of rows of x (",nobs,")",sep=""))
  }
  weights <- as.double(weights)
  
  ####################################################################
  # standardize x if necessary
  if (intercept) {
    meansd <- weighted_mean_sd(x, weights)
    xm <- meansd$mean
  } else {
    xm <- rep(0.0, times = nvars)
  }
  
  if (standardize) {
    for (j in 1:nvars){
      x[,j] <- x[,j] / sqrt(mean(Mod(x[,j])^2))
    }
  }
  
  ####################################################################
  # check on limits
  control <- classo.control()
  ####################################################################
  # if (thresh >= control$epsnr)
  #   warning("thresh should be smaller than glmnet.control()$epsnr",
  #           call. = FALSE)
  # 
  # if(any(lower.limits > 0)){ stop("Lower limits should be non-positive") }
  # if(any(upper.limits < 0)){ stop("Upper limits should be non-negative") }
  # lower.limits[lower.limits == -Inf] = -control$big
  # upper.limits[upper.limits == Inf] = control$big
  # if (length(lower.limits) < nvars) {
  #   if(length(lower.limits) == 1) lower.limits = rep(lower.limits, nvars) else
  #     stop("Require length 1 or nvars lower.limits")
  # } else lower.limits = lower.limits[seq(nvars)]
  # if (length(upper.limits) < nvars) {
  #   if(length(upper.limits) == 1) upper.limits = rep(upper.limits, nvars) else
  #     stop("Require length 1 or nvars upper.limits")
  # } else upper.limits = upper.limits[seq(nvars)]
  # 
  # if (any(lower.limits == 0) || any(upper.limits == 0)) {
  #   ###Bounds of zero can mess with the lambda sequence and fdev;
  #   ###ie nothing happens and if fdev is not zero, the path can stop
  #   fdev <- glmnet.control()$fdev
  #   if(fdev!= 0) {
  #     glmnet.control(fdev = 0)
  #     on.exit(glmnet.control(fdev = fdev))
  #   }
  # }
  ####################################################################

  ####################################################################
  # get null deviance and lambda max
  start_val <- get_start(x, y, weights, intercept)
  
  # work out lambda values
  nlam <- as.integer(nlambda)
  user_lambda <- FALSE   # did user provide their own lambda values?
  if (is.null(lambda)) {
    if (lambda.min.ratio >= 1) {
      stop("lambda.min.ratio should be less than 1")
    }
    lambda_max <- start_val$lambda_max
    
    # compute lambda sequence
    ulam <- exp(seq(log(lambda_max), log(lambda_max*lambda.min.ratio), 
                    length.out = nlam))
  } else {  # user provided lambda values
    user_lambda <- TRUE
    if (any(lambda < 0)) {
      stop("lambdas should be non-negative")
    }
    ulam <- as.double(rev(sort(lambda)))
    nlam <- as.integer(length(lambda))
  }
  
  # start progress bar
  if (trace.it == 1) {
    pb <- utils::txtProgressBar(min = 0, max = nlam, style = 3)
  }

    
  if (intercept){
    a0 <- rep(NA, length = nlam)
  }
  beta <- matrix(0, nrow = nvars, ncol = nlam)
  dev.ratio <- rep(NA, length = nlam)
  fit <- NULL
  # mnl <- min(nlam, control$mnlam)
  mnl <- nlam
  for (k in 1:nlam) {
    # get the correct lambda value to fit
    if (k > 1) {
      cur_lambda <- ulam[k]
    } else {
      ####################################################################
      # cur_lambda <- ifelse(user_lambda, ulam[k], control$big)
      cur_lambda <- ifelse(user_lambda, ulam[k], ulam[k])
      ####################################################################
    }
    
    if (trace.it == 2) {
      cat("Fitting lambda index", k, ":", ulam[k], fill = TRUE)
    }
    
    ####################################################################
    # changes of classocd_warm & classocd_warm_screen:
    # 1. add weights
    # 2. add thresh and  maxit
    if(k == 1) {
      b0 <- as.complex(rep(0,nvars))
      fit <- .Fortran("classocd_warm", 
                      x = as.complex(x), 
                      y = as.complex(y), 
                      n = as.integer(nobs),
                      p = as.integer(nvars),
                      lambda = as.double(cur_lambda),
                      b0 = as.complex(b0),
                      b = as.complex(rep(0, nvars))
      )
      
    } else {
      prev_lambda <- ulam[k-1]
      fit <- .Fortran("classocd_warm_screen", 
                      x = as.complex(x), 
                      y = as.complex(y), 
                      n = as.integer(nobs),
                      p = as.integer(nvars),
                      lambda = as.double(cur_lambda),
                      lambda0 = as.double(prev_lambda),
                      b0 = as.complex(b0),
                      b = as.complex(rep(0, nvars))
      )
    }
    ####################################################################
    if (trace.it == 1) {
      utils::setTxtProgressBar(pb, k)
    }
    
    if (intercept){
      a0[k] <- fit$b[1]
      beta[,k] <- as.matrix(fit$b[-1])
    }else{
      beta[,k] <- as.matrix(fit$b)
    }
    
    dev.ratio[k] <- (1 - (dev_comp(x,y,weights,beta[,k]) / start_val$nulldev))
    b0 <- fit$b
    
    # early stopping if dev almost 1 or no improvement
    if (k >= mnl && user_lambda == FALSE) {
      if (dev.ratio[k] > control$devmax) {
        break
      }
      else if (k > 1) {
        if (dev.ratio[k] - dev.ratio[k-1] < control$fdev * dev.ratio[k]){
          break
        }
      }
    }
  }
  if (trace.it == 1) {
    utils::setTxtProgressBar(pb, nlam)
    cat("", fill = TRUE)
  }
  
  # truncate a0, beta, dev.ratio, lambda if necessary
  if (k < nlam) {
    if (intercept){
      a0 <- a0[1:k]
    }
    beta <- beta[, 1:k, drop = FALSE]
    dev.ratio <- dev.ratio[1:k]
    ulam <- ulam[1:k]
  }
    
  # output
  stepnames <- paste0("s", 0:(length(ulam) - 1))
  out <- list()
  if (intercept){
    out$a0 <- a0
    names(out$a0) <- stepnames
  }else{
    out$a0 <- NULL
  }
  rownames(beta) <- vnames
  colnames(beta) <- stepnames
  out$beta <- beta
  out$df <- colSums(abs(beta) > 0)
  out$dim <- dim(beta)
  out$lambda <- ulam
  out$dev.ratio <- dev.ratio
  out$nulldev <- start_val$nulldev
  out$call <- this.call
  out$nobs <- nobs
  
  classofit <- list(
    call = this.call,
    Df = out$df,
    dev.ratio = out$dev.ratio,
    lambda = out$lambda
  )
  class(out) <- c("classofit","classo")
  
  return(out)
}
  

####################################################################
dev_comp <- function(x,y,weights,beta) {
  return(sum(c(weights,weights) %*% (c(Re(y),Im(y)) - rbind((Re(x) %*% Re(beta)),(Im(x) %*% Im(beta))))^2))
}
  
####################################################################
get_start <- function(x,y,weights,intercept) {
  
  ####################################################################
  nobs <- nrow(x)
  nvars <- ncol(x)
  
  # compute mu and null deviance
  if (intercept) {
    mu <- rep(weighted.mean(y,weights), times = nobs)
  } else {
    mu <- rep(0, times = nobs)
  }
  
  nulldev <- sum(c(weights,weights) %*% (c(Re(y),Im(y)) - c(Re(mu),Im(mu)))^2)
  
  ####################################################################
  # compute lambda max
  r <- y - mu
  eta <- mu
  v <- rep.int(1, length(mu))
  m.e <- rep.int(1, length(eta))
  w <- weights / sum(weights)
  rv <- r / v * m.e * w
  g <- abs(t(rv) %*% x)
  lambda_max <- max(g)
  
  return(list(nulldev = nulldev, 
       mu = mu, 
       lambda_max = lambda_max))
}
  

####################################################################
weighted_mean_sd <- function(x, weights=rep(1,nrow(x))){
  weights <- weights/sum(weights)
  xm <- drop(t(weights)%*%x)
  xv <- drop(t(weights)%*%scale(x,xm,FALSE)^2)
  xv[xv < 10*.Machine$double.eps] <- 0
  list(mean = xm, sd = sqrt(xv))
}
  
  
  
  
  
  
  