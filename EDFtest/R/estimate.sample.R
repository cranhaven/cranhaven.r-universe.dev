#' MLE for univariate sample
#'
#' @description
#' Estimate parameters of various distributions by the method of maximum likelihood.
#' The following families are available: Normal(location=\eqn{\mu},scale=\eqn{\sigma^2}),
#' Gamma(shape=\eqn{\alpha},scale=\eqn{\beta}), Logistic(location=\eqn{\mu},scale=s),
#' Laplace(location=\eqn{\mu},scale=b), Weibull(shape=\eqn{\alpha},scale=\eqn{\beta}),
#' and Exponential(scale=\eqn{\theta}).
#'
#' @param x A random sample.
#' @param use.rate Logical; if TRUE use the rate instead of the scale.
#' @param use.sd Logical; if TRUE use the sd instead of the scale for Laplace distribution.
#' @param eps Stopping criterion, 1e-7 by default.
#' @param verbose Logical; if TRUE, print estimates in each iteration.
#'
#' @return Estimated parameters of the assumed distribution by MLE.
#'
#' @name estimate
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' estimate.uniform(x0)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' estimate.normal(x1)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' estimate.gamma(x2)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' estimate.logistic(x3)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' estimate.laplace(x4)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' estimate.weibull(x5)
#'
#' x6=rexp(n=100,rate=1/2)
#' estimate.exp(x6,use.rate=TRUE)
NULL

#' @export
#' @rdname estimate
estimate.uniform = function(x){
  c(min(x),max(x))
}

#' @export
#' @rdname estimate
estimate.normal = function(x){
  c(mean(x),sd(x))
}

#' @export
#' @rdname estimate
estimate.gamma <- function(x,use.rate=FALSE){
  # Estimate shape and scale parameters of the Gamma distribution
  # by the method of maximum likelihood.
  # Use the digamma and trigamma functions in Base R and do Newton-Raphson
  # on the profile log-likelihood for alpha
  n <- length(x)
  m1 <- mean(x)
  m2 <- var(x)
  b <- m2/m1
  a <- m1/b
  mlog <- mean(log(x))
  logm=log(m1)
  aold <- a
  anew <- aold -(log(aold)-logm + mlog -digamma(aold))/(1/aold-trigamma(aold))
  bnew=m1/anew
  if( anew < 0) anew <- aold/2
  while ( abs(anew-aold) > 1e-7){
    aold <- anew
    old.score = (log(aold)-log(m1)+ mlog -digamma(aold))
    old.score.derivative = 1/aold-trigamma(aold)
    anew <- aold - old.score/old.score.derivative
    if( anew < 0) anew <- aold/2
  }
  beta <- m1/anew
  alpha <- anew
  if(use.rate){
    return(c(alpha,1/beta))
  }
  c(alpha, beta)
}

#' @export
#' @rdname estimate
estimate.logistic <- function(x,eps=1e-7,verbose=FALSE){
  hessianscore.logistic=function(x,theta){
    #
    # This is the hessian and score function for the two parameter logistic distribution
    #
    scale=theta[2]
    location=theta[1]
    y=(x-location)/scale
    e=exp(-y)
    r=(1-e)/(1+e)
    ell = sum(-log(scale) + y-2*log(1+exp(y)))
    s.scale= y*r/scale -1/scale
    s.location= r/scale
    score = c(sum(s.location),sum(s.scale))
    h.den=(scale*(1+e))^2
    r.mm = -2*sum(e/h.den)
    r.ms = sum((e^2-2*y*e-1)/h.den)
    r.ss= sum(((2*y+1)*e^2-2*(y^2-1)*e-(2*y-1))/h.den)
    Hessian = matrix(c(r.mm,r.ms,r.ms,r.ss),nrow=2)
    ev=eigen(-Hessian)$values
    if(any(ev <= 0)) cat("Warning: Hessian contains some eigenvalue of the wrong sign\n")
    list(loglikelihood = ell,score=score,Hessian=Hessian)
  } # for Newton-Raphson

  m1 <- mean(x)
  m2 <- var(x)
  iterates=0
  binit = sqrt(m2*3/pi^2)
  ainit=m1
  thetaold=c(ainit,binit)
  ders = hessianscore.logistic(x,theta=thetaold)
  ell.old=ders$loglikelihood
  if(verbose){
    cat("Initial Estimates and Likelihood\n")
    cat(thetaold)
    cat(ders$loglikelihood)
    cat("\n Score\n")
    cat(ders$score)
  }
  step = solve(ders$Hessian,ders$score)

  thetanew = thetaold-step
  while(thetanew[2]<0){
    if(verbose) cat("Step too far so \n New theta ",thetanew," step size ",step,"\n\n")
    step[2]=step[2]/2
    thetanew = thetaold-step
  }
  ders = hessianscore.logistic(x,theta=thetanew)
  iter=0
  while( ders$loglikelihood < ell.old){
    iter=iter+1
    step=step/2
    thetanew = thetaold-step
    if(verbose){
      cat("Likelihood didn't increase \n")
      cat(" was ",ell.old," now ",ders$loglikelihood,"\n")
      cat(" so \n New theta ",thetanew," step size ",step,"\n\n")
    }
    ders = hessianscore.logistic(x,theta=thetanew)
    if(verbose){
      cat(ders$score)
      cat("\n Iteration ",iter," log likelihood ",ders$loglikelihood,"\n")
    }
  }
  delta = sqrt(sum((thetanew-thetaold)^2))
  if(verbose){
    print(cbind(thetaold, thetanew, ders$score,abs(thetaold-thetanew),ders$loglikelihood))
  }
  while ( abs(delta) > eps){
    if(verbose){
      cat("Looping\n")
    }
    thetaold=thetanew
    ders = hessianscore.logistic(x,theta=thetanew)
    step =solve(ders$Hessian,ders$score)
    thetanew = thetaold-step
    delta = sqrt(sum((thetanew-thetaold)^2))
    if(verbose){
      print(cbind(thetaold, thetanew, ders$score, abs(thetaold-thetanew),ders$loglikelihood))
    }
    iterates=iterates+1
    if(iterates>50)break()
  }
  thetanew
}

#' @export
#' @rdname estimate
estimate.laplace = function(x,use.sd=FALSE){
  med = median(x)
  MAD = mean(abs(x-med))
  if(use.sd){
    return(c(med,sqrt(2)*MAD))
  }
  c(med,MAD)
}

#' @export
#' @rdname estimate
estimate.weibull <- function(x,eps=1e-7){
  n <- length(x)
  m1 <- mean(x)
  m2 <- var(x)
  b <- m2/m1
  a <- m1/b
  mlog <- mean(log(x))
  aold <- a
  ml <- mean(log(x))
  mla <- mean(x^aold*log(x))
  ma <- mean(x^aold)
  anew <- 1/(mla/ma-ml)
  anew <- sqrt(anew*aold)

  while ( abs(anew-aold) > eps){
    aold <- anew
    mla <- mean(x^aold*log(x))
    ma <- mean(x^aold)
    anew <- 1/(mla/ma-ml)
    anew <- sqrt(anew*aold)
  }
  beta <- mean(x^anew)^(1/anew)
  alpha <- anew
  c(alpha, beta)
}

#' @export
#' @rdname estimate
estimate.exp = function(x,use.rate=FALSE){
  if(use.rate){
    return(1/mean(x))
  }
  mean(x)
}

