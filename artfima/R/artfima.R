artfima <-
function(z, glp=c("ARTFIMA", "ARFIMA", "ARIMA"), arimaOrder=c(0,0,0), 
         likAlg=c("exact", "Whittle"), fixd=NULL, b0=NULL, 
         lambdaMax = 3, dMax = 10) {
#
#######FIRST SECTION: set up for second section
  #
  #option fixd!=NULL only for ARTFIMA
  #the following are important "hyper-parameter" settings only for ARTFIMA
  # ignored for ARFIMA and ARIMA:
  #dMax <- 10 #abs(dHat) constrained to be less than dMax
  #lambdaMax <- 3 #max possible value for lambda estimate
  lambdaMin <- 0.000001 #min possible. This is fixed.
  #Generally you don't want to change this since it may result in numerical
  # inaccuracies. Only makes sense to consider changing these settings when
  # estimate has converged to boundary.
  #Note: because we reset for ARFIMA, we reset both these to dHi, dLo, etc in
  #the code a few lines below! Sorry for the confusion but is for the best!!
  #
  optAlg <- "None"
  constant <- TRUE
  stopifnot(is.numeric(z) && (is.ts(z) || is.vector(z)))
  stopifnot(length(arimaOrder==3) && is.numeric(arimaOrder) 
            && all(arimaOrder>=0))
  stopifnot(all(abs(arimaOrder-round(arimaOrder))<.Machine$double.eps^0.5))
  glp <- match.arg(glp)
  likAlg <- match.arg(likAlg)
  p <- arimaOrder[1]
  d0 <- arimaOrder[2]
  q <- arimaOrder[3]
  if (d0 > 0) {
    w <- diff(z, differences=d0)
  } else {
    w <- z
  }
  glpOrder <-switch(glp, "ARTFIMA"=2, "ARFIMA"=1, "ARIMA"=0)
  #fixd: must be null or numeric <2 and >=-0.5
  #number of additional parameters, 0 for ARMA, 1 for ARFIMA, 2 for ARTFIMA
  #   except when fixd is not NULL then it is glpOrder-1, for 
  glpAdd <- glpOrder-ifelse(is.null(fixd), 0, 1)
#
  stopifnot(is.null(b0) || is.null(fixd)) #b0 only available when fixd=NULL
  stopifnot(is.null(fixd)||(is.numeric(fixd)&&fixd<=2&&fixd>=-0.5)) 
  stopifnot(all(arimaOrder>=0))
  stopifnot(!(is.numeric(fixd)&&glpOrder!=2))
  is.wholenumber <- function(x) abs(x - round(x)) < .Machine$double.eps^0.5
  stopifnot(is.wholenumber(p), is.wholenumber(d0), is.wholenumber(q))
#
#upper and lower limits: used with penalty method optimization.
#Remark: convergence may be a problem with more liberal limits - be careful
  lambdaLo <- lambdaMin
  lambdaHi <- lambdaMax #(could be a little larger)
  dHi <- dMax #ARTFIMA limit (could be a little larger)
  dfHi <- 0.49 #ARFIMA limit
  #lower and upper limits with "L-BFGS-B". Useful sometimes!
  if (glp=="ARTFIMA") {
    blo<- c(lambdaLo, -dHi, rep(-0.99,p+q))
    bhi<- c(lambdaHi, dHi,  rep(0.99,p+q))
  } else {
    if (glp=="ARFIMA") {
      blo<- c(-dfHi, rep(-0.99,p+q))
    } else { #ARMA
      blo<- rep(-0.99,p+q) 
    }
    bhi <- -blo
  }
#
  mnw <- mean(w)
  if(d0 > 0) {
    w <- diff(z, differences=d0)
    }
  if(!constant) {
    mnw <- 0
  }
  w <- w-mnw
  varw <- var(w)
  n <- length(w)
#initialization
  nbeta <- p+q+glpAdd #adjusted for fixd
  binit <- numeric(nbeta)
  stopifnot(is.null(b0) || length(b0)==nbeta) #b0 must be right length
#Whittle method is fast because this is only done once
  if (likAlg%in%c("Whittle")) {
    Ip <- Periodogram(w)
  }
#null model and penalty
  nullModelLoglikelihood <- (-n/2)*log(sum(w^2)/n)
  entropyPenalty <-  switch(likAlg,
      exact=-nullModelLoglikelihood,
      Whittle=sum(w^2)
  )
  entropyPenalty <- entropyPenalty+2*abs(entropyPenalty)
#negative log-likelihood=entropy
#begin Entropy. beta - pacf parameterization
#note - ***parameters passed using lexical scoping***
#also tacvf r is exported to arfima environment
  count <- 0
  Entropy<-function(beta) { #begin Entropy() 
#in the optimization, put lambda, d, phi, theta
    phi<-theta<-lambda <- d <- numeric(0)
    r <- NA
    count <<- count+1
    if (glpOrder==2) {
      if (is.null(fixd)) { #full ARTFIMA
        d <- beta[1]
        lambda <- beta[2]
        if (abs(d)>dHi || lambda>lambdaHi || lambda < lambdaLo) {
          return(entropyPenalty)
        }
      } else { #fixd used, constrained ARTFIMA
        d <- fixd
        lambda <- beta[1]
        if (lambda>lambdaHi || lambda < lambdaLo) {
          return(entropyPenalty)
        }
      }
    } else { #ARFIMA
      if (glpOrder==1) {
        d <- beta[1]
        if (abs(d) >= dfHi) {
          return(entropyPenalty)
        }
      } 
    } #ARMA component
    if ((p>0 || q>0) && any(abs(beta[(1+glpAdd) : (p+q+glpAdd)])>=1.0)) {
      return(entropyPenalty)
    }
    if(p>0) phi <- try(PacfToAR(beta[(1+glpAdd):(p+glpAdd)]))
    if(q>0) theta <- try(PacfToAR(beta[(p+glpAdd+1):(p+q+glpAdd)]))
    if (!is.numeric(phi) || !is.numeric(theta)) {
      return(entropyPenalty)
    }
#exact
    if (likAlg=="exact") {
    r <- try(artfimaTACVF(d=d, lambda=lambda, phi = phi, theta = theta, 
                        maxlag = n-1))
    if (!is.numeric(r)) {
      negLL <- entropyPenalty
    } else {
      negLL <- try(-DLLoglikelihood(r, w), silent=TRUE)
    }
    negLL <- ifelse(is.numeric(negLL), negLL, entropyPenalty)
    } else { #Whittle
      fp <- artfimaSDF(n=n, d=d, lambda=lambda, phi=phi, theta=theta, 
                       plot="none")
      negLL <- mean(Ip/fp)
    }
#
#debugging...##############################
#cat("\n ***********iter = ", count, fill=TRUE)
#cat("negLL=", negLL, fill=TRUE)
#cat("beta=",beta,fill=TRUE)
#cat("d=", d, ",  lambda=", lambda, fill=TRUE)
#cat("r[1:10] = ", r[1:10], fill=TRUE)
#cat("w[1:10] = ", w[1:10], fill=TRUE)
##########################################
    negLL
  }#end Entropy()
#
#trace=6 for full output
  trace <- 0
  #
#####
#####SECOND SECTION: determining the parameter estimates
#
#Brent for ARTFIMA(0,0,0) with fixd (only lambda estimated)
  if (length(binit)==1 && is.numeric(fixd) && glpOrder==2) {
    optAlg <- "Brent"
    binit[1] <- 0.02 #initial value for lambda in constrained model
    ans<-optim(par=binit, fn=Entropy, method="Brent", upper=lambdaHi, 
               lower=lambdaLo, control=list(trace=trace), hessian=TRUE)
  } 
  #nbeta > 0
  else {
    #BEGIN: call opt()
    if (length(binit) > 0) {
      #we use "BFGS" with penalty as first choice, followed by others if needed
      #BEGIN: initialization
      if (length(b0) == 0){
        if (length(binit) > 0) { #non-null model
          if(glpOrder==2) {
            if (is.numeric(fixd)) {
              binit[1] <- 0.025 #lambda
            } else {
              binit[1] <- 0.3    #d
              binit[2] <- 0.025  #lambda
            }
          } else {
            if(glpOrder==1) {
              binit[1] <- 0.2 #d
            }
          }
          if (p > 0) {
            phiInit <- ARToPacf(rep(c(0.1, -0.1), p)[1:p])
            binit[glpAdd+(1:p)] <- phiInit
          }
          if (q > 0) {
            thetaInit <- ARToPacf(rep(c(0.1, -0.1), q)[1:q])
            binit[glpAdd+p+(1:q)] <-thetaInit
          }
          
        } else {
          binit <- b0 #special intialization
        }
      }  #END: initialization
      #default is "BFGS" but we try others if there is a problem
      optAlg <- "BFGS"
      ans <- try(optim(par=binit, fn=Entropy, method="BFGS", 
                       control=list(trace=trace, maxit=500), hessian=TRUE),
                 silent=TRUE)
      #try "L-BFGS-U" if error
      if(class(ans)=="try-error" || ans$convergence>0) {
        optAlg <- "L-BFGS-B"
        ans <- try(optim(par=binit, fn=Entropy, method="L-BFGS-B", lower=blo, 
                         upper=bhi, control=list(trace=trace, maxit=500), 
                         hessian=TRUE), silent=TRUE)
      }
      #try CG if error
      if(class(ans)=="try-error" || ans$convergence>0) {
        optAlg <- "CG"
        ans <- try(optim(par=binit, fn=Entropy, method="CG", 
                         control=list(trace=trace, maxit=500), hessian=TRUE),
                   silent=TRUE)
      }
      #try Nelder-Mead if error
      if(class(ans)=="try-error" || ans$convergence>0) {
        optAlg <- "Nelder-Mead"
        ans <- try(optim(par=binit, fn=Entropy, method="Nelder-Mead", 
                         control=list(trace=trace, maxit=500), hessian=TRUE),
                   silent=TRUE)
      }
      #finished with optim() else if length(binit) = 0, null case
      } else {
      #at present we only allow all parameters null
        ans <- NULL
        r <- try(artfimaTACVF(d=numeric(0), lambda=numeric(0), phi = numeric(0), 
                            theta = numeric(0), maxlag = n-1))
        if (!is.numeric(r)) {
          negLL <- entropyPenalty
        } else {
          negLL <- try(-DLLoglikelihood(r, w), silent=TRUE)
        }
        ans$value <- negLL
        ans$hessian <- ans$par <- numeric(0)
        ans$convergence <- 0
      }
    }
  #
  #THIRD SECTION: wrapping up
  negLL <- ans$value
  bHat <- ans$par
  lambdaHat <- dHat <- phiHat <- thetaHat <- numeric(0)
#BEGIN: get dHat ...
#get dHat and lambdaHat and test for boundary estimates
  onBoundary <- FALSE
  if (glpOrder > 0)  {
    dHat <- bHat[glpOrder]
  }
  if (glpOrder == 1 && abs(abs(dHat)-dfHi)<0.01) {
    onBoundary <- TRUE
  }
  if (glpOrder==2) {
    if (is.null(fixd)) {
      dHat <- bHat[1]
      lambdaHat <- bHat[2]
      } else {
        dHat <- fixd
        lambdaHat <- bHat[1]
      }
#only about upper boundary for lambda
    distBoundary = min(c(abs(lambdaHi-lambdaHat), abs(dHat-dHi)))
    if (distBoundary < 0.01) {
      onBoundary <- TRUE
    }
  }#end glpOrder==2
#END: get dHat ...
#
  if (p > 0) {
    phiHat <- PacfToAR(bHat[(1+glpAdd):(p+glpAdd)])
  }
  if (q > 0) {
    thetaHat <- PacfToAR(bHat[(p+1+glpAdd):(p+q+glpAdd)])
  }
  rHat <- artfimaTACVF(d=dHat, lambda=lambdaHat, 
                   phi = phiHat, theta = thetaHat, maxlag = n-1)
  convergence <- ans$convergence
#since Entropy is used, Hessian is pd
  varbeta <- Hinv<-try(solve(ans$hessian), silent=TRUE)
  if(!all(is.numeric(Hinv))) Hinv <- matrix(NA, nrow=nbeta, ncol=nbeta)
  if(!any(is.na(Hinv)) && all(diag(Hinv)>0)) { #next square-root diagnonal
    sebHat <- suppressWarnings(sqrt(diag(Hinv)))
    } else {
      sebHat<-rep(NA, nbeta)
    }
  if (is.numeric(fixd)) sebHat <- c(0, sebHat) 
  rhoHat <- rHat[-1]/rHat[1]
  seMean <- sqrt(var(w)/n*(1+2*sum(1-(1:(n-1))/n*rhoHat^2))/n)
  constantHat <- mnw
  res <- try(DLResiduals(rHat, w), silent=TRUE)
  if (class(res)=="try-error") {
    res <- NA
  }
  ansEx <- try(exactLoglikelihood(rHat, w), silent=TRUE)
  if(class(ansEx)!="try-error") { #possible converged but still problem
    LL <- ansEx$LL
    sigmaSq <- ansEx$sigmaSq
    } else {
      LL <- NA
      sigmaSq <- NA
    }
  if (likAlg%in%c("Whittle")) { #adjust sebHat
    if (identical(is.numeric(sebHat)&&sigmaSq>=0, TRUE)) {
        sebHat <- sqrt(sigmaSq)*sebHat/sqrt(n)
        varbeta <- sigmaSq*varbeta
    } else {
        sebHat <- rep(NA, nbeta)
        varbeta <- NA
    }
  }
#
  snr <- (varw-sigmaSq)/sigmaSq
  K <- nbeta
  aic <- (-2)*LL + 2*(K+2)
  bic <- (-2)*LL + (K+2)*log(length(w))
  out<-list(dHat=dHat, lambdaHat=lambdaHat, phiHat=phiHat, thetaHat=thetaHat, 
            constant=constantHat, sigmaSq=sigmaSq, bHat=bHat, seMean=seMean, 
            se=sebHat, n=n, snr=snr, likAlg=likAlg, LL=LL, aic=aic, bic=bic,
            nbeta=nbeta, convergence=convergence, glp=glp, b0=ans$par, 
            arimaOrder=arimaOrder, glpOrder=glpOrder, fixd=fixd, glpAdd=glpAdd,
            tacvf=rHat, z=z, res=res, nullModelLogLik=nullModelLoglikelihood, 
            onBoundary=onBoundary, message=ans$message, optAlg=optAlg, 
            varbeta = varbeta, hessian=ans$hessian)
 class(out) <- "artfima"
 out
}
