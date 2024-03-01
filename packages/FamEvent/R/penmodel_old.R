penmodel <- function(formula, cluster="famID", gvar="mgene", parms, cuts=NULL, data, design="pop", base.dist="Weibull", agemin=NULL, robust=FALSE){
  
  if(any(is.na(data[, gvar]))) stop("data include missing genetic information, use penmodelEM function.")
  options(na.action='na.omit')
  agemin.data <- attr(data, "agemin")
  if(!is.null(agemin.data)) agemin <- agemin.data
  else if(is.null(agemin)) stop("agemin is not found. Please specify agemin.")
    
  if(agemin > 70) warning("agemin is set too high.")
  
  # if(sum(data$time <=  agemin, na.rm = TRUE) > 0) cat("Individuals with time <= agemin were removed from the analysis.\n")
  
  data <- data[data$time >  agemin, ]
  
  data$famID.byuser <- data[, cluster]
  
  Call <- match.call()
  indx <- match(c("formula", "data"), names(Call), nomatch = 0)
  if (indx[1] == 0) stop("A formula argument is required")
  temp <- Call[c(1, indx)]
  temp[[1L]] <- quote(stats::model.frame)

  temp$formula <- if (missing(data)) terms(formula)
  else terms(formula, data = data)
  
  temp$data <- data

  if (is.R()) m <- eval(temp, parent.frame())
  else m <- eval(temp, sys.parent())
  
  Terms <- attr(m, "terms")
  Y <- model.extract(m, "response")
  
  if (!inherits(Y, "Surv")) stop("Response must be a survival object.")
  
  type <- attr(Y, "type")
  if (type == "counting") stop("start-stop type Surv objects are not supported.")
  if (type == "mright" || type == "mcounting") stop("multi-state survival is not supported.")

  if(base.dist=="piecewise"){
    if(is.null(cuts)) stop("The cuts should be specified")
    if(any(cuts > max(Y[,1]) | cuts < min(Y[,1]))) stop("Some value(s) of the cuts are beyond the range.")
  }


  X <- model.matrix(Terms, m)
  
  if (is.R()) {
    assign <- lapply(attrassign(X, Terms)[-1], function(x) x - 1)
    xlevels <- .getXlevels(Terms, m)
    contr.save <- attr(X, "contrasts")
  }
  else {
    assign <- lapply(attr(X, "assign")[-1], function(x) x - 1)
    xvars <- as.character(attr(Terms, "variables"))
    xvars <- xvars[-attr(Terms, "response")]
    if (length(xvars) > 0) {
      xlevels <- lapply(m[xvars], levels)
      xlevels <- xlevels[!unlist(lapply(xlevels, is.null))]
      if (length(xlevels) == 0) 
        xlevels <- NULL
    }
    else xlevels <- NULL
    contr.save <- attr(X, "contrasts")
  }

  nvar <- ncol(X)-1
  var.names <- colnames(X)[-1]
  if(nvar==1) X <- matrix(X[,-1])
  else X <- X[,-1]

  #number of parameters for baseline
  nbase <- ifelse(base.dist=="logBurr", 3, ifelse(base.dist=="piecewise", length(cuts)+1, 2)) 

  #nk <- ifelse(is.null(frailty.dist),0,1)
  colnames(X) <- var.names
  vbeta <- parms[-c(1:(nbase))]
 
#  if(length(vbeta) != nvar) stop("The size of parms is incorrect.")
  if(length(parms) != (nvar+nbase) ) stop("The size of parms is incorrect.")
  
  if(is.null(data$weight)) data$weight <- 1
 
  if(base.dist=="lognormal"){
  	if(parms[2] <= 0) stop("parms[2] has to be > 0")
  	else est1 <- optim(c(parms[1], log(parms[2]), vbeta), loglik_rev, X=X, Y=Y, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin)
  } 
  else if(any(parms[1:nbase]<=0)) stop("All baseline parameters should be > 0")
  else {
    est1 <- optim(c(log(parms[1:nbase]), vbeta), loglik_rev, X=X, Y=Y, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin)
  }
  #print(est1)
  
  logLik <- -est1$value
    EST <- est1$par
#    Var <- try(solve(est1$hessian), TRUE)
    H <- hessian(loglik_rev, est1$par, X=X, Y=Y, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin)
    Var <- try(solve(H), TRUE)
    
  if(!is.null(attr(Var,"class"))) stop("Model did not converge.\n  Try again with different initial values.")
  else{ 
    bparms.name <- c("log(lambda)","log(rho)", "log(eta)")
    if(base.dist=="lognormal") bparms.name[1] <- "lambda" 
    else if(base.dist=="piecewise") bparms.name <- paste0("log(q", 1:nbase,")")
    
    parms.cov <- Var
    parms.se <- sqrt(diag(parms.cov))
    parms.cov.robust <- NULL
    parms.se.robust <- NULL
    names(EST) <- names(parms.se)  <- rownames(parms.cov) <- colnames(parms.cov) <- c(bparms.name[1:nbase], colnames(X))
    
    if(robust){
      grad <- numericGradient(loglik_rev, est1$par, X=X, Y=Y, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin, vec=TRUE)
      Jscore <- t(grad)%*%grad

      j2 <- jacobian(loglik_rev, est1$par, X=X, Y=Y, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin, vec=TRUE)
#  	 print("j2")
#  	 print(t(j2)%*%j2)
#  	 print("Jscore")
#  	 print(Jscore)
 		 parms.cov.robust <- Var%*%(Jscore)%*%Var
 		 parms.se.robust <- sqrt(diag(parms.cov.robust))
 		 rownames(parms.cov.robust) <- colnames(parms.cov.robust) <- c(bparms.name[1:nbase], colnames(X))
    }
  }
    
    aic = 2*length(EST) - 2*logLik
    
    out <- list(estimates = EST, varcov = parms.cov, varcov.robust = parms.cov.robust, se = parms.se, se.robust = parms.se.robust,logLik = logLik, AIC=aic)  
    class(out) <- "penmodel"
    attr(out, "design") <- design
    attr(out, "base.dist") <- base.dist
#    attr(out, "frailty.dist") <- frailty.dist
    attr(out, "agemin") <- agemin
    attr(out, "cuts") <- cuts
    attr(out, "nbase") <- nbase
    attr(out, "data") <- data
    attr(out, "robust") <- robust
    attr(out, "formula") <- formula
    attr(out, "X") <- X
    attr(out, "Y") <- Y
    invisible(out)
    
}#end
  