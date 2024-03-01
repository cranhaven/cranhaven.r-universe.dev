penmodel_c <- function(formula1, formula2, cluster="famID", gvar="mgene", parms, cuts=NULL, data, design="pop", base.dist="Weibull", frailty.dist="none", agemin=NULL, robust=FALSE){
  
  if(any(is.na(data[, gvar]))) stop("data include missing genetic information, use penmodelEM function.")
  options(na.action='na.omit')
  if(is.null(agemin)) agemin <- attr(data, "agemin")
  if(is.null(agemin)) stop("agemin is not found. Please specify agemin.")
    
  if(agemin > 70) warning("agemin is set too high.")
  if(length(base.dist)==1) base.dist <- rep(base.dist,2)
  
  if(!frailty.dist%in%c("none", "gamma", "cgamma", "lognormal", "clognormal")) stop("Unrecognized frailty.dist.")
  
  # if(sum(data$time <=  agemin, na.rm = TRUE) > 0) cat("Individuals with time <= agemin were removed from the analysis.\n")
  
  data <- data[data$time >  agemin, ]
  
  data$famID.byuser <- data[, cluster]
  
  Call <- match.call()
  indx1 <- match(c("formula1", "data"), names(Call), nomatch = 0)
  indx2 <- match(c("formula2", "data"), names(Call), nomatch = 0)
  
  if (indx1[1] == 0 | indx2[1] == 0) stop("A formula argument is required")
  temp1 <- Call[c(1, indx1)]
  names(temp1)[2] <- "formula"
  temp1[[1L]] <- quote(stats::model.frame)
  
  temp2 <- Call[c(1, indx2)]
  names(temp2)[2] <- "formula"
  temp2[[1L]] <- quote(stats::model.frame)
  
  if (missing(data)){ 
    temp1$formula <- terms(formula1)
    temp2$formula <- terms(formula2)
  }
  else {
    temp1$formula <- terms(formula1, data = data)
    temp2$formula <- terms(formula2, data = data)
  }
  temp1$data <- data
  temp2$data <- data
  
  if (is.R()){
    m1 <- eval(temp1, parent.frame())
    m2 <- eval(temp2, parent.frame())
  }
  else{ 
    m1 <- eval(temp1, sys.parent())
    m2 <- eval(temp2, sys.parent())
  }
  
  Terms1 <- attr(m1, "terms")
  Terms2 <- attr(m2, "terms")
  
  Y1 <- model.extract(m1, "response")
  Y2 <- model.extract(m2, "response")
  
  if (!inherits(Y1, "Surv") | !inherits(Y2, "Surv")) stop("Response must be a survival object.")
  
  type <- attr(Y1, "type")
  if (type == "counting") stop("start-stop type Surv objects are not supported.")
  if (type == "mright" || type == "mcounting") stop("multi-state survival is not supported.")

  if(base.dist[1]=="piecewise"){
    if(is.null(cuts)) stop("The cuts should be specified")
    if(any(cuts > max(Y1[,1]) | cuts < min(Y1[,1]))) stop("Some value(s) of the cuts are beyond the range.")
  }

  if(base.dist[2]=="piecewise"){
    if(is.null(cuts)) stop("The cuts should be specified")
    if(any(cuts > max(Y2[,1]) | cuts < min(Y2[,1]))) stop("Some value(s) of the cuts are beyond the range.")
  }
  
  X1 <- model.matrix(Terms1, m1)
  X2 <- model.matrix(Terms2, m2)
  
  if (is.R()) {
    assign1 <- lapply(attrassign(X1, Terms1)[-1], function(x) x - 1)
    assign2 <- lapply(attrassign(X2, Terms2)[-1], function(x) x - 1)
    xlevels1 <- .getXlevels(Terms1, m1)
    xlevels2 <- .getXlevels(Terms2, m2)
    contr.save1 <- attr(X1, "contrasts")
    contr.save2 <- attr(X2, "contrasts")
  }
  else {
    assign1 <- lapply(attr(X1, "assign")[-1], function(x) x - 1)
    xvars1 <- as.character(attr(Terms1, "variables"))
    xvars1 <- xvars1[-attr(Terms1, "response")]
    assign2 <- lapply(attr(X2, "assign")[-1], function(x) x - 1)
    xvars2 <- as.character(attr(Terms2, "variables"))
    xvars2 <- xvars2[-attr(Terms2, "response")]
    
    if (length(xvars1) > 0) {
      xlevels1 <- lapply(m1[xvars1], levels)
      xlevels1 <- xlevels1[!unlist(lapply(xlevels1, is.null))]
      if (length(xlevels1) == 0) xlevels1 <- NULL
    }
    else xlevels1 <- NULL
    
    if (length(xvars2) > 0) {
      xlevels2 <- lapply(m2[xvars2], levels)
      xlevels2 <- xlevels2[!unlist(lapply(xlevels2, is.null))]
      if (length(xlevels2) == 0) xlevels2 <- NULL
    }
    else xlevels2 <- NULL
    
    contr.save1 <- attr(X1, "contrasts")
    contr.save2 <- attr(X2, "contrasts")
  }

  nvar1 <- ncol(X1)-1
  var.names1 <- colnames(X1)[-1]
  if(nvar1==1) X1 <- matrix(X1[,-1])
  else X1 <- X1[,-1]

  nvar2 <- ncol(X2)-1
  var.names2 <- colnames(X2)[-1]
  if(nvar2==1) X2 <- matrix(X2[,-1])
  else X2 <- X2[,-1]
  
  #number of parameters for baseline
  nbase1 <- ifelse(base.dist[1]=="logBurr", 3, ifelse(base.dist[1]=="piecewise", length(cuts)+1, 2)) 
  nbase2 <- ifelse(base.dist[2]=="logBurr", 3, ifelse(base.dist[2]=="piecewise", length(cuts)+1, 2)) 
  nbase <- c(nbase1, nbase2)
  
  #nk <- ifelse(is.null(frailty.dist),0,1)
  parms1 <- parms[[1]]
  parms2 <- parms[[2]]
  colnames(X1) <- var.names1
  vbeta1 <- parms1[(nbase1+1):(nbase1+nvar1)]
  colnames(X2) <- var.names2
  vbeta2 <- parms2[(nbase2+1):(nbase2+nvar2)]
  
  if(is.null(data$weight)) data$weight <- 1
  

  if(base.dist[1]=="lognormal"){
  if(parms1[2] <= 0) stop("The second baseline parameter for the lognormal distribution has to be > 0")
  else  parms1[1] <- exp(parms1[1])
  } 
  else if(any(parms1[1:nbase1]<=0)) stop("All baseline parameters should be > 0")
  
  if(base.dist[2]=="lognormal"){
    if(parms2[2] <= 0) stop("The second baseline parameter for the lognormal distribution has to be > 0")
    else parms2[1] <- exp(parms2[1])
  } 
  else if(any(parms2[1:nbase2]<=0)) stop("All baseline parameters should be > 0")

  
  if(frailty.dist=="none"){
    if(length(parms1) != (nvar1+nbase1) ) stop("The size of parms[[1]] is incorrect.")
    if(length(parms2) != (nvar2+nbase2) ) stop("The size of parms[[2]] is incorrect.")
    kappa <- NULL
    theta0 <- c(log(parms1[1:nbase1]), log(parms2[1:nbase2]), vbeta1, vbeta2)
    est1 <- optim(theta0, loglik_ind_c, X1=X1, X2=X2, Y1=Y1, Y2=Y2, 
                  cuts=cuts, nbase=nbase, data=data, design=design, 
                  base.dist=base.dist, agemin=agemin, control = list(maxit = 50000))
  }
  else{
    kappa <- parms[[3]] 
    theta0 <- c(log(parms1[1:nbase1]), log(parms2[1:nbase2]), vbeta1, vbeta2, log(kappa))
    if(frailty.dist%in%c("gamma", "lognormal") & length(kappa) != 2) stop("The size of parms[[3]] for kappa should be 2.")
    if(frailty.dist%in%c("cgamma", "clognormal") & length(kappa) != 3) stop("The size of parms[[3]] for kappa should be 3.")
  
    est1 <- optim(theta0, loglik_frailty_corr, X1=X1, X2=X2, Y1=Y1, Y2=Y2, 
                  cuts=cuts, nbase=nbase, data=data, design=design, 
                  base.dist=base.dist, frailty.dist=frailty.dist, 
                  agemin=agemin, control = list(maxit = 50000))
    
    }
  

  logLik <- -est1$value
    EST <- est1$par
#    Var <- try(solve(est1$hessian), TRUE)
    if(frailty.dist=="none") H <- hessian(loglik_ind_c, est1$par, X1=X1, X2=X2, Y1=Y1, Y2=Y2, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin)
    else H <- hessian(loglik_frailty_corr, est1$par, X1=X1, X2=X2, Y1=Y1, Y2=Y2, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin)
    Var <- try(solve(H), TRUE)
     
  if(!is.null(attr(Var,"class"))) stop("Model did not converge.\n  Try again with different initial values.")
  else{ 
    bparms.name1 <-  c("log.lambda1","log.rho1", "log.eta1")
    bparms.name2 <- c("log.lambda2","log.rho2", "log.eta2")
    if(base.dist[1]=="lognormal") bparms.name1[1] <- "lambda1" 
    else if(base.dist[1]=="piecewise") bparms.name1 <- paste0("log.q1_", 1:nbase1)
    if(base.dist[2]=="lognormal") bparms.name2[1] <- "lambda2" 
    else if(base.dist[2]=="piecewise") bparms.name2 <- paste0("log.q2_", 1:nbase1)
    
    x.name1 <- paste0(colnames(X1),"1")
    x.name2 <- paste0(colnames(X2),"2")
    
    parms.cov <- Var
    parms.se <- sqrt(diag(parms.cov))
    parms.cov.robust <- NULL
    parms.se.robust <- NULL
    
    if(frailty.dist=="none") parms.name <- c(bparms.name1[1:nbase1], bparms.name2[1:nbase2], x.name1, x.name2)
    else parms.name <- c(bparms.name1[1:nbase1], bparms.name2[1:nbase2], x.name1, x.name2, c("log.k1", "log.k2", "log.k0")[1:length(kappa)])
    
    names(EST) <- names(parms.se)  <- rownames(parms.cov) <- colnames(parms.cov) <- parms.name
    if(robust){
      if(frailty.dist=="none")
        grad <- jacobian(loglik_ind_c, est1$par, X1=X1, X2=X2, Y1=Y1, Y2=Y2, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, agemin=agemin, vec=TRUE)
      else
        grad <- jacobian(loglik_frailty_corr, est1$par, X1=X1, X2=X2, Y1=Y1, Y2=Y2, cuts=cuts, nbase=nbase, data=data, design=design, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin, vec=TRUE)
      
      Jscore <- t(grad)%*%grad
 		 parms.cov.robust <- Var%*%(Jscore)%*%Var
 		 parms.se.robust <- sqrt(diag(parms.cov.robust))
 		 rownames(parms.cov.robust) <- colnames(parms.cov.robust) <- parms.name
    }
  }
    
    aic = 2*length(EST) - 2*logLik
    
    out <- list(estimates = EST, varcov = parms.cov, varcov.robust = parms.cov.robust, se = parms.se, se.robust = parms.se.robust,logLik = logLik, AIC=aic)  
    class(out) <- "penmodel_c"
    attr(out, "design") <- design
    attr(out, "base.dist") <- base.dist
    attr(out, "frailty.dist") <- frailty.dist
    attr(out, "agemin") <- agemin
    attr(out, "cuts") <- cuts
    attr(out, "nbase") <- nbase
    attr(out, "data") <- data
    attr(out, "robust") <- robust
    attr(out, "formula1") <- formula1
    attr(out, "formula2") <- formula2
    attr(out, "X1") <- X1
    attr(out, "X2") <- X2
    attr(out, "Y1") <- Y1
    attr(out, "Y2") <- Y2
    invisible(out)
    
}#end
  