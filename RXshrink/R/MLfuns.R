"MLboot" <-  
function (form, data, reps=100, seed, rscale=1) 
{ 
    if (missing(form) || !inherits(form, "formula")) 
        stop("First argument to MLboot must be a valid linear regression formula.")
    yvar <- deparse(form[[2]])
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to MLboot must be an existing Data Frame.")
    dfname <- deparse(substitute(data))
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response variable in the MLboot formula must be an existing variable.")
    if (reps < 10) reps <- 10
    if (reps > 10000)
        cat("\nMore than 10,000 MLboot replications are NOT recommended...")
    if (missing(seed)) seed <- 1 + floor(1000 * runif(1))
    set.seed(seed)
    if (rscale != 1) rscale <- 2
    dmat <- as.matrix(data)
    samp <- dmat
    n <- length(data[,1])
    for (j in 1:reps) {
      if (j == 1) dfsam <- data        # use the given data rows...
      else {
        sam <- sample(1:n, replace=T)
        for (i in 1:n ) {
          samp[i,] <- dmat[sam[i],]    # sample from the given rows with replacement...
          } 
        dfsam <- data.frame(samp)	
      }
      out <- MLcalc(form, dfsam, rscale)
      if (out[1] == "Rsq1") {
        cat("Rsq1 exception at step: j =", j,"\n")
        sam <- sample(1:n, replace=T)
        for (i in 1:n ) {
          samp[i,] <- dmat[sam[i],]    # sample again from the given rows with replacement...
          } 
        dfsam <- data.frame(samp)	
        out <- MLcalc(form, dfsam, rscale)       # Second attempt at j-th sample...
        if (out[1] == "Rsq1") break              # Second consecutive attempt also failed...
        }	  
      if (j == 1) {
        ols.beta <- out$beta[1,]
        ols.rmse <- out$rmse[1,]
        opt.dmse <- out$dMSE
        opt.beta <- out$beta[2,]
        opt.rmse <- out$rmse[2,]
      }
      else {
        ols.beta <- rbind(ols.beta, out$beta[1,])
        ols.rmse <- rbind(ols.rmse, out$rmse[1,])
        opt.dmse <- rbind(opt.dmse, out$dMSE)
        opt.beta <- rbind(opt.beta, out$beta[2,])
        opt.rmse <- rbind(opt.rmse, out$rmse[2,])
      }
    }
    p <- out$p
    Totreps <- length(opt.beta[,1])
    RXolist <- list(data = dfname, form = form, reps = Totreps, seed = seed,
        p = p, n = n)
    RXolist <- c(RXolist, list(ols.beta = ols.beta, ols.rmse = ols.rmse, 
        opt.dmse = opt.dmse, opt.beta = opt.beta, opt.rmse = opt.rmse))
    class(RXolist) <- "MLboot" 
    RXolist 
} 
  
"print.MLboot" <-
function (x, ...) 
{
    cat("\nMLboot Object: Resampling of data.frame Observations WITH Replacement...\n")
    cat("Data Frame:", x$data, "\n")
    cat("Regression Equation:\n")
    print(x$form)
    cat("\n    Number of Replications,       reps =", x$reps, "\n")
    cat("    Random Number Seed Value,     seed =", x$seed, "\n")
    cat("    Number of Predictors,            p =", x$p, "\n")
    cat("    Number of Observations,          n =", x$n, "\n\n")
    cat("OLS Beta Coefficient matrix            = ols.beta\n")
    cat("ML Optimally Biased Coefficient matrix = opt.beta\n")
    cat("OLS Relative MSE Risk matrix           = ols.rmse\n")
    cat("ML Optimally Biased Coefficient matrix = opt.rmse\n") 		
    cat("ML Shrinkage Delta-factor matrix       = opt.dmse\n\n")	
} 
  
"MLhist" <- 
function(x, comp="opt.beta", xvar=1, npct=95, bins=50) 
{ 
  if (missing(x) || !inherits(x, "MLboot")) 
    stop("\nFirst argument to MLhist must be an existing MLboot object.\n\n")
  if (comp != "ols.beta" && comp != "ols.rmse" && comp != "opt.rmse" && 
    comp != "opt.dmse") comp <- "opt.beta" 
  if (xvar <= 1) xvar <- 1 
  if (xvar > x$p) xvar <- x$p 
  if (npct < 66) npct <- 66 
  if (npct > 100) npct <- 100 
  npct <- round(npct) 
  xout <- paste0("x$", comp, "[,", xvar, "]")
  xv <- eval(str2lang(xout))
  xlng <- length(xv)
  xost <- xv[order(xv)]                # xv order-statistics...
  noin <- round(xlng * npct / 100)     # chosen number of order-statistics
  nlo <- 1 + round((xlng-noin)/2)
  nup <- xlng - nlo + 1
  hst <- hist(xost[nlo:nup], breaks=bins, main = paste("Histogram of a MLboot Distribution"),
    xlab = paste(npct, "% of MLboot order-statistics"))
  abline(v=xv[1], col="blue", lwd=2, lty=2)  # Add location of observed sample estimate...
  RXolist <- list(x = xout, npct = npct, rbins = bins, dbins=hst$breaks, ntot = xlng, p = x$p,
    nlo = nlo, nup = nup, noin = noin, xmn = xv[1])
  class(RXolist) <- "MLhist"
  RXolist 
} 
  
"print.MLhist" <- 
function (x, ...) 
{ 
    cat("\nMLhist Object: Characteristics of the displayed MLboot() Distribution...\n") 
    cat("\n    MLboot() Distribution x$comp[,xvar]        : ", x$x, "\n")
    cat("\n    Percentage of Distribution Displayed, npct =", x$npct, "\n")
    cat("    Number of Histogram Bins requested,  rbins =", x$rbins, "\n")
    cat("    Number of Histogram Bins displayed,  dbins =", x$dbins, "\n")
    cat("    Total Number of Estimates available,  ntot =", x$ntot, "\n")
    cat("    First Order-Statistic Included,        nlo =", x$nlo, "\n")
    cat("    Last Order-Statistic Included,         nup =", x$nup, "\n")
    cat("    Number of >>Middle<< Estimates shown, noin =", x$noin, "\n")
    cat("    Observed Mean-Value of All Estimates,  xmn =", x$xmn, "\n\n")
} 
  
"MLcalc" <- 
function (form, data, rscale = 1) 
{ 
    if (missing(form) || !inherits(form, "formula")) 
        stop("First argument to MLcalc must be a valid linear regression formula.")
    yvar <- deparse(form[[2]])
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to MLcalc must be an existing Data Frame.")
    dfname <- deparse(substitute(data))
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response variable in the MLcalc formula must be an existing variable.")
    if (rscale != 1) rscale <- 2  # just two rscale options from eff.ridge()
    lmobj <- lm(form, data)
    yvec <- as.matrix(lmobj$model[, 1])
    xmat <- as.matrix(lmobj$model[, 2:length(lmobj$model)])
    p <- ncol(xmat)
    if (p < 2) 
        stop("Number on non-constant regressor variables must be at least 2.")
    n <- nrow(xmat)
    if (n != nrow(yvec)) 
        stop("Numbers of observations in XMAT and YVEC must match.")
    if (n < p + 4) 
        stop("Number of observations must exceed number of regressors by at least 4.")
    mx <- matrix(apply(xmat, 2, "mean"), nrow = 1)
    crx <- xmat - matrix(1, n, 1) %*% mx
    xscale <- diag(sqrt(diag(var(crx))))
    crx <- crx %*% solve(xscale)
    sx <- svd(crx) # sx object is Singular Value Decomposition of Centered X-matrix
    eigval <- matrix(sx$d^2, ncol = 1) # p x 1
    eiginv <- rep(0, p)                # p x 1
    svainv <- rep(0, p)                # p x 1
    for (i in 1:p) {
      if (eigval[i] > .Machine$double.eps) {
        eiginv[i] <- 1/eigval[i]
        svainv[i] <- sqrt(eiginv[i])
        } 
    } 
    eiginv <- diag(eiginv, ncol = p)   # p x p
    svainv <- diag(svainv, ncol = p)   # p x p
    cry <- matrix(yvec - mean(yvec), ncol = 1) # n x 1
    yscale <- sqrt(var(cry))
    cry <- cry/yscale[1, 1]
    smse <- sx$v %*% eiginv %*% t(sx$v)
    risk <- diag(smse)
    tsmse <- sum(risk)
    comp <- svainv %*% t(sx$u) %*% cry
    betaols <- sx$v %*% comp
    exev <- matrix(0, p, 1)
    infd <- matrix(0, p, 1)
    delta <- matrix(1, p, 1)
    idty <- diag(p)
    d <- idty
    sv <- matrix(sx$d, p, 1)
    ssy <- t(cry) %*% cry
    rho <- (sv * comp)/sqrt(ssy[1, 1])
    arho <- matrix(abs(rho), nrow = 1)
    r2 <- sum(arho^2)     # OLS "R^2" statistic....
    if (r2 >= 1) {
        cat("\nMaximum Likelihood Shrinkage is not applicable when RSQUARE=1.\n")
        return("Rsq1") 
        } 
    res <- cry - crx %*% betaols
    s2 <- t(res) %*% res/(n - p - 1)
    varrho <- s2[1, 1]/ssy[1, 1]
    tstat <- rho/sqrt(varrho)
    frat <- rho^2/varrho
    stat <- cbind(eigval, sv, comp, rho, tstat)
    dimnames(stat) <- list(1:p, c("LAMBDA", "SV", "COMP", "RHO", "TRAT"))
    RXolist <- list(data = dfname, form = form, p = p, n = n, 
        r2 = r2, s2 = s2, prinstat = stat, gmat = sx$v)
    OmR2dN <- (1 - r2 )/n
    dMSE <- rep(1,p)         # meaningless initial values...
    for( i in 1:p ) { 
        dMSE[i] <- (rho[i])^2 / ( (rho[i])^2 + OmR2dN )    # Maximum-Likelihood shrinkage factors...
    } 
    kinc <- 1                          # kinc*dMSE values then have minimum MSE risk...
    dFact <- (n - p - 3)/(n - p - 1) 
    srat <- solve(diag(as.vector(sv), ncol = p)) %*% tstat 
    mext <- p - sum(dMSE)    # Not needed or used below...                                 
    dinc <- dMSE	         # Vector of p Optimal Shrinkage-Factors...
    diag( d ) <- dMSE 
    omd <- 1 - dinc          # Strictly Positive values...
    ddomd <- dinc/omd                  # Diagonal Elements...
    rxi <- sum(arho * sqrt(ddomd))
    slik <- 2/(rxi + sqrt(4 * n + rxi^2))
    clik <- 2 * n * log(slik) + sum(ddomd) - (rxi/slik) - 
        n * log((1 - r2)/n)
    if (clik < 0) clik <- 0
    minc <- p - sum(dinc)
    bstar <- sx$v %*% d %*% comp       # MLbeta
    vecr <- (idty - d) %*% srat
    compr <- dFact * vecr %*% t(vecr) + (2 * d - idty) * eiginv
    diagc <- diag(diag(compr), ncol = p)
    lowr <- eiginv * d^2
    maxd <- matrix(pmax(diagc, lowr), p, p)
    compr <- compr - diagc + maxd
    smse <- sx$v %*% compr %*% t(sx$v)
    rinc <- diag(smse)
    lowb <- sx$v %*% lowr %*% t(sx$v)
    rinc <- pmax(rinc, diag(lowb))
    tinc <- sum(rinc)
    emse <- eiginv - compr
    sfac <- min(abs(diag(emse)))/100
    if (sfac < 1e-05) sfac <- 1e-05
    eign <- eigen(emse/sfac)
    einc <- sort(eign$values) * sfac   # Increasing order; negative values first...
    cinc <- matrix(0, p, 1)
    if (is.na(einc[1])) einc[1] <- 0
    if (einc[1] < 0) {  	
        eign$vectors <- sx$v %*% eign$vectors
        cinc <- eign$vectors[, p]
        if (rscale == 2) {
            cinc <- cinc %*% xscale
            cinc <- cinc/sqrt(sum(cinc^2)) 
        } 
    } 
    bstar <- t(cbind(betaols, bstar))
    risk <- t(cbind(risk, rinc))
    if (rscale == 2) {
        bstar <- as.double(yscale) * (bstar %*% solve(xscale)) 
        risk <- as.double(yscale^2) * (risk %*% solve(xscale^2)) 
    } 
    RXolist <- c(RXolist, list(beta = bstar, rmse = risk, dMSE = dMSE, ys = yscale, xs = diag(xscale)))  
    class(RXolist) <- "MLcalc" 
    RXolist 
} 
  
"print.MLcalc" <- 
function (x, ...) 
{ 
    cat("\nMLcalc Object: Most Likely Coefficient Estimates under Normal-Theory\n")
    cat("Data Frame:", x$data, "\n")
    cat("Regression Equation:\n")
    print(x$form)
    cat("\n    Number of Regressor Variables, p =", x$p, "\n")
    cat("    Number of Observations, n =", x$n, "\n")
    cat("\nPrincipal Axis Summary Statistics of Ill-Conditioning...\n")
    print.default(x$prinstat, quote = FALSE)
    cat("\n    Residual Mean Square for Error =", x$s2, "\n")
    cat("    Estimate of Residual Std. Error =", sqrt(x$s2), "\n")
    cat("\nOLS Beta Coefficients =\n", x$beta[1,], "\n")
    cat("\nML Optimally Biased Coefficients =\n", x$beta[2,], "\n") 
    cat("\nOLS Relative MSE Risks =\n", x$rmse[1,], "\n")
    cat("\nML Minimum Relative Risks =\n", x$rmse[2,], "\n") 
    cat("\ndMSE Estimates =\n", x$dMSE, "\n\n") 
} 
  
"MLtrue" <-  
function (form, data, seed, go=TRUE, truv, trub, truc) 
{ 
    if (missing(form) || !inherits(form, "formula"))
        stop("First argument to MLtrue must be a valid linear model formula.")
    yvar <- deparse(form[[2]])
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to MLtrue must be an existing Data Frame.")
    dfname <- deparse(substitute(data))
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response y-variable in the MLtrue() <formula> must be within the input data.frame.")
    lmobj <- lm(form, data)
    yvec <- as.matrix(lmobj$model[, 1])
    mnam <- dimnames(lmobj$model)[[2]]
    ynam <- mnam[1]
    xmat <- as.matrix(lmobj$model[, 2:length(lmobj$model)])
    xnam <- mnam[2:length(mnam)]
    p <- ncol(xmat)
    if (p < 2) {
        cat("\nNumber on non-constant regressor variables must be at least 2.\n")
        return("p_less_than_2")        
    }
    n <- nrow(xmat)
    if (n != nrow(yvec)) 
        stop("Numbers of observations in XMAT and YVEC must match.")
    if (n < p + 4) {
        cat("\nNumber of observations must exceed number of regressors by at least 4.\n")
        return("n_less_than_p+4")   
    }
    mx <- matrix(apply(xmat, 2, "mean"), nrow = 1)
    crx <- xmat - matrix(1, n, 1) %*% mx
    xscale <- diag(sqrt(diag(var(crx))))
    crx <- crx %*% solve(xscale)
    sx <- svd(crx) # sx object is Singular Value Decomposition of Centered X-matrix
    eigval <- matrix(sx$d^2, ncol = 1) # p x 1
    eiginv <- rep(0, p)                # p x 1
    svainv <- rep(0, p)                # p x 1
    for (i in 1:p) {
      if (eigval[i] > .Machine$double.eps) {
        eiginv[i] <- 1/eigval[i] 
        svainv[i] <- sqrt(eiginv[i]) 
        } 
    } 
    eiginv <- diag(eiginv, ncol = p)   # p x p
    svainv <- diag(svainv, ncol = p)   # p x p
    cry <- matrix(yvec - mean(yvec), ncol = 1) # n x 1
    yscale <- sqrt(var(cry))
    cry <- cry/yscale[1, 1]
    smse <- sx$v %*% eiginv %*% t(sx$v)
    risk <- diag(smse)
    tsmse <- sum(risk)
    comp <- svainv %*% t(sx$u) %*% cry
    betaols <- sx$v %*% comp
    sv <- matrix(sx$d, p, 1)
    ssy <- t(cry) %*% cry
    rho <- (sv * comp)/sqrt(ssy[1, 1])
    r2 <- sum(rho^2)     # OLS "R^2" statistic....
    if (r2 >= 1) { 
        cat("\nMaximum Likelihood Shrinkage is not applicable when RSQUARE=1.\n") 
        return("Rsq_is_1") 
    } 
    res <- cry - crx %*% betaols
    s2 <- t(res) %*% res/(n - p - 1)
    varrho <- s2[1, 1]/ssy[1, 1]
    tstat <- rho/sqrt(varrho)
    frat <- rho^2/varrho
    stat <- cbind(eigval, sv, comp, rho, tstat)
    dimnames(stat) <- list(1:p, c("LAMBDA", "SV", "COMP", "RHO", "TRAT"))
    RXolist <- list(data = dfname, form = form, p = p, n = n, 
        r2 = r2, s2 = s2, prinstat = stat, gmat = sx$v)              
    RXolist <- c(RXolist, list(beta = betaols, comp = comp, rmse = risk, ys = yscale, xs = diag(xscale)))  
    class(RXolist) <- "MLtrue"	
    if (!go) { 
        cat("\nPrint the MLtrue output.list to View (default) parameter values...\n\n")
        return(RXolist) 
    } 
    else { 
        tvar  <- s2          # Set defaults...
        tbeta <- betaols 
        tcomp <- comp 
        useb <- FALSE 
        if (!missing(truv) && truv >= 0) tvar <- truv 
        if (!missing(trub)) { 
            useb <- TRUE 
            if(length(trub) == p) tbeta <- as.matrix(trub, p, 1)
        } 
        if (!useb && !missing(truc)) {  # ignore truc when useb==TRUE
            if(length(truc) == p) tcomp <- as.matrix(truc, p, 1)
        } 
    } 
    tsig <- sqrt(tvar)
    if (useb) {
        Yhat <- crx %*% tbeta
    } 
    else { 
        Yhat <- crx %*% sx$v %*% tcomp   
    } 
    if (missing(seed)) 
        seed <- sample(1001, 1) - 1 
    set.seed(seed) 
    Yvec <- Yhat + as.matrix(rnorm(n, sd=tsig), n, 1)
    new <- data.frame(cbind(Yvec, Yhat, crx, cry))
    names(new) <- c("Yvec", "Yhat", xnam, ynam)
    RXolist <- c(list(new = new, Yvec = Yvec, Yhat = Yhat, seed = seed, tvar = tvar,
        tbeta = tbeta, tcomp = tcomp, useb = useb), RXolist)
    class(RXolist) <- "MLtrue" 
    RXolist 
} 
  
"print.MLtrue" <- 
function (x, ...) 
{ 
    cat("\nMLtrue Object: OLS Beta-Coefficient Estimates and Related Statistics...\n") 
    cat("Data Frame:", x$data, "\n") 
    cat("Regression Equation:\n") 
    print(x$form) 
    cat("\n    Number of Regressor Variables, p =", x$p, "\n")
    cat("    Number of Observations, n =", x$n, "\n")
    cat("\nPrincipal Axis Summary Statistics of Ill-Conditioning...\n")
    print.default(x$prinstat, quote = FALSE)
    cat("\nOLS Residual Mean Square for Error = Estimate of truv =\n", x$s2, "\n")
    cat("\nOLS Beta Coefficients   =\n", x$beta, "\n")
    cat("\nUncorrelated Components =  (COMP-column Above)\n", x$comp, "\n")
    if (length(x$new) > 0) {
        cat("\nRandom Number SEED value  =", x$seed, "\n") 
        cat("\nTrue error Variance, tvar =", x$tvar, "\n") 
        if (x$useb) { 
            cat("\nTrue OLS Beta Coefficients, tbeta =\n", x$tbeta, "\n") 
            } 
        else cat("\nTrue Uncorrelated Components, tcomp =\n", x$tcomp, "\n") 
    } 
    cat("\n") 
} 
  
