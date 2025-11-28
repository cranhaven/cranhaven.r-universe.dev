"YonX" <- function (form, data, delmax = 0.999999) 
{ 
    if (missing(form) || !inherits(form, "formula"))
        stop("First argument to YonX() must be a valid linear regression formula.") 
    yvar <- deparse(form[[2]]) 
    xvar <- deparse(form[[3]]) 
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to YonX() must be an existing Data Frame.") 
    dfname <- deparse(substitute(data))
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response Y-variable in the YonX() formula must be an existing variable.")
    if (!is.element(xvar, dimnames(data)[[2]])) 
        stop("Regressor X-variable in the YonX() formula must be an existing variable.")
    lmobj <- lm(form, data)
    yvec <- as.matrix(lmobj$model[,1])
    xvec <- as.matrix(lmobj$model[,2])
    p <- ncol(xvec)
    if (p > 1) 
        stop("The formula must contain only 1 (non-constant) Regressor X-variable.")
    n <- length(xvec)
    if (n < 5) 
        stop("Number of observations must be at least 5.")
    mx <- mean(xvec)
    crx <- xvec - mx * matrix(1, n, 1)           # "centering" ...No "rescaling" in YonX()... 
    eigval <- as.numeric(t(crx) %*% crx)         # No use of svd()...
    sv <- sqrt(eigval)                           # Singular Value...
    cry <- yvec - mean(yvec) * matrix(1, n, 1)   # "centering" ...No "rescaling" used in YonX() 
    bvec <- as.numeric(t(crx) %*% cry) / eigval  # bvec[1] = b0 (OLS)...
    risk <- 1/eigval                             # relative risk (OLS)...
    exev <- 0                                    # no excess...
    delta <- 1                                   # no shrinkage...
    d <- 1
    cold <- 1
    ssy <- as.numeric(t(cry) %*% cry)  # scalar
    rho <- (sv * bvec)/sqrt(ssy)       # correlation (scalar)
    arho <- abs(rho)                   # scalar
    r2 <- arho^2                       # OLS "R^2" coef. of determination....
    if (r2 >= 1) 
        stop(" Maximum Likelihood Shrinkage is not applicable when RSQUARE=1.")
    res <- cry - crx %*% bvec          # residual vector when bvec = OLS...
    s2 <- as.numeric(t(res) %*% res)/(n - 2)
    varrho <- s2/ssy
    tstat <- rho/sqrt(varrho)
    frat <- tstat^2
    stat <- cbind(eigval, sv, bvec, rho, tstat)
    dimnames(stat) <- list(1, c("LAMBDA", "SV", "COMP", "RHO", "TRAT"))
    yxnam <- c(yvar,xvar)
    RXolist <- list(dfname = dfname, form = form, p = 1, n = n, 
        r2 = r2, s2 = s2, prinstat = stat, yxnam = yxnam, yvec = yvec, xvec = xvec)
    OmR2dN <- (1 - r2)/n
    dMSE <- rho^2 / (rho^2 + OmR2dN )  # ML Estimate is Biased Upwards...
    Phi2ML <- n*r2/(1-r2)              # Maximum Likelihood Estimate
    Phi2UB <- ((n-3)*r2 - 1)/(1-r2)    # Unbiased Estimate for "n" at least 5...
    Phi2CR <- max(0,Phi2UB)            # Correct Range Estimate...
    dALT <- Phi2CR / (Phi2CR + 1)
    mcal <- 0                # 1 - delta = 0...
    kinc <- 1/dMSE           # innitially > 1
    kstar <- kinc
    srat <- tstat / sv
    MCAL <- 0
    KSTAR <- kinc
    C <- Inf
    E <- Inf
    R <- Inf
    steps <- 1000  
    leng <- steps + 1
    MCAL <-  rep(MCAL, leng)
    KSTAR <- rep(KSTAR, leng)
    C <- rep(C, leng)
    E <- rep(E, leng)
    R <- rep(R, leng)
    bvec <- rep(bvec, leng)
    risk <- rep(risk, leng)
    delta <- rep(delta, leng)
    kstar <- rep(kstar,leng)
    mcal <- rep(mcal, leng)
    term <- n * log((1 - r2)/n)
    for (i in 1:steps) {
        ip <- i + 1                # values are 2:1001
        mobj <- i/steps
        iter <- kofm1(mobj, dMSE)  # internal function returns 2 values...
        kinc <- iter$kStar
        d <- iter$d
        omd <- max(1 - d, 1 - delmax)   # Strictly Positive values...
        ddomd <- d/omd
        rxi <- arho * sqrt(ddomd)
        slik <- 2/(rxi + sqrt(4 * n + rxi^2))
        clik <- 2 * n * log(slik) + ddomd - (rxi/slik) - term
        if (clik < 1 - delmax) clik <- 1 - delmax
        ebay <- frat * omd - log(omd)
        sr2d <- d * rho^2
        if( sr2d >= 1 ) {rcof <- Inf} else {
            rcof <- abs(log(omd)) + n * log((1 - sr2d)/(1 - r2))
        }
        minc <- 1 - d
        MCAL[ip] <- minc
        KSTAR[ip] <- kinc
        C[ip] <- clik
        E[ip] <- ebay
        R[ip] <- rcof
        binc <- d * bvec[1]                                # shrunken beta coeff (OLS)
        omds <- (1 - d) * srat                             # srat of line 59...
        risk[ip] <- omds^2 + (2*d - 1)/eigval
        bvec[ip] <- binc
        delta[ip] <- d
        kstar[ip] <- kinc
        mcal[ip] <- minc
    }
    mlik <- cbind(MCAL, KSTAR, C, E, R)
    dimnames(mlik) <- list(1:leng, c("M", "KSTAR", "CLIK", "EBAY", "RCOF"))
    sext <- cbind(risk, kstar, mcal)
    dimnames(sext) <- list(1:leng, c("TSMSE", "KSTAR", "MCAL"))
    mUnr <- mofk1(k=1, dMSE)
    minC <- min(mlik[,3])
    minE <- min(mlik[,4])
    minR <- min(mlik[,5])
    eqlR <- risk[1]                    # Relative Risk [1] = relative Variance of OLS estimate...
    for (ip in 2:leng) {
        fr <- (ip-1)/steps
        if (mlik[ip,3] <= minC) {mClk <- fr}  
    }
    qrsk <- risk                       # Initial values...
    idx <- 1000*round(mUnr,3) + 1
    rskO <- risk[idx]                  # Assume this RR value is approximately correct (idx != 1)...
    afac <- abs(rskO - eqlR)
    minRR <- eqlR                      # Initial value...
    for (ip in 2:leng) {               # leng = 1001...
        m <- (ip-1)/steps              # m = 0.001, 0.002, ..., 1.000
        mo <- mUnr
        qrsk[ip] <- afac*(m/mo)^2 - 2 * afac * (m/mo) + eqlR         # quadratic eqn...
        if (qrsk[ip] < minRR) {minRR <- qrsk[ip]; mRRm <- m}
        if (qrsk[ip] <= eqlR) {mReql <- m}    
    }
    exev <- risk[1] - qrsk	
    RXolist <- c(RXolist, list(coef = bvec, rmse = risk, spat = delta, qrsk = qrsk, exev = exev,
        mlik = mlik, sext = sext, mUnr = mUnr, mClk = mClk, minC = minC, minE = minE, minR = minR,
        minRR = minRR, mRRm = mRRm, mReql = mReql, Phi2ML = Phi2ML, Phi2UB = Phi2UB, dALT = dALT,
        dMSE = dMSE))
    class(RXolist) <- "YonX"
    RXolist
} 
  
"print.YonX" <- function (x, ...) 
{
    cat("\nYonX Object: Shrinkage in Simple Regression [One X-variable]\n")
    cat("Data Frame:", x$dfname, "\n")
    cat("Regression Equation [with Implicit Intercept]:\n")
    print(x$form)
    cat("\n    Number of Observations, n =", x$n, "\n")
    cat("\nPrincipal Axis Summary Statistics of Ill-Conditioning...\n")
    print.default(x$prinstat, quote = FALSE)
    cat("\n    Residual Mean Square for Error =", x$s2, "\n")
    cat("    Estimate of Residual Std. Error =", sqrt(x$s2), "\n")
    cat("\nThe extent of shrinkage (M value) most likely to be optimal\n")
    cat("depends upon whether one uses the Classical, Empirical Bayes, or\n")
    cat("Random Coefficient criterion.\n")
    if (x$r2 > 0.999) {
        cat("\n\nWARNING! R-squared exceeds 0.999;")
        cat("  Details of calculations are possibly Misleading...\n")
    }
    b0 <- x$prinstat[3]
    bm <- b0 * x$dMSE 
    cat("\n    OLS Beta Coefficient (BLUE),                bHat =", b0)
    cat("\n    Minimum MSE Risk (Optimally Biased) Beta,   bStar=", bm)
    cat("\n    Most Likely UNRestricted Shrinkage Extent,  mUnr =", x$mUnr)
    cat("\n    Corresponding Expected -2*log(Likelihood Ratio)  = 0.0")   
    cat("\n    Most Likely m-Value (three decimal places), mClk =", x$mClk)
    cat("\n    Smallest Observed -2*log(Likelihood Ratio), minC =", x$minC)
    cat("\n    Smallest Observed EBAY criterion,           minE =", x$minE) 
    cat("\n    Smallest Observed RCOF criterion,           minR =", x$minR) 
    cat("\n    Estimated NonCentrality of F-test,          Phi2 =", x$Phi2UB)  
    cat("\n    dMSE Estimate (Optimal Shrinkage Factor)    dMSE =", x$dMSE, "\n\n") 
} 
 
"plot.YonX" <- function (x, trace = "all", ...) 
{ 
    mcal <- x$sext[,3]    # MCAL values are in the 3rd column... 
    if (trace != "coef" && trace != "rmse" && trace != "spat" && trace != "lglk" && 
        trace != "seq"  && trace != "YonX" && trace != "exev") trace <- "all" 
    steps <- length(x$spat)-1  
    mO <- x$mUnr    # m-extent with k*==1 in YonX()...
    mC <- x$mClk    # m-extent with Classical Maximum Likelihood of min MSE Risk...
    mM <- x$mRRm    # m-extent of Minimum Estimated Relative Risk...
    mE <- x$mReql   # m-extent with Relative Risk equal to OLS Relative Variance at m = 0... 
    b0 <- x$prinstat[3]
    bm <- b0 * x$dMSE
    be <- b0 * (1 - mE) 
    opar <- par(no.readonly = TRUE)  
    on.exit(par(opar))
    if (trace == "all") par(mfrow=c(2,2)) else par(mfrow=c(1,1))  
    if (trace == "all" || trace == "seq" || trace == "coef") {
        plot(mcal, x$coef, ann = FALSE, type = "n")
        abline(h = 0, col = gray(0.9), lwd = 2)
        abline(v = 0,  col = "blue", lty = 2, lwd = 1)
        abline(v = mO, col = "purple", lty = 2, lwd = 1)
        abline(v = mE, col = "red", lty = 2, lwd = 1)  
        lines(mcal, x$coef, col = 1, lty = 1, lwd = 2)    
        points(0, b0, col = "blue", lwd = 2)
        points(mO, bm, col = "purple", lwd = 2)
        points(mE, be, col = "red", lwd = 2)
        title(main = paste("COEFFICIENT TRACE"),
            xlab = "m = Multicollinearity Allowance", ylab = "Fitted Coefficient")   
    }
    if (trace == "seq") {
        cat("\nPress the Enter key to view the Relative MSE trace...")  
        scan()  
    }  
    if (trace == "all" || trace == "seq" || trace == "rmse") {
        rmax <- max(x$qrsk)
        plot(mcal, x$qrsk, ann = FALSE, type = "n", ylim=c(0,rmax))
        abline(h = 0, col = gray(0.9), lwd = 2)
        abline(v = 0,  col = "blue", lty = 2, lwd = 1)
        abline(v = mC, col = "purple", lty = 2, lwd = 1)      
        abline(v = mE, col = "red", lty = 2, lwd = 1) 
        lines(mcal, x$qrsk, col = 1, lty = 1, lwd = 2)  
        points(0, x$qrsk[1], col = "blue", lwd = 2)     
        points(mC, x$qrsk[mapp(mC,x)], col = "purple", lwd = 2)
        points(mE, x$qrsk[mapp(mE,x)], col = "red", lwd = 2)  
        title(main = paste("RELATIVE MSE RISK TRACE"),
            xlab = "m = Multicollinearity Allowance", ylab = "Relative MSE Risk")  
    }
    if (trace == "seq") {
        cat("\nPress the Enter key to view the SPAT trace...")  
        scan()  
    } 
    if (trace == "all" || trace == "seq" || trace == "spat") {
        plot(mcal, x$spat, ann = FALSE, type = "n")
        abline(h = 0, col = gray(0.9), lwd = 2)    
        abline(v = 0,  col = "blue", lty = 2, lwd = 1)     
        abline(v = mC, col = "purple", lty = 2, lwd = 1)
        abline(v = mE, col = "red", lty = 2, lwd = 1)
        lines(mcal, x$spat, col = 1, lty = 1, lwd = 2)
        points(0, x$spat[1], col = "blue", lwd = 2)    
        points(mC, x$spat[mapp(mC,x)], col = "purple", lwd = 2)
        points(mE, x$spat[mapp(mE,x)], col = "red", lwd = 2)   
        title(main = paste("SHRINKAGE PATTERN TRACE"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Delta Factor") 
    }
    if (trace == "seq") {
        cat("\nPress the Enter key to view the -2 log(Likelihood Ratio) Trace...")  
        scan()  
    }
    if (trace == "all" || trace == "seq" || trace == "lglk") {
        cmax <- x$mlik[1001,3] * 1.2
        plot(mcal, x$mlik[,3], ann = FALSE, type = "n", ylim=c(0,cmax))
        abline(h = 0, col = gray(0.9), lwd = 2)     
        abline(v = mE, col = "red", lty = 2, lwd = 1)
        abline(v = mC, col="purple", lty = 2, lwd = 1) 
        lines(mcal, x$mlik[,3], col = 1, lty = 1, lwd = 2)  
        points(mC, 0, col = "purple", lwd = 2) 
        points(mE, x$mlik[mapp(mE,x),3], col = "red", lwd = 2)  
        title(main = paste("-2 log(Likelihood Ratio) TRACE"),
            xlab = "m = Multicollinearity Allowance", ylab = "-2 log(LR)")
    }		
    if (trace == "all" || trace == "seq") {
        cat("\nPress the Enter key to view the YonX Scatter plot...")  
        scan()  
    }
    if (trace == "YonX" || trace == "all" || trace == "seq") {
        par(mfrow=c(1,1))
        yvec <- x$yvec
        xvec <- x$xvec
        v1 <- matrix(1, x$n, 1)
        ym <- mean(yvec)
        xm <- mean(xvec)
        b0fit <- (ym - b0*xm)*v1 + b0*xvec
        bmfit <- (ym - bm*xm)*v1 + bm*xvec
        befit <- (ym - be*xm)*v1 + be*xvec  
        plot(xvec, yvec, xlab = paste("Xvar =", x$yxnam[2]),
            ylab = paste("Yvar =", x$yxnam[1]), main = "YonX Shrinkage Plot")
        lines(xvec, b0fit, lwd=2, col="blue")
        lines(xvec, bmfit, lwd=2, col="purple")
        lines(xvec, befit, lwd=2, col="red")
        abline(h = ym, v = xm, lty = 2)
    } 
    if (trace == "exev") {
        par(mfrow=c(1,1)) 
        plot(mcal, x$exev, ann = FALSE, type = "n") 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        abline(v = 0,  col = "blue", lty = 2, lwd = 1) 
        abline(v = mO, col = "purple", lty = 2, lwd = 1)
        abline(v = mE, col = "red", lty = 2, lwd = 1)
        lines(mcal, x$exev, col = 1, lty = 1, lwd = 2)
        points(0, x$exev[1], col = "blue", lwd = 2) 
        points(mO, x$exev[mapp(mO,x)], col = "purple", lwd = 2) 
        points(mE, x$exev[mapp(mE,x)], col = "red", lwd = 2) 
        title(main = paste("EXCESS EIGENVALUE"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Excess MSE Risk") 
    } 
}
