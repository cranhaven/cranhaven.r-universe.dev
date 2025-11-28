"eff.ridge" <-
function (form, data, rscale = 1, steps = 20, ...) 
{ 
    if (missing(form) || !inherits(form, "formula"))
        stop("First argument to eff.ridge must be a valid linear regression formula.") 
    yvar <- deparse(form[[2]])
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to eff.ridge must be an existing Data Frame.") 
    dfname <- deparse(substitute(data)) 
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response variable in the eff.ridge formula must be an existing variable.") 
    lmobj <- lm(form, data) 
    yvec <- as.matrix(lmobj$model[, 1]) 
    xmat <- as.matrix(lmobj$model[, 2:length(lmobj$model)]) 
    if (rscale < 1) 
        rscale <- 0 
    if (rscale > 1) 
        rscale <- 2 
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
    xscale <- diag(p) 
    if (rscale >= 1) {
        xscale <- diag(sqrt(diag(var(crx))))
        crx <- crx %*% solve(xscale) 
    } 
    sx <- svd(crx) # sx object is Singular Value Decomposition of Centered X-matrix 
    eigval <- matrix(sx$d^2, ncol = 1)         # p x 1
    eiginv <- solve(diag(sx$d^2, ncol = p))    # p x p
    cry <- matrix(yvec - mean(yvec), ncol = 1) # n x 1
    yscale <- 1 
    if (rscale >= 1) { 
        yscale <- sqrt(var(cry)) 
        cry <- cry/yscale[1, 1] 
    } 
    smse <- sx$v %*% eiginv %*% t(sx$v) 
    risk <- diag(smse) 
    tsmse <- sum(risk) 
    comp <- solve(diag(sx$d, ncol = p)) %*% t(sx$u) %*% cry 
    bstar <- sx$v %*% comp 
    exev <- matrix(0, p, 1) 
    infd <- as.numeric(matrix(NA, p, 1)) 
    delta <- matrix(1, p, 1) 
    idty <- diag(p) 
    d <- idty 
    cold <- delta 
    sv <- matrix(sx$d, p, 1) 
    ssy <- t(cry) %*% cry 
    rho <- (sv * comp)/sqrt(ssy[1, 1]) 
    arho <- matrix(abs(rho), nrow = 1) 
    r2 <- sum(arho^2)     # OLS "R^2" statistic....
    if (r2 >= 1) 
        stop(" Maximum Likelihood Shrinkage is not applicable when RSQUARE=1.") 
    res <- cry - crx %*% bstar 
    s2 <- t(res) %*% res/(n - p - 1) 
    varrho <- s2[1, 1]/ssy[1, 1] 
    tstat <- rho/sqrt(varrho) 
    frat <- rho^2/varrho 
    stat <- cbind(eigval, sv, comp, rho, tstat) 
    dimnames(stat) <- list(1:p, c("LAMBDA", "SV", "COMP", "RHO", "TRAT")) 
    RXolist <- list(dfname = dfname, form = form, p = p, n = n, 
        r2 = r2, s2 = s2, prinstat = stat, rscale = rscale, data = data, gmat = sx$v) 
    OmR2dN <- (1 - r2 )/n 
    dMSE <- rep(1,p)      # meaningless initial values...
    mMSE <- p             # Maximum possible value...
    for (i in 1:p) { 
        dMSE[i] <- (rho[i])^2 / ( (rho[i])^2 + OmR2dN )    # Maximum-Likelihood estimate...
        mMSE <- mMSE - dMSE[i] 
    }
    const <- (n - p - 3)/(n - p - 1) 
    srat <- solve(diag(as.vector(sv), ncol = p)) %*% tstat
    mcal <- 0
    MCAL <- 0 
    C <- Inf 
    E <- Inf 
    R <- Inf 
    maxinc <- p * steps      # Number of Lattice Steps with "m" strictly > ZERO.
    IDhit <- 0               # Inferior Direction "hit" switch...
    for (inc in 1:maxinc) { 
        meobj <- inc/steps 
        iter <- meff(meobj, p, dMSE)  # Function defined below...
        minc <- iter$meobj 
        d <- iter$d          # Diagonal Matrix, p x p...
        dinc <- diag(d)      # Zero Off-Diagonal Elements...
        omd <- 1 - dinc      # Strictly Positive value...
        ddomd <- dinc/omd    # Diagonal Elements...
        rxi <- sum(arho * sqrt(ddomd)) 
        slik <- 2/(rxi + sqrt(4 * n + rxi^2)) 
        clik <- 2 * n * log(slik) + sum(ddomd) - (rxi/slik) - 
            n * log((1 - r2)/n) 
        if (clik < 0) clik <- 0 
        ebay <- sum(frat * omd - log(omd)) 
        sr2d <- sum(dinc * rho^2) 
        if( sr2d >= 1 ) { rcof <- Inf } else { 
            rcof <- -sum(log(omd)) + n * log((1 - sr2d)/(1 - r2)) }
        MCAL <- rbind(MCAL, minc) 
        C <- rbind(C, clik) 
        E <- rbind(E, ebay) 
        R <- rbind(R, rcof) 
        binc <- sx$v %*% d %*% comp 
        vecr <- (idty - d) %*% srat 
        compr <- const * vecr %*% t(vecr) + (2 * d - idty) * eiginv 
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
        if (sfac < 1e-05) 
            sfac <- 1e-05 
        eign <- eigen(emse/sfac) 
        einc <- sort(eign$values) * sfac         # Increasing order; negative values first...
        cinc <- as.numeric(matrix(NA, p, 1)) 
        if (is.na(einc[1])) 
            einc[1] <- 0 
        if (einc[1] < 0) {
            eign$vectors <- sx$v %*% eign$vectors 
            cinc <- eign$vectors[, p] 
            if (IDhit > 0 && t(cold) %*% cinc < 0) cinc <- -1 * cinc 
            IDhit <- 1 
            cold <- cinc 
        } 
        bstar <- cbind(bstar, binc) 
        risk <- cbind(risk, rinc) 
        exev <- cbind(exev, einc) 
        infd <- cbind(infd, cinc) 
        delta <- cbind(delta, dinc) 
        tsmse <- rbind(tsmse, tinc) 
        mcal <- rbind(mcal, minc) 
    } 
    if (rscale == 2) { 
        bstar <- yscale[1,1] * solve(xscale) %*% bstar 
        risk <- yscale[1,1]^2 * solve(xscale^2) %*% risk 
    } 
    mlik <- cbind(MCAL, C, E, R) 
    dimnames(mlik) <- list(0:maxinc, c("M", "CLIK", "EBAY", "RCOF")) 
    bstar <- t(bstar) 
    risk <- t(risk) 
    exev <- t(exev) 
    infd <- t(infd) 
    delta <- t(delta) 
    sext <- cbind(tsmse, mcal) 
    dimnames(sext) <- list(0:maxinc, c("TSMSE", "MCAL"))
    mStar <- p - sum(dMSE) 
    minC <- min(mlik[,2]) 
    for( i in 1:maxinc ) { 
        if( mlik[i,2] <= minC ) { 
            mClk <- (i-1)/steps 
            break 
        } 
    } 
    RXolist <- c(RXolist, list(coef = bstar, rmse = risk, exev = exev, 
        infd = infd, spat = delta, mlik = mlik, sext = sext, mStar = mStar, 
        mMSE = mMSE, mClk = mClk, minC = minC, dMSE = dMSE)) 
    class(RXolist) <- "eff.ridge" 
    RXolist 
}

"print.eff.ridge" <-
function (x, ...) 
{ 
    cat("\neff.ridge Object: Shrinkage via the Efficient PATH...\n" )
    cat("Data Frame:", x$dfname, "\n") 
    cat("Regression Equation:\n") 
    print(x$form) 
    cat("\n    Number of Regressor Variables, p =", x$p, "\n") 
    if (x$p > 20) cat("    Traces for more that p = 30 variables cannot be plotted.\n")
    cat("    Number of Observations, n =", x$n, "\n") 
    cat("\nPrincipal Axis Summary Statistics of Ill-Conditioning...\n") 
    print.default(x$prinstat, quote = FALSE) 
    cat("\n    Residual Mean Square for Error =", x$s2, "\n") 
    cat("    Estimate of Residual Std. Error =", sqrt(x$s2), "\n") 
    cat("\nThe extent of shrinkage (M value) most likely to be optimal\n") 
    cat("depends upon whether one uses the Classical, Empirical Bayes, or\n") 
    cat("Random Coefficient criterion.  In each case, the objective is to\n") 
    cat("minimize the minus-two-log-likelihood statistics listed below:\n") 
    print.default(x$mlik, quote = FALSE) 
    cat("\nExtent of Shrinkage Statistics...\n") 
    print.default(x$sext, quote = FALSE) 
    if (x$r2 > 0.999) { 
        cat("\n\nWARNING! R-squared exceeds 0.999;") 
        cat("  Details of calculations are possibly Misleading...\n") 
    } 
    cat("\n    Most Likely UNRestricted Shrinkage Extent, mStar =", x$mStar) 
    cat("\n    Corresponding Expected -2*log(Likelihood Ratio)  = 0.0") 
    cat("\n    Most Likely m-Value on Observed Lattice,    mClk =", x$mClk) 
    cat("\n    Smallest Observed -2*log(Likelihood Ratio), minC =", x$minC) 
    cat("\n    dMSE Estimates =", x$dMSE, "\n\n") 
}

"plot.eff.ridge" <-
function (x, trace = "all", LP = 0, HH = 0, ...) 
{
    trkey <- FALSE
    if (LP != 0) {
        trkey <- TRUE
        LT <- c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center")
        LG <- LT[B19(LP)]
    }
    if (x$p > 30)
	        stop("Number of x-variables exceeds 30; No Trace plots attempted...")
    myty <- c(1,2,4,5,6,7,8,9,10,11,12,3,1,2,4,5,6,7,8,9,10,11,12,1,2,4,5,6,7,8)  # postpones use of lty=3 "dotted" assuming p <= 30			
    mcal <- x$sext[,2]    # MCAL values are in the 2nd column...  
    mcalp <- rep(mcal, times = x$p)  
    if (trace != "coef" && trace != "rmse" && trace != "exev" && 
        trace != "infd" && trace != "spat" && trace != "seq")
        trace <- "all" 
    mV <- x$mStar    # Optimal m-extent in eff.ridge()... 
    opar <- par(no.readonly = TRUE)
    if (HH == 0) on.exit(par(opar)) 
    if (HH == 1) par(mfrow=c(2,1))  	
    if (trace == "all") par(mfrow=c(3,2)) else if (HH < 1)  par(mfrow=c(1,1))
    if (trace == "all" || trace == "seq" || trace == "coef") { 
        plot(mcalp, x$coef, ann = FALSE, type = "n") 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$coef[, i], col = i, lty = myty[i], lwd = 2) 
        title(main = paste("COEFFICIENT TRACE"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Fitted Coefficients") 
        if(trkey) legend(LG, all.vars(x$form)[2:(x$p+1)], col=1:(x$p), lty=myty[1:(x$p)], lwd=2) 
    } 
    if (trace == "seq") {
        cat("\nPress the Enter key to view the RMSE trace...") 
        scan() 
    } 
    if (trace == "all" || trace == "seq" || trace == "rmse") {
        plot(mcalp, x$rmse, ann = FALSE, type = "n") 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$rmse[, i], col = i, lty = myty[i], 
            lwd = 2) 
        title(main = paste("RELATIVE MSE"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Scaled MSE Risk") 
        if (trkey) legend(LG, all.vars(x$form)[2:(x$p+1)], col=1:(x$p), lty=myty[1:(x$p)], lwd=2) 
    } 
    if (trace == "seq") { 
        cat("\nPress the Enter key to view the EXEV trace...") 
        scan() 
    } 
    if (trace == "all" || trace == "seq" || trace == "exev") { 
        plot(mcalp, x$exev, ann = FALSE, type = "n") 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$exev[, i], col = i, lty = myty[i], lwd = 2) 
        title(main = paste("EXCESS EIGENVALUES"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Least Squares minus EffGRR") 
        if (trkey) legend(LG, paste("Component", 1:(x$p)), col=1:(x$p), lty=myty[1:(x$p)], lwd=2) 
    } 
    if (trace == "seq") { 
        cat("\nPress the Enter key to view the INFD trace...") 
        scan() 
    } 
    if (trace == "all" || trace == "seq" || trace == "infd") { 
        plot(mcalp, x$infd, ann = FALSE, type = "n", ylim = c(-1,1)) 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$infd[, i], col = i, lty = myty[i], lwd = 2) 
        title(main = paste("INFERIOR DIRECTION"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Direction Cosines") 
        if(trkey) legend(LG, all.vars(x$form)[2:(x$p+1)], col=1:(x$p), lty=myty[1:(x$p)], lwd=2) 
    } 
    if (trace == "seq") { 
        cat("\nPress the Enter key to view the SPAT trace...") 
        scan() 
    } 
    if (trace == "all" || trace == "seq" || trace == "spat") { 
        plot(mcalp, x$spat, ann = FALSE, type = "n") 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$spat[, i], col = i, lty = myty[i], lwd = 2) 
        title(main = paste("SHRINKAGE PATTERN"), 
            xlab = "m = Multicollinearity Allowance", ylab = "Shrinkage Delta-Factors") 
        if(trkey) legend(LG, paste("Component", 1:(x$p)), col=1:(x$p), lty=myty[1:(x$p)], lwd=2) 
    }
}

