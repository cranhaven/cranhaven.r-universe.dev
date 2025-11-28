"uc.lars" <- 
function (form, data, rscale = 1, type = "lar", trace = FALSE, 
    eps = .Machine$double.eps, omdmin = 9.9e-13) 
{ 
    if (missing(form) || !inherits(form, "formula"))
        stop("First argument to uc.lars must be a valid linear regression formula.") 
    yvar <- deparse(form[[2]]) 
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to uc.lars must be an existing Data Frame.") 
    dfname <- deparse(substitute(data)) 
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response variable in the uc.lars formula must be an existing variable.") 
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
        xscale <- diag(sqrt(diag(var(crx))) )
        crx <- crx %*% solve(xscale) 
    } 
    sx <- svd(crx) 
    eigval <- matrix(sx$d^2, ncol = 1) 
    eiginv <- solve(diag(sx$d^2, ncol = p)) 
    cry <- matrix(yvec - mean(yvec), ncol = 1) 
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
    cold <- delta            # initial value for line 141...
    sv <- matrix(sx$d, p, 1) 
    ssy <- t(cry) %*% cry 
    rho <- (sv * comp)/sqrt(ssy[1, 1]) 
    arho <- matrix(abs(rho), nrow = 1) 
    r2 <- sum(arho^2) 
    if (r2 >= 1) 
        stop(" Maximum likelihood shrinkage cannot be performed when RSQUARE=1.") 
    res <- cry - crx %*% bstar 
    s2 <- t(res) %*% res/(n - p - 1) 
    varrho <- s2[1, 1]/ssy[1, 1] 
    tstat <- rho/sqrt(varrho) 
    frat <- rho^2/varrho 
    stat <- cbind(eigval, sv, comp, rho, tstat) 
    dimnames(stat) <- list(1:p, c("LAMBDA", "SV", "COMP", "RHO", 
        "TRAT")) 
    RXolist <- list(data = dfname, form = form, p = p, n = n, 
        r2 = r2, s2 = s2, prinstat = stat, gmat = sx$v) 
    larsobj <- lars(sx$u, cry, type, trace, eps) 
    bhat <- as.matrix(larsobj$beta) %*% solve(diag(sx$d, ncol = p)) %*% 
        t(sx$v) 
    if (p != length(bhat[1, ])) 
        stop("Number of coefficients for LARS and shrinkage must match.") 
    steps <- length(bhat[, 1]) 
    binc <- bstar - bhat[steps, ] 
    if (sum(binc^2) > omdmin) 
        stop("OLS coefficients for LARS and shrinkage must match.") 
    mcal <- 0 
    const <- (n - p - 3)/(n - p - 1) 
    srat <- solve(diag(as.vector(sv), ncol = p)) %*% tstat 
    MCAL <- 0 
    C <- Inf 
    E <- Inf 
    R <- Inf 
    IDhit <- 0 
    for (inc in 2:steps) { 
        binc <- bhat[steps + 1 - inc, ] 
        dinc <- (t(sx$v) %*% binc)/comp 
        for (j in 1:p) { 
            if (abs(dinc[j]) < omdmin) 
                dinc[j] <- 0 
            if (dinc[j] < 0) 
                stop("A negative Uncorrelated Component Shrinkage Factor should not occur.") 
        } 
        d <- matrix(dinc, p, p) 
        minc <- p - sum(dinc) 
        omd <- pmax(1 - dinc, omdmin) 
        ddomd <- dinc/omd 
        rxi <- sum(t(arho) * sqrt(ddomd)) 
        slik <- 2/(rxi + sqrt(4 * n + rxi^2)) 
        clik <- 2 * n * log(slik) + sum(ddomd) - (rxi/slik) - 
            n * log((1 - r2)/n) 
        ebay <- sum(frat * omd - log(omd)) 
        sr2d <- sum(dinc * rho^2) 
        rcof <- -sum(log(omd)) + n * log((1 - sr2d)/(1 - r2)) 
        C <- rbind(C, clik) 
        E <- rbind(E, ebay) 
        R <- rbind(R, rcof) 
        MCAL <- rbind(MCAL, minc) 
        vecr <- (idty - d) %*% srat 
        compr <- const * vecr %*% t(vecr) + (2 * d - idty) * 
            eiginv 
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
            if (rscale == 2) { 
                cinc <- cinc %*% xscale 
                cinc <- cinc/sqrt(sum(cinc^2)) 
            } 
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
        bstar <- yscale * solve(xscale) %*% bstar 
        risk <- yscale^2 * solve(xscale^2) %*% risk 
    } 
    mlik <- cbind(MCAL, C, E, R) 
    dimnames(mlik) <- list(0:(steps - 1), c("M", "CLIK", "EBAY", "RCOF")) 
    bstar <- t(bstar) 
    risk <- t(risk) 
    exev <- t(exev) 
    infd <- t(infd) 
    delta <- t(delta) 
    sext <- cbind(tsmse, mcal) 
    dimnames(sext) <- list(0:(steps - 1), c("TSMSE", "MCAL")) 
    minC <- min(mlik[,2]) 
    mClk <- 1/steps 
    for( i in 1:length(mlik[,2]) ) { 
        if( mlik[i,2] == minC ) { 
            mClk <- i/steps 
            break 
        } 
    } 
    RXolist <- c(RXolist, list(lars = larsobj, coef = bstar, 
        rmse = risk, exev = exev, infd = infd, spat = delta, 
        mlik = mlik, sext = sext, mClk = mClk, minC = minC)) 
    class(RXolist) <- "uc.lars" 
    RXolist 
} 
  
"plot.uc.lars" <- 
function (x, trace = "all", trkey = FALSE, ...) 
{ 
    mcal <- x$sext[, 2] 
    mcalp <- rep(mcal, times = x$p) 
    if (trace != "coef" && trace != "rmse" && trace != "exev" && 
        trace != "infd" && trace != "spat" && trace != "seq") 
        trace <- "all" 
    mV <- x$mClk    # m-extent with min Classical -2*log(Like) 
    opar <- par(no.readonly = TRUE) 
    on.exit(par(opar)) 
    myty <- c(1,2,4,5,6,7,8,9,10,11,12,3)        # KEY CHANGE: lty == 3 (dotted line) is moved to 12th position...
    if (trace == "all") 
        par(mfrow=c(3,2)) 
    else 
        par(mfrow=c(1,1)) 
    if (trace == "all" || trace == "seq" || trace == "coef") { 
        plot(mcalp, x$coef, ann = FALSE, type = "n") 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$coef[, i], col = i, lty = myty[i], lwd = 2) 
        title(main = paste("COEFFICIENT TRACE:", x$lars$type), 
            xlab = "m = Multicollinearity Allowance", ylab = "Fitted Coefficients") 
        if( trkey ) 
            legend("bottom",all.vars(x$form)[2:(x$p+1)], col=1:(x$p), lty=1:(x$p), lwd=2) 
    } 
    if (trace == "seq") { 
        cat("\nPress the Enter key to view the RMSE trace...") 
        scan() 
    } 
    if (trace == "all" || trace == "seq" || trace == "rmse") { 
        plot(mcalp, x$rmse, ann = FALSE, type = "n") 
        abline(v = mV, col = "gray", lty = 2, lwd = 2) 
        abline(h = 0, col = gray(0.9), lwd = 2) 
        for (i in 1:x$p) lines(mcal, x$rmse[, i], col = i, lty = myty[i], lwd = 2) 
        title(main = paste("RELATIVE MSE:", x$lars$type), 
            xlab = "m = Multicollinearity Allowance", ylab = "Scaled MSE Risk") 
        if( trkey ) 
            legend("bottom",all.vars(x$form)[2:(x$p+1)], col=1:(x$p), lty=1:(x$p), lwd=2) 
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
        title(main = paste("EXCESS EIGENVALUES:", x$lars$type), 
            xlab = "m = Multicollinearity Allowance", ylab = "Least Squares minus uclars") 
        if( trkey ) 
            legend("bottom",paste("Component",1:(x$p)), col=1:(x$p), lty=1:(x$p), lwd=2) 
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
        title(main = paste("INFERIOR DIRECTION:", x$lars$type), 
            xlab = "m = Multicollinearity Allowance", ylab = "Direction Cosines") 
        if( trkey ) 
            legend("bottom",all.vars(x$form)[2:(x$p+1)], col=1:(x$p), lty=1:(x$p), lwd=2) 
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
        title(main = paste("SHRINKAGE PATTERN:", x$lars$type), 
            xlab = "m = Multicollinearity Allowance", ylab = "Shrinkage Delta-Factors") 
        if( trkey ) 
            legend("bottom",paste("Component",1:(x$p)), col=1:(x$p), lty=1:(x$p), lwd=2) 
    } 
} 
  
"print.uc.lars" <- 
function (x, ...) 
{ 
    cat("\nuc.lars Object: Uncorrelated Component LARS Shrinkage\n") 
    cat("Data Frame:", x$data, "\n") 
    cat("Regression Equation:\n") 
    print(x$form) 
    cat("\n    Number of Regressor Variables, p =", x$p, "\n") 
    cat("    Number of Observations, n =", x$n, "\n") 
    cat("\nPrincipal Axis Summary Statistics of Ill-Conditioning...\n") 
    print.default(x$prinstat, quote = FALSE )
    cat("\n    Residual Mean Square for Error =", x$s2, "\n") 
    cat("    Estimate of Residual Std. Error =", sqrt(x$s2), 
        "\n\n") 
    cat("\nThe extent of shrinkage (M value) most likely to be optimal\n") 
    cat("depends upon whether one uses the Classical, Empirical Bayes, or\n") 
    cat("Random Coefficient criterion.  In each case, the objective is to\n") 
    cat("minimize the minus-two-log-likelihood statistics listed below:\n") 
    print.default(x$mlik, quote = FALSE) 
    cat("\nOutput from LARS invocation...\n") 
    print(x$lars) 
    cat("\nExtent of shrinkage statistics...\n") 
    print.default(x$sext, quote = FALSE) 
    cat("\n    Most Likely Observed MCAL Value, mClk =", x$mClk) 
    cat("\n    Minimum Classical -2*log(Like), minCLIK =", x$minC, "\n\n") 
} 
  
