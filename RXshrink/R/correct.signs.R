"correct.signs" <- 
function (form, data) 
{ 
    if (missing(form) || !inherits(form, "formula"))
        stop("First argument to correct.signs must be a valid linear regression formula.")
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to correct.signs must be an existing Data Frame.")
    yvar <- deparse(form[[2]])
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response variable in the correct.signs formula must be an existing variable.")
    dfname <- deparse(substitute(data))
    lmobj <- lm(form, data)
    yvec <- as.matrix(lmobj$model[, 1])
    xmat <- as.matrix(lmobj$model[, 2:length(lmobj$model)])
    p <- ncol(xmat)
    if (p < 2) 
        stop("Number on non-constant regressor variables must be at least 2.")
    n <- nrow(xmat)
    if (n < p + 4) 
        stop("Number of observations must exceed number of regressors by at least 4.")
    mx <- matrix(apply(xmat, 2, "mean"), nrow = 1)
    crx <- xmat - matrix(1, n, 1) %*% mx   # centered xmat
    xscale <- diag(sqrt(diag(var(crx))))
    crx <- crx %*% solve(xscale)           # centered and rescaled xmat
    sx <- svd(crx)
    eigval <- matrix(sx$d^2, ncol = 1)
    eiginv <- solve(diag(sx$d^2, ncol = p))
    cry <- matrix(yvec - mean(yvec), ncol = 1)  # centered yvec
    yscale <- sqrt(var(cry))
    cry <- cry/yscale[1, 1]                # centered and rescaled yvec
    smse <- sx$v %*% eiginv %*% t(sx$v)    # G Lambda^-1 G' = Scaled (Relative) MeanSqError
    risk <- diag(smse)
    tsmse <- sum(risk)
    comp <- solve(diag(sx$d, ncol = p)) %*% t(sx$u) %*% cry    # Uncorrelated Components
    bstar <- sx$v %*% comp                 # Gc = OLS Beta Coefficients (Delta = I)
    sv <- matrix(sx$d, p, 1)               # Singular Values vector
    ssy <- sum(cry^2)                      # y'y = n-1 ...centered & rescaled
    rho <- (sv * comp)/sqrt(ssy)           # Principal Correlations vector
    arho <- matrix(abs(rho), nrow = 1)     # Absolute Principal Correlations
    r2 <- sum(arho^2)                      # Squared Multiple Correlation (R^2)
    if (r2 >= 1) 
        stop(" STOP: Rsquare = 1 here.")
    res <- cry - crx %*% bstar             # OLS Residual Vector
    s2 <- sum(res^2)/(n - p - 1)           # Unbiased sigma^2 Estimate
    varrho <- s2/ssy
    tstat <- rho/sqrt(varrho)              # Component t-Statistics
    frat <- rho^2/varrho
    stat <- cbind(eigval, sv, comp, rho, tstat)
    dimnames(stat) <- list(1:p, c("LAMBDA", "SV", "COMP", "RHO", "TRAT"))
    RXolist <- list(data = dfname, form = form, p = p, n = n, 
        r2 = r2, s2 = s2, prinstat = stat)
    xpy = t(crx) %*% cry          # X'y ...centered & rescaled
    cxpy = xpy/(n-1)              # Correlation form of X'y
    kpb = sum(rho^2/sv^2)*n/((1+(n-1)*r2)*ssy)   # Max. Like. k-factor rescaling xpy
    # Formula for kpb uses a correction to Obenchain(1978), (4.2), when ssy differs from 1.
    delp = kpb * eigval           # B(=) "shrinkage" factors 
    betap = kpb * xpy     # B(=): most likely to have minimum MSE risk parallel to beta...
    xbp = crx %*% betap           # B(=) predictions of cry
    bmf = sum(xbp*cry)/sum(xbp^2) # multiplicative factor to minimize residual SS...
    bpfit = bmf * betap   # Bfit: rescaling of B(=) yielding best fit to cry...	
    stat <- cbind(bstar, cxpy, delp, betap, bpfit)   # redefine stat matrix 
    dimnames(stat) <- list(1:p, c("OLS", "X'y", "Delta", "B(=)", "Bfit"))
    RXolist <- c(RXolist, list(kpb = kpb, bmf = bmf, signs = stat))
    r2ols <- sum((cry - crx %*% bstar)^2)
    r2bpp <- sum((cry - crx %*% betap)^2)
    r2bpf <- sum((cry - crx %*% bpfit)^2)
    stat <- cbind(r2ols, r2bpp, r2bpf)   # redefine stat matrix
    dimnames(stat) <- list("RSS", c("OLS", "B(=)", "Bfit"))
    RXolist <- c(RXolist, list(loff = stat))
    sqcols <- cor(cry, crx %*% bstar)^2  # squared correlations
    sqcxpy <- cor(cry, crx %*% cxpy)^2
    stat <- cbind(sqcols, sqcxpy)   # redefine stat matrix 
    dimnames(stat) <- list("Rsq", c("OLS", "Bfit")) 
    RXolist <- c(RXolist, list(sqcor = stat)) 
    class(RXolist) <- "correct.signs" 
    RXolist 
} 
  
"print.correct.signs" <-
function (x, ...) 
{ 
    cat("\ncorrect.signs Object: Eliminate Wrong-Signs due to Ill-Conditioning.") 
    cat("\n        B(=) Estimate with Minimum Risk Parallel to True Beta.\n\n") 
    cat("Data Frame:", x$data, "\n") 
    cat("Regression Equation:\n" )
    print(x$form) 
    cat("\n    Number of Regressor Variables, p =", x$p, "\n")
    cat("    Number of Observations, n =", x$n, "\n")
    cat("\nPrincipal Axis Summary Statistics of Ill-Conditioning...\n")
    print.default(x$prinstat, quote = FALSE)
    cat("\n    Residual Mean Square for Error =", x$s2, "\n")
    cat("    Estimate of Residual Std. Error =", sqrt(x$s2), "\n")
    cat("\nComparison of Beta Coefficient Statistics...\n")
    print.default(x$signs, quote = FALSE)
    cat("\n    OLS Beta estimate uses No Shrinkage: Delta = I.\n")
    cat("    X'y is expressed here as Correlations.\n")
    cat("    B(=) Delta 'Shrinkage' Factors are proportional to LAMBDAs\n")
    if( x$signs[2,3] > 1.0)
	    cat("    Note that B(=) uses 'Shrinkage' Factors > 1 here.\n")
    if( x$signs[1,3] > 1.0 && x$signs[2,3] <= 1.0)
	    cat("    Note that B(=) uses a 'Shrinkage' Factor > 1 here.\n")
    cat("    B(=) also uses a Common Rescaling k-Factor =", x$kpb, "here.\n")
    cat("    Bfit estimate parallel to B(=) has minimum Residual Sum-of-Squares.\n")
    cat("    Multiplicative B(=) Factor yielding Bfit estimate =", x$bmf, "\n")
    cat("\nResidual Sum-of-Squares: Lack-of-Fit...\n")
    print.default(x$loff, quote = FALSE)
    cat("\nSquared Correlation between response and its predictions...\n")
    print.default(x$sqcor, quote = FALSE) 
    cat("\n") 
} 
  
