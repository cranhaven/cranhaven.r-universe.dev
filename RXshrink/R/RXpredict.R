"RXpredict" <- function (x, data, m="minMSE", rscale=1) 
{ 
    cond <- class(x) %in% c("qm.ridge", "eff.ridge", "aug.lars", "uc.lars", "MLcalc", "correct.signs" ) 
    if (missing(x) || cond == FALSE) { 
        cat("The First argument to RXpredict() must be an output object from one of Six RXshrink\n") 
        cat("functions: eff.ridge, qm.ridge, aug.lars, uc.lars, MLcalc or correct.signs ...\n") 
        stop() 
    } 
    if (missing(data) || !inherits(data, "data.frame")) { 
        cat("The Second argument to RXpredict must be an existing Data Frame.\n") 
        stop() 
    } 
    if (is.character(m) && m != "minMSE") { 
        cat("The Third argument to RXpredict must be either m=\"minMSE\" or a numeric m-Extent.\n") 
        stop() 
    } 
    if (rscale >=1) {rscale <- 1} else {rscale <- 0} 
    mMax <- as.numeric(x$p) 
    tsteps <- 1 
    if (inherits(x, "MLcalc")) { 
        mReq <- mMax - sum(x$dMSE) 
        if (is.numeric(m) && m != mReq) {        # Return Fitted values for OLS = BLUE. 
            bstar <- x$beta[1,]; mReq <- 0.0 
            if (m != mReq) cat("The requested m-Extent for MLcalc() has been reset to m = 0.0 for OLS.\n") 
        } 
    if (m == "minMSE" || m == mReq) bstar <- x$beta[2,]   # Return Optimally Biased Fitted values. 
    } 
    else if (inherits(x, "correct.signs")) { 
        bstar <- x$signs[,5]           # use the rescaled B(=) vector for prediction...
        mReq <- 1                      # m-Extent is > 0 but otherwise ill-defined here.
    } else { 
        if (m == "minMSE") {mReq <- x$mClk} else {mReq <- as.numeric(m)} 
        bstar <- x$coef 
        tsteps <- nrow(x$coef) - 1 
    } 
    m <- mReq      # assure "m" is a numeric scalar...
    if (m <= 0.0) { 
        cat("While m = 0.0 is a Valid request, all requests for Estimation and Prediction via OLS\n") 
        cat("ideally use the lm() function. Furthermore, predict() offers a \"newdata\" argument.\n") 
        if (m < 0.0) stop("m-Extent cannot be Negative.\n") 
    } 
    if (m >= mMax) { 
        cat("\nAll Coefficients are Shrunken to ZERO when m-Extent equals the Number of\n") 
        cat("Non-Constant X-Vars. All predictions then equal the Intercept Estimate.\n") 
        if (m > mMax) stop("m-Extent cannot exceed ", mMax, " in this regression model.\n") 
    } 
    stsize <- mMax/tsteps              # Step-Size == 1/steps in eff.ridge()
    midx <- round(m/stsize, 0)         # m-Index: "row" number == midx + 1
    mobs <- stsize*midx                # Nearest "observed" value of of m...
    lmobj <- lm(x$form, data) 
    yvec <- as.matrix(lmobj$model[, 1]) 
    n <- nrow(yvec) 
    xmat <- as.matrix(lmobj$model[,-1]) 
    if (inherits(x, "MLcalc") || inherits(x, "correct.signs")) {coef <- bstar} else {coef <- bstar[midx+1,]} 
    # bstar coefficients for specified m-extent
    mx <- matrix(apply(xmat, 2, "mean"), nrow = 1) 
    cx <- xmat - matrix(1, n, 1) %*% mx          # Centering is always applied (implicit intercept)...
    if (rscale == 1) { 
        xscale <- diag(sqrt(diag(var(cx))))      # Rescaling to equal variances & std. errors...
        crx <- cx %*% solve(xscale)              # Centered and Rescaled X-matrix...
    } 
    cry <- matrix(yvec - mean(yvec), ncol = 1)   # n x 1 ; more Centering...
    yscale <- 1 
    if (rscale == 1) { 
        yscale <- sqrt(var(cry)) 
        cry <- cry/yscale[1, 1] 
    } 
    cryprd <- crx %*% coef 
    if (inherits(x, "correct.signs") && 0 < m) { 
        cryprd <- cryprd * (mMax - m) / mMax 
        mobs <- m 
    } 
    yvecprd <- matrix( mean(yvec), n, 1) + cryprd %*% sqrt(var(yvec)) 
    if (inherits(x, "MLcalc")) mobs <- m 
    if (inherits(x, "aug.lars") || inherits(x, "uc.lars")) mobs <- x$mlik[mobs+1,1] 
    RXolist <- list(class=class(x), cryprd = as.vector(cryprd), cry = as.vector(cry), 
                    yvecprd = as.vector(yvecprd), yvec = as.vector(yvec), m = m, 
                    mobs = mobs) 
    class(RXolist) <- "RXpredict" 
    RXolist 
} 
 
"print.RXpredict" <- 
function (x, ...) 
{ 
    cat("\nRXpredict Object: In-Sample Predictions (fitted.values) for RXshrink functions...\n") 
    cat("\nClass of Estimation Method:", x$class, "\n") 
    cat("\nCentered and Rescaled y-Outcome Vector: cry =\n") 
    print.default(x$cry, quote = FALSE) 
    cat("\nFitted.Values = Predictions of cry: cryprd =\n") 
    print.default(x$cryprd, quote = FALSE) 
    cat("\nObserved y-Outcome Vector: yvec =\n") 
    print.default(x$yvec, quote = FALSE) 
    cat("\nPredictions of yvec: yvecprd =\n") 
    print.default(x$yvecprd, quote = FALSE) 
    cat("\nShrinkage m-Extent requested: m =", x$m) 
    if (x$class == "correct.signs") { 
        cat(" ...but m-Extent is NOT well-defined in \"correct.signs\" cases.\n") 
    } else { 
        cat("\n") 
    } 
    cat("\nObserved m-Extent most close to the requested m is: mobs =", x$mobs, "\n") 
} 
  
"plot.RXpredict" <- 
function (x, fit = "yvecprd", ...) 
{ 
    if (fit != "yvecprd" && fit != "cryprd") fit <- "both" 
    p <- length(x$cryprd) 
    obs <- 1:p 
    m <- x$mobs    # m-extent actually found and saved... 
    op <- par(no.readonly = TRUE) 
    if (fit == "both") {par(mfrow=c(2,1))} else {par(mfrow=c(1,1))} 
    if (fit == "both" || fit == "yvecprd") { 
        plot(obs, x$yvec, ann = FALSE, type = "n") 
        points(obs, x$yvec) 
        lines(obs, x$yvecprd, lwd=2, col="blue") 
        title(main = paste(x$class, "Predictions for m-Extent =", round(m,2)), 
            xlab = "Observation Numbers", ylab = "Observed and Predicted y-Outcomes") 
    } 
    if (fit == "both" || fit == "cryprd") { 
        plot(obs, x$cry, ann = FALSE, type = "n") 
        abline(h = 0, col = gray(0.9), lty=2, lwd = 2) 
        points(obs, x$cry) 
        lines(obs, x$cryprd, lwd=2, col="blue") 
        title(main = paste(x$class, "Fitted.Values for m-Extent =", round(m,2)), 
            xlab = "Observation Numbers", ylab = "Centered and Rescaled y-Outcomes") 
    } 
    par(op) 
} 
  
