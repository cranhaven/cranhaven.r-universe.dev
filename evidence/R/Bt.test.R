Bt.test <- function(formula, data, plotit = TRUE) {
    if (!is.data.frame(data)) 
        stop("this function only works on a data.frame")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("      Bayesian independent samples t-test (Bernardo)\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    m <- model.frame(formula, data)
    X <- model.matrix(formula, data)
    Y <- model.response(m)
    resp.name <- as.character(formula[[2]])
    pred.name <- as.character(formula[[3]])
    pred <- m[, 2]
    n.all <- length(Y)
    if (!is.factor(pred)) 
        stop("predictor must be a factor")
    p <- length(levels(pred))
    if (p > 2) 
        stop("predictor has > 2 levels; consider using Bnmeans instead")
    mns <- tapply(Y, pred, mean)
    cat("\n  Group means:", mns, "\n")
    if (mns[2] > mns[1]) {
        cat("  mean2 > mean1, so I will relevel\n")
        nwl <- as.character(levels(pred)[2])
        pred <- relevel(pred, ref = nwl)
        mns <- tapply(Y, pred, mean)
        cat("  New group means:", round(mns, 6), "\n")
    }
    nm1 <- as.character(levels(pred)[1])
    nm2 <- as.character(levels(pred)[2])
    x1 <- Y[pred == nm1]
    x2 <- Y[pred == nm2]
    n <- length(x1)
    m <- length(x2)
    x1bar <- mean(x1)
    x2bar <- mean(x2)
    Ybar <- mean(Y)
    s <- sqrt(sum((x1 - x1bar)^2, (x2 - x2bar)^2)/(n + 
        m - 2))
    xy.std <- (c(x1 - x1bar, x2 - x2bar))/s
    th <- mean(xy.std)
    h <- (2 * n * m)/(m + n)
    t0 <- (x1bar - x2bar)/(s * sqrt(2/h))
    pi1 <- h/(4 * (m + n))
    df <- n + m - 2
    options(warn = -1)
    pi <- function(theta) {
        exp(-0.5 * log(1 + pi1 * theta^2) + try(dt(t0, 
            df, theta * sqrt((h/2)), log = TRUE), silent = TRUE))
    }
    prob <- pt(t0, df, lower.tail = FALSE)
    
    cat("===============================================\n")
    cat("Bayesian approach with reference prior (Bernardo):\n")
    int <- integrate(pi, -Inf, Inf, subdivisions = 1e+05)[[1]]
    Pi <- function(theta) pi(theta)/int
    p0 <- integrate(Pi, -Inf, 0, subdivisions = 1e+05)[[1]]
    F <- function(theta) ((n + m)/2) * log(1 + (h/(2 * 
        (n + m))) * theta^2) * Pi(theta)
    p <- integrate(F, -Inf, 0, subdivisions = 1e+05)[[1]]
    chek <- integrate(Pi, -Inf, 0)[[1]]
    IS <- integrate(F, 0, Inf, subdivisions = 1e+05)[[1]]
    BF <- exp(IS)
    B10.cat <- c("(negligible)", "(positive)", "(strong)", 
        "(very strong)")
    bound <- c(1, 3, 20, 150)
    is <- which(bound <= BF)
    is <- is[length(is)]
    IS.cat <- B10.cat[is]
    cat(" (theta is the standardized difference between the two means)\n")
    cat(" P(theta < 0 | data) =", round(chek, 3), "(reference posterior)\n")
    cat(" d(H0 | t, m, n) =", round(IS, 3), "(intrinsic test statistic)\n", 
        "Bayes factor B against H0:", round(BF, 3), 
        IS.cat, "\n")
    cat("===============================================\n")
    cat("General information, frequentist approach:\n")
    cat(" n(1) =", m, "   n(2) =", n, "\n")
    cat(" mean(1) =", round(x1bar, 3), "; mean(2) =", 
        round(x2bar, 3), "; grand mean =", round(Ybar, 
            3), "\n", "s_pooled =", round(s, 3), "; t =", 
        round(t0, 3), "; df =", df, "\n", "P(t.test) =", 
        round(2 * prob, 3), "(two-sided)\n")
    if (plotit) {
        # determine appropriate plotting range
        xr <- seq(-3, 3, 0.01)
        yr <- Pi(xr)
        maxy <- max(yr)
        miny <- min(yr)
        yrng <- maxy - miny
        xrp <- xr[yr > yrng/100]
        k <- min(xrp)
        kk <- max(xrp)
        xrng <- c(k, kk)
        ## now plot
        plot(seq(k, kk, 0.01), Pi(seq(k, kk, 0.01)), 
            ty = "l", xlab = "theta", ylab = "posterior density", 
            xlim = xrng)
        title("Reference posterior of theta | data")
        abline(h = 0)
        if (k >= 0) {
            incr <- -0.01
            xx <- c(k, seq(k, 0, incr), 0)
            yy <- c(0, Pi(seq(k, 0, incr)), 0)
        } else {
            incr <- 0.01
            xx <- c(k, seq(k, 0, incr), 0)
            yy <- c(0, Pi(seq(k, 0, incr)), 0)
        }
        polygon(xx, yy, col = "gray")
    }
    options(warn = 0)
}
