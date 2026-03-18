"ICEuncrt" <-
function (df, trtm, xeffe, ycost, lambda = 1, ceunit = "cost", R = 25000, seed = 0) 
{
    if (missing(df) || !inherits(df, "data.frame")) 
        stop("The first argument to ICEuncrt must be an existing Data Frame.")
    if (lambda <= 0) 
        stop("The lambda argument to ICEuncrt must be strictly positive.")
    if (ceunit != "effe") 
        ceunit <- "cost"
    ICEuncol <- list(df = deparse(substitute(df)), lambda = lambda, ceunit = ceunit, R = R)
    if (missing(trtm)) 
        stop("The Second argument to ICEuncrt must name the Treatment factor.")
    trtm <- deparse(substitute(trtm))
    if (!is.element(trtm, dimnames(df)[[2]])) 
        stop("Treatment factor must be an existing Data Frame variable.")
    if (length(table(df[, trtm])) != 2) 
        stop("Treatment factor must assume exactly two different levels.")
    if (missing(xeffe)) 
        stop("The Third argument to ICEuncrt must name the Treatment Effectiveness variable.")
    xeffe <- deparse(substitute(xeffe))
    if (!is.element(xeffe, dimnames(df)[[2]])) 
        stop("Effectiveness measure must be an existing Data Frame variable.")
    if (missing(ycost)) 
        stop("The Fourth argument to ICEuncrt must name the Treatment Cost variable.")
    ycost <- deparse(substitute(ycost))
    if (!is.element(ycost, dimnames(df)[[2]])) 
        stop("Cost measure must be an existing Data Frame variable.")
    effcst <- na.omit(df[, c(trtm, xeffe, ycost)])
    names(effcst) <- c("trtm", "effe", "cost")
    effcst <- effcst[do.call(order, effcst), ]
    if (ceunit != "cost") 
        effcst[, 3] <- effcst[, 3]/lambda
    else effcst[, 2] <- effcst[, 2] * lambda
    if (R > 25000) 
        R <- 25000
    else if (R < 50) 
        R <- 50
    idx <- table(as.numeric(effcst$trtm))
    rowstd <- 1:idx[1]
    rownew <- 1:idx[2] + idx[1]
    nstd <- length(rowstd)
    nnew <- nstd + length(rownew)
    t1 <- ICEd2m(effcst, rownew, rowstd)
    t <- matrix(rep(0, R * 2), nrow = R, ncol = 2)
    if (seed == 0)
        seed <- 1 + floor(25000 * runif(1))
    set.seed(seed)
    for (i in 1:R) {
        rowstd <- ICErunif(1, nstd)
        rownew <- ICErunif(nstd + 1, nnew)
        t[i, ] <- ICEd2m(effcst, rownew, rowstd)
    }
    icer.bias <- t1[1] / t1[2]
    icer.boot <- mean(t[,1]) / mean(t[,2])
    icer.unbi <- 2 * icer.bias - icer.boot
    ICEuncol <- c(ICEuncol, list(trtm = trtm, xeffe = xeffe, ycost = ycost, effcst = effcst, 
        t1 = t1, t = t, icer.bias = icer.bias, icer.boot = icer.boot, icer.unbi = icer.unbi,		
        seed = seed))
    class(ICEuncol) <- "ICEuncrt"
    ICEuncol
}

"plot.ICEuncrt" <-
function (x, lfact = 1, swu = FALSE, alibi = FALSE, ...) 
{
    if (missing(x) || !inherits(x, "ICEuncrt")) 
        stop("The first argument to plot(ICEuncrt) must be an ICEuncrt object.")
    if (lfact > 0) 
        lambda <- lfact * x$lambda
    else lambda <- x$lambda
    ceunit <- x$ceunit
    if (ceunit != "cost") 
        ceunit <- "effe"
    if (swu) {
        if (ceunit == "cost") { 
            ceunit <- "effe"
            if (x$lambda != 1) {
                x$t1[1] <- x$t1[1] / x$lambda
                x$t[, 1] <- x$t[, 1] / x$lambda
                x$t1[2] <- x$t1[2] / x$lambda
                x$t[, 2] <- x$t[, 2] / x$lambda
                }
            }
        else {
            ceunit <- "cost"
            if (x$lambda != 1) {
                x$t1[1] <- x$t1[1] * x$lambda
                x$t[, 1] <- x$t[, 1] * x$lambda
                x$t1[2] <- x$t1[2] * x$lambda
                x$t[, 2] <- x$t[, 2] * x$lambda
                }
            }
        }
    if (lfact != 1) {
        x$lambda <- lambda
        if (ceunit == "cost") {
            x$t1[1] <- x$t1[1] * lfact
            x$t[, 1] <- x$t[, 1] * lfact
        }
        else {
            x$t1[2] <- x$t1[2] / lfact
            x$t[, 2] <- x$t[, 2] / lfact
        }
    }
    emax <- max(abs(max(x$t[, 1])), abs(min(x$t[, 1])))
    cmax <- max(abs(max(x$t[, 2])), abs(min(x$t[, 2])))
    if (alibi == FALSE) {
        plot(x$t[, 1], x$t[, 2], ann = FALSE, type = "p", ylim = c(-cmax, 
            cmax), xlim = c(-emax, emax))
        par(lty = 1)
        abline(v = 0, h = 0)
        par(lty = 2)
        abline(v = x$t1[1], h = x$t1[2])
        par(lty = 3)
        abline(c(0, 1))
        title(main = paste("ICE Alias Uncertainty for Lambda =", 
            lambda), xlab = "Effectiveness Difference", ylab = "Cost Difference", 
            sub = paste("Units =", ceunit, ": Bootstrap Reps =", x$R))
    }
    else {
        amax <- max(emax, cmax)
        plot(x$t[, 1], x$t[, 2], ann = FALSE, type = "p", ylim = c(-amax, 
            amax), xlim = c(-amax, amax))
        par(lty = 1)
        abline(v = 0, h= 0)
        par(lty = 2)
        abline(v = x$t1[1], h = x$t1[2])
        par(lty = 3)
        abline(c(0, 1))   # Show as lambda == 1
        title(main = paste("ICE Alibi Uncertainty for Lambda =", 
            lambda), xlab = "Effectiveness Difference", ylab = "Cost Difference", 
            sub = paste("Units =", ceunit, ": Bootstrap Reps =", x$R))
    }
}

"print.ICEuncrt" <-
function (x, lfact = 1, swu = FALSE, ...) 
{
    if (missing(x) || !inherits(x, "ICEuncrt")) 
        stop("The first argument to print.ICEuncrt() must be an ICEuncrt object.")
    cat("\nIncremental Cost-Effectiveness (ICE) Bivariate Bootstrap Uncertainty\n")
    if (lfact > 0) 
        lambda <- lfact * x$lambda
    else lambda <- x$lambda
    ceunit <- x$ceunit
    if (ceunit != "cost") 
        ceunit <- "effe"
    if (swu) {
        if (ceunit == "cost") { 
            ceunit <- "effe"
            if (x$lambda != 1) {
                x$t1[1] <- x$t1[1] / x$lambda
                x$t[, 1] <- x$t[, 1] / x$lambda
                x$t1[2] <- x$t1[2] / x$lambda
                x$t[, 2] <- x$t[, 2] / x$lambda
                }
            }
        else {
            ceunit <- "cost"
            if (x$lambda != 1) {
                x$t1[1] <- x$t1[1] * x$lambda
                x$t[, 1] <- x$t[, 1] * x$lambda
                x$t1[2] <- x$t1[2] * x$lambda
                x$t[, 2] <- x$t[, 2] * x$lambda
                }
            }
        }
    cat(paste("\nShadow Price = Lambda =", lambda))
    cat(paste("\nBootstrap Replications, R =", x$R))
    cat(paste("\nEffectiveness variable Name =", x$xeffe))
    cat(paste("\n     Cost     variable Name =", x$ycost))
    cat(paste("\n  Treatment   factor   Name =", x$trtm))
    cat(paste("\nNew treatment level is =", names(table(x$effcst[,1]))[2],
        "and Standard level is =", names(table(x$effcst[,1]))[1], "\n"))
    cat(paste("\nCost and Effe Differences are both expressed in", 
        ceunit, "units\n"))
    if (lfact != 1) {
        x$lambda <- lambda
        if (ceunit == "cost") {
            x$t1[1] <- x$t1[1] * lfact
            x$t[, 1] <- x$t[, 1] * lfact
        }
        else {
            x$t1[2] <- x$t1[2] / lfact
            x$t[, 2] <- x$t[, 2] / lfact
        }
    }
    cat(paste("\nObserved  Treatment Diff =", round(x$t1[1], digits = 3)))
    cat(paste("\nMean Bootstrap Trtm Diff =", round(mean(x$t[,1]), digits = 3), "\n"))
    cat(paste("\nObserved Cost Difference =", round(x$t1[2], digits = 3)))
    cat(paste("\nMean Bootstrap Cost Diff =", round(mean(x$t[,2]), digits = 3), "\n"))
    cat(paste("\nConsistent (Biased) ICER =", round(x$icer.bias, digits = 4), "\n"))
    cat(paste("\nBootstrap Mean ICE ratio =", round(x$icer.boot, digits = 4), "\n"))
    cat(paste("\nUnbiased  ICER  estimate =", round(x$icer.unbi, digits = 4), "\n\n"))
}
