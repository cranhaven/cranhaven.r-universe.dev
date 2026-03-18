"ICEscale" <-
function (df, trtm, xeffe, ycost, lambda = 1, ceunit = "cost") 
{
    if (missing(df) || !inherits(df, "data.frame")) 
        stop("The first argument to ICEscale must be an existing Data Frame.")
    if (missing(trtm)) 
        stop("The Second argument to ICEscale must name the Treatment factor.")
    trtm <- deparse(substitute(trtm))
    if (!is.element(trtm, dimnames(df)[[2]])) 
        stop("Treatment factor must be an existing Data Frame variable.")
    if (length(table(df[, trtm])) != 2) 
        stop("Treatment factor must assume exactly two different levels.")
    if (missing(xeffe)) 
        stop("The Third argument to ICEscale must name the Treatment Effectiveness variable.")
    xeffe <- deparse(substitute(xeffe))
    if (!is.element(xeffe, dimnames(df)[[2]])) 
        stop("Effectiveness measure must be an existing Data Frame variable.")
    if (missing(ycost)) 
        stop("The Fourth argument to ICEscale must name the Treatment Cost variable.")
    ycost <- deparse(substitute(ycost))
    if (!is.element(ycost, dimnames(df)[[2]])) 
        stop("Cost measure must be an existing Data Frame variable.")
    if (lambda <= 0) 
        stop("The lambda argument to ICEscale must be strictly positive.")
    if (ceunit != "effe") 
        ceunit <- "cost"
    effcst <- na.omit(df[, c(trtm, xeffe, ycost)])
    if (ceunit != "cost") 
        effcst[, 3] <- effcst[, 3]/lambda
    else effcst[, 2] <- effcst[, 2] * lambda
    names(effcst) <- c("trtm", "effe", "cost")
    effcst <- effcst[do.call(order, effcst), ]
    idx <- table(as.numeric(effcst$trtm))
    rowstd <- 1:idx[1]
    rownew <- 1:idx[2] + idx[1]
    nstd <- length(rowstd)
    nnew <- length(rownew)
    t1 <- c(mean(effcst[rownew, 2]) - mean(effcst[rowstd, 2]), 
        mean(effcst[rownew, 3]) - mean(effcst[rowstd, 3]))
    s1 <- c(sqrt(var(effcst[rownew, 2])/nnew + var(effcst[rowstd, 
        2])/nstd), sqrt(var(effcst[rownew, 3])/nnew + var(effcst[rowstd, 
        3])/nstd))
    ICEsclol <- list(trtm = trtm, xeffe = xeffe, ycost = ycost, 
        effcst = effcst, lambda = lambda, ceunit = ceunit, t1 = t1, 
        s1 = s1)
    class(ICEsclol) <- "ICEscale"
    ICEsclol
}

"print.ICEscale" <-
function (x, ...) 
{
    if (missing(x) || !inherits(x, "ICEscale")) 
        stop("The first argument to print.ICEscale must be an ICEscale object.")
    cat("\nIncremental Cost-Effectiveness (ICE) Lambda Scaling Statistics\n")
    cat(paste("\nSpecified Value of Lambda   =", x$lambda))
    cat(paste("\nCost and Effe Differences are both expressed in", 
        x$ceunit, "units\n"))
    cat(paste("\nEffectiveness variable Name =", x$xeffe))
    cat(paste("\n     Cost     variable Name =", x$ycost))
    cat(paste("\n  Treatment   factor   Name =", x$trtm))
    cat(paste("\nNew treatment level is =", names(table(x$effcst[, 
        1]))[2], "and Standard level is =", names(table(x$effcst[, 
        1]))[1], "\n"))
    cat(paste("\nObserved  Treatment Diff =", round(x$t1[1], 
        digits = 3)))
    cat(paste("\nStd. Error of Trtm Diff  =", round(x$s1[1], 
        digits = 3), "\n"))
    cat(paste("\nObserved Cost Difference =", round(x$t1[2], 
        digits = 3)))
    cat(paste("\nStd. Error of Cost Diff  =", round(x$s1[2], 
        digits = 3), "\n"))
    cat(paste("\nObserved  ICE  Ratio     =", round(x$t1[2]/x$t1[1], 
        digits = 3), "\n"))
    slam <- x$s1[2]/x$s1[1]
    cat(paste("\nStatistical Shadow Price =", round(slam, digits = 3)))
    cat(paste("\nPower-of-Ten Shadow Price=", 10^(as.integer(log10(slam))), 
        "\n\n"))
}
