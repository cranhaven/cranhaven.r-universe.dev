## Internal functions :
##   doPanelFitPlot.stepfun
##   doPanelFitPlot.stepfun.stepfun
##   doPanelFitPlot.isplineFun
## Export functions :
##   plot.panelReg
##   print.panelReg

##############################################################################
doPanelFitPlot.stepfun <- function(baseline, timeGrid, baselineSE, ...) {
    plot(baseline, do.points=FALSE, xlab="", ylab = "", ...)
    ##        main="Cumulative Baseline Mean", ...)
    title(xlab = "Time", ylab = expression(hat(Lambda[0])(t)), line = 2, cex.lab = 1)
}

##############################################################################
doPanelFitPlot.stepfun.se <- function(baseline, timeGrid, baselineSE,
                                      baselineMat = NULL, method =NULL,...) {
    y <- baseline(timeGrid)
    if (method == "SC") {
        low <- pmin(2 * y - apply(baselineMat, 2, function(x) quantile(x, .025)), 1)
        high <- pmax(2 * y - apply(baselineMat, 2, function(x) quantile(x, .975)), 0)
    } else {
        low <- y * exp(- 1.96 * baselineSE / y)
        high <- y * exp(1.96 * baselineSE / y)
    }
    lowFun <- stepfun(timeGrid, c(0, low))
    highFun <- stepfun(timeGrid, c(0, high))
    plot(highFun, do.points = FALSE, lty = 2, xlab = "", ylab = "", ...)
    ## main = "Cumulative Baseline Mean")
    title(xlab = "Time", ylab = expression(hat(Lambda[0])(t)), line = 2, cex.lab = 1)
    plot(baseline, do.points = FALSE, add = TRUE, ...)
    plot(lowFun, do.points = FALSE, lty=2, add = TRUE)
}

##############################################################################
doPanelFitPlot.isplineFun <- function(baseline, timeGrid, baselineSE, ...) {
    plot(baseline, xlab = "", ylab = "", ...)
    ## main = "Cumulative Baseline Mean (I-Spline)", ...)
    title(xlab = "Time", ylab = expression(hat(Lambda[0])(t)), line = 2, cex.lab = 1)    
}

##############################################################################
doPanelFitPlot.isplineFun.se <- function(baseline, timeGrid, baselineSE, ...) {
    y <- baseline(timeGrid)
    low <- y * exp(- 1.96 * baselineSE / y)
    high <- y * exp(1.96 * baselineSE / y)
    plot(baseline, xlab = "", ylab = "", ## main = "Cumulative Baseline Mean (I-Spline)",
         ylim = c(0, 1.05 * max(high)), ...)
    title(xlab = "Time", ylab = expression(hat(Lambda[0])(t)), line = 2, cex.lab = 1)
    points(timeGrid, high, type="l", lty=2)
    points(timeGrid, low, type="l", lty=2)
}

##############################################################################
## Method dispatch
##############################################################################
setGeneric("doPanelFitPlot",
           function(baseline, timeGrid, baselineSE, baselineMat, method, ...) {
               standardGeneric("doPanelFitPlot")
           })

setOldClass(c("stepfun", "function"))

setMethod("doPanelFitPlot",
          signature(baseline="stepfun", baselineSE="NULL"),
          doPanelFitPlot.stepfun)

setMethod("doPanelFitPlot",
          signature(baseline="stepfun", baselineSE="numeric"),
          doPanelFitPlot.stepfun) ## doPanelFitPlot.stepfun.se)

setOldClass(c("isplineFun", "function"))

setMethod("doPanelFitPlot",
          signature(baseline="isplineFun", baselineSE="NULL"),
          doPanelFitPlot.isplineFun)

setMethod("doPanelFitPlot",
          signature(baseline="isplineFun", baselineSE="numeric"),
          doPanelFitPlot.isplineFun) ## doPanelFitPlot.isplineFun.se)

##############################################################################
## Plot a PanelReg object
##############################################################################
#' Plot a \code{panelReg} Object.
#'
#' Plot the estimated baseline mean function. If \code{"se"} option of
#'   \code{panelReg} is not \code{"NULL"}, 95% point wise confidence
#'   interval is also plotted.
#'
#' @param x The result of a call to the \code{panelReg} function.
#' @param ... Other graphical parameters such as line type, color, or axis labels.
#'
#' @seealso \code{\link{panelReg}} \code{\link{panelReg.object}}
#' @export
#' @example inst/examples/ex_plotPanelReg.R
plot.panelReg <- function(x, ...) {
    doPanelFitPlot(baseline = x$baseline, timeGrid = x$timeGrid,
                   baselineSE = x$baselineSE, baselineMat = x$baselineMat,
                   method = x$method,...)
}

##############################################################################
## Print a panelReg object
##############################################################################
#' @export
print.panelReg <- function(x, digits=max(options()$digits - 4, 3), ...) {
    savedig <- options(digits = digits)
    on.exit(options(savedig))
    coef <- x$beta
    se <- sqrt(diag(x$betaVar))
    if(all(dim(se) == 0) & !is.null(dim(se))) se <- rep(NA, length(coef))
    ## Print results
    cat("\n")
    cat("Call:\n")
    dput(x$call)
    cat("\n")
    if (!is.null(x$beta)) {
        tmp <- data.frame(coef, exp(coef), se,
                          z = coef/se, p = signif(1 - pchisq((coef/ se)^2, 1), digits - 1))
        dimnames(tmp) <- list(names(coef), c("coef", "exp(coef)", "se(coef)", "z", "Pr(>|z|)"))
        printCoefmat(tmp, dig.tst=max(1, min(5, digits)))
    } else {cat("Null model")}
    cat("\n")
    invisible()
}

##############################################################################
## Print coef(panelReg)
##############################################################################
#' @export
coef.panelReg <- function(object, ...) {
    if (class(object) != "panelReg") stop("Most be panelReg class")
    if (is.null(object$beta)) return(NULL)
    else return(object$beta)
}

##############################################################################
## Print vcov(panelReg)
##############################################################################
#' @export
vcov.panelReg <- function(object, ...) {
    if (class(object) != "panelReg") stop("Most be panelReg class")
    if (is.null(object$betaVar)) {
        return(NULL)
    } else {
        if (is.null(dim(object$betaVar))) names(object$betaVar) <- names(object$beta)
        else colnames(object$betaVar) <- rownames(object$betaVar) <- names(object$beta)
        return(object$betaVar)
    }
}
