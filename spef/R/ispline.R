##############################################################################
# bspBasis is a list of bspline basis parameters
#   list(df, knots, intercept=TRUE, Boundary.knots)
##############################################################################
ispline <- function(x, bspBasis) {
    n <- length(x)
    # B-Spline matrix, n * (bspBasis$df + 1)
    bspMat <- do.call("bs", c(list(x=x), bspBasis))
    breaks <- c(bspBasis$Boundary.knots[1], bspBasis$knots,
                bspBasis$Boundary.knots[2])
    idx <- as.numeric(cut(x, breaks, include.lowest=TRUE, right=FALSE)) + 3
    sqMat <- t(apply(matrix(idx), 1, function(u) seq(u, u - 3)))
    # I-Spline matrix
    ispMat <- matrix(0, n, bspBasis$df + 1)
    for (i in 1:n) {
        ispMat[i, seq(1, idx[i] - 4)] <- 1
        ispMat[i, sqMat[i, ]] <- cumsum(bspMat[i, sqMat[i, ]])
    }
    ispMat[, -1]
}

##############################################################################
# Create I-Spline function
##############################################################################
isplineFun <- function(coef, bspBasis) {
    f <- function(x) {
        c(ispline(x, bspBasis) %*% coef)
    }
    attr(f, "coef") <- coef
    attr(f, "df") <- bspBasis$df
    attr(f, "knots") <- bspBasis$knots
    attr(f, "Boundary.knots") <- bspBasis$Boundary.knots
    class(f) <- c("isplineFun", "function")
    f
}

##############################################################################
# plot I-Spline function
##############################################################################
#' @export
plot.isplineFun <- function(x, xlab="x", ylab="f(x)", main=NULL, type="l", ...) {
    bd <- attr(x, "Boundary.knots")
    xVal <- seq(bd[1], bd[2], length=101)
    yVal <- x(xVal)
    plot(xVal, yVal, xlab=xlab, ylab=ylab, main=main, type=type, ...)
    ## abline(v=attr(x, "knots"), lty="dotted", col="red")
}

##############################################################################
## bspBasis <- list(df=6, knots=c(0.3, 0.5, 0.6), intercept=TRUE, Boundary.knots=c(0, 1))
## x <- seq(0, 1, length=101)
## ispMat <- ispline(x, bspBasis)
## matplot(x, ispMat, type="l")
## abline(v=bspBasis$knots, lty="dotted", col="red")
