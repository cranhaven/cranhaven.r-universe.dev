#' Periodic B-Spline Basis for Polynomial Splines
#'
#' Generate the periodic B-spline basis matrix for a polynomial spline.
#'
#' @param x the predictor variable.  Missing values are allowed.
#' @param df degrees of freedom; one can specify 'df' rather than 'knots'; 'pbs()'
#' then chooses 'df - 1' knots at suitable quantiles of 'x' (which will ignore missing values).
#' @param knots the _internal_ breakpoints that define the spline. The number of internal knots must be greater than or equal to degree polynomial regression. See also 'Boundary.knots'.
#' @param degree degree of the piecewise polynomial-default is 3 for cubic splines.
#' @param intercept if 'TRUE', an intercept is included in the basis; default is 'FALSE'
#' @param Boundary.knots boundary points at which to set the period of the perodic
#' B-spline basis(default the range of the data). If both 'knots' and 'Boundary.knots'
#' are supplied, the basis parameters do not depend on 'x'. Data CAN NOT be extended
#' beyond 'Boundary.knots'. Typical Bourday knots are start and end values of period.
#'
#' @details This function and documentation is copied from the CRAN package
#' \href{https://CRAN.R-project.org/package=pbs}{pbs}
#' by Shuangcai Wang <\email{swang1@@gmail.com}>
#'
#' @return A matrix of dimension 'length(x) * (df)', where either 'df' was supplied or if
#' 'knots' were supplied, 'df = length(knots) + intercept'. Attributes are returned that
#' correspond to the arguments to 'pbs', and explicitly give the 'knots', 'Boundary.knots'
#' etc for use by 'predict.pbs()'.
#'
#' pbs()' is based on the function 'spline.des()' in package splines. It generates a
#' basis matrix for representing the family of piecewise polynomials with the specified
#' interior knots and degree, evaluated at the values of 'x'. A primary use is in modeling
#' formulas to directly specify a piecewise polynomial term in a model.
#'
#' @examples
#' require(stats); require(graphics); require(splines)
#' x = seq(1,628)/100
#' z = rep(seq(1, 314)/100, 2)
#'
#' pbs(x, df = 5, Boundary.knots = c(0, 2*pi))
#' pbs(x, knots=c(pi/2, pi, pi*3/2), Boundary.knots = c(0, 2*pi))
#' #### example of one periodic functions
#' y= sin(x) + cos(2*x) +
#' rnorm(628, 0, 0.1) ## x has a period of 2*pi
#' ## df method, need to use large enough df to get a better fit.
#' ## May use max loglik to choose optimal df
#' summary( fm1 <- lm(y ~ pbs(x, df = 10, Boundary.knots = c(0, 2*pi))) )
#' plot(x, y, xlab = "x", ylab = "sin(x)", pch="x", cex=.5)
#'
#' lines(x, predict(fm1, data.frame(x=x, z=z)), col='blue')
#' lines(x, sin(x) + cos(2*x), col='red')
#'
#' ## knots methods, usually selected at turning points
#' summary( fm2 <- lm(y ~ pbs(x, knots=c(pi/2, pi, pi*3/2),
#'           Boundary.knots = c(0, 2*pi)))
#' )
#' plot(x, y, xlab = "x", ylab = "sin(x)", pch="x", cex=.5)
#'
#' lines(x, predict(fm2, data.frame(x=x, z=z)), col='blue')
#' lines(x, sin(x) + cos(2*x), col='red')
#'
#' #### example of two periodic functions
#' x0 = seq(1,628, by=4)/100
#' z0 = seq(1, 314, by=3)/100
#' x = rep(x0, each=length(z0))
#' z = rep(z0, length(x0))
#' y = sin(x) + cos(2*z) +
#'    rnorm(length(x), 0, 0.1) ## x has a period of 2*pi and z of pi
#'
#' summary( fm3 <- lm(y ~ pbs(x, df = 5, Boundary.knots = c(0, 2*pi))+
#'                        pbs(z, df = 5, Boundary.knots = c(0, pi)))
#' )
#'
#' plot(sin(x) + cos(2*3), predict(fm3, data.frame(x=x, z=3)))
#' summary(sin(x) + cos(2*3)- predict(fm3, data.frame(x=x, z=3)))
#' ## End(Not run)
#'
#' @export
pbs <- function (x, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
                 Boundary.knots = range(x)) {
    periodic <- TRUE
    nx <- names(x)
    x <- as.vector(x)
    nax <- is.na(x)
    if (nas <- any(nax))
        x <- x[!nax]
    if (!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        outside <- (ol <- x < Boundary.knots[1L]) | (or <- x >
            Boundary.knots[2L])
    }
    else outside <- FALSE
    ord <- 1 + (degree <- as.integer(degree))
    if (ord <= 1)
        stop("'degree' must be integer >= 1")

    if (periodic == TRUE){
        if (!missing(df) && missing(knots)) {
            nIknots <- df - 1 + (1 - intercept)
            if (nIknots < 0) {
                nIknots <- 0
                warning("'df' was too small; have used  ", ord -
                    (1 - intercept))
            }
            knots <- if (nIknots > 0) {
                knots <- seq.int(from = 0, to = 1, length.out = nIknots +
                    2)[-c(1, nIknots + 2)]
                stats::quantile(x[!outside], knots)
            }
        }
    } else {
        if (!missing(df) && missing(knots)) {
            nIknots <- df - ord + (1 - intercept)
            if (nIknots < 0) {
                nIknots <- 0
                warning("'df' was too small; have used  ", ord -
                    (1 - intercept))
            }
            knots <- if (nIknots > 0) {
                knots <- seq.int(from = 0, to = 1, length.out = nIknots +
                    2)[-c(1, nIknots + 2)]
                stats::quantile(x[!outside], knots)
            }
        }
    }
    ## get periodic B-spline knots
    getPeriodBsplineKnots = function(knots, degree=3){
        knots = sort(knots)
        nKnots = length(knots)
        if (degree < 1){
            stop("'degree' must be integer >= 1")
        }

        if (nKnots - 2 <  degree) {
            stop("number of internal knots(no boudary) must be greater than or equal to 2+ degree")
        }
        closeKnots = knots
        for (i in 1:degree){
            closeKnots = c(closeKnots, knots[nKnots] + knots[i+1] - knots[1])
        }

        for (i in 1:degree){
            closeKnots = c(knots[1] - knots[nKnots] + knots[nKnots-i], closeKnots)
        }
        return(closeKnots)
    }

    if (periodic){
        Aknots <- getPeriodBsplineKnots(sort(c(Boundary.knots, knots)), degree=degree ) ## no repeat boundary knots 3 more time
        if (any(outside)) {
            stop("some 'x' values beyond boundary knots may cause ill-conditioned bases")
        }

        basisInterior <- splines::spline.des(Aknots, x, ord)$design
        basisInteriorLeft = basisInterior[,1:degree, drop = FALSE]
        basisInteriorRight = basisInterior[,(ncol(basisInterior)-degree+1):ncol(basisInterior), drop = FALSE]
        #print(basisInteriorLeft + basisInteriorRight)
        basis = cbind(basisInterior[,-c(1:degree, (ncol(basisInterior)-degree+1):ncol(basisInterior)), drop = FALSE],  basisInteriorLeft + basisInteriorRight)
        #df = df - degree
    } else {
        Aknots <- sort(c(rep(Boundary.knots, ord), knots))
        if (any(outside)) {
            warning("some 'x' values beyond boundary knots may cause ill-conditioned bases")
            derivs <- 0:degree
            scalef <- gamma(1L:ord)
            basis <- array(0, c(length(x), length(Aknots) - degree -
                1L))
            if (any(ol)) {
                k.pivot <- Boundary.knots[1L]
                xl <- cbind(1, outer(x[ol] - k.pivot, 1L:degree,
                    "^"))
                tt <- splines::spline.des(Aknots, rep(k.pivot, ord), ord,
                    derivs)$design
                basis[ol, ] <- xl %*% (tt/scalef)
            }
            if (any(or)) {
                k.pivot <- Boundary.knots[2L]
                xr <- cbind(1, outer(x[or] - k.pivot, 1L:degree,
                    "^"))
                tt <- splines::spline.des(Aknots, rep(k.pivot, ord), ord,
                    derivs)$design
                basis[or, ] <- xr %*% (tt/scalef)
            }
            if (any(inside <- !outside))
                basis[inside, ] <- splines::spline.des(Aknots, x[inside],
                    ord)$design
        }
        else basis <- splines::spline.des(Aknots, x, ord)$design

    }
    if (!intercept)
        basis <- basis[, -1L, drop = FALSE]
    n.col <- ncol(basis)
    if (nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax, ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1L:n.col)
    a <- list(degree = degree, knots = if (is.null(knots)) numeric(0L) else knots,
        Boundary.knots = Boundary.knots, intercept = intercept, periodic = periodic)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("pbs", "basis")
    basis

}


predict.pbs <- function(object, newx, ...)
{
    if(missing(newx))
        return(object)
    a <- c(list(x = newx), attributes(object)[
                c("degree", "knots", "Boundary.knots", "intercept", "periodic")])
    do.call("pbs", a)
}



makepredictcall.pbs <- function(var, call)
{
    if(as.character(call)[1L] != "pbs") return(call)
    at <- attributes(var)[c("degree", "knots", "Boundary.knots", "intercept", "periodic")]
    xxx <- call[1L:2]
    xxx[names(at)] <- at
    xxx
}

