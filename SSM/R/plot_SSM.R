#' Plot smooth supersaturated model main effects
#'
#' \code{plot.SSM} is a plot method for SSM objects. It plots the main effects
#' of the SSM only, that is the subset of basis terms that are dependent on a
#' single variable only.  For single variable data this is a plot of the
#' complete model.
#'
#' For each variable, the effect is plotted over \eqn{[-1, 1]} by default
#' although passing an alternate range to the \code{xlim} argument will override
#' this.
#'
#' The \code{yrange} argument is designed to automatically compute the relevant
#' plot range for each effect. By default a \code{ylim} value is passed to
#' \code{plot} that covers the range of responses. "full" results in a
#' \code{ylim} value that covers the range of predictions or, if appropriate,
#' the range of the metamodel error credible interval.
#'
#' For single variable data, setting \code{GP} to TRUE will plot a credible
#' interval for the metamodel error estimating Gaussian process if this has been
#' computed for the SSM object.
#'
#' @param x An SSM object.
#' @param ... (optional) arguments to pass to the \code{plot} function.
#' @param grid (optional) A number. This specifies the resolution of the plot,
#'   \emph{i.e.} how many model evaluations are used to construct the curve.
#' @param yrange (optional) Character. Only "full" will have an effect.
#' @param GP (optional) Logical. For single variable data, the credible interval
#'   of the metamodel error estimator will be plotted if TRUE.
#' @export
#' @examples
#' # A single variable example
#' X <- seq(-1, 1, 0.25)
#' Y <- sapply(X, "^", 3)
#' s <- fit.ssm(X, Y, GP = TRUE)
#' plot(s)
#'
#' # A six variable example
#' data(attitude)
#' X <- transform11(attitude[ 2:7])
#' Y <- attitude[ , 1]
#' s <- fit.ssm(X, Y)
#' plot(s)
plot.SSM <- function(x, ..., grid = 200, yrange = "full", GP = TRUE){
  # plots main effects for each variable over [-1,1] when x is an SSM object
  # if ssm is univariate, will plot the ssm with GP metamodel error if computed
  # arguments:
  #     grid:    specifies the resolution of the plot.
  #     GP:      GP=TRUE will plot metamodel GP for univariate data
  #     yrange:  "full" will fix ylim so entire GP bounds are shown for
  #              univariate case, or will show entire response on main effects
  #              plots. Alternatively, ylim can be explicitly set as an argument
  #              in ... and will override any set behaviour for yrange.
  #     ...:     arguments passed on to plot (overrides default behaviour).
  #              These do not effect the plotting of design points though.
  arguments <- list(...)
  if (yrange[1] == "response") yrange <- range(x@response)
  if(is.null(arguments$xlim))
    arguments$xlim <- c(-1,1)
  arguments$type <- "l"

  if (x@dimension == 1){
    z <- seq(arguments$xlim[1], arguments$xlim[2], length.out = grid)
    y <- c()
    for (i in 1:grid) y[3 * i - 2:0] <- as.numeric(predict(x, z[i]))
    y <- matrix(y, ncol=3, byrow=TRUE)
    if (yrange[1] == "full")
      yrange <- range(y)
    if(is.null(arguments$ylim))
      arguments$ylim <- yrange
    arguments$x <- cbind(z, y[,2])
    do.call(plot, arguments)
    #    plot(
    #         x, y[,1],
    #         xlim = c(-1,1),
    #        # ylim = range(y, na.rm = TRUE),
    #         ylim = yrange,
    #         type="l", lty="dashed"
    #         )

    ## The following use dashed lines for CI
    #    points(x, y[,1], type = "l", lty = "dashed")
    #    points(x, y[,3], type = "l", lty = "dashed")

    ## The following uses grey shading for CI
    polygon(c(z, rev(z)), c(y[,1], rev(y[,3])), border = NA, col = "lightgray")
    lines(z, y[,2])

    points(x@design[1:x@design_size,], x@response, pch=16)
  } else {

    if (length(x@main_sobol) < 1)
      x <- update.sensitivity(x)

    z <- seq(arguments$xlim[1], arguments$xlim[2], length.out = grid)
    zz <- matrix(rep(z, x@dimension), ncol=x@dimension)
    X <- construct.dmm(x@basis, zz, x@P)
    main_theta <- matrix(rep(x@theta, x@dimension),
                         ncol = x@dimension) * x@main_ind
    res <- X %*% main_theta
    par(
      mfrow = c(
        ceiling(sqrt(x@dimension)),
        ceiling(x@dimension / ceiling(sqrt(x@dimension)))
      ),
      mar = c(1,2,1,1)
    )
    for (i in 1:x@dimension){
      arguments$x <- cbind(z, res[ , i] + x@theta[1])
      if (yrange == "full"){
        arguments$ylim <- range(res[ , i] + x@theta[1])
      } else {
        arguments$ylim <- range(x@response)
      }
      do.call(plot, arguments)
      points(x@design[ , i], x@response, pch = 16)
    }
  }
}


#' Point prediction of smooth supersaturated models.
#'
#' This method gives the prediction of an SSM object at a point. If the
#' SSM has a metamodel error estimate then a \eqn{(1 - \alpha)} credible
#' interval is also output.
#'
#' @param object An SSM object.
#' @param x A \eqn{d} length vector identifying the prediction point.
#' @param alpha (optional) A number in \eqn{[0, 1]} for the \eqn{(1 - \alpha)}
#'   metamodel error estimate credible interval. Set to \code{0.05} by default.
#' @param ... further arguments passed to or from other methods.
#' @return Either a number if the SSM has no metamodel error estimating Gaussian
#' process, or three numbers giving the model prediction (\code{$model}), and
#' the lower and upper bounds of the credible interval (\code{$lower} and
#' \code{$upper}) respectively.
#' @export
#' @examples
#' data(attitude)
#' X <- transform11(attitude[ 2:7])
#' Y <- attitude[ , 1]
#' # with no metamodel error estimating GP.
#' s <- fit.ssm(X, Y)
#' predict(s, rep(1,6))
#'
#' # with metamodel error estimating GP.
#' s <- fit.ssm(X, Y, GP = TRUE)
#' predict(s, rep(1,6))
predict.SSM <- function (object, x, alpha=0.05, ...){
  # uses the ssm to predict an observation at point x
  # arguments:
  #   object:      the "ssm" fitted model.
  #   x:        the prediction location
  #   alpha:    Set the alpha value for the prediction interval if a metamodel
  #             GP has been computed.

  if(object@fail == TRUE) stop("The SSM must be fit to data before it can be used
                            for prediction.\n")

  if (object@dimension == 1){
    X <- (rep(x, object@basis_size) ^ t(object@basis)) %*% object@P
  } else {
    X <- matrix (rep (x, object@basis_size), nrow = object@dimension)
    X <- (X ^ t(object@basis)) %*% object@P
    X <- apply (X, 2, prod)
  }

  fit   <- sum(X %*% object@theta)
  bound <- 0

  if (length(object@r)>0){
    s <- object
    s@design <- rbind(x, object@design)
    new.dist <- new.distance(ssm = s, type = object@distance_type)[1, -1]
    if(sum(new.dist == 0) > 0)
      return(list(lower = fit, model = fit, upper = fit))
    new.cov  <- compute.covariance.from.distance(new.dist, object@r, object@type, object)
    V <- object@sigma * (1 - sum(diag(solve(object@covariance,
                                         new.cov %*% t(new.cov)))))
    if(V < 0) V <- 0
    if(V > 1) V <- 1
    bound   <- qnorm(1 - alpha / 2, mean = 0, sd = sqrt (V))
    return (list(lower = fit - bound, model = fit, upper = fit + bound))
  }
  return(fit)
}

