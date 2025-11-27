#' Coverage Plot for MDEI Object
#'
#' @param object An object of class MDEI.
#' @param xvar The variable to plot along the x-axis.  May be `treat` for the treatment
#' variable, the name of a column in the covariate matrix in the \code{obj}, or a numeric vector,
#' the length of the data.   Default is \code{"treat"}.
#' @param sigval The value to see if it is covered by the conformal interval. Either a single value or a vector.   Default is \code{0}.
#' @param target Either tau` or `theta`.  The first, `tau`, is the marginal effect at each point, while the second
#' `theta`, is the portion of the conditional mean of the outcome that is a function of the treatment and the outcome.
#' @param colors A vector with two elements.  The first is the color of the confidence interval at points
#' where the conformal interval does not contain \code{sigval}, the second the color where it does.   Default is \code{c(gray(.7), gray(0))}.
#' @param cex.point The size of the points in the figure.  Default is \code{0.5}.
#' @param xlabel Label for x-axis of figure.  Default is \code{""}.
#' @param ylabel Label for y-axis of figure.  Default is \code{""}.
#' @param ... Additional arguments to be passed to \code{plot}.
#' @export
#'
#'@return No return value.
#'@rdname coverPlot
coverPlot <-
  function(object,
           xvar = "treat",
           sigval = 0,
           target = "tau",
           colors = c(gray(.7), gray(0)),
           cex.point = 0.5,
           xlabel = "",
           ylabel = "",
           ...) {
    # Set up point estimates and CI
    obj <- object
    pointest <- obj$tau.est
    CI <- obj$CIs.tau
    if (target == "theta") {
      pointest <- obj$theta.est
      CI <- obj$CIs.theta
    }
    
    # set up variable to be plotted
    if (xvar[1] == "treat") {
      xvar <- obj$internal$treat
    } else{
      if (is.character(xvar[1]))
        xvar <- obj$internal$X0[, xvar]
    }
    
    cover.curr <- apply(CI - sigval, 1, prod) < 0
    plot(
      xvar,
      pointest,
      type = "n",
      ylim = range(CI),
      xlab = xlabel,
      ylab = ylabel
    )
    lines(xvar[sort(xvar, index.return = T)$ix], (xvar * 0 + sigval)[sort(xvar, index.return =
                                                                            T)$ix])
    segments(
      x0 = xvar,
      x1 = xvar,
      y0 = CI[, 1],
      y1 = CI[, 2],
      col = ifelse(cover.curr,
                   colors[1],
                   colors[2])
    )
    
    points(xvar, pointest, pch = 19, cex = cex.point)
    print("Proportion of the time that the confidence interval contains signal:")
    print(mean(cover.curr))
    return(cover.curr)
  }

#' Summary of an MDEI Object
#'
#' Summary of an object of class MDEI.  
#' @param object An object of class MDEI.
#' @param features Number of spline bases to include.
#' @param ... Additional arguments to be passed to \code{plot}.
#' @export
#' 
#' @return \describe{
#' \item{coeftable}{A table with three columns: the names of selected spline interactions, the average coefficient, and
#' proportion of time it was included in the model. Averages over taken over subsamples in the
#' split sample strategy.Note that the coefficients are interactions between
#' spline interactions that can be accessed through \code{obj$internal$Xmat.spline}.}
#'}
#'
#'
#'
#'@rdname summary
summary.MDEI <-
  function(object,
           features = 10,
           ...){
    coeftable<- object$coefficients[1:features,]
    class(coeftable) <- 'summary.MDEI'
    return(coeftable)
  }
