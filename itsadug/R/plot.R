#' Fade out the areas in a surface without data.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @import mgcv
#' @description Add a transparency Rug to a contour plot or image.
#' 
#' @param x Observations on x-axis.
#' @param y Observations on y-axis.
#' @param n.grid Resolution of Rug. Defaults to 30, 
#' which means that the x- and y-axis are divided in 30 bins. 
#' A two-value vector ould be used to specify different bins for 
#' x- and y-axis.
#' @param too.far plot grid nodes that are too far from the points defined by 
#' the variables given in view can be excluded from the plot. too.far 
#' determines what is too far. The grid is scaled into the unit square along 
#' with the view variables and then grid nodes more than too.far from the 
#' predictor variables are excluded. Based on 
#' \code{\link[mgcv]{exclude.too.far}} of Simon N. Wood.
#' @param col Color representing missing data. Defaults to 'white'.
#' @param alpha Transparency, number between 0 (completely 
#' transparent) and 1 (non-transparent). Defaults to 1.
#' @param use.data.range Logical value, indicating whether \code{x} and 
#' \code{y} are the data that the plot is based on. Defaults to TRUE.
#' @return Plots a shaded image over the contour plot or image.
#' @author Jacolien van Rij, based on Simon N. Wood's 
#' \code{\link[mgcv]{exclude.too.far}}
#' @section Warning:
#' On Linux \code{\link{x11}} devices may not support transparency. 
#' In that case, a solution might be to write the plots immediately to a file 
#' using functions such as \code{\link{pdf}}, or \code{\link{png}}.
#' @seealso 
#' \code{\link[graphics]{rug}}, \code{\link[graphics]{contour}}, 
#' \code{\link[graphics]{image}}
#' @examples
#' data(simdat)
#' 
#' # Introduce extreme values:
#' set.seed(123)
#' newdat <- simdat[sample(which(simdat$Time < 1500),
#'     size=round(.5*length(which(simdat$Time < 1500)))),]
#' newdat <- rbind(newdat, 
#'     simdat[sample(which(simdat$Time > 1500),
#'     size=5),])
#' # Some simple GAM with tensor:
#' m1 <- bam(Y ~ te(Time, Trial), data=newdat)
#' # plot summed effects:
#' fvisgam(m1, view=c('Time', 'Trial'), zlim=c(-15,15))
#' fadeRug(newdat$Time, newdat$Trial)
#' # check with data points:
#' points(newdat$Time, newdat$Trial, pch=16, col=alpha(1))
#' 
#' # compare with default rug:
#' fvisgam(m1, view=c('Time', 'Trial'), zlim=c(-15,15))
#' rug(newdat$Time)
#' rug(newdat$Trial, side=2)
#' fadeRug(newdat$Time, newdat$Trial)
#' # and compare with too.far:
#' fvisgam(m1, view=c('Time', 'Trial'), zlim=c(-15,15),
#'     too.far=.03)
#' vis.gam(m1, view=c('Time', 'Trial'), zlim=c(-15,15),
#'     too.far=.03, plot.type='contour', color='topo')
#' 
#' # in case fade rug overlaps with color legend:
#' fvisgam(m1, view=c('Time', 'Trial'), zlim=c(-15,15),
#'      add.color.legend=FALSE)
#' fadeRug(newdat$Time, newdat$Trial, alpha=.75)
#' gradientLegend(c(-15,15), pos=.875)
#' 
#' # change x- and y-grid, and color:
#' fvisgam(m1, view=c('Time', 'Trial'), zlim=c(-15,15))
#' points(newdat$Time, newdat$Trial)
#' fadeRug(newdat$Time, newdat$Trial, n.grid=c(100,10), col='gray')
#' @family Functions for plotting
fadeRug <- function(x, y, n.grid = 30, too.far = 0.03, col = "white", alpha = 1, use.data.range = TRUE) {
    n.grid.x <- n.grid.y <- n.grid[1]
    if (length(n.grid) == 2) {
        n.grid.y <- n.grid[2]
    }
    xlim <- range(x, na.rm = TRUE)
    ylim <- range(y, na.rm = TRUE)
    if (use.data.range == FALSE) {
        xlim = par()$usr[1:2]
        ylim = par()$usr[3:4]
    }
    newd <- expand.grid(x = seq(xlim[1], xlim[2], length = n.grid.x), y = seq(ylim[1], ylim[2], length = n.grid.y))
    newd <- newd[order(newd[, "y"], newd[, "x"]), ]
    x.step <- diff(seq(xlim[1], xlim[2], length = n.grid.x))[1]
    y.step <- diff(seq(ylim[1], ylim[2], length = n.grid.y))[1]
    too.far.raster <- rep(alpha(col, f = 0), nrow(newd))
    ex.tf = NULL
    if (too.far > 0) {
        ex.tf <- mgcv::exclude.too.far(newd[, "x"], newd[, "y"], x, y, dist = too.far)
        too.far.raster[ex.tf] <- alpha(col, f = alpha)
    }
    ## for debugging: newd <- newd[!ex.tf,] points(newd$x, newd$y, pch=16, col=alpha(col, f=alpha))
    ## points(newdat$Time, newdat$Trial, pch='*', col=alpha(1)) raster images are row-first, in contrast to
    ## images...
    too.far.raster <- matrix(too.far.raster, byrow = TRUE, ncol = n.grid.x, nrow = n.grid.y)
    too.far.raster <- as.raster(too.far.raster[nrow(too.far.raster):1, ])
    gfc <- getFigCoords("p")
    rasterImage(too.far.raster, xleft = gfc[1], xright = gfc[2], ybottom = gfc[3], ytop = gfc[4])
}





#' Add rug to plot, based on model.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @import stats
#' @description Add rug based on model data.
#' 
#' @param model gam or bam object.
#' @param view Text string containing the name of the smooth
#' to be displayed. Note that 
#' variables coerced to factors in the model formula won't work as view 
#' variables.
#' @param cond A named list of the values to use for the other predictor terms 
#' (not in view). Used for choosing between smooths that share the same view 
#' predictors.
#' @param data.rows Vector of numbers (indices of rows in data) or vector of 
#' logical vales (same length as rows in data) for selecting specific data 
#' points.
#' @param rm.ranef Logical: whether or not to remove random effects. 
#' Default is TRUE.
#' @param print.summary Logical: whether or not to print information messages.
#' Default set to the print info messages option 
#' (see \code{\link{infoMessages}}).
#' @param ... Optional graphical parameters (see \code{\link[graphics]{rug}}).
#' @author Jacolien van Rij
#' @examples
#' plot(cars$speed, cars$dist, pch=16, col=alpha(1))
#' lm1 <- lm(dist ~ speed, dat=cars)
#' abline(lm1, col='red', lwd=2)
#' rug_model(lm1, view='speed')
#' rug_model(lm1, view='dist', side=2)
#' 
#' \dontrun{
#' library(itsadug)
#' data(simdat)
#' m1 <- bam(Y ~ Group + te(Time, Trial, by=Group), data=simdat)
#' # plot:
#' fvisgam(m1, view=c('Time', 'Trial'), cond=list(Group='Adults'))
#' rug_model(m1, view='Time', cond=list(Group='Adults'))
#' rug_model(m1, view='Trial', cond=list(Group='Adults'), side=2)
#' }
#' @family Functions for plotting
rug_model <- function(model, view, cond = NULL, data.rows = NULL, rm.ranef = NULL, print.summary = getOption("itsadug_print"), 
    ...) {
    dat <- NULL
    view <- view[1]
    if ("lm" %in% class(model)) {
        dat <- model$model
    } else if ("lmerMod" %in% class(model)) {
        dat <- model@frame
    }
    if (!is.null(data.rows)) {
        dat <- dat[data.rows, ]
    } else if (!is.null(cond)) {
        if (!is.null(rm.ranef)) {
            if (rm.ranef == TRUE) {
                for (i in 1:length(model$smooth)) {
                  if ("random" %in% names(model$smooth[[i]])) {
                    terms <- model$smooth[[i]]$term
                    for (j in terms) {
                      if ((j %in% names(cond)) & !(j %in% view)) {
                        cond[[j]] <- NULL
                      }
                    }
                  }
                }
            } else if (inherits(rm.ranef, c("numeric", "integer"))) {
                for (i in rm.ranef) {
                  if ("random" %in% names(model$smooth[[i]])) {
                    terms <- model$smooth[[i]]$term
                    for (j in terms) {
                      if ((j %in% names(cond)) & !(j %in% view)) {
                        cond[[j]] <- NULL
                      }
                    }
                  }
                }
            }
        }
        for (i in names(cond)) {
            if ((!i %in% view) & (!inherits(dat[, i], c("numeric", "integer")))) {
                dat <- dat[dat[, i] %in% cond[[i]], ]
            }
        }
    }
    if ((nrow(dat) == 0)) {
        rug(model$model[, view], ...)
        if (print.summary == TRUE) {
            cat("Note: Selection of grouping predictors does not seem to appear in data. Rug of all data is being added.\n")
        }
    } else {
        rug(dat[, view], ...)
    }
}





