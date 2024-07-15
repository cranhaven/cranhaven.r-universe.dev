#' Projection plots for optimization routines.
#'
#' @param x An \code{optproj} object, i.e., output from function \code{\link{optim_proj}}.
#' @param xnames Optional vector of element names of potential solution for plot titles.
#' @param xind Integer or logical vector of indices indicating which projections should be plotted.  Defaults to all projection plots.
#' @param equalize If \code{TRUE}, narrows the range in each projection plot such that the y-value is more or less the same at either endpoint.
#' @param layout Optional vector giving the number of rows and columns in the plot layout.  For \code{nx} plots, defaults to \code{c(nr, nc)}, where \code{nr = floor(nx)} and \code{nc = ceiling(nx/nr)}.
#' @param xlab,ylab Outer x-axis and y-axis labels.
#' @param ... Further arguments to be passed to or from other methods.
#' @return A grid of projection plots, with vertical lines at the potential solution.
#' @export
plot.optproj <- function(x, xnames, xind, equalize = FALSE,
                         layout, xlab, ylab, ...) {
  xsol <- x$xsol
  nx <- length(xsol)
  xout <- x$xproj
  yout <- x$yproj
  # which projections to plot
  if(missing(xind)) xind <- 1:nx
  if(is.logical(xind)) xind <- which(xind) # convert T/F's to indices
  nx2 <- length(xind)
  # plot titles
  if(missing(xnames)) {
    xnames <- paste0("x[",1:nx,"]")
    # converts to expression so symbol "theta_i" is plotted
    xnames <- parse(text = xnames)
  }
  # set up plot region
  opar <- par(no.readonly = TRUE) # save specs of current plot
  on.exit(par(opar)) # restore plot parameters when exiting function
  # plot size
  if(missing(layout)) {
    layout <- floor(sqrt(nx2))
    layout <- c(layout, ceiling(nx2/layout))
  }
  par(mfrow = layout, mar = c(2,2.5,2,0), oma = c(2.5, 2.5, .5, 1))
  # plot itself
  for(ii in 1:nx2) {
    ix <- xind[ii]
    if(equalize) {
      xlim <- .equalize_lims(xout[,ix], yout[,ix], xsol[ix], x$maximize)
      ylim <- xlim$ylim
      xlim <- xlim$xlim
    } else {
      xlim <- range(xout[,ix], na.rm = TRUE, finite = TRUE)
      ylim <- range(yout[,ix], na.rm = TRUE, finite = TRUE)
    }
    plot(xout[,ix], yout[,ix], type = "l", main = xnames[ix],
         xlim = xlim, ylim = ylim, xlab = "", ylab = "")
    ## title(main = xnames[ix], cex.main = 2)
    abline(v = xsol[ix], col = "red")
  }
  # labels in margin
  if(missing(xlab)) xlab <- "Parameter"
  if(missing(ylab)) ylab <- "Objective Function"
  mtext(side = 2, text = ylab, line = .5, outer = TRUE)
  mtext(side = 1, text = xlab, line = 1, outer = TRUE)
}

# equalize plot limits
.equalize_lims <- function(xseq, yval, xsoli, maximize) {
  if(!maximize) yval <- -yval
  vth <- !is.na(yval) & yval > -Inf # valid values
  lth <- xseq < xsoli # on the left of solution
  rth <- xseq > xsoli # on the right
  # larger of the min value on each size
  lbd <- max(min(yval[vth & lth]), min(yval[vth & rth]))
  # rescale xseq to be on this range
  ibd <- c(which.min(ifelse(vth & lth, abs(yval-lbd), Inf)),
           which.min(ifelse(vth & rth, abs(yval-lbd), Inf)))
  if(!maximize) yval <- -yval
  list(xlim = xseq[ibd],
       ylim = range(yval[ibd[1]:ibd[2]], na.rm = TRUE)) # new limits
}
