
# Function to plot the Bayesian posterior resulting from the comb method
#   described in Kruschke (2015) DBDA.

plotComb <- function(x, y, credMass=0.95, plot=TRUE, showMode=FALSE,
           shadeHDI=NULL, ...) {
  if(length(credMass) != 1 || credMass <= 0 || credMass >= 1)
    stop("credMass must be in 0 < credMass < 1")

  # Calculate HDI:
  sorted = sort(y, decreasing=TRUE )
  heightIdx = min( which( cumsum( sorted) >= sum(y) * credMass ) )
  height = sorted[heightIdx]
  indices = which( y >= height )
  gaps <- which(diff(indices) > 1)
  begs <- indices[c(1, gaps + 1)]
  ends <- indices[c(gaps, length(indices))]
  HDI <- cbind(lower = x[begs], upper = x[ends])
  attr(HDI, "credMass") <- credMass
  attr(HDI, "height") <- height

  # Do the plot:
  if(plot) {
    dots <- list(...)
    # if(length(dots) == 1 && class(dots[[1]]) == "list")
    if(length(dots) == 1 && inherits(dots[[1]], "list"))  # Fixed 2022-06-06
      dots <- dots[[1]]
    defaultArgs <- list(xlab=deparse(substitute(x)),
      yaxt="n", ylab="", main="", cex.lab=1.5,
      cex=1.4, col="skyblue", bty="n", lwd=5,
      xlim=range(x))
    useArgs <- modifyList(defaultArgs, dots)

    selPlot <- names(useArgs) %in%
      c(names(as.list(args(plot.default))), names(par(no.readonly=TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- x
    plotArgs$y <- y
    plotArgs$type <- "l"
    do.call("plot", plotArgs)
    abline(h=0, col='grey')
    # Display the HDI.
    if(!is.null(credMass)) {
      ht <- attr(HDI, "height")
      if(!is.null(shadeHDI))  {
        for (i in 1:nrow(HDI)) {
          inHDI <- which(x >= HDI[i, 1] & x <= HDI[i, 2])
          polyx <- c(HDI[i, 1], HDI[i, 1], x[inHDI], HDI[i, 2], HDI[i, 2])
          polyy <- c(0, ht, y[inHDI], ht, 0)
          polygon(polyx, polyy, border=NA, col=shadeHDI)
        }
      } else {
        segments(HDI, 0, HDI, ht, lty=2)
      }
      do.call(lines, plotArgs)
      segments(HDI[, 1], ht, HDI[, 2], ht, lwd=4, lend='butt')
      text( mean(HDI), ht, bquote(.(100*credMass) * "% HDI" ),
            adj=c(.5,-1.7), cex=useArgs$cex, xpd=TRUE )
      # text( HDI, ht, bquote(.(signif(HDI, 3))),
      text( HDI, ht, signifish(HDI, 3),
            pos=3, cex=useArgs$cex, xpd=TRUE )
    }
    # Display mean or mode:
    cenTendHt <- 0.9 * max(y)
    if ( showMode==FALSE ) {
        meanParam <- sum(x * y)
        text( meanParam, cenTendHt,
              bquote(mean==.(signifish(meanParam,3))), adj=c(.5,0), cex=useArgs$cex, xpd=TRUE )
    } else {
        modeParam <- x[which.max(y)]
        text( modeParam, cenTendHt,
              bquote(mode==.(signifish(modeParam,3))), adj=c(.5,0), cex=useArgs$cex, xpd=TRUE )
    }

  }  # end if(plot)
  return(HDI)
}


