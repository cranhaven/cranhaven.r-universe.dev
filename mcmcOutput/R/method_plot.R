# Function based on original code by John Kruschke in package BEST.
# Modified by Mike to make best use of ... argument and various other
#  enhancements, including shading of the HDI in showCurve=TRUE plots.


plot.mcmcOutput <- function(x, params, layout=c(3,3),
    center = c("mean", "median", "mode"), CRImass=0.95,
    compVal=NULL, ROPE=NULL, HDItextPlace=0.7,
    showCurve=FALSE, shadeHDI=NULL, ... ) {

  title <- deparse(substitute(x))
  center <- match.arg(center)

  # Deal with ... argument:
  dots <- list(...)
  if(length(dots) == 1 && inherits(dots[[1]], "list"))
    dots <- dots[[1]]
  if(!is.null(dots$main)) {
    title <- dots$main
    dots$main <- NULL
  }
  if(!is.null(dots$credMass)) {
    CRImass <- dots$credMass
    dots$credMass <- NULL
    warning("Argument 'credMass' is deprecated, please use 'CRImass'.", call.=FALSE)
  }
  if(!is.null(dots$showMode)) {
    if(dots$showMode)
      center <- "mode"
    dots$showMode <- NULL
    warning("Argument 'showMode' is deprecated, please use 'center'.", call.=FALSE)
  }

  # Deal with subsetting
  if(!missing(params)) {
    params <- matchStart(params, colnames(x))
    if(length(params) == 0)
      stop("No columns match the specification in 'params'.", call.=FALSE)
    x <- x[params]
  }

  nPlots <- ncol(x)

  defaultArgs <- list(
    yaxt="n", ylab="", main="", cex.lab=1.5, cex.main=2,
    cex=1.4, col="skyblue", border="white", bty="n", lwd=5, freq=FALSE)
  useArgs <- modifyList(defaultArgs, dots)
  # Get breaks argument
  breaks <- dots$breaks

  # Deal with layout
  if(nPlots > 1 && nPlots < prod(layout)) {  # adjust the layout
    if(nPlots <= 3) {
      layout <- c(nPlots, 1)
    } else if(nPlots <= 6) {
      layout <- c(ceiling(nPlots/2), 2)
    }
  }

  # Do the plots
  if(nPlots > 1) {     # Don't touch mfrow if only 1 plot
    old.par <- par(mar = c(4,1,1,1)+0.1, oma=c(1,1,3,1), mfrow=layout)
  } else {
    old.par <- par(mar = c(4,1,4,1)+0.1)  # title on inner margin
  }
    on.exit(par(old.par))
  if(nPlots > prod(layout)) {
    old.ask <- devAskNewPage(dev.interactive(orNone=TRUE))
    on.exit(devAskNewPage(old.ask), add=TRUE)
  }

  for(i in 1:nPlots) {
    if(is.null(dots$xlab))
      useArgs$xlab <- colnames(x)[i]
    postPlot1(x[, i], CRImass=CRImass, compVal=compVal, ROPE=ROPE,
        HDItextPlace=HDItextPlace, center=center, showCurve=showCurve,
             shadeHDI=shadeHDI, useArgs=useArgs, breaks=breaks)
    title(title, outer=nPlots > 1, cex.main=useArgs$cex.main)
  }
}
# .............................................................................


# Does a plot for a single vector (1 parameter)
postPlot1 <- function(x1, CRImass, compVal, ROPE, HDItextPlace, center, showCurve,
           shadeHDI, useArgs, breaks) {

  # Remove NAs
  x1 <- x1[!is.na(x1)]
  text <- NULL
  if(length(x1) == 0) {
    text <- "No non-NA values."
  } else if(diff(range(x1)) < sqrt(.Machine$double.eps)) {
    text <- paste("All values\nare the same\n", signifish(x1[1], 4))
  }
  if(!is.null(text)){
    plot(0,0, type = "n", xlim=c(-1,1), ann = FALSE, axes = FALSE)
    title(xlab=useArgs$xlab, cex.lab=useArgs$cex.lab)
    text(0,0, text, cex=useArgs$cex)
    return(NULL)
  }
  if(is.null(useArgs$xlim))
    useArgs$xlim <- range(compVal, ROPE, hdi(x1, 0.99))

  if(is.null(breaks)) {
    if(all(x1 == round(x1))) { # all integers
      if(diff(range(x1)) < 80) { # one bar per value
        breaks <- seq(min(x1), max(x1) + 1, by=0.5) - 0.25
      } else {
        breaks <- hist(x1, breaks=50, plot=FALSE)$breaks
      }
    } else {
      nbreaks <- ceiling(diff(range(x1)) /
                          diff(hdi(x1)) * 18)
      breaks <- seq( from=min(x1), to=max(x1),
                     length.out=nbreaks)
    }
  }
  histinfo <- hist(x1, breaks=breaks, plot=FALSE)
  histinfo$xname <- useArgs$xlab

  if (showCurve) {
    densCurve <- densityFolded(x1, adjust=2)
    cenTendHt <- 0.9 * max(densCurve$y)  # For plotting
    selPlot <- names(useArgs) %in%
      c(names(as.list(args(plot.default))), names(par(no.readonly=TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- densCurve$x
    plotArgs$y <- densCurve$y
    plotArgs$type <- "l"
    do.call(plot, plotArgs)
    abline(h=0, col='grey')
    # Display the HDI.
    if(!is.null(CRImass)) {
      HDI <- hdi(densCurve, CRImass, allowSplit=TRUE)
      ht <- attr(HDI, "height")
      if(nrow(HDI) == 1)  # hdi is not split
        HDI <- matrix(hdi(x1, CRImass), nrow=1)
      if(!is.null(shadeHDI))  {
        for (i in 1:nrow(HDI)) {
          inHDI <- which(densCurve$x >= HDI[i, 1] & densCurve$x <= HDI[i, 2])
          polyx <- c(HDI[i, 1], HDI[i, 1], densCurve$x[inHDI], HDI[i, 2], HDI[i, 2])
          polyy <- c(0, ht, densCurve$y[inHDI], ht, 0)
          polygon(polyx, polyy, border=NA, col=shadeHDI)
        }
      } else {
        segments(HDI, 0, HDI, ht, lty=2)
      }
      do.call(lines, plotArgs)
      segments(HDI[, 1], ht, HDI[, 2], ht, lwd=4, lend='butt')
      text( mean(HDI), ht, bquote(.(100*CRImass) * "% HDI" ),
            adj=c(.5,-1.7), cex=useArgs$cex, xpd=TRUE )
      # text( HDI, ht, bquote(.(signif(HDI, 3))),
      text( HDI, ht, signifish(HDI, 3),
            pos=3, cex=useArgs$cex, xpd=TRUE )
    }
  } else {
    cenTendHt <- 0.9 * max(histinfo$density)  # For plotting
    plot.histogram.args.names <- c("freq", "density", "angle", "border",
      "main", "sub", "xlab", "ylab", "xlim", "ylim", "axes", "labels",
      "add") # plot.histogram not exported, so need to cheat!
    selPlot <- names(useArgs) %in%
      c(plot.histogram.args.names, names(par(no.readonly=TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$lwd <- 1
    plotArgs$x <- histinfo
    do.call(plot, plotArgs)
    # Display the HDI.
    if(!is.null(CRImass)) {
      HDI <- hdi( x1, CRImass )
      lines(HDI, c(0,0), lwd=4, lend='butt')
      text( mean(HDI), 0, bquote(.(100*CRImass) * "% HDI" ),
            adj=c(.5,-1.7), cex=useArgs$cex, xpd=TRUE )
      safeText(HDI, c(0,0), signifish(HDI,3),
            adj=c(HDItextPlace,-0.5), cex=useArgs$cex, xpd=TRUE )
    }
  }

  # Display mean, median or mode:
  cenTend <- switch(center,
      mean = mean(x1),
      median = median(x1),
      mode = getMode(x1) )
  safeText(cenTend, cenTendHt,
      paste(center, "=", signifish(cenTend,3)),
      adj=c(0.5,0), cex=useArgs$cex, xpd=TRUE)
  lines(c(cenTend,cenTend), c(0.96*cenTendHt,0), lty='dashed')

  # Display the comparison value.
  if (!is.null(compVal)) {
    cvHt <- 0.8 * cenTendHt
    cvCol <- "darkgreen"
    pcgtCompVal <- round(100 * mean(x1 > compVal))
    pcltCompVal <- 100 - pcgtCompVal
    lines(c(compVal,compVal), c(0.96*cvHt,0),
            lty="dotted", lwd=1, col=cvCol )
    safeText(compVal, cvHt, cex=0.8*useArgs$cex, col=cvCol, xpd=TRUE,
        bquote(.(pcltCompVal)*"% < " * .(signifish(compVal,3)) * " < "*
        .(pcgtCompVal)*"%" ))
  }
  # Display the ROPE.
  if (!is.null(ROPE)) {
    ROPEtextHt <- 0.55 * cenTendHt
    ropeCol <- "darkred"
    pcInROPE <- mean(x1 > ROPE[1] & x1 < ROPE[2])
    segments(x0 = ROPE, y0 = c(0,0), y1=rep(0.96*ROPEtextHt,2),
      lty="dotted", lwd=2, col=ropeCol)
    safeText(mean(ROPE), ROPEtextHt,
           bquote(.(round(100*pcInROPE))*"% in ROPE"),
           adj=c(0.5,0), cex=1, col=ropeCol, xpd=TRUE )
  }
}
