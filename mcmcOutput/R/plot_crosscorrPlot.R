
# Version as at 2020-04-21, major rehash from previous.
# rewritten for class mcmcOutput, or anything coercible

crosscorrPlot <- function(x, params=NULL, col, addSpace=c(0,0), ...) {

  # Grab name of 'x'
  xName <- deparse(substitute(x))
  # Deal with graphics arguments, including '...'
  if(missing(col)) {
    col <- colorRampPalette(c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8",
       "#ffffbf", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027"))(255)
  }
  dots <- list(...)
  if(length(dots) == 1 && inherits(dots[[1]], "list"))
    dots <- dots[[1]]

  defaultArgs <- list(main=paste("Cross-correlation plot for", xName),
      xlab="", ylab="", cex.axis=1.2, axes=FALSE, #border=NA,
      srt=45, legendAsp = 0.1,  # legend aspect ratio
      tcl = 0.1, lwd.ticks = 1, # tick width and length in box units
      offset = 0.2)             # move parameter names out of corner
  useArgs <- modifyList(defaultArgs, dots)

  # Deal with numerical input
  x <- try(mcmcOutput(x), silent=TRUE)
  if(inherits(x, "try-error"))
    stop("Sorry, can't convert your input to a mcmcOutput object.", call.=FALSE)

  # Deal with 'params'
  if(!is.null(params)) {
    params <- matchStart(params, colnames(x))
    if(length(params) == 0)
      stop("No columns match the specification in 'params'.", call.=FALSE)
    x <- x[, params]
  }
  if(!is.numeric(x))
    stop("Sorry, can't find the numbers in your input.", call.=FALSE)
  if(ncol(x) < 2)
    stop("Need at least 2 columns to do correlations.", call.=FALSE)

  crosscorr <- try(suppressWarnings(cor(x)), silent=TRUE)
  if(inherits(crosscorr, "try-error"))
    stop("Sorry, can't extract a correlation matrix from your input.", call.=FALSE)
  corVec <- crosscorr[lower.tri(crosscorr)]
  colorid <- ceiling((corVec + 1) * length(col) / 2)
  colorid[colorid < 1] <- 1 # occurs if cor == -1

  oldpar <- par(pty='s', mar=c(1,1,5,4)) ; on.exit(par(oldpar))
  if(length(useArgs$mar) == 4)
    par(mar = useArgs$mar)

  ( Npars <- nrow(crosscorr) )
  # plot blank outline
  xmax <- Npars - 1 + addSpace[1]
  ymax <- Npars - 1 + addSpace[2]
  selPlot <- names(useArgs) %in%
    c(names(as.list(args(plot.default))), names(par(no.readonly=TRUE)))
  plotArgs <- useArgs[selPlot]
  plotArgs$x <- 1
  plotArgs$y <- 1
  plotArgs$xlim <- c(0, xmax)
  plotArgs$ylim <- c(0, ymax)
  plotArgs$type <- 'n'
  do.call(MASS::eqscplot, plotArgs)
  # plot rectangles in same order as corVec
  xvec <- col(crosscorr)[lower.tri(crosscorr)]
  yvec <- Npars - row(crosscorr)[lower.tri(crosscorr)]
  selPlot <- names(useArgs) %in%
    c(names(as.list(args(rect))), names(par(no.readonly=TRUE)))
  rectArgs <- useArgs[selPlot]
  rectArgs$xleft <- xvec - 1
  rectArgs$ybottom <- yvec
  rectArgs$xright <- xvec
  rectArgs$ytop <- yvec+1
  rectArgs$col <- col[colorid]
  do.call(rect, rectArgs)

  # Do the legend
  ys <- seq(ymax/2, ymax, length.out=length(col)+1)
  xwidth <- ymax/2 * useArgs$legendAsp
  # colour strip
  rect(xleft=rep(xmax, length(col)), ybottom=ys[1:length(col)],
      xright=rep(xmax, length(col)) + xwidth, ytop=ys[-1],
      col=col, xpd=TRUE, border=NA)
  # border to colour strip
  rectArgs$xleft <- rep(xmax,2)
  rectArgs$ybottom <- c(ys[1], mean(ys))
  rectArgs$xright <- rep(xmax+xwidth,2)
  rectArgs$ytop <- c(mean(ys), ys[length(ys)])
  rectArgs$xpd <- TRUE
  rectArgs$col <- NULL
  do.call(rect, rectArgs)
  text(x=rep(xmax+xwidth, 3), y=c(0.5,0.75,1)*(ymax), c("-1", "0", "+1"),
    pos=4, offset=0.3, cex=useArgs$cex.axis, xpd=TRUE)#, ...)

  # parameter labels
  text(x=0:(Npars-1)+useArgs$offset, y=(Npars-1):0+useArgs$offset,
    labels = rownames(crosscorr), adj=0, srt=useArgs$srt,
    cex=useArgs$cex.axis, xpd=TRUE)
  # Add ticks
  segments(x0=1:(Npars-1), x1=1:(Npars-1)+useArgs$tcl,
      y0=(Npars-1):1-0.5, y1=(Npars-1):1-0.5, lwd=useArgs$lwd.ticks, lend=1)
  segments(x0=1:(Npars-1)-0.5, x1=1:(Npars-1)-0.5,
      y0=(Npars-1):1, y1=(Npars-1):1+useArgs$tcl, lwd=useArgs$lwd.ticks, lend=1)

  return(invisible(crosscorr))
}

