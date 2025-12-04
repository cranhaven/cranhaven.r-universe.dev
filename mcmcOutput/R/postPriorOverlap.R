# Some of the plotting code taken from package BEST, originally by John Kruschke.

postPriorOverlap <- function(x, prior, priorPars, breaks=NULL,
           hcols=c("skyblue", "yellow", "green", "white"), ...) {

  # Does a posterior histogram for a single parameter, adds the prior,
  #   displays and calculates the overlap.
  # Returns the overlap.

  objName <- deparse(substitute(x))

  # Deal with ... argument:
  dots <- list(...)
  if(length(dots) == 1 && inherits(dots[[1]], "list"))
    dots <- dots[[1]]
  defaultArgs <- list(
    xlab=objName, yaxt="n", ylab="", main="Posterior-prior overlap", cex.lab=1.5,
    cex=1.4, bty="n")
  useArgs <- histArgs <- modifyList(defaultArgs, dots)

  # get breaks: a sensible number over the hdi; cover the full range (and no more);
  #   equal spacing.
  if(is.null(breaks)) {
    nbreaks <- ceiling(diff(range(x)) / diff(hdi(x)) * 18)
    breaks <- seq( from=min(x), to=max(x), length.out=nbreaks)
  }
  # plot posterior histogram:
  histArgs$x <- x
  histArgs$breaks <- breaks
  histArgs$col <- hcols[1]
  histArgs$border <- hcols[4]
  histArgs$freq <- FALSE
  histArgs$add <- FALSE

  histinfo <- do.call(hist, histArgs)

  if (is.numeric(prior))  {
    # plot the prior as a histogram if it's numeric
    histArgs$x <- prior
    histArgs$breaks <- c(-Inf, breaks, Inf)
    histArgs$col <- hcols[2]
    histArgs$border <- hcols[4]
    histArgs$freq <- FALSE
    histArgs$add <- TRUE
    priorInfo <- do.call(hist, histArgs)$density[2:length(breaks)]
  } else if (is.function(prior)) {
    # check that the function works:
    priorPars$x <- histinfo$mids
    priorInfo <- try(do.call(prior, priorPars), silent=TRUE)
    if(inherits(priorInfo, "try-error"))
      stop(paste("Incorrect arguments for the density function", substitute(prior)))
  }
  # get (and plot) the overlap
  minHt <- pmin(priorInfo, histinfo$density)
  rect(breaks[-length(breaks)], rep(0, length(breaks)-1), breaks[-1], minHt,
    col=hcols[3], border=hcols[4])
  overlap <- sum(minHt * diff(histinfo$breaks))
  # Add curve if prior is a function
  if (is.function(prior))
    lines(histinfo$mids, priorInfo, lwd=2, col='brown')
  # Add text
  text(mean(breaks), 0, paste0("overlap = ", round(overlap*100), "%"), pos=3)#, ...)

  return(overlap)
}

if(FALSE) {
foo <- x <- rbeta(1e5, 5, 7)
prior <- runif(1e5)
prior <- dbeta
priorPars <- list(shape1=0.2, shape2=0.2)
breaks=NULL
hcols=c("skyblue", "yellow", "green", "white")
dots <- list()
dots <- list(col='red')
objName <- "foo"

postPriorOverlap(foo, runif(1e5))
postPriorOverlap(foo, dbeta, priorPars)
postPriorOverlap(foo, runif(1e5), main="Testing...", lwd=5, cex.main=2)
postPriorOverlap(foo, runif(1e5), main="Testing...", col='red')
}
