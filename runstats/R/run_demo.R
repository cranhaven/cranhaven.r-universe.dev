

#' Generates plots for demo of package functions which take time series and
#' window width parameters
#'
#' @param func runstats package core function
#' @param plt.title.vec vector of function-specific plot titles
#'
#' @importFrom grDevices rgb dev.off
#' @importFrom graphics abline lines par plot points polygon title
#'
#' @return \code{NULL}
#'
#' @examples
#' \dontrun{
#' func <- RunningMean
#' vec <- c("black: x\nred: W-width running window",
#'          "RunningMean(x, W)",
#'          "RunningMean(x, W, circular = TRUE)")
#' plot.no.pattern(func, vec)
#' }
#'
#' @noRd
#'
plot.no.pattern <- function(func, plt.title.vec, func.name){

  ## Fixed constraints
  N <- 600
  W <- 100
  by.val <- 10

  ## Simulate signal
  set.seed(123)
  if (func.name %in% c("RunningSd", "RunningVar")){
    rnorm.vec <- rnorm(N, mean = 0, sd = c(seq(1, 100, length.out = N/2), rev(seq(1, 100, length.out = N/2))))
    x <- rnorm.vec
  } else {
    rnorm.vec <- rnorm(N)
    x <- cumsum(rnorm.vec)
  }

  ## Function output
  out.cir.F <- do.call(func, args = list(x = x, W = W, circular = FALSE))
  out.cir.T <- do.call(func, args = list(x = x, W = W, circular = TRUE))


  ## Set window of 2 plots
  # par(mfrow = c(2, 1))
  par(mfrow = c(3, 1))
  ## Predefine y axis ranges
  ylim.signalx <- range(x)
  ylim.signalx.max0 <- max(ylim.signalx) + 1
  ylim.signalx.min0 <- min(ylim.signalx) - 1
  ylim.out <- range(c(out.cir.F, out.cir.T), na.rm = TRUE)
  ylim.out.max0 <- max(ylim.out) + 1
  ylim.out.min0 <- min(ylim.out) - 1

  ## Sequence of x axis indices iteration
  seq.here <- sort(unique(c(seq(1, N - W + 1, by = by.val), N - W + 1)))
  ## Font size scalar
  # par.w2 <- 1
  par.w2 <- 1.6

  for(i in seq.here){

    # Set all plots size
    par(cex.lab = par.w2, cex.axis = par.w2 * 3/4, cex.main = par.w2, cex.sub = par.w2)

    ## Plot 1
    ##
    ## Margins area
    par(mar = c(4.1, 4.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:N, rep(NA, N), type = 'l', xlab = '', ylab = '', ylim = ylim.signalx, xlim = c(0, N))
    ## Red shaded area
    polygon(c(i, i, (i+W-1), (i+W-1)),
            c(ylim.signalx.min0, ylim.signalx.max0, ylim.signalx.max0, ylim.signalx.min0),
            col=rgb(1, 0, 0, 0.05), border = NA )
    ## Plot signal x
    lines(1:N, x, type = 'l')
    ## Add title
    title(plt.title.vec[1], adj = 0, line = 1)
    ## Plot pattern border marks (vertical lines)
    abline(v = i, lty = 2, col = "red")
    abline(v = (i+W-1), lty = 2, col = "red")

    ## Plot 2
    ##
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.F[1:i], xlim = c(1, N), type = 'l', col = 'blue', lwd = 3,
         xlab = '', ylab = '',
         ylim = ylim.out)
    ## Add title
    title(plt.title.vec[2], adj = 0, line = 1)
    ## Add output border mark (vertical line)
    abline(v = i, lty = 2, col = "blue")

    ## Plot 3
    ##
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.F[1:i], xlim = c(1, N), type = 'l', col = 'blue', lwd = 3,
         xlab = 'Time [s]', ylab = '',
         ylim = ylim.out)
    ## Add title
    title(plt.title.vec[3], adj = 0, line = 1)
    ## Add output border mark (vertical line)
    abline(v = i, lty = 2, col = "blue")

    ## Sleep system before plotting the next plot
    if (!(i == max(seq.here))){
      Sys.sleep(0.1)
    } else {
      Sys.sleep(0.03)
    }

  }


  ## Set window of 3 plots
  par(mfrow = c(3, 1))
  ## Sequence of x axis indices iteration
  seq.here <- sort(unique(c(seq(N - W + 1, N,  by = 10), N)))
  ## Font size scalar
  par.w3 <- 1.6

  for(i in seq.here){

    ## If this is the last plot, clean the plot space
    if (i == max(seq.here)){
      dev.off()
      par(mfrow = c(3, 1))
    }

    # Set all plots size
    par(cex.lab = par.w3, cex.axis = par.w3* 2/3, cex.main = par.w3, cex.sub = par.w3)

    ## Plot 1
    ##
    ## Margins area
    par(mar = c(4.1, 4.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:N, rep(NA, N), type = 'l', xlab = '', ylab = '', ylim = ylim.signalx, xlim = c(0,N))
    ## Red shaded area: part A
    polygon(c(i, i, N, N),
            c(ylim.signalx.min0, ylim.signalx.max0, ylim.signalx.max0, ylim.signalx.min0),
            col = rgb(1, 0, 0, 0.05), border = NA)
    ## Red shaded area: part B
    polygon(c(1, 1, (W - (N - i) - 1), (W - (N - i) - 1)),
            c(ylim.signalx.min0, ylim.signalx.max0, ylim.signalx.max0, ylim.signalx.min0),
            col=rgb(1, 0, 0, 0.05), border = NA )
    ## Plot signal x
    lines(1:N, x, type = 'l')
    ## Plot pattern: part A
    title(plt.title.vec[1], adj = 0, line = 1)
    abline(v = i, lty = 2, col = "red")
    ## Plot pattern: part B
    if (i == seq.here[1]){
      abline(v = N, lty = 2, col = "red")
    } else {
      pat.idx.B.end <- W - (N - i) - 1
      pat.idx.B <- 1:pat.idx.B.end
      abline(v = (pat.idx.B.end), lty = 2, col = "red")
    }


    ## Plot 2
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.F[1:i], xlim = c(0, N), type = 'l', col = 'blue', lwd = 3,
         xlab = '', ylab = '',
         ylim = range(c(out.cir.F, out.cir.T), na.rm = TRUE))
    title(plt.title.vec[2], adj = 0, line = 1)
    abline(v = i, lty = 2, col = "blue")

    ## Plot 3
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.T[1:i], xlim = c(0, N), type = 'l', col = 'blue', lwd = 3,
         xlab = 'Time [s]', ylab = '',
         ylim = range(c(out.cir.F, out.cir.T), na.rm = TRUE))
    title(plt.title.vec[3], adj = 0, line = 1)
    abline(v = i, lty = 2, col = "blue")

    ## Sleep system before plotting the next plot
    Sys.sleep(0.1)
  }

}




#' Generates plots for demo of package functions which take time series and
#' a short-timed pattern
#'
#' @param func runstats package core function
#' @param plt.title.vec vector of function-specific plot titles
#'
#' @importFrom grDevices rgb dev.off
#' @importFrom graphics abline lines par plot points polygon title
#'
#' @return \code{NULL}
#'
#' @examples
#' \dontrun{
#' func <- RunningCov
#' vec <- c("black: x\nred: running pattern",
#'          "RunningCov(x, pattern)",
#'          "RunningCov(x, pattern, circular = TRUE)")
#' plot.with.pattern(func, vec)
#' }
#'
#' @noRd
#'
plot.with.pattern <- function(func, plt.title.vec, func.name){

  ## Fixed constraints
  N <- 600
  x <- sin(seq(0, 2 * pi * 4, length.out = N))
  N.pat <- N / 3 + 1
  pat <- x[1:N.pat]
  pat.l <- length(pat)
  by.val <- 10

  ## Function output
  out.cir.F <- do.call(func, args = list(x = x, y = pat, circular = FALSE))
  out.cir.T <- do.call(func, args = list(x = x, y = pat, circular = TRUE))


  ## Set window of 2 plots
  # par(mfrow = c(2, 1))
  par(mfrow = c(3, 1))

  ## Predefine y axis ranges
  ylim.signalx <- c(-1,1)
  ylim.signalx.max0 <- max(ylim.signalx) + 1
  ylim.signalx.min0 <- min(ylim.signalx) - 1
  ylim.out <- range(c(out.cir.F, out.cir.T), na.rm = TRUE)
  ylim.out.max0 <- max(ylim.out) + 1
  ylim.out.min0 <- min(ylim.out) - 1

  ## Sequence of x axis indices iteration
  seq.here <- sort(unique(c(seq(1, N - N.pat + 1, by = by.val), N - N.pat + 1)))
  ## Font size scalar
  # par.w2 <- 1
  par.w2 <- 1.6

  for(i in seq.here){

    # Set all plots size
    par(cex.lab = par.w2, cex.axis = par.w2 * 3/4 , cex.main = par.w2, cex.sub = par.w2)

    ## Plot 1
    ##
    ## Margins area
    par(mar = c(4.1, 4.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:N, rep(NA, N), type = 'l', xlab = '', ylab = '', ylim = ylim.signalx)
    ## Red shaded area
    polygon(c(i, i, (i+pat.l-1), (i+pat.l-1)),
            c(ylim.signalx.min0, ylim.signalx.max0, ylim.signalx.max0, ylim.signalx.min0),
            col=rgb(1, 0, 0, 0.05), border = NA )
    ## Plot signal x
    lines(1:N, x, type = 'l')
    ## Add title
    title(plt.title.vec[1], adj = 0, line = 1)
    ## Plot pattern
    lines(i:(i+pat.l-1), pat, col = rgb(1, 0, 0, alpha = 0.8), lwd = 3)
    ## Plot pattern border marks (vertical lines)
    abline(v = i, lty = 2, col = "red")
    abline(v = (i+pat.l-1), lty = 2, col = "red")

    ## Plot 2
    ##
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.F[1:i], xlim = c(0, N), type = 'l', col = 'blue', lwd = 3,
         xlab = '', ylab = '',
         ylim = ylim.out)
    ## Add title
    title(plt.title.vec[2], adj = 0, line = 1)
    ## Add output border mark (vertical line)
    abline(v = i, lty = 2, col = "blue")

    ## Plot 3
    ##
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.F[1:i], xlim = c(0, N), type = 'l', col = 'blue', lwd = 3,
         xlab = 'Time [s]', ylab = '',
         ylim = ylim.out)
    ## Add title
    title(plt.title.vec[3], adj = 0, line = 1)
    ## Add output border mark (vertical line)
    abline(v = i, lty = 2, col = "blue")

    ## Sleep system before plotting the next plot
    if (!(i == max(seq.here))){
      Sys.sleep(0.1)
    } else {
      Sys.sleep(0.03)
    }

  }


  ## Set window of 3 plots
  par(mfrow = c(3, 1))
  ## Sequence of x axis indices iteration
  seq.here <- sort(unique(c(seq(N - N.pat + 1, N,  by = 10), N)))
  ## Font size scalar
  par.w3 <- 1.6

  for(i in seq.here){

    ## If this is the last plot, clean the plot space
    if (i == max(seq.here)){
      dev.off()
      par(mfrow = c(3, 1))
    }

    # Set all plots size
    par(cex.lab = par.w3, cex.axis = par.w3 * 2/3, cex.main = par.w3, cex.sub = par.w3)

    ## Plot 1
    ##
    ## Margins area
    par(mar = c(4.1, 4.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:N, rep(NA, N), type = 'l', xlab = '', ylab = '', ylim = ylim.signalx)
    ## Red shaded area: part A
    polygon(c(i, i, N, N),
            c(ylim.signalx.min0, ylim.signalx.max0, ylim.signalx.max0, ylim.signalx.min0),
            col = rgb(1, 0, 0, 0.05), border = NA)
    ## Red shaded area: part B
    polygon(c(1, 1, (N.pat - (N - i) - 1), (N.pat - (N - i) - 1)),
            c(ylim.signalx.min0, ylim.signalx.max0, ylim.signalx.max0, ylim.signalx.min0),
            col=rgb(1, 0, 0, 0.05), border = NA )
    ## Plot signal x
    lines(1:N, x, type = 'l')
    ## Plot pattern: part A
    title(plt.title.vec[1], adj = 0, line = 1)
    pat.idx.A <- i:N
    lines(pat.idx.A, pat[1:length(pat.idx.A)], col = rgb(1, 0, 0, alpha = 0.8), lwd = 3)
    if (length(pat.idx.A)==1){
      points(pat.idx.A, pat[1:length(pat.idx.A)], col = rgb(1, 0, 0, alpha = 0.8), lwd = 3, pch = 19)
    }
    abline(v = i, lty = 2, col = "red")
    ## Plot pattern: part B
    if (i == seq.here[1]){
      abline(v = N, lty = 2, col = "red")
    } else {
      pat.idx.B.end <- N.pat - (N - i) - 1
      pat.idx.B <- 1:pat.idx.B.end
      lines(pat.idx.B, pat[(N.pat - pat.idx.B.end + 1):N.pat], col = rgb(1, 0, 0, alpha = 0.8), lwd = 3)
      abline(v = (pat.idx.B.end), lty = 2, col = "red")
    }


    ## Plot 2
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.F[1:i], xlim = c(0, N), type = 'l', col = 'blue', lwd = 3,
         xlab = '', ylab = '',
         ylim = range(c(out.cir.F, out.cir.T), na.rm = TRUE))
    title(plt.title.vec[2], adj = 0, line = 1)
    abline(v = i, lty = 2, col = "blue")

    ## Plot 3
    ## Margins area
    par(mar = c(5.1, 4.1, 3.1, 2.1), mgp = c(3, 1, 0), las = 0) ## inside (b,l,t,r)
    ## Plot body
    plot(1:i, out.cir.T[1:i], xlim = c(0, N), type = 'l', col = 'blue', lwd = 3,
         xlab = 'Time [s]', ylab = '',
         ylim = range(c(out.cir.F, out.cir.T), na.rm = TRUE))
    title(plt.title.vec[3], adj = 0, line = 1)
    abline(v = i, lty = 2, col = "blue")

    ## Sleep system before plotting the next plot
    if (!(i == max(seq.here))){
      Sys.sleep(0.1)
    }
  }

}




#' Demo visualization of package functions
#'
#' Generates demo visualization of output of methods for computing running
#' statistics.
#'
#' @param func.name Character value; one of the following:
#' \itemize{
#'   \item "RunningMean",
#'   \item "RunningSd",
#'   \item "RunningVar",
#'   \item "RunningCov",
#'   \item "RunningCor",
#'   \item "RunningL2Norm".
#' }
#'
#' @return \code{NULL}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' runstats.demo(func.name = "RunningMean")
#' runstats.demo(func.name = "RunningSd")
#' runstats.demo(func.name = "RunningVar")
#' runstats.demo(func.name = "RunningCov")
#' runstats.demo(func.name = "RunningCor")
#' runstats.demo(func.name = "RunningL2Norm")
#' }
#'
runstats.demo <- function(func.name = "RunningCov"){

  ## Match function name in argument with the package function
  func <- switch(func.name,
                 "RunningMean"   = RunningMean,
                 "RunningSd"     = RunningSd,
                 "RunningVar"    = RunningVar,
                 "RunningCov"    = RunningCov,
                 "RunningCor"    = RunningCor,
                 "RunningL2Norm" = RunningL2Norm)
  if (is.null(func)) stop("Undefined func.name argument value.")

  ## Match function name in argument with the plot title
  plt.title.vec <-  switch(func.name,
                           "RunningMean"   =  c("black: time-series x\nred: running window of length W",
                                               "RunningMean(x, W, circular = FALSE)",
                                               "RunningMean(x, W, circular = TRUE)"),

                           "RunningSd"     =  c("black: time-series x\nred: running window of length W",
                                               "RunningSd(x, W, circular = FALSE)",
                                               "RunningSd(x, W, circular = TRUE)"),

                           "RunningVar"    =  c("black: time-series x\nred: running window of length W",
                                               "RunningVar(x, W, circular = FALSE)",
                                               "RunningVar(x, W, circular = TRUE)"),

                           "RunningCov"    =  c("black: time-series x\nred: pattern y",
                                               "RunningCov(x, y, circular = FALSE)",
                                               "RunningCov(x, y, circular = TRUE)"),

                           "RunningCor"    =  c("black: time-series x\nred: pattern y",
                                               "RunningCor(x, y, circular = FALSE)",
                                               "RunningCor(x, y, circular = TRUE)"),

                           "RunningL2Norm" =  c("black: time-series x\nred: pattern y",
                                               "RunningL2Norm(x, y, circular = FALSE)",
                                               "RunningL2Norm(x, y, circular = TRUE)"))

  if (func.name %in% c("RunningCov", "RunningCor", "RunningL2Norm")){
    plot.with.pattern(func, plt.title.vec, func.name)
  } else {
    plot.no.pattern(func, plt.title.vec, func.name)
  }

}
