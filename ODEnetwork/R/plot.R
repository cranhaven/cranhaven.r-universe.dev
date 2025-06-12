#' Plots Results of ODEnetwork
#' 
#' Plots the results of \code{simuNetwork} of the given \code{\link{ODEnetwork}}
#' in different ways.
#'
#' @param x [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param state [\code{character}]\cr
#'    The type of result, that is plotted.
#'    If \code{1}, only state1 (position or angle) is plotted over time.
#'    If \code{2}, only state2 (velocity or magnitude) is plotted over time.
#'    If \code{12}, state1 and state2 are plotted over time.
#'    If \code{1vs2}, state2 is plotted over state1.
#'    Default is \code{state12}
#' @param var [\code{numeric(n)}]\cr
#'    Subset of variables to plot. Default is \code{NULL}, which plots all variables.
#' @param ... Additional arguments.
#' @method plot ODEnetwork
#' @export
#' @examples
#' masses <- c(1, 2)
#' dampers <- diag(c(0.1, 0.5))
#' dampers[1, 2] <- 0.05
#' springs <- diag(c(4, 10))
#' springs[1, 2] <- 6
#' odenet <- ODEnetwork(masses, dampers, springs)
#' odenet <- setState(odenet, c(1, 3), c(0, 0))
#' odenet <- simuNetwork(odenet, seq(0, 10, by = 0.05))
#' plot(odenet)
#' plot(odenet, var = 2L)
#' plot(odenet, state = "1")
#' plot(odenet, state = "2")
#' plot(odenet, state = "1vs2")
plot.ODEnetwork <- function(x, ..., state = "12", var = NULL) {
  assertChoice(state, c("12", "1", "2", "1vs2"))
  assertIntegerish(var, lower = 1L, upper = length(x$masses), any.missing = FALSE, null.ok = TRUE)
  # Check ode result
  if (is.null(x$simulation$results))
    stop("Simulation results missing!")
  # Read ode result
  if (is.null(var)) {
    mRes <- x$simulation$results
  } else {
    var <- unique(sort(c(2*var-1, 2*var))) + 1
    mRes <- x$simulation$results[, c(1, var)]
    attr(mRes, "class") <- class(x$simulation$results)
  }
  switch(state
        , "12" = {
          op <- graphics::par(no.readonly = TRUE)
          graphics::plot(mRes, ...)
          graphics::par(op)
        }
        , "1" = {
          classes <- class(mRes)
          mRes <- mRes[, c(1, seq(2, ncol(mRes), by=2))]
          attr(mRes, "class") <- classes
          op <- graphics::par(no.readonly = TRUE)
          graphics::plot(mRes, ...)
          graphics::par(op)
        }
        , "2" = {
          classes <- class(mRes)
          mRes <- mRes[, c(1, seq(3, ncol(mRes), by=2))]
          attr(mRes, "class") <- classes
          op <- graphics::par(no.readonly = TRUE)
          graphics::plot(mRes, ...)
          graphics::par(op)
        }
        , "1vs2" = {
          # calculate plot size
          intVars <- ncol(mRes)-1
          intRows <- intCols <- floor(sqrt(intVars/2))
          if (intCols*intRows < intVars/2) {
            intCols <- intCols + 1
            if (intCols*intRows < intVars/2)
              intRows <- intRows + 1
          }
          
          op <- graphics::par(mfrow = c(intRows, intCols))
          if (x$coordtype == "cartesian") {
            for(intVar in seq(2, intVars, by=2)) {
              # plot x vs. v
              graphics::plot(mRes[, intVar], mRes[, intVar+1], type = "l"
                             , xlab = colnames(mRes)[intVar], ylab = colnames(mRes)[intVar+1], ...)
              # plot starting point
              graphics::points(mRes[1, intVar], mRes[1, intVar+1], col = "red", pch = 13)
            }
          } else {
            crclSteps <- seq(0, 2*pi, length.out = 400)
            for(intVar in seq(2, intVars, by=2)) {
              circles <- pretty(c(0, max(mRes[, intVar])))
              plotrange <- c(-max(circles), max(circles))
              mCartesian <- convertCoordinates(mRes[, c(intVar, intVar+1)])
              # plot area
              graphics::plot(1, type = "n"
                             , xlim = plotrange, ylim = plotrange
                             , xlab = paste("Oscillator", as.numeric(gsub("\\D", "", colnames(mRes)[intVar])))
                             , ylab = ""
                             , axes = FALSE, frame.plot = FALSE, asp = 1
                             )
              # add axis and label
              graphics::axis(2
                             , at = sort(union(-circles, circles))
                             , labels = FALSE
                             , pos = -1.22 * max(circles)
                             )
              graphics::text(-1.25 * max(circles), sort(union(-circles, circles))
                             , labels = sort(union(-circles, circles))
                             , xpd = TRUE, pos = 2
                             )
              graphics::text(1.3 * max(circles), 0, labels = "Magnitude", xpd = TRUE, srt = 270)
              # draw circle lines
              for (i in circles) {
                graphics::lines(i*cbind(cos(crclSteps), sin(crclSteps)), col = grDevices::gray(0.9))
              }
              # add grid
              axpos <- c(0, pi/2, pi, 3/2*pi)
              axlab <- expression(0
                                  , textstyle(frac(1,2))*pi
                                  , pi
                                  , textstyle(frac(3,2))*pi)
              for (i in 1:length(axpos)) {
                coordpair <- matrix(c(max(circles), axpos[i]), ncol = 2)
                coordpair <- as.vector(convertCoordinates(coordpair))
                graphics::segments(0, 0, coordpair[1], coordpair[2], col = grDevices::gray(0.9))
                graphics::text(coordpair[1]*1.12, coordpair[2]*1.12, axlab[i], xpd = TRUE)
              }
              # plot data
              graphics::lines(mCartesian, ...)
              # plot starting point
              graphics::points(mCartesian[1, 1], mCartesian[1, 2], col = "red", pch = 13)
            }
          }
          graphics::par(op)
        }
  )
  
  return(invisible(TRUE))
}
