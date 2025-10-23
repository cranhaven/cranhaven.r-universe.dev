#' Plot the empirical points and the regression
#'
#' @param eq The name of the equation.
#' @param nlsDR The result returned by the \code{devRateModel} function.
#' @param rangeT The range of temperatures over which the regression is plotted.
#'   This argument may be overwritten depending on the equation.
#' @param optText A logical indicating whether the name of the equation should be written
#'   in the topright corner of the plot.
#' @param spe A logical indicating if special plotting rules from literature should apply.
#' @param ... Additional arguments for the plot.
#' @return Nothing.
#' @examples
#' myT <- 5:15
#' myDev <- -0.05 + rnorm(n = length(myT), mean = myT, sd = 1) * 0.01
#' myNLS <- devRateModel(eq = campbell_74, temp = myT, devRate = myDev,
#'   startValues = list(aa = 0, bb = 0))
#' devRatePlot(eq = campbell_74, nlsDR = myNLS,
#'   spe = TRUE, pch = 16, lwd = 2, ylim = c(0, 0.10))
#' @export
devRatePlot <- function(
  eq, nlsDR, rangeT = 10, optText = TRUE, spe = TRUE, ...){

  if (inherits(nlsDR, "nls")){
    temp <- get("T", nlsDR$m$getEnv())
    devRate <- get("rT", nlsDR$m$getEnv())
  } else {
    temp <- get("T", nlsDR[[1]]$m$getEnv())
    devRate <- get("rT", nlsDR[[1]]$m$getEnv())
  }
  minX <- -100
  maxX <- 100
  if(spe == TRUE){
    switch(EXPR = eq$id,
      "eq010" = { # janisch_32
        s <- seq(
          from = min(temp, na.rm = TRUE) - rangeT,
          to = max(temp, na.rm = TRUE) + rangeT,
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "Development rate", ...)
        graphics::lines(
          s,
          stats::predict(nlsDR, newdata = list(T = s)), ...)
      },
      "eq020" = { # davidson_44
        maxX <- temp[devRate == max(devRate, na.rm = TRUE)][!is.na(temp[devRate == max(devRate, na.rm = TRUE)])][1]
        s <- seq(
          from = min(temp, na.rm = TRUE) - rangeT,
          to = max(temp, na.rm = TRUE) + rangeT,
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "development rate",
          xlim = c(0, maxX),  ...)
        graphics::lines(
          s,
          stats::predict(nlsDR, newdata = list(T = s)), ...)
      },
      "eq030" = { # campbell_74
        minX <- -stats::coef(nlsDR)[1]/stats::coef(nlsDR)[2]
        maxX <- max(temp, na.rm = TRUE)
        s1 <- seq(
          from = min(temp, na.rm = TRUE),
          to = min(max(temp, na.rm = TRUE), maxX),
          length = 100)
        s2 <- seq(
          from = minX,
          to = min(temp, na.rm = TRUE),
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "development rate",
          xlim = range(c(s1, s2)), ...)
        graphics::lines(
          s1,
          stats::predict(nlsDR, newdata = list(T = s1)), ...)
        graphics::lines(
          s2,
          stats::predict(nlsDR, newdata = list(T = s2)),
          lty = 2, ...)
      },
      "eq040" = { # stinner_74
        s1 <- seq(
          from = min(temp, na.rm = TRUE),
          to = stats::coef(nlsDR[[2]]),
          length = 100)
        s2 <- seq(
          from = stats::coef(nlsDR[[2]]),
          to = max(temp, na.rm = TRUE),
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "development rate",
          xlim = range(c(s1, s2)), ...)
        graphics::lines(
          s1,
          stats::predict(nlsDR[[1]], newdata = list(T = s1)), ...)
        graphics::lines(
          s2,
          stats::predict(nlsDR[[2]], newdata = list(x = s2)), ...)
      },
      "eq100" = { # taylor_81
        s1 <- seq(
          from = min(temp, na.rm = TRUE),
          to = abs(stats::coef(nlsDR)[2]) + abs(stats::coef(nlsDR)[3]),
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "development rate", ...)
        graphics::lines(
          s1,
          stats::predict(nlsDR, newdata = list(T = s1)), ...)
      },
      "eq150" = { # lamb_92
        s1 <- seq(
          from = min(temp, na.rm = TRUE),
          to = stats::coef(nlsDR[[1]])[2],
          length = 100)
        s2 <- seq(
          from = stats::coef(nlsDR[[1]])[2],
          to = max(temp, na.rm = TRUE),
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "development rate",
          xlim = range(c(s1, s2)), ...)
        graphics::lines(
          s1,
          stats::predict(nlsDR[[1]], newdata = list(T = s1)), ...)
        graphics::lines(
          s2,
          stats::predict(nlsDR[[2]], newdata = list(x = s2)), ...)
      },
      { # otherwise:
        s <- seq(
          from = min(temp, na.rm = TRUE) - rangeT,
          to = max(temp, na.rm = TRUE) + rangeT,
          length = 100)
        graphics::plot(
          x = temp,
          y = devRate,
          xlab = "Temperature",
          ylab = "development rate", ...)
        graphics::lines(
          s,
          stats::predict(nlsDR, newdata = list(T = s)), ...)
      }
    )
  } else {
    s <- seq(
      from = min(temp, na.rm = TRUE) - rangeT,
      to = max(temp, na.rm = TRUE) + rangeT,
      length = 100)
    graphics::plot(
      x = temp,
      y = devRate,
      xlab = "Temperature",
      ylab = "development rate", ...)
    graphics::lines(
      s,
      stats::predict(nlsDR, newdata = list(T = s)), ...)
  }

  if(optText == TRUE){
    graphics::text(
      x = graphics::par("xaxp")[2],
      y = graphics::par("yaxp")[2],
      pos = 2,
      labels = paste0(eq$name, " (", eq$refShort, ")"), ...)
  }
}
