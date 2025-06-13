#' Plotting to filescreen options.
#'
#' @param x object of class \dQuote{msl.trend} (see \code{\link{msl.trend}}) or
#' \dQuote{custom.trend} (see \code{\link{custom.trend}}).
#'
#' @param type numeric, enables a user defined input to select the type of chart
#' to be plotted. 5 seperate options are available:
#'
#' \itemize{
#'   \item 1: The default setting provides a single 3 panel chart with the time
#'   series in the top panel, velocity in the middle panel and acceleration in
#'   the bottom panel;
#'   \item 2: Single panel plot of time series;
#'   \item 3: Single panel plot of velocity;
#'   \item 4: Single panel plot of acceleration; and
#'   \item 5: Alternative 2 panel chart with the time series in the top panel
#'   and velocity in the bottom panel.
#' }
#'
#' @param ci numeric, enables a user defined input to select the type of
#' confidence interval to be displayed on the plots. The default setting (ci = 1)
#' corresponds to a 95\% confidence interval whilst ci = 2 provides a 99\%
#' confidence interval.
#'
#' @details This function provides summary plots direct to the screen for both
#' \dQuote{msl.trend} (see \code{\link{msl.trend}}) and \dQuote{custom.trend}
#' (see \code{\link{custom.trend}}) objects. The same range of alternative plotting
#' to file options (in JPEG format) are available via \code{\link{msl.fileplot}}.
#'
#' @seealso \code{\link{msl.trend}}, \code{\link{custom.trend}},
#' \code{\link{msl.fileplot}}
#'
#' @examples
#'
#' # Plot to screen from "msl.trend" object
#'
#' data(s) # "msl.trend" object
#' str(s) # check object
#'
#' msl.screenplot(s) # default screen plot output, 3 panels, 95% confidence intervals
#' msl.screenplot(s, type=2) # plot time series, 95% confidence intervals
#' msl.screenplot(s, type=3) # plot velocity, 95% confidence intervals
#' msl.screenplot(s, type=4, ci=2) # plot acceleration, 99% confidence intervals
#' msl.screenplot(s, type=5, ci=2) # 2 panels, 99% confidence intervals
#'
#' @export
msl.screenplot <- function(x, type = 1, ci = 1) {
    # -----------------------------------------------------------------
    object <- x
    summ <- object$Summary
    graphics.off()  # close active graphics windows
    # -----------------------------------------------------------------
    # object is a msl.trend or custom.trend output dataframe
    # type alternatives = c(1,2,3,4,5) type = 1 (default)
    # default (3 plots: time series, velocity, acceleration)
    # type = 2 (single plot: time series) type = 3 (single plot: velocity)
    # type = 4 (single plot: acceleration) type = 5 (double plot: time series
    # and velocity)
    # ci alternatives = c('1','2') ci = 1 (default) (confidence interval = 95%)
    # ci = 2 (confidence interval = 99%)
    # -----------------------------------------------------------------
    # If not msl.trend or custom.trend object
    if (class(object) == "msl.trend" | class(object) == "custom.trend") {
        class(object) <- class(object)
    } else {
        stop("object is not an msl.trend or custom.trend object: plotting terminated")
    }
    # -----------------------------------------------------------------
    # check if original time series contains missing values
    if (any(is.na(summ$MSL)) == TRUE) {
        p <- 0
    } else {
        p <- 1
    }

    # -----------------------------------------------------------------
    # specific settings, portions of object

      n <- length(summ[, 1])
      n2 <- n - 3
      # dataframe without NA's for acceleration plot
      summ2 <- summ[4:n2, ]

      # -----------------------------------------------------------------
      # to include margin for banner heading or not
      # header means banner printing of Station_name
      if (object$Station.Name == "Station Name not entered") { # No top margin required
        MAR <- 1 # marker for no margin on top
      } else {
        MAR <- 2 # marker for margin on top
      }

      # -----------------------------------------------------------------
      # type not entered or entered outside range
      if (type == 1 | type == 2 | type == 3 | type == 4 | type == 5) {
        type <- type
      } else {
        message("default type (3 Panel) setting applied")
        type <- 1
      }

      # -----------------------------------------------------------------
      # ci not entered or entered outside range
      if (ci == 1 | ci == 2) {
        # If ci not entered or entered outside range
        ci <- ci
      } else {
        message("default 95% CONFIDENCE INTERVAL setting applied")
        ci <- 1
      }
      # -----------------------------------------------------------------
      # Confidence interval input
      if (ci == 2) {
        ci = 2.575  # multiplication factor for 99% CI
        lab1 <- paste("99% Confidence Interval")
      } else {
        # default setting is 95% CI
        ci = 1.96
        lab1 <- paste("95% Confidence Interval")
      }
      # -----------------------------------------------------------------
      # Geocentric Velocity (conditioning parameter)
      if (object$Vertical.Land.Motion$mm.yr == "NA") { # if vlm not entered
        p2 <- 0 # conditioning parameter for NO vlm
      } else {
        p2 <- 1 # include vlm on relevant velocity charts and legends
        labe <- paste0("Vertical Land Motion = ",
                       object$Vertical.Land.Motion$mm.yr, " mm/year")  # legend with vlm
      }
      # -----------------------------------------------------------------
      # check slope of graph for locating chart 1 legends
      if (summ$Trend[n] - summ$Trend[1] < 0) {
        laba = paste("topright")  # Chart description to topright
        labb = paste("bottomleft")  # Chart key to bottomleft
      } else {
        # default setting
        laba = paste("topleft")  # Chart description to topleft
        labb = paste("bottomright")  # Chart key to bottomright
      }
      # -----------------------------------------------------------------
      if (requireNamespace("plyr", quietly = TRUE)) {
        plyr::round_any
      }
      if (n < 100) {
        # setting up x-axis plotting parameters
        xtic = 10  # year ticks on x-axis
        xlo <- plyr::round_any(min(summ[, 1]), 10, floor)
        xhi <- plyr::round_any(max(summ[, 1]), 10, ceiling)
      } else {
        # default
        xtic = 20
        xlo <- plyr::round_any(min(summ[, 1]), 20, floor)
        xhi <- plyr::round_any(max(summ[, 1]), 20, ceiling)
      }
      xlim = c(xlo, xhi)


      # -----------------------------------------------------------------
      # plotting routines for msl.trend and custom.trend objects (to screen)
      # -----------------------------------------------------------------
      # plot 3 charts: time series, velocity and acceleration (JPEG)
      if (type == 1) {
        if (MAR == 2) {
          out <- c(4.1, 5.5, 2.1, 0.2) # include margin for header
        } else {
          out <- c(4.1, 5.5, 0.2, 0.2) # no margin for header
        }
        opar <- par(no.readonly = TRUE)  # capture current settings
        on.exit(par(opar))
        par(mfrow = c(3, 1), las = 1)
        par(oma = out)
        par(mar = c(0,0,0,0), las = 1) # chart 1 (time series)
        ylen <- max(summ[, 2], na.rm = TRUE) - min(summ[, 2], na.rm = TRUE)
        if (ylen < 200) {
          # setting up y-axis plotting parameters
          ytic = 20  # year ticks on y-axis
          ylo <- plyr::round_any(min(summ[, 2], na.rm = TRUE), 20, floor)
          yhi <- plyr::round_any(max(summ[, 2], na.rm = TRUE), 20, ceiling)
        } else {
          # default
          ytic = 50
          ylo <- plyr::round_any(min(summ[, 2], na.rm = TRUE), 50, floor)
          yhi <- plyr::round_any(max(summ[, 2], na.rm = TRUE), 50, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ[, 1], summ[, 2], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ[, 1], rev(summ[, 1])),
                c((summ[, 3] + ci * summ[, 4]), rev((summ[, 3] - ci * summ[, 4]))),
                col = "azure3", border = NA)
        title(ylab = "Millimetres", outer = TRUE, font.lab = 2, cex.lab = 1.3,
              line = 3.9, adj = 0.87)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 1.1)
        if (p == 0) {
          # gap filling routine required
          lines(summ[, 1], summ[, 10], col = "red")
          lines(summ[, 1], summ[, 2])
          lines(summ[, 1], summ[, 3], lwd = 2)
          legend(laba, legend = "Relative Mean Sea Level (MSL)",
                 inset = c(-0.02, -0.01), bty = "n", text.font = 2, cex = 1.4)
          legend(labb, bg = "white",
                 legend = c("KEY", "Annual Average Data", "Gap Filling",
                            "MSL Trend", lab1, "Peak Rate"),
                 text.font = c(2, 1, 1, 1, 1, 1), lty = c(0, 1, 1, 1, 1, 3),
                 lwd = c(1, 1, 1, 3, 8, 2),
                 col = c("black", "black", "red", "black", "azure3", "blue"),
                 cex = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9))
        }
        if (p == 1) {
          # no gap filling routine required
          lines(summ[, 1], summ[, 2])
          lines(summ[, 1], summ[, 3], lwd = 2)
          legend(laba, legend = "Relative Mean Sea Level (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 1.4)
          legend(labb, bg = "white",
                 legend = c("KEY", "Annual Average Data", "MSL Trend", lab1,
                            "Peak Rate"), text.font = c(2, 1, 1, 1, 1),
                 lty = c(0, 1, 1, 1, 3), lwd = c(1, 1, 3, 8, 3),
                 col = c("black", "black", "black", "azure3", "blue"),
                 cex = c(0.9, 0.9, 0.9, 0.9, 0.9))
        }
        # -----------------------------------------------------------------
        par(mar = c(0, 0, 0, 0), las = 1)  # chart 2 (velocity)
        minV <- min((summ[, 5] - ci * summ[, 6]), summ$VelGeo, na.rm = TRUE)
        maxV <- max((summ[, 5] + ci * summ[, 6]), summ$VelGeo, na.rm = TRUE)
        ylen <- maxV - minV
        if (ylen <= 1) {
          # setting up y-axis plotting parameters
          ytic = 0.1  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.1, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.1, ceiling)
        }
        if (ylen > 1 & ylen <= 2) {
          # setting up y-axis plotting parameters
          ytic = 0.2  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.2, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.2, ceiling)
        }
        if (ylen > 2 & ylen <= 5) {
          # setting up y-axis plotting parameters
          ytic = 0.5  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.5, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.5, ceiling)
        }
        if (ylen > 5 & ylen <= 10) {
          # setting up y-axis plotting parameters
          ytic = 1  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 1, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 1, ceiling)
        }
        if (ylen > 10) {
          # setting up y-axis plotting parameters
          ytic = 2  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 2, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 2, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ[, 1], summ[, 5], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ[, 1], rev(summ[, 1])), c((summ[, 5] + ci * summ[, 6]),
                                                rev((summ[, 5] - ci * summ[, 6]))),
                col = "azure3", border = NA)
        lines(summ[, 1], summ[, 5], lwd = 2)
        abline(h = 0, lty = 2)
        abline(h = max(summ[, 5]), lty = 3, col = "blue", lwd = 2)
        if (p2 == 0) { # no vlm
          legend("topleft", legend = "Relative Velocity (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 1.4)
        } else {
          legend("topleft", legend = "Velocity (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 1.4)
          legend("bottomleft", legend = labe, inset = c(-0.005, -0.005),
                 bty = "n", text.font = 1, cex = 1.0)
          legend("bottomright", bg = "white",
                 legend = c("KEY", "Relative Velocity", "Geocentric Velocity"),
                 text.font = c(2, 1, 1),
                 lty = c(0, 1, 1),
                 lwd = c(1, 3, 3),
                 col = c("black", "black", "red"),
                 cex = c(0.9, 0.9, 0.9))
          lines(summ[, 1], summ$VelGeo, lwd = 2, col = "red")
        }
        title(ylab = "Millimetres/yr", outer = TRUE, font.lab = 2, cex.lab = 1.3,
              line = 3.9, adj = 0.5)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 1.1)
        # -----------------------------------------------------------------
        par(mar = c(0, 0, 0, 0), las = 1)  # chart 3 (acceleration)
        ylen <- max(summ2[, 7] + ci * summ2[, 8]) - min(summ2[, 7] - ci * summ2[, 8])
        if (ylen <= 0.1) {
          # setting up y-axis plotting parameters
          ytic = 0.01  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.01, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.01, ceiling)
        }
        if (ylen > 0.1 & ylen <= 0.2) {
          # setting up y-axis plotting parameters
          ytic = 0.02  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.02, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.02, ceiling)
        }
        if (ylen > 0.2 & ylen <= 0.5) {
          # setting up y-axis plotting parameters
          ytic = 0.05  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.05, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.05, ceiling)
        }
        if (ylen > 0.5 & ylen <= 1) {
          # setting up y-axis plotting parameters
          ytic = 0.1  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.1, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.1, ceiling)
        }
        if (ylen > 1) {
          # setting up y-axis plotting parameters
          ytic = 0.2  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.2, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.2, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ2[, 1], summ2[, 7], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ2[, 1],
                  rev(summ2[, 1])), c((summ2[, 7] + ci * summ2[, 8]),
                                      rev((summ2[, 7] - ci * summ2[, 8]))),
                col = "azure3", border = NA)
        lines(summ2[, 1], summ2[, 7], lwd = 2)
        abline(h = 0, lty = 2)
        abline(h = max(summ2[, 7]), lty = 3, col = "blue", lwd = 2)
        legend("topleft", legend = "Acceleration (MSL)",
               inset = c(-0.02, -0.01), bty = "n", text.font = 2, cex = 1.4)
        title(ylab = expression(paste(bold("Millimetres/yr" ^ "2"))), outer = TRUE,
              font.lab = 2, cex.lab = 1.3, line = 3.9, adj = 0.12)
        title(xlab = "Year", outer = TRUE, font.lab = 2, cex.lab = 1.3, line = 2.6)
        axis(side = 1, tck = -0.030, at = seq(xlo, xhi, by = xtic), outer = TRUE,
             labels = NA)
        axis(side = 1, at = seq(xlo, xhi, by = xtic), outer = TRUE, lwd = 0, line = 0.1,
             cex.axis = 1.1)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 1.1)
        graphics::mtext(object$Station.Name, side = 3, outer = TRUE, font = 2,
                        line = 0.35, cex = 1.2, adj = 0.5)
        graphics::par(opar)  # restore original settings

      }


      # -----------------------------------------------------------------
      # plot time series only
        if (type == 2) {
          if (MAR == 2) {
            out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
          } else {
            out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
          }
        opar <- par(no.readonly = TRUE)  # capture current settings
        on.exit(par(opar))
        par(oma = out)
        par(mar = c(0,0,0,0), las = 1)
        ylen <- max(summ[, 2], na.rm = TRUE) - min(summ[, 2], na.rm = TRUE)
        if (ylen < 200) {
          # setting up y-axis plotting parameters
          ytic = 20  # year ticks on y-axis
          ylo <- plyr::round_any(min(summ[, 2], na.rm = TRUE), 20, floor)
          yhi <- plyr::round_any(max(summ[, 2], na.rm = TRUE), 20, ceiling)
        } else {
          # default
          ytic = 50
          ylo <- plyr::round_any(min(summ[, 2], na.rm = TRUE), 50, floor)
          yhi <- plyr::round_any(max(summ[, 2], na.rm = TRUE), 50, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ[, 1], summ[, 2], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ[, 1], rev(summ[, 1])), c((summ[, 3] + ci * summ[, 4]),
                                                rev((summ[, 3] - ci * summ[, 4]))),
                col = "azure3", border = NA)
        title(ylab = "Millimetres", outer = TRUE, font.lab = 2, cex.lab = 0.9, line = 2.8)
        title(xlab = "Year", outer = TRUE, font.lab = 2, cex.lab = 0.9, line = 1.5)
        axis(side = 1, tck = -0.030, at = seq(xlo, xhi, by = xtic), outer = TRUE,
             labels = NA)
        axis(side = 1, at = seq(xlo, xhi, by = xtic), outer = TRUE, lwd = 0, line = -0.5,
             cex.axis = 0.75)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 0.75)
        graphics::mtext(object$Station.Name, side = 3, outer = TRUE, font = 2,
                        line = 0.20, cex = 1.1, adj = 0.5)
        if (p == 0) {
          # gap filling routine required
          lines(summ[, 1], summ[, 10], col = "red")
          lines(summ[, 1], summ[, 2])
          lines(summ[, 1], summ[, 3], lwd = 2)
          legend(laba, legend = "Relative Mean Sea Level (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 0.85)
          legend(labb, bg = "white", legend = c("KEY", "Annual Average Data",
                                                "Gap Filling", "MSL Trend", lab1),
                 text.font = c(2, 1, 1, 1, 1), lty = c(0, 1, 1, 1, 1),
                 lwd = c(1, 1, 1, 3, 8),
                 col = c("black", "black", "red", "black", "azure3"),
                 cex = c(0.5, 0.5, 0.5, 0.5, 0.5))
        }
        if (p == 1) {
          # no gap filling routine required
          lines(summ[, 1], summ[, 2])
          lines(summ[, 1], summ[, 3], lwd = 2)
          legend(laba, legend = "Relative Mean Sea Level (MSL)",
                 inset = c(-0.02, -0.01), bty = "n", text.font = 2, cex = 0.85)
          legend(labb, bg = "white", legend = c("KEY", "Annual Average Data",
                                                "MSL Trend", lab1),
                 text.font = c(2, 1, 1, 1), lty = c(0, 1, 1, 1),
                 lwd = c(1, 1, 3, 8), col = c("black", "black", "black", "azure3"),
                 cex = c(0.5, 0.5, 0.5, 0.5))
        }
        par(opar)  # restore original settings

      }


      # -----------------------------------------------------------------
      # plot velocity series only
      if (type == 3) {
        if (MAR == 2) {
          out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
        } else {
          out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
        }
        opar <- par(no.readonly = TRUE)  # capture current settings
        on.exit(par(opar))
        par(oma = out)
        par(mar = c(0,0,0,0), las = 1)
        minV <- min((summ[, 5] - ci * summ[, 6]), summ$VelGeo, na.rm = TRUE)
        maxV <- max((summ[, 5] + ci * summ[, 6]), summ$VelGeo, na.rm = TRUE)
        ylen <- maxV - minV
        if (ylen <= 1) {
          # setting up y-axis plotting parameters
          ytic = 0.1  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.1, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.1, ceiling)
        }
        if (ylen > 1 & ylen <= 2) {
          # setting up y-axis plotting parameters
          ytic = 0.2  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.2, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.2, ceiling)
        }
        if (ylen > 2 & ylen <= 5) {
          # setting up y-axis plotting parameters
          ytic = 0.5  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.5, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.5, ceiling)
        }
        if (ylen > 5 & ylen <= 10) {
          # setting up y-axis plotting parameters
          ytic = 1  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 1, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 1, ceiling)
        }
        if (ylen > 10) {
          # setting up y-axis plotting parameters
          ytic = 2  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 2, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 2, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ[, 1], summ[, 5], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ[, 1], rev(summ[, 1])), c((summ[, 5] + ci * summ[, 6]),
                                                rev((summ[, 5] - ci * summ[, 6]))),
                col = "azure3", border = NA)
        lines(summ[, 1], summ[, 5], lwd = 2)
        abline(h = 0, lty = 2)
        abline(h = max(summ[, 5]), lty = 3, col = "blue", lwd = 2)
        if (p2 == 0) { # no vlm
          legend("topleft", legend = "Relative Velocity (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 0.85)
          legend("bottomright", bg = "white",
                 legend = c("KEY", "MSL Velocity", "Peak Rate", lab1),
                 text.font = c(2, 1, 1, 1),
                 lty = c(0, 1, 3, 1),
                 lwd = c(1, 2, 2, 8),
                 col = c("black", "black", "blue", "azure3"),
                 cex = c(0.5, 0.5, 0.5, 0.5))
        } else {
          legend("topleft", legend = "Velocity (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 0.85)
          legend("bottomleft", legend = labe, inset = c(-0.005, -0.005),
                 bty = "n", text.font = 1, cex = 0.6)
          legend("bottomright", bg = "white",
                 legend = c("KEY", "Relative Velocity", "Peak Rate", lab1, "Geocentric Velocity"),
                 text.font = c(2, 1, 1, 1, 1),
                 lty = c(0, 1, 3, 1, 1),
                 lwd = c(1, 2, 2, 8, 2),
                 col = c("black", "black", "blue", "azure3", "red"),
                 cex = c(0.5, 0.5, 0.5, 0.5, 0.5))
          lines(summ[, 1], summ$VelGeo, lwd = 2, col = "red")
        }
        title(ylab = "Millimetres/yr", outer = TRUE, font.lab = 2, cex.lab = 0.9,
              line = 2.8)
        title(xlab = "Year", outer = TRUE, font.lab = 2, cex.lab = 0.9, line = 1.5)
        axis(side = 1, tck = -0.030, at = seq(xlo, xhi, by = xtic), outer = TRUE,
             labels = NA)
        axis(side = 1, at = seq(xlo, xhi, by = xtic), outer = TRUE, lwd = 0, line = -0.5,
             cex.axis = 0.75)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 0.75)
        graphics::mtext(object$Station.Name, side = 3, outer = TRUE, font = 2,
                        line = 0.20, cex = 1.1, adj = 0.5)
        par(opar)  # restore original settings

      }

      # -----------------------------------------------------------------
      # plot acceleration series only
      if (type == 4) {
        if (MAR == 2) {
          out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
        } else {
          out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
        }
        opar <- par(no.readonly = TRUE)  # capture current settings
        on.exit(par(opar))
        par(oma = out)
        par(mar = c(0,0,0,0), las = 1)
        ylen <- max(summ2[, 7] + ci * summ2[, 8]) - min(summ2[, 7] - ci * summ2[, 8])
        if (ylen <= 0.1) {
          # setting up y-axis plotting parameters
          ytic = 0.01  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.01, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.01, ceiling)
        }
        if (ylen > 0.1 & ylen <= 0.2) {
          # setting up y-axis plotting parameters
          ytic = 0.02  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.02, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.02, ceiling)
        }
        if (ylen > 0.2 & ylen <= 0.5) {
          # setting up y-axis plotting parameters
          ytic = 0.05  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.05, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.05, ceiling)
        }
        if (ylen > 0.5 & ylen <= 1) {
          # setting up y-axis plotting parameters
          ytic = 0.1  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.1, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.1, ceiling)
        }
        if (ylen > 1) {
          # setting up y-axis plotting parameters
          ytic = 0.2  # year ticks on x-axis
          ylo <- plyr::round_any(min(summ2[, 7] - ci * summ2[, 8]) - ylen * 0.15,
                                 0.2, floor)
          yhi <- plyr::round_any(max(summ2[, 7] + ci * summ2[, 8]) + ylen * 0.15,
                                 0.2, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ2[, 1], summ2[, 7], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ2[, 1], rev(summ2[, 1])),
                c((summ2[, 7] + ci * summ2[, 8]), rev((summ2[, 7] - ci * summ2[, 8]))),
                col = "azure3", border = NA)
        lines(summ2[, 1], summ2[, 7], lwd = 2)
        abline(h = 0, lty = 2)
        abline(h = max(summ2[, 7]), lty = 3, col = "blue", lwd = 2)
        legend("topleft", legend = "Acceleration (MSL)",
               inset = c(-0.02, -0.01), bty = "n", text.font = 2, cex = 0.85)
        legend("bottomright", bg = "white", legend = c("KEY", "MSL Acceleration",
                                                       "Peak Acceleration", lab1),
               text.font = c(2, 1, 1, 1), lty = c(0, 1, 3, 1), lwd = c(1, 2, 2, 8),
               col = c("black", "black", "blue", "azure3"),
               cex = c(0.5, 0.5, 0.5, 0.5))
        title(ylab = expression(paste(bold("Millimetres/yr" ^ "2"))), outer = TRUE,
              font.lab = 2, cex.lab = 0.9, line = 2.8)
        title(xlab = "Year", outer = TRUE, font.lab = 2, cex.lab = 0.9, line = 1.5)
        axis(side = 1, tck = -0.030, at = seq(xlo, xhi, by = xtic), outer = TRUE,
             labels = NA)
        axis(side = 1, at = seq(xlo, xhi, by = xtic), outer = TRUE, lwd = 0, line = -0.5,
             cex.axis = 0.75)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 0.75)
        graphics::mtext(object$Station.Name, side = 3, outer = TRUE, font = 2,
                        line = 0.20, cex = 1.1, adj = 0.5)
        par(opar)  # restore original settings

      }

      # -----------------------------------------------------------------
      # plot 2 charts: time series and velocity (JPEG)
      if (type == 5) {
        if (MAR == 2) {
          out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
        } else {
          out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
        }
        opar <- par(no.readonly = TRUE)  # capture current settings
        on.exit(par(opar))
        par(mfrow = c(2, 1), las = 1)
        par(oma = out)
        par(mar = c(0,0,0,0), las = 1) # chart 1 (time series)
        ylen <- max(summ[, 2], na.rm = TRUE) - min(summ[, 2], na.rm = TRUE)
        if (ylen < 200) {
          # setting up y-axis plotting parameters
          ytic = 20  # year ticks on y-axis
          ylo <- plyr::round_any(min(summ[, 2], na.rm = TRUE), 20, floor)
          yhi <- plyr::round_any(max(summ[, 2], na.rm = TRUE), 20, ceiling)
        } else {
          # default
          ytic = 50
          ylo <- plyr::round_any(min(summ[, 2], na.rm = TRUE), 50, floor)
          yhi <- plyr::round_any(max(summ[, 2], na.rm = TRUE), 50, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ[, 1], summ[, 2], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ[, 1], rev(summ[, 1])),
                c((summ[, 3] + ci * summ[, 4]), rev((summ[, 3] - ci * summ[, 4]))),
                col = "azure3", border = NA)
        title(ylab = "Millimetres", outer = TRUE, font.lab = 2, cex.lab = 0.9,
              line = 3.0, adj = 0.8)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 0.75)
        if (p == 0) {
          # gap filling routine required
          lines(summ[, 1], summ[, 10], col = "red")
          lines(summ[, 1], summ[, 2])
          lines(summ[, 1], summ[, 3], lwd = 2)
          legend(laba, legend = "Relative Mean Sea Level (MSL)",
                 inset = c(-0.02, -0.01), bty = "n", text.font = 2, cex = 0.85)
          legend(labb, bg = "white",
                 legend = c("KEY", "Annual Average Data", "Gap Filling",
                            "MSL Trend", lab1, "Peak Rate"),
                 text.font = c(2, 1, 1, 1, 1, 1), lty = c(0, 1, 1, 1, 1, 3),
                 lwd = c(1, 1, 1, 2, 8, 2),
                 col = c("black", "black", "red", "black", "azure3", "blue"),
                 cex = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
        }
        if (p == 1) {
          # no gap filling routine required
          lines(summ[, 1], summ[, 2])
          lines(summ[, 1], summ[, 3], lwd = 2)
          legend(laba, legend = "Relative Mean Sea Level (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 0.85)
          legend(labb, bg = "white",
                 legend = c("KEY", "Annual Average Data", "MSL Trend", lab1,
                            "Peak Rate"), text.font = c(2, 1, 1, 1, 1),
                 lty = c(0, 1, 1, 1, 3), lwd = c(1, 1, 2, 8, 2),
                 col = c("black", "black", "black", "azure3", "blue"),
                 cex = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
        }
        # -----------------------------------------------------------------
        par(mar = c(0,0,0,0), las = 1)  # chart 2 (velocity)
        minV <- min((summ[, 5] - ci * summ[, 6]), summ$VelGeo, na.rm = TRUE)
        maxV <- max((summ[, 5] + ci * summ[, 6]), summ$VelGeo, na.rm = TRUE)
        ylen <- maxV - minV
        if (ylen <= 1) {
          # setting up y-axis plotting parameters
          ytic = 0.1  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.1, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.1, ceiling)
        }
        if (ylen > 1 & ylen <= 2) {
          # setting up y-axis plotting parameters
          ytic = 0.2  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.2, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.2, ceiling)
        }
        if (ylen > 2 & ylen <= 5) {
          # setting up y-axis plotting parameters
          ytic = 0.5  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 0.5, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 0.5, ceiling)
        }
        if (ylen > 5 & ylen <= 10) {
          # setting up y-axis plotting parameters
          ytic = 1  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 1, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 1, ceiling)
        }
        if (ylen > 10) {
          # setting up y-axis plotting parameters
          ytic = 2  # year ticks on x-axis
          ylo <- plyr::round_any(minV - ylen * 0.15, 2, floor)
          yhi <- plyr::round_any(maxV + ylen * 0.15, 2, ceiling)
        }
        ylim = c(ylo, yhi)
        plot(summ[, 1], summ[, 5], type = "n", axes = F, lty = 0, xlim = xlim,
             ylim = ylim, xlab = NA, ylab = NA)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = "lightcyan1")
        graphics::box()
        polygon(c(summ[, 1], rev(summ[, 1])), c((summ[, 5] + ci * summ[, 6]),
                                                rev((summ[, 5] - ci * summ[, 6]))),
                col = "azure3", border = NA)
        lines(summ[, 1], summ[, 5], lwd = 2)
        abline(h = 0, lty = 2)
        abline(h = max(summ[, 5]), lty = 3, col = "blue", lwd = 2)
        if (p2 == 0) { # no vlm
          legend("topleft", legend = "Relative Velocity (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 0.9)
        }
        if (p2 == 1) { # vlm
          legend("topleft", legend = "Velocity (MSL)", inset = c(-0.02, -0.01),
                 bty = "n", text.font = 2, cex = 0.85)
          legend("topright", legend = labe, inset = c(0.03, 0.01),
                 bty = "n", text.font = 1, cex = 0.6)
          legend("bottomright", bg = "white",
                 legend = c("KEY", "Relative Velocity", "Geocentric Velocity"),
                 text.font = c(2, 1, 1),
                 lty = c(0, 1, 1),
                 lwd = c(1, 3, 3),
                 col = c("black", "black", "red"),
                 cex = c(0.5, 0.5, 0.5))
          lines(summ[, 1], summ$VelGeo, lwd = 2, col = "red")
        }
        title(ylab = "Millimetres/yr", outer = TRUE, font.lab = 2, cex.lab = 0.9,
              line = 3.0, adj = 0.2)
        title(xlab = "Year", outer = TRUE, font.lab = 2, cex.lab = 0.9, line = 1.5)
        axis(side = 1, tck = -0.030, at = seq(xlo, xhi, by = xtic), outer = TRUE,
             labels = NA)
        axis(side = 1, at = seq(xlo, xhi, by = xtic), outer = TRUE, lwd = 0, line = -0.5,
             cex.axis = 0.75)
        axis(side = 2, tck = -0.025, at = seq(ylo, yhi, by = ytic), outer = TRUE,
             labels = NA)
        axis(side = 2, at = seq(ylo, yhi, by = ytic), cex.axis = 0.75)
        graphics::mtext(object$Station.Name, side = 3, outer = TRUE, font = 2,
                        line = 0.20, cex = 1.1, adj = 0.5)
        graphics::par(opar)  # restore original settings

      }

}
