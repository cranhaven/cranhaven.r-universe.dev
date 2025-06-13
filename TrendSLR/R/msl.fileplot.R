#' Plotting to file options in JPEG format.
#'
#' @param x object of class \dQuote{msl.trend} (see \code{\link{msl.trend}}) or
#' \dQuote{custom.trend} (see \code{\link{custom.trend}}).
#'
#' @param resol numeric, enables a user defined resolution in dpi from [300 to 1800]
#' where 1800 is the default setting.
#'
#' @param wdir character string, providing the name of the directory to send
#' output files (e.g., \dQuote{C:/myproject/}). If this field is left blank the function
#' will terminate with a warning message sent to the console.
#'
#' @param file_name is a character string indicating the name of the output file.
#' There is no need to include the file extension *.jpeg. If this field is left
#' blank the output file will be automatically saved in the defined  directory
#' (wdir) under the default name \dQuote{Plot1.jpeg}.
#'
#' @param type numeric, enables a user defined input to select the type of chart
#' to be plotted. 5 seperate options are available:
#'
#' \itemize{
#'   \item 1: The default setting provides a single 3 panel chart with the time
#'   series in the top panel, velocity in the middle panel and acceleration
#'   in the bottom panel (width = 160 mm, height = 210 mm);
#'   \item 2: Single panel plot of time series (width = 160 mm, height = 80 mm);
#'   \item 3: Single panel plot of velocity (width = 160 mm, height = 80 mm);
#'   \item 4: Single panel plot of acceleration (width = 160 mm, height = 80 mm); and
#'   \item 5: Alternative 2 panel chart with the time series in the top panel
#'   and velocity in the bottom panel (width = 160 mm, height = 150 mm).
#' }
#'
#' @param ci numeric, enables a user defined input to select the type of
#' confidence interval to be displayed on the plots. The default setting (ci = 1)
#' corresponds to a 95\% confidence interval whilst ci = 2 provides a 99\%
#' confidence interval.
#'
#' @param header logical, if \sQuote{TRUE} then the station_name (if provided)
#' in the \dQuote{msl.trend} (see \code{\link{msl.trend}}) or
#' \dQuote{custom.trend} (see \code{\link{custom.trend}}) object will be passed
#' to the main banner printed above the plot. If set to \sQuote{FALSE} then the
#' banner header will be excluded. Default = TRUE.
#'
#' @details This function provides report quality JPEG format summary plots for both
#' \dQuote{msl.trend} (see \code{\link{msl.trend}}) and \dQuote{custom.trend}
#' (see \code{\link{custom.trend}}) objects. The same range of alternative screen
#' plotting options are available via \code{\link{msl.screenplot}}.
#'
#' @seealso \code{\link{msl.trend}}, \code{\link{custom.trend}},
#' \code{\link{msl.screenplot}}
#'
#' @examples
#'
#' # Plot to file from "custom.trend" object
#'
#' data(t) # "custom.trend" object
#' str(t) # check object
#'
#' # -------------------------------------------------------------------------
#' # The following call to msl.fileplot can be found in the temporary
#' # directory under the file name "Plot1.jpeg".
#' # -------------------------------------------------------------------------
#'
#' wd <- tempdir() # find temp directory
#' msl.fileplot(t, wdir = wd) # default screen plot output
#'
#' @export
msl.fileplot <- function(x, resol = 1800, wdir = " ", file_name = " ", type = 1,
                         ci = 1, header = TRUE) {
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
    # wdir = " "
    # wdir not entered or entered outside range STOP
    if (wdir == " ") {
      stop("User must input a directory to send plot file: plotting terminated")
    } else {
      wdir <- wdir
    }

    # -----------------------------------------------------------------
    # header option not entered or entered outside range
    # header means banner printing of Station_name
    if (header == TRUE | header == FALSE) {
      header <- header
    } else {
      print("default header setting applied")
      header <- TRUE
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
      # type not entered or entered outside range
      if (type == 1 | type == 2 | type == 3 | type == 4 | type == 5) {
        type <- type
      } else {
        print("default type (3 Panel) setting applied")
        type <- 1
      }

      # -----------------------------------------------------------------
      # ci not entered or entered outside range
      if (ci == 1 | ci == 2) {
        # If ci not entered or entered outside range
        ci <- ci
      } else {
        print("default 95% CONFIDENCE INTERVAL setting applied")
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
      # resolution not entered or entered outside range
      if (resol >= 300 && resol <= 1800) {
        # within designated range of resolution
        resol <- resol
      } else {
        # If resol not entered or entered outside range
        print("default resolution setting (1800 dpi) applied")
        resol <- 1800
      }
      # -----------------------------------------------------------------
      # If file_name not entered
      if (file_name == " ") {
        file_name <- "Plot1.jpeg"  # default
        print("output can be found in defined directory as Plot1.jpeg")
      } else {
        file_name <- paste0(file_name,".jpeg")  # output file name
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
      # plotting routines for msl.trend and custom.trend objects (JPEG OUTPUTS)
      # -----------------------------------------------------------------
      # plot 3 charts: time series, velocity and acceleration (JPEG)
      if (type == 1) {
        if (header == TRUE) {
          out <- c(4.1, 5.5, 2.1, 0.2) # include margin for header
        } else {
          out <- c(4.1, 5.5, 0.2, 0.2) # no margin for header
        }

        grDevices::jpeg(file.path(wdir, file_name), width = 160, height = 210, units = "mm",
                        res = resol)
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
        grDevices::dev.off()  # turn off jpeg device
      }


      # -----------------------------------------------------------------
      # plot time series only
        if (type == 2) {
          if (header == TRUE) {
            out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
          } else {
            out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
          }

        grDevices::jpeg(file.path(wdir, file_name), width = 160, height = 80, units = "mm", res = resol)
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
          legend(laba, legend = "Relative Mean Sea Level (MSL)", inset = c(-0.03, -0.01),
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
                 inset = c(-0.03, -0.01), bty = "n", text.font = 2, cex = 0.85)
          legend(labb, bg = "white", legend = c("KEY", "Annual Average Data",
                                                "MSL Trend", lab1),
                 text.font = c(2, 1, 1, 1), lty = c(0, 1, 1, 1),
                 lwd = c(1, 1, 3, 8), col = c("black", "black", "black", "azure3"),
                 cex = c(0.5, 0.5, 0.5, 0.5))
        }
        par(opar)  # restore original settings
        grDevices::dev.off()  # turn off jpeg device
      }


      # -----------------------------------------------------------------
      # plot velocity series only
      if (type == 3) {
        if (header == TRUE) {
          out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
        } else {
          out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
        }
        grDevices::jpeg(file.path(wdir, file_name), width = 160, height = 80, units = "mm", res = resol)
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
          legend("topleft", legend = "Relative Velocity (MSL)", inset = c(-0.03, -0.01),
                 bty = "n", text.font = 2, cex = 0.85)
          legend("bottomright", bg = "white",
                 legend = c("KEY", "MSL Velocity", "Peak Rate", lab1),
                 text.font = c(2, 1, 1, 1),
                 lty = c(0, 1, 3, 1),
                 lwd = c(1, 2, 2, 8),
                 col = c("black", "black", "blue", "azure3"),
                 cex = c(0.5, 0.5, 0.5, 0.5))
        } else {
          legend("topleft", legend = "Velocity (MSL)", inset = c(-0.03, -0.01),
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
        grDevices::dev.off()  # turn off jpeg device
      }

      # -----------------------------------------------------------------
      # plot acceleration series only
      if (type == 4) {
        if (header == TRUE) {
          out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
        } else {
          out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
        }

        grDevices::jpeg(file.path(wdir, file_name), width = 160, height = 80, units = "mm", res = resol)
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
               inset = c(-0.03, -0.01), bty = "n", text.font = 2, cex = 0.85)
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
        grDevices::dev.off()  # turn off jpeg device
      }

      # -----------------------------------------------------------------
      # plot 2 charts: time series and velocity (JPEG)
      if (type == 5) {
        if (header == TRUE) {
          out <- c(2.6, 4.0, 1.5, 0.2) # include margin for header
        } else {
          out <- c(2.6, 4.0, 0.2, 0.2) # no margin for header
        }

        grDevices::jpeg(file.path(wdir, file_name), width = 160, height = 150, units = "mm", res = resol)
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
        grDevices::dev.off()  # turn off jpeg device
      }

}
