#'Plot Plumes/Timeseries Of Anomaly Correlation Coefficients
#'
#'Plots plumes/timeseries of ACC from an array with dimensions 
#'(output from \code{ACC()}): \cr
#'c(nexp, nobs, nsdates, nltime, 4)\cr
#'where the fourth dimension is of length 4 and contains the lower limit of 
#'the 95\% confidence interval, the ACC, the upper limit of the 95\% 
#'confidence interval and the 95\% significance level given by a one-sided 
#'T-test.
#'
#'@param ACC An ACC array with with dimensions:\cr
#'  c(nexp, nobs, nsdates, nltime, 4)\cr
#'  with the fourth dimension of length 4 containing the lower limit of the 
#'  95\% confidence interval, the ACC, the upper limit of the 95\% confidence 
#'  interval and the 95\% significance level.
#'@param sdates A character vector of startdates: c('YYYYMMDD','YYYYMMDD').
#'@param toptitle A character string of the main title, optional.
#'@param sizetit A multiplicative factor to scale title size, optional.
#'@param ytitle A character string of the title of Y-axis for each experiment:
#'  c('', ''), optional.
#'@param limits A numeric vector c(lower limit, upper limit): limits of the 
#'  Y-axis, optional.
#'@param legends A character vector of flags to be written in the legend, 
#'  optional.
#'@param freq A integer: 1 = yearly, 12 = monthly, 4 = seasonal, ... Default: 12.
#'@param biglab A logical value for presentation/paper plot, Default = FALSE.
#'@param fill A logical value if filled confidence interval. Default = FALSE.
#'@param linezero A logical value if a line at y=0 should be added. Default = FALSE.
#'@param points A logical value if points instead of lines. Default = TRUE.\cr
#'  Must be TRUE if only 1 leadtime.
#'@param vlines A vector of x location where to add vertical black lines, optional.
#'@param fileout A character string of the output file name. Extensions allowed: 
#'  eps/ps, jpeg, png, pdf, bmp and tiff. Default is NULL.
#'@param width A numeric of the file width, in the units specified in the 
#'  parameter size_units (inches by default). Takes 8 by default.
#'@param height A numeric of the file height, in the units specified in the parameter 
#'  size_units (inches by default). Takes 5 by default.
#'@param size_units A character string of the units of the size of the device 
#'  (file or window) to plot in. Inches ('in') by default. See ?Devices and the 
#'  creator function of the corresponding device.
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param \dots Arguments to be passed to the method. Only accepts the following
#'  graphical parameters:\cr
#'  adj ann ask bg bty cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg fig fin font font.axis font.lab font.main font.sub 
#'  lend lheight ljoin lmitre mar mex mfcol mfrow mfg mkh oma omd omi page 
#'  plt smo srt tck tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog\cr
#'  For more information about the parameters see `par`.
#'
#'@examples
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                                c('observation'), startDates,
#'                                                leadtimemin = 1,
#'                                                leadtimemax = 4,
#'                                                output = 'lonlat',
#'                                                latmin = 27, latmax = 48,
#'                                                lonmin = -12, lonmax = 40)
#'  }
#'sampleData$mod <- Season(sampleData$mod, monini = 11, moninf = 12, monsup = 2)
#'sampleData$obs <- Season(sampleData$obs, monini = 11, moninf = 12, monsup = 2) 
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)

#'acc <- ACC(ano_exp, ano_obs, lat = sampleData$lat, dat_dim = 'dataset')
#'acc_bootstrap <- ACC(ano_exp, ano_obs, lat = sampleData$lat, conftype = 'bootstrap', 
#'                     dat_dim = 'dataset')
#'# Combine acc results for PlotACC
#'res <- array(c(acc$conf.lower, acc$acc, acc$conf.upper, acc$p.val), 
#'             dim = c(dim(acc$acc), 4))
#'res_bootstrap <- array(c(acc$acc_conf.lower, acc$acc, acc$acc_conf.upper, acc$p.val),
#'                       dim = c(dim(acc$acc), 4))
#'  \donttest{
#'PlotACC(res, startDates)
#'PlotACC(res_bootstrap, startDates)
#'  }
#'@importFrom grDevices dev.cur dev.new dev.off 
#'@importFrom stats ts
#'@export
PlotACC <- function(ACC, sdates, toptitle = "", sizetit = 1, ytitle = "", 
                    limits = NULL, legends = NULL, freq = 12, biglab = FALSE, 
                    fill = FALSE, linezero = FALSE, points = TRUE, vlines = NULL, 
                    fileout = NULL, 
                    width = 8, height = 5, size_units = 'in', res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.axis", "cex.lab", "cex.main", "col", "lab", "las", "lty",
                    "lwd", "mai", "mgp", "new", "pch", "pin", "ps", "pty")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  #
  if (length(dim(ACC)) != 5 | dim(ACC)[5] != 4) {
    stop("5 dim needed : c(nexp, nobs, nsdates, nltime, 4)")
  }
  nexp <- dim(ACC)[1]
  nobs <- dim(ACC)[2]
  nleadtime <- dim(ACC)[4]
  nsdates <- dim(ACC)[3]
  if (is.null(limits) == TRUE) {
    ll <- min(ACC, na.rm = TRUE)
    ul <- max(ACC, na.rm = TRUE)
    if (biglab) {
      ul <- ul + 0.3 * (ul - ll)
    } else {
      ul <- ul + 0.2 * (ul - ll)
    }
  } else {
    ll <- limits[1]
    ul <- limits[2]
  }
  yearinit <- as.integer(substr(sdates[1], 1, 4))
  moninit <- as.integer(substr(sdates[1], 5, 6))
  lastyear <- as.integer(substr(sdates[nsdates], 1, 4)) + (moninit + (
                         nleadtime - 1) * 12 / freq - 1) %/% 12
  lastmonth <- (moninit + (nleadtime - 1) * (12 / freq) - 1) %% 12 + 1
  empty_ts <- ts(start = c(yearinit, (moninit - 1) %/% (12 / freq) + 1), 
                 end = c(lastyear, (lastmonth - 1) %/% (12 / freq) + 1), 
                 frequency = freq)
  color <- c("red4", "dodgerblue4", "lightgoldenrod4", "deeppink4", 
             "mediumpurple4", "green4", "orange4", "lightblue4", "mediumorchid4", 
             "olivedrab4")
  colorblock <- c("red1", "dodgerblue1", "lightgoldenrod1", "deeppink1", 
                  "mediumpurple1", "green1", "orange1", "lightblue1", 
                  "mediumorchid1", "olivedrab1")

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res, width = width, height = height)
  }

  # Load the user parameters
  par(userArgs)

  if (biglab) {
    par(mai = c(1, 1.1, 0.5, 0), mgp = c(2.8, 0.9, 0))
    par(cex = 1.3, cex.lab = 2, cex.axis = 1.8)
    cexmain <- 2.2
    legsize <- 1.5
  } else {
    par(mai = c(0.8, 0.8, 0.5, 0.1), mgp = c(2, 0.5, 0))
    par(cex = 1.3, cex.lab = 1.5, cex.axis = 1.1)
    cexmain <- 1.5
    legsize <- 1
  }
  plot(empty_ts, ylim = c(ll, ul), xlab = "Time (years)", ylab = ytitle, 
       main = toptitle, cex.main = cexmain * sizetit)
  for (jexp in 1:nexp) {
    for (jobs in 1:nobs) {
      numcol <- jobs + (jexp - 1) * nobs
      for (jdate in 1:nsdates) {
        year0 <- as.integer(substr(sdates[jdate], 1, 4))
        mon0 <- as.integer(substr(sdates[jdate], 5, 6))
        start <- (year0 - yearinit) * freq + 1
        end <- start + nleadtime - 1
        var <- array(dim = c(3, length(empty_ts)))
        var[, start:end] <- t(ACC[jexp, jobs, jdate, , 1:3])
        if (fill) {
          par(new = TRUE)
          bordup <- ACC[jexp, jobs, jdate, , 3]
          borddown <- ACC[jexp, jobs, jdate, , 1]
          tmp <- c(start:end)
          xout <- is.na(bordup + borddown)
          tmp <- tmp[which(xout == FALSE)]
          xx <- c(tmp, rev(tmp))
          bordup <- bordup[which(xout == FALSE)]
          borddown <- borddown[which(xout == FALSE)]
          yy <- c(bordup, rev(borddown))
          if (jdate == 1) {
            matplot(t(var), type = "l", lty = 1, lwd = 1, ylim = c(ll, ul), 
                    col = color[numcol], xlab = "", ylab = "", axes = FALSE)
          }
          polygon(xx, yy, col = colorblock[numcol], border = NA)
        }
        if (points) {
          par(new = TRUE)
          plot(var[2, ], type = "p", lty = 1, lwd = 6, ylim = c(ll, ul), 
               col = color[numcol], xlab = "", ylab = "", axes = FALSE,
               cex = 0.6)
          par(new = TRUE)
          plot(var[1, ], type = "p", pch = 6, lwd = 3, ylim = c(ll, ul), 
               col = color[numcol], xlab = "", ylab = "", axes = FALSE,
               cex = 0.6)
          par(new = TRUE)
          plot(var[3, ], type = "p", pch = 2, lwd = 3, ylim = c(ll, ul), 
               col = color[numcol], xlab = "", ylab = "", axes = FALSE,
               cex = 0.6)
          par(new = TRUE)
          for (jind in start:end) {
            lines(c(jind, jind), var[c(1, 3), jind], lwd = 1, 
                  ylim = c(ll, ul), col = color[numcol], xlab = "", 
                  ylab = "", axes = FALSE)
          }
        } else {
          par(new = TRUE)
          plot(var[2, ], type = "l", lty = 1, lwd = 4, ylim = c(ll, ul), 
               col = color[numcol], xlab = "", ylab = "", axes = FALSE)
          par(new = TRUE)
          plot(var[1, ], type = "l", lty = 1, lwd = 1, ylim = c(ll, ul), 
               col = color[numcol], xlab = "", ylab = "", axes = FALSE)
          par(new = TRUE)
          plot(var[3, ], type = "l", lty = 1, lwd = 1, ylim = c(ll, ul), 
               col = color[numcol], xlab = "", ylab = "", axes = FALSE)
        }
      }
    }
  }
  if (linezero) {
    abline(h = 0, col = "black")
  }
  if (is.null(vlines) == FALSE) {
    for (x in vlines) {
      abline(v = x, col = "black")
    }
  }
  if (is.null(legends) == FALSE) {
    if (points) {
      legend(0, ul, legends[1:(nobs * nexp)], lty = 3, lwd = 10, 
             col = color[1:(nobs * nexp)], cex = legsize)
    } else {
      legend(0, ul, legends[1:(nobs * nexp)], lty = 1, lwd = 4, 
             col = color[1:(nobs * nexp)], cex = legsize)
    }
  }

  # If the graphic was saved to file, close the connection with the device
  if(!is.null(fileout)) dev.off()
}
