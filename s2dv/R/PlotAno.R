#'Plot Anomaly time series
#'
#'Plots time series of raw or smoothed anomalies of any variable output from 
#'\code{Load()} or \code{Ano()} or or \code{Ano_CrossValid()} or 
#'\code{Smoothing()}.
#'
#'@param exp_ano A numerical array containing the experimental data:\cr
#'  c(nmod/nexp, nmemb/nparam, nsdates, nltime).
#'@param obs_ano A numerical array containing the observational data:\cr
#'  c(nobs, nmemb, nsdates, nltime)
#'@param sdates A character vector of start dates in the format of 
#'  c('YYYYMMDD','YYYYMMDD').
#'@param toptitle Main title for each experiment: c('',''), optional.
#'@param ytitle Title of Y-axis for each experiment: c('',''), optional.
#'@param limits c(lower limit, upper limit): limits of the Y-axis, optional.
#'@param legends List of observational dataset names, optional.
#'@param freq 1 = yearly, 12 = monthly, 4 = seasonal, ... Default: 12.
#'@param biglab TRUE/FALSE for presentation/paper plot. Default = FALSE.
#'@param fill TRUE/FALSE if the spread between members should be filled. 
#'  Default = TRUE.
#'@param memb TRUE/FALSE if all members/only the ensemble-mean should be 
#'  plotted.\cr
#'  Default = TRUE.
#'@param ensmean TRUE/FALSE if the ensemble-mean should be plotted. 
#'  Default = TRUE.
#'@param linezero TRUE/FALSE if a line at y=0 should be added. 
#'  Default = FALSE.
#'@param points TRUE/FALSE if points instead of lines should be shown. 
#'  Default = FALSE.
#'@param vlines List of x location where to add vertical black lines, optional.
#'@param sizetit Multiplicative factor to scale title size, optional.
#'@param fileout Name of the output file for each experiment: c('',''). 
#'  Extensions allowed: eps/ps, jpeg, png, pdf, bmp and tiff. If filenames 
#'  with different extensions are passed, it will be considered only the first 
#'  one and it will be extended to the rest. The default value is NULL, which
#'  the pop-up window shows.
#'@param width File width, in the units specified in the parameter size_units 
#'  (inches by default). Takes 8 by default.
#'@param height File height, in the units specified in the parameter 
#'  size_units (inches by default). Takes 5 by default.
#'@param size_units Units of the size of the device (file or window) to plot 
#'  in. Inches ('in') by default. See ?Devices and the creator function of the 
#'  corresponding device.
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param \dots Arguments to be passed to the method. Only accepts the following
#'  graphical parameters:\cr  
#'  adj ann ask bg bty cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg fig font font.axis font.lab font.main font.sub lend 
#'  lheight ljoin lmitre mar mex mfcol mfrow mfg mkh oma omd omi page plt smo 
#'  srt tck tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog \cr
#'  For more information about the parameters see `par`.
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'smooth_ano_exp <- Smoothing(ano_exp, time_dim = 'ftime', runmeanlen = 12)
#'smooth_ano_obs <- Smoothing(ano_obs, time_dim = 'ftime', runmeanlen = 12)
#'smooth_ano_exp <- Reorder(smooth_ano_exp, c(2, 3, 4, 1))
#'smooth_ano_obs <- Reorder(smooth_ano_obs, c(2, 3, 4, 1))
#'PlotAno(smooth_ano_exp, smooth_ano_obs, startDates, 
#'        toptitle = paste('smoothed anomalies'), ytitle = c('K', 'K', 'K'), 
#'        legends = 'ERSST', biglab = FALSE)
#'
#'@importFrom grDevices dev.cur dev.new dev.off 
#'@importFrom stats ts
#'@export
PlotAno <- function(exp_ano, obs_ano = NULL, sdates, toptitle = rep('', 15),
                    ytitle = rep('', 15), limits = NULL, legends = NULL, 
                    freq = 12, biglab = FALSE, fill = TRUE, memb = TRUE, 
                    ensmean = TRUE, linezero = FALSE, points = FALSE, 
                    vlines = NULL, sizetit = 1, 
                    fileout = NULL, 
                    width = 8, height = 5, size_units = 'in', res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.axis", "cex.lab", "cex.main", "col", "fin", "lab", "las", "lty", "lwd", "mai", "mgp", "new", "pch", "pin", "ps", "pty")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  #
  #  Get some arguments
  # ~~~~~~~~~~~~~~~~~~~~
  #
  if (length(dim(exp_ano)) != 4 ) {
    stop("4 dim needed : c(nexp/nobs, nmemb, nsdates, nltime)")
  }
  nexp <- dim(exp_ano)[1]
  nmemb <- dim(exp_ano)[2]
  nleadtime <- dim(exp_ano)[4]
  nsdates <- dim(exp_ano)[3]
  if (is.null(obs_ano) == FALSE) { 
    nobs <- dim(obs_ano)[1] 
    if (length(dim(obs_ano)) != 4 ) {
      stop("4 dim needed : c(nexp/nobs, nmemb, nsdates, nltime)") 
    }
    if (dim(obs_ano)[3] != nsdates | dim(obs_ano)[4] != nleadtime ) {
      stop("obs and exp must have same number of sdates & ltimes") 
    }
  } else {
    nobs <- 0
  }
  # sdate check
  if (!all(nchar(sdates) == 8)) {
    stop ("The parameter 'sdates' must be formatted as YYYYMMDD.")
  }

  if (is.null(limits) == TRUE) {
    if (memb) {
      ll <- min(min(exp_ano, na.rm = TRUE), min(obs_ano, na.rm = TRUE), na.rm = TRUE)
      ul <- max(max(exp_ano, na.rm = TRUE), max(obs_ano, na.rm = TRUE), na.rm = TRUE)
    }
    else{
      ll <- min(min(MeanDims(exp_ano, 2), na.rm = TRUE), min(obs_ano, na.rm = TRUE), 
                na.rm = TRUE)
      ul <- max(max(MeanDims(exp_ano, 2), na.rm = TRUE), max(obs_ano, na.rm = TRUE),
                na.rm = TRUE)
    }
    if (nobs > 0) {
      if (biglab) {
        ul <- ul + 0.3 * (ul - ll)
      } else {
        ul <- ul + 0.2 * (ul - ll)
      }
    }
  } else {
    ll <- limits[1]
    ul <- limits[2]
  }
  #
  #  Define some plot parameters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  yearinit <- as.integer(substr(sdates[1], 1, 4))
  moninit <- as.integer(substr(sdates[1], 5, 6))
  lastyear <- as.integer(substr(sdates[nsdates], 1, 4)) + (moninit + (
                         nleadtime - 1) * 12 / freq - 1) %/% 12
  lastmonth <- (moninit + (nleadtime - 1) * (12 / freq) - 1) %% 12 + 1
  empty_ts <- ts(start = c(yearinit, (moninit - 1) %/% (12 / freq) + 1), 
                 end = c(lastyear, (lastmonth - 1) %/% (12 / freq) + 1), 
                 frequency = freq)
  color <- c("red4", "orange4", "lightgoldenrod4", "olivedrab4", "green4",
             "lightblue4", "dodgerblue4", "mediumpurple4", "mediumorchid4",
             "deeppink4")
  color <- c(color, color, color, color, color, color, color, color, color, 
             color, color)
  colorblock <- c("red1", "orange1", "lightgoldenrod1", "olivedrab1", "green1",
                  "lightblue1", "dodgerblue1", "mediumpurple1", "mediumorchid1",
                   "deeppink1")
  colorblock <- c(colorblock, colorblock, colorblock, colorblock, colorblock,
                  colorblock, colorblock, colorblock, colorblock, colorblock)
  type <- c(1, 3, 2, 4)
  thickness <- c(1, 3, 2, 2)
  #
  #  Loop on the experiments : one plot for each
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  for (jexp in 1:nexp) {
    #
    #  Define plot layout
    # ~~~~~~~~~~~~~~~~~~~~
    #

    # Open connection to graphical device
    if (!is.null(fileout)) {
      saveToFile(fileout[jexp])
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
      par(mai = c(0.8, 0.8, 0.5, 0.3), mgp = c(2, 0.5, 0))
      par(cex = 1.3, cex.lab = 1.5, cex.axis = 1.1)
      cexmain <- 1.5
      legsize <- 1
    }
    plot(empty_ts, ylim = c(ll, ul), xlab = "Time (years)", ylab = ytitle[jexp],
          main = toptitle[jexp], cex.main = cexmain * sizetit)
    # 
    #  Plot experimental data + all observational datasets sdate by sdate
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    for (jdate in 1:nsdates) {
      year0 <- as.integer(substr(sdates[jdate], 1, 4))
      mon0 <- as.integer(substr(sdates[jdate], 5, 6))
      start <- (year0 - yearinit) * freq + 1
      end <- start + nleadtime - 1
      var <- array(dim = c(nmemb, length(empty_ts)))
      var[, start:end] <- exp_ano[jexp, , jdate, ]
      #
      #  Compute parameters for filling max-min over members 
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
      #
      if (fill) { 
        par(new = TRUE)
        bordup <- array(dim = nleadtime)
        borddown <- array(dim = nleadtime)
        for (jt in 1:nleadtime) {
          bordup[jt] <- max(exp_ano[jexp, , jdate, jt], na.rm = TRUE)
          borddown[jt] <- min(exp_ano[jexp, , jdate, jt], na.rm = TRUE)
        }
        tmp <- c(start:end)
        xout <- is.na(bordup + borddown) 
        tmp <- tmp[which(xout == FALSE)]
        xx <- c(tmp, rev(tmp))
        bordup <- bordup[which(xout == FALSE)]
        borddown <- borddown[which(xout == FALSE)]
        yy <- c(bordup, rev(borddown))
        #
        #  Plotting 
        # ~~~~~~~~~~
        # 
        if (jdate == 1) {
          matplot(t(var), type = "l", lty = 1, lwd = 1, ylim = c(ll, ul),
                  col = color[jdate], xlab = "", ylab = "", axes = FALSE)
        } 
        # Max-min member range
        polygon(xx, yy, col = colorblock[jdate], border = NA) 
      }
      if (ensmean) {  # Ensemble-mean
        par(new = TRUE)
        if (points) {
          plot(MeanDims(t(var), 2), type = "p", lty = 1, lwd = 4, 
               ylim = c(ll, ul), col = color[jdate], xlab = "", ylab = "",
               axes = FALSE)
        } else {
          plot(MeanDims(t(var), 2), type = "l", lty = 1, lwd = 4, 
               ylim = c(ll, ul), col = color[jdate], xlab = "", ylab = "",
               axes = FALSE)
        }
      }
      if (memb) {
        par(new = TRUE)  # All members
        if (points) { 
          matpoints(t(var), type = "p", lty = 1, lwd = 1, pch = 20, 
                    ylim = c(ll, ul), col = color[jdate], xlab = "", ylab = "",
                    axes = FALSE)
        } else {
          matplot(t(var), type = "l", lty = 1, lwd = 1, ylim = c(ll, ul),
                  col = color[jdate], xlab = "", ylab = "", axes = FALSE)
        }
      }
      if (nobs > 0) {   
        for (jobs in 1:nobs) { 
          for (jmemb in 1:dim(obs_ano)[2]) {
            var <- array(dim = length(empty_ts))
            var[start:end] <- obs_ano[jobs, jmemb, jdate, ]
            par(new = TRUE)  # Observational datasets
            if (points) {
              plot(var, type = "p", lty = 1, lwd = 4, pch = 20, 
                   ylim = c(ll, ul), col = 1, ylab = "", xlab = "", 
                   axes = FALSE)
            } else {
              plot(var, lty = type[jobs], lwd = thickness[jobs], type = "l",
                   ylim = c(ll, ul), col = 1, ylab = "", xlab = "", 
                   axes = FALSE)
            }
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
        legend('topleft', legends[1:nobs], lty = 3, lwd = 10, col = 1, 
               cex = legsize)
      } else {
        legend('topleft', ul, legends[1:nobs], lty = type[1:nobs], 
               lwd = thickness[1:nobs], col = 1, cex = legsize)
      }
    }

    # If the graphic was saved to file, close the connection with the device
    if(!is.null(fileout)) dev.off()
  }
}
