#'Plots Climatologies
#'
#'Plots climatologies as a function of the forecast time for any index output 
#'from \code{Clim()} and organized in matrix with dimensions:\cr
#'c(nmod/nexp, nmemb/nparam, nltime) or c(nmod/nexp, nltime) for the 
#'experiment data\cr
#'c(nobs, nmemb, nltime) or c(nobs, nltime) for the observational data
#'
#'@param exp_clim Matrix containing the experimental data with dimensions:\cr
#'  c(nmod/nexp, nmemb/nparam, nltime) or c(nmod/nexp, nltime) 
#'@param obs_clim Matrix containing the observational data (optional) with 
#'  dimensions:\cr
#'  c(nobs, nmemb, nltime) or c(nobs, nltime) 
#'@param toptitle Main title, optional.
#'@param ytitle Title of Y-axis, optional.
#'@param monini Starting month between 1 and 12. Default = 1.
#'@param freq 1 = yearly, 12 = monthly, 4 = seasonal, ... Default = 12.
#'@param limits c(lower limit, upper limit): limits of the Y-axis, optional.
#'@param listexp List of experiment names, optional.
#'@param listobs List of observational dataset names, optional.
#'@param biglab TRUE/FALSE for presentation/paper plot. Default = FALSE.
#'@param leg TRUE/FALSE to plot the legend or not.
#'@param sizetit Multiplicative factor to scale title size, optional.
#'@param width File width, in the units specified in the parameter size_units 
#'  (inches by default). Takes 8 by default.
#'@param height File height, in the units specified in the parameter 
#'  size_units (inches by default). Takes 5 by default.
#'@param size_units Units of the size of the device (file or window) to plot 
#'  in. Inches ('in') by default. See ?Devices and the creator function of the 
#'  corresponding device.
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param fileout Name of output file. Extensions allowed: eps/ps, jpeg, png, 
#'  pdf, bmp and tiff. The default value is NULL, which the figure is shown
#'  in a pop-up window.
#'@param ... Arguments to be passed to the method. Only accepts the following
#'  graphical parameters:\cr
#'  adj ann ask bg bty cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg fig font font.axis font.lab font.main font.sub lend 
#'  lheight ljoin lmitre mar mex mfcol mfrow mfg mkh oma omd omi page pch plt 
#'  smo srt tck usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog \cr
#'  For more information about the parameters see `par`.
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'PlotClim(clim$clim_exp, clim$clim_obs, toptitle = paste('climatologies'), 
#'         ytitle = 'K', monini = 11, listexp = c('CMIP5 IC3'), 
#'         listobs = c('ERSST'), biglab = FALSE, fileout = NULL)
#'
#'@importFrom grDevices dev.cur dev.new dev.off 
#'@importFrom stats ts
#'@export
PlotClim <- function(exp_clim, obs_clim = NULL, toptitle = '', ytitle = '', 
                     monini = 1, freq = 12, limits = NULL, 
                     listexp = c('exp1', 'exp2', 'exp3'), 
                     listobs = c('obs1', 'obs2', 'obs3'), biglab = FALSE, 
                     leg = TRUE, sizetit = 1, fileout = NULL,
                     width = 8, height = 5, size_units = 'in', res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.axis", "cex.lab", "cex.main", "col", "fin", "lab", "las", "lty", "lwd", "mai", "mgp", "new", "pin", "ps", "pty", "tcl")
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
  if (length(dim(exp_clim)) != 2 & length(dim(exp_clim)) != 3 ) {
    stop("2 or 3 dim needed : c(nexp, nltime) or c(nexp, nmemb, nltime)")
  }
  if (length(dim(exp_clim)) < 3) {
    exp_clim <- InsertDim(exp_clim, 2, 1)
  }
  nleadtime <- dim(exp_clim)[3]
  nexp <- dim(exp_clim)[1]
  if (is.null(obs_clim)) { 
    nobs <- 0
  } else { 
    nobs <- dim(obs_clim)[1]
    if (length(dim(obs_clim)) != 2 & length(dim(obs_clim)) != 3 ) {
      stop("2 or 3 dim needed : c(nobs, nltime) or c(nobs, nmemb, nltime)")
    } 
    if (length(dim(obs_clim)) < 3) {
      obs_clim <- InsertDim(obs_clim, 2, 1)
    }
    if (dim(obs_clim)[3] != nleadtime) {
      stop("obs and exp must have same number of ltimes")
    }
  }
  if (is.null(limits) == TRUE) {
    ll <- min(min(exp_clim, na.rm = TRUE), min(obs_clim, na.rm = TRUE), na.rm = TRUE)
    ul <- max(max(exp_clim, na.rm = TRUE), max(obs_clim, na.rm = TRUE), na.rm = TRUE)
    if (biglab) {
      ul <- ul + 0.3 * (ul - ll)
    } else {
      ul <- ul + 0.2 * (ul - ll)
    }
  } else {
    ll <- limits[1]
    ul <- limits[2]
  }
  lastyear <- (monini + (nleadtime - 1) * 12 / freq - 1) %/% 12
  lastmonth <- (monini + (nleadtime - 1) * 12 / freq - 1) %% 12 + 1
  #
  #  Define some plot parameters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  if (biglab) {
    labind <- seq(1, nleadtime, max(nleadtime %/% 5, 1))
  } else {
    labind <- seq(1, nleadtime, max(nleadtime %/% 10, 1))
  }
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
              "Oct", "Nov", "Dec")
  labyear <- ((labind - 1) * 12 / freq + monini - 1) %/% 12
  labmonth <- months[((labind - 1) * 12 / freq + monini - 1) %% 12 + 1]
  for (jx in 1:length(labmonth)) {
    y2o3dig <- paste("0", as.character(labyear[jx]), sep = "") 
    labmonth[jx] <- paste(labmonth[jx], "\nYr ", substr(y2o3dig,
                          nchar(y2o3dig) - 1, nchar(y2o3dig)), sep = "")
  }
  empty_ts <- ts(start = c(0000, (monini - 1) %/% (12 / freq) + 1), 
                 end = c(lastyear, (lastmonth - 1) %/% (12 / freq) + 1), 
                 frequency = freq)
  empty <- array(dim = length(empty_ts))
  color <- c("red1", "dodgerblue1", "green1", "orange1", "lightblue1",
             "deeppink1", "mediumpurple1", "lightgoldenrod1", "olivedrab1", 
             "mediumorchid1")
  type <- c(1, 3, 2, 4)
  thickness <- c(1, 3, 1, 2)
  #
  #  Define plot layout
  # ~~~~~~~~~~~~~~~~~~~~
  # 

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res, width = width, height = height)
  }

  # Load the user parameters
  par(userArgs)

  if (biglab) {
    par(mai = c(1.25, 1.4, 0.5, 0), mgp = c(4, 2.5, 0))
    par(cex = 1.3, cex.lab = 2, cex.axis = 1.8)
    cexmain <- 2.2
    legsize <- 1.5
  } else {
    par(mai = c(1, 1.1, 0.5, 0), mgp = c(3, 1.8, 0))
    par(cex = 1.3, cex.lab = 1.5, cex.axis = 1.1)
    cexmain <- 1.5
    legsize <- 1
  }
  plot(empty, ylim = c(ll, ul), xlab = "Time (months)", ylab = ytitle,
       main = toptitle, cex.main = cexmain * sizetit, axes = FALSE)
  axis(1, at = labind, labels = labmonth)
  axis(2)
  box()
  #
  #  Loops on experimental and observational data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  for (jexp in 1:nexp) {
    for (jmemb in 1:dim(exp_clim)[2]) {
      par(new = TRUE)
      plot(exp_clim[jexp, jmemb, ], type = "l", lty = 1, lwd = 2, 
           ylim = c(ll, ul), col = color[jexp], ylab = "", xlab = "", 
           axes = FALSE)
    }
  }
  if (nobs > 0) {
    for (jobs in 1:nobs) {
      for (jmemb in 1:dim(obs_clim)[2]) {
        par(new = TRUE)
        plot(obs_clim[jobs, jmemb, ], lty = type[jobs], lwd = thickness[jobs],
             type = "l", ylim = c(ll, ul), col = 1, ylab = "", xlab = "", 
             axes = FALSE)
      }
    }
    if (leg) {
      legend(1, ul, c(listexp[1:nexp], listobs[1:nobs]), 
             lty = c(array(1, dim = nexp), type[1:nobs]),
             lwd = c(array(2, dim = nexp), thickness[1:nobs]),
             col = c(color[1:nexp], array(1, dim = nobs)), cex = legsize)
    }
  } else {
    if (leg) {
      legend(1, ul, listexp[1:nexp], lty = 1, lwd = 2, col = color[1:nexp],
             cex = legsize)
    }
  }
  
  # If the graphic was saved to file, close the connection with the device
  if(!is.null(fileout)) dev.off()
}
