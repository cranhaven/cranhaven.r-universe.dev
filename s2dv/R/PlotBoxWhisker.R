#'Box-And-Whisker Plot of Time Series with Ensemble Distribution
#'
#'Produce time series of box-and-whisker plot showing the distribution of the 
#'members of a forecast vs. the observed evolution. The correlation between
#'forecast and observational data is calculated and displayed. Only works for 
#'n-monthly to n-yearly time series. 
#'
#'@param exp Forecast array of multi-member time series, e.g., the NAO index 
#'  of one experiment. The expected dimensions are 
#'  c(members, start dates/forecast horizons). A vector with only the time 
#'  dimension can also be provided. Only monthly or lower frequency time 
#'  series are supported. See parameter freq.
#'@param obs Observational vector or array of time series, e.g., the NAO index 
#'  of the observations that correspond the forecast data in \code{exp}.
#'  The expected dimensions are c(start dates/forecast horizons) or 
#'  c(1, start dates/forecast horizons). Only monthly or lower frequency time 
#'  series are supported. See parameter freq.
#'@param toptitle Character string to be drawn as figure title.
#'@param ytitle Character string to be drawn as y-axis title.
#'@param monini Number of the month of the first time step, from 1 to 12.
#'@param yearini Year of the first time step.
#'@param freq Frequency of the provided time series: 1 = yearly, 12 = monthly, 
#  4 = seasonal, ... Default = 12.
#'@param expname Experimental dataset name.
#'@param obsname Name of the observational reference dataset.
#'@param drawleg TRUE/FALSE: whether to draw the legend or not.
#'@param fileout Name of output file. Extensions allowed: eps/ps, jpeg, png, 
#'  pdf, bmp and tiff. \cr
#'  Default = 'output_PlotBox.ps'.
#'@param width File width, in the units specified in the parameter size_units 
#'  (inches by default). Takes 8 by default.
#'@param height File height, in the units specified in the parameter 
#'  size_units (inches by default). Takes 5 by default.
#'@param size_units Units of the size of the device (file or window) to plot 
#'  in. Inches ('in') by default. See ?Devices and the creator function of the 
#'  corresponding device.
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param ... Arguments to be passed to the method. Only accepts the following
#'  graphical parameters:\cr
#'  ann ask bg cex.lab cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg fig font font.axis font.lab font.main font.sub lend 
#'  lheight ljoin lmitre mex mfcol mfrow mfg mkh oma omd omi page pin plt pty 
#'  smo srt tck tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog \cr
#'  For more information about the parameters see `par`.
#'
#'@return Generates a file at the path specified via \code{fileout}.
#'
#'@seealso EOF, ProjectField, NAO
#'@keywords datagen
#'@author History:\cr
#'0.1  -  2013-09  (F. Lienert, \email{flienert@@ic3.cat})  -  Original code\cr
#'0.2  -  2015-03  (L. Batte, \email{lauriane.batte@@ic3.cat})  -  Removed all\cr
#'  normalization for sake of clarity.
#'1.0  -  2016-03  (N. Manubens, \email{nicolau.manubens@@bsc.es})  -  Formatting to R CRAN
#'@examples
#'# See examples on Load() to understand the first lines in this example
#'  \dontrun{
#'data_path <- system.file('sample_data', package = 's2dverification')
#'expA <- list(name = 'experiment', path = file.path(data_path,
#'             'model/$EXP_NAME$/$STORE_FREQ$_mean/$VAR_NAME$_3hourly',
#'             '$VAR_NAME$_$START_DATE$.nc'))
#'obsX <- list(name = 'observation', path = file.path(data_path,
#'             '$OBS_NAME$/$STORE_FREQ$_mean/$VAR_NAME$',
#'             '$VAR_NAME$_$YEAR$$MONTH$.nc'))
#'
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(expA), list(obsX), startDates,
#'                   leadtimemin = 1, leadtimemax = 4, output = 'lonlat',
#'                   latmin = 27, latmax = 48, lonmin = -12, lonmax = 40)
#'  }
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                      c('observation'), startDates,
#'                                      leadtimemin = 1,
#'                                      leadtimemax = 4,
#'                                      output = 'lonlat',
#'                                      latmin = 20, latmax = 80,
#'                                      lonmin = -80, lonmax = 40)
#'# No example data is available over NAO region, so in this example we will 
#'# tweak the available data. In a real use case, one can Load() the data over 
#'# NAO region directly.
#'sampleData$lon[] <- c(40, 280, 340)
#'sampleData$lat[] <- c(20, 80)
#'  }
#'# Now ready to compute the EOFs and project on, for example, the first 
#'# variability mode.
#'ano <- Ano_CrossValid(sampleData$mod, sampleData$obs)
#'ano_exp <- array(ano$exp, dim = dim(ano$exp)[-2])
#'ano_obs <- array(ano$obs, dim = dim(ano$obs)[-2])
#'nao <- NAO(exp = ano_exp, obs = ano_obs, lat = sampleData$lat, lon = sampleData$lon)
#'# Finally plot the nao index
#'  \dontrun{
#'nao$exp <- Reorder(nao$exp, c(2, 1))
#'nao$obs <- Reorder(nao$obs, c(2, 1))
#'PlotBoxWhisker(nao$exp, nao$obs, "NAO index, DJF", "NAO index (PC1) TOS", 
#'               monini = 12, yearini = 1985, freq = 1, "Exp. A", "Obs. X")
#'  }
#'
#'@importFrom grDevices dev.cur dev.new dev.off 
#'@importFrom stats cor
#'@export
PlotBoxWhisker <- function(exp, obs, toptitle = '', ytitle = '', monini = 1, 
                           yearini = 0, freq = 1, expname = "exp 1", 
                           obsname = "obs 1", drawleg = TRUE,
                           fileout = NULL, 
                           width = 8, height = 5, size_units = 'in', res = 100, ...) {

  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("adj", "bty", "cex", "cex.axis", "cex.main", "col", "din", "fin", "lab", "las", "lty", "lwd", "mai", "mar", "mgp", "new", "pch", "ps")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  # Checking exp
  if (is.numeric(exp)) {
    if (is.null(dim(exp)) || length(dim(exp)) == 1) {
      dim(exp) <- c(1, length(exp))
    }
  }
  if (!is.numeric(exp) || length(dim(exp)) != 2) {
    stop("Parameter 'exp' must be a numeric vector or array of dimensions c(forecast horizons/start dates) or c(ensemble members, forecast horizons/start dates)")
  }

  # Checking obs
  if (is.numeric(obs)) {
    if (is.null(dim(obs)) || length(dim(obs)) == 1) {
      dim(obs) <- c(1, length(obs))
    }
  }
  if (!is.numeric(obs) || length(dim(obs)) != 2) {
    stop("Parameter 'obs' must be a numeric vector or array of dimensions c(forecast horizons/start dates) or c(1, forecast horizons/start dates)")
  }

  # Checking consistency in exp and obs
  if (dim(exp)[2] != dim(obs)[2]) {
    stop("'exp' and 'obs' must have data for the same amount of time steps.")
  }

  if (!is.character(toptitle) || !is.character(ytitle)) {
    stop("Parameters 'ytitle' and 'toptitle' must be character strings.")
  }

  if (!is.numeric(monini)) {
    stop("'monini' must be a month number, from 1 to 12.")
  }
  if (monini < 1 || monini > 12) {
    stop("'monini' must be >= 1 and <= 12.")
  }

  if (!is.numeric(yearini)) {
    stop("'yearini' must be a month number, from 1 to 12.")
  }

  if (!is.numeric(freq)) {
    stop("'freq' must be a number <= 12.")
  }

  if (!is.character(expname) || !is.character(obsname)) {
    stop("'expname' and 'obsname' must be character strings.")
  }

  if (!is.logical(drawleg)) {
    stop("Parameter 'drawleg' must be either TRUE or FALSE.")
  }

  if (!is.character(fileout) && !is.null(fileout)) {
    stop("Parameter 'fileout' must be a character string.")
  }

  ntimesteps <- dim(exp)[2]
  lastyear <- (monini + (ntimesteps - 1) * 12 / freq - 1) %/% 12 + yearini
  lastmonth <- (monini + (ntimesteps - 1) * 12 / freq - 1) %% 12 + 1
  #
  #  Define some plot parameters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  labind <- seq(1, ntimesteps)
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
              "Oct", "Nov", "Dec")
  labyear <- ((labind - 1) * 12 / freq + monini - 1) %/% 12 + yearini
  labmonth <- months[((labind - 1) * 12 / freq + monini - 1) %% 12 + 1]
  for (jx in 1:length(labmonth)) {
    y2o3dig <- paste("0", as.character(labyear[jx]), sep = "")
    labmonth[jx] <- paste(labmonth[jx], "\nYr ", substr(y2o3dig,
                          nchar(y2o3dig) - 1, nchar(y2o3dig)), sep = "")
  }

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res, width = width, height = height)
  }

  # Load the user parameters
  par(userArgs)

  ## Observed time series.
  #pc.o <- ts(obs[1, ], deltat = 1, start = yr1, end = yr2)
  pc.o <- obs[1, ]
  ## Normalization of obs, forecast members. Fabian
  ## Normalization of forecast should be according to ensemble
  ## mean, to keep info on ensemble spread, no? Lauriane pc.o <-
  ## pc.o/sd(pc.o) sd.fc <- apply(exp,c(1),sd)
  ## exp <- exp/sd.fc mn.fc <-
  ## apply(exp,2, mean) exp <-
  ## exp/sd(mn.fc) Produce plot.
  par(mar = c(5, 6, 4, 2))
  boxplot(exp, add = FALSE, main = toptitle, 
    ylab = "", xlab = "", col = "red", lwd = 2, t = "b", 
    axes = FALSE, cex.main = 2, ylim = c(-max(abs(c(exp, pc.o))), max(abs(c(exp, pc.o)))))
  lines(1:ntimesteps, pc.o, lwd = 3, col = "blue")
  abline(h = 0, lty = 1)
  if (drawleg) {
    legend("bottomleft", c(obsname, expname), lty = c(1, 1), lwd = c(3, 
      3), pch = c(NA, NA), col = c("blue", "red"), horiz = FALSE, 
      bty = "n", inset = 0.05)
  }
  ##mtext(1, line = 3, text = tar, cex = 1.9)
  mtext(3, line = -2, text = paste(" AC =", round(cor(pc.o, 
        apply(exp, c(2), mean)), 2)), cex = 1.9, adj = 0)
  axis(2, cex.axis = 2)
  mtext(2, line = 3, text = ytitle, cex = 1.9)
  par(mgp = c(0, 4, 0))
  ##axis(1, c(1:ntimesteps), NA, cex.axis = 2)
  axis(1, seq(1, ntimesteps, by = 1), labmonth, cex.axis = 2)
  box()

  # If the graphic was saved to file, close the connection with the device
  if(!is.null(fileout)) dev.off()
}

