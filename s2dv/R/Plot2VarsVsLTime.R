#'Plot two scores with confidence intervals in a common plot
#'
#'Plot two input variables that have the same dimensions in a common plot.
#'One plot for all experiments.
#'The input variables should have dimensions (nexp/nmod, nltime).
#'
#'@param var1 Matrix of dimensions (nexp/nmod, nltime).
#'@param var2 Matrix of dimensions (nexp/nmod, nltime).
#'@param toptitle Main title, optional.
#'@param ytitle Title of Y-axis, optional.
#'@param monini Starting month between 1 and 12. Default = 1.
#'@param freq 1 = yearly, 12 = monthly, 4 = seasonal, ... Default = 12.
#'@param nticks Number of ticks and labels on the x-axis, optional.
#'@param limits c(lower limit, upper limit): limits of the Y-axis, optional.
#'@param listexp List of experiment names, up to three, optional.
#'@param listvars List of names of input variables, optional.
#'@param biglab TRUE/FALSE for presentation/paper plot. Default = FALSE.
#'@param hlines c(a, b, ...) Add horizontal black lines at Y-positions a, b, 
#'  ... The default value is NULL.
#'@param leg TRUE/FALSE if legend should be added or not to the plot. 
#'  Default = TRUE.
#'@param siglev TRUE/FALSE if significance level should replace confidence 
#'  interval.\cr 
#'  Default = FALSE.
#'@param sizetit Multiplicative factor to change title size, optional.
#'@param show_conf TRUE/FALSE to show/not confidence intervals for input 
#'  variables.
#'@param fileout Name of output file. Extensions allowed: eps/ps, jpeg, png, 
#'  pdf, bmp and tiff. The default value is NULL.
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
#'  adj ann ask bg bty cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg fig font font.axis font.lab font.main font.sub lend 
#'  lheight ljoin lmitre mar mex mfcol mfrow mfg mkh oma omd omi page pch plt 
#'  smo srt tck tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog \cr
#'  For more information about the parameters see `par`.
#'
#'@details
#'Examples of input:\cr
#'------------------\cr\cr  
#'RMSE error for a number of experiments and along lead-time: (nexp, nltime)
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'runmean_months <- 12
#'smooth_ano_exp <- Smoothing(data = ano_exp, runmeanlen = runmean_months)
#'smooth_ano_obs <- Smoothing(data = ano_obs, runmeanlen = runmean_months)
#'dim_to_mean <- 'member'  # mean along members
#'required_complete_row <- 'ftime'  # discard startdates for which there are NA leadtimes
#'leadtimes_per_startdate <- 60
#'rms <- RMS(MeanDims(smooth_ano_exp, dim_to_mean), 
#'           MeanDims(smooth_ano_obs, dim_to_mean), 
#'           comp_dim = required_complete_row, dat_dim = 'dataset',
#'           limits = c(ceiling((runmean_months + 1) / 2), 
#'           leadtimes_per_startdate - floor(runmean_months / 2)))
#'smooth_ano_exp_m_sub <- smooth_ano_exp - InsertDim(MeanDims(smooth_ano_exp, 'member', 
#'                                                            na.rm = TRUE), 
#'                                                   posdim = 3, 
#'                                                   lendim = dim(smooth_ano_exp)['member'], 
#'                                                   name = 'member')
#'suppressWarnings({
#'spread <- Spread(smooth_ano_exp_m_sub, compute_dim = c('member', 'sdate'))
#'})
#'#Combine rms outputs into one array 
#'rms_combine <- abind::abind(rms$conf.lower, rms$rms, rms$conf.upper, along = 0)
#'rms_combine <- Reorder(rms_combine, c(2, 3, 1, 4))
#'  \donttest{
#'Plot2VarsVsLTime(InsertDim(rms_combine[, , , ], 1, 1), Reorder(spread$sd, c(1, 3, 2)), 
#'                 toptitle = 'RMSE and spread', monini = 11, freq = 12, 
#'                 listexp = c('CMIP5 IC3'), listvar = c('RMSE', 'spread'))
#'  }
#'
#'@importFrom grDevices png jpeg postscript pdf svg bmp tiff postscript dev.cur dev.new dev.off
#'@importFrom stats ts 
#'@export
Plot2VarsVsLTime <- function(var1, var2, toptitle = '', ytitle = '', monini = 1,
                             freq = 12, nticks = NULL, limits = NULL, listexp =
                             c('exp1', 'exp2', 'exp3'), listvars = c('var1',
                             'var2'), biglab = FALSE, hlines = NULL, leg = TRUE,
                             siglev = FALSE, sizetit = 1, show_conf = TRUE,
                             fileout = NULL, 
                             width = 8, height = 5, size_units = 'in', res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.axis", "cex.lab", "cex.main", "col", "fin", "lab", "las", "lty", "lwd", "mai", "mgp", "new", "pin", "ps", "pty")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  #
  nvars <- 2

  if (length(dim(var1)) != length(dim(var2))) { 
    print("the two input variables should have the same dimensions")
    stop()
  }
  if (length(dim(var1)) >= 4) { 
    print("dimensions of input variables should be 3")
    stop()
  }
  nleadtime <- dim(var1)[3]
  nexp <- dim(var1)[1]
  var <- array(dim = c(nvars, nexp, 3, nleadtime))
  for (jvar in 1:nvars) {
    varname <- paste("var", as.character(jvar), sep = "")
    var[jvar, , , ] <- get(varname)
    rm(varname)
  }

  if (is.null(limits) == TRUE) {
    ll <- min(var1, na.rm = TRUE)
    ul <- max(var1, na.rm = TRUE)
    if (biglab) {
      ul <- ul + 0.4 * (ul - ll)
    } else {
      ul <- ul + 0.3 * (ul - ll)
    }
  } else {
    ll <- limits[1]
    ul <- limits[2]
  }
  lastyear <- (monini + (nleadtime - 1) * 12 / freq - 1) %/% 12
  lastmonth <- (monini + (nleadtime - 1) * 12 / freq - 1) %% 12 + 1
  empty_ts <- ts(start = c(0000, (monini - 1) %/% (12 / freq) + 1), 
                 end = c(lastyear, (lastmonth - 1) %/% (12 / freq) + 1), 
                 frequency = freq)
  empty <- array(dim = length(empty_ts))
  #
  #  Define some plot parameters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  if (is.null(nticks)) {
    if (biglab) {
      nticks <- 5
    } else {
      nticks <- 10
    }
  }
  labind <- seq(1, nleadtime, max(nleadtime %/% nticks, 1))
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
              "Oct", "Nov", "Dec")
  labyear <- ((labind - 1) * 12 / freq + monini - 1) %/% 12
  labmonth <- months[((labind - 1) * 12 / freq + monini - 1) %% 12 + 1]
  for (jx in 1:length(labmonth)) {
    y2o3dig <- paste("0", as.character(labyear[jx]), sep = "")
    labmonth[jx] <- paste(labmonth[jx], "\nYr ", substr(y2o3dig, nchar(y2o3dig)
                    - 1, nchar(y2o3dig)), sep = "")
  }
  color <- c("red1", "dodgerblue1", "green1", "orange1", "lightblue1", 
             "deeppink1", "mediumpurple1", "lightgoldenrod1", "olivedrab1",
             "mediumorchid1")
  type <- c(1, 3)
  if (siglev == TRUE) {
    lines <- c("n", "l", "n")
  }
  else{
    lines <- c("l", "l", "l")
  }
  thickness <- array(dim = c(3))
  thickness[1] <- c(1)
  thickness[2] <- c(8)
  thickness[3] <- thickness[1]
  
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
    par(mai = c(1.25, 1.4, 0.5, 1), mgp = c(4, 2.5, 0))
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
  if (is.null(hlines) != TRUE) { 
    for (jy in 1:length(hlines)) {
      par(new = TRUE) 
      abline(h = hlines[jy])
    }
  }
  #
  #  Loop on experimental data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                 
  legendnames <- array(dim = nexp * nvars)
  legendthick <- array(dim = nexp * nvars)
  legendsty <- array(dim = nexp * nvars)
  legendcol <- array(dim = nexp * nvars)
  if (show_conf == TRUE) {
    start_line <- 3
    end_line <- 1
  } else {
    start_line <- 2
    end_line <- 2
  }
  for (jint in seq(start_line, end_line, -1)) {
    ind <- 1
    for (jexp in 1:nexp) {
      for (jvar in 1:nvars) {
        par(new = TRUE)
        plot(var[jvar, jexp, jint, ], type = lines[jint], ylim = c(ll, ul), 
             col = color[jexp], lty = type[jvar], lwd = thickness[jint],
             ylab = "", xlab = "", axes = FALSE)
        legendnames[ind] <- paste(listexp[jexp], listvars[jvar])
        legendthick[ind] <- 2
        legendsty[ind] <- type[jvar]
        legendcol[ind] <- color[jexp]
        ind <- ind + 1
      }
    }
  }
  if (leg) {
    legend(1, ul, legendnames, lty = legendsty, lwd = legendthick,
           col = legendcol, cex = legsize)
  }

  # If the graphic was saved to file, close the connection with the device
  if(!is.null(fileout)) dev.off()
}
