#'Plots A Vertical Section
#'
#'Plot a (longitude,depth) or (latitude,depth) section.
#'
#'@param var Matrix to plot with (longitude/latitude, depth) dimensions.
#'@param horiz Array of longitudes or latitudes.
#'@param depth Array of depths.
#'@param toptitle Title, optional.
#'@param sizetit Multiplicative factor to increase title size, optional.
#'@param units Units, optional.
#'@param brks Colour levels, optional.
#'@param cols List of colours, optional.
#'@param axelab TRUE/FALSE, label the axis. Default = TRUE.
#'@param intydep Interval between depth ticks on y-axis. Default: 200m.
#'@param intxhoriz Interval between longitude/latitude ticks on x-axis.\cr
#'  Default: 20deg.
#'@param drawleg Draw colorbar. Default: TRUE.
#'@param width File width, in the units specified in the parameter size_units 
#'  (inches by default). Takes 8 by default.
#'@param height File height, in the units specified in the parameter 
#' size_units (inches by default). Takes 5 by default.
#'@param size_units Units of the size of the device (file or window) to plot 
#' in. Inches ('in') by default. See ?Devices and the creator function of the 
#' corresponding device.
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param fileout Name of output file. Extensions allowed: eps/ps, jpeg, png, 
#'  pdf, bmp and tiff. \cr
#'  Default = NULL
#'@param ... Arguments to be passed to the method. Only accepts the following
#'  graphical parameters:\cr
#'  adj ann ask bg bty cex.lab cex.sub cin col.axis col.lab col.main col.sub 
#'  cra crt csi cxy err family fg fig fin font font.axis font.lab font.main 
#'  font.sub lend lheight ljoin lmitre lty lwd mex mfcol mfrow mfg mkh oma omd 
#'  omi page pch pin plt pty smo srt tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs 
#'  yaxt ylbias ylog \cr
#'  For more information about the parameters see `par`.
#'
#'@examples
#'sampleData <- s2dv::sampleDepthData
#'PlotSection(sampleData$mod[1, 1, 1, 1, , ], sampleData$lat, sampleData$depth,
#'            toptitle = 'temperature 1995-11 member 0')
#'@importFrom grDevices dev.cur dev.new dev.off rainbow
#'@export
PlotSection <- function(var, horiz, depth, toptitle = '', sizetit = 1, 
                        units = '', brks = NULL, cols = NULL, axelab = TRUE, 
                        intydep = 200, intxhoriz = 20, drawleg = TRUE, 
                        fileout = NULL, width = 8, height = 5, 
                        size_units = 'in', res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.axis", "cex.main", "col", "lab", "las", "mai", "mar", "mgp", "new", "ps", "tck")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  #
  #  Input arguments 
  # ~~~~~~~~~~~~~~~~~
  #
  dims <- dim(var)
  if (length(dims) > 2) {
    stop("Only 2 dimensions expected for var : (lon,depth) or (lat,depth)")
  }
  if (dims[1] != length(horiz) | dims[2] != length(depth)) {
    if (dims[1] == length(depth) & dims[2] == length(horiz)) {
      var <- t(var)
      dims <- dim(var)
    } else {
      stop("Inconsistent var dimensions and longitudes/latitudes + depth")
    }
  }
  dhoriz <- horiz[2:dims[1]] - horiz[1:(dims[1] - 1)]
  wher <- which(dhoriz > (mean(dhoriz) + 5))
  if (length(wher) > 0) {
    horiz[(wher + 1):dims[1]] <- horiz[(wher + 1):dims[1]] - 360
  }
  horizb <- sort(horiz, index.return = TRUE)
  depthb <- sort(-abs(depth), index.return = TRUE)
  horizmin <- floor(min(horiz) / 10) * 10
  horizmax <- ceiling(max(horiz) / 10) * 10
  depmin <- min(depth)
  depmax <- max(depth)
  if (is.null(brks) == TRUE) {
    ll <- signif(min(var, na.rm = TRUE), 4)
    ul <- signif(max(var, na.rm = TRUE), 4)
    if (is.null(cols) == TRUE) {
      cols <- c("dodgerblue4", "dodgerblue1", "forestgreen", "yellowgreen",
                "white", "white", "yellow", "orange", "red", "saddlebrown")
    }
    nlev <- length(cols)
    brks <- signif(seq(ll, ul, (ul - ll) / nlev), 4)
  } else {
    if (is.null(cols) == TRUE) {
      nlev <- length(brks) - 1
      cols <- rainbow(nlev)
    } else {
      if (length(cols) != (length(brks) - 1)) {
        stop("Inconsistent colour levels / list of colours")
      }
    }
  }
  #
  #  Plotting the section
  # ~~~~~~~~~~~~~~~~~~
  #

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res, width = width, height = height)
  }

  # Load the user parameters
  par(userArgs)

  xmargin <- 0.5
  ymargin <- 0.5
  topmargin <- 3
  if (axelab) {
    ymargin <- ymargin + 2.5
    xmargin <- xmargin + 1.5
  }
  if (drawleg) {
    layout(matrix(1:2, ncol = 1, nrow = 2), heights = c(5, 1))
    xmargin <- max(xmargin - 1.8, 0)
  }
  if (toptitle == '') { 
    topmargin <- topmargin - 2.5 
  } 
  par(mar = c(xmargin, ymargin, topmargin, 0.5), cex = 1.4, 
      mgp = c(2.5, 0.5, 0), las = 1)
  image(horizb$x, depthb$x, array(0, dims), col = 'grey', breaks = c(-1, 1),
        axes = FALSE, xlab = "", ylab = "", main = toptitle, 
        cex.main = 1.5 * sizetit)
  image(horizb$x, depthb$x, var[horizb$ix, depthb$ix], col = cols, 
        breaks = brks, axes = FALSE, xlab = "", ylab = "", add = TRUE)
  if (axelab) {
    minhoriz <- ceiling(round(min(horizb$x), 0) / 10) * 10
    maxhoriz <- floor(round(max(horizb$x), 0) / 10) * 10
    axis(1, at = seq(minhoriz, maxhoriz, intxhoriz), tck = -0.02)
    maxdepth <- floor(round(max(depthb$x), 0) / 10) * 10
    axis(2, at = seq(-8000, 0, intydep), tck = -0.015)
  }
  box()
  #
  #  Colorbar
  # ~~~~~~~~~~
  #
  if (drawleg) {
    par(mar = c(1.5, ymargin, 2.5, 0.5), mgp = c(1.5, 0.3, 0), las = 1, 
        cex = 1.2)
    image(1:length(cols), 1, t(t(1:length(cols))), axes = FALSE, col = cols,
          xlab = units, ylab = '')
    box()
    axis(1, at = seq(0.5, length(brks) - 0.5, 1), labels = brks, cex.axis = 1)
  }

  # If the graphic was saved to file, close the connection with the device
  if(!is.null(fileout)) dev.off()
}
