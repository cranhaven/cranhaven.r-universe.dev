#'Function to convert any numerical table to a grid of coloured squares.
#'
#'This function converts a numerical data matrix into a coloured 
#'grid. It is useful for a slide or article to present tabular results as 
#'colors instead of numbers.
#'
#'@param var A numerical matrix containing the values to be displayed in a 
#'  colored image.
#'@param brks A vector of the color bar intervals. The length must be one more 
#'  than the parameter 'cols'. Use ColorBar() to generate default values.
#'@param cols A vector of valid color identifiers for color bar. The length
#'  must be one less than the parameter 'brks'. Use ColorBar() to generate 
#'  default values.
#'@param toptitle A string of the title of the grid. Set NULL as default. 
#'@param title.color A string of valid color identifier to decide the title 
#'  color. Set "royalblue4" as default.
#'@param xtitle A string of title of the x-axis. Set NULL as default.
#'@param ytitle A string of title of the y-axis. Set NULL as default.
#'@param xlabels A vector of labels of the x-axis. The length must be 
#'  length of the column of parameter 'var'. Set the sequence from 1 to the 
#'  length of the column of parameter 'var' as default.
#'@param xvert A logical value to decide whether to place x-axis labels 
#'  vertically. Set FALSE as default, which keeps the labels horizontally. 
#'@param ylabels A vector of labels of the y-axis The length must be 
#'  length of the row of parameter 'var'. Set the sequence from 1 to the 
#'  length of the row of parameter 'var' as default.
#'@param line An integer specifying the distance between the title of the 
#'  x-axis and the x-axis. Set 3 as default. Adjust if the x-axis labels 
#'  are long.
#'@param figure.width A positive number as a ratio adjusting the width of the 
#'  grids. Set 1 as default.
#'@param legend A logical value to decide to draw the grid color legend or not. 
#'  Set TRUE as default.
#'@param legend.width A number between 0 and 0.5 to adjust the legend width.
#'  Set 0.15 as default.
#'@param xlab_dist A number specifying the distance between the x labels and 
#'  the x axis. If not specified, it equals to -1 - (nrow(var) / 10 - 1).
#'@param ylab_dist A number specifying the distance between the y labels and 
#'  the y axis. If not specified, it equals to 0.5 - ncol(var) / 10.
#'@param fileout A string of full directory path and file name indicating where 
#'  to save the plot. If not specified (default), a graphics device will pop up. 
#'@param size_units A string indicating the units of the size of the device 
#'  (file or window) to plot in. Set 'px' as default. See ?Devices and the 
#'  creator function of the corresponding device.
#'@param res A positive number indicating resolution of the device (file or window) 
#'  to plot in. See ?Devices and the creator function of the corresponding device.
#'@param ... The additional parameters to be passed to function ColorBar() in 
#'  s2dv for color legend creation.
#'@return A figure in popup window by default, or saved to the specified path.
#'
#'@examples 
#'#Example with random data
#' PlotMatrix(var = matrix(rnorm(n = 120, mean = 0.3), 10, 12),
#'            cols = c('white','#fef0d9','#fdd49e','#fdbb84','#fc8d59',
#'                       '#e34a33','#b30000', '#7f0000'),
#'            brks = c(-1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 1),
#'            toptitle = "Mean Absolute Error", 
#'            xtitle = "Forecast time (month)", ytitle = "Start date",
#'            xlabels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
#'                        "Aug", "Sep", "Oct", "Nov", "Dec"))
#'@importFrom grDevices dev.new dev.off dev.cur 
#'@export
PlotMatrix <- function(var, brks = NULL, cols = NULL, 
                       toptitle = NULL, title.color = "royalblue4", 
                       xtitle = NULL, ytitle = NULL, xlabels = NULL, xvert = FALSE, 
                       ylabels = NULL, line = 3, figure.width = 1, legend = TRUE, 
                       legend.width = 0.15, xlab_dist = NULL, ylab_dist = NULL, 
                       fileout = NULL, size_units = 'px', res = 100, ...) {

  # Check variables:  
  if (!is.matrix(var))
    stop("Input values are not a matrix")
  if (!is.numeric(var))
    stop("Input values are not always numbers")

  # Build: brks, cols 
  colorbar <- ColorBar(brks = brks, cols = cols, vertical = FALSE, 
                       plot = FALSE, ...) 
  brks <- colorbar$brks
  cols <- colorbar$cols

  n.cols <- length(cols) ## number of colours
  n.brks <- length(brks) ## number of intervals

  if (n.brks != n.cols + 1)
    stop("There must be one break more than the number of colors")
  ncols <- ncol(var) ## number of columns of the image
  nrows <- nrow(var) ## number of rows of the image
  if (ncols < 2)
    stop("Matrix must have at least two columns")
  if (nrows < 2)
    stop("Matrix must have at least two rows")
  if (!is.null(xlabels) && length(xlabels) != ncols)
      stop(paste0("The number of x labels must be equal to the number of ",
                  "columns of the data matrix"))
  if (!is.null(ylabels) && length(ylabels) != nrows)
      stop(paste0("The number of y labels must be equal to the number of ",
                  "rows of the data matrix"))
  if (!is.numeric(figure.width) || figure.width < 0)
      stop("figure.width must be a positive number")
  if (!is.numeric(legend.width) || legend.width < 0 || legend.width > 0.5)
      stop("legend.width must be a number from 0 to 0.5")

    
  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout,
                                width = 80 * ncols * figure.width, 
                                height = 80 * nrows,
                                units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res,
            width = 8 * figure.width, height = 5)
  }

  if (!is.null(fileout)) {

  # Draw empty plot:
    par(mar = c(4, 4, 1, 0), fig = c(0.1, 1 - legend.width, 0.1, 0.9))
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylim = c(0.5, nrows + 0.5), 
         xlim = c(-0.5, ncols - 1 + 0.5), ann = F,  bty = "n")
    
  # Add axes titles:
    label.size <- 1.2 * (max(ncols, nrows) / 10) ^ 0.5  
    mtext(side = 1, text = xtitle, line = line, cex = label.size, font = 3)
    mtext(side = 2, text = ytitle, line = 3, cex = label.size, font = 3)

  # Add plot title:
    if (is.null(title.color)) title.color <- "royalblue4"
    mtext(side = 3, text = toptitle, cex = 1.75 * (nrows / 10) ^ 0.7, 
          col = title.color)

  # Add axis labels:
    axis.size <- (max(ncols, nrows) / 10) ^ 0.3
    if (is.null(xlabels)) xlabels = 1:ncols
    if (is.null(ylabels)) ylabels = 1:nrows

   if(is.null(xlab_dist)) { ## Add x axis labels
        axis(1, at = seq(0, ncols - 1), las = ifelse(xvert, 2, 1), labels = xlabels, 
             cex.axis = axis.size, tck = 0, lwd = 0, line = - 1 - (nrows / 10 - 1))  
    } else {
        axis(1, at = seq(0, ncols - 1), las = ifelse(xvert, 2, 1), labels = xlabels, 
             cex.axis = axis.size, tck = 0, lwd = 0, line = xlab_dist) 
    }
    if(is.null(ylab_dist)) { ## Add y axis labels
        axis(2, at = seq(1, nrows), las = 1, labels = rev(ylabels), 
             cex.axis = axis.size, tck = 0, lwd = 0, line = 0.5 - ncols / 10)
    } else {
        axis(2, at = seq(1, nrows), las = 1, labels = rev(ylabels), 
             cex.axis = axis.size, tck = 0, lwd = 0, line = ylab_dist)
    }

  } else {

  # Draw empty plot:
    par(mar = c(4, 4, 1, 0), fig = c(0.1, 1 - legend.width, 0.1, 0.9))
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylim = c(0.5, nrows + 0.5),
         xlim = c(-0.5, ncols - 1 + 0.5), ann = F,  bty = "n")

  # Add axes titles:
    label.size <- 1.2 # * (max(ncols, nrows) / 10) ^ 0.5
    mtext(side = 1, text = xtitle, line = line, cex = label.size, font = 3)
    mtext(side = 2, text = ytitle, line = 3, cex = label.size, font = 3)

  # Add plot title:
    if (is.null(title.color)) title.color <- "royalblue4"
    mtext(side = 3, text = toptitle, cex = 1.5,  #* (nrows / 10) ^ 0.7, 
          col = title.color)

  # Add axis labels:
    axis.size <- 1 #(max(ncols, nrows) / 10) ^ 0.3
    if (is.null(xlabels)) xlabels = 1:ncols
    if (is.null(ylabels)) ylabels = 1:nrows

    if(is.null(xlab_dist)){  ## Add x axis labels
        axis(1, at = seq(0, ncols - 1), las = ifelse(xvert, 2, 1), labels = xlabels, 
             cex.axis = axis.size, tck = 0, lwd = 0, line = - 1 - (nrows / 10 - 1))
    } else {
        axis(1, at = seq(0, ncols - 1), las = ifelse(xvert, 2, 1), labels = xlabels, 
             cex.axis = axis.size, tck = 0, lwd = 0, line = xlab_dist)  
    }
    if(is.null(ylab_dist)){ ## Add y axis labels
        axis(2, at = seq(1, nrows), las = 1, labels = rev(ylabels),
             cex.axis = axis.size, tck = 0, lwd = 0, line = 0.5 - ncols / 10)  
    } else { 
        axis(2, at = seq(1, nrows), las = 1, labels = rev(ylabels),
             cex.axis = axis.size, tck = 0, lwd = 0, line = ylab_dist)  
    }

  }

  # Create an array of colors instead of numbers (it starts all gray):                
  array.colors <- array("gray", c(nrows, ncols))
  for (int in n.cols:1) array.colors[var <= brks[int + 1]] <- cols[int]

  # fill with colors the cells in the figure:
  for (p in 1:nrows) {
    for (l in 0:(ncols - 1)) {
      polygon(c(0.5 + l - 1, 0.5 + l - 1, 1.5 + l - 1, 1.5 + l - 1),
              c(-0.5 + nrows + 1 - p, 0.5 + nrows + 1 - p, 
                 0.5 + nrows + 1 - p, -0.5 + nrows + 1 - p), 
                 col = array.colors[p, 1 + l], border = "black")
    }
  }

  # Draw color legend:
  if (legend) {
    par(fig = c(1 - legend.width - 0.01, 
                1 - legend.width + legend.width * min(1, 10 / ncols), 
                0.3, 0.8), new = TRUE)
    #legend.label.size <- (max(ncols, nrows) / 10) ^ 0.4
    ColorBar(brks = brks, cols = cols, vertical = TRUE, ...)
  }
    
  # If the graphic was saved to file, close the connection with the device
  if (!is.null(fileout)) dev.off()
   invisible(list(brks = brks, cols = cols))
 
}
