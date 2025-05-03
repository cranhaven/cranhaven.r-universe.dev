#'Function to convert any 3-d numerical array to a grid of coloured triangles. 
#'
#'This function converts a 3-d numerical data array into a coloured 
#'grid with triangles. It is useful for a slide or article to present tabular 
#'results as colors instead of numbers. This can be used to compare the outputs 
#'of two or four categories (e.g. modes  of variability, clusters, or forecast 
#'systems). 
#'
#'@param data Array with three named dimensions: 'dimx', 'dimy', 'dimcat', 
#'  containing the values to be displayed in a coloured image with triangles.
#'@param brks A vector of the color bar intervals. The length must be one more 
#'  than the parameter 'cols'. Use ColorBar() to generate default values.
#'@param cols A vector of valid colour identifiers for color bar. The length
#'  must be one less than the parameter 'brks'. Use ColorBar() to generate 
#'  default values.
#'@param toptitle A string of the title of the grid. Set NULL as default.   
#'@param sig_data Logical array with the same dimensions as 'data' to add layers 
#'  to the plot. A value of TRUE at a grid cell will draw a dot/symbol on the 
#'  corresponding triangle of the plot. Set NULL as default.
#'@param pch_sig Symbol to be used to represent sig_data. Takes 18 
#' (diamond) by default. See 'pch' in par() for additional accepted options.
#'@param col_sig Colour of the symbol to represent sig_data. 
#'@param cex_sig Parameter to increase/reduce the size of the symbols used 
#'  to represent sig_data.
#'@param xlab A logical value (TRUE) indicating if xlabels should be plotted
#'@param ylab A logical value (TRUE) indicating if ylabels should be plotted
#'@param xlabels A vector of labels of the x-axis The length must be 
#'  length of the col of parameter 'data'. Set the sequence from 1 to the 
#'  length of the row of parameter 'data' as default.
#'@param xtitle A string of title of the x-axis. Set NULL as default.
#'@param ylabels A vector of labels of the y-axis The length must be 
#'  length of the row of parameter 'data'. Set the sequence from 1 to the 
#'  length of the row of parameter 'data' as default. 
#'@param ytitle A string of title of the y-axis. Set NULL as default.
#'@param legend A logical value to decide to draw the color bar legend or not. 
#'  Set TRUE as default.
#'@param lab_legend A vector of labels indicating what is represented in each 
#'category (i.e. triangle). Set the sequence from 1 to the length of 
#' the categories (2 or 4).  
#'@param cex_leg A number to indicate the increase/reductuion of the lab_legend  
#'  used to represent sig_data.
#'@param col_leg Color of the legend (triangles).
#'@param cex_axis A number to indicate the increase/reduction of the axis labels.
#'@param fileout A string of full directory path and file name indicating where 
#'  to save the plot. If not specified (default), a graphics device will pop up. 
#'@param mar A numerical vector of the form c(bottom, left, top, right) which 
#'  gives the number of lines of margin to be specified on the four sides of the 
#'  plot. 
#'@param size_units A string indicating the units of the size of the device 
#'  (file or window) to plot in. Set 'px' as default. See ?Devices and the 
#'  creator function of the corresponding device.
#'@param res A positive number indicating resolution of the device (file or 
#'  window) to plot in. See ?Devices and the creator function of the 
#'  corresponding device.
#'@param figure.width a numeric value to control the width of the plot.
#'@param ... The additional parameters to be passed to function ColorBar() in 
#'  s2dv for color legend creation.
#'@return A figure in popup window by default, or saved to the specified path.
#'
#'@author History:\cr
#'1.0  -  2020-10  (V.Torralba, \email{veronica.torralba@bsc.es})  -  Original code
#'
#'@examples 
#'# Example with random data
#'arr1 <- array(runif(n = 4 * 5 * 4, min = -1, max = 1), dim = c(4,5,4))
#'names(dim(arr1)) <- c('dimx', 'dimy', 'dimcat')
#'arr2 <- array(TRUE, dim = dim(arr1))
#'arr2[which(arr1 < 0.3)] <- FALSE
#'PlotTriangles4Categories(data = arr1,
#'                         cols = c('white','#fef0d9','#fdd49e','#fdbb84','#fc8d59'),
#'                         brks = c(-1, 0, 0.1, 0.2, 0.3, 0.4), 
#'                         lab_legend = c('NAO+', 'BL','AR','NAO-'), 
#'                         xtitle = "Target month", ytitle = "Lead time",
#'                         xlabels = c("Jan", "Feb", "Mar", "Apr"))
#'@importFrom grDevices dev.new dev.off dev.cur 
#'@importFrom graphics plot points polygon text title axis
#'@importFrom RColorBrewer brewer.pal
#'@importFrom s2dv ColorBar
#'@export
PlotTriangles4Categories <- function(data, brks = NULL, cols = NULL,
                                     toptitle = NULL, sig_data = NULL,
                                     pch_sig = 18, col_sig = 'black',
                                     cex_sig = 1, xlab = TRUE, ylab = TRUE,
                                     xlabels = NULL, xtitle = NULL, 
                                     ylabels = NULL, ytitle = NULL,
                                     legend = TRUE, lab_legend = NULL,
                                     cex_leg = 1, col_leg = 'black',
                                     cex_axis = 1.5, mar = c(5, 4, 0, 0),
                                     fileout = NULL, size_units = 'px',
                                     res = 100, figure.width = 1, ...) {
  # Checking the dimensions
  if (length(dim(data)) != 3) {
    stop("Parameter 'data' must be an array with three dimensions.")
  }
  
  if (any(is.na(data))){
    stop("Parameter 'data' cannot contain NAs.")
  }
  
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must be an array with named dimensions.")
  }else{
    if (!any(names(dim(data)) == 'dimx') | !any(names(dim(data)) == 'dimy') |
        !any(names(dim(data)) == 'dimcat')) {
      stop("Parameter 'data' should contain 'dimx', 'dimy' and 'dimcat' dimension names. ")
    }
  }
  if (!is.vector(mar) & length(mar) != 4) {
    stop("Parameter 'mar' must be a vector of length 4.")
  }
  if (!is.null(sig_data)) {
    if (!is.logical(sig_data)) {
      stop("Parameter 'sig_data' array must be logical.")}
    else if (length(dim(sig_data)) != 3) {
      stop("Parameter 'sig_data' must be an array with three dimensions.")
    }else if (any(dim(sig_data) != dim(data))){
      stop("Parameter 'sig_data' must be an array with the same dimensions as 'data'.") 
    }else if(!is.null(names(dim(sig_data)))) {
      if (any(names(dim(sig_data)) != names(dim(data)))) {
        stop("Parameter 'sig_data' must be an array with the same named dimensions as 'data'.")}
    }
  }

  if (dim(data)['dimcat'] != 4 && dim(data)['dimcat'] != 2) {
    stop(
      "Parameter 'data' should contain a dimcat dimension with length equals
      to two or four as only two or four categories can be plotted.")
  }      
  
  # Checking what is available and generating missing information 
  if (!is.null(lab_legend) &&
      length(lab_legend) != 4 && length(lab_legend) != 2) {
    stop("Parameter 'lab_legend' should contain two or four names.")
  }
  
  datadim <- dim(data)
  nrow <- dim(data)['dimy']
  ncol <- dim(data)['dimx']
  ncat <- dim(data)['dimcat']
  
  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout,
                                width = 80 * ncol * figure.width, 
                                height = 80 * nrow,
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
  
  if (is.null(xlabels)){
    xlabels = 1:ncol
  }
  if (is.null(ylabels)){
    ylabels = 1:nrow
  }

  if (!is.null(brks) && !is.null(cols)) {
    if (length(brks) != length(cols) + 1) {
      stop("The length of the parameter 'brks' must be one more than 'cols'.")
    }
  }  
  if (is.null(brks)) {
    brks <- seq(min(data, na.rm = T), max(data, na.rm = T), length.out = 9)
  }
  if (is.null(cols)) {
    cols <- rev(brewer.pal(length(brks) - 1, 'RdBu'))
  }
  
  # The colours for each triangle/category are defined
  data_cat <- array(cols[length(cols)], dim = datadim)
  names(dim(data_cat)) <- names(dim(data))
  for (i in (length(cols) - 1):1) {
    data_cat[data < brks[i + 1]] <- cols[i]
  }
  
  if(legend){
    layout(matrix(c(1, 2, 1, 3), 2, 2, byrow = T),
           widths = c(10, 3.4), heights = c(10, 3.5))
    par(oma = c(1, 1, 1, 1), mar = mar)
    if(is.null(lab_legend)) {
      lab_legend = 1:ncat
    }
  }
  
  plot(ncol, nrow, xlim = c(0, ncol), ylim=c(0, nrow), xaxs = "i", yaxs = 'i', type = "n", 
       xaxt = "n", yaxt = "n", ann = F, axes = F)
  
  box(col = 'black',lwd = 1)
  
  if (! is.null(toptitle)){
    title(toptitle, cex = 1.5)
  }
  
  if (!is.null(xtitle)){
    mtext(side = 1, text = xtitle, line = 4, cex = 1.5)
  }
  if (!is.null(ytitle)){
    mtext(side = 2, text = ytitle, line = 2.5, cex = 1.5)
  }
  
  if (xlab){
    axis(1, at =(1:ncol) - 0.5, las = 2, labels = xlabels, cex.axis = cex_axis)
  }
  if (ylab){
    axis(2, at = (1:nrow) - 0.5, las = 2, labels = ylabels, cex.axis = cex_axis)
  }
  
  
  #The triangles are plotted
  for(p in 1:ncol){
    for(l in 1:nrow){
      if (ncat == 4){
        coord_triangl <- list(xs=list(c(p-1, p-0.5, p-1),c(p-1, p-0.5, p),c(p, p-0.5, p),c(p-1, p-0.5, p)),
                              ys=list( c(l-1, -0.5+l, l), c(l-1, -0.5+l, l-1),c(l-1, -0.5+l, l),c(l, -0.5+l, l)))
        
        coord_sig <- list(x=c(p-0.75,p-0.5,p-0.25,p-0.5),y=c(l-0.5,l-0.75,l-0.5,l-0.25))
      }
      
      if (ncat==2){
        coord_triangl <- list(xs=list(c(p-1, p, p-1),c(p-1, p, p)),
                             ys=list(c(l-1, l, l),c(l-1,l-1, l)))
        coord_sig <- list(x=c(p-(2/3),p-(1/3)),y=c(l-(1/3),l-(2/3)))
      }
      for (n in 1:ncat) {
        polygon(coord_triangl$xs[[n]],
                coord_triangl$ys[[n]],
                col = Subset(
                  data_cat,
                  along = c('dimcat', 'dimx', 'dimy'),
                  indices = list(n, p, l)))
        if (!is.null(sig_data) &&
            Subset(sig_data,along = c('dimcat', 'dimx', 'dimy'),
                   indices = list(n, p, l))) {
          points(
            x = coord_sig$x[n],
            y = coord_sig$y[n],
            pch = pch_sig,
            cex = cex_sig,
            col = col_sig
          )
        }
      }
    }
  }
  
  # legend
  
  if(legend){
    # Colorbar
    par(mar=c(0,0,0,0))
    ColorBar(brks = brks, cols = cols, vertical = T, draw_ticks = T, draw_separators = T,
             #             extra_margin = c(0,0,2.5,0),label_scale = 1.5,...)
             extra_margin = c( 0, 0, 0, 0), label_scale = 1.5, ...)
    
    par(mar = c(0.5, 2.5, 0.5, 2.5))
    plot(1, 1, xlim = c(0, 1), ylim =c(0, 1), xaxs = "i", yaxs = 'i', type = "n", 
         xaxt = "n", yaxt = "n", ann = F, axes = F)
    
    box(col = col_leg)
    p = l = 1
    if (ncat == 4){
      coord_triangl <- list(xs = list(c(p -1, p - 0.5, p - 1), c(p - 1, p - 0.5, p),
                                      c(p, p - 0.5, p), c(p - 1, p - 0.5, p)),
                            ys = list(c(l - 1, - 0.5 + l, l), c(l - 1, - 0.5 + l, l - 1),
                                      c(l - 1, - 0.5 + l, l), c(l, - 0.5 + l, l)))
      
      coord_sig <- list(x = c(p - 0.75, p - 0.5, p - 0.25, p - 0.5), 
                        y = c(l - 0.5, l - 0.75, l - 0.5, l - 0.25))
    }
    
    if (ncat==2){
      coord_triangl<- list(xs=list(c(p-1, p, p),c(p-1, p, p-1)),
                           ys=list( c(l-1,l-1, l), c(l-1, l, l)))
      coord_sig<- list(x=c(p-(2/3),p-(1/3)),y=c(l-(1/3),l-(2/3)))
    }
    for (n in 1:ncat) {
      polygon(coord_triangl$xs[[n]],
              coord_triangl$ys[[n]],border=col_leg)
      text(x=coord_sig$x[[n]],y=coord_sig$y[[n]],labels = lab_legend[n],cex=cex_leg,col=col_leg)
      
    }
  }
  
  # If the graphic was saved to file, close the connection with the device
  if (!is.null(fileout)) dev.off()
}
.SelectDevice <- function(fileout, width, height, units, res) {
  # This function is used in the plot functions to check the extension of the 
  # files where the graphics will be stored and select the right R device to 
  # save them.
  # If the vector of filenames ('fileout') has files with different 
  # extensions, then it will only accept the first one, changing all the rest 
  # of the filenames to use that extension.

  # We extract the extension of the filenames: '.png', '.pdf', ...
  ext <- regmatches(fileout, regexpr("\\.[a-zA-Z0-9]*$", fileout))

  if (length(ext) != 0) {
    # If there is an extension specified, select the correct device
    ## units of width and height set to accept inches
    if (ext[1] == ".png") {
      saveToFile <- function(fileout) {
        png(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else if (ext[1] == ".jpeg") {
      saveToFile <- function(fileout) {
        jpeg(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else if (ext[1] %in% c(".eps", ".ps")) {
      saveToFile <- function(fileout) {
        postscript(file = fileout, width = width, height = height)
      }
    } else if (ext[1] == ".pdf") {
      saveToFile <- function(fileout) {
        pdf(file = fileout, width = width, height = height)
      }
    } else if (ext[1] == ".svg") {
      saveToFile <- function(fileout) {
        svg(filename = fileout, width = width, height = height)
      }
    } else if (ext[1] == ".bmp") {
      saveToFile <- function(fileout) {
        bmp(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else if (ext[1] == ".tiff") {
      saveToFile <- function(fileout) {
        tiff(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else {
      .warning("file extension not supported, it will be used '.eps' by default.")
      ## In case there is only one filename
      fileout[1] <- sub("\\.[a-zA-Z0-9]*$", ".eps", fileout[1])
      ext[1] <- ".eps"
      saveToFile <- function(fileout) {
        postscript(file = fileout, width = width, height = height)
      }
    }
    # Change filenames when necessary
    if (any(ext != ext[1])) {
      .warning(paste0("some extensions of the filenames provided in 'fileout' are not ", ext[1],". The extensions are being converted to ", ext[1], "."))
      fileout <- sub("\\.[a-zA-Z0-9]*$", ext[1], fileout)
    }
  } else {
    # Default filenames when there is no specification
    .warning("there are no extensions specified in the filenames, default to '.eps'")
    fileout <- paste0(fileout, ".eps")
    saveToFile <- postscript
  }

  # return the correct function with the graphical device, and the correct 
  # filenames
  list(fun = saveToFile, files = fileout)
}

.warning <- function(...) {
  # Function to use the 'warning' R function with our custom settings
  # Default: no call information, indent to 0, exdent to 3, 
  #  collapse to \n
  args <- list(...)

  ## In case we need to specify warning arguments
  if (!is.null(args[["call."]])) {
    call <- args[["call."]]
  } else {
    ## Default: don't show info about the call where the warning came up
    call <- FALSE
  }
  if (!is.null(args[["immediate."]])) {
    immediate <- args[["immediate."]]
  } else {
    ## Default value in warning function
    immediate <- FALSE
  }
  if (!is.null(args[["noBreaks."]])) {
    noBreaks <- args[["noBreaks."]]
  } else {
    ## Default value warning function
    noBreaks <- FALSE
  }
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value warning function
    domain <- NULL
  }
  args[["call."]] <- NULL
  args[["immediate."]] <- NULL
  args[["noBreaks."]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n!"
  }
  args[["collapse"]] <- NULL

  ## Warning tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "! Warning: "
  }
  args[["tag"]] <- NULL

  warning(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
    ), collapse = collapse)),  call. = call, immediate. = immediate, 
    noBreaks. = noBreaks, domain = domain)
}

