######################################################################################
#@description Produce box-and-whisker plot(s) of the given (grouped) values.
# @details The function creates a boxplot from input data using R function graphics::boxplot.
# It allows the user to specify other set of data to over impose in the plot as dots
#
#
#@param x A numeric matrix
#@param use.cols Logical indicating if columns (by default) or rows (use.cols = FALSE) should be plotted.
#@param outputDir A path to the directory where the output files are written.
#@param plotName Name to be used while saving the file ('boxplot' by default).
#@param plotTitle Title for the plot ('Boxplot' by default).
#@param ylab,xlab Plot labels
#@param stripchartMatrixList List of numercal matrices used to produce 1-dim scatter plots to overlap the boxplots. NULL by default.
#@param stripchartCol A vector of colours to use for the scatter plots, one colour for each element
#in the stripchartMatrixList. If NULL it will be randomly generated using the rainbow function. NULL by default.
#@param stripchartPch A vector of either integers specifying symbols or single characters to be
#used as the default in plotting points, one for each element in the stripchartMatrixList. Default is 2
#@param ... Further arguments to boxplot.matrix
#
#
#@author Alessandro Barberis
######################################################################################
.boxplot.matrix2 <- function(x, use.cols=TRUE, outputDir, plotName="boxplot", plotTitle="Boxplot", ylab=NULL, xlab=NULL, stripchartMatrixList=NULL, stripchartCol=NULL, stripchartPch=rep(21, length(stripchartMatrixList)), group.names=NULL,...){
  ###########Check the input
  if(missing(x)){
    stop("Need to specify a numeric matrix.")
  }
  if(missing(outputDir)){
    stop("Need to specify an output directory")
  }

  filePath = file.path(outputDir, paste0(plotName, ".pdf"))

  #Define some useful variables
  x.ncol = dim(x)[2]
  x.nrow = dim(x)[1]

  if(use.cols){
    pdf.width.elem = x.ncol
    bottom.label.len = max(nchar(colnames(x)))
  }else{
    pdf.width.elem = x.nrow
    bottom.label.len = max(nchar(rownames(x)))
  }

  mar.bottom = bottom.label.len/2 + 1#adding space for the vertical labels and bottom legend

  if(pdf.width.elem<=25){
    #Default pdf values (in inches)
    width=7
    height=7
  }else if(pdf.width.elem>25 && pdf.width.elem<=50){
    width=10
    height=7
  }else if(pdf.width.elem>50 && pdf.width.elem<=75){
    width=18
    height=7
  }

  grDevices::pdf(file = filePath, width = width)
  #Make the outer margin to save space for the legend
  graphics::par(oma = c(1, 0, 1, 0))
  #Set up other graphics parameters
  graphics::par(las=2)#label vertical to the axis
  graphics::par(mar=c(mar.bottom,6,2,4))#c(bottom, left, top, right)
  #Draw the boxplot
  graphics::boxplot.matrix(x = x, ylab=ylab, use.cols = T, outline=T, outpch = NA, cex.main=0.9, main=plotTitle,names=group.names, ... = ...)
  graphics::mtext(text=xlab, side = 1, line = mar.bottom-1, las=1)#Draw xlabel

  #Plot additional vertical scatter plots, if any
  if(!is.null(stripchartMatrixList)){
    stripchartNum = length(stripchartMatrixList)
    #1) Check the graphics parameters
    #1.1)Check the symbols/characters to be used in plotting points
    if(length(stripchartPch)!=stripchartNum)
      stripchartPch = rep(2, stripchartNum)
    #1.2) Check the stripchartCol, if missing create a color palette
    if(is.null(stripchartCol) || length(stripchartCol)!=stripchartNum || !.areAllColours(stripchartCol))
      stripchartCol = grDevices::rainbow(n = stripchartNum)
    #2) Loop over list
    for(i in 1:stripchartNum){
      stripchartMatrix = stripchartMatrixList[[i]]
      stripchartMatrix.col = dim(stripchartMatrix)[2]
      stripchartMatrix.row = dim(stripchartMatrix)[1]
      #3) Check if dim are fine
      if(use.cols){
        if(pdf.width.elem==stripchartMatrix.col){
          #4) Add the scatter plot
          graphics::stripchart(x=as.data.frame(stripchartMatrix), vertical = TRUE, method = "jitter",pch = stripchartPch[i], col = stripchartCol[i], bg = stripchartCol[i],add=TRUE,cex=0.5)
        }else{
          next;
        }
      }else{
        if(pdf.width.elem==stripchartMatrix.row){
          #4) Add the scatter plot
          graphics::stripchart(x=as.data.frame(t(stripchartMatrix)), vertical = TRUE, method = "jitter",pch = stripchartPch[i], col = stripchartCol[i], bg = stripchartCol[i], add=TRUE,cex=0.5)
        }else{
          next;
        }
      }
    }
  }



  #Overlay the entire figure region with a new, single plot. Then call legend with the location
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  graphics::legend(x="bottom",      # location of the legend on the heatmap plot
                   legend = c(names(stripchartMatrixList)), # category labels
                   col = stripchartCol,  # color key
                   #inset=c(-0.38,0),
                   pch = stripchartPch,             # plotting symbols
                   pt.bg = stripchartCol, #background color for the points
                   # lwd = 2,            # line width
                   cex = 0.6,
                   pt.cex=0.8,
                   xpd=TRUE,
                   horiz = TRUE, #draw an horizontal legend
                   inset = c(0,0),
                   bty = "n"
  )

  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
}

######################################################################################
#@description Check if the input values are valid R colour specifications.
# @details The function checks if the input values are valid R colour specifications using grDevices::col2rgb function
# Valid R colour specifications are either a color name (as listed by colors()),
# a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb),
# or a positive integer i meaning palette()[i]
#
#
#@param x Vector of values to test
#
#
#@author Alessandro Barberis
#@references https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
######################################################################################
.areColours <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  })
}

######################################################################################
#@description Check if the input values are valid R colour specifications.
# @details The function checks if the input values are valid R colour specifications using grDevices::col2rgb function
# Valid R colour specifications are either a color name (as listed by colors()),
# a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb),
# or a positive integer i meaning palette()[i]
#
#
#@param x Vector of values to test
#
#
#@author Alessandro Barberis
######################################################################################
.areAllColours <- function(x) {
  all(.areColours(x))
}
