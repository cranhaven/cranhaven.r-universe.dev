
#' @title Positionals
#'
#' @description Creates a positionals plot of the GENEAcitv .bin data. 
#'
#' @param AccData object to plot, can be matrix or accdata object
#' @param start time at which to start (Default set at 0).
#' @param end time at which to end (Default set at 1).
#' @param length Length of interval.
#'@param max.points maximum number of data points to plot. Data will be downsampled to achieve this.
#'@param ... Arguements passed to code{\link[GENEAread]{get.intervals}}
#'
#'@details From the raw data to create a representation of arm elevation and wrist rotation.
#'@return There is no return to the console. As a side effect an rgl graphic is created.
#'@import GENEAread  
#' @importFrom stats cov median qnorm quantile reshape rnorm runif runmed sd var
#' @importFrom utils tail
#' @importFrom grDevices dev.new grey hcl heat.colors
#'@export
#'
#'@examples
#'\dontrun{
#' x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
#' positionals(x)
#' }


positionals = function(AccData, start = 0, end = 1, length = NULL, max.points = 1e6,  ...){
  
  # Reading the data in correctly. 
  tmp2 = get.intervals(AccData, start, end, length, incl.date = T, 
                       size = max.points, ...)
  
  ind = expand((bapply.basic(1: nrow(tmp2), 100, 
                             function(t) 
                               sum(cov(1:100, tmp2[t, 2:4])^2) / sum(apply(tmp2[t, 2:4], 2, sd)^2)) / var(1:100)) %bq% "[0, 0.5]", nrow(tmp2))
  
  # Taking the timestamp as our x value
  x = tmp2[ind,1]
  # Y axis as the up arm elevation
  y = -acos(tmp2[ind,3] / sqrt(rowSums(tmp2[ind,-1]^2)) ) *180/pi +90
  
  # Colours on the plot - set as a and b then ab is the difference. this can be changed
  a = 90
  b = 360
  ab = b-a
  cols = hcl(a:b)
  col = cols[floor(length(cols) * (sign(-tmp2[ind, 2]) * 180 * acos(-tmp2[ind, 4]/sqrt(rowSums(tmp2[ind, -c(1,3)]^2)))/pi + 180)/360) + 1]
  #cols =
  # Dataframe 
  xx = as.POSIXct(tmp2[ind,1], origin = "1970-01-01",tz="GMT")
  s = data.frame(x = as.POSIXct(tmp2[ind,1], origin = "1970-01-01",tz="GMT"),
                 y = -acos(tmp2[ind,3] / sqrt(rowSums(tmp2[ind,-1]^2)) ) *180/pi +90, 
                 col)
  
  # Legend variables
  lx = as.numeric(as.character(seq(x[1] , quantile(x, 0.1), len = (ab + 1))))
  ly = rep(95, ab)
  
  x1 = x2 = y1 = y2 = c()
  
  # Set up legend data frame here. 
  for (i in 1:(ab)){
    x1[i] = as.POSIXct(lx[i], origin = "1970-01-01",tz="GMT")
    x2[i] = as.POSIXct(lx[i+1], origin = "1970-01-01",tz="GMT")
    y1[i] = 90
    y2[i] = 110
  }
  
  tempindex = data.frame(xmin = as.POSIXct(x1, origin = "1970-01-01",tz="GMT"),
                         xmax = as.POSIXct(x2, origin = "1970-01-01",tz="GMT"),
                         ymin = y1,
                         ymax = y2)
  
  tempindex1 <- transform(tempindex, 
                          id = 1:ab,
                          tier = cols[-(ab + 1)])
  
  # Legend lines - Black lines to break down the colours.
  # The lines Should show 90, 180, 270 and 360 Degrres
  
  lineindex <- rbind(tempindex1[1,],
                     tempindex1[90,],
                     tempindex1[180,],
                     tempindex1[ab,])
  
  # Plot arm elevation and 
  
  p = ggplot(data = s,
             aes(y = y,
                 x = x))
  
  p = p + geom_line(colour = col)
  
  p = p + scale_fill_manual(values = cols[-(ab+1)]) + guides(fill = "none")
  
  # Now adding in the labels for the legend
  p = p + annotate("text", x = tempindex1$xmin[30], y = 105, label = "CCW")
  p = p + annotate("text", x = tempindex1$xmin[240], y = 105, label = "CW")
  
  # Degrees points - Can be added later
  p = p + annotate("text", x = tempindex1$xmin[1],  y = 95, alpha = .8, size = 3, label = "90")
  p = p + annotate("text", x = tempindex1$xmin[89], y = 95, alpha = .8, size = 3, label = "180")
  p = p + annotate("text", x = tempindex1$xmin[179],y = 95, alpha = .8, size = 3, label = "270")
  p = p + annotate("text", x = tempindex1$xmin[ab], y = 95, alpha = .8, size = 3, label = "360")
  
  # Add Axis labels  
  p = p + labs(x = "", y="Arm Elevation")
  
  # Caclulate the breaks to be 10% larger than the data? Or the same 
  xmin = as.POSIXct(as.numeric(as.character(round(x[1]))), origin = "1970-01-01",tz="GMT")
  xmax = as.POSIXct(as.numeric(as.character(round(x[length(x)]))), origin = "1970-01-01",tz="GMT")
  break_width = difftime(xmax,xmin,units = "secs")/4
  
  # Limits should be 10% on the breakwidth 
  xmin_lim = as.POSIXct(as.numeric(xmin) - as.numeric(difftime(xmax, xmin, units = "secs")/40), origin = "1970-01-01",tz="GMT")
  xmax_lim = as.POSIXct(as.numeric(xmax) + as.numeric(difftime(xmax, xmin, units = "secs")/40), origin = "1970-01-01",tz="GMT")
  breaks = seq(xmin,xmax,break_width)
  
  p = p + scale_x_datetime(
    breaks = breaks,
    labels = format(breaks, "%a-%d\n%H:%M", tz = "GMT"),
    expand = c(0, 0),
    limits = c(xmin_lim,xmax_lim)
  )
  
  # Y axis
  p = p + scale_y_continuous(limits = c(-120,120),
                             breaks = seq(-90,90,30))
  
  return(p)
}


expand <- function(X, length = (length(X)*100)){
  c(rep(X, each = floor(length / length(X))), rep(tail(X,1), length - length(X) * floor(length/length(X))))
}

"%bq%" = function(X, y){
  if (is.character(y)){
    if (length(y) == 4){ 
      y = paste(y[1],y[2], ",",y[3], y[4], sep="")
    }
    y = strsplit(y, ",")[[1]]
    nc = nchar(y[2])
    yl = quantile(X, c(as.numeric(substring(y[1], 2)), as.numeric(substring(y[2], 1,nc - 1))))
    if (substr(y[1],1,1) == "["){
      res = (X >= yl[1])
    }else {
      res = (X >  yl[1])
    }
    if (substr(y[2],nc,nc) == "]"){
      res = res &(X <= yl[2] )
    }else {
      res = res & (X < yl[2])
    }
  } else {
    y = quantile(X, y)
    res = (X >= y[1] ) & (X<= y[2])
  }
  res
}

bapply.basic <- function(X, k, FUN) {
  res = rep(0, floor(length(X) / k))
  for (i in 1:floor(length(X)/k)){
    res[i] = FUN(X[ (i-1)*k + 1:k])
  }
  return(res)
}

bapply <- function(X, k, FUN) {
  dimout = length(FUN(X[1:k]))
  res = matrix(0, dimout, floor(length(X) / k))
  for (i in 1:floor(length(X)/k)){
    res[(i-1)* dimout + 1:dimout] = FUN(X[ (i-1)*k + 1:k])
    return(res)
  }
}

#'@title produces 2d kernel density plots.
#'
#'@description Creates a 2d kernel density plot.
#'
#'@param x x coords
#'@param y y coords
#'@param h bandwidths in x,y direction
#'@param h.relative adjustment to bandwidths relative to normal reference model. (For non-smooth data, will want < 1, probably.
#'@param col colour to use
#'@param add if FALSE, plot a whole new plot. Otherwise, superimpose on to existing plot.
#'@param drawlabels A value of false will give no labels on the contours whereas true will.
#'@param n resolution of underlying grid
#'@param shade shade in the interiors of the contours?
#'@param breaks breakpoints to plot contours at. These correspond to quantiles of the data - e.g. at 0.9, the space contained in the contours corresponds to an estimated 10 of the population, with the highest probability.
#'@param ... additional params for contour see code{\link[graphics]{contour}}
#'
#'@details Creates a 2d kernel density plot
#'@importFrom grDevices col2rgb rgb
#'@importFrom MASS bandwidth.nrd
#'@keywords internal
#'@export

kde2dplot = function(x, y=NULL, h, h.relative = 1, col = "red", add=FALSE, drawlabels = FALSE,n = 100, shade = TRUE, breaks = c(0.1, 0.25, 0.5, 0.75, 0.9),...){
  
  if (is.null(y)){
    y = x[,2]
    x = x[,1]
  }
  
  if (missing(h)) h = c(bandwidth.nrd(x), bandwidth.nrd(y))*h.relative
  
  d <- kde2d(x,y,h=h,n=n)
  dtmp = (sort(d$z))
  colr = col2rgb(col)
  breaksraw = dtmp[findInterval(breaks, cumsum(dtmp)/sum(dtmp))]
  if (shade) image(d, col = rgb(colr[1], colr[2], colr[3], alpha = c(breaks^2,1) * 0.3*255, maxColorValue = 255), breaks = c(0,breaksraw, max(dtmp)), add = add, ...)
  contour(d,add=T,levels=  breaksraw , col=col, drawlabels = drawlabels,...)
  invisible(d)
}


  