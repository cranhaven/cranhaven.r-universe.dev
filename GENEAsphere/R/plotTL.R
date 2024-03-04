
#' From the raw data to create a representation of Light and Temperature. 
#'
#'@title Produces a line plot of Temperature and Light.
#'
#'@description  3 line plot of Temperature and Light.
#'
#'@param AccData  object to plot, can be matrix or AccData object.
#'@param start Start of Data to plot. Passed to get.intervals, see code{\link[GENEAread]{get.intervals}}
#'@param end End of Data to plot.
#'@param length Length of interval 
#'@param resolution Resolution to plot the data given the data's frequency. 
#'@importFrom GENEAread get.intervals apply.epoch
#'@importFrom graphics abline axis box contour grid image legend mtext par plot points rect text frame
#'@export
#'
#'@details Creates a temperature and ligth plot with 2 distinct axis from epoched data. The epoch is dependent on the resolution. 
#'
#'@examples
#'
#' \dontrun{
#' x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
#' plotTL(x)
#' }

plotTL = function(AccData, start = NULL, end = NULL, length = NULL, resolution = 100){
  
  epoch = floor(nrow(AccData$data.out)/resolution + 1)/AccData$freq
  lightobj = apply.epoch(AccData$data.out, epoch, incl.date = T, FUN = function(t) max(t[,5]))
  tempobj = apply.epoch(AccData$data.out, epoch, incl.date = T, function(t) mean(t[,7]))
  time =  as.POSIXct(as.numeric(as.character(round(as.numeric(convert.time(lightobj[,1]))))), origin = "1970-01-01",tz="GMT")
  light = lightobj[,2]
  temp = tempobj[,2]
  
  # Melt the dataframe 
  s = data.frame(time,light,temp)
 
  p <- ggplot()
  
  p <- p + geom_line(data = s, 
                     aes(y = temp,
                         x = time),
                     stat = "summary", colour = "red")
  
  scalefactor = abs((min(light) - max(light)) / mean(temp))
  
  p <- p + geom_line(data = s, 
                     aes(y = (light/scalefactor),
                         x = time),
                     stat = "summary", colour = "yellow")
  
  p <- p + scale_y_continuous("Temp Values", 
                              sec.axis=sec_axis(~.*scalefactor+(min(light)/1.05), 
                                                name="light values") ) 
  
  p = p + labs(x = " ", y = "Temperature")
  # Caclulate the breaks to be 10% larger than the data? Or the same 
  xmin = time[1]
  xmax = time[length(time)]
  break_width = round(difftime(xmax,xmin,units = "secs")/4)
  
  # Limits should be 10% on the breakwidth 
  xmin_lim = (xmin) - (difftime(xmax, xmin, units = "secs")/40)
  xmax_lim = (xmax) + (difftime(xmax, xmin, units = "secs")/40)
  breaks = seq(xmin,xmax,break_width)
  p = p + scale_x_datetime(
    breaks = seq(xmin,xmax,break_width),
    labels = format(breaks, "%a-%d\n%H:%M", tz = "GMT"),
    expand = c(0, 0),
    limits = c(xmin_lim,xmax_lim)
  )
  
  return(p)
}

  