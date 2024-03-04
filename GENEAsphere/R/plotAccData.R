
#' From the raw data to create a representation of light, temperature and MAGSA (Mean Absolute Gravity Substituted Acceleration). 
#'
#'@title plotAccData
#'
#'@description  Creates a plot of the Acc Data given a resolution.
#'
#'@param x should be an AccData object.
#'@param what What variable to plot against time. Options are: \enumerate{
#'\item sd: Standard of movement given the resolutions
#'\item mean: Mean of movementgiven the resolutions
#'\item temperature
#'\item light
#'\item voltage}
#'@param draw if TRUE, plot a whole new plot. Otherwise, superimpose on to existing plot.
#'@param resolution Resolution of plot to create.
#'@param ... resolution of underlying grid.
#'
#'@importFrom GENEAread get.intervals apply.epoch convert.time
#'@importFrom graphics axis.POSIXct lines
#'@export
#'
#'@details Creates a line plot at a certain resolution from the GENEAread AccData objects available.
#'
#'@examples \dontrun{
#' ## AccData = read.bin(datafile) # where data file is a GENEActiv .bin file.
#' ## saveRDS(AccData , "AccData.rds")
#' x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
#' plotAccData(x, what = ("sd"))
#' plotAccData(x, what = ("sd"))
#' plotAccData(x, what = ("mean"))
#' plotAccData(x, what = ("temperature"))
#' plotAccData(x, what = ("light"))
#' plotAccData(x, what = ("voltage"))
#' }

plotAccData <- function(x, 
                        what = c("sd", "mean", "temperature", "light", "voltage"), 
                        draw = TRUE, 
                        resolution = 200,
                        ...){
  
  what = match.arg(what)
  epoch = floor((nrow(x$data.out)/resolution + 1)/x$freq)
  
  if (class(x) == "AccData"){
    # Create the time sequence to plot over. 
    t1 = as.POSIXct(x$data.out[1, 1], tz = "GMT", origin = "1970-01-01")
    t2 = as.POSIXct(x$data.out[length(x$data.out[,1]), 1], tz = "GMT", origin = "1970-01-01")
    t_del = (t2 - t1)/5
    time_seq = seq(t1, t2, by = t_del)
    
    if (what == "sd"){
      obj = apply.epoch(x$data.out, epoch.size = epoch, incl.date = TRUE, FUN = function(t) sd(svm(t)))
    } else if (what == "mean"){
      obj = apply.epoch(x$data.out, epoch.size = epoch, incl.date = TRUE, FUN = function(t) mean(svm(t)))
    } else if (what == "temperature"){
      data = cbind(x$data.out[,1], x$data.out[,7])
      obj = apply.epoch(x$data.out, epoch.size = epoch, incl.date = TRUE, function(t) mean(t[,2]))
    } else  if (what == "light"){
      data = cbind(x$data.out[,1], x$data.out[,5])
      obj = apply.epoch(data, epoch.size = epoch, incl.date = TRUE, FUN = function(t) max(t[,2]))
    } else if (what == "voltage"){
      obj = data.frame("time" = x$page.timestamps, "Voltage" = x$page.volts)
     
      if (draw){ 
        plot(as.POSIXct(obj[,1], tz = "GMT", origin = "1970-01-01"),
             obj[,2], type = "l", xlab = "Time", ylab = paste0("Voltage in volts"),
             xlim = c(obj[1,1], obj[length(obj[,1]),1]), xaxt = "n")
        axis.POSIXct(1,
                     at = time_seq,
                     labels = format(time_seq,  "%d-%m-%y %H:%M"))
        return(invisible(obj))
      } else {
        lines(as.POSIXct(obj[,1], tz = "GMT", origin = "1970-01-01"),
              obj[,2], type = "l", xlab = "Time", ylab = paste0("Voltage in volts"),
               xlim = c(obj[1,1], obj[length(obj[,1]),1]), xaxt = "n")
        return(NULL)
      }
    }
    
    if (draw){
      plot(as.POSIXct(x$data.out[(obj[,1]), 1], tz = "GMT", origin = "1970-01-01"),
           obj[,2] ,  type = "l", xlab = "Time", ylab = paste0(what, " in mg"),
           xlim = c(t1,t2), xaxt = "n")
      axis.POSIXct(1,
                   at = time_seq,
                   labels = format(time_seq,  "%d-%m-%y %H:%M"))
      return(invisible(obj))
    } else {
      lines(as.POSIXct(x$data.out[(obj[,1]), 1], tz = "GMT", origin = "1970-01-01"),
            obj[,2] ,  type = "l", xlab = "Time", ylab = paste0(what, " in mg"),
            xlim = c(t1,t2), xaxt = "n")
      return(NULL)
    } 
  } else{
    stop("X needs to be an AccData object")
  }
}

