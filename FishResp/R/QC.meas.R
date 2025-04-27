#' Quality Control of Raw Data
#'
#' Graphical quality control tests of temperature and oxygen raw data before and after correction for background respiration
#'
#' @usage
#' QC.meas(clean.data,
#'         QC = c("Temperature",
#'                "Total.O2.phases",
#'                "Corrected.O2.phases",
#'                "Total.O2.chambers",
#'                "Corrected.O2.chambers"))
#'
#' @param clean.data  a data frame obtained by using the function \code{\link{correct.meas}}
#' @param QC  string: the name of a visual QC test. Five options are available:
#' \itemize{
#' \item  "Temperature": a graph of temperature vs. time  ordered by chambers
#' \item  "Total.O2.chambers": a graph of dissolved oxygen vs. time ordered by chambers
#' \item  "Total.O2.phases": a graph of dissolved oxygen vs. time ordered by chambers and phases
#' \item  "Corrected.O2.chambers": a graph of dissolved oxygen corrected for background respiration vs. time ordered by chambers
#' \item  "Corrected.O2.phases": a graph of dissolved oxygen corrected for background respiration vs. time ordered by chambers and phases
#'  }
#'
#' @importFrom lattice xyplot
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' \dontrun{
#' # if the data have been already loaded to R,
#' # skip the first line of the code:
#' data(SMR.clean)
#'
#' QC.meas(SMR.clean, "Temperature")
#' QC.meas(SMR.clean, "Total.O2.phases")
#' QC.meas(SMR.clean, "Corrected.O2.phases")
#' QC.meas(SMR.clean, "Total.O2.chambers")
#' QC.meas(SMR.clean, "Corrected.O2.chambers")
#' }
#' @export

QC.meas <- function(clean.data,
                    QC = c("Temperature", "Total.O2.phases", "Corrected.O2.phases",
                           "Total.O2.chambers", "Corrected.O2.chambers")){

  if (QC == "Temperature"){
    return(xyplot(Temp~Time|Phase*Chamber.No, data=clean.data,
                  par.strip.text=list(cex=0.6), cex=0.5,
                  xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), as.table = T,
                  scales=list(tck=c(1,0), x=list(cex=0.7), y=list(cex=0.7))))
  }
  else if (QC == "Total.O2.phases"){
    return(xyplot(O2~Time|Phase*Chamber.No, data=clean.data,
                  par.strip.text=list(cex=0.6), cex=0.5,
                  xlab = "Time (s)", ylab = paste("DO (", clean.data$DO.unit[1], "/L)", sep = ""), as.table = T,
                  scales=list(tck=c(1,0), x=list(cex=0.7), y=list(cex=0.7))))
  }

  else if (QC == "Corrected.O2.phases"){
    return(xyplot(O2.correct~Time|Phase*Chamber.No, data=clean.data,
                  par.strip.text=list(cex=0.6), cex=0.5,
                  xlab = "Time (s)", ylab = paste("DO (", clean.data$DO.unit[1], "/L)", sep = ""), as.table = T,
                  scales=list(tck=c(1,0), x=list(cex=0.7), y=list(cex=0.7))))
  }

  else if (QC == "Total.O2.chambers"){
    return(xyplot(O2~Date.Time|Chamber.No, data=clean.data, cex=0.5,
                  xlab = "Time", ylab = paste("DO (", clean.data$DO.unit[1], "/L)", sep = ""), as.table = T))
  }

  else if (QC == "Corrected.O2.chambers"){
    return(xyplot(O2.correct~Date.Time|Chamber.No, data=clean.data, cex=0.5,
                  xlab = "Time", ylab = paste("DO (", clean.data$DO.unit[1], "/L)", sep = ""), as.table = T))
  }

  else
  {
    print ("This quality test does not exist! Please, choose among avaliable variants: Temperature, Total.O2.chambers, Total.O2.phases, Corrected.O2.chambers or Corrected.O2.phases")
  }
}
