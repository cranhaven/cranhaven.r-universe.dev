#' Quality Control of Animal Activity
#'
#' Graphical quality control tests for animal activity in chambers over the period of measurements defined in the function \code{\link{correct.meas}}. The function is used for determination of time period for calculation of standard or resting metabolic rate. Note, that mass-specific metabolic rate is calculated for each period of measurements (not raw data).
#'
#' @param clean.data  a data frame obtained by using the function \code{\link{correct.meas}}
#' @param compare  logical: if TRUE then two graphs are plotted to compare mass-specific metabolic rate before and after correction for background respiration
#' @param output  logical: if TRUE then the functions return output or print it
#' @usage
#' QC.activity(clean.data, compare = TRUE, output = FALSE)
#'
#' @details QC.activity uses functions \code{\link{extract.slope}} and \code{\link{calculate.MR}} with default parameters (excluding \eqn{r^{2} = 0}) to plot a graph of animal activity
#'
#' @importFrom lattice xyplot strip.custom
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first line of the code:
#' data(SMR.clean)
#'
#' QC.activity(SMR.clean, compare = TRUE)
#'
#' @export

QC.activity <- function(clean.data, compare = TRUE, output = FALSE){
  Chamber.No<-NULL
  slope.data.all<-extract.slope(clean.data, r2=0, method = "all")
  MR.data.all<-calculate.MR(slope.data.all, density = 1000,
                            plot.BR = FALSE, plot.MR.abs = FALSE, plot.MR.mass = FALSE)
  a <- nlevels(MR.data.all$Chamber.No)

  if(compare == TRUE){
    BW = MR.data.all$Mass/1000
    MR.mass.with.BR <- MR.data.all$MR.abs.with.BR/BW
    a <- xyplot(MR.mass + MR.mass.with.BR ~ Date.Time, groups=Chamber.No, grid = TRUE,
           data=MR.data.all, main="Activity during the whole period of measurements",
           type=c("a", "p"), allow.multiple=T, layout=(c(1,2)),
           xlab="Time", ylab = bquote("Mass-specific metabolic rate (" ~ .(clean.data$DO.unit[1]) ~ kg^-1 ~ h^-1 ~ ")"),
           strip = strip.custom(factor.levels = c("MO2 after correction for background respiration",
                                                    "MO2 before correction for background respiration")),
           auto.key=list(space="top", columns=4,
                           title="Chambers", cex.title=1, between.columns = 0.5,
                           lines=TRUE, points = FALSE, cex = 0.8),
           par.settings = list(superpose.line = list(col = c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00", "brown" , "#03c4a1"))))
    plot(a)
    }
  else{
    b <- xyplot(MR.mass ~ Date.Time, groups=Chamber.No, grid = TRUE,
           data=MR.data.all, main="Activity during the whole period of measurements",
           type=c("a", "p", lwd = 2),
           xlab="Time", ylab = bquote("Mass-specific metabolic rate (" ~ .(clean.data$DO.unit[1]) ~ kg^-1 ~ h^-1 ~ ")"),
           auto.key=list(space="top", columns=4, par.strip.text=list(cex=2),
                         title="Chambers", cex.title=1, between.columns = 0.5,
                         lines=TRUE, points = FALSE, cex = 0.8),
           par.settings = list(superpose.line = list(col = c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00", "brown" , "#03c4a1"))))
    plot(b)
  }
  if(output == TRUE){
    return(MR.data.all)
  }
}
