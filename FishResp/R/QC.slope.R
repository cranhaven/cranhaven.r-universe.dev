#' Quality Control of Slope(s)
#'
#' Graphical quality control test of extracted slopes represents a visual comparison of linear regression of corrected \eqn{O_{2}} concentration over time with current and alternative length of measurements.
#'
#' @usage
#' QC.slope(slope.data, clean.data,
#'          chamber = c("CH1", "CH2", "CH3", "CH4",
#'                      "CH5", "CH6", "CH7", "CH8"),
#'          current = 999999, alter = 999999, residuals = FALSE)
#'
#' @param slope.data  a data frame obtained by using the function \code{\link{extract.slope}}
#' @param clean.data  a data frame obtained by using the function \code{\link{correct.meas}}
#' @param chamber  string: the chamber chosen for the QC test
#' @param current  integer: current length of measurements for slope estimation (in seconds, black line)
#' @param alter  integer: alternative length of measurements for slope estimation (in seconds, red line)
#' @param residuals  logical: if TRUE then regression diagnostic graphs are plotted for each slope estimation (black graphs: for current slope estimation; red graphs: for alternative slope estimation). More information on diagnostic graphs can be found in the documentation of the function \code{\link{plot.lm}}.
#'
#' @importFrom chron chron times
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot axis box layout lines mtext panel.smooth points
#' @importFrom stats coef lm predict.lm qqline rstandard
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first four lines of the code:
#' data(SMR.clean)
#' data(SMR.slope)
#' data(AMR.clean)
#' data(AMR.slope)
#'
#' QC.slope(SMR.slope, SMR.clean, chamber = "CH1",
#'          current = 1200, alter = 600)
#'
#' QC.slope(AMR.slope, AMR.clean, chamber = "CH4",
#'          current = 600, alter = 300, residuals = TRUE)
#'
#' @export

QC.slope <- function(slope.data, clean.data,
                     chamber = c("CH1", "CH2", "CH3", "CH4",
                                 "CH5", "CH6", "CH7", "CH8"),
                     current = 999999, alter = 999999, residuals = FALSE){

  #==================================================================================================================================================#
  ### Plotting Raw Data -- subsetting those values with the minimal oxygen consumption (i.e. lowest resting metabolic rate)
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  Chamber.No <- Phase <- m1.df <- m2.df <- Time <- NULL

  chlevels<-levels(slope.data$Chamber.No)
  extracted.data<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                             Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric(), Temp=numeric(), O2=numeric(), BOD=numeric(), O2.correct=numeric())

  for(i in 1:length(chlevels)){
    meas<-as.character(subset(slope.data, Chamber.No==chlevels[i])$Phase)
    chlevels.df<-subset(clean.data, Chamber.No==chlevels[i])
    for (m in 1:length(meas)){
      out.df<-subset(chlevels.df, Phase==meas[m])
      row.names(out.df)<-NULL
      extracted.data<-rbind(extracted.data, out.df)
      }
    }

  rm(chlevels)
  rm(i)
  rm(meas)
  rm(chlevels.df)
  rm(m)
  rm(out.df)

  a <-length(slope.data$Chamber.No[slope.data$Chamber.No == chamber])

  if (a <= 3){
    par(mfrow = c(a, 1))
    }
  else{
    dev.new()
    par(mfrow = c(3, 1), ask = T)
    }

  rm(a)
  chamber.df<-subset(extracted.data, Chamber.No == chamber)
  meas<-unique(as.character(chamber.df$Phase))

  if(residuals == FALSE){
    for(m in 1:length(meas)){
      m.df<-subset(chamber.df, Phase==meas[m])
      m1.df<-subset(m.df, Time<=current)
      m2.df<-subset(m.df, Time<=alter)
      model.1<-lm(O2.correct~Time, data=m1.df)
      model.2<-lm(O2.correct~Time, data=m2.df)
      plot(m.df$O2.correct~m.df$Time, main=meas[m], las=1, col = "steelblue2",
           xlab = "Time (s)", ylab = paste("DO (", slope.data$DO.unit[1], "/L)", sep = ""))

      abline(coef(model.1)[1], coef(model.1)[2], col="black", lwd=2)
      abline(coef(model.2)[1], coef(model.2)[2], col = "red", lwd=2, lty=2)
      legend("topright", legend=c(expression(),
                                  bquote(.(current) ~ "s:" ~ r^2 ~ "=" ~ .(round(summary(model.1)$r.sq, digits=3)) ~ "; slope" ~ "=" ~ .(round(coef(model.1)[2], digits=5))),
                                  bquote(.(alter) ~ "s:" ~ r^2 ~ "=" ~ .(round(summary(model.2)$r.sq, digits=3)) ~ "; slope" ~ "=" ~ .(round(coef(model.2)[2], digits=5)))),
                                  lty=c(1,2), lwd=c(2,2), col=c("black", "red"))
      }
    }


  if(residuals == TRUE){
    for(m in 1:length(meas)){
      m.df<-subset(chamber.df, Phase==meas[m])
      m1.df<-subset(m.df, Time<=current)
      m2.df<-subset(m.df, Time<=alter)
      model.1<-lm(O2.correct~Time, data=m1.df)
      model.2<-lm(O2.correct~Time, data=m2.df)

      layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))

      plot(m.df$O2.correct~m.df$Time, main=meas[m], las=1, col = "steelblue2",
           xlab = "Time (s)", ylab = paste("DO (", slope.data$DO.unit[1], "/L)", sep = ""))

      abline(coef(model.1)[1], coef(model.1)[2], col="black", lwd=2)
      abline(coef(model.2)[1], coef(model.2)[2], col = "red", lwd=2, lty=2)
      legend("topright", legend=c(expression(),
                                  bquote(.(current) ~ "s:" ~ r^2 ~ "=" ~ .(round(summary(model.1)$r.sq, digits=3)) ~ "; slope" ~ "=" ~ .(round(coef(model.1)[2], digits=5))),
                                  bquote(.(alter) ~ "s:" ~ r^2 ~ "=" ~ .(round(summary(model.2)$r.sq, digits=3)) ~ "; slope" ~ "=" ~ .(round(coef(model.2)[2], digits=5)))),
                                  lty=c(1,2), lwd=c(2,2), col=c("black", "red"))

      panel.smooth.1 <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
        cex = 1, col.smooth = "black", span = 2/3, iter = 3, ...){
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
        }

      plot(model.1, which = 1, col = "steelblue2", panel = panel.smooth.1, id.n = 0)
      abline(h = 0, lty = 3, col = "black")
      plot(model.1, which = 2, col = "steelblue2", id.n = 0)
      qqline(rstandard(model.1), col = "black", lwd = 1, lty=3)
      mtext(paste("Regression Diagnostic for ", current, "s", sep = ""), side = 3, line = 2, font = 2, col.main= "blue")
      plot(model.1, which = 3, col = "steelblue2", panel = panel.smooth.1, id.n = 0)
      plot(model.2, which = 1, col = "steelblue2", panel = panel.smooth, caption = NULL, col.lab="red", id.n = 0)
      box(col = 'red')
      axis(1, col = 'red', col.axis = 'red', col.ticks = 'red')
      axis(2, col = 'red', col.axis = 'red', col.ticks = 'red')
      abline(h = 0, lty = 3, col = "red")
      mtext("Residuals vs Fitted", side = 3, line = 0.3, col= "red")
      plot(model.2, which = 2, col = "steelblue2", panel = panel.smooth, caption = NULL, col.lab="red", id.n = 0)
      qqline(rstandard(model.2), col = "red", lwd = 1, lty=3)
      box(col = 'red')
      axis(1, col = 'red', col.axis = 'red', col.ticks = 'red')
      axis(2, col = 'red', col.axis = 'red', col.ticks = 'red')
      mtext(paste("Regression Diagnostic for ", alter, "s", sep = ""), side = 3, line = 2, font = 2, col = "red")
      mtext("Normal Q-Q", side = 3, line = 0.3, col= "red")
      plot(model.2, which = 3, col = "steelblue2", panel = panel.smooth, caption = NULL, col.lab="red", id.n = 0)
      mtext("Scale-Location", side = 3, line = 0.3, col= "red")
      box(col = 'red')
      axis(1, col = 'red', col.axis = 'red', col.ticks = 'red')
      axis(2, col = 'red', col.axis = 'red', col.ticks = 'red')

      rm(model.1)
      rm(model.2)
      }
    }

  rm(meas)
  rm(chamber.df)
  rm(m)
  rm(m.df)
  rm(m1.df)
  rm(m2.df)
  }
