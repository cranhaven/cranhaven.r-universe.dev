#' Extraction of Slope(s)
#'
#' The function extracts the slopes of the linear regression of corrected \eqn{O_{2}} concentration over time with defined parameters (see Arguments).
#'
#' @usage
#' extract.slope(clean.data,
#'               method = c("all", "min", "max",
#'                          "lower.tail", "upper.tail",
#'                          "calcSMR.mlnd", "calcSMR.quant",
#'                          "calcSMR.low10", "calcSMR.low10pc"),
#'               r2=0.95, length = 999999, n.slope = 1000,
#'               percent = 10, p = 0.25, G = 1:4)
#'
#' @param clean.data  a data frame obtained by using the function \code{\link{correct.meas}}
#' @param method  string: the method of extracting slopes:
#'  \itemize{
#'   \item 'all' all slopes
#'   \item 'min' extracts lowest absolute slopes, specify the number of extracted\cr slopes (parameter: n.slope)
#'   \item 'max' extracts highest absolute slopes, specify the number of extracted slopes (parameter: n.slope)
#'   \item 'lower.tail' extracts slopes from a lower tail of absolute slope distribution, specify percentage of a lower tail (parameter: percent)
#'   \item 'upper.tail' extracts slopes from an upper tail of absolute slope distribution, specify percentage of an upper tail (parameter: percent)
#'   \item 'calcSMR.mlnd' calculates the mean of the lowest normal distribution\cr (MLND) using the parameter G (see Appendix S1 in Chabot et al, 2016)
#'   \item 'calcSMR.quant' calculates quantile value of slope distribution using the parameter p (see Appendix S1 in Chabot et al, 2016)
#'   \item 'calcSMR.low10' calculates the mean of the 10 lowest absolute slopes (see Appendix S1 in Chabot et al, 2016)
#'   \item 'calcSMR.low10pc' calculates the mean of the lowest 10% of absolute slopes, after the 5 from 10 lowest have been removed as outliers (see Herrmann & Enders, 2000; Appendix S1 in Chabot et al, 2016)
#'   }
#' @param r2  numeric: minimal coefficient of determination (\eqn{r^{2}}) for extracted slopes. Coefficient of determination is used as a threshold of quality to be determined by the user (by default \eqn{r^{2}} = 0.95)
#' @param length  integer: length of a measurement period for slope calculations (in seconds; by default - full length)
#' @param n.slope  integer: the number of extracted slopes, only one slope is calculated for each measurement phase (used in the methods "min" and "max"; by default - all slopes)
#' @param percent integer: percentage of lower or upper tail (used in the methods "lower.tail" and "upper.tail", respectively; by default percent = 10)
#' @param p integer: p-value of quantile used in the method "calcSMR.quant" (by default p = 0.25)
#' @param G integer: G value is used in the method "calcSMR.mlnd" (by default G = 1:4)
#'
#' @return The function returns a data frame with the information about extracted slopes. The data frame is used in the functions \code{\link{QC.slope}} and \code{\link{calculate.MR}}.
#'
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#' @importFrom chron chron times
#' @importFrom grDevices dev.new
#' @importFrom stats coef lm predict.lm quantile time
#' @importFrom utils head read.table tail write.table
#'
#' @references {
#' \enumerate{
#'   \item Chabot, D., Steffensen, J. F., & Farrell, A. P. (2016). The determination of standard metabolic rate in fishes. Journal of Fish Biology, 88(1), 81-121.
#'   \item Herrmann, J. P., & Enders, E. C. (2000). Effect of body size on the standard metabolism of horse mackerel. Journal of Fish Biology, 57(3), 746-760.
#'   }
#' }
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first two lines of the code:
#' data(SMR.clean)
#' data(AMR.clean)
#'
#' SMR.slope <- extract.slope(SMR.clean,
#'                            method = "min",
#'                            n.slope = 3,
#'                            r2=0.95,
#'                            length = 1200)
#'
#' AMR.slope <- extract.slope(AMR.clean,
#'                            method = "all",
#'                            r2=0.95,
#'                            length = 300)
#'
#' @export

extract.slope <- function(clean.data, method = c("all", "min", "max", "lower.tail", "upper.tail",
                                                 "calcSMR.mlnd", "calcSMR.quant", "calcSMR.low10", "calcSMR.low10pc"),
                          r2=0.95, length = 999999, n.slope = 1000, percent = 10, p = 0.25, G = 1:4){

  Chamber.No <- Phase <- out.df <- R2 <- NULL
  MR.est.all<-data.frame(Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Date.Time = chron(), Phase=factor(),
                         Temp=numeric(), Slope.with.BR=numeric(), Slope=numeric(), SE=numeric(), R2=numeric())

  chamber<-levels(clean.data$Chamber.No)
  meas<-levels(clean.data$Phase)

  for(i in 1:length(chamber)){
    chamber.df<-subset(clean.data, Chamber.No==chamber[i])
    for(m in 1:length(meas)){
      m.df<-subset(chamber.df, Phase==meas[m])
      m.df<-subset(m.df, m.df$Time<=length)
      model.with.BR <- lm(O2~Time, data=m.df)
      model<-lm(O2.correct~Time, data=m.df)
      out.df<-data.frame(Chamber.No=head(m.df, 1)$Chamber.No, Ind=head(m.df, 1)$Ind, Mass=head(m.df, 1)$Mass, Volume=head(m.df, 1)$Volume,
                         Date.Time = tail(m.df, 1)$Date.Time, Phase=head(m.df, 1)$Phase, Temp=mean(m.df$Temp), Slope.with.BR = coef(model.with.BR)[2],
                         Slope=coef(model)[2],  SE=summary(model)$coef[4], R2=summary(model)$r.squared)
      row.names(out.df)<-NULL
      MR.est.all<-rbind(MR.est.all, out.df)
    }
  }
  head(MR.est.all)
  rm(chamber)
  rm(meas)
  rm(i)
  rm(chamber.df)
  rm(m)
  rm(m.df)
  rm(model)
  rm(out.df)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  MR.est<-data.frame(Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Phase=factor(), Temp=numeric(),
                     Slope.with.BR=numeric(), Slope=numeric(), SE=numeric(), R2=numeric())

  # I've tweaked the code slightly here to ensure that we're only looking at data which fit a model of linear O2 consumption
  MR.temporal<-subset(MR.est.all, R2>=r2)
  chamber<-levels(MR.temporal$Chamber.No)

  for(i in 1:length(chamber)){
    chamber.df<-subset(MR.temporal, Chamber.No==chamber[i])

    if(method == "all"){
      s<-order(chamber.df$Slope)
      out.df<-chamber.df[s,]
      rm(s)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "min"){
      s<-head(order(chamber.df$Slope, decreasing=T),n.slope)
      out.df<-chamber.df[s,]
      rm(s)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "max"){
      s<-head(order(chamber.df$Slope, decreasing=F),n.slope)
      out.df<-chamber.df[s,]
      rm(s)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "lower.tail"){
      rate <- as.numeric(quantile(abs(chamber.df$Slope), percent/100))
      s <- chamber.df$Phase[abs(chamber.df$Slope) <= rate]
      out.df<-chamber.df[chamber.df$Phase %in% s,]
      rm(rate, s)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "upper.tail"){
      rate <- as.numeric(quantile(chamber.df$Slope, percent/100))
      s <- chamber.df$Phase[chamber.df$Slope <= rate]
      out.df<-chamber.df[chamber.df$Phase %in% s,]
      rm(rate, s)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "calcSMR.mlnd"){
      the.Mclust <- Mclust(chamber.df$Slope,  G=G)
      cl <- the.Mclust$classification
      cl2 <- as.data.frame(table(cl))
      cl2$cl <- as.numeric(levels(cl2$cl))
      valid <- cl2$Freq>=0.1*length(time)
      the.cl <- max(cl2$cl[valid])
      left.distr.R2 <- chamber.df$R2[the.Mclust$classification==the.cl]
      left.distr.Temp <- chamber.df$Temp[the.Mclust$classification==the.cl]
      left.distr.Slope.with.BR <- chamber.df$Slope.with.BR[the.Mclust$classification==the.cl]
      left.distr.SE <- chamber.df$SE[the.Mclust$classification==the.cl]
      left.distr <- chamber.df$Slope[the.Mclust$classification==the.cl]
      mlnd = the.Mclust$parameters$mean[the.cl]
      s <- as.numeric(mlnd)

      out.df<-chamber.df[1,]
      out.df$Date.Time<-NA
      out.df$Phase<-"M"
      out.df$Temp<-mean(left.distr.Temp)
      out.df$Slope.with.BR<-mean(left.distr.Slope.with.BR)
      out.df$Slope<-s
      out.df$SE<-mean(left.distr.SE)
      out.df$R2<-mean(left.distr.R2)

      rm(s, the.Mclust, cl, cl2, valid, the.cl, left.distr, mlnd)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "calcSMR.quant"){
      quant=quantile(abs(chamber.df$Slope), p)
      rate <- as.numeric(-abs(quant))
      s <- which.min(abs(chamber.df$Slope - rate))
      out.df <- chamber.df[s,]
      out.df$Slope <- rate
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
      rm(quant, rate, s)
    }
    else if (method == "calcSMR.low10"){
      u = sort(chamber.df$Slope, decreasing = TRUE)
      max.low10=max(u[1:10])
      min.low10=min(u[1:10])
      low10=mean(u[1:10])
      s <- as.numeric(low10)

      out.df<-chamber.df[1,]
      out.df$Date.Time<-NA
      out.df$Phase<-NA
      out.df$Temp<-mean(chamber.df$Temp[chamber.df$Slope >= min.low10])
      out.df$Slope.with.BR<-mean(chamber.df$Slope.with.BR[chamber.df$Slope >= min.low10])
      out.df$Slope<-s
      out.df$SE<-NA
      out.df$R2<-mean(chamber.df$R2[chamber.df$Slope >= min.low10])

      rm(s, u, low10, max.low10, min.low10)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else if (method == "calcSMR.low10pc"){
      u = sort(chamber.df$Slope, decreasing = TRUE)
      max.low10pc=max(u[6:10])
      min.low10pc=min(u[6:10])
      low10pc = mean(u[6:(5 + round(0.1*(length(u)-5)))])
      s <- as.numeric(low10pc)
      u.BR = chamber.df$Slope.with.BR[chamber.df$Slope >= min.low10pc]
      low10pc.BR = mean(u.BR[6:(5 + round(0.1*(length(u.BR)-5)))])
      s.BR <- as.numeric(low10pc.BR)
      out.df<-chamber.df[1,]
      out.df$Date.Time<-NA
      out.df$Phase<-NA
      out.df$Temp<-mean(chamber.df$Temp[chamber.df$Slope <= max.low10pc & chamber.df$Slope >= min.low10pc])
      out.df$Slope.with.BR<-s.BR
      out.df$Slope<-s
      out.df$SE<-NA
      out.df$R2<-mean(chamber.df$R2[chamber.df$Slope <= max.low10pc & chamber.df$Slope >= min.low10pc ])

      rm(s, u, low10pc, max.low10pc, min.low10pc)
      row.names(out.df)<-NULL
      MR.est<-rbind(MR.est, out.df)
    }
    else{
      print("Please, choose an appropriate method from available variants")
    }
  }

  rm(chamber)
  rm(i)
  rm(chamber.df)
  rm(out.df)
  rm(MR.temporal)

  slope.data <- MR.est
  slope.data$Volume <- as.numeric(as.character(slope.data$Volume))
  slope.data$Mass <- as.numeric(as.character(slope.data$Mass))
  slope.data$DO.unit <- clean.data$DO.unit[1]
  return(slope.data)
}
