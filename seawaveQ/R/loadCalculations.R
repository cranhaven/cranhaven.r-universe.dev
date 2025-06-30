#' Function to calculate pesticide loads in kilograms per year and summarize trends.
#' 
#' Parameter load (mass) is the product of water-quality concentration 
#' (a mass per volume) and an associated streamflow rate (volume per time). This
#' function generates an annual time series of pesticide loads on either a
#' calendar year basis or a water year basis and summarizes load trends.
#' @name loadCalculations
#' @title Calculate annual loads and summarize trends
#' @note In this load calculation function, daily pesticide concentration 
#' estimates provided by the \code{fitswavecav} function are corrected for 
#' retransformation bias (the concentration model is built on the base-10 
#' logarithm of concentration; therefore, a bias correction is required when 
#' transforming back to the original units) and then used to calculate daily 
#' loads. The bias correction is based on the quasi-maximum likelihood estimator 
#' (Cohn and others, 1989) that was developed for natural logarithms, with an 
#' adjustment for the base-10 logarithm of the concentration. To calculate 
#' loads, the bias-corrected concentration estimates (assumed to be in 
#' micrograms per liter) are multiplied by daily streamflow and a constant, 
#' 0.892998605, which converts the load units 
#' (micrograms per liter * cubic feet per second) to kilograms per year. 
#' Daily loads are summed to annual values. See page 70 and equation 26 of 
#' Oelsner and others (2017) for further details regarding the load 
#' calculation and bias correction.
#' Users may modify this function to convert to units other than kilograms
#' per year.
#' @param dailyDat is the daily streamflow data in the form of a data frame
#' with three columns representing a station ID, date, and streamflow.
#' @param pestPredict is the continuous (daily) estimation of pesticide
#' concentrations for one or more pesticides at a single site. This should be
#' in the form of the fourth element of the list returned by \code{fitswavecav}.
#' @param modRes is the first element of the list returned by \code{fitswavecav} 
#' and includes the scale parameter for one or more pesticide trend models at
#' a single site. The scale parameter is used in the bias correction.
#' @param concTrends the SEAWAVE-Q trend in flow-normalized annual load. Cannot 
#' be different (computationally) from the trend in flow-normalized annual 
#' concentration when there is no trend in flow (Oelsner and others, 2017).
#' @param yrtype allows one to calculate annual loads based on a calendar 
#' year or a water year, where a water year is the 12-month period October 1 
#' through September 30 designated by the calendar year in which it ends. A 
#' yrtype of 1 represents a calendar year and is the default because that
#' is the way the original model was developed. A yrtype of 2 represents a
#' water year. 
#' @param alpha is the significance level or alpha value for statistical
#' significance and confidence intervals.
#' @keywords datagen ts
#' @return Two data frames, the first contains the annual loads, the second
#' contains the trend summary.
#' @format The first data frame returned has one row for each pesticide-year at
#' a particular site and four columns. The general format is as follows: \cr
#' \tabular{lll}{
#'  pstaid \tab character \tab the station identification number \cr
#'  pcode \tab character \tab the parameter code for which a load was calculated\cr
#'  year or wyear \tab numeric \tab the year or water year for which a load was calculated \cr
#'  load \tab numeric \tab the load in kilograms per year \cr
#' }
#' The second data frame returned has one row for each pesticide at
#' a particular site and 11 columns. The general format is as follows: \cr
#' \tabular{lll}{
#'  pcode \tab character \tab the parameter code for which a load trend was calculated\cr
#'  mclass \tab numeric \tab a value of 1 or 2\cr
#'  mclass \tab numeric \tab a value of 1 or 2\cr
#'  alpha \tab numeric \tab a significance level \cr
#'  ltndPpor \tab numeric \tab the load trend in percent over the period of record \cr
#'  luciPpor \tab numeric \tab the load upper confidence interval for the trend in\cr
#'   \tab \tab percent over the period of record \cr
#'  llciPpor \tab numeric \tab the load lower confidence interval for the trend in\cr
#'   \tab \tab percent over the period of record \cr
#'  baseLoad \tab numeric \tab the base load, the load for the first year of trend period \cr
#'  ltndOrigPORPercentBase \tab numeric \tab the load trend in original units over\cr
#'   \tab \tab the period of record\cr
#'   \tab \tab (calculation based on percent per year and base load)\cr
#'  luciOrigPORPercentBase \tab numeric \tab the load trend upper confidence interval\cr
#'   \tab \tab for the trend in original units over the period of record\cr
#'   \tab \tab (calculation based on percent per year and base load)\cr
#'  llciOrigPORPercentBase \tab numeric \tab the load trend lower confidence interval\cr
#'   \tab \tab for the trend in original units over the period of record\cr
#'   \tab \tab (calculation based on percent per year and base load)\cr
#'  ltndlklhd \tab numeric \tab is the load trend likelihood \cr
#' }
#' @export
#' @author Karen R. Ryberg
#' @examples
#' data(swData)
#' modMoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha)
#' myfit1 <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha, 
#' tanm = "myfit1", pnames = c("04035", "04037", "04041"), yrstart = 1995, 
#' yrend = 2003, tndbeg = 1995, tndend = 2003, iwcav = c("flowa30", "flowa1"), 
#' dcol = "dates", qwcols = c("R", "P"))
#' MoRivOmahaLoadsYr <- loadCalculations(cqwMoRivOmaha[, 1:3], myfit1[[4]], 
#' myfit1[[1]], myfit1[[6]])
#' MoRivOmahaLoadsYr
#' @references
#' Cohn, T.A., DeLong, L.L., Gilroy, E.J., Hirsch, R.M., and Wells, D.K., 1989, 
#' Estimating constituent loads: Water Resources Research, v. 25, no. 5, 
#' p. 937--942.
#'
#' Oelsner, G.P., Sprague, L.A., Murphy, J.C., Zuellig, R.E., Johnson, H.M., 
#' Ryberg, K.R., Falcone, J.A., Stets, E.G., Vecchia, A.V., Riskin, M.L., 
#' De Cicco, L.A., Mills, T.J., and Farmer, W.H., 2017, Water-quality trends in 
#' the Nation's rivers and streams, 1972--2012---Data preparation, statistical 
#' methods, and trend results (ver. 2.0, October 2017): U.S. Geological Survey 
#' Scientific Investigations Report 2017--5006, 136 p., 
#' \url{https://doi.org/10.3133/sir20175006}.
#' 
loadCalculations <- function(dailyDat, pestPredict, modRes, concTrends, 
                             yrtype = 1, alpha = 0.10) {
  pcode <- NULL
  # compute a decimal day that matches the one in predictions
  DecimalTime <- function(date) {
    yr <- year(date)
    mo <- month(date)
    da <- day(date)
    dectime <- yr + (mo - 1)/12 + (da - 0.5)/366
    dectime
  }
  dailyDat$dectime <- round(DecimalTime(dailyDat$dates), digits = 3)
  dailyDat$year <- year(dailyDat$dates)
  dailyDat$month <- month(dailyDat$dates)
  dimnames(dailyDat)[[2]][1] <- "pstaid"
  dimnames(dailyDat)[[2]][3] <- "Q"
  dailyDat$wyear <- dailyDat$year
  dailyDat$wyear[dailyDat$month > 9] <- dailyDat$year[dailyDat$month > 9] + 1
  pestPredict$dectime <- round(pestPredict$dectime, digits = 3)
  pestPredict <- melt(pestPredict, id = "dectime", na.rm = TRUE)
  dimnames(pestPredict)[[2]][2:3] <- c("pcode", "conc")
  mergedDat <- join(pestPredict, dailyDat, by = "dectime")
  modRes$pcode <- paste("P", modRes$pname, sep = "")
  mergedDat <- join(mergedDat, modRes[, c("scl", "pcode")], by = "pcode")
  o <- order(mergedDat$pstaid, mergedDat$pcode, mergedDat$dectime)
  mergedDat <- mergedDat[o, ]
  # bias correction for concentration
  # fitswavecav returns back transformed concentration values, but they are not
  # bias corrected
  mergedDat$biascorconc <- mergedDat$conc * exp(0.5 * (log(10) * mergedDat$scl) ^ 2)
  k <- 0.8929986
  # load in kilograms per year
  mergedDat$load <- mergedDat$biascorconc * mergedDat$Q * k
  mergedDat <- mergedDat[, c("pstaid", "pcode", "year", "wyear", "load")]
  if (yrtype == 1) {
    # calendar year load
    loads <- aggregate(load ~ pstaid + pcode + year, data = mergedDat, sum)
    o <- order(loads$pstaid, loads$pcode, loads$year)
  } else if (yrtype == 2) {
    # water year load
    loads <- aggregate(load ~ pstaid + pcode + wyear, data = mergedDat, sum)
    o <- order(loads$pstaid, loads$pcode, loads$wyear)
  } else {
    warning("yrtype argument must be a numeric 1 or 2")
  }    
  loads <- loads[o, ]
  
  trends <- data.frame(concTrends[, 1:2])
  o <- order(trends$pname)
  trends <- trends[o, ]
  trends$pcode <- paste0("P", trends$pname)
  trends <- trends[, c(3, 2)]
  mycolNams <- c("alpha", "ltndPpor", "luciPpor", "llciPpor", "baseLoad", 
                 "ltndOrigPORPercentBase", "luciOrigPORPercentBase", 
                 "llciOrigPORPercentBase", "ltndlklhd")
  trends[mycolNams] <- NA
  
  for (i in 1:dim(trends)[[1]]) {
    subloads <- subset(loads, pcode == trends$pcode[i])
    ctnd <- modRes[i, ]$cxmattndlin
    setnd <- modRes[i, ]$sexmattndlin
    if (yrtype == 1 ) {
      por <- subloads$year[length(subloads$year)] - subloads$year[1]
    } else if (yrtype == 2) {
      por <- subloads$wyr[length(subloads$wyr)] - subloads$wyr[1]
    }
    pval <- modRes[i, ]$pvalxmattndlin
    ltndOrigPORPercentBase <- formatC(subloads[1, "load"] * ((10 ^ ctnd) ^ por) - subloads[1, "load"], 
                                    digits = 2, flag = "#", format = "f")
    
    formatC(subloads[1, "load"] * ((10 ^ (ctnd + qnorm(1 - alpha/2) * setnd)) ^ por) - subloads[1, "load"], 
            digits = 2, flag = "#", format = "f") 
    
    luciOrigPORPercentBase <- formatC(subloads[1, "load"] * 
                                      ((10 ^ (ctnd + qnorm(1 - alpha/2) * 
                                              modRes[i, ]$sexmattndlin)) ^ por) - 
                                        subloads[1, "load"], digits = 2, flag = "#", 
                                      format = "f") 
    llciOrigPORPercentBase <- formatC(subloads[1, "load"] * 
                                      ((10 ^ (ctnd - qnorm(1 - alpha/2) * 
                                              modRes[i, ]$sexmattndlin)) ^ por) - 
                                        subloads[1, "load"], digits = 2, flag = "#", 
                                      format = "f") 
    cat("The change over the period of record, based on a trend in percent of", 
        "\n", "base load, in original units is: ", 
        ltndOrigPORPercentBase, ", ", 100*(1 - alpha), "% CI ", 
        "[", llciOrigPORPercentBase, ", ", luciOrigPORPercentBase, "]", 
        ", \n where base load was ", 
        formatC(subloads[1, "load"], digits = 2, flag = "#", format = "f"), ".\n", 
        sep = "")
    
    ltndlklhd <- round(1 - pval/2, digits = 4)
    
    trends[i, 3:11] <- c(alpha, concTrends[i, 4:6], round(subloads[1, "load"], digits = 2),
                         ltndOrigPORPercentBase, luciOrigPORPercentBase, llciOrigPORPercentBase,
                         ltndlklhd)
    cat("\n", "\n")
  }
  
  loadsTrends <- list(loads, trends)
  return(loadsTrends)
}
