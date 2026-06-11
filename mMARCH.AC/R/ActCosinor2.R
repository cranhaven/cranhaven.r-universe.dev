#' @title Cosinor Model for Circadian Rhythmicity
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.
#'
#'
#' @param x \code{vector} vector of dimension n*1440 which reprsents n days of 1440 minute activity data
#' @param window The calculation needs the window size of the data. E.g window = 1 means each epoch is in one-minute window.
#' @param n1440, the number of points of a day. Default is 1440 for the minute-level data. 
#'
#'
#' @importFrom cosinor cosinor.lm
#' @importFrom cosinor2 correct.acrophase
#'
#' @return A list with elements
#' \item{mes}{MESOR which is short for midline statistics of rhythm, which is a rhythm adjusted mean. This represents mean activity level.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{acro}{acrophase, a meaure of the time of the overall high values recurring in each cycle. Here it has a unit of radian. This represents time to reach the peak.}
#' \item{acrotime}{acrophase in the unit of the time (hours)}
#' \item{ndays}{Number of days modeled}
#'
#'
#' @references Cornelissen, G. Cosinor-based rhythmometry. Theor Biol Med Model 11, 16 (2014). https://doi.org/10.1186/1742-4682-11-16
#' @export 



ActCosinor2 = function(
  x,
  window = 1,
  n1440 =1440
){
 
  if(1440 %% window != 0){
   # stop("Only use window size that is an integer factor of 1440") #gw 5/5/21
  }


  if(length(x) %% (1440/window) != 0){
    stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
  }
  
 
  dim = n1440/window     #gw 5/5/21 , x= vector for ndays
  n.days = length(x)/dim
  n60 = n1440/24         #gw 5/5/21


  tmp.dat = data.frame(time = rep(1:dim, n.days) / (n60/window), Y = x) #gw 5/5/21
  #print(dim(tmp.dat))
  #print(head(tmp.dat))

  fit = cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)

  mesor = fit$coefficients[1]
  amp = fit$coefficients[2]
  # acr = fit$coefficients[3]
  acr = correct.acrophase(fit)
  acrotime = (-1) * acr * 24/(2 * pi)

  names(mesor) = names(amp) = names(acr) = names(acrotime) = NULL

  ret = list("mes" = mesor,
             "amp" = amp,
             "acr" = acr,
             "acrotime" = acrotime,
             "ndays" = n.days)

  return(ret)

}




#' @title Cosinor Model for Circadian Rhythmicity for the Whole Dataset
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.This function is a whole dataset
#' wrapper for \code{ActCosinor}.
#'
#' @param count.data \code{data.frame} of dimension n * (p+2) containing the
#' p dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequence of days within each subject.
#' @param window \code{numeric} The calculation needs the window size of the data. E.g window = 1 means each epoch is in one-minute window.
#' @param daylevel  \code{logical}   If the cosinor model was run for day-level data. The default value is FALSE while the activity data for all days were used for model fitting. When the value is TRUE, the single day data were used for model fitting.
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>% do
#'
#'
#' @return A \code{data.frame} with the following 5 columns
#' \item{ID}{ID}
#' \item{ndays}{number of days}
#' \item{mes}{MESRO, which is short for midline statistics of rhythm, which is a rhythm adjusted mean. This represents mean activity level.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{acro}{acrophase, a meaure of the time of the overall high values recurring in each cycle. Here it has a unit of radian. This represents time to reach the peak.}
#' \item{acrotime}{acrophase in the unit of the time (hours)}
#' \item{ndays}{Number of days modeled}
#'
#' @export 



ActCosinor_long2 = function(
  count.data,
  window = 1,
  daylevel = FALSE
){
  #window = shrink data into Ncol/window columns.
  ID = Day = value = . = NULL
  rm(list = c("ID", "Day", "value", "."))

  long.count = reshape(count.data, varying = names(count.data)[3:ncol(count.data)],direction = "long",
                       timevar = "Time",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:((ncol(count.data)-2)*nrow(count.data))))
  long.count = long.count[
    with(long.count, order(ID, Day,Time)),
    ]   # long.count= ID, day, time,values

  n1440=ncol(count.data)-2                      #gw 5/5/21
  if (!daylevel) result= long.count  %>% group_by(ID) %>% do(out = ActCosinor2(.$values,
                                               window = window, n1440=n1440))  #gw 5/5/21


  if (daylevel) result= long.count  %>% group_by(ID,Day) %>% do(out = ActCosinor2(.$values,
                                               window = window, n1440=n1440))  #gw 4/28/23  daylevel fitting, output=  ID  Day  ndays mes_1 amp_1 acr_1 acrotime_1




  out = unlist(result$out)

  result$ndays = out[which(names(out) == "ndays")]
  result$mes = out[which(names(out) == "mes")]
  result$amp = out[which(names(out) == "amp")]
  result$acr = out[which(names(out) == "acr")]
  result$acrotime = out[which(names(out) == "acrotime")]

  result$out = NULL
  last4c<-(ncol(result)-3):ncol(result)
  names(result)[last4c] = paste0(names(result)[last4c],"_",window)
  return(result)

}

