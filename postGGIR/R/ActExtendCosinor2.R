#' @title Extended Cosinor Model for Circadian Rhythmicity
#' @description Extended cosinor model based on sigmoidally transformed cosine curve using anti-logistic transformation
#'
#'
#' @param x \code{vector} vector of dimension n*1440 which represents n days of 1440 minute activity data
#' @param window The calculation needs the window size of the data. E.g window = 1 means each epoch is in one-minute window.
#' @param lower A numeric vector of lower bounds on each of the five parameters (in the order of minimum, amplitude, alpha, beta, acrophase) for the NLS. If not given, the default lower bound for each parameter is set to \code{-Inf}.
#' @param upper A numeric vector of upper bounds on each of the five parameters (in the order of minimum, amplitude, alpha, beta, acrophase) for the NLS. If not given, the default lower bound for each parameter is set to \code{Inf}
#' @param n1440, the number of points of a day. Default is 1440 for the minute-level data.
#'
#' @importFrom cosinor cosinor.lm
#' @importFrom cosinor2 correct.acrophase
#' @importFrom minpack.lm nls.lm nls.lm.control
#' @importFrom stats coef residuals
#'
#' @return A list with elements
#' \item{minimum}{Minimum value of the of the function.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{alpha}{It determines whether the peaks of the curve are wider than the troughs: when alpha is small, the troughs are narrow and the peaks are wide; when alpha is large, the troughs are wide and the peaks are narrow.}
#' \item{beta}{It dertermines whether the transformed function rises and falls more steeply than the cosine curve: large values of beta produce curves that are nearly square waves.}
#' \item{acrotime}{acrophase is the time of day of the peak in the unit of the time (hours)}
#' \item{F_pseudo}{Measure the improvement of the fit obtained by the non-linear estimation of the transformed cosine model}
#' \item{UpMesor}{Time of day of switch from low to high activity. Represents the timing of the rest- activity rhythm. Lower (earlier) values indicate increase in activity earlier in the day and suggest a more advanced circadian phase.}
#' \item{DownMesor}{Time of day of switch from high to low activity. Represents the timing of the rest-activity rhythm. Lower (earlier) values indicate decline in activity earlier in the day, suggesting a more advanced circadian phase.}
#' \item{MESOR}{A measure analogous to the MESOR of the cosine model (or half the deflection of the curve) can be obtained from mes=min+amp/2. However, it goes through the middle of the peak, and is therefore not equal to the MESOR of the cosine model, which is the mean of the data.}
#' \item{ndays}{Number of days modeled.}
#'
#'
#' @references Marler MR, Gehrman P, Martin JL, Ancoli-Israel S. The sigmoidally transformed cosine curve: a mathematical model for circadian rhythms with symmetric non-sinusoidal shapes. Stat Med.
#' @export 


ActExtendCosinor2 = function(
  x,
  window = 1,
  lower = c(0, 0, -1, 0, -3), ## min, amp, alpha, beta, phi
  upper = c(Inf, Inf, 1, Inf, 27),
  n1440=1440
){
  if(1440 %% window != 0){
    stop("Only use window size that is an integer factor of 1440")
  }


  if(length(x) %% (1440/window) != 0){
    stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
  }
 

 
  dim = n1440/window  #gw 5/5/21 
  n.days = length(x)/dim
  n60 = n1440/24 #gw 5/5/21

  # Stage 1 ---- Cosinor Model
  tmp.dat = data.frame(time = rep(1:dim, n.days) / (n60/window), Y = x)
  print(dim(tmp.dat))
  print(head(tmp.dat))
  fit = cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)

  mesor = fit$coefficients[1]
  amp = fit$coefficients[2]
  acr = correct.acrophase(fit)
  acrotime = (-1) * acr * 24/(2 * pi)

  names(mesor) = names(amp) = names(acr) = names(acrotime) = NULL

  # Stage 2 ---- Transformation

  ## Set up the initial values
  e_min0 = max(mesor - amp, 0)
  e_amp0 = 2 * amp
  e_phi0 = acrotime
  e_par0 = c(e_min0, e_amp0, 0, 2, e_phi0) ## min, amp, alpha, beta, phi

  fit_nls<-NULL
  fit_nls = try(nls.lm(e_par0, fn = fn_obj,
                   lower = lower,
                   upper = upper,
                   tmp.dat = tmp.dat,
                   control = nls.lm.control(maxiter = 1000)))
  ## Estimated exteded cosinor parameters,in the order of
  ## minimum, amplitude, alpha, beta, acrophase 
  ## 5/25 fix nls.lm errors for some individual
  e_min = NA
  e_amp = NA
  e_alpha = NA
  e_beta = NA
  e_acrotime = NA  
  F_pseudo = NA 
  UpMesor = NA
  DownMesor = NA
  MESOR = NA  
 if (attr(fit_nls,"class")!="try-error"){ 

  coef.nls = coef(fit_nls)

  e_min = coef.nls[1]
  e_amp = coef.nls[2]
  e_alpha = coef.nls[3]
  e_beta = coef.nls[4]
  e_acrotime = coef.nls[5]

  ## Pseudo F statistics
  RSS_cos = sum((fit$fit$residuals)^2)
  RSS_ext = sum(residuals(fit_nls)^2)
  F_pseudo = ((RSS_cos - RSS_ext)/2)/(RSS_ext/(nrow(tmp.dat) - 5))

  ## Derived metrics
  UpMesor = -acos(e_alpha)/(2*pi/24) + e_acrotime
  DownMesor = acos(e_alpha)/(2*pi/24) + e_acrotime
  MESOR = e_min + e_amp/2
  } #if nls.lm is good

  ret = list("minimum" = e_min,
             "amp" = e_amp,
             "alpha" = e_alpha,
             "beta" = e_beta,
             "acrotime" = e_acrotime,
             "F_pseudo" = F_pseudo,
             "UpMesor" = UpMesor,
             "DownMesor" = DownMesor,
             "MESOR" = MESOR,
             "ndays" = n.days)

  return(ret)

}


## Objective function to optimize for extended cosinor model
fn_obj = function(par, tmp.dat) {
  ct = cos((tmp.dat[, 1] - par[5]) * 2 * pi / 24)
  lct = exp(par[4] * (ct - par[3])) / (1 + exp(par[4] * (ct - par[3])))
  rt = par[1] + par[2] * lct
  tmp.dat[, 2] - rt
}



#' @title Cosinor Model for Circadian Rhythmicity for the Whole Dataset
#' @description Extended cosinor model based on sigmoidally transformed cosine curve using anti-logistic transformation.This function is a whole dataset
#' wrapper for \code{ActExtendCosinor}.
#'
#' @param count.data \code{data.frame} of dimension n * (p+2) containing the
#' p dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequence of days within each subject.
#' @param window The calculation needs the window size of the data. E.g window = 1 means each epoch is in one-minute window.
#' window size as an argument.
#' @param lower A numeric vector of lower bounds on each of the five parameters (in the order of minimum, amplitude, alpha, beta, acrophase) for the NLS. If not given, the default lower bound for each parameter is set to \code{-Inf}.
#' @param upper A numeric vector of upper bounds on each of the five parameters (in the order of minimum, amplitude, alpha, beta, acrophase) for the NLS. If not given, the default lower bound for each parameter is set to \code{Inf}
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>% do
#'
#'
#' @return A \code{data.frame} with the following 5 columns
#' \item{ID}{ID}
#' \item{ndays}{number of days}
#' \item{minimum}{Minimum value of the of the function.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{alpha}{It determines whether the peaks of the curve are wider than the troughs: when alpha is small, the troughs are narrow and the peaks are wide; when alpha is large, the troughs are wide and the peaks are narrow.}
#' \item{beta}{It dertermines whether the transformed function rises and falls more steeply than the cosine curve: large values of beta produce curves that are nearly square waves.}
#' \item{acrotime}{acrophase is the time of day of the peak in the unit of the time (hours)}
#' \item{F_pseudo}{Measure the improvement of the fit obtained by the non-linear estimation of the transformed cosine model}
#' \item{UpMesor}{Time of day of switch from low to high activity. Represents the timing of the rest- activity rhythm. Lower (earlier) values indicate increase in activity earlier in the day and suggest a more advanced circadian phase.}
#' \item{DownMesor}{Time of day of switch from high to low activity. Represents the timing of the rest-activity rhythm. Lower (earlier) values indicate decline in activity earlier in the day, suggesting a more advanced circadian phase.}
#' \item{MESOR}{A measure analogous to the MESOR of the cosine model (or half the deflection of the curve) can be obtained from mes=min+amp/2. However, it goes through the middle of the peak, and is therefore not equal to the MESOR of the cosine model, which is the mean of the data.}
#'
#' @export 


ActExtendCosinor_long2 = function(
  count.data,
  window = 1,
  lower = c(0, 0, -1, 0, -3), ## min, amp, alpha, beta, phi
  upper = c(Inf, Inf, 1, Inf, 27)
){
  ID = value = . = NULL
  rm(list = c("ID", "value", "."))

  long.count = reshape(count.data, varying = names(count.data)[3:ncol(count.data)],direction = "long",
                       timevar = "Time",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:((ncol(count.data)-2)*nrow(count.data))))
  long.count = long.count[
    with(long.count, order(ID, Day,Time)),
  ]

  n1440=ncol(count.data)-2     
  result= long.count  %>% group_by(ID) %>% do(out = ActExtendCosinor2(.$values,
                                                               window = window, lower = lower, upper = upper, n1440=n1440))

  out = unlist(result$out)

  result$ndays = out[which(names(out) == "ndays")]
  result$minimum = out[which(names(out) == "minimum")]
  result$amp = out[which(names(out) == "amp")]
  result$alpha = out[which(names(out) == "alpha")]
  result$beta = out[which(names(out) == "beta")]
  result$acrotime = out[which(names(out) == "acrotime")]
  result$F_pseudo = out[which(names(out) == "F_pseudo")]
  result$UpMesor = out[which(names(out) == "UpMesor")]
  result$DownMesor = out[which(names(out) == "DownMesor")]
  result$MESOR = out[which(names(out) == "MESOR")]


  result$out = NULL
  names(result)[3:11] = paste0(names(result)[3:11],"_",window)
  return(result)

}
