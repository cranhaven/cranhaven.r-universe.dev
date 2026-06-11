#' @title Intradaily Variability
#' @description This function calcualte intradaily variability, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} of activity data
#' @return IV
#'
#'
#' @export
#' @references Junrui Di et al. Joint and individual representation of domains of physical activity, sleep, and circadian rhythmicity. Statistics in Biosciences.
#'
#'
#'

IV2 = function(
  x
){
  mean.counts = mean(x)
  numerator = sum(diff(x)^2)/(length(x) - 1)
  denominator = sum((x - mean.counts)^2)/length(x)
  return(numerator/denominator)
}


#' @title Intradaily Variability for the Whole Dataset
#' @description This function calcualte intradaily variability, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity. This function is a whole dataset
#' wrapper for \code{IV}.
#'
#' @param count.data \code{data.frame} of dimension n * (1440+2) containing the
#' 1440 dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequency of days within each subject.
#' @param  window an \code{integer} indicating what is the window to bin the data before
#' the function can be apply to the dataset. For details, see \code{bin_data}.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#'
#'
#'
#' @return A \code{data.frame} with the following 5 columns
#' \item{ID}{ID}
#' \item{Day}{Day}
#' \item{IV}{IV}
#'
#'
#' @export
#' @references Junrui Di et al. Joint and individual representation of domains of physical activity, sleep, and circadian rhythmicity. Statistics in Biosciences.
#'
#'
#'



IV_long2 = function(
  count.data,
  window = 1,
  method = c("average","sum")
  ){
  x = count.data
  x_bin = cbind(x[,1:2],
                as.data.frame(
                  t(apply(x[,-c(1:2)], 1,
                          FUN = bin_data2, window = window, method = method))))
  iv_out = as.data.frame(cbind(x_bin[,c(1,2)], apply(x_bin[,-c(1,2)], 1, IV2)))
  names(iv_out) = c("ID","Day",paste0("IV_",window))
  return(iv_out)
}
