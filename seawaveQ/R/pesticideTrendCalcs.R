#' Internal function to summarize the trend results.
#' 
#' pesticideTrendCalcs is called from within \link{fitswavecav}
#' @name pesticideTrendCalcs
#' @title Summarize linear trends
#' @note Based on trend calculations used to display and summarize pesticide
#' trends here \url{https://nawqatrends.wim.usgs.gov/swtrends/}.
#' A likelihood value that is the functional equivalent of the two-sided 
#' p-value associated with the significance level of the trend was 
#' determined as follows:
#' Likelihood = (1 - (p-value / 2)), where p-value is the 
#' p-value for the trend coefficient (Oelsner and others, 2017).
#' @param tndbeg is the beginning (in whole or decimal years) of the 
#' trend period. Zero means the begin date will be the beginning of the
#' concentration data, cdat.
#' @param tndend is the end of the trend (treated as December 31
#' of that year). Zero means the end date will be the end of the 
#' concentration data, cdat.
#' @param ctnd is the concentration trend, the coefficient on the time variable.
#' @param pval is the p-value for the linear trend component.
#' @param alpha is the significance level or alpha value for statistical
#' significance and confidence intervals.
#' @param setnd is the standard error for the linear trend component.
#' @param scl is the scale factor from the \code{survreg.object}.
#' @param baseConc is the base concentration, the median concentration 
#' (midpoint of the trend line) for the first year of the trend analysis .
#' @param mclass indicates the class of model to use.
#' A class 1 model is the the traditional SEAWAVE-Q model that has a
#' linear time trend. A class 2 model is a newer option for longer
#' trend periods that uses a set of restricted cubic splines on the 
#' time variable to provide a more flexible model. 
#' @return The data frame returned has one row for each chemical analyzed and
#' summaries of the trend.
#' @format The data frame returned has one row for each chemical analyzed 
#' and the number of columns are defined as follows: \cr
#' \tabular{lll}{
#'  pname \tab character \tab parameter analyzed \cr
#'  mclass \tab numeric \tab a value of 1 or 2\cr
#'  alpha \tab numeric \tab a significance level \cr
#'  ctndPpor \tab numeric \tab the concentration trend in percent over the period of record \cr
#'  cuciPpor \tab numeric \tab the concentration upper confidence interval for the trend in\cr
#'   \tab \tab percent over the period of record \cr
#'  clciPpor \tab numeric \tab the concentration lower confidence interval for the trend in\cr
#'   \tab \tab percent over the period of record \cr
#'  baseConc \tab numeric \tab the base concentration, median concentration or midpoint of\cr
#'   \tab \tab trend line, for first year of trend period \cr
#'  ctndOrigPORPercentBase \tab numeric \tab the concentration trend in original units over\cr
#'   \tab \tab the period of record\cr
#'   \tab \tab (calculation based on percent per year and base concentration)\cr
#'  cuciOrigPORPercentBase \tab numeric \tab the concentration trend upper confidence interval\cr
#'   \tab \tab for the trend in original units over the period of record\cr
#'   \tab \tab (calculation based on percent per year and base concentration)\cr
#'  clciOrigPORPercentBase \tab numeric \tab the concentration trend lower confidence interval\cr
#'   \tab \tab for the trend in original units over the period of record\cr
#'   \tab \tab (calculation based on percent per year and base concentration)\cr
#'  ctndlklhd \tab numeric \tab is the concentration trend likelihood \cr
#' }
#' @export
#' @author Karen R. Ryberg
#' @references
#' Oelsner, G.P., Sprague, L.A., Murphy, J.C., Zuellig, R.E., Johnson, H.M., 
#' Ryberg, K.R., Falcone, J.A., Stets, E.G., Vecchia, A.V., Riskin, M.L., 
#' De Cicco, L.A., Mills, T.J., and Farmer, W.H., 2017, Water-quality trends in 
#' the Nation's rivers and streams, 1972--2012---Data preparation, statistical 
#' methods, and trend results (ver. 2.0, October 2017): U.S. Geological Survey 
#' Scientific Investigations Report 2017--5006, 136 p., 
#' \url{https://doi.org/10.3133/sir20175006}.
pesticideTrendCalcs <- function(tndbeg, tndend, ctnd, pval, alpha, setnd, scl, 
                                baseConc, mclass) {
  if (mclass == 1) {
    por <- tndend - tndbeg
    ctndPYR <- 100 * (10 ^ ctnd - 1)
    ctndPpor <- formatC(100 * ((10 ^ ctnd) ^ por) - 100 , digits = 4, 
                        flag = "#", format = "f")
    cuciPpor <- formatC(100 * ((10 ^ (ctnd + qnorm(1 - alpha/2) * setnd) ) ^ por) - 
                          100, digits = 4, flag = "#", format = "f")
    clciPpor <- formatC(100 * ((10 ^ (ctnd - qnorm(1 - alpha/2) * setnd) ) ^ por) - 
                          100, digits = 4, flag = "#", format = "f")

    ctndOrigPORPercentBase <- formatC(baseConc * ((10 ^ ctnd) ^ por) - baseConc, 
                                      digits = 4, flag = "#", format = "f")
    cuciOrigPORPercentBase <- formatC(baseConc * (10 ^ (ctnd + qnorm(1 - alpha/2) * 
                                                          setnd)) ^ por - baseConc, 
                                      digits = 4, flag = "#", format = "f")
    clciOrigPORPercentBase <- formatC(baseConc * 
                                        (10 ^ (ctnd - qnorm(1 - alpha/2) * setnd)) ^ por - 
                                        baseConc, digits = 4, flag = "#", 
                                      format = "f")
  ctndlklhd <- round(1 - pval/2, digits = 4)
  cat("\n")
  trends <- c(alpha, round(baseConc, digits = 4), ctndPpor, cuciPpor, clciPpor,  
              ctndOrigPORPercentBase, cuciOrigPORPercentBase, clciOrigPORPercentBase,
              ctndlklhd)
  } else if (mclass == 2) {
    # nothing
  } else {
    mclassmes <- c("Invalid model class. Currently, 
                   the only valid mclass options are numeric values of 1 or 2.")
    stop(mclassmes)
  }
  return(as.numeric(trends))
}
