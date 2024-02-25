#' Fire and extreme temperature in portugal
#'
#' Bivariate dataset of daily mean temperature and number of wildfires in the Porto
#' district (Portugal) for the period 1980 to 2005. Daily mean temperature data
#' from E-OBS (Cornes et al. (2018) <10.1029/2017JD028200>
#' and wildfire data from Pereira et al. (2011) <10.5194/nhess-11-3343-2011>.

#'
#' @docType data
#'
#' @usage data(porto)
#'
#' @format Two column dataframe:
#' #' \itemize{
#' \item temp2.temperature (mean daily temperature in °C)
#' \item temp2.nb (daily number of wildfires)
#'}
#' @keywords datasets
#'
#' @references
#' Tilloy, A., Malamud, B.D., Winter, H. and Joly-Laugel, A., 2020.
#' Evaluating the efficacy of bivariate extreme modelling approaches
#' for multi-hazard scenarios. Natural Hazards and Earth System Sciences, 20(8), pp.2091-2117.
"fire01meantemp"

#'
#' @examples
#' data(porto)
