#' Yield data of paddy
#'
#' @description  Dataset on paddy yield used by Chandra et al. (2016).
#'
#' @docType data
#'
#' @usage data(paddy)
#'
#' @format A data frame with 70 observations on the following 9 variables:
#' \describe{
#'   \item{D}{Small area code}
#'   \item{latitude}{Latitude of each small areas}
#'   \item{longitude}{Longitude of each small areas}
#'   \item{n}{Sample size of each small areas}
#'   \item{y}{Average yield data of paddy crop for the year 2009-10 (direct estimates for the small areas)}
#'   \item{var}{Estimated variance of y}
#'   \item{x1}{First covariate  (average household size) used by Chandra et al. (2016)}
#'   \item{x2}{Second covariate  (female population of marginal household) used by Chandra et al. (2016)}
#'   \item{indicator}{Index for sample and non-sample area}
#'   }

#'
#' @examples
#' data(paddy)
#' yield <- paddy$y
#' summary(yield)
#' @section Reference:
#' Chandra, H., salvati, N., chambers, R. and Sud, U. C. (2016). A Spatially Nonstationary Fay-Herriot Model for Small Area Estimation - An Application to Crop Yield Estimation. Seventh International Conference on Agricultural Statistics. Rome. DOI:10.1481/icasVII.2016.f35.
"paddy"
