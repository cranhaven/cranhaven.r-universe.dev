#' Head count data
#'
#' @description  Dataset on head count used by Chandra et al. (2017).
#'
#' @docType data
#'
#' @usage data(headcount)
#'
#' @format A data frame with 71 observations on the following 11 variables:
#' \describe{
#'   \item{Area}{Small area code}
#'   \item{lat}{Latitude of each small areas}
#'   \item{long}{Longitude of each small areas}
#'   \item{N}{Sample size of each small areas}
#'   \item{n}{Sample size of each small areas}
#'   \item{y}{Head count (direct estimates for the small areas)}
#'   \item{ps}{proportion of head count}
#'   \item{var}{Estimated variance}
#'   \item{x1}{First covariate used by Chandra et al. (2017)}
#'   \item{x2}{Second covariate used by Chandra et al. (2017)}
#'   \item{x3}{Second covariate  used by Chandra et al. (2017)}
#'   }

#'
#' @examples
#' data(headcount)
#' y <- headcount$y
#' summary(y)
#' @section Reference:
#' Chandra, H., Salvati, N., & Chambers, R. (2017). Small area prediction of counts under a non-stationary spatial model. Spatial Statistics. 20. 30-56. DOI:10.1016/j.spasta.2017.01.004.
"headcount"
