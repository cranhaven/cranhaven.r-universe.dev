#' Rat tumor data
#'
#' This is the `Rat tumor data' data from Dinse and Lagakos (1984). 112 female and 207 male rats.
#' This data can be considered as a current status data where
#' the time (T) from birth to the onset of the tumor is the main variable of interest but
#' is never observed, but can only be guessed by the set of the death time (survtime) and the
#' tumor indicator (tumor). Quotes are from Dinse and Lagakos (1984).
#'
#' @docType data
#' @format A data frame with 319 rows and 6 variables:
#' \describe{
#'  \item{dose.lvl}{'dose level of PBB (coded 0-5).'}
#'  \item{weight}{'initial weight in grams.'}
#'  \item{cage.no}{'number of the cage tier.'}
#'  \item{survtime}{'survival time (age) in weeks.'}
#'  \item{tumor}{'response indicator (1 = hyperplasia present, 0 = hyperplasia absent).'}
#'  \item{male}{The gender (0 = female, 1 = male).}
#' }
#' @usage data(rat2)
#' @name rat2
#'
#' @references
#' Dinse, G. E., & Lagakos, S. W. (1983). Regression analysis of tumour prevalence data.
#' Journal of the Royal Statistical Society: Series C (Applied Statistics), 32(3), 236-248.
#'
#' Dinse, G. E., & Lagakos, S. W. (1984). Correction: Regression analysis of tumour prevalence data.
#' Journal of the Royal Statistical Society: Series C (Applied Statistics), 33(1), 79-80.
#'
NULL
