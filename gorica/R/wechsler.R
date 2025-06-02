#' Wechsler intelligence test data
#'
#' Dataset based on McArdle and Prescott (1992, p.90). This study evaluates
#' intelligence and cognitive ability in a sample of individuals over 18 years
#' of age (N = 1680) using the IQ test Wechsler Adult Intelligence Scale-Revised
#' (WAIS-R) (Wechsler, 1981).
#'
#' \tabular{lll}{
#'   \strong{age} \tab \code{integer} \tab Participants' age (recoded)\cr
#'   \strong{edc} \tab \code{factor} \tab Whether a participant graduated high
#'   school or not (1 = not graduated, 2 = graduated)\cr
#'   \strong{y1} \tab \code{integer} \tab information; general knowledge of participants \cr
#'   \strong{y2} \tab \code{integer} \tab comprehension; ability of abstract reasoning or judgment \cr
#'   \strong{y3} \tab \code{integer} \tab similarities; unifying a theme \cr
#'   \strong{y4} \tab \code{integer} \tab vocabulary; verbal definition \cr
#'   \strong{y5} \tab \code{integer} \tab picture completion; perceiving visual images with missing features \cr
#'   \strong{y6} \tab \code{integer} \tab block design; arranging blocks to match a design \cr
#'   \strong{y7} \tab \code{integer} \tab picture arrangement; ordering cards with true story lines \cr
#'   \strong{y8} \tab \code{integer} \tab object assembly; reassembling puzzles
#' }
#' @docType data
#' @keywords datasets
#' @name wechsler
#' @usage data(wechsler)
#' @references McArdle, J. J., & Prescott, C. A. (1992). Age-based construct
#' validation using structural equation modeling. Experimental Aging Research,
#' 18, 87-115.
#' @format A data frame with 1680 rows and 10 variables.
NULL
