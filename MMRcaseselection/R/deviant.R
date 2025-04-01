#' Identification of the most deviant case
#'
#' Identification of the most deviant case (= worst predicted case),
#' based on regression estimates.
#'
#' Proposed by Seawright, Jason and John Gerring (2008):
#' Case Selection Techniques in Case Study Research: A Menu of
#' Qualitative and Quantitative Options. \emph{Political Research Quarterly}
#' 61 (2): 294-308.
#' (\url{https://journals.sagepub.com/doi/pdf/10.1177/1065912907313077})
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most deviant case with the largest absolute
#' residual of all cases.
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_deviant(df)
#' @export
most_deviant <- function(lmobject) {
  if (class(lmobject) == "lm") {
    absresid <- sort(abs(residuals(lmobject)))
    return(absresid[length(absresid)])
  }
  else{
    stop("Input into function is not of class lm")
  }
}

#' Identification of the most overpredicted case
#'
#' The case with the largest negative difference between the observed
#' value and the predicted value on the outcome. Depending on the research
#' question, there might be a specific interest in the case for which the
#' model performs worst and yields a larger predicted value.
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most overpredicted case with the largest negative residual
#' (the most negative residual).
#' @importFrom stats lm residuals
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_overpredicted(df)
#' @export
most_overpredicted <- function(lmobject) {
  if (class(lmobject) == "lm") {
    resid <- sort(residuals(lmobject))
    return(resid[1])
  }
  else{
    stop("Input into function is not of class lm")
  }
}

#' Identification of the most underpredicted case
#'
#' The case with the largest positive difference between the observed
#' value and the predicted value on the outcome. Depending on the research
#' question, there might be a specific interest in the case for which the
#' model performs worst and yields a smaller predicted value.
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most underpredicted case with the largest positive residual
#' (the most positive residual).
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_underpredicted(df)
#'
#' @export
most_underpredicted <- function(lmobject) {
  if (class(lmobject) == "lm") {
    resid <- sort(residuals(lmobject))
    return(resid[length(resid)])
  }
  else{
    stop("Input into function is not of class lm")
  }
}
