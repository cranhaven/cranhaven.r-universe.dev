

#' Identification of the most typical case
#'
#' The most typical case (= best predicted case) based on regression estimates.
#'
#' Proposed by Seawright, Jason and John Gerring (2008):
#' Case Selection Techniques in Case Study Research: A Menu of
#' Qualitative and Quantitative Options. \emph{Political Research Quarterly}
#' 61 (2): 294-308.
#' (\url{https://journals.sagepub.com/doi/pdf/10.1177/1065912907313077})
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most typical case having the smallest absolute
#' residual of all cases.
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_typical(df)
#'
#' @export
most_typical <- function(lmobject) {
  if (class(lmobject) == "lm") {
  absresid <- sort(abs(residuals(lmobject)))
  return(absresid[1])
  }
  else{
    stop("Input into function is not of class lm")
  }
}
