
#' Extremeness of cases on an independent variable
#'
#' Extremeness of a case is calculated by the difference between a case's
#' value on the independent variable and the variable's mean value.
#'
#' Calculating the absolute value of the difference between the cases' values
#' and the variable's mean value is proposed by Seawright, Jason (2016): The
#' Case for Selecting Cases That Are Deviant or Extreme on the Independent
#' Variable. \emph{Sociological Methods & Research} 45 (3): 493-525.
#' (\url{https://doi.org/10.1177/0049124116643556})
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param ind_var Independent variable for which extremeness values should
#' be calculated. Has to be entered as a character.
#'
#' @return A dataframe with
#'
#' - all variables in the linear model,
#'
#' - absolute extremeness (absolute value of difference between variable
#' score and mean value of variable),
#'
#' - extremeness (difference between variable score and mean value of
#' variable), which can be useful when the direction of extremeness is relevant.
#'
#' The rows are ordered in decreasing order of the absolute extreme values.
#'
#' @importFrom stats lm
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' extreme_on_x(df, "wt")
#'
#' @export
extreme_on_x <- function(lmobject = NULL, ind_var = NULL) {
  if (is.null(ind_var)) {
    stop("Please specify the independent variable")
  }
  if (class(lmobject) != "lm") {
    stop("lmobject input into function is not of class lm")
  }
  if (isFALSE(ind_var %in% names(lmobject$model[, 2:ncol(lmobject$model)]))) {
    stop("Chosen ind_var is not among the independent variables")
  }
  else{
    tempx <- lmobject$model[, ind_var]
    tempdf <- lmobject$model
    tempdf$`abs. extremeness` <- abs(tempx - mean(tempx))
    tempdf$extremeness <- tempx - mean(tempx)
    tempdf <- tempdf[order(tempdf$`abs. extremeness`, decreasing = T), ]
    return(tempdf)
  }
}

#' Extremeness of cases on the dependent variable
#'
#' Extremeness of a case is calculated by the difference between a case's
#' value on the dependent variable and the variable's mean value.
#'
#' Calculating the absolute value of the difference between the cases' values
#' and the variable's mean value is proposed by Seawright, Jason (2016): The
#' Case for Selecting Cases That Are Deviant or Extreme on the Independent
#' Variable. \emph{Sociological Methods & Research} 45 (3): 493-525.
#' (\url{https://doi.org/10.1177/0049124116643556})
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return A dataframe with
#'
#' - all variables in the linear model,
#'
#' - absolute extremeness (absolute value of difference between variable
#' score and mean value of variable),
#'
#' - extremeness (difference between variable score and mean value of
#' variable), which can be useful when the direction of extremeness is relevant.
#'
#' The rows are ordered in decreasing order of the absolute extreme values.
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' extreme_on_y(df)
#'
#' @importFrom stats lm
#'
#' @export
extreme_on_y <- function(lmobject) {
  if (class(lmobject) == "lm") {
    tempy <- lmobject$model[, 1]
    tempdf <- lmobject$model
    tempdf$`abs. extremeness` <- abs(tempy - mean(tempy))
    tempdf$extremeness <- tempy - mean(tempy)
    tempdf <- tempdf[order(tempdf$`abs. extremeness`, decreasing = T), ]
    return(tempdf)
  }
  else{
    stop("lmobject input into function is not of class lm")
  }
}
