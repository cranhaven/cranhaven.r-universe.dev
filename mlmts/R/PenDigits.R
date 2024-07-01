#'
#' @title PenDigits
#' @description Multivariate time series (MTS) indicating writing of 44 people drawing the digits from 0 to 9. Each instance is made up
#' of the x and y coordinates of the pen-tip traced accross a digital screen.
#' @usage data(PenDigits)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 10992 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 8 rows (time points) spatial points and 2 columns
#' (variables) indicating coordinate. The first 7494 elements correspond to the training set, whereas the last 3498 elements
#' correspond to the test set. The numeric vector \code{classes} is formed by integers from 1 to 10, indicating that there are 10
#' different classes in the database. Each class is associated with a different digit.
#' For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"PenDigits"

