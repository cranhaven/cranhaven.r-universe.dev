#'
#' @title UWaveGestureLibrary
#' @description Multivariate time series (MTS) including gestures from certain subjects measured with an accelerometer.
#' @usage data(UWaveGestureLibrary)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 440 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 315 rows (time points) indicating time recordings and 3 columns
#' (variables) indicating coordinate (x, y or z) of each motion. The first 120 elements correspond to the training set, whereas the last 320 elements
#' correspond to the test set. The numeric vector \code{classes} is formed by integers from 1 to 8, indicating that there are 8
#' different classes in the database. Each class is associated with a different gesture.
#' For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata2", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata2::UWaveGestureLibrary".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"UWaveGestureLibrary"
