#'
#' @title Epilepsy
#' @description Multivariate time series (MTS) of some participants simulating several activities. In particular, data was
#' collected from 6 participants using a tri-axial accelerometer on the dominant wrist while conducting 4 different activities
#' @usage data(Epilepsy)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 275 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 206 rows (time points) indicating acceleration trajectory and 3 columns (variables) indicating the axis in the accelerometer. The first 137 elements
#' correspond to the training set, whereas the last 138 elements correspond to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 4, indicating that there are 4 different classes in the database. Each class is associated with a different
#' activity. For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"Epilepsy"

