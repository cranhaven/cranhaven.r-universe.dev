#'
#' @title HandMovementDirection
#' @description Multivariate time series (MTS) indicating the movement of a joystick by two subjects with their
#' hand and wrist.
#' @usage data(HandMovementDirection)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 234 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 400 rows (time points) indicating MEG observations and 10 columns (variables) indicating MEG channel. The first 160 elements
#' correspond to the training set, whereas the last 74 elements correspond to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 4, indicating that there are 4 different classes in the database. Each class is associated with a different
#' direction (right, up, down and left). For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata1", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata1::HandMovementDirection".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"HandMovementDirection"

