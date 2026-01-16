#'
#' @title FingerMovements
#' @description Multivariate time series (MTS) indicating the finger movements of a subject while typing at a computer
#' keyboard.
#' @usage data(FingerMovements)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 416 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 50 rows (time points) indicating EEG observations and 28 columns (variables) indicating EEG channel. The first 316 elements
#' correspond to the training set, whereas the last 100 elements correspond to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 2, indicating that there are 2 different classes in the database. Each class is associated with a different
#' side (left and right). For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata1", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata1::FingerMovements".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"FingerMovements"

