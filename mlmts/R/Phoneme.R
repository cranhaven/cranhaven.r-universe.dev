#'
#' @title Phoneme
#' @description Multivariate time series (MTS) involving segmented audios of male and female speakers collected from Google Translate.
#' @usage data(Phoneme)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 6668 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 217 rows (time points) indicating readings in a spectrogram and 11 columns
#' (variables) indicating frequency band from the spectrogram. The first 3315 elements correspond to the training set, whereas the last 3353 elements
#' correspond to the test set. The numeric vector \code{classes} is formed by integers from 1 to 39, indicating that there are 39
#' different classes in the database. Each class is associated with a different phoneme.
#' For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata2", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata2::Phoneme".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"Phoneme"

