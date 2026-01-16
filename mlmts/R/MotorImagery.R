#'
#' @title MotorImagery
#' @description Multivariate time series (MTS) involving imagined movements performed by a subject with either the left small finger or the
#' tongue. The time series of the electrical brain activity were stored during the corresponding trials
#' @usage data(MotorImagery)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 378 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 3000 rows (time points) indicating time recordings in EEG and 64 columns
#' (variables) indicating EEG electrodes. The first 278 elements correspond to the training set, whereas the last 100 elements
#' correspond to the test set. The numeric vector \code{classes} is formed by integers from 1 to 2, indicating that there are 2
#' different classes in the database. Each class is associated with the label 'finger' or 'tongue' (the imagined movements).
#' For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' To access this dataset, execute the code "install.packages("ueadata2", repos="https://anloor7.github.io/drat")"
#' and use the following syntax: "ueadata2::MotorImagery".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"MotorImagery"

