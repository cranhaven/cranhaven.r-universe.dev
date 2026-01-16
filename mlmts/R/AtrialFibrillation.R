#'
#' @title AtrialFibrillation
#' @description Multivariate time series (MTS) of two-channel ECG recordings of atrial fibrillation. The database has been created
#' from data used in the Computers in Cardiology Challenge 2004.
#' @usage data(AtrialFibrillation)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 30 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 640 rows (time points) indicating ECG measures and 2 columns (variables) indicating ECG leads. The first 15 elements
#' correspond to the training set, whereas the last 15 elements correspond to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 3, indicating that there are 3 different classes in the database. Each class is associated with a different
#' type of atrial fibrillation. For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"AtrialFibrillation"


