#'
#' @title EthanolConcentration
#' @description Multivariate time series (MTS) indicating the concentration of ethanol of several water-and-ethanol solutions
#' in 44 distinct, real-whisky bottles.
#' @usage data(EthanolConcentration)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 524 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 1751 rows (time points) indicating time measurements and 3 columns (variables) indicating recording. The first 261 elements
#' correspond to the training set, whereas the last 263 elements correspond to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 4, indicating that there are 4 different classes in the database. Each class is associated with a different
#' concentration of ethanol. For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata1", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata1::EthanolConcentration".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"EthanolConcentration"

