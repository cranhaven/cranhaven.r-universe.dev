#'
#' @title CharacterTrajectories
#' @description Multivariate time series (MTS) of character samples, captured using a WACOM tablet. Data was recorded at 200Hz.
#' @usage data(CharacterTrajectories)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 80 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 182 rows (time points) indicating velocity trajectory and 3 columns (variables) indicating spatial dimension. The first 1422 elements
#' correspond to the training set, whereas the last 1436 elements correspond to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 20, indicating that there are 20 different classes in the database. Each class is associated with a different
#' alphabetical character. For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata1", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata1::CharacterTrajectories".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"CharacterTrajectories"

