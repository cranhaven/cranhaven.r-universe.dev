#'
#' @title EigenWorms_2
#' @description Multivariate time series (MTS) indicating the movement of the worm Caenorhabditis elegans. The motion of worms
#' in an agar plate is recorded as a combination of six base shapes.
#' @usage data(EigenWorms_2)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 129 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 17984 rows (time points) indicating velocity trajectory and 3 columns (variables) indicating spatial dimension. The first 1422 elements
#' correspond to the training set, whereas the last 1436 elements correspond to the test set.
#' The last 129 elements
#' of the whole dataset are stored here. All these elements pertain to the test set. The numeric vector \code{classes} is formed
#' by integers from 1 to 20, indicating that there are 20 different classes in the database. Each class is associated with a different
#' alphabetical character. For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' To access this dataset, run "install.packages("ueadata6", repos="https://anloor7.github.io/drat")"
#' and use the syntax "ueadata6::EigenWorms_2".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"EigenWorms_2"

