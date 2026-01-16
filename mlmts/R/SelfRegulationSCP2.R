#'
#' @title SelfRegulationSCP2
#' @description Multivariate time series (MTS) taken from an Amyotrophyc Lateral Sclerosis (ALS) subject asked to move a cursor up and down
#' on a computer screen while his cortical potentials were taken.
#' @usage data(SelfRegulationSCP1)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 380 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 1152 rows (time points) indicating time recordings over an interval of 4.5 seconds
#'  and 7 columns (variables) indicating EEG channel. The first 200 elements correspond to the training set, whereas the last 180 elements
#' correspond to the test set. The numeric vector \code{classes} is formed by integers from 1 to 2, indicating that there are 2
#' different classes in the database. Each class is associated with the label 'negativity' (downward movement of the cursor) or 'positivity'
#' (upward movement of the cursor). For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata2", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata2::SelfRegulationSCP2".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"SelfRegulationSCP2"

