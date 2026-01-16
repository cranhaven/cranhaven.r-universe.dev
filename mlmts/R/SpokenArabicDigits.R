#'
#' @title SpokenArabicDigits
#' @description Multivariate time series (MTS) involving sound of 44 males and 44 females Arabic native speakers between the ages of 18 and 40.
#' The 13 Mel Frequency Cepstral Coefficients (MFCCs) were computed.
#' @usage data(SpokenArabicDigits)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 8798 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 93 rows (time points) indicating time recordings and 13 columns
#' (variables) indicating different MFCCs. The first 6599 elements correspond to the training set, whereas the last 2199 elements
#' correspond to the test set. The numeric vector \code{classes} is formed by integers from 1 to 10, indicating that there are 10
#' different classes in the database. Each class is associated with a different spoken arabic digit.
#' For more information, see \insertCite{bagnall2018uea;textual}{mlmts}.
#' Run "install.packages("ueadata2", repos="https://anloor7.github.io/drat")"
#' to access this dataset and use the syntax "ueadata2::SpokenArabicDigits".
#' @references{
#'
#'   \insertRef{bagnall2018uea}{mlmts}
#'
#'   \insertRef{ruiz2021great}{mlmts}
#'
#'   \insertRef{bagnallweb}{mlmts}
#'
#' }
"SpokenArabicDigits"

