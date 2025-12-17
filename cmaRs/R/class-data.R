#' Classification Data - Breast Cancer Wisconsin (Diagnostic)
#'
#' Pre-processed and standardized Breast Cancer Wisconsin (Diagnostic)
#' is a real-life data set from University of California at Irvine (UCI)
#' machine learning data repository UCI: Machine Learning Repository
#' (available at http://archive.ics.uci.edu/ml/).
#'
#' @docType data
#'
#' @usage data(classdata.std)
#'
#' @format A data frame with 569 rows and 31 variables.
#'
#' @keywords datasets
#'
#' @references Dua, D. and Graff, C. (2017). UCI machine learning repository.
#'
#' @source http://archive.ics.uci.edu/ml/index.php
#'
#' @examples
#' \dontrun{
#' data(classdata.std)
#' cmaRs(y ~ ., classification = TRUE, data = classdata.std)
#' }
"classdata.std"
