#' Prediction Data - Concrete Slump Test
#'
#' Pre-processed and standardized Concrete Slump Test is a
#' real-life data set from
#' University of California at Irvine (UCI) machine learning data
#' repository UCI: Machine Learning Repository
#' (available at http://archive.ics.uci.edu/ml/).
#' @docType data
#'
#' @usage data(preddata.std)
#'
#' @format A data frame with 103 rows and 8 variables.
#'
#' @keywords datasets
#'
#' @references Yeh, I.-C. (2007).
#' Modeling slump flow of concrete using second-order regressions
#' and artificial neural networks.
#' Cement and Concrete Composites, 29(6): 474 - 480.
#'
#' @source https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength
#'
#' @examples
#' \dontrun{
#' data(preddata.std)
#' cmaRs(Compressive_Strength ~ ., classification = FALSE, data = preddata.std)
#' }
"preddata.std"
