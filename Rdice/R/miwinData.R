
#' Miwin set of dice.
#'
#' A dataset containing the Miwin set of dice. This set is non-transitively losing, with losing probabilities of 17/36.
#'
#' @docType data
#'
#' @usage data(miwin)
#'
#' @format A data table with 3 columns. Each column represents a die with six faces.
#'
#' @keywords datasets
#' @examples
#' data(miwin)
#' is.nonTransitive(miwin, prob = 17/36)
"miwin"
