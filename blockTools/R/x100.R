#' @title Simulated data for demonstrating blockTools functionality
#' 
#' @description
#' Simulated data for demonstrating \code{blockTools} functionality.
#' 
#' @usage data(x100)
#' 
#' @format A dataframe with 100 rows and 6 columns.
#' \describe{
#'   \item{id}{Identifies units.}
#'   \item{id2}{Identifies subunits.}
#'   \item{b1}{Numeric blocking variable 1.}
#'   \item{b2}{Numeric blocking variable 2.}
#'   \item{g}{Character variable to identify which of three groups each unit is 
#'   in (\dQuote{a}, \dQuote{b}, or \dQuote{c}).}
#'   \item{ig}{Numeric variable that can be ignored.}
#' }
#' 
#' @examples
#' data(x100)
#' head(x100)
#' 
#' @author Ryan T. Moore
#' 
#' @keywords datasets
"x100"