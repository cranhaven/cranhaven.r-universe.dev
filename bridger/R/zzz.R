#' @title zzz
#'
#' @description Runs on loading bridger
#'
#' @return No return value, called to set global variables and specify import packages
#'
#' @param libname Legacy dummy
#' @param pkgname Legacy dummy
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @import patchwork
#' @importFrom grDevices cairo_pdf
#' @importFrom magrittr %>%

.onLoad <- function(libname, pkgname) {
  # Set global variables for non-standard evaluation
  utils::globalVariables(c(
    ".", "C", "D", "H", "S", "HC", "Probability", "Shape", "Total", "card", "hand1", "hand2", "hand3", "hand4",
    "na.omit", "name", "printHand_1", "printHand_2", "printHand_3", "printHand_4", "printHand_5", "printHand_6",
    "rowid", "runif", "shape", "suit", "value", "str_split", "str_sub", "str_sub"
  ))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n        --- bridger --- \n For all your bridge hand needs\n")
}
