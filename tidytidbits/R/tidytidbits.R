#' @importFrom grDevices cairo_pdf dev.off png
#' @importFrom methods substituteDirect
#' @importFrom stats complete.cases prop.test setNames chisq.test fisher.test
#' @importFrom utils write.table
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @rawNamespace import(rlang, except=c("prepend", "flatten_raw"))
#' @rawNamespace import(tibble, except="has_name")
#' @import dplyr
#' @rawNamespace import(purrr, except=c("flatten", "flatten_int", "flatten_chr", "flatten_dbl", "flatten_lgl", "flatten_raw", "as_function", "splice", "invoke", "%||%", "%@%", "list_along", "rep_along", "modify"))
#' @import tidyr
#' @import tidyselect
#' @import stringr
#' @import forcats
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".", ".prop_test", "where"))
