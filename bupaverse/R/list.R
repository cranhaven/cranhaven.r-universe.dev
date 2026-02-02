# From https://github.com/tidyverse/tidyverse/blob/main/R/utils.R
#' @title List all packages in the bupaverse
#'
#' @param include_self [`logical`] (default `TRUE`): Include [`bupaverse`] in the list?
#'
#' @returns
#' Returns a [`list`] of all packages included in the [`bupaverse`].
#'
#' @examples
#' bupaverse_packages()
#'
#' @export
bupaverse_packages <- function(include_self = TRUE) {

  raw <- utils::packageDescription("bupaverse")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "bupaverse")
  }

  names
}