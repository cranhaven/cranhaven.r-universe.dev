# From https://github.com/tidyverse/tidyverse/blob/main/R/utils.R

msg <- function(x, startup = FALSE) {

  if (startup) {
    if (!isTRUE(getOption("bupaverse.quiet"))) {
      rlang::inform(x, class = "packageStartupMessage")
    }
  } else {
    rlang::inform(x)
  }
}

invert <- function(x) {

  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}