# From https://github.com/tidyverse/tidyverse/blob/main/R/tidyverse.R
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
ignore_unused_imports <- function() {
  bupaR::eventlog
  edeaR::augment
  eventdataR::patients
  processcheckR::check_rule
  processmapR::process_map
}

## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end

globalVariables(c("."))
NULL
