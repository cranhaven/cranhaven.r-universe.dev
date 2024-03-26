## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)

library(APCalign)
library(readr)
library(dplyr)
library(here)

#' Format table with kable and default styling for html
#'
#' @param ... arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
util_kable_styling_html <- function(...) {
  txt <-
    kableExtra::kable(...) %>%
    kableExtra::kable_styling(...,
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "left"
    )

  # hack to add margin to plot
  gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', txt)
}

my_kable_styling <- util_kable_styling_html

match_taxa_documentation <-
  readr::read_csv(here("inst", "extdata", "match_taxa_documentation.csv"),
    show_col_types = FALSE
  )

update_taxonomy_documentation <-
  readr::read_csv(here("inst/", "extdata", "update_taxonomy_documentation.csv"),
    show_col_types = FALSE, skip = 1
  )


APCalign_outputs_documentation <-
  readr::read_csv(here("inst/", "extdata", "APCalign_outputs_documentation.csv"),
    show_col_types = FALSE
  )

## ----results='show'-----------------------------------------------------------
match_taxa_documentation %>%
  my_kable_styling()

## ----results='show'-----------------------------------------------------------
update_taxonomy_documentation %>%
  my_kable_styling()

## ----results='show'-----------------------------------------------------------
APCalign_outputs_documentation %>%
  my_kable_styling()

