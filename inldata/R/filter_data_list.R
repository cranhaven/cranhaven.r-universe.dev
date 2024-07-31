#' Filter Data List Column
#'
#' @description Create a data list column filter for a React Table.
#'   Requires that the \pkg{htmltools} packages is available.
#'
#' @param table_id 'character' string.
#'   Unique table identifier.
#' @param style 'character' string.
#'   CSS style applied to input HTML tag.
#'
#' @return Returns a function to perform filtering.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' f <- filter_data_list("table-id")

# Code adapted from R-package 'reactable' vignette at
# https://github.com/glin/reactable/blob/HEAD/vignettes/custom-filtering.Rmd
# Accessed on 2024-05-30
# License: MIT
# YEAR: 2019
# COPYRIGHT HOLDER: Greg Lin, Tanner Linsley

filter_data_list <- function(table_id, style = "width: 100%; height: 28px;") {

  # check system dependencies
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Creating a data list column filter requires the 'htmltools' package", call. = FALSE)
  }

  # check arguments
  checkmate::assert_string(table_id)
  checkmate::assert_string(style)

  # make function
  function(values, name) {
    list_id <- sprintf("%s-%s-list", table_id, name)
    htmltools::tagList(
      htmltools::tags$input(
        type = "text",
        list = list_id,
        oninput = sprintf(
          "Reactable.setFilter('%s', '%s', event.target.value || undefined)",
          table_id, name
        ),
        "aria-label" = sprintf("Filter %s", name),
        style = style
      ),
      htmltools::tags$datalist(
        id = list_id,
        lapply(sort(unique(values)),
          FUN = function(x) {
            htmltools::tags$option(value = x)
          }
        )
      )
    )
  }

}
