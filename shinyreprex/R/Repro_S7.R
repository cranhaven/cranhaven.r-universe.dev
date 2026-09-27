#' Reproducible Code
#'
#' @description
#' An S7 object that holds the code and packages required to re-create a given reactive.
#'
#' @param code Code chunks found in a given expression
#' @param packages Packages found in the function calls in the code and/or pre-requisites
#' @param prerequisites Code chunks used to generate reactive objects found in the code
#'
#' @importFrom purrr map
#' @importFrom styler style_text
#'
#' @keywords internal
#' @rdname repro_s7
Repro <- S7::new_class(
  name = "Repro",
  properties = list(
    # Code chunks found in a given reactive expression
    code = S7::new_property(
      S7::class_list,
      default = list(),
      setter = function(self, value) {
        # Able to initially set code slot as empty list
        if (length(value) > 0L || length(self@code) == 0L) {
          self@code <- c(self@code, value)
        }
        self
      }
    ),

    packages = S7::new_property(
      S7::class_character,
      default = character(),
      setter = function(self, value) {
        # Able to initially set packages slot as empty character vector
        if (length(value) > 0L || length(self@packages) == 0L) {
          self@packages <- unique(c(self@packages, value))
        }
        self
      }
    ),

    prerequisites = S7::new_property(
      S7::class_list,
      default = list(),
      setter = function(self, value) {
        # Able to initially set prerequisites slot as empty list
        if (is.null(self@prerequisites) && is.list(value)) {
          self@prerequisites <- value
        } else if (is_new_reactive(value, c(self@code, self@prerequisites))) {
          self@prerequisites <- c(self@prerequisites, value[setdiff(names(value), names(self@prerequisites))])
        }
        self
      }
    ),

    calls = S7::new_property(
      getter = function(self) {
        if (length(self@packages) > 0L) {
          pkg_calls <- c(paste0("library(", self@packages, ")"), "")
        } else {
          pkg_calls <- NULL
        }

        prereq_calls <- self@prerequisites |>
          unlist(recursive = FALSE, use.names = FALSE) |>
          purrr::map(constructive::deparse_call) |>
          purrr::map(c, "") |>
          unlist(use.names = FALSE)

        code_calls <- self@code |>
          purrr::map(constructive::deparse_call) |>
          unlist(use.names = FALSE)

        c(pkg_calls, prereq_calls, code_calls)
      }
    ),

    script = S7::new_property(
      getter = function(self) {
        if (sum(nchar(self@calls)) > 0L) {
          self@calls |>
            styler::style_text() |>
            paste(collapse = "\n")
        } else {
          ""
        }
      }
    )
  )
)

#' Reactive Variables Definition Check
#'
#' @description
#' A helper function to check whether or not the reactive variables to be added
#' to the `Repro` object already exists. Used to avoid duplicate definitions being
#' added to a script.
#'
#' @param new,exisitng A named list of reactive variable definitions
#'
#' @returns
#' A boolean stating whether or not there is at least one reactive definition
#' in `new` that doesn't exist in `existing`
#'
#' @keywords internal
is_new_reactive <- function(new, exisitng) {
  length(new) > 0L &&
    rlang::is_named(new) &&
    !all(names(new) %in% names(exisitng))
}
