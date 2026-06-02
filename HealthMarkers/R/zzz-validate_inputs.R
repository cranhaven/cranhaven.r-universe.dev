#' Validate required inputs for a calling function
#'
#' Ensures required keys exist in `col_map` and have non-empty mappings.
#' Missing keys are reported in a stable order aligned with tests.
#'
#' @param data data.frame or tibble
#' @param col_map named list mapping keys to column names
#' @param fun_name character scalar naming the calling function (e.g., "lipid_markers").
#'   Used to look up built-in required keys when `required_keys` is not supplied.
#' @param required_keys optional character vector of required col_map keys. When
#'   supplied, this takes precedence over the `fun_name` built-in lookup, making
#'   the function useful for any caller regardless of `fun_name`.
#' @return invisibly TRUE on success; otherwise aborts
#' @examples
#' df <- data.frame(TG = c(1.5, 2.0), HDL_c = c(1.2, 1.0),
#'   LDL_c = c(2.0, 2.5), TC = c(4.5, 5.0))
#' # Using built-in lookup
#' validate_inputs(df,
#'   list(TG = "TG", HDL_c = "HDL_c", LDL_c = "LDL_c", TC = "TC"),
#'   fun_name = "lipid_markers")
#' # Using explicit required_keys (works for any function)
#' validate_inputs(df,
#'   list(TG = "TG", HDL_c = "HDL_c"),
#'   fun_name = "my_function",
#'   required_keys = c("TG", "HDL_c"))
#' @export
validate_inputs <- function(data, col_map, fun_name, required_keys = NULL) {
  if (!is.data.frame(data)) {
    rlang::abort("validate_inputs(): `data` must be a data.frame or tibble.")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("validate_inputs(): `col_map` must be a named list.")
  }
  if (!is.character(fun_name) || length(fun_name) != 1L || !nzchar(fun_name)) {
    rlang::abort("validate_inputs(): `fun_name` must be a single non-empty string.")
  }

  # When required_keys is explicitly supplied, use it directly.
  # Otherwise fall back to the built-in per-function lookup.
  if (!is.null(required_keys)) {
    if (!is.character(required_keys)) {
      rlang::abort("validate_inputs(): `required_keys` must be a character vector.")
    }
    required <- required_keys
  } else {
    required <- switch(
      fun_name,
      lipid_markers = c("TG", "HDL_c", "LDL_c", "TC"),
      character(0)
    )
  }

  is_empty_map <- function(v) {
    is.null(v) || length(v) == 0L ||
      (is.atomic(v) && length(v) == 1L &&
         (is.na(v) || identical(v, "") || !nzchar(as.character(v))))
  }

  missing_keys <- vapply(required, function(k) {
    if (!(k %in% names(col_map))) return(k)
    if (is_empty_map(col_map[[k]])) return(k)
    NA_character_
  }, character(1))
  missing_keys <- missing_keys[!is.na(missing_keys)]

  if (length(missing_keys)) {
    rlang::abort(
      paste0("you must supply col_map entries for: ", paste(missing_keys, collapse = ", "))
    )
  }

  invisible(TRUE)
}