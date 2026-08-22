#' @importFrom rlang .data
#' @importFrom rlang !! !!! %||%
#' @importFrom data.table .I .N .SD
utils::globalVariables(c(
  ".SDcols",
  ".",
  "..",
  "impute_req",
  "mab_condition",
  "mab_success",
  "mab_assign_prob",
  "period_number",
  "successes",
  "..condition",
  "count",
  "known_success",
  "weight",
  "new_success_date",
  "trial",
  "..month_col",
  "treatment_block",
  "var",
  "estimator",
  "assignment_date",
  "block",
  "n_success",
  "cumulative_count",
  "cumulative_success",
  "failure_rate",
  "success_rate",
  "impute_block",
  "time_model_args",
  "ipw_weights",
  "mhat",
  "month_date",
  "n",
  ":=",
  "cluster_means",
  "n_periods",
  "df",
  "se",
  "Coef",
  "arm1",
  "arm2",
  "Est",
  "SE"
))

#' Miscellaneous Helper Functions
#' @name misc_helpers
NULL

#-------------------------------------------------------------------------------
#' Column as a Named Vector
#' @rdname misc_helpers
#' @description
#' Converts `data.frame` column into a vector using another column as the names for the vector
#' @param df `data.frame` used.
#' @param val Column name of values
#' @param name Column name of names
#' @returns vector with values `val` and names `name`
#' @keywords internal
as_named_vec <- function(df, val, name) {
  x <- df[[val]]
  names(x) <- df[[name]]
  x
}

#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing if TRUE
#' @rdname misc_helpers
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message, this will always be
#' the `verbose` argument passed from higher functions.
#' @returns Text output of `message` to the console when `log = TRUE`. If
#' `log = FALSE`, returns nothing.
#' @keywords internal

verbose_log <- function(log, message) {
  if (log) {
    cat(message, "\n")
  }
}
#------------------------------------------
#------------------------------------------------------------------------------
#' Formula Parser
#' @description Parses the input formula for [mab_from_rct()]
#' @param formula `formula` passed from [mab_from_rct()]
#' @returns List of columns specified from formula.
#' @keywords internal

formula_parse <- function(formula) {
  formula <- as.character(formula)

  outcome <- formula[2]

  obc <- strsplit(formula[3], "\\+") |>
    lapply(trimws) |>
    unlist()

  condition_col <- obc[1]
  other_vars <- lapply(
    list(
      obc[grepl("block\\((.*?)\\)", obc)],
      obc[grepl("cluster\\((.*?)\\)", obc)]
    ),
    gather_args
  )

  parsed <- list(
    condition_col = condition_col,
    success_col = outcome,
    block_cols = .block(other_vars[[1]][["args"]]),
    cluster_col = .cluster(other_vars[[2]][["args"]])
  )

  return(parsed)
}
#' Gather Args
#' @param x String representing an `R` expression, like `"block(x1)"`.
#' @returns A list containing the function call, and the arguments so `"block(x1)"` gets turned into a list
#' with elements `block, "x1"`.
#' @describeIn formula_parse Helper for formula parsing. Parses the expression, and splits the function call from the arguments.
#' @keywords internal

gather_args <- function(x) {
  if (length(x) == 0) {
    return(list(NULL))
  }
  call <- rlang::parse_expr(x) |>
    as.list()

  args <- vapply(
    call[-1],
    rlang::as_label,
    character(1)
  )
  return(list(call = call[[1]], args = args))
}

#' @describeIn formula_parse Block
#' @param ... Blocking variables from `formula`
#' @returns Vector of blocking variables
#' @keywords internal
.block <- function(...) {
  c(...)
}
#' @describeIn formula_parse Cluster
#' @param c cluster variable from `formula`
#' @returns cluster variable
#' @keywords internal
.cluster <- function(c) {
  c
}
