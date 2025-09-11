#' Create a dataset
#'
#' @description
#' This high-level function simplifies the process of creating a dataset from
#' the ABCD or HBCD Study data by allowing users to create an analysis-ready
#' dataset in a single step. It executes the lower-level functions provided in
#' the `NBDCtools` package in sequence to load, join, and transform the data.
#'
#' The function expects study data to be stored as one `.parquet` or `.tsv` file
#' per database table within a specified directory, provided as `dir_data`.
#' Variables specified in `vars` and `tables` will be full-joined together,
#' while variables specified in `vars_add` and `tables_add` will be left-joined
#' to these variables. For more details, see [join_tabulated()].
#'
#' In addition to the main `create_dataset()` function, there are two
#' study-specific variations:
#'
#' - `create_dataset_abcd()`: for the ABCD study.
#' - `create_dataset_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `create_dataset()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inherit join_tabulated params
#' @param categ_to_factor logical. Whether to convert categorical
#'   variables to factors class, see [transf_factor()] (Default: `TRUE`).
#' @param add_labels logical. Whether to adds variable and value labels to the
#'   variables, see [transf_label()] (Default: `TRUE`).
#' @param value_to_label logical. Whether to convert the categorical
#'   variables' numeric values to labels, see [transf_value_to_label()]
#'   (Default: `FALSE`). To run this process, `categ_to_factor` and `add_labels`
#'   must be `TRUE`.
#' @param value_to_na logical. Whether to convert categorical
#'   missingness/non-response codes to `NA`, see [transf_value_to_na()]
#'   (Default: `FALSE`). To run this process, `categ_to_factor` and `add_labels`
#'   must be `TRUE`.
#' @param time_to_hms logical. Whether to convert time variables to
#'   `hms` class, see [transf_time_to_hms()] (Default: `FALSE`).
#' @param bind_shadow logical. Whether to bind the shadow matrix to the
#'   dataset (Default: `FALSE`). See more in details.
#' @param ... additional arguments passed to downstream functions after
#'   the [join_tabulated()] step. See examples for details.
#'
#' @details
#' ## Order
#'
#' This high-level function executes the different steps in the following order:
#'
#' 1. Read the data/shadow matrix using [join_tabulated()].
#' 1. Convert categorical variables to factors using [transf_factor()].
#' 1. Add labels to the variables and values using [transf_label()].
#' 1. Convert categorical variables' numeric values to labels using
#'    [transf_value_to_label()].
#' 1. Convert categorical missingness/non-response codes to `NA` using
#'    [transf_value_to_na()].
#' 1. Convert time variables to `hms` class using [transf_time_to_hms()].
#' 1. If `bind_shadow` and the study is `"HBCD"`, replace the missing values
#'    in the shadow due to joining multiple
#'    datasets using [shadow_replace_binding_missing()].
#' 1. Bind the shadow matrix to the data using [shadow_bind_data()].
#'
#' Not all steps are executed by default. The above order represents the maximal
#' order of execution.
#'
#' ## `bind_shadow`
#'
#' If `bind_shadow` is `TRUE`, the shadow matrix will be added to the data using
#' [shadow_bind_data()].
#'
#' - **HBCD study:** For the `HBCD` study, this function uses the shadow matrix
#' from the `dir_data` directory by default (the HBCD Study releases a
#' `_shadow.parquet`/`_shadow.tsv` file per table that accompanies the data).
#' Alternatively, one can set `naniar_shadow = TRUE` as part of the `...`
#' arguments to use `naniar::as_shadow()` to create a shadow matrix from the
#' data.
#' - **ABCD study:** The `ABCD` Study does not currently release shadow
#' matrices. If `bind_shadow` is set to `TRUE`, the function will create the
#' shadow matrix from the data using `naniar::as_shadow()`; no extra
#' `naniar_shadow = TRUE` argument is needed.
#'
#' @return A tibble with the analysis-ready dataset.
#' @export
#' @examples
#' \dontrun{
#' # most common use case
#' create_dataset(
#'   dir_data = "6_0/data",
#'   study = "abcd",
#'   vars = c("var1", "var2", "var3")
#' )
#'
#' # to handle with tagged missingness
#' create_dataset(
#'   dir_data = "1_0/data",
#'   study = "hbcd",
#'   vars = c("var1", "var2", "var3"),
#'   value_to_na = TRUE
#' )
#'
#' # to bind shadow matrices to the data
#' create_dataset(
#'   dir_data = "1_0/data/",
#'   study = "hbcd",
#'   vars = c("var1", "var2", "var3"),
#'   bind_shadow = TRUE
#' )
#'
#' # to use the additional arguments
#' # for example in `value_to_na` option, the underlying function
#' # `transf_value_to_na()` has 2 more arguments,
#' # which can be passed to the `create_dataset()` function
#' create_dataset(
#'   dir_data = "6_0/data",
#'   study = "abcd",
#'   vars = c("var1", "var2", "var3"),
#'   value_to_na = TRUE,
#'   missing_codes = c("999", "888", "777", "666", "555", "444", "333", "222"),
#'   ignore_col_pattern = "__dk$|__dk__l$"
#' )
#'
#' # use study specific functions
#' create_dataset_abcd(
#'   dir_data = "6_0/data",
#'   vars = c("var1", "var2", "var3")
#' )
#' }
create_dataset <- function(
  dir_data,
  study,
  vars = NULL,
  tables = NULL,
  vars_add = NULL,
  tables_add = NULL,
  release = "latest",
  format = "parquet",
  bypass_ram_check = FALSE,
  categ_to_factor = TRUE,
  add_labels = TRUE,
  value_to_label = FALSE,
  value_to_na = FALSE,
  time_to_hms = FALSE,
  bind_shadow = FALSE,
  ...
) {
  time_start <- Sys.time()
  chk::chk_logical(categ_to_factor)
  chk::chk_logical(add_labels)
  chk::chk_logical(value_to_label)
  chk::chk_logical(value_to_na)
  chk::chk_logical(time_to_hms)
  chk::chk_logical(bind_shadow)

  args_user <- list(...)

  # decide if naniar_shadow is used, if so, some steps are skipped
  use_default_shadow <- TRUE
  if (bind_shadow && study == "hbcd") {
    if ("naniar_shadow" %in% names(args_user) &&
        args_user$naniar_shadow) {
      use_default_shadow <- FALSE
    }
  } else if (bind_shadow && study == "abcd") {
    # for ABCD study, naniar_shadow is always used
    use_default_shadow <- FALSE
    args_user$naniar_shadow <- TRUE
  }

  if (bind_shadow && use_default_shadow) {
    cli::cli_progress_step(
      "Loading the shadow matrix from the {.val {dir_data}} directory."
    )
    data_shadow <- join_tabulated(
      dir_data = dir_data,
      study = study,
      vars = vars,
      tables = tables,
      vars_add = vars_add,
      tables_add = tables_add,
      release = release,
      format = format,
      shadow = TRUE,
      remove_empty_rows = !bind_shadow,
      bypass_ram_check = bypass_ram_check
    )
    invisible(gc()) # force garbage collection to free memory
  } else {
    data_shadow <- NULL
  }

  cli::cli_progress_step(
    "Loading the data from the {.val {dir_data}} directory."
  )
  data <- join_tabulated(
    dir_data = dir_data,
    study = study,
    vars = vars,
    tables = tables,
    vars_add = vars_add,
    tables_add = tables_add,
    release = release,
    format = format,
    shadow = FALSE,
    remove_empty_rows = !bind_shadow,
    bypass_ram_check = bypass_ram_check
  )
  invisible(gc())

  if (categ_to_factor) {
    cli::cli_progress_step("Converting categorical variables to factors.")
    data <- create_dataset_action(
      action = transf_factor,
      args = list(data = data, study = study, release = release),
      args_user = args_user
    )
  }
  if (add_labels) {
    cli::cli_progress_step("Adding variable and value labels.")
    data <- create_dataset_action(
      action = transf_label,
      args = list(data = data, study = study, release = release),
      args_user = args_user
    )
  }

  if (value_to_label) {
    if (!categ_to_factor || !add_labels) {
      cli::cli_abort(
        "To convert values to labels, both {.arg categ_to_factor}
        and {.arg add_labels} must be {.val TRUE}."
      )
    }
    cli::cli_progress_step(
      "Converting categorical variables' numeric values to labels."
    )
    data <- create_dataset_action(
      action = transf_value_to_label,
      args = list(data = data),
      args_user = args_user
    )
  }

  if (value_to_na) {
    if (!categ_to_factor || !add_labels) {
      cli::cli_abort(
        "To convert values to NA, both {.arg categ_to_factor}
        and {.arg add_labels} must be {.val TRUE}."
      )
    }
    cli::cli_progress_step(
      "Converting categorical missingness/non-response codes to {.val NA}."
    )
    data <- create_dataset_action(
      action = transf_value_to_na,
      args = list(data = data),
      args_user = args_user
    )
  }

  if (time_to_hms) {
    cli::cli_progress_step("Converting time variables to {.cls hms} class.")
    data <- create_dataset_action(
      action = transf_time_to_hms,
      args = list(data = data, study = study, release = release),
      args_user = args_user
    )
  }

  if (bind_shadow) {
    if (use_default_shadow) {
      cli::cli_progress_step(
        "Replacing the missing values in the shadow matrices
        due to joining multiple datasets."
      )
      # additional processing of shadow matrix for session ID transformation
      # because of value_to_label
      if (value_to_label) {
        data_shadow$session_id <- transf_session_id(
          session_id = data_shadow$session_id,
          sessions_df = get_sessions(study = study, release = release)
        )
        attr(data_shadow, "transform_type") <- TRUE
        attr(data_shadow, "transform_label") <- TRUE
        data_shadow <- data_shadow |>
          transf_value_to_label()
      }
      data_shadow <- create_dataset_action(
        action = shadow_replace_binding_missing,
        args = list(data = data, shadow = data_shadow),
        args_user = args_user
      )
    }
    cli::cli_progress_step("Binding the shadow matrix to the data.")
    data <- create_dataset_action(
      action = shadow_bind_data,
      args = list(data = data, shadow = data_shadow),
      args_user = args_user
    ) |>
      filter_empty_rows()
  }
  time_end <- Sys.time()
  time_used <- difftime(time_end, time_start, units = "mins")

  try(cli::cli_progress_cleanup(), silent = TRUE)
  cli::cli_inform(c(
    v = "A dataset with {.val {nrow(data)}} rows and {.val {ncol(data)}}
    columns has been created. Time used: {.val {round(time_used, 2)}} minutes."
  ))
  data
}

create_dataset_action <- function(action, args, args_user = NULL) {
  if (!is.function(action)) {
    cli::cli_abort("`action` must be a function.")
  }
  args_user <- if (is.null(args_user)) {
    list()
  } else {
    filter_args(args_user, action)
  }
  action_name <- deparse(substitute(action))
  purrr::iwalk(
    names(args_user),
    ~ cli::cli_progress_step(
      "Argument {.arg {names(args_user)[.y]}} is passed to
      {.fn {action_name}}."
    )
  )
  do.call(action, append(args, args_user))
}

get_func_args <- function(func) {
  args <- formals(func)
  args <- args[!names(args) %in% c("...")]
  args
}

filter_args <- function(args, func) {
  func_args <- get_func_args(func)
  args <- args[names(args) %in% names(func_args)]
  args
}

#' @rdname create_dataset
#' @export
create_dataset_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn create_dataset_abcd}. It is set to {.val abcd} by default.")
  }
  create_dataset(study = "abcd", ...)
}

#' @rdname create_dataset
#' @export
create_dataset_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn create_dataset_hbcd}. It is set to {.val hbcd} by default.")
  }
  create_dataset(study = "hbcd", ...)
}
