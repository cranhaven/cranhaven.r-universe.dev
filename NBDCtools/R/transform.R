# type and label ---------------------------------------------------------------

#' Convert categorical columns to factor
#'
#' @description
#' Based on the specifications in the data dictionary, transforms all
#' categorical columns to factor.
#'
#' @param data tibble. The data to be transformed. Columns are
#' expected to be in the data dictionary. If not, they will be skipped.
#' @param study character. NBDC study (One of `"abcd"` or `"hbcd"`.
#' @param release character. Release version (Default: `"latest"`).
#' @return A tibble with the transformed data.
#' @export
#' @examples
#' \dontrun{
#' transf_factor(data, study = "abcd")
#' }
transf_factor <- function(
  data,
  study,
  release = "latest"
) {
  chk::chk_data(data)
  chk::chk_string(study)
  chk::chk_subset(study, names(get_data_pkg("dds")))

  dd <- get_dd(study = study, release = release)
  levels <- get_levels(study = study, release = release)
  sessions <- get_sessions(study = study, release = release)

  col_names <- check_dd(
    col_names = colnames(data),
    dd = dd
  )
  cols_factor <- dd |>
    filter(
      type_data %in% c("character"),
      type_level %in% c("nominal", "ordinal"),
      name %in% col_names
    ) |>
    pull(name, type_level)
  cols_factor <- c("session_id", cols_factor)

  data[cols_factor] <- purrr::map2(
    .x = cols_factor,
    .y = names(cols_factor),
    function(.x, .y) {
      x_trans <- data[[.x]]
      x_meta_label <- attr(x_trans, "label")
      x_labels <- attr(x_trans, "labels")

      if (.x == "session_id") {
        return(transf_session_id(
          session_id = x_trans,
          sessions_df = sessions,
          x_meta_label = x_meta_label,
          x_labels = x_labels
        ))
      }

      x_levels <- get_var_levels(col_name = .x, levels = levels)
      x_trans <- data[[.x]] |>
        factor(levels = x_levels, ordered = .y == "ordinal")

      # fix labels
      if (!is.null(x_meta_label)) {
        attr(x_trans, "label") <- x_meta_label
      }
      if (!is.null(x_labels)) {
        attr(x_trans, "labels") <- x_labels
      }
      x_trans
    }
  )
  attr(data, "transform_type") <- TRUE
  data
}

#' Add variable/value labels
#'
#' @description
#' This function can add variable labels and value labels to the
#' data. The variable labels are descriptive information about the column,
#' and the value labels are the levels of the factor variables.
#'
#' @details
#' ### Two types of labels
#' At least one of `add_var_label` or `add_value_label` must be set to `TRUE`.
#' If both are `FALSE`, an error will be raised.
#'
#' ### Text columns
#' The [transf_factor()] function has a `convert_text` argument,
#' which will convert text columns to unordered factors. When one uses
#' a type transformed data to add labels, the text-factor columns
#' will not have labels at variable level.
#'
#' @param data tibble. The data to be transformed.
#' @param study character. NBDC study (One of `"abcd"` or `"hbcd"`.
#' @param release character. Release version (Default: `"latest"`).
#' @param add_var_label logical. Whether to add variable labels (Default:
#'   `TRUE`).
#' @param add_value_label logical. Whether to add value labels (Default:
#'   `TRUE`).
#' @param id_cols_labels named character vector. A named vector of labels for
#'   the identifier columns, with the names being the column names and the
#'   values being the labels.
#'
#' @return A tibble with the labelled data.
#' @seealso [transf_factor()] for transforming categorical columns to factors.
#' @export
#' @examples
#' \dontrun{
#' transf_label(data)
#' }
transf_label <- function(
    data,
    study,
    release = "latest",
    add_var_label = TRUE,
    add_value_label = TRUE,
    id_cols_labels = c(
      "participant_id" = "Participant identifier",
      "session_id" = "Event identifier",
      "run_id" = "Run identifier"
    )
) {
  chk::chk_data(data)
  chk::chk_logical(add_var_label)
  chk::chk_logical(add_value_label)
  chk::chk_character(id_cols_labels)
  chk::chk_subset(
    names(id_cols_labels),
    union(get_id_cols_abcd(), get_id_cols_hbcd())
  )

  if (!add_var_label && !add_value_label) {
    cli::cli_abort(
      "At least one of `add_var_label` or `add_value_label` must be TRUE."
    )
  }

  dd <- get_dd(study = study, release = release)
  levels <- get_levels(study = study, release = release)
  sessions <- get_sessions(study = study, release = release)

  col_names <- colnames(data)
  col_names <- check_dd(
    col_names = col_names,
    dd = dd
  )
  # factor cols
  cols_factor <- dd |>
    filter(
      type_data %in% c("character"),
      type_level %in% c("nominal", "ordinal")
    ) |>
    pull(name) |>
    intersect(col_names)

  data_meta_label <- if (add_var_label) {
    label_vars(
      data = data,
      dd = dd,
      col_names = col_names
    )
  } else {
    tibble(name = "", label = "")
  }

  data[col_names] <- purrr::map(col_names, function(.x) {
    x_trans <- data[[.x]]
    if (.x %in% names(id_cols_labels)) {
      id_col_label <- get_list_value(id_cols_labels, .x)
      attr(x_trans, "label") <- id_col_label
      if (.x == "session_id") {
        x_trans <- transf_session_id(
          session_id = x_trans,
          sessions_df = sessions,
          x_meta_label = id_col_label,
          transf_type = FALSE,
          add_labels = TRUE
        )
      }
      return(x_trans)
    }

    # extract meta labels
    x_meta_label <- data_meta_label |>
      filter(name == {{ .x }}) |>
      pull(label)

    if (add_var_label) {
      attr(x_trans, "label") <- x_meta_label
    }
    # simple meta labeling
    if (!.x %in% cols_factor) {
      return(x_trans)
    }
    # factor labeling
    if (add_value_label) {
      x_levels <- get_var_levels(.x, levels)
      attr(x_trans, "labels") <- x_levels
    }
    x_trans
  })
  attr(data, "transform_label") <- TRUE
  data
}

#' Get the value of a named list
#'
#' @param list_options named character list. The list providing key-value pairs.
#' @param levels option. The option to get the value for.
#'
#' @return character. The value corresponding to the option in the list.
#' @noRd
#' @keywords internal
get_list_value <- function(list_options, option) {
  if (option %in% names(list_options)) {
    return(list_options[[option]])
  } else {
    return("Invalid option")
  }
}

#' Transform the session_id column to a factor
#'
#' @description
#' This function transforms the session_id column to an ordered
#' factor with the levels and labels from the sessions table.
#'
#' @param session_id character vector. The session_id column of the data.
#' @param sessions tibble. The sessions table.
#' @param x_meta_label character. The label of the session_id column.
#' @param x_labels named character vector. The labels of the session_id column.
#' @param transf_type logical. Whether to transform the session_id column to a
#' factor (Default: `TRUE`).
#' @param add_labels logical. Whether to add labels to the session_id column
#' (Default: `FALSE`).
#'
#' @return A tibble of the data with the `session_id` column
#' transformed to a factor.
#' @noRd
transf_session_id <- function(
    session_id,
    sessions_df,
    x_meta_label = NULL,
    x_labels = NULL,
    transf_type = TRUE,
    add_labels = FALSE) {
  sessions <- session_id |>
    unique()
  session_levels <- sessions_df |>
    filter(session_id %in% sessions) |>
    distinct(session_id, label)

  if (transf_type) {
    session_id <- factor(
      session_id,
      levels = as.character(session_levels$session_id),
      ordered = FALSE
    )
  }

  # append col label back if any
  if (!is.null(x_meta_label)) {
    attr(session_id, "label") <- x_meta_label
  }

  if (add_labels) {
    attr(session_id, "labels") <- setNames(
      as.character(session_levels$session_id),
      session_levels$label
    )
  } else {
    # if no labels are added, but original labels are provided
    if (!is.null(x_labels)) {
      attr(session_id, "labels") <- x_labels
    }
  }

  session_id
}

#' Label all variables
#'
#' @param data tibble. The data to label.
#' @param dd tibble. The data dictionary table
#' @param col_names character vector. Cleaned column names of the data.
#'
#' @return data.frame, the meta labels of data
#' @noRd
label_vars <- function(data, dd, col_names) {
  ## get all labels
  dd_selected <- dd |>
    filter(name %in% col_names) |>
    select(name, label)
  # make sure tbl_col_names has the same order as selected_dd
  data.frame(name = col_names) |>
    left_join(dd_selected, by = "name")
}

#' Get the levels of a variable
#'
#' @param col_name character. The name of the column.
#' @param levels tibble. The levels table.
#'
#' @return named vector, the levels of the variable, sorted
#' @noRd
get_var_levels <- function(col_name, levels) {
  var_level_df <- levels |>
    filter(name == {{ col_name }}) |>
    arrange(order_level)
  if (nrow(var_level_df) == 0) {
    cli::cli_abort(
      "No levels found for {.var {col_name}} in levels table, please check"
    )
  }
  var_level_df |>
    pull(value, label)
}

# value to label ---------------------------------------------------------------

#' Convert values to labels for categorical variables
#'
#' @description
#' Converts the values of categorical/factor columns (e.g., `"1"`, `"2"`) to
#' their labels (e.g., `"Male"`, `"Female"`). The value labels will be set to
#' the values.
#'
#' @details
#' ### Input requirements
#' The data must be type transformed and labelled. See
#' [transf_factor()] and [transf_label()] for details.
#'
#' ```r
#' data <- data |>
#'   transf_factor() |>
#'   transf_label()
#' ```
#'
#' @param data tibble. The labelled dataset
#' @param transf_sess_id logical. Whether to transform the `session_id` column
#' @return A tibble with factor columns transformed to labels.
#' @export
#' @examples
#' \dontrun{
#' transf_value_to_label(data)
#' transf_value_to_label(data, value_to_na = TRUE)
#' }
transf_value_to_label <- function(data, transf_sess_id = FALSE) {
  chk::chk_data(data)
  chk::chk_logical(transf_sess_id)
  # must be typed and labelled
  check_type_label(data)
  cols <- if (transf_sess_id) {
    where(is.factor)
  } else {
    select(data, where(is.factor) & !session_id) |>
      colnames()
  }
  data |>
     mutate(
      across(
        .cols = cols,
        function(.x) {
          col_label <- attr(.x, "label")
          value_labels <- attr(.x, "labels")

          if (is.null(value_labels)) {
            return(.x) # No labels to transform
          }
          label_values <- names(value_labels) |>
            setNames(paste0("^", value_labels, "$"))

          .x <- factor(
            stringr::str_replace_all(.x, label_values),
            levels = names(value_labels),
            ordered = inherits(.x, "ordered")
          )

          if(!is.null(col_label)) {
            attr(.x, "label") <- col_label
          }

          .x
        }
      )
    )
}

# value to na ------------------------------------------------------------------

#' Convert categorical missingness/non-response codes to `NA`
#'
#' @description
#' This function converts the missing codes in the dataset to NA
#' in all factor columns. Example of missing codes are `999`, `888`, `777`, etc.
#' @param data tibble. The labelled dataset and type converted data.
#' @param missing_codes character vector. The missing codes to be
#' converted to NA
#' @param ignore_col_pattern character. A regex pattern to ignore columns
#' that should not be converted to NA.
#' @param id_cols character vector. The names of the ID columns to be
#' excluded from the conversion (Default: identifier columns used in ABCD and
#' HBCD).
#' @details
#' ### Use case
#' This function works the best with `ABCD` data where the missing codes
#' are strictly defined. For `HBCD` data, the missing codes are still
#' under discussion. The function may work, but for some undecided future
#' missing codes, the function may not work as expected.
#'
#' In case of `HBCD` data or other aribitrary missing codes that one wishes
#' to convert to NA, it is recommended to use the
#' [sjmisc::set_na_if()] function instead.
#'
#' ### Input requirements
#' The data must be type transformed and labelled. See
#' [transf_factor()] and [transf_label()] for details.
#'
#' ```r
#' data <- data |>
#'   transf_factor() |>
#'   transf_label()
#' ```
#'
#' @return A tibble of the dataset with missing codes converted to NA
#' @export
#' @examples
#' \dontrun{
#' data <- data |>
#'   transf_factor() |>
#'   transf_label()
#'
#' transf_value_to_na(data)
#' }
transf_value_to_na <- function(
  data,
  missing_codes = c("999", "888", "777", "666", "555", "444", "333", "222"),
  ignore_col_pattern = "__dk$|__dk__l$",
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd())
) {
  chk::chk_data(data)
  chk::chk_string(ignore_col_pattern)
  chk::chk_character(missing_codes)
  chk::chk_character(id_cols)

  # must be typed and labelled
  check_type_label(data)

  data |>
    mutate(
      across(
        c(
          where(is.factor),
          -any_of(id_cols),
          -matches(ignore_col_pattern)
        ),
        function(.x) {
          labels <- attr(.x, "labels")
          names(labels) <- names(attr(.x, "labels"))
          na_values <- labels[labels %in% missing_codes] |>
            as.numeric()
          .x |>
            sjlabelled::set_na(na = na_values, as.tag = TRUE)
        }
      )
    )
}

# time to hms ------------------------------------------------------------------

#' Convert time columns to `hms` format
#'
#' @description
#' This function converts time columns to `hms` format.
#'
#' @details
#' The input data with time columns are expected to have character format
#' of `"HH:MM:SS"`. If it is not in this format, the function will return NA
#' for that row.
#'
#' @param data tibble. The data to be converted.
#' @param study character. NBDC study (One of `"abcd"` or `"hbcd"`).
#' @param release character. Release version (Default: `"latest"`).
#'
#' @return A tibble with time columns converted to `hms` format.
#' @export
#' @examples
#' \dontrun{
#' transf_time_to_hms(data)
#' }
transf_time_to_hms <- function(
  data,
  study,
  release = "latest"
) {
  chk::chk_data(data)

  dd <- get_dd(study = study, release = release)

  vars_time <- dd |>
    filter(type_data == "time") |>
    pull(name)

  regex <- "^(0[0-9]|1[0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]|60)$|^24:00:00$"

  data |>
    mutate(
      across(
        any_of(vars_time),
        \(x) {
          if_else(stringr::str_detect(x, regex), x, NA) |>
            hms::as_hms()
        }
      )
    )
}
