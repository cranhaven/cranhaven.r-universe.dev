#' Bind the shadow matrix to the data
#'
#' @description
#' This function binds the shadow matrix to the data.
#'
#' @param data tibble. The data.
#' @param shadow tibble. The shadow matrix. If `naniar_shadow` is `TRUE`,
#' this argument is ignored.
#' @param naniar_shadow logical. Whether to use `naniar::as_shadow()` to
#' create the shadow matrix from data instead of providing it as an
#' argument.
#' @param id_cols character. The columns to join by (the identifier column(s))
#' in the data and shadow matrices (Default: identifier columns used in ABCD and
#' HBCD).
#' In `naniar_shadow = TRUE`, these columns are not included in the
#' shadow matrix.
#' @param suffix character. The suffix to add to the shadow columns.
#' Default is `"_shadow"`. For example, if the column name is
#' `"var1"` and the suffix is `"_shadow"`, the
#' resulted column name will be `"var1_shadow"`.
#'
#' If `naniar_shadow = TRUE`, the suffix is `_NA`, as this suffix will
#' have the most compatibility with other functions in the `naniar` package.
#'
#' @details
#' ### Data requirements
#'
#' If `naniar_shadow = FASLE` and `shadow` is provided, the two dataframes
#' must have the same columns, order of the columns does not matter, but
#' ID columns must be the same in both dataframes. If there are extra
#' rows in the shadow matrix, they will be ignored.
#'
#' ### ABCD and HBCD data
#'
#' NBDC releases HBCD data with shadow matrices, which can be used for
#' the `shadow` argument. To work with ABCD data, the option for
#' now is to use `naniar_shadow = TRUE`, which will create a shadow matrix
#' from the data using `naniar::as_shadow()`.
#'
#' @return a dataframe of the data matrix with shadow columns. It will be
#' 2x the size of the original data matrix.
#' @export
#' @examples
#' shadow <- tibble::tibble(
#'   participant_id = c("1", "2", "3"),
#'   session_id = c("1", "2", "3"),
#'   var1 = c("Unknown", NA, NA),
#'   var2 = c("Wish not to answer", NA, NA)
#' )
#' data <- tibble::tibble(
#'   participant_id = c("1", "2", "3"),
#'   session_id = c("1", "2", "3"),
#'   var1 = c(NA, NA, 1),
#'   var2 = c(NA, 2, NA)
#' )
#' shadow_bind_data(data, shadow)
#' \dontrun{
#' shadow_bind_data(data, naniar_shadow = TRUE)
#' }
shadow_bind_data <- function(
  data,
  shadow = NULL,
  naniar_shadow = FALSE,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd()),
  suffix = "_shadow"
) {
  chk::chk_data(data)
  chk::chk_logical(naniar_shadow)
  chk::chk_character(id_cols)
  chk::chk_string(suffix)

  if (naniar_shadow) {
    check_pkgs(list("naniar" = list()))
    # create shadow matrix from data
    return(bind_cols(
      data,
      data |>
        select(-any_of(id_cols)) |>
        naniar::as_shadow()
    ))
  }

  chk::chk_data(shadow)
  if (!setequal(names(data), names(shadow))) {
    cli::cli_abort("data and shadow do not have the same columns.")
  }
  if (nrow(data) != nrow(shadow)) {
    cli::cli_abort("data and shadow do not have the same number of rows.")
  }
  if (!identical(names(shadow), names(data))) {
    shadow <- select(shadow, names(data))
  }

  id_cols <- intersect(names(data), id_cols)
  # preserve id cols
  data |>
    select(any_of(id_cols)) |>
    bind_cols(
      data |>
        left_join(
          shadow,
          by = id_cols,
          suffix = c("", suffix)
        ) |>
        select(
          -any_of(id_cols)
        )
    )
}

#' Fix binding resulted missingness in shadow matrices
#'
#' @description
#' This function replaces the missing values in the shadow matrices.
#' This is done by checking if the values in
#' shadow matrices are both NA. If they are, the value in the shadow
#' matrix is replaced with `Missing due to joining`.
#'
#' @param data tibble. The data.
#' @param shadow tibble. The shadow matrix.
#' @param id_cols character (vector). The possible unique identifier columns.
#' The data does not need to have all of these columns, but if they are
#' present, they will be used to identify unique rows (Default: identifier
#' columns used in ABCD and HBCD).
#' For example, the ABCD data usually has only `participant_id` and
#' `session_id`, so if `run_id` is provided, it will be ignored.
#' @param replacement character. The value to replace the missing values with.
#' @details
#' Data and shadow requirements: The two dataframes must have the same
#' columns and the same number of rows. They must have the same column names,
#' but the order of the columns does not matter. It is recommended to
#' use the same column order and the same row order (by ID columns) in both
#' dataframes, which saves some processing time.
#'
#' @return A tibble of the shadow matrix with missing values replaced.
#' @export
#' @examples
#' shadow <- tibble::tibble(
#'   participant_id = c("1", "2", "3"),
#'   session_id = c("1", "2", "3"),
#'   var1 = c("Unknown", NA, NA),
#'   var2 = c("Wish not to answer", NA, NA)
#' )
#' data <- tibble::tibble(
#'   participant_id = c("1", "2", "3"),
#'   session_id = c("1", "2", "3"),
#'   var1 = c(NA, NA, 1),
#'   var2 = c(NA, 2, NA)
#' )
#' shadow_replace_binding_missing(data, shadow)
shadow_replace_binding_missing <- function(
  data,
  shadow,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd()),
  replacement = "Missing due to joining"
) {
  check_data(data)
  check_data(shadow)
  chk::chk_string(replacement)
  chk::chk_character(id_cols)
  if (!setequal(names(data), names(shadow))) {
    cli::cli_abort("data and shadow do not have the same columns.")
  }
  if (nrow(data) != nrow(shadow)) {
    cli::cli_abort("data and shadow do not have the same number of rows.")
  }
  # sort shadow by the same order as data if needed
  if (!identical(names(shadow), names(data))) {
    shadow <- select(shadow, names(data))
  }
  # find potential ID columns
  id_cols <- intersect(names(data), id_cols)
  if (length(id_cols) == 0) {
    cli::cli_abort("No ID columns found. Cannot ensure row order matching.")
  }

  have_same_rows <- purrr::map(
    id_cols,
    ~ {
      identical(
        as.character(data[[.x]]),
        as.character(shadow[[.x]])
      )
    }
  ) |>
    unlist() |>
    all()
  if (!have_same_rows) {
    id_data <- tidyr::unite(
      data, "id", any_of(id_cols),
      sep = "", remove = TRUE
    ) |>
      pull(id)
    id_shadow <- tidyr::unite(
      shadow, "id", any_of(id_cols),
      sep = "", remove = TRUE
    ) |>
      pull(id)
    if (!setequal(id_data, id_shadow)) {
      wrong_idx <- which(id_data != id_shadow)
      cli::cli_abort(c(
        "data and shadow do not have the same IDs.",
        "The following rows are different: ",
        paste(wrong_idx, collapse = ", ")
      ))
    }
    cli::cli_warn(c(
      "data and shadow have the same IDs, but they are not in the same order.",
      "The shadow matrix will be sorted by the ID columns."
    ))
    data <- data |>
      # as id cols of data maybe factor, convert to character to ensure
      # levels do not affect the order
      mutate(across(all_of(id_cols), as.character)) |>
      arrange(data, across(all_of(id_cols)))
    shadow <- arrange(shadow, across(all_of(id_cols)))
  }

  # find cells that are NA in both dataframes
  both_na <- is.na(data) & is.na(shadow)
  # ignore the ID columns
  both_na[, colnames(both_na) %in% id_cols] <- FALSE
  # replace both NA cells in shadow
  shadow[both_na] <- replacement
  shadow
}
