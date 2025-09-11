# all metadata -----------------------------------------------------------------

#' Get metadata
#'
#' @description
#' Retrieves metadata (data dictionary, levels table, event map)
#' for a given study and release version. Allows for filtering by
#' variables and tables.
#'
#' @param study character. The study name. One of "abcd" or "hbcd".
#' @param release character. Release version (Default: `"latest"`).
#' @param vars character (vector). Vector with the names of variables to be
#'   included.
#' @param tables character (vector). Vector with the names of tables to be
#'   included.
#' @param type character. Type of metadata to retrieve. One of `"dd"`,
#' `"levels"`, `"sessions"` (Default: `"dd"`).
#'
#' @return Data frame with the metadata.
#' @export
#' @examples
#' get_metadata("abcd", type = "levels")
#'
#' get_metadata("hbcd", release = "1.0")
#'
#' get_metadata("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))
#'
#' get_metadata("abcd", tables = "ab_g_dyn")
#'
#' get_metadata("abcd", tables = "ab_g_dyn")
#'
#' get_metadata("abcd", type = "sessions")
get_metadata <- function(
  study,
  release = "latest",
  vars = NULL,
  tables = NULL,
  type = "dd"
) {
  if (!is_on_cran()) {
    check_data_pkg_installed()
  }
  chk::chk_string(study)
  chk::chk_subset(study, names(get_data_pkg("dds")))
  chk::chk_string(release)
  release_names <- c(names(get_data_pkg("dds")[[study]]), "latest")
  if (!release %in% release_names) {
    chk::abort_chk(
      glue::glue(
        "Invalid release '{release}'. Valid releases are: ",
        "{paste(release_names, collapse = ', ')}\n",
        "If you believe this version should exist, it might be the metadata\n",
        "is outdated. ",
        "Please update the `NBDCtoolsData` package to get the latest metadata."
      )
    )
  }
  chk::chk_subset(release, release_names)
  if (release == "latest") {
    releases_study <- names(get_data_pkg("dds")[[study]])
    release <- releases_study[which.max(releases_study)]
  }
  if (!is.null(vars)) {
    chk::chk_character(vars)
    vars_valid <- get_data_pkg("dds")[[study]][[release]] |>
      pull(name)
    if (!chk::vld_subset(vars, vars_valid)) {
      chk::abort_chk(
        glue::glue(
          "The following variable(s) do not exist in the metadata: ",
          "{paste(setdiff(vars, vars_valid), collapse = ', ')}"
        )
      )
    }
  }
  if (!is.null(tables)) {
    chk::chk_character(tables)
    tables_valid <- get_data_pkg("dds")[[study]][[release]] |>
      pull(table_name)
    if (!chk::vld_subset(tables, tables_valid)) {
      chk::abort_chk(
        glue::glue(
          "The following table(s) do not exist in the metadata: ",
          "{paste(setdiff(tables, tables_valid), collapse = ', ')}"
        )
      )
    }
  }
  chk::chk_string(type)
  chk::chk_subset(type, c("dd", "levels", "sessions"))

  meta <- switch(
    type,
    dd = get_data_pkg("dds")[[study]][[release]],
    levels = get_data_pkg("levels")[[study]][[release]],
    sessions = get_data_pkg("sessions")[[study]][[release]]
  )

  if (is.null(meta)) {
    chk::abort_chk(
      glue::glue(
        "No metadata found for study '{study}' and release '{release}'."
      )
    )
  }

  if (type == "sessions") {
    return(meta)
  }

  vars_combined <- c(
    vars,
    get_data_pkg("dds")[[study]][[release]] |>
      filter(
        table_name %in% tables
      ) |>
      arrange(
        match(table_name, tables)
      ) |>
      pull(
        name
      )
  )

  if (length(vars_combined) > 0) {
    meta <- meta |>
      filter(
        name %in% vars_combined
      ) |>
      arrange(
        match(name, vars_combined)
      )
  }

  meta
}


# data dictionary --------------------------------------------------------------

#' Get data dictionary
#'
#' @description
#' Retrieves data dictionary for a given study and release version. Allows for
#' filtering by variables and tables. Wrapper around
#' [NBDCtools::get_metadata()].
#'
#' In addition to the main `get_dd()` function, there are two
#' study-specific variations:
#'
#' - `get_dd_abcd()`: for the ABCD study.
#' - `get_dd_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_dd()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_dd()] function.
#' @return Data frame with the data dictionary.
#' @export
#' @examples
#' get_dd("abcd")
#'
#' get_dd("hbcd", release = "1.0")
#'
#' get_dd("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))
#'
#' get_dd("abcd", tables = "ab_g_dyn")
#'
#' get_dd_abcd()
#'
#' get_dd_hbcd(release = "1.0")
get_dd <- function(
  study,
  release = "latest",
  vars = NULL,
  tables = NULL
) {
  get_metadata(study, release, vars, tables, type = "dd")
}

#' @rdname get_dd
#' @export
get_dd_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_dd_abcd}. It is set to {.val abcd} by default.")
  }
  get_dd(study = "abcd", ...)
}

#' @rdname get_dd
#' @export
get_dd_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_dd_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_dd(study = "hbcd", ...)
}


# levels table -----------------------------------------------------------------

#' Get levels table
#'
#' @description
#' Retrieves levels table for a given study and release version. Allows for
#' filtering by variables and tables. Wrapper around
#' [NBDCtools::get_metadata()].
#'
#' In addition to the main `get_levels()` function, there are two
#' study-specific variations:
#'
#' - `get_levels_abcd()`: for the ABCD study.
#' - `get_levels_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_levels()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_levels()] function.
#' @return Data frame with the levels table.
#' @export
#' @examples
#' get_levels("abcd")
#'
#' get_levels("hbcd", release = "1.0")
#'
#' get_levels("abcd", vars = c("ab_g_dyn__visit_type"))
#'
#' get_levels("abcd", tables = "ab_g_dyn")
#'
#' get_levels_abcd(release = "6.0")
#'
#' get_levels_hbcd()
get_levels <- function(study, release = "latest", vars = NULL, tables = NULL) {
  get_metadata(study, release, vars, tables, type = "levels")
}

#' @rdname get_levels
#' @export
get_levels_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_levels_abcd}. It is set to {.val abcd} by default.")
  }
  get_levels(study = "abcd", ...)
}

#' @rdname get_levels
#' @export
get_levels_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_levels_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_levels(study = "hbcd", ...)
}


# sessions table ---------------------------------------------------------------

#' Get sessions table
#'
#' @description
#' Retrieves the sessions table for a given study and release version. Wrapper
#' around [NBDCtools::get_metadata()].
#'
#' In addition to the main `get_sessions()` function, there are two
#' study-specific variations:
#'
#' - `get_sessions_abcd()`: for the ABCD study.
#' - `get_sessions_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_sessions()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_sessions()] function.
#' @return Data frame with the sessions table.
#' @export
#' @examples
#' get_sessions("abcd")
#'
#' get_sessions("hbcd")
#'
#' get_sessions_abcd(release = "6.0")
#'
#' get_sessions_hbcd(release = "1.0")
get_sessions <- function(
  study,
  release = "latest"
) {
  get_metadata(study, release, type = "sessions")
}

#' @rdname get_sessions
#' @export
get_sessions_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_sessions_abcd}. It is set to {.val abcd} by default.")
  }
  get_sessions(study = "abcd", ...)
}

#' @rdname get_sessions
#' @export
get_sessions_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_sessions_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_sessions(study = "hbcd", ...)
}


# identifier columns -----------------------------------------------------------

#' Get identifier columns
#'
#' @description
#' Retrieves the identifier columns for a given study and release version.
#'
#' In addition to the main `get_id_cols()` function, there are two
#' study-specific variations:
#'
#' - `get_id_cols_abcd()`: for the ABCD study.
#' - `get_id_cols_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_id_cols()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_id_cols()] function.
#' @return character vector with the identifier columns.
#' @export
#' @examples
#' get_id_cols("abcd")
#'
#' get_id_cols("hbcd")
#'
#' get_id_cols_abcd(release = "6.0")
#'
#' get_id_cols_hbcd(release = "1.0")
get_id_cols <- function(
  study,
  release = "latest"
) {
  get_dd(study, release) |>
    pull(identifier_columns) |>
    stringr::str_split(" \\| ") |>
    unlist() |>
    unique()
}

#' @rdname get_id_cols
#' @export
get_id_cols_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_id_cols_abcd}. It is set to {.val abcd} by default.")
  }
  get_id_cols(study = "abcd", ...)
}

#' @rdname get_id_cols
#' @export
get_id_cols_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_id_cols_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_id_cols(study = "hbcd", ...)
}
