#' Join tabulated data
#'
#' @description
#' Joins selected variables and/or whole tables from the tabulated data/shadow
#' files into a single data frame. Expects the data files to be stored in one
#' directory in `.parquet` or `.tsv` format, with one file per table following
#' the naming convention of the respective NBDC dataset (from the ABCD or HBCD
#' studies). Typically, this will be the `rawdata/phenotype/` directory within
#' a BIDS dataset downloaded from the NBDC Data Hub.
#'
#' Variables specified in `vars` and `tables` will be full-joined together,
#' i.e., all rows will be kept, even if they do not have a value for all
#' columns. Variables specified in `vars_add` will be left-joined to the
#' variables selected in `vars` and `tables`, i.e., only the values for already
#' existing rows will be added and no new rows will be created. This is useful
#' for adding variables to the dataset that are important for a given analysis
#' but are not the main variables of interest (e.g., design/nesting or
#' demographic information). By left-joining these variables, one avoids
#' creating new rows that contain only missing values for the main variables of
#' interest selected using `vars` and `tables`. If the same variables are
#' specified in `vars`/`tables` and `vars_add`/`tables_add`, the variables in
#' `vars_add`/`tables_add` will be ignored.
#'
#' In addition to the main `join_tabulated()` function, there are two
#' study-specific variations:
#'
#' - `join_tabulated_abcd()`: for the ABCD study.
#' - `join_tabulated_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `join_tabulated()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @param dir_data character. Path to the directory with the data files in
#'   `.parquet` or `.tsv` format.
#' @param study character. NBDC study (One of `"abcd"` or `"hbcd"`).
#' @param vars character (vector). Name(s) of variable(s) to be joined.
#'   (Default: `NULL`, i.e., no variables are selected; one of `tables` or
#'   `vars` has to be provided).
#' @param tables character (vector). Name(s) of table(s) to be joined (Default:
#'   `NULL`, i.e., no tables are selected; one of `tables` or `vars` has to be
#'   provided).
#' @param vars_add character (vector). Name(s) of additional variable(s) to be
#'   left-joined to the variables selected in `vars` and `tables` (Default:
#'   `NULL`, i.e., no additional variables are selected)
#' @param tables_add character (vector). Name(s) of additional table(s) to be
#'   left-joined to the variables selected in `vars` and `tables` (Default:
#'   `NULL`, i.e., no additional tables are selected)
#' @param release character. Release version (Default: `"latest"`)
#' @param format character. Data format (One of `"parquet"` or `"tsv"`; default:
#'   `"parquet"`).
#' @param shadow logical. Whether to join the shadow matrix
#'   instead of the data table (default: `FALSE`).
#' @param remove_empty_rows logical. Whether to filter out rows that have
#'   all values missing in the joined variables, except for the
#'   ID columns (default: `TRUE`).
#' @param ... Additional arguments passed to the underlying function
#' [join_tabulated()]
#'
#' Note: Turning this parameter to `FALSE` is useful for shadow matrices
#' processing. Some shadow-related functions expect the shadow matrix to
#' have the same dimensions as the original data to proceed correctly.
#' See [shadow_bind_data()] and [shadow_replace_binding_missing()]
#'
#' @param bypass_ram_check logical. If `TRUE`, the function will not abort
#' if the number of variables exceeds 10000 and current available RAM is
#' less than 75% of the estimated RAM usage. This can prevent the long
#' loading time of the data, but failing in the middle due to insufficient RAM.
#' For large datasets, it is recommended to save 2 times or more of
#' estimated RAM before running this function.
#'
#' This argument is only used for the ABCD study, as the HBCD data
#' is small enough to be loaded without RAM issues with most personal computers.
#' As HBCD data grows in the future, this may change.
#'
#' @return A tibble of data or shadow matrix with the joined variables.
#' @export
#' @examples
#' \dontrun{
#' join_tabulated(
#'   dir_data = "path/to/data/",
#'   vars     = c("var_1", "var_2", "var_3"),
#'   tables   = c("table_1", "table_2"),
#'   study    = "abcd",
#'   release  = "6.0"
#' )
#' }
join_tabulated <- function(
  dir_data,
  study,
  vars = NULL,
  tables = NULL,
  vars_add = NULL,
  tables_add = NULL,
  release = "latest",
  format = "parquet",
  shadow = FALSE,
  remove_empty_rows = TRUE,
  bypass_ram_check = FALSE
) {
  if (!is_on_cran()) {
    check_data_pkg_installed()
  }
  chk::chk_dir(dir_data)
  chk::chk_string(study)
  chk::chk_subset(study, names(get_data_pkg("dds")))
  if (!is.null(vars)) chk::chk_character(vars)
  if (!is.null(tables)) chk::chk_character(tables)
  if (!is.null(vars_add)) chk::chk_character(vars_add)
  if (!is.null(tables_add)) chk::chk_character(tables_add)
  if (is.null(vars) & is.null(tables)) {
    chk::abort_chk(
      "At least one of `vars` or `tables` must be specified."
    )
  }
  chk::chk_string(format)
  chk::chk_subset(format, c("parquet", "tsv"))
  chk::chk_logical(shadow)
  if (study == "abcd" && isTRUE(shadow)) {
    cli::cli_abort("The `shadow` argument is not supported for the ABCD study.")
  }
  chk::chk_logical(remove_empty_rows)
  chk::chk_logical(bypass_ram_check)

  dd <- get_dd(study, release, vars, tables)
  n_vars <- length(dd$name)

  if (!is.null(vars_add) | !is.null(tables_add)) {
    dd_add <- get_dd(study, release, vars_add, tables_add) |>
      filter(
        !name %in% dd$name
      )
    has_add <- nrow(dd_add) > 0
  } else {
    has_add <- FALSE
  }

  if (has_add) {
    vars_all <- c(dd$name, dd_add$name)
    n_vars_add <- length(dd_add$name)
    n_tables <- length(unique(c(dd$table_name, dd_add$table_name)))
    cli::cli_progress_step(
      "Joining {n_vars + n_vars_add} variable{?s} ({n_vars} main;
      {n_vars_add} additional) from {n_tables} table{?s}"
    )
  } else {
    vars_all <- dd$name
    n_tables <- length(unique(dd$table_name))
    cli::cli_progress_step(
      "Joining {n_vars} variable{?s} from {n_tables} table{?s}..."
    )
  }

  n_vars <- length(unique(vars_all))
  if (!bypass_ram_check && n_vars >= 10000 && study == "abcd") {
    est <- estimate_loading(n_vars = n_vars)
    cli::cli_inform(c(
      i = "Estimated time to load {n_vars} variables is about
      {round(as.numeric(est$time_readable[1]))/10} to
      {round(as.numeric(est$time_readable[1]))} {est$time_readable[2]}",
      i = "Estimated RAM usage is about
      {round(as.numeric(est$ram_readable[1])) * 0.75} to
      {round(as.numeric(est$ram_readable[1]))} {est$ram_readable[2]}"
    ))
    ava_ram <- get_available_memory()
    if (length(ava_ram) == 0) {
      cli::cli_warn(
        "Could not estimate available RAM. Skipping RAM check."
      )
      ava_ram <- 999999
    }
    if (ava_ram < est$ram * 0.75) {
      cli::cli_abort(c(
        "Your available RAM ({round(ava_ram, 2)} GB) may not be enough to
        load {n_vars} variables.",
        "If you are sure that you want to
        proceed, set {.arg bypass_ram_check = TRUE}."
      ))
    }
  }

  id_cols <- get_id_cols(study, release)

  data <- purrr::map(
    unique(dd$identifier_columns),
    ~ join_by_identifier(dir_data, dd, .x, format, shadow = shadow)
  )
  # find first df with most id cols
  first_full_tbl <- purrr::map_int(
    data,
    ~ {
      names(.x) |>
        intersect(id_cols) |>
        length()
    }) |>
    which.max() |>
    first()

  data <- purrr::reduce(
    data[-first_full_tbl],
    .init = data[[first_full_tbl]],
    ~ full_join(
      .x, .y,
      # join id_cols first then y to save time
      by = intersect(names(.x), id_cols) |>
        intersect(names(.y))
    )
  )


  if (remove_empty_rows) {
    data <- data |>
      filter_empty_rows(id_cols = id_cols)
  }

  if (has_add) {
    data_add <- purrr::map(
      unique(dd_add$identifier_columns),
      ~ join_by_identifier(dir_data, dd_add, .x, format, shadow = shadow)
    )
    # find first df with most id cols
    first_full_tbl_add <- purrr::map_int(
      data_add,
      ~ {
        names(.x) |>
          intersect(id_cols) |>
          length()
      }) |>
      which.max() |>
      first()

    data_add <- purrr::reduce(
        data_add[-first_full_tbl_add],
        .init = data_add[[first_full_tbl_add]],
        ~ full_join(
          .x, .y,
          by = intersect(names(.x), id_cols) |>
            intersect(names(.y))
        )
      )

    data <- left_join(
      data,
      data_add,
      by = intersect(names(data), id_cols) |>
        intersect(names(data_add))
    )
  }

  data |>
    mutate(
      across(
        all_of(intersect(id_cols, names(data))),
        as.character
      )
    ) |>
    relocate(
      all_of(intersect(id_cols, names(data))),
      all_of(vars_all)
    ) |>
    arrange(
      across(all_of(intersect(id_cols, names(data))))
    )
}

#' @rdname join_tabulated
#' @export
join_tabulated_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn join_tabulated_abcd}. It is set to {.val abcd} by default.")
  }
  join_tabulated(study = "abcd", ...)
}

#' @rdname join_tabulated
#' @export
join_tabulated_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn join_tabulated_hbcd}. It is set to {.val hbcd} by default.")
  }
  join_tabulated(study = "hbcd", ...)
}

#' Join by identifier set
#'
#' @description
#' Internal helper for [join_tabulated()] that joins requested variables from
#' all tables that have the given (set of) identifier column(s).
#'
#' @inheritParams join_tabulated
#' @param identifiers character (vector). Identifier column(s).
#' @param dd tibble. Data frame with the data dictionary.
#' @param format character. Data format (One of `"parquet"` or `"tsv"`; default:
#'  `"parquet"`).
#' @param shadow logical. Whether to join the shadow matrix
#' instead of the data table (default: `FALSE`).
#' @return A tibble with the joined variables for the given (set of) identifier
#'   column(s).
#' @export
#' @keywords internal
join_by_identifier <- function(
  dir_data,
  dd,
  identifiers,
  format = "parquet",
  shadow = FALSE
) {
  chk::chk_dir(dir_data)
  chk::chk_data(dd)
  chk::chk_character(identifiers)
  chk::chk_string(format)
  chk::chk_subset(format, c("parquet", "tsv"))
  chk::chk_logical(shadow)

  tables <- dd |>
    filter(
      identifier_columns == identifiers
    ) |>
    pull(
      table_name
    ) |>
    unique()

  id_cols <- stringr::str_split(identifiers, " \\| ")[[1]]

  env_load <- new.env()
  cli::cli_progress_bar(
    "Loading variables from files",
    total = length(tables),
    .envir = env_load
  )
  env_join <- new.env()
  cli::cli_progress_bar(
    "Joining variables",
    total = length(tables) - 1,
    .envir = env_join
  )
  on.exit({
    cli::cli_progress_done(.envir = env_load)
    cli::cli_progress_done(.envir = env_join)
  }, TRUE)

  tables |>
    purrr::map(
      ~ {
        on.exit(cli::cli_progress_update(.envir = env_load), TRUE)
        dd_table <- dd |>
          filter(
            table_name == .x
          )
        file_path <- if (shadow) {
          glue::glue("{dir_data}/{.x}_shadow.{format}")
        } else {
          glue::glue("{dir_data}/{.x}.{format}")
        }
        read_file(
          file = file_path,
          dd = dd_table,
          format = format
        )
      }
    ) |>
    purrr::reduce(
      ~ {
        on.exit(cli::cli_progress_update(.envir = env_join), TRUE)
        full_join(.x, .y, by = id_cols)
      }
    )
}

#' Read file
#'
#' @description
#' Internal helper for [NBDCtools::join_by_identifier()] that reads
#' data/shadow matrix for a given file in either
#' `.parquet` or `.tsv` format.
#'
#' @param file character. Path to the file.
#' @param dd tibble. Data frame with the data dictionary used to select columns
#'   and determine the column types if reading from a `.tsv` file.
#' @param format character. Data format (One of `"parquet"` or `"tsv"`)
#'
#' @return A tibble with the data/shadow matrix from the file.
#' @export
#' @keywords internal
read_file <- function(file, dd, format) {
  chk::chk_file(file)
  chk::chk_data(dd)
  chk::chk_string(format)
  chk::chk_subset(format, c("parquet", "tsv"))

  id_cols <- dd |>
    pull(
      identifier_columns
    ) |>
    unique() |>
    stringr::str_split(" \\| ") |>
    _[[1]]
  vars <- dd$name

  if (format == "parquet") {
    arrow::read_parquet(
      file = file,
      col_select = all_of(c(id_cols, vars))
    )
  } else {
    read_dsv_formatted(
      file = file,
      dd = dd,
      action = "ignore"
    )
  }
}

#' Read delimiter (tab/comma) separated values file correctly formatted
#'
#' @description
#' Reads in a `.tsv` or `.csv` file with correctly formatted column types.
#' Uses [readr::read_tsv()]/[readr::read_csv()] internally and specifies the
#' column types explicitly using the `col_types` argument utilizing information
#' from the data dictionary. Returns only the identifier columns and the columns
#' specified in the data dictionary, i.e., all columns in the file that are not
#' specified in the data dictionary are ignored.
#'
#' @details
#' ***WHY THIS IS IMPORTANT:*** `readr::read_tsv()`/`readr::read_csv()` (like
#' other commands to load text files in R or other programming languages) by
#' default infers the column types from the data. This doesn't always work
#' perfectly. For example, it may interpret a column with only integers as a
#' double, or a column with only dates as a character. Sometimes a column may
#' even be read in completely empty because, by default,
#' `readr::read_tsv()`/`readr::read_csv()` only considers the first 1000 rows
#' when inferring the data type and interprets the column as an empty logical
#' vector if those rows are all empty. The NBDC datasets store categorical
#' data as integers formatted as character. By default,
#' `readr::read_tsv()`/`readr::read_csv()` may interpret them as numeric. By
#' specifying the column types explicitly based on what is defined in the
#' data dictionary, we can avoid these issues.
#'
#' ***GENERAL RECOMMENDATION:*** Other file formats like `.parquet` correctly
#' store the column types and don't need to be handled explicitly. They also
#' offer other advantages like faster reading speed and smaller file sizes. As
#' such, these formats should generally be preferred over `.tsv`/`.csv` files.
#' However, if you have to work with `.tsv`/`.csv` files, this function can help
#' you avoid common pitfalls.
#'
#'
#' @param file character. Path to the `.tsv` or `.csv` file.
#' @param dd tibble. Data dictionary specifying the column types. Only columns
#'   specified in the data dictionary are read.
#' @param action character. What to do if there are columns in the file that are
#'   not specified in the data dictionary (One of `"warn"`, `"error"`, or
#'   `"ignore"`; default: `"warn"`).
#'
#' @return A tibble with the data/shadow matrix read from the `.tsv`
#' or `.csv` file.
#' @export
#' @examples
#' \dontrun{
#' dd <- NBDCtools::get_dd("abcd", "6.0")
#' read_tsv_formatted("path/to/file.tsv", dd)
#' }
read_dsv_formatted <- function(
  file,
  dd,
  action = "warn"
) {
  chk::chk_file(file)
  chk::chk_data(dd)
  chk::chk_string(action)
  chk::chk_subset(action, c("warn", "error", "ignore"))
  extension <- tools::file_ext(file)
  if (!extension %in% c("tsv", "csv")) {
    chk::abort_chk(
      "The extension of the file specified as `file` has to be `.tsv` or `.csv`"
    )
  }
  if (extension == "tsv") {
    read_fun <- readr::read_tsv
  } else {
    read_fun <- readr::read_csv
  }

  cols <- read_fun(
    file = file,
    n_max = 0,
    show_col_types = FALSE
  ) |>
    names()

  dd_table <- dd |>
    filter(
      name %in% cols
    )

  id_cols <- dd_table |>
    pull(
      identifier_columns
    ) |>
    unique() |>
    stringr::str_split(" \\| ") |>
    _[[1]]

  cols_unspecified <- setdiff(cols, c(id_cols, dd_table$name))

  if (nrow(dd_table) == 0) {
    cli::cli_abort(
      "None of the columns in the file is specified the data dictionary."
    )
  }
  if (length(cols_unspecified) > 0 & action == "warn") {
    cli::cli_alert_warning(
      paste0(
        "{length(cols_unspecified)} column{?s} in the file are not specified ",
        "in the data dictionary and will be ignored!"
      )
    )
  }
  if (length(cols_unspecified) > 0 & action == "error") {
    cli::cli_abort(
      paste0(
        "{length(cols_unspecified)} column{?s} in the file are not specified ",
        "in the data dictionary!"
      )
    )
  }

  col_types <- dd_table |>
    mutate(
      col_type = case_when(
        type_data %in% c("character", "text", "time") ~ "character",
        type_data == "date" ~ "date",
        type_data == "timestamp" ~ "datetime",
        type_data == "double" ~ "double",
        type_data == "integer" ~ "integer",
      )
    ) |>
    pull(
      col_type,
      name
    )

  read_fun(
    file = file,
    col_select = all_of(c(id_cols, names(col_types))),
    col_types = col_types,
    na = c("", "n/a")
  )
}
