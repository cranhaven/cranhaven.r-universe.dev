#' Write RCDF data to multiple formats
#'
#' Exports RCDF-formatted data to one or more supported open data formats. The function automatically dispatches to the appropriate writer function based on the \code{formats} provided.
#'
#' @param data A named list or RCDF object. Each element should be a table or tibble-like object (typically a \code{dbplyr} or \code{dplyr} table).
#' @param path The target directory where output files should be saved.
#' @param formats A character vector of file formats to export to. Supported formats include: \code{"csv"}, \code{"tsv"}, \code{"json"}, \code{"parquet"}, \code{"xlsx"}, \code{"dta"}, \code{"sav"}, and \code{"sqlite"}.
#' @param ... Additional arguments passed to the respective writer functions.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_csv}  \link[rcdf]{write_rcdf_tsv}  \link[rcdf]{write_rcdf_json}  \link[rcdf]{write_rcdf_xlsx}  \link[rcdf]{write_rcdf_dta}  \link[rcdf]{write_rcdf_sav}  \link[rcdf]{write_rcdf_sqlite}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_as(data = rcdf_data, path = temp_dir, formats = c("csv", "xlsx"))
#'
#' unlink(temp_dir, force = TRUE)


write_rcdf_as <- function(data, path, formats, ...) {

  valid_formats <- c("csv", "tsv", "json", "parquet", "xlsx", "dta", "sav", "sqlite")
  label_formats <- c("CSV", "TSV", "JSON", "Parquet", "Excel", "Stata", "SPSS", "SQLite")

  valid_format_args <- which(formats %in% valid_formats)

  if(length(valid_format_args) < length(formats)) {

    n_invalid <- length(formats) - length(valid_format_args)
    n_invalid_s <- ''
    if(n_invalid > 1) { n_invalid_s <- 's' }

    stop(glue::glue('{n_invalid} invalid format{n_invalid_s} found.'))

  }


  for(i in seq_along(formats)) {

    data_format <- formats[i]
    label_format <- label_formats[which(valid_formats == data_format)]

    write_rcdf_fn <- eval(parse(text = glue::glue("write_rcdf_{data_format}")))

    write_rcdf_fn(
      data = data,
      path = path,
      ...,
      parent_dir = label_format
    )

  }

}



#' Write RCDF data to CSV files
#'
#' Writes each table in the RCDF object as a separate \code{.csv} file.
#'
#' @param data A valid RCDF object.
#' @param path The base output directory.
#' @param ... Additional arguments passed to \code{write.csv()}.
#' @param parent_dir Optional subdirectory under \code{path} to group CSV files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_csv(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_csv <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    utils::write.csv(
      x = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.csv"))
    )
  }

}


#' Write RCDF data to TSV files
#'
#' Writes each table in the RCDF object as a separate tab-separated \code{.txt} file.
#'
#' @param data A valid RCDF object.
#' @param path The base output directory.
#' @param ... Additional arguments passed to \code{write.table()}.
#' @param parent_dir Optional subdirectory under \code{path} to group TSV files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_tsv(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_tsv <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    utils::write.table(
      x = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.txt")),
      sep = "\t"
    )
  }

}


#' Write RCDF data to JSON files
#'
#' Writes each table in the RCDF object as a separate \code{.json} file.
#'
#' @param data A valid RCDF object.
#' @param path The output directory for files.
#' @param ... Additional arguments passed to \code{jsonlite::write_json()}.
#' @param parent_dir Optional subdirectory under \code{path} to group JSON files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_json(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)


write_rcdf_json <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    jsonlite::write_json(
      x = dplyr::collect(data[[record]]),
      path = file.path(path, glue::glue("{record}.json")),
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }

}


#' Write RCDF data to Excel files
#'
#' Writes each table in the RCDF object as a separate \code{.xlsx} file using the \code{openxlsx} package.
#'
#' @param data A valid RCDF object.
#' @param path The output directory.
#' @param ... Additional arguments passed to \code{openxlsx::write.xlsx()}.
#' @param parent_dir Optional subdirectory under \code{path} to group Excel files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_xlsx(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_xlsx <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    openxlsx::write.xlsx(
      x = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.xlsx")),
      ...
    )
  }

}


#' Write RCDF data to Stata \code{.dta} files
#'
#' Writes each table in the RCDF object to a \code{.dta} file for use in Stata.
#'
#' @param data A valid RCDF object.
#' @param path Output directory for files.
#' @param ... Additional arguments passed to \code{foreign::write.dta()}.
#' @param parent_dir Optional subdirectory under \code{path} to group Stata files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_dta(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_dta <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    haven::write_dta(
      data = dplyr::collect(data[[record]]),
      path = file.path(path, glue::glue("{record}.dta")),
      ...
    )
  }

}


#' Write RCDF data to SPSS \code{.sav} files
#'
#' Writes each table in the RCDF object to a \code{.sav} file using the \code{haven} package for compatibility with SPSS.
#'
#' @param data A valid RCDF object.
#' @param path Output directory for files.
#' @param ... Additional arguments passed to \code{haven::write_sav()}.
#' @param parent_dir Optional subdirectory under \code{path} to group SPSS files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_sav(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)


write_rcdf_sav <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    haven::write_sav(
      data = dplyr::collect(data[[record]]),
      path = file.path(path, glue::glue("{record}.sav")),
      ...
    )
  }

}


#' Write RCDF data to a SQLite database
#'
#' Writes all tables in the RCDF object to a single SQLite database file.
#'
#' @param data A valid RCDF object.
#' @param path Output directory for the database file.
#' @param db_name Name of the SQLite database file (without extension).
#' @param ... Additional arguments passed to \code{DBI::dbWriteTable()}.
#' @param parent_dir Optional subdirectory under `path` to store the SQLite file.
#'
#' @return Invisibly returns \code{NULL}. A \code{.db} file is written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_sqlite(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_sqlite <- function(data, path, db_name = "cbms_data", ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, glue::glue("{db_name}.db")))

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    DBI::dbWriteTable(
      conn = conn,
      name = record,
      value = dplyr::collect(data[[record]]),
      ...
    )

  }

  DBI::dbDisconnect(conn, force = TRUE)

}
