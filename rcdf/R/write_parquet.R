#' Write Parquet file with optional encryption
#'
#' This function writes a dataset to a Parquet file. If an encryption key is provided, the data will be encrypted before writing.
#' Otherwise, the function writes the data as a regular Parquet file without encryption.
#'
#' @param data A data frame or tibble to write to a Parquet file.
#' @param path The file path where the Parquet file will be written.
#' @param ... Additional arguments passed to \code{arrow::write_parquet()} if no encryption key is provided.
#' @param encryption_key A list containing \code{aes_key} and \code{aes_iv}. If provided, the data will be encrypted using AES before writing to Parquet.
#'
#' @return None. The function writes the data to a Parquet file at the specified \code{path}.
#' @export
#'
#' @examples
#'
#' data <- mtcars
#' key <- "5bddd0ea4ab48ed5e33b1406180d68158aa255cf3f368bdd4744abc1a7909ead"
#' iv <- "7D3EF463F4CCD81B11B6EC3230327B2D"
#'
#' temp_dir <- tempdir()
#'
#' rcdf::write_parquet(
#'   data = data,
#'   path = file.path(temp_dir, "mtcars.parquet"),
#'   encryption_key = list(aes_key = key, aes_iv = iv)
#' )
#'
#' unlink(file.path(temp_dir, "mtcars.parquet"), force = TRUE)
#'

write_parquet <- function(data, path, ..., encryption_key = NULL) {

  aes_key <- encryption_key$aes_key
  aes_iv <- encryption_key$aes_iv

  if(is.null(aes_key) | is.null(aes_iv)) {
    arrow::write_parquet(x = data, sink = path, ...)
  } else {

    pq_name <- "__TEMP_DATA__"
    pq_conn <- DBI::dbConnect(drv = duckdb::duckdb())
    pq_encrypt <- glue::glue("PRAGMA add_parquet_key('{aes_key}', '{aes_iv}')")
    pq_query <- glue::glue("COPY {pq_name} TO '{path}' (ENCRYPTION_CONFIG {{ footer_key: '{aes_key}' }});")

    DBI::dbExecute(conn = pq_conn, statement = "INSTALL httpfs")
    DBI::dbExecute(conn = pq_conn, statement = "LOAD httpfs")
    DBI::dbExecute(conn = pq_conn, statement = pq_encrypt)

    DBI::dbWriteTable(
      conn = pq_conn,
      name = pq_name,
      value = data,
      overwrite = TRUE
    )

    DBI::dbExecute(conn = pq_conn, statement = pq_query)
    suppressWarnings(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE))
  }

}



#' Write RCDF data to Parquet files
#'
#' This function writes an RCDF object (a list of data frames) to multiple Parquet files. Each data frame in the list is written to its corresponding Parquet file in the specified path.
#'
#' @param data A list where each element is a data frame or tibble that will be written to a Parquet file.
#' @param path The directory path where the Parquet files will be written.
#' @param ... Additional arguments passed to \code{rcdf::write_parquet()} while writing each Parquet file.
#' @param parent_dir An optional parent directory to be included in the path where the files will be written.
#' @param primary_key A \code{data.frame} or \code{tibble} that includes at least two columns: \code{file} and \code{pk_field_name}.
#' @param ignore_duplicates A \code{logical} flag. If \code{TRUE}, a warning is issued when duplicates are found. If \code{FALSE}, the function stops with an error.
#'
#' @return A character vector of file paths to the written Parquet files.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' temp_dir <- tempdir()
#'
#' write_rcdf_parquet(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_parquet <- function(data, path, ..., parent_dir = NULL, primary_key = NULL, ignore_duplicates = TRUE) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)
  records <- records[!grepl('__data_dictionary', records)]

  for(i in seq_along(records)) {

    record_i <- records[i]
    data_i <- data[[record_i]]

    if(!is.null(primary_key)) {
      check_duplicates(
        data = dplyr::collect(data_i),
        record = record_i,
        primary_key = primary_key,
        ignore_duplicates = ignore_duplicates
      )
    }

    rcdf::write_parquet(
      data = dplyr::collect(data_i),
      path = file.path(path, glue::glue("{record_i}.parquet")),
      ...
    )
  }

  list.files(path, pattern = '.parquet', full.names = TRUE)

}


check_duplicates <- function(data, record, primary_key, ignore_duplicates) {

  pk <- dplyr::pull(dplyr::filter(primary_key, file == record), pk_field_name)
  if(length(pk) == 0) return(NULL)

  dup_records <- data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of(unlist(stringr::str_split(pk, ', '))))) |>
    dplyr::count() |>
    dplyr::filter(n > 1)

  dup_n <- nrow(dup_records)

  if(dup_n == 0) return(NULL)

  if_plural <- ""
  if(dup_n > 1) { if_plural <- "s" }

  if(ignore_duplicates) {
    cli::cli_warn("Detected potential duplicates in `{record}` based on provided `primary_key`: {dup_n} row{if_plural}")
  } else {
    stop(cli::cli_warn("Detected potential duplicates in `{record}` based on provided `primary_keys`: {dup_n} row{if_plural}"))
  }
}


