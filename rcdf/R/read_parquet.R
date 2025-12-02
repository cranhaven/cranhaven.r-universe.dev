#' Read Parquet file with optional decryption
#'
#' This function reads a Parquet file, optionally decrypting it using the provided decryption key. If no decryption key is provided, it reads the file normally without decryption. It supports reading Parquet files as Arrow tables or regular data frames, depending on the \code{as_arrow_table} argument.
#'
#' @param path The file path to the Parquet file.
#' @param ... Additional arguments passed to \code{arrow::open_dataset()} when no decryption key is provided.
#' @param decryption_key A list containing \code{aes_key} and \code{aes_iv}. If provided, the Parquet file will be decrypted using these keys. Default is `NULL`.
#' @param as_arrow_table Logical. If \code{TRUE}, the function will return the result as an Arrow table. If \code{FALSE}, a regular data frame will be returned. Default is \code{TRUE}.
#' @param metadata Optional metadata (e.g., a data dictionary) to be applied to the resulting data.
#'
#' @return An Arrow table or a data frame, depending on the value of \code{as_arrow_table}.
#' @export
#'
#' @examples
#' # Using sample Parquet files from `mtcars` dataset
#' dir <- system.file("extdata", package = "rcdf")
#'
#' # Without decryption
#' df <- read_parquet(file.path(dir, "mtcars.parquet"))
#' df
#'
#' # With decryption
#' decryption_key <- list(
#'   aes_key = "5bddd0ea4ab48ed5e33b1406180d68158aa255cf3f368bdd4744abc1a7909ead",
#'   aes_iv = "7D3EF463F4CCD81B11B6EC3230327B2D"
#' )
#'
#' df_with_encryption <- read_parquet(
#'   file.path(dir, "mtcars-encrypted.parquet"),
#'   decryption_key = decryption_key
#'  )
#' df_with_encryption

read_parquet <- function(path, ..., decryption_key = NULL, as_arrow_table = TRUE, metadata = NULL) {

  aes_key <- decryption_key$aes_key
  aes_iv <- decryption_key$aes_iv

  if(is.null(aes_key) | is.null(aes_iv)) {
    return(arrow::open_dataset(sources = path, ...))
  }

  pq_conn <- DBI::dbConnect(drv = duckdb::duckdb())
  pq_decrypt <- glue::glue("PRAGMA add_parquet_key('{aes_key}', '{aes_iv}')")
  pq_query <- glue::glue("SELECT * FROM read_parquet('{path}', encryption_config = {{ footer_key: '{aes_key}' }});")

  DBI::dbExecute(conn = pq_conn, statement = pq_decrypt)
  data <- DBI::dbGetQuery(conn = pq_conn, statement = pq_query)
  suppressWarnings(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE))

  if(!is.null(metadata)) { data <- add_metadata(data, metadata) }

  if(as_arrow_table) {
    data <- arrow::arrow_table(data)
  }

  return(data)

}
