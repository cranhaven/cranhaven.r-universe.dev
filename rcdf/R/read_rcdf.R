#' Read and decrypt RCDF data
#'
#' This function reads an RCDF file, decrypts its contents using the specified decryption key,
#' and loads it into R as an RCDF object.
#'
#' @param path A string specifying the path to the RCDF archive (zip file). If a directory is provided, all \code{.rcdf} files within that directory will be processed.
#' @param decryption_key The key used to decrypt the RCDF contents. This can be an RSA or AES key, depending on how the RCDF was encrypted.
#' @param ... Additional parameters passed to other functions, if needed.
#' @param password A password used for RSA decryption (optional).
#' @param metadata An optional list of metadata object containing data dictionaries, value sets, and primary key constraints for data integrity measure (a \code{data.frame} or \code{tibble} that includes at least two columns: \code{file} and \code{pk_field_name}. This metadata is applied to the data if provided.
#' @param ignore_duplicates A \code{logical} flag. If \code{TRUE}, a warning is issued when duplicates are found, based on the primary key/s defined during creation of RCDF file. If \code{FALSE}, the function stops with an error.
#' @param recursive Logical. If \code{TRUE} and \code{path} is a directory, the function will search recursively for \code{.rcdf} files.
#' @param return_meta Logical. If \code{TRUE}, the metadata will be returned as an attribute of the RCDF object.
#'
#' @return An RCDF object, which is a list of Parquet files (one for each record) along with attached metadata.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' rcdf_data
#'
#' # Using encrypted/password protected private key
#' rcdf_path_pw <- file.path(dir, 'mtcars-pw.rcdf')
#' private_key_pw <- file.path(dir, 'sample-private-key-pw.pem')
#' pw <- '1234'
#'
#' rcdf_data_with_pw <- read_rcdf(
#'   path = rcdf_path_pw,
#'   decryption_key = private_key_pw,
#'   password = pw
#' )
#'
#' rcdf_data_with_pw
#'

read_rcdf <- function(
  path,
  decryption_key,
  ...,
  password = NULL,
  metadata = list(),
  ignore_duplicates = TRUE,
  recursive = FALSE,
  return_meta = FALSE
) {

  if(!fs::file_exists(path)) {
    stop(glue::glue("Specified RCDF file does not exist: {path}"))
  }

  if(fs::is_dir(path)) {
    rcdf_files <- list.files(path, pattern = "\\.rcdf$", full.names = TRUE, recursive = recursive)

    if(length(rcdf_files) == 0) {
      stop(glue::glue("No valid RCDF files in the path specified: {path}"))
    }
  } else {
    if(!grepl("\\.rcdf$", path)) {
      stop(glue::glue("Not a valid RCDF file: {path}"))
    }
    rcdf_files <- path
  }

  data_dictionary <- metadata$dictionary

  conn_duckdb <- DBI::dbConnect(duckdb::duckdb())

  for(i in seq_along(rcdf_files)) {

    rcdf_file <- rcdf_files[i]
    meta <- extract_rcdf(rcdf_file)
    key <- decrypt_key(data = meta, key = decryption_key, password = password)

    pq_files <- list.files(
      path = file.path(meta$dir, 'lineage'),
      pattern = '\\.parquet',
      full.names = TRUE
    )

    if(!is.null(meta$dictionary) & is.null(metadata$dictionary)) {

      if(is.null(data_dictionary)) {
        data_dictionary <- meta$dictionary
      } else {
        data_dictionary <- dplyr::bind_rows(data_dictionary, meta$dictionary)
      }
    }

    pk <- NULL
    if(!ignore_duplicates) {
      pk <- metadata$primary_key
      if(is.null(pk)) { pk <- metadata$primary_keys }
      if(is.null(pk) & "pk_field_name" %in% names(meta$checksum)) { pk <- meta$checksum }
    }

    for(j in seq_along(pq_files)) {

      pq_file <- pq_files[j]
      record <- fs::path_ext_remove(basename(pq_file))

      pk_q <- NULL
      if(!is.null(pk)) {
        pk_i <- dplyr::filter(pk, file == record)

        if(nrow(pk_i) > 0) {
          pk_q <- pk_i$pk_field_name[1]
        }
      }

      DBI::dbExecute(
        conn_duckdb,
        glue::glue("PRAGMA add_parquet_key('{key$aes_key}', '{key$aes_iv}')")
      )

      if(DBI::dbExistsTable(conn_duckdb, record)) {

        DBI::dbExecute(
          conn_duckdb,
          glue::glue(
            "CREATE TABLE {record}_temp
            AS SELECT * FROM read_parquet('{pq_file}',
            encryption_config = {{ footer_key: '{key$aes_key}' }});"
          )
        )
        if(!is.null(pk_q)) {
          DBI::dbExecute(conn_duckdb, "ALTER TABLE {record}_temp ADD PRIMARY KEY ({pk_q})")
        }

        DBI::dbExecute(
          conn_duckdb,
          glue::glue("INSERT INTO {record} (SELECT * FROM {record}_temp);")
        )

        DBI::dbExecute(conn_duckdb, glue::glue("DROP TABLE IF EXISTS {record}_temp;"))

      } else {
        DBI::dbExecute(
          conn_duckdb,
          glue::glue(
            "CREATE TABLE {record}
            AS SELECT * FROM read_parquet('{pq_file}',
            encryption_config = {{ footer_key: '{key$aes_key}' }});"
          )
        )

        if(!is.null(pk_q)) {
          DBI::dbExecute(conn_duckdb, "ALTER TABLE {record} ADD PRIMARY KEY ({pk_q})")
        }
      }
    }
  }

  pq <- rcdf_list()
  records <- DBI::dbListTables(conn_duckdb)

  for(i in seq_along(records)) {

    record_i <- records[i]

    pq_i <- dplyr::collect(dplyr::tbl(conn_duckdb, record_i))
    if(nrow(pq_i) == 0) next

    if(length(data_dictionary) > 0) {
      pq_i <- add_metadata(pq_i, data_dictionary)
    }

    pq[[record_i]] <- pq_i

  }

  DBI::dbDisconnect(conn_duckdb, shutdown = TRUE)
  unlink(meta$dir_base, recursive = TRUE, force = TRUE)

  if(!is.null(data_dictionary) & return_meta) {
    pq[['__data_dictionary']] <- data_dictionary
  }

  if(return_meta) {
    meta$dictionary <- NULL
    meta$dir <- NULL
    attr(pq, 'metadata') <- meta
  }

  return(pq)

}


extract_rcdf <- function(path, meta_only = FALSE) {

  temp_dir <-  tempdir()
  temp_dir_split <- (length(stringr::str_split_1(temp_dir, '\\/')) - 1)

  # Extract the base part of the temp directory
  temp_dir_extract <- stringr::str_split_1(temp_dir, '\\/')[1:temp_dir_split]

  temp_dir_rcdf <- paste0(paste0(temp_dir_extract, collapse = '/'), '/')

  temp_dir_rcdf <- list.files(
    path = temp_dir_rcdf,
    pattern = '__rcdf_temp__',
    include.dirs = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )

  if(length(temp_dir_rcdf) > 0) {
    for(i in seq_along(temp_dir_rcdf)) {
      unlink(temp_dir_rcdf[i], recursive = TRUE, force = TRUE)
    }
  }

  temp_dir_to <- file.path(temp_dir, '__rcdf_temp__', fs::path_ext_remove(basename(path)))

  zip::unzip(path, exdir = temp_dir_to, junkpaths = TRUE)
  if(!meta_only) {
    zip::unzip(file.path(temp_dir_to, 'lineage.zip'), exdir = temp_dir_to)
  }

  unlink(file.path(temp_dir_to, 'lineage.zip'), recursive = TRUE, force = TRUE)

  meta <- jsonlite::read_json(file.path(temp_dir_to, 'metadata.json'), simplifyVector = TRUE)

  meta$dir <- temp_dir_to
  meta$dir_base <- file.path(temp_dir, '__rcdf_temp__')
  meta

}

decrypt_key <- function(data, key, password = NULL) {

  if(inherits(key, 'rsa')) {

    meta <- extract_key(data)

    aes_key <- openssl::base64_decode(meta$key)
      openssl::rsa_decrypt(key, password = password) |>
      unserialize()

    aes_iv <- openssl::base64_decode(meta$iv)
      openssl::rsa_decrypt(key, password = password) |>
      unserialize()

  } else if (inherits(key, 'character')) {

    if(grepl('.pem$', key)) {

      aes_key <- decrypt_info_rsa(data$key_admin, prv_key = key, password = password)
      aes_iv <- decrypt_info_rsa(data$iv_admin, prv_key = key, password = password)

    } else {

      aes_key <- decrypt_info_aes(data$key_app, key = list(aes_key = key))
      aes_iv <- decrypt_info_aes(data$iv_app, key = list(aes_key = key))

    }

  }

  return(
    list(
      aes_key = aes_key,
      aes_iv = aes_iv
    )
  )
}
