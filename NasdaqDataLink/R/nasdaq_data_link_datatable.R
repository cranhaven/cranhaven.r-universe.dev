#' Retrieves Data from the Nasdaq Data Link Datatable endpoint
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param datatable_code Datatable code on Nasdaq Data Link specified as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.datatable('ZACKS/FC', paginate=TRUE)
#' }
#' @export
NasdaqDataLink.datatable <- function(datatable_code, paginate = FALSE, ...) {
  path <- paste0("datatables/", datatable_code)
  nasdaq_data_link.datatable.perform(path, paginate, list(...))
}

nasdaq_data_link.datatable.perform <- function(path, paginate, params) {
  # make request for first page of data
  json <- do.call(nasdaq_data_link.api, c(path = path, params))
  datatable <- json$datatable
  data <- datatable$data

  # contains a list of names and corresponding types
  columns <- datatable$columns
  next_cursor_id <- json$meta$next_cursor_id
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  # continue to make requests for data if paginate=TRUE and there is data
  while (isTRUE(paginate) && !is.null(next_cursor_id)) {
    params["qopts.cursor_id"] <- next_cursor_id
    json <- do.call(nasdaq_data_link.api, c(path = path, params))
    df_page <- as.data.frame(json$datatable$data, stringsAsFactors = FALSE)
    df <- rbind(df, df_page)
    next_cursor_id <- json$meta$next_cursor_id

    # only fetch a maximum of 1,000,000 rows
    if (nrow(df) >= nasdaq_data_link.datatable.max_rows() && !is.null(next_cursor_id)) {
      warning(paste("This call returns a larger amount of data than NasdaqDataLink.datatable() allows.",
                    "Please view our documentation on developer methods to request more data.",
                    "https://github.com/nasdaq/data-link-r/blob/master/README.md#datatables"), call. = FALSE)
      break
    }
  }

  if (!isTRUE(paginate) && !is.null(next_cursor_id)) {
    warning(paste("This call returns more data. To request more pages, please set paginate=TRUE",
                  "in your NasdaqDataLink.datatable() call. For more information see our documentation:",
                  "https://github.com/nasdaq/data-link-r/blob/master/README.md#datatables"), call. = FALSE)
  }

  df <- nasdaq_data_link.datatable.set_df_columns(df, columns)

  return(df)
}

#' Downloads a zip with all data requested from a Nasdaq Data Link database
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param datatable_code Datatable code on Nasdaq Data Link specified as a string.
#' @param filename Filename (including path) of file to download.
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.datatable.bulk_download_to_file('ZACKS/EE')
#' }
#' @export
NasdaqDataLink.datatable.bulk_download_to_file <- function(datatable_code, filename, ...) {
  download_link <- NasdaqDataLink.datatable.bulk_download_url(datatable_code, ...)

  response <- httr::GET(download_link,
                        httr::write_disk(filename, overwrite = TRUE),
                        httr::progress())
}

#' Generates and returns a bulk download url
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param datatable_code Datatable code on Nasdaq Data Link specified as a string.
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters. Please see \url{https://docs.data.nasdaq.com/docs/parameters-1} for a full list of parameters.
#' @return Returns the download url.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' url = NasdaqDataLink.datatable.bulk_download_url("ZACKS/EE", ticker="AAPL")
#' }
#' @export
NasdaqDataLink.datatable.bulk_download_url <- function(datatable_code, ...) {
  path <- paste0("datatables/", datatable_code)
  params <- c(list(...), qopts.export='true')

  json <- nasdaq_data_link.datatable.poll_export(path, params)

  download_link <- json$datatable_bulk_download$file$link
  return(download_link)
}

nasdaq_data_link.datatable.poll_export <-function(path, params) {
  json <- do.call(nasdaq_data_link.api, c(path = path, params))
  first_time <- Sys.time()
  while(!nasdaq_data_link.datatable.export_ready(json, first_time)) {
    Sys.sleep(60)
    json <- do.call(nasdaq_data_link.api, c(path = path, params))
  }
  return(json)
}

nasdaq_data_link.datatable.snapshot_time <- function(response) {
  if(is.null(response$datatable_bulk_download$file$data_snapshot_time)) {
    return('1970-01-01')
  } else {
    return(response$datatable_bulk_download$file$data_snapshot_time)
  }
}

nasdaq_data_link.datatable.export_ready <- function(response, first_time='1970-01-01') {
  return(response$datatable_bulk_download$file$status == 'fresh' | nasdaq_data_link.datatable.snapshot_time(response) >= first_time)
}
nasdaq_data_link.datatable.set_df_columns <- function(df, columns) {
  ncols <- length(columns[, 1])
  # if df is empty create an empty df with ncolumns set
  # or else we won't be able to set the column names
  if (nrow(df) <= 0 && ncols > 0) {
    df <- data.frame(matrix(ncol = ncols, nrow = 0))
  }

  # set column names
  names(df) <- columns[, 1]

  # set column types
  df <- nasdaq_data_link.datatable.convert_df_columns(df, columns[, 2])

  return(df)
}

nasdaq_data_link.datatable.convert_df_columns <- function(df, column_types) {
  if (length(column_types) <= 0) {
    return(df)
  }
  column_types <- tolower(column_types)
  for (i in 1:length(column_types)) {
    if (grepl("^float|^bigdecimal|^integer|^double", column_types[i])) {
      df[, i] <- as.numeric(df[, i])
    } else if (grepl("^datetime", column_types[i])) {
      df[, i] <- as.POSIXct(df[, i])
    } else if (grepl("^date", column_types[i])) {
      df[, i] <- as.Date(df[, i])
    } else {
      df[, i] <- as.character(df[, i])
    }
  }
  return(df)
}

nasdaq_data_link.datatable.max_rows <- function() {
  return(1000000)
}
