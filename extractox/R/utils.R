#' Verify SSL
#'
#' @param verify_ssl Boolean.
#' @param ... Any other arguments to be supplied to `req_option`
#' @keywords internal
#' @noRd
#' @return Named list.
set_ssl <- function(verify_ssl, ...) {
  libcurl_opt <- list(...)
  if (!verify_ssl) {
    libcurl_opt[["ssl_verifyhost"]] <- 0
    libcurl_opt[["ssl_verifypeer"]] <- 0
  }
  libcurl_opt
}


#' Download and Process Data from a URL
#'
#' Downloads data from a specified URL, processes the response, and returns a
#' cleaned data frame. The function handles HTTP requests, saves temporary
#' files, and extracts table data from HTML content. Initially developed for
#' EPA's PPRTVS data extraction but designed to be generalizable for similar
#' use cases.
#'
#' @param url Character string specifying the URL to download data from
#' @param url_query_param List of query parameters to be added to the URL
#' @param file_name Character string specifying the name for the downloaded file
#' @param file_ext Character string specifying the file extension.
#'   Default is "file".
#' @param verbose Logical indicating whether to display progress messages.
#'   Default is FALSE.
#' @return A data frame containing:
#'   * The processed table data from the HTML content
#'   * Clean column names (via janitor::clean_names)
#'   * An additional column 'date_downloaded' with the response timestamp
#' @keywords internal
#' @noRd
download_db <- function(url,
                        url_query_param,
                        file_name,
                        file_ext = "file",
                        verbose = TRUE) {
  check_internet(verbose = verbose)

  # Perform the request and get a response
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Downloading data from {.url {url}}.")
  }

  dat_file <- tempfile(fileext = file_ext)

  if (isTRUE(check_need_libcurl_condathis())) {
    condathis_downgrade_libcurl()

    url_to_use <-
      paste0(
        url,
        "?",
        paste(names(url_query_param),
          url_query_param,
          sep = "=",
          collapse = "&"
        )
      )

    req <- condathis::run("curl", "-o", dat_file, url_to_use, "-v",
      env_name = "openssl-linux-env",
      verbose = FALSE
    )
    http_date <- gsub(
      "< Date: |\\r", "",
      grep("< Date:",
        strsplit(req$stderr, "\n")[[1]],
        value = TRUE
      )
    )
  } else {
    req <- httr2::request(url) |>
      httr2::req_url_query(
        !!!url_query_param,
        multi = "explore"
      ) |>
      httr2::req_perform()

    req |>
      httr2::resp_body_raw() |>
      writeBin(dat_file)

    http_date <- httr2::resp_date(req)
  }

  out <- dat_file |>
    rvest::read_html() |>
    rvest::html_nodes("table") |>
    rvest::html_table(fill = TRUE)

  out_cl <- out[[1]] |>
    janitor::clean_names()

  out_cl[, "date_downloaded"] <- http_date

  out_cl
}

#' Search and Match Data
#'
#' This function searches for matches in a dataframe based on a given list of
#' ids and search type, then combines the results into a single dataframe,
#' making sure that NA rows are added for any missing ids. The column
#' `query` is a the end of the dataframe.
#'
#' @param dat The dataframe to be searched.
#' @param ids A vector of ids to search for.
#' @param search_type The type of search: "casrn" or "name".
#' @param col_names Column names to be used when creating a new dataframe in
#'   case of no matches.
#' @param chemical_col The name of the column in dat where chemical names
#'    are stored.
#' @return A dataframe with search results.
#' @keywords internal
#'
#' @details This function is used in `extr_pprtv` and `extr_monograph`.
#'
#' @seealso
#' \code{\link{extr_pprtv}}, \code{\link{extr_monograph}}
search_and_match <- function(dat,
                             ids,
                             search_type,
                             col_names,
                             chemical_col = "chemical") {
  results <- lapply(ids, function(id) {
    if (search_type == "casrn") {
      match <- dat[dat$casrn == id, ]
    } else if (search_type == "name") {
      match <- dat[grepl(id, dat[[chemical_col]]), ]
    }

    if (nrow(match) == 0) {
      match <- data.frame(matrix(NA, nrow = 1, ncol = length(col_names)))
      names(match) <- col_names
    }

    match$query <- id
    match
  })

  out <- do.call(rbind, results)

  # Add NA rows for missing ids
  out <- merge(data.frame(query = ids, stringsAsFactors = FALSE), out,
    by = "query", all.x = TRUE
  )
  out <- out[, col_names]

  return(out)
}


#' Write Dataframes to Excel
#'
#' This function creates an Excel file with each dataframe in a list
#' as a separate sheet.
#'
#' @param df_list A named list of dataframes to write to the Excel file.
#' @param filename The name of the Excel file to create.
#' @return No return value. The function prints a message indicating
#'   the completion of the Excel file writing.
#' @export
#' @examples
#' \donttest{
#' tox_dat <- extr_comptox("50-00-0")
#' temp_file <- tempfile(fileext = ".xlsx")
#' write_dataframes_to_excel(tox_dat, filename = temp_file)
#' }
write_dataframes_to_excel <- function(df_list, filename) {
  if (isFALSE(requireNamespace("openxlsx", quietly = TRUE))) {
    cli::cli_abort(message = "{.pkg  openxlsx} not installed. Install it with: `install.packages('openxlsx')`") # nolint
  }

  wb <- openxlsx::createWorkbook()

  for (name in names(df_list)) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, sheet = name, df_list[[name]])
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  cli::cli_alert_info("Excel file written in {filename}...")
}
