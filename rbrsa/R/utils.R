#' Parse JSON response from BDDK/Finturk APIs
#' Shared parser for both API endpoints. Extracts non-hidden columns from JSON response.
#' @param parsed_json The parsed$Json object from API response.
#' @return Data frame with parsed data.
#' @importFrom stats setNames
#' @noRd
parse_json <- function(parsed_json) {
  # Keep all columns. The FinTurk website "hide" some columns for UI display purposes
  cols_to_keep <- seq_along(parsed_json$colModels)
  col_models <- parsed_json$colModels[cols_to_keep]
  col_labels <- parsed_json$colNames[cols_to_keep]
  rows <- parsed_json$data$rows
  
  if (length(rows) == 0) return(data.frame())
  
  final_names <- ifelse(col_labels != "", col_labels,
                        sapply(col_models, function(x) x$name))
  
  filtered_rows <- lapply(rows, function(row) row$cell[cols_to_keep])
  
  col_list <- lapply(seq_along(cols_to_keep), function(i) {
    col_vals <- sapply(filtered_rows, function(x) x[[i]])
    col_vals[sapply(col_vals, is.null)] <- NA
    unlist(col_vals)
  })
  
  df <- list2DF(setNames(col_list, final_names))
  return(df)
}

#' Save Fetched Data to Multiple Formats
#'
#' @param df Data frame to save (with fetch_info attribute for auto-naming).
#' @param filename  **Required**. A non-empty string (without extension) must be provided.
#' @param format Output format: "rds", "csv", or "xlsx". Default "rds".
#' @return Full file path (invisibly).
#' @importFrom utils write.csv
#' @export
#' @examples
#' \donttest{
#'   my_data <- fetch_bddk1(2024, 1, 15)
#'   temp_file <- tempfile() # filename should be without extension
#'   save_data(my_data, temp_file, format = "csv")
#' }
save_data <- function(df, filename = NULL, format = "rds") {
  valid_formats <- c("rds", "csv", "xlsx")
  if (!format %in% valid_formats) {
    stop("Invalid 'format'. Must be one of: ", paste(valid_formats, collapse = ", "))
  }
  if (is.null(filename) || missing(filename) || filename == "") {
    stop("Argument 'filename' is required and cannot be empty")
  }
  filename <- paste0(filename, ".", format)

  switch(format,
         csv = {
           write.csv(df, filename, row.names = FALSE)
           message(sprintf("Data saved to %s", filename))
         },
         xlsx = {
           if (!requireNamespace("writexl", quietly = TRUE)) {
             stop("Please install 'writexl' package for Excel export")
           }
           writexl::write_xlsx(df, filename)
           message(sprintf("Data saved to %s", filename))
         },
         rds = {
           saveRDS(df, filename)
           message(sprintf("Data saved to %s", filename))
         })

  invisible(filename)
}


#' Convert plaka (license plate number)  to province name
#' Maps Turkish license plate numbers to province names used in the Finturk API.
#'
#' @param plaka license plate number (0 for "HEPSI", 1-81 for provinces,
#' 999 for "YURT DISI")
#' @return province name in ALL CAPS as required by API
#' @export
#' @examples
#' plaka_to_city(6)   # "ANKARA"
#' plaka_to_city(34)  # "ISTANBUL"
#' plaka_to_city(0)   # "HEPSI"
plaka_to_city <- function(plaka) {
  cities <- get("cities", envir = asNamespace("rbrsa"))
  invalid <- setdiff(plaka, cities$plaka)
  invalid
  if (length(invalid) > 0) {
    stop(sprintf(
      "Invalid plaka(s): %s. Valid plaka: 0, 1-81, 999",
      paste(invalid, collapse = ", ")
    ))
  }
  # Match plaka codes to city names 
  cities$il[match(plaka, cities$plaka)]  
}

#' List Available Cities for Finturk
#' Print available cities for Finturk quarterly data with plaka (license plate) numbers.
#' @return Data frame of available cities
#' @export
#' @examples
#' list_cities()
list_cities <- function() {
  cities <- get("cities", envir = asNamespace("rbrsa"))

  message("Available cities for Finturk quarterly data")
  message("Use license plate number (plaka) in fetch_finturk functions:")
  message("Valid values: 0 (HEPSI/ALL), 1-81, 999 (YURT DISI/ABROAD)")
  
  df = data.frame(
    plaka = cities$plaka,
    il = cities$il
  )
  print(df, row.names = FALSE)
  invisible(df)
}

#' List available groups
#' Print available banking groups for a data source.
#'
#' @param source Either "bddk" or "finturk"
#' @param lang Either "tr" or "en" for names. "en" is default
#' @return Data frame of available groups (invisibly)
#' @export
#' @examples
#' list_groups("bddk")
#' list_groups("finturk","tr")

list_groups <- function(source = c("bddk", "finturk"), lang = c("en", "tr")) {
  source <- match.arg(source)
  lang <- match.arg(lang)

  if (source == "bddk") {
    groups <- get("bddk_groups", envir = asNamespace("rbrsa"))
    name_col <- if (lang == "tr") "name_tr" else "name_en"
  } else {
    groups <- get("finturk_groups", envir = asNamespace("rbrsa"))
    name_col <- if (lang == "tr") "name_tr" else "name_en"
  }

  df <- data.frame(
    Group_Code = groups$grup_kod,
    Name = groups[[name_col]],
    stringsAsFactors = FALSE
  )

  message(sprintf("Available banking groups for %s data:", source))
  print(df, row.names = FALSE)

  invisible(df)
}

#' List Available Tables
#' Print available tables for a data source.
#'
#' @param source Either "bddk" or "finturk"
#' @param lang Either "tr" or "en" for column names. "en" is default
#' @return Data frame of available tables (invisibly)
#' @export
#' @examples
#' list_tables("bddk")
#' list_tables("finturk", "tr")

list_tables <- function(source = c("bddk", "finturk"), lang = c("en", "tr")) {
  source <- match.arg(source)
  lang <- match.arg(lang)

  if (source == "bddk") {
    tables <- get("bddk_tables", envir = asNamespace("rbrsa"))
    title_col <- if (lang == "tr") "title_tr" else "title_en"
  } else {
    tables <- get("finturk_tables", envir = asNamespace("rbrsa"))
    title_col <- if (lang == "tr") "title_tr" else "title_en"
  }

  df <- data.frame(
    Table_No = tables$table_no,
    Title = tables[[title_col]]
  )

  message(sprintf("Available tables for %s data:", source))
  print(df, row.names = FALSE)

  invisible(df)
}


