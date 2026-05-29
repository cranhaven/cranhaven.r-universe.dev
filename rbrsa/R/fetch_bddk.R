
#' Fetch Monthly Data from BDDK with Multiple Group Codes
#'
#' Retrieves monthly banking data from the BDDK API for specified group codes.
#' Supports multiple group codes in a single request, returning a combined
#' data frame with consistent numeric `grup_kod` values.
#'
#' @param year Year as 4-digit integer (YYYY).
#' @param month Month as integer (1-12).
#' @param table_no Table number to fetch (1-17). No default. 
#' Use \code{\link{list_tables}} with \code{source = "bddk"} to see available options.
#' @param grup_kod Group code (10001-10010). Default 10001. 
#' Use \code{\link{list_groups}} with \code{source = "bddk"} to see available options.
#' @param currency Currency code ("TL" or "USD"). Default "TL".
#' @param lang Language ("en" or "tr"). Default "en".
#' @return Data frame with a `fetch_info` attribute that contains query details.
#'
#' @examples
#' # Single group code
#' fetch_bddk1(2020, 3, 1, grup_kod = 10001)
#'
#' # Multiple group codes
#' fetch_bddk1(2020, 3, 1, grup_kod = c(10001, 10002))
#'
#' # Turkish language output
#' fetch_bddk1(2020, 3, 1, grup_kod = 10001, lang = "tr")
#'
#' @seealso [fetch_finturk1()] for quarterly province-level data.
#' @importFrom rlang :=
#' @export
fetch_bddk1 <- function(year, month, table_no, grup_kod = 10001,
                        currency = "TL", lang = "en") {
  base_url <- sprintf("https://www.bddk.org.tr/BultenAylik/%s/Home/BasitRaporGetir", lang)
  
  # Build parameters exactly as Python does
  params <- list(
    tabloNo = as.character(table_no),
    yil = as.character(year),
    ay = as.character(month),
    paraBirimi = currency
  )
  params
  
    # Add each grup_kod
  for (kod in grup_kod) {
    params <- c(params, list(taraf = as.character(kod)))
  }
  params
  # httr2 handles this format correctly
  req <- httr2::request(base_url) |>
    httr2::req_body_form(!!!params) |>
    httr2::req_timeout(30)
  
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop(sprintf("HTTP error: %s", e$message))
  )
  
  parsed <- jsonlite::fromJSON(
    httr2::resp_body_string(resp, encoding = "UTF-8"), 
    simplifyVector = FALSE
  )
  
  if (!isTRUE(parsed$success)) stop("API reported unsuccessful request")
  
  df <- parse_json(parsed$Json)

  if (nrow(df) == 0) {
    warning(sprintf("No data for table %d, %d-%02d, grup_kod %s", 
                    table_no, year, month, paste(grup_kod, collapse = ", ")))
    return(data.frame())
  }

  # First column contains group names (text)
  colnames(df)[1] <- "group_name"
  
  # Determine blocks of consecutive identical group names
  group_rle <- rle(df$group_name)
  group_counts <- group_rle$lengths
  group_names <- group_rle$values
  df$grup_kod <- rep(grup_kod, group_rle$lengths)
  
  df$period <- sprintf("%d-%02d", year, month)
  df$currency <- currency
  return(df)
}



#' Fetch multiple period table from BDDK website
#'
#' Fetches BDDK data for a range of months by calling fetch_bddk1 iteratively.
#'
#' @param start_year,end_year Starting/ending year (YYYY).
#' @param start_month,end_month Starting/ending month (1-12).
#' @param table_no Table number to fetch (1-17). No default. 
#' Use \code{\link{list_tables}} with \code{source = "bddk"} to see available options.
#' @param grup_kod Group code (10001-10016). Default 10001. 
#' Use \code{\link{list_groups}} with \code{source = "bddk"} to see available options.
#' @param currency Currency code ("TL" or "USD"). Default "TL".
#' @param lang Language ("en" or "tr"). Default "en".
#' @param delay Delay between requests in seconds. Default 0.5.
#' @param verbose Print progress messages. Default TRUE.
#' @return Combined data frame with "fetch_info" attribute.
#' 
#' @examples
#' \donttest{
#'   # Fetch multiple months
#'   my_dat <- fetch_bddk(2024, 1, 2024, 3, table_no = 15)
#' }
#' 
#' @seealso [fetch_finturk()] for quarterly province-level data.
#' @export
fetch_bddk <- function(start_year, start_month, end_year, end_month, table_no,
                       grup_kod = 10001, currency = "TL", lang = "en",
                       delay = 0.5, verbose = TRUE) {
  
  # Validation and month sequence generation
  if (start_year > end_year || (start_year == end_year && start_month > end_month)) {
    stop("Start date must be before or equal to end date")
  }
  
  month_dates <- seq(
    from = as.Date(paste(start_year, start_month, "01", sep = "-")),
    to = as.Date(paste(end_year, end_month, "01", sep = "-")),
    by = "month"
  )
  
  if (verbose) {
    cat(sprintf("Fetching table %d for %d months: %s to %s\n", 
                table_no, length(month_dates),
                format(min(month_dates), "%Y-%m"),
                format(max(month_dates), "%Y-%m")))
  }
  
  results <- list(); errors <- list()
  for (i in seq_along(month_dates)) {
    year <- as.integer(format(month_dates[i], "%Y"))
    month <- as.integer(format(month_dates[i], "%m"))
    
    if (verbose) cat(sprintf("[%d/%d] %d-%02d... ", i, length(month_dates), year, month))
    
    tryCatch({
      df_month <- fetch_bddk1(year, month, table_no, grup_kod, currency, lang)
      if (nrow(df_month) > 0) {
        results[[length(results) + 1]] <- df_month
        if (verbose) cat(sprintf("%d rows\n", nrow(df_month)))
      } else {
        if (verbose) cat("No data\n")
      }
    }, error = function(e) {
      if (verbose) cat(sprintf("Error: %s\n", e$message))
      errors[[length(errors) + 1]] <- list(year = year, month = month, error = e$message)
    })
    
    if (i < length(month_dates) && delay > 0) Sys.sleep(delay)
  }
  
  if (length(results) == 0) {
    warning("No data was successfully retrieved")
    return(data.frame())
  }
  
  combined_df <- do.call(rbind, results)
  rownames(combined_df) <- NULL
  
  attr(combined_df, "fetch_info") <- list(
    start_date = format(min(month_dates), "%Y-%m"),
    end_date = format(max(month_dates), "%Y-%m"),
    table_no = table_no,
    currency = currency,
    grup_kod = grup_kod,
    lang = lang,
    # months_requested = length(month_dates),
    # months_successful = length(results),
    # months_failed = length(errors),
    errors = errors
  )
  
  combined_df
}


