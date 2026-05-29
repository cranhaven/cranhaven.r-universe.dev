
#' Fetch Quarterly Data from BDDK FinTurk with Multiple Provinces
#'
#' Retrieves quarterly banking data from the BDDK FinTurk API for specified
#' group codes and provinces. Supports multiple group codes and province
#' codes in a single request.
#'
#' @param year Year as 4-digit integer (YYYY).
#' @param month Month as integer (3,6,9,12 for quarterly data).
#' @param table_no Table number to fetch (1-7). No default.
#' @param grup_kod Group code (10001-1007). Default 10001.
#' @param il plaka (license plate) number (0-81); 999 = Yurt Disi. Default 0.
#'   0=HEPSI (All Cities), 1=Adana, 6=Ankara, 34=Istanbul, 35=Izmir, etc.
#'   See \code{\link{list_cities}} for full list.
#' @return Data frame with a `fetch_info` attribute that contains query details.
#' @details
#' The FinTurk API only provides data for quarter-ending months (March, June,
#'   September, December). Province codes follow Turkey's standard license
#'   plate numbering (1 = Adana, 6 = Ankara, 34 = Istanbul, etc.).
#'
#' @examples
#' # Single group, all provinces
#' fetch_finturk1(2020, 3, 1, grup_kod = 10001)
#'
#' # Multiple groups and specific provinces
#' fetch_finturk1(2020, 3, 1, grup_kod = c(10006, 10007), il = c(6, 34))
#'
#' # Single group, single province
#' fetch_finturk1(2020, 3, 1, grup_kod = 10001, il = 34)
#'
#' @seealso [fetch_bddk1()] for monthly data without province granularity.
#' @importFrom rlang :=
#' @export
fetch_finturk1 <- function(year, month, table_no, 
                          grup_kod = 10001, il = 0) {

  if (!month %in% c(3, 6, 9, 12)) {
    stop("FinTurk requires quarterly months (3, 6, 9, 12)")
  }
  
  grup_kod <- as.character(grup_kod)
  il <- as.numeric(il)
  city_names <- plaka_to_city(il)
  
  # Build the base request
  base_url <- "https://www.bddk.org.tr/BultenFinTurk/tr/Home/VeriGetir"
  req <- httr2::request(base_url) |>
    httr2::req_body_form(
      tabloNo = as.character(table_no),
      donem = sprintf("%d-%d", year, month)
    )
  
  # Dynamically add tarafList parameters (grup_kod)
  for (i in seq_along(grup_kod)) {
    param_name <- sprintf("tarafList[%d]", i-1)
    req <- httr2::req_body_form(req, !!param_name := grup_kod[i])
  }
  
  # Dynamically add sehirList parameters (cities)
  for (i in seq_along(city_names)) {
    param_name <- sprintf("sehirList[%d]", i-1)
    req <- httr2::req_body_form(req, !!param_name := city_names[i])
  }
  
  # Add headers and timeout (keep your existing code)
  req <- req |>
    httr2::req_headers(
      `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
      `X-Requested-With` = "XMLHttpRequest",
      `Referer` = "https://www.bddk.org.tr/BultenFinTurk"
    ) |>
    httr2::req_timeout(30)
  
  # Perform request (keep your existing tryCatch/parsing)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop(sprintf("HTTP error: %s", e$message))
  )

  parsed <- jsonlite::fromJSON(
    httr2::resp_body_string(resp, encoding = "UTF-8"), 
    simplifyVector = FALSE
  )
  
  if (!isTRUE(parsed$success)) {
    warning("API reported unsuccessful request")
    return(data.frame())
  }
  
  # Parse the JSON response
  df <- parse_json(parsed$Json)
  df
  if (nrow(df) == 0) {
    warning(sprintf("No data for table %s, %d-%02d", table_no, year, month))
    return(data.frame())
  }

  colnames(df)[colnames(df) == "Eftodu"] <- "grup_kod"
#  colnames(df)[colnames(df) == "\u015eehir"] <- "il_adi"  # Unicode for S
  s_idx <- grep("^\u015eehir$", colnames(df), ignore.case = TRUE, perl = TRUE)
  colnames(df)[s_idx] <- "il_adi"

  # colnames(df)[1] <- "grup_kod"
  # char_cols <- which(sapply(df, is.character))
  # colnames(df)[char_cols[1]] <- "il_adi"
  
  # Map city name to plaka code
  cities <- get("cities", envir = asNamespace("rbrsa"))
  city_to_plaka <- setNames(cities$plaka, cities$il)
  df$plaka <- city_to_plaka[df$il_adi]

  df$grup_kod <- as.character(df$grup_kod)
  df$period <- sprintf("%d-%02d", year, month)
  df$table_no <- as.character(table_no)
  
  # Enhanced fetch_info attribute
  attr(df, "fetch_info") <- list(
    start_date = sprintf("%d-%02d", year, month),
    end_date = sprintf("%d-%02d", year, month),
    table_no = table_no,
    grup_kod = grup_kod,
    il = il,
    cities = city_names
  )
  
  return(df)
}

#' Fetch multiple period FinTurk data
#'
#' Fetches FinTurk data for a range of quarters by calling fetch_finturk1 iteratively.
#'
#' @param start_year,end_year Starting/ending year (YYYY).
#' @param start_month,end_month Starting/ending month (3,6,9,12).
#' @param table_no Table number to fetch (1-7). No default. 
#' Use \code{\link{list_tables}} with \code{source = "finturk"} to see available options.
#' @param grup_kod Group code (10001-10007). Default 10001. 
#' Use \code{\link{list_groups}} with \code{source = "finturk"} to see available options.
#' @param il plaka (license plate) number (0-81, 99). Default 0.
#' @param delay Delay between requests in seconds. Default 0.5.
#' @param verbose Print progress messages. Default TRUE.
#' @return Combined data frame with "fetch_info" attribute.
#' 
#' @examples
#' \donttest{
#'   # Fetch multiple quarters
#'   my_data <- fetch_finturk(2024, 3, 2024, 9, table_no = 1)
#' }
#' 
#' @seealso [fetch_bddk()] for monthly BRSA data .
#' @export
fetch_finturk <- function(start_year, start_month, end_year, end_month, 
                          table_no, grup_kod = 10001, il = 0,
                          delay = 0.5, verbose = TRUE) {
  
  valid_months <- c(3, 6, 9, 12)
  if (!start_month %in% valid_months || !end_month %in% valid_months) {
    stop("Start and end months must be one of 3,6,9,12 (quarterly)")
  }
  if (!is.numeric(il)) {
    stop("FinTurk requires plaka (license plate) number: 0, 1-81, 999")
  }
  
  # Convert plate to city code for API
  city_name <- plaka_to_city(il)
  
  if (start_year > end_year || (start_year == end_year && start_month > end_month)) {
    stop("Start period must be before or equal to end period")
  }
  
  periods <- list()
  for (year in start_year:end_year) {
    for (month in valid_months) {
      if (year == start_year && month < start_month) next
      if (year == end_year && month > end_month) next
      periods <- c(periods, list(list(year = year, month = month)))
    }
  }
  
  if (verbose) {
    cat(sprintf("Fetching table %d for %d quarterly periods: %d-%02d to %d-%02d\n", 
                table_no, length(periods), start_year, start_month, end_year, end_month))
  }
  
  results <- list(); errors <- list()
  for (i in seq_along(periods)) {
    p <- periods[[i]]
    if (verbose) cat(sprintf("[%d/%d] %d-%02d... ", i, length(periods), p$year, p$month))
    
    tryCatch({
      df <- fetch_finturk1(p$year, p$month, table_no, grup_kod, il)
      if (nrow(df) > 0) {
        results[[length(results) + 1]] <- df
        if (verbose) cat(sprintf("%d rows\n", nrow(df)))
      } else {
        if (verbose) cat("No data\n")
      }
    }, error = function(e) {
      if (verbose) cat(sprintf("Error: %s\n", e$message))
      errors[[length(errors) + 1]] <- list(year = p$year, month = p$month, error = e$message)
    })
    
    if (i < length(periods) && delay > 0) Sys.sleep(delay)
  }
  
  if (length(results) == 0) {
    warning("No data retrieved")
    return(data.frame())
  }
  
  combined_df <- do.call(rbind, results)
  rownames(combined_df) <- NULL
  
  attr(combined_df, "fetch_info") <- list(
    start_date = sprintf("%d-%02d", start_year, start_month),
    end_date = sprintf("%d-%02d", end_year, end_month),
    table_no = table_no,
    grup_kod = grup_kod,
    il = il,
    # periods_requested = length(periods),
    # periods_successful = length(results),
    # periods_failed = length(errors),
    errors = errors
  )
  
  combined_df
}


