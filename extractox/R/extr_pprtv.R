#' Extract Data from EPA PPRTVs
#'
#' Extracts data for specified identifiers (CASRN or chemical names) from the
#' EPA's Provisional Peer-Reviewed Toxicity Values (PPRTVs) database. The
#' function retrieves and processes data, with options to use cached files
#' or force a fresh download.
#'
#' @param ids Character vector of identifiers to search (e.g., CASRN or chemical
#'   names).
#' @param search_type Character string specifying the type of identifier:
#'   "casrn" or "name". Default is "casrn". If `search_type` is "name", the
#'   function performs a partial match search for the chemical name. NOTE:
#'   Since partial mached is use, multiple seraches might match the same
#'   chemical, therefore chemical ids might not be  uniques.
#' @param verbose Logical indicating whether to display progress messages.
#'   Default is TRUE.
#' @param force Logical indicating whether to force a fresh download of the
#'   database. Default is TRUE.
#' @param get_all Logical. If TRUE ignore all the other ignore `ids`,
#'   `search_type`, set  `force = TRUE` and get the all dataset.
#'   This is was introduced for debugging purposes.
#' @return A data frame with extracted information matching the specified
#'   identifiers, or NULL if no matches are found.
#' @seealso \href{https://www.epa.gov/pprtv/provisional-peer-reviewed-toxicity-values-pprtvs-assessments}{EPA PPRTVs} # nolint
#' @export
#' @examples
#' \donttest{
#' condathis::with_sandbox_dir({ # this is to write on tempdir as for CRAN policies # nolint
#'
#'   # Extract data for a specific CASRN
#'   Sys.sleep(4) # Sleep to avoid overwhelming the server
#'   extr_pprtv(ids = "107-02-8", search_type = "casrn", verbose = TRUE)
#'
#'   Sys.sleep(4) # Sleep to avoid overwhelming the server
#'   # Extract data for a chemical name
#'   out <- extr_pprtv(
#'     ids = "Acrolein", search_type = "name", verbose = TRUE,
#'     force = TRUE
#'   )
#'   print(out)
#'
#'   Sys.sleep(3) # Sleep to avoid overwhelming the server
#'   # Extract data for multiple identifiers
#'   out2 <- extr_pprtv(
#'     ids = c("107-02-8", "79-10-7", "42576-02-3"),
#'     search_type = "casrn",
#'     verbose = TRUE,
#'     force = TRUE
#'   )
#'   print(out2)
#' })
#' }
extr_pprtv <- function(ids, search_type = "casrn", verbose = TRUE, force = TRUE,
                       get_all = FALSE) {
  if (all(base::missing(ids), !isTRUE(get_all))) {
    cli::cli_abort("The argument {.field ids} is required.")
  }

  if (all(!search_type %in% c("casrn", "name"), !isTRUE(get_all))) {
    cli::cli_abort("The argument {.field search_type} needs to be either `casrn`
                   or `name`.")
  }

  file_name <- "epa_pprtvs.rds" # Filename for caching

  # Check if path is present otherwise it download it again
  full_path_cache_file <- file.path(
    tools::R_user_dir("extractox",
      which = "cache"
    ),
    file_name
  )

  cache_present <- file.exists(full_path_cache_file)

  full_path_cache_file <- normalizePath(full_path_cache_file, mustWork = FALSE)

  # dir.exists(Sys.getenv("R_USER_CACHE_DIR"))

  if (any(isTRUE(force), !cache_present, isTRUE(get_all))) {
    check_internet(verbose = verbose)
    dat <- download_db(
      url = "https://cfpub.epa.gov/ncea/pprtv/atoz.cfm",
      url_query_param = list(excel = "yes"),
      file_name = file_name,
      verbose = verbose
    )

    # get eveything
    if (get_all == TRUE) {
      return(dat)
    }
    path_cache <- save_to_cache(dat, file_name, verbose = verbose)
  } else {
    dat <- read_from_cache(file_name = file_name, verbose = verbose)

    if (isTRUE(verbose)) {
      date_download <- dat$date_downloaded[1]
      cli::cli_alert_info("Cache date {.strong {date_download}}.")
      cli::cli_alert_info("Set `force = TRUE` to force download from EPA.")
    }
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Extracting EPA PPRTVs.")
  }

  col_names <- c(
    "pprtv_substance_id", "chemical", "casrn", "last_revision",
    "pprtv_assessment", "iris_link", "rf_c_value", "rf_d_value",
    "woe", "date_downloaded", "query"
  )

  out <- search_and_match(
    dat = dat,
    ids = ids,
    search_type = search_type,
    col_names = col_names,
    chemical_col = "chemical"
  )

  check_na_warn(dat = out, col_to_check = "chemical", verbose = verbose)

  out
}
