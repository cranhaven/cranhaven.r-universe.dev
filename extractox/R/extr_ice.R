#' Extract Data from NTP ICE Database
#'
#' The `extr_ice` function sends a POST request to the ICE API to search for
#' information based on specified chemical IDs and assays.
#'
#' @param casrn A character vector specifying the CASRNs for the search.
#' @param assays A character vector specifying the assays to include in the
#'    search. Default is NULL, meaning all assays are included. If you don't
#'    know the exact assay name, you can use the `extr_ice_assay_names()`
#'    function to search for assay names that match a pattern you're interested in.
#' @param verify_ssl Boolean to control of SSL should be verified or not.
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is TRUE.
#' @param ... Any other arguments to be supplied to `req_option` and
#'    thus to `libcurl`.
#' @return A data frame containing the extracted data from the ICE API.
#' @seealso
#' \code{\link{extr_ice_assay_names}},
#' \href{https://ice.ntp.niehs.nih.gov/}{NTP ICE database}
#' @export
#' @examples
#' \donttest{
#' extr_ice(casrn = c("50-00-0"))
#' }
extr_ice <- function(casrn,
                     assays = NULL,
                     verify_ssl = FALSE,
                     verbose = TRUE,
                     ...) {
  if (base::missing(casrn)) {
    cli::cli_abort("The argument {.field {casrn}} is required.")
  }

  # Check if online
  base_url <- "https://ice.ntp.niehs.nih.gov/api/v1/search"
  # check_internet()
  # Unfortunatelly this would fail see #7
  check_internet(verbose = verbose)

  # Perform the request and get a response
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Sending request to ICE database...")
  }

  libcurl_opt <- set_ssl(verify_ssl = verify_ssl, other_opt = ...)

  resp <- tryCatch(
    {
      httr2::request(base_url) |>
        httr2::req_retry(max_tries = 2, backoff = ~3) |>
        httr2::req_body_json(list(chemids = casrn, assays = assays),
          auto_unbox = FALSE
        ) |>
        httr2::req_options(!!!libcurl_opt) |>
        httr2::req_perform()
    },
    error = function(e) {
      cli::cli_abort("Failed to perform the request: {conditionMessage(e)}")
    }
  )

  check_status_code(resp, verbose = verbose)

  # This is used in case no results are retrieved in next chunk
  col_names <- c(
    "assay", "endpoint", "substance_type", "casrn", "qsar_ready_id",
    "value", "unit", "species", "receptor_species", "route", "sex",
    "strain", "life_stage", "tissue", "lesion", "location",
    "assay_source", "in_vitro_assay_format", "reference",
    "reference_url", "dtxsid", "substance_name", "pubmed_id"
  )

  out <- stats::setNames(
    as.data.frame(matrix(ncol = length(col_names), nrow = 0)),
    col_names
  )

  # Parse the JSON content
  content <- tryCatch(
    {
      content <- httr2::resp_body_json(resp)
      content
    },
    error = function(e) {
      if (grepl("Unexpected content type \"text/plain\"", e$message)) {
        if (isTRUE(verbose)) {
          cli::cli_warn("It seems that the ids were not found in ICE:
                        {conditionMessage(e)}")
        }
        NULL # Or another suitable value
      } else {
        cli::cli_abort("An unexpected error occurred: {conditionMessage(e)}")
      }
    }
  )

  # if nothing is retrieved
  if (is.null(content)) {
    dat <- out
  } else {
    # Extract and combine data from the response
    dat <- tryCatch(
      {
        do.call(rbind, content$endPoints) |>
          as.data.frame()
      },
      error = function(e) {
        cli::cli_abort("Failed to parse the JSON content: {conditionMessage(e)}")
      }
    )
  }

  out <- data.frame(lapply(dat, unlist))
  names(out) <- col_names

  ids_not_found <- casrn[!casrn %in% out$casrn]
  ids_founds <- casrn[casrn %in% out$casrn]

  if (nrow(out) > 0) {
    out[, "query"] <- paste(ids_founds, collapse = ", ")
    to_add <- stats::setNames(
      data.frame(matrix(
        ncol = ncol(out),
        nrow = length(ids_not_found)
      )),
      names(out)
    )
    to_add$query <- ids_not_found
    out <- rbind(to_add, out)
  } else {
    out[1:length(casrn), "query"] <- casrn
  }

  if (all(length(ids_not_found) > 0, isTRUE(verbose))) {
    cli::cli_warn("Chemical{?s} {.field {ids_not_found}} not found!")
  }

  out
}


#' Extract Assay Names from the ICE Database
#'
#' This function allows users to search for assay names in the ICE database
#' using a regular expression. If no search pattern is provided (`regex = NULL`),
#' it returns all available assay names.
#'
#' @param regex A character string containing the regular expression to search for,
#' or `NULL` to retrieve all assay names.
#' @param verbose A logical value indicating whether to print detailed messages.
#'  Default is TRUE.
#' @return A character vector of matching assay names.
#' @export
#' @examples
#' \donttest{
#' extr_ice_assay_names("OPERA")
#' extr_ice_assay_names(NULL)
#' extr_ice_assay_names("Vivo")
#' }
extr_ice_assay_names <- function(regex = NULL, verbose = TRUE) {
  if (is.null(regex)) {
    cli::cli_alert_info("Returning all available assay names from the ICE database.")
    return(all_ice_assays)
  }

  if (!is.character(regex) || length(regex) != 1) {
    cli::cli_abort("Please provide a single valid regular expression as a character
                    string, or NULL to retrieve all assay names.")
  }

  # Search for matches
  matches <- grep(regex, all_ice_assays, value = TRUE)

  if (all(length(matches) == 0, isTRUE(verbose))) {
    cli::cli_warn("No assay names found matching the search pattern {.field regex}.\n
                  Please note that searches are case sensitive.")
  }

  matches
}
