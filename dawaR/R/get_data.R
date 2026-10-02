#' Get data from section
#'
#' @param as_list Whether or not to return the data as a list or in a dataframe.
#'   Data.frame is the default output.
#'
#' @inheritParams dawa
#'
#' @return Returns either a dataframe with all the data for the specified
#'   section or a list with the same information.
#'
#' @export
#' @family Data functions
#'
#' @examples
#' if (connection_check()) {
#'   x <- get_data("regioner")
#'   head(x)
#' }
get_data <- function(section, as_list = FALSE, ...) {
  warn_api_deprecation()

  if (!section %in% available_sections()) {
    cli::cli_abort(
      "You have provided section {.var {section}}.
                   It is not available through the api"
    )
  }

  params <- rlang::list2(...)

  if (!connection_check()) {
    cli::cli_alert_warning(
      "You do not have access to api.dataforsyningen.dk.
        Please check your connection settings."
    )
    return(NULL) # Exit early if no connection is detected
  }

  response <- dawa(
    section = section,
    verbose = FALSE,
    struktur = "flad",
    func_params = params
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (as_list == TRUE) {
    output <- lapply(response, function(x) {
      x[1] <- as.numeric(x[1]) # Convert the first element to double
      x[2] <- as.numeric(x[2]) # Convert the second element to double
      return(x)
    })
  } else if (as_list == FALSE) {
    df <- do.call(rbind.data.frame, response)

    if ("kode" %in% colnames(df)) {
      df[["kode"]] <- as.double(df[["kode"]])
    }

    if ("dagi_id" %in% colnames(df)) {
      df[["dagi_id"]] <- as.double(df[["dagi_id"]])
    }

    if ("nummer" %in% colnames(df)) {
      df[["nummer"]] <- as.double(df[["nummer"]])
    }

    output <- df
  }

  return(output)
}
