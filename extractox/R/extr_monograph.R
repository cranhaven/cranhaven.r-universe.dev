#' Retrieve WHO IARC Monograph Information
#'
#' This function returns information regarding Monographs from the World Health
#' Organization (WHO) International Agency for Research on Cancer (IARC)  based on
#' CAS Registry Number or Name of the chemical. Note that the data is not fetched
#' dynamically from the website, but has retrieved and  copy hasbeen saved as
#' internal data in the package.
#'
#' @param search_type A character string specifying the type of search to
#'   perform. Valid options are "casrn" (CAS Registry Number) and "name"
#' .  (name of the chemical). If `search_type` is "casrn", the function filters
#' .  by the CAS Registry Number.
#'   If `search_type` is "name", the function performs a partial match search
#'   for the chemical name.
#' @param ids A character vector of IDs to search for.
#' @param verbose A logical value indicating whether to print detailed messages.
#' .   Default is TRUE.
#' @param get_all Logical. If TRUE ignore all the other ignore `ids`,
#'   `search_type`, set  `force = TRUE` and get the all dataset.
#'   This is was introduced for debugging purposes.
#' @return A data frame containing the relevant information from the WHO IARC,
#' .  including Monograph `volume`, `volume_publication_year`, `evaluation_year`,
#' .  and `additional_information` where the chemical was described.
#' @seealso \url{https://monographs.iarc.who.int/list-of-classifications/}
#' @export
#' @examples
#' {
#'   dat <- extr_monograph(search_type = "casrn", ids = c("105-74-8", "120-58-1"))
#'   str(dat)
#'
#'   # Example usage for name search
#'   dat2 <- extr_monograph(
#'     search_type = "name",
#'     ids = c("Aloe", "Schistosoma", "Styrene")
#'   )
#'   str(dat2)
#' }
extr_monograph <- function(ids,
                           search_type = "casrn",
                           verbose = TRUE,
                           get_all = FALSE) {
  if (isTRUE(get_all)) {
    return(who_iarc_monographs)
  }

  if (base::missing(ids)) {
    cli::cli_abort("The argument {.field ids} is required.")
  }

  if (!search_type %in% c("casrn", "name")) {
    cli::cli_abort("The argument {.field search_type} needs to be either `casrn`
                   or `name`.")
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Extracting WHO IARC monographs...\nLast updated: 2024-11-29 5:08pm (CET)")
  }

  col_names <- c(names(who_iarc_monographs), "query")

  out <- search_and_match(
    dat = who_iarc_monographs,
    ids = ids,
    search_type = search_type,
    col_names = col_names,
    chemical_col = "agent"
  )

  check_na_warn(dat = out, col_to_check = "agent", verbose = verbose)

  out
}
