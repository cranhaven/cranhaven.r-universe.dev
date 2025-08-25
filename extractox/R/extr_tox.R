#' Extract Toxicological Information from Multiple Databases
#'
#' This wrapper function retrieves toxicological information for specified chemicals
#' by calling several external functions to query multiple databases, including PubChem,
#' the Integrated Chemical Environment (ICE), CompTox Chemicals Dashboard,
#' and the Integrated Risk Information System (IRIS) and other.
#'
#' Specifically, this function:
#'   \itemize{
#'     \item Calls \code{\link{extr_monograph}} to return monographs informations
#'        from WHO IARC.
#'     \item Calls \code{\link{extr_pubchem_ghs}} to retrieve GHS classification
#'         data from PubChem.
#'     \item Calls \code{\link{extr_ice}} to gather assay data from the ICE database.
#'     \item Calls \code{\link{extr_iris}} to retrieve risk assessment information
#'         from the IRIS database.
#'     \item Calls \code{\link{extr_comptox}} to retrieve data from the CompTox
#'        Chemicals Dashboard.
#'   }
#' @param casrn A character vector of CAS Registry Numbers (CASRN) representing
#'    the chemicals of interest.
#' @param verbose A logical value indicating whether to print detailed messages.
#'     Default is TRUE.
#' @param force Logical indicating whether to force a fresh download of the EPA
#'    PPRTV database. Default is TRUE.
#' @param delay Numeric value indicating the delay in seconds between requests
#'    to avoid overwhelming the server. Default is 3 seconds.
#' @return A list of data frames containing toxicological information retrieved
#'   from each database:
#'   \describe{
#'     \item{who_iarc_monographs}{Lists if any, the WHO IARC monographs related
#'       to that chemical.}
#'     \item{pprtv}{Risk assessment data from the EPA PPRTV}
#'     \item{ghs_dat}{Toxicity data from PubChem's Globally Harmonized System (GHS)
#'       classification.}
#'     \item{ice_dat}{Assay data from the Integrated Chemical Environment (ICE)
#'       database.}
#'     \item{iris}{Risk assessment data from the IRIS database.}
#'     \item{comptox_list}{List of dataframe with toxicity information
#'       from the CompTox Chemicals Dashboard.}
#'   }
#' @export
#' @examples
#' \donttest{
#' condathis::with_sandbox_dir({ # this is to write on tempdir as for CRAN policies # nolint
#'   Sys.sleep(4) # To avoid overwhelming the server
#'   extr_tox(casrn = c("100-00-5", "107-02-8"), delay = 4)
#' })
#' }
extr_tox <- function(casrn, verbose = TRUE, force = TRUE, delay = 2) {
  if (base::missing(casrn)) {
    cli::cli_abort("The argument {.field {casrn}} is required.")
  }

  ghs_dat <- extr_pubchem_ghs(casrn, verbose = verbose)

  comptox_list <- extr_comptox(casrn, verbose = verbose)

  ice_dat <- extr_ice(
    casrn = casrn,
    assays = NULL,
    verbose = verbose
  )

  iris_filt <- extr_iris(casrn = casrn, verbose = verbose, delay = delay)

  extracted_monographs <- extr_monograph(
    ids = casrn, search_type = "casrn",
    verbose = verbose
  )

  Sys.sleep(delay)

  extracted_pprtv <- extr_pprtv(ids = casrn, verbose = verbose)

  list_1 <- list(
    who_iarc_monographs = extracted_monographs,
    pprtv = extracted_pprtv,
    ghs_dat = ghs_dat,
    iris = iris_filt,
    ice = ice_dat
  )
  out <- c(list_1, comptox_list)
  out
}
