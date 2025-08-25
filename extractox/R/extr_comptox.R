#' Download and Extract Data from CompTox Chemistry Dashboard
#'
#' This function interacts with the CompTox Chemistry Dashboard to download and
#' extract a wide range of chemical data based on user-defined search criteria.
#' It allows for flexible input types and supports downloading various chemical
#' properties, identifiers, and predictive data. It was inspired by the
#' `ECOTOXr::websearch_comptox` function.
#'
#' @param ids A character vector containing the items to be searched within the
#'   CompTox Chemistry Dashboard. These can be chemical names, CAS Registry
#'   Numbers (CASRN), InChIKeys, or DSSTox substance identifiers (DTXSID).
#' @param download_items A character vector of items to be downloaded.
#'   This includes a comprehensive set of chemical properties, identifiers,
#'   predictive data, and other relevant information. By Default, it downloads
#'   all the info.
#' \describe{
#'   \item{CASRN}{The Chemical Abstracts Service Registry Number, a unique
#'      numerical identifier for chemical substances.}
#'   \item{INCHIKEY}{The hashed version of the full International Chemical
#'      Identifier (InChI) string.}
#'   \item{IUPAC_NAME}{The International Union of Pure and Applied Chemistry
#'      (IUPAC) name of the chemical.}
#'   \item{SMILES}{The Simplified Molecular Input Line Entry System (SMILES)
#'      representation of the chemical structure.}
#'   \item{INCHI_STRING}{The full International Chemical Identifier (InChI)
#'      string.}
#'   \item{MS_READY_SMILES}{The SMILES representation of the chemical structure,
#'      prepared for mass spectrometry analysis.}
#'   \item{QSAR_READY_SMILES}{The SMILES representation of the chemical
#'      structure, prepared for quantitative structure-activity relationship
#'      (QSAR) modeling.}
#'   \item{MOLECULAR_FORMULA}{The chemical formula representing the number and
#'      type of atoms in a molecule.}
#'   \item{AVERAGE_MASS}{The average mass of the molecule, calculated based on
#'      the isotopic distribution of the elements.}
#'   \item{MONOISOTOPIC_MASS}{The mass of the molecule calculated using the most
#'      abundant isotope of each element.}
#'   \item{QC_LEVEL}{The quality control level of the data.}
#'   \item{SAFETY_DATA}{Safety information related to the chemical.}
#'   \item{EXPOCAST}{Exposure predictions from the EPA's ExpoCast program.}
#'   \item{DATA_SOURCES}{Sources of the data provided.}
#'   \item{TOXVAL_DATA}{Toxicological values related to the chemical.}
#'   \item{NUMBER_OF_PUBMED_ARTICLES}{The number of articles related to the
#'      chemical in PubMed.}
#'   \item{PUBCHEM_DATA_SOURCES}{Sources of data from PubChem.}
#'   \item{CPDAT_COUNT}{The number of entries in the Chemical and Product
#'      Categories Database (CPDat).}
#'   \item{IRIS_LINK}{Link to the EPA's Integrated Risk Information System
#'     (IRIS) entry for the chemical.}
#'   \item{PPRTV_LINK}{Link to the EPA's Provisional Peer-Reviewed Toxicity
#'     Values (PPRTV) entry for the chemical.}
#'   \item{WIKIPEDIA_ARTICLE}{Link to the Wikipedia article for the chemical.}
#'   \item{QC_NOTES}{Notes related to the quality control of the data.}
#'   \item{ABSTRACT_SHIFTER}{Information related to the abstract shifter.}
#'   \item{TOXPRINT_FINGERPRINT}{The ToxPrint chemoinformatics fingerprint of
#'      the chemical.}
#'   \item{ACTOR_REPORT}{The Aggregated Computational Toxicology Resource
#'      (ACTOR) report for the chemical.}
#'   \item{SYNONYM_IDENTIFIER}{Identifiers for synonyms of the chemical.}
#'   \item{RELATED_RELATIONSHIP}{Information on related chemicals.}
#'   \item{ASSOCIATED_TOXCAST_ASSAYS}{Assays associated with the chemical in
#'      the ToxCast database.}
#'   \item{TOXVAL_DETAILS}{Details of toxicological values.}
#'   \item{CHEMICAL_PROPERTIES_DETAILS}{Details of the chemical properties.}
#'   \item{BIOCONCENTRATION_FACTOR_TEST_PRED}{Predicted bioconcentration factor
#'      from tests.}
#'   \item{BOILING_POINT_DEGC_TEST_PRED}{Predicted boiling point in degrees
#'      Celsius from tests.}
#'   \item{48HR_DAPHNIA_LC50_MOL/L_TEST_PRED}{Predicted 48-hour LC50 for Daphnia
#'      in mol/L from tests.}
#'   \item{DENSITY_G/CM^3_TEST_PRED}{Predicted density in g/cm³ from tests.}
#'   \item{DEVTOX_TEST_PRED}{Predicted developmental toxicity from tests.}
#'   \item{96HR_FATHEAD_MINNOW_MOL/L_TEST_PRED}{Predicted 96-hour LC50 for
#'       fathead minnow in mol/L from tests.}
#'   \item{FLASH_POINT_DEGC_TEST_PRED}{Predicted flash point in degrees Celsius
#'      from tests.}
#'   \item{MELTING_POINT_DEGC_TEST_PRED}{Predicted melting point in degrees
#'      Celsius from tests.}
#'   \item{AMES_MUTAGENICITY_TEST_PRED}{Predicted Ames mutagenicity from tests.}
#'   \item{ORAL_RAT_LD50_MOL/KG_TEST_PRED}{Predicted oral LD50 for rats in
#'      mol/kg from tests.}
#'   \item{SURFACE_TENSION_DYN/CM_TEST_PRED}{Predicted surface tension in dyn/cm
#'      from tests.}
#'   \item{THERMAL_CONDUCTIVITY_MW_M×K_TEST_PRED}{Predicted thermal conductivity
#'      in mW/m×K from tests.}
#'   \item{TETRAHYMENA_PYRIFORMIS_IGC50_MOL/L_TEST_PRED}{Predicted IGC50 for
#'      Tetrahymena pyriformis in mol/L from tests.}
#'   \item{VISCOSITY_CP_CP_TEST_PRED}{Predicted viscosity in cP from tests.}
#'   \item{VAPOR_PRESSURE_MMHG_TEST_PRED}{Predicted vapor pressure in mmHg from
#'      tests.}
#'   \item{WATER_SOLUBILITY_MOL/L_TEST_PRED}{Predicted water solubility in mol/L
#'      from tests.}
#'   \item{ATMOSPHERIC_HYDROXYLATION_RATE_\(AOH\)_CM3/MOLECULE\*SEC_OPERA_PRED}{Predicted # nolint
#'      atmospheric hydroxylation rate in cm³/molecule\*sec from OPERA.}
#'   \item{BIOCONCENTRATION_FACTOR_OPERA_PRED}{Predicted bioconcentration factor
#'      from OPERA.}
#'   \item{BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED}{Predicted biodegradation # nolint
#'      half-life in days from OPERA.}
#'   \item{BOILING_POINT_DEGC_OPERA_PRED}{Predicted boiling point in degrees
#'      Celsius from OPERA.}
#'   \item{HENRYS_LAW_ATM-M3/MOLE_OPERA_PRED}{Predicted Henry's law constant in
#'      atm-m³/mole from OPERA.}
#'   \item{OPERA_KM_DAYS_OPERA_PRED}{Predicted Km in days from OPERA.}
#'   \item{OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED}{Predicted octanol-air
#'      partition coefficient (log Koa) from OPERA.}
#'   \item{SOIL_ADSORPTION_COEFFICIENT_KOC_L/KG_OPERA_PRED}{Predicted soil
#'      adsorption coefficient (Koc) in L/kg from OPERA.}
#'   \item{OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED}{Predicted octanol-water
#'      partition coefficient (log P) from OPERA.}
#'   \item{MELTING_POINT_DEGC_OPERA_PRED}{Predicted melting point in degrees
#'      Celsius from OPERA.}
#'   \item{OPERA_PKAA_OPERA_PRED}{Predicted pKa (acidic) from OPERA.}
#'   \item{OPERA_PKAB_OPERA_PRED}{Predicted pKa (basic) from OPERA.}
#'   \item{VAPOR_PRESSURE_MMHG_OPERA_PRED}{Predicted vapor pressure in mmHg from
#'      OPERA.}
#'   \item{WATER_SOLUBILITY_MOL/L_OPERA_PRED}{Predicted water solubility in mol/L # nolint
#'      from OPERA.}
#'   \item{EXPOCAST_MEDIAN_EXPOSURE_PREDICTION_MG/KG-BW/DAY}{Predicted median
#'      exposure from ExpoCast in mg/kg-bw/day.}
#'   \item{NHANES}{National Health and Nutrition Examination Survey data.}
#'   \item{TOXCAST_NUMBER_OF_ASSAYS/TOTAL}{Number of assays in ToxCast.}
#'   \item{TOXCAST_PERCENT_ACTIVE}{Percentage of active assays in ToxCast.}
#' }
#' @param mass_error Numeric value indicating the mass error tolerance for
#'   searches involving mass data. Default is `0`. Not used if libcurl depends
#'   on OpenSSL.
#' @param verify_ssl Logical value indicating whether SSL certificates should be
#'   verified. Default is `FALSE`. Not used if libcurl depends on OpenSSL.
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is TRUE.
#' @param delay Number of seconds to delay between the initial request and the
#'   subsequent request to download the Excel file.
#' @param ... Additional arguments passed to `httr2::req_options()`. Not used if
#'   libcurl depends on OpenSSL.
#' @details This function is designed to handle potential connection issues with
#'   EPA servers on Linux systems. These servers may not support modern security
#'   protocols (unsafe legacy renegotiation), causing errors with newer versions
#'   of `libcurl` when linked with `OpenSSL`.
#'   To ensure reliability, the function automatically detects if your system's
#'   `libcurl` is likely to be affected. If so, it uses the `{condathis}`
#'   package to download and run the request with a known-compatible version of
#'   `curl` (`7.78.0`).
#' @seealso \href{https://www.epa.gov/comptox-tools/comptox-chemicals-dashboard-resource-hub}{CompTox # nolint
#'   Chemicals Dashboard Resource Hub}
#' @return A cleaned data frame containing the requested data from CompTox.
#' @export
#' @examples
#' \donttest{
#' # Example usage of the function:
#' extr_comptox(ids = c("Aspirin", "50-00-0"))
#' }
extr_comptox <- function(ids,
                         download_items = c(
                           "CASRN",
                           "INCHIKEY",
                           "IUPAC_NAME",
                           "SMILES",
                           "INCHI_STRING",
                           "MS_READY_SMILES",
                           "QSAR_READY_SMILES",
                           "MOLECULAR_FORMULA",
                           "AVERAGE_MASS",
                           "MONOISOTOPIC_MASS",
                           "QC_LEVEL",
                           "SAFETY_DATA",
                           "EXPOCAST",
                           "DATA_SOURCES",
                           "TOXVAL_DATA",
                           "NUMBER_OF_PUBMED_ARTICLES",
                           "PUBCHEM_DATA_SOURCES",
                           "CPDAT_COUNT",
                           "IRIS_LINK",
                           "PPRTV_LINK",
                           "WIKIPEDIA_ARTICLE",
                           "QC_NOTES",
                           "ABSTRACT_SHIFTER",
                           "TOXPRINT_FINGERPRINT",
                           "ACTOR_REPORT",
                           "SYNONYM_IDENTIFIER",
                           "RELATED_RELATIONSHIP",
                           "ASSOCIATED_TOXCAST_ASSAYS",
                           "TOXVAL_DETAILS",
                           "CHEMICAL_PROPERTIES_DETAILS",
                           "BIOCONCENTRATION_FACTOR_TEST_PRED",
                           "BOILING_POINT_DEGC_TEST_PRED",
                           "48HR_DAPHNIA_LC50_MOL/L_TEST_PRED",
                           "DENSITY_G/CM^3_TEST_PRED",
                           "DEVTOX_TEST_PRED",
                           "96HR_FATHEAD_MINNOW_MOL/L_TEST_PRED",
                           "FLASH_POINT_DEGC_TEST_PRED",
                           "MELTING_POINT_DEGC_TEST_PRED",
                           "AMES_MUTAGENICITY_TEST_PRED",
                           "ORAL_RAT_LD50_MOL/KG_TEST_PRED",
                           "SURFACE_TENSION_DYN/CM_TEST_PRED",
                           "THERMAL_CONDUCTIVITY_MW/(M*K)_TEST_PRED",
                           "TETRAHYMENA_PYRIFORMIS_IGC50_MOL/L_TEST_PRED",
                           "VISCOSITY_CP_CP_TEST_PRED",
                           "VAPOR_PRESSURE_MMHG_TEST_PRED",
                           "WATER_SOLUBILITY_MOL/L_TEST_PRED",
                           "ATMOSPHERIC_HYDROXYLATION_RATE_(AOH)_CM3/MOLECULE*SEC_OPERA_PRED", # nolint: line_length_linter.
                           "BIOCONCENTRATION_FACTOR_OPERA_PRED",
                           "BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED",
                           "BOILING_POINT_DEGC_OPERA_PRED",
                           "HENRYS_LAW_ATM-M3/MOLE_OPERA_PRED",
                           "OPERA_KM_DAYS_OPERA_PRED",
                           "OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED",
                           "SOIL_ADSORPTION_COEFFICIENT_KOC_L/KG_OPERA_PRED",
                           "OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED",
                           "MELTING_POINT_DEGC_OPERA_PRED",
                           "OPERA_PKAA_OPERA_PRED",
                           "OPERA_PKAB_OPERA_PRED",
                           "VAPOR_PRESSURE_MMHG_OPERA_PRED",
                           "WATER_SOLUBILITY_MOL/L_OPERA_PRED",
                           "EXPOCAST_MEDIAN_EXPOSURE_PREDICTION_MG/KG-BW/DAY",
                           "NHANES",
                           "TOXCAST_NUMBER_OF_ASSAYS/TOTAL",
                           "TOXCAST_PERCENT_ACTIVE"
                         ),
                         mass_error = 0,
                         verify_ssl = FALSE,
                         verbose = TRUE,
                         delay = 7,
                         ...) {
  if (base::missing(ids)) {
    cli::cli_abort("The argument {.field ids} is required.")
  }

  check_internet(verbose = verbose)
  xlsx_file <- tempfile(fileext = ".xlsx")


  base_url <-
    "https://comptox.epa.gov/dashboard-api/batchsearch/export/?lb2ljny4"

  download_items <- c("DTXCID", download_items)

  # Need to downgrade libcurl?
  if (isTRUE(check_need_libcurl_condathis())) {
    condathis_downgrade_libcurl()

    resp <- extr_comptox_openssl_(
      ids = ids,
      download_items = download_items,
      mass_error = mass_error,
      xlsx_file = xlsx_file,
      base_url = base_url,
      verbose = verbose,
      delay = delay
    )
  } else {
    resp <- extr_comptox_(
      ids = ids,
      download_items = download_items,
      mass_error = mass_error,
      verify_ssl = verify_ssl,
      xlsx_file = xlsx_file,
      base_url = base_url,
      verbose = verbose,
      delay = delay,
      ...
    )
  }

  sheet_names <- readxl::excel_sheets(xlsx_file)

  suppressMessages(
    dat_list <- lapply(sheet_names, readxl::read_excel, path = xlsx_file)
  )

  names(dat_list) <- paste0("comptox_", gsub(" ", "_", tolower(sheet_names)))
  unlink(xlsx_file)

  out <- lapply(dat_list, janitor::clean_names)

  # This is to get the ids that were not retrieved
  dat <- out$comptox_main_data

  ids_not_found <- dat$input[grepl("Found 0|fails checksum", dat$found_by)]

  if (all(isTRUE(verbose), length(ids_not_found) != 0)) {
    cli::cli_warn("Chemical{?s} {.field {ids_not_found}} not found!")
  }

  out
}

#' @inherit extr_comptox title description
#' @keywords internal
#' @noRd
#' @inheritParams extr_comptox
#' @param  xlsx_file Path to file to write with results.
#' @param  base_url Comptox url.
extr_comptox_ <- function(ids,
                          download_items = NULL,
                          mass_error = 0,
                          verify_ssl = FALSE,
                          xlsx_file,
                          base_url,
                          verbose = TRUE,
                          delay = 7,
                          ...) {
  libcurl_opt <- set_ssl(verify_ssl = verify_ssl, other_opt = ...)

  identifier_types <- c("chemical_name", "CASRN", "INCHIKEY", "dtxsid")
  input_type <- "IDENTIFIER"

  params <- list(
    identifierTypes = identifier_types,
    massError = mass_error,
    downloadItems = download_items,
    searchItems = paste0(ids, collapse = "\n"),
    inputType = input_type,
    downloadType = "EXCEL"
  )

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Sending request to CompTox...")
  }

  error_result <- NULL

  post_result <- tryCatch(
    {
      httr2::request(base_url) |>
        httr2::req_retry(max_tries = 2, backoff = ~3) |>
        httr2::req_body_json(params) |>
        httr2::req_options(!!!libcurl_opt) |>
        httr2::req_method("POST") |>
        httr2::req_perform()
    },
    error = function(e) {
      error_result <<- e
      NULL
    }
  )
  if (is.null(post_result)) {
    cli::cli_abort("Failed to perform the request: {conditionMessage(error_result)}")
  }


  check_status_code(post_result, verbose = verbose)

  response_body <- httr2::resp_body_string(post_result)

  # base url for download
  base_url_down <-
    "https://comptox.epa.gov/dashboard-api/batchsearch/export/content/"

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Getting info from CompTox...")
  }

  Sys.sleep(delay)

  resp <- tryCatch(
    {
      httr2::request(paste0(base_url_down, response_body)) |>
        httr2::req_options(!!!libcurl_opt) |>
        httr2::req_perform()
    },
    error = function(e) {
      cli::cli_abort("Failed to perform the request: {conditionMessage(e)}")
    }
  )

  check_status_code(resp, verbose = verbose)

  resp |>
    httr2::resp_body_raw() |>
    writeBin(xlsx_file)
}


#' @inherit extr_comptox title description
#' @inheritParams extr_comptox
#' @param  xlsx_file Path to file to write with results.
#' @param  base_url Comptox url.
#' @keywords internal
#' @noRd
extr_comptox_openssl_ <- function(ids,
                                  download_items = NULL,
                                  mass_error = 0,
                                  xlsx_file,
                                  base_url,
                                  verbose = TRUE,
                                  delay = 7) {
  # Create JSON-like string
  json_string <- sprintf(
    '{
    "identifierTypes": ["chemical_name", "CASRN", "INCHIKEY", "dtxsid"],
    "massError": %d,
    "downloadItems": [%s],
    "searchItems": "%s",
    "inputType": "IDENTIFIER",
    "downloadType": "EXCEL"
  }',
    mass_error,
    paste(shQuote(download_items, type = "cmd"), collapse = ", "),
    paste(ids, collapse = "\\n")
    # Double backslashes for R to produce
    # \n in the output
  )


  if (isTRUE(verbose)) {
    cli::cli_alert_info("Sending request to CompTox...")
  }

  # Remove newlines for Windows compatibility
  # check windows
  json_string <- gsub("\n", " ", json_string)

  curl_res <- condathis::run("curl",
    "-X",
    "POST",
    base_url,
    "-H", "User-Agent: httr2/1.0.1 r-curl/5.2.1 libcurl/8.4.0",
    "-H", "Accept: */*",
    "-H", "Accept-Encoding: deflate, gzip",
    "-H", "Content-Type: application/json",
    "-d",
    json_string,
    env_name = "openssl-linux-env",
    verbose = "silent"
  )

  base_url_down <-
    "https://comptox.epa.gov/dashboard-api/batchsearch/export/content/"

  full_url_down <- paste0(base_url_down, curl_res$stdout)

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Getting info from CompTox...")
  }
  Sys.sleep(delay)

  arg_call <- c(
    "-f",
    "-s",
    "-S",
    "-L",
    "--create-dirs",
    "--silent",
    "-o",
    xlsx_file,
    "--compressed"
  )

  other_headers <- c(
    "-H",
    "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:131.0) Gecko/20100101 Firefox/131.0" # nolint
  )

  curl_res_2 <- condathis::run("curl",
    arg_call,
    full_url_down,
    other_headers,
    env_name = "openssl-linux-env",
    error = "continue",
    verbose = "silent"
    # stdout = xlsx_file
  )

  if (curl_res_2$stderr != "") {
    cli::cli_abort("Failed to perform the request: {curl_res_2$stderr}")
  }
}
