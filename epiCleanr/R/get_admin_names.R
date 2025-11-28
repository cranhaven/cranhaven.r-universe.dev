#' Retrieve Administrative Names from GeoNames
#'
#' This function grabs administrative region names (such as districts,
#' provinces, etc.) for a given country from the `GeoNames` website. It accepts
#' both country names and various country coding schemes.
#'
#' @param country_name_or_code Character or numeric. The name or code of the
#' country for which administrative names are to be retrieved. This can be in
#' various formats such as country name, ISO codes, UN codes, etc., see
#' `countrycode::codelist()` for the full list of codes and naming conventions
#' used.
#' @param silent_mode A logical indicating whether to suppress messages.
#'         Default is TRUE.
#'
#' @return A list containing administrative region names and details for
#'         different administrative levels (e.g., ADM1, ADM2, etc.). Each
#'         element of the list corresponds to a different administrative level
#'         and contains a data frame with columns such as country_code,  ascii
#'         name, alternate names, latitude, longitude, and date last updated.
#'
#' @examples
#' \donttest{
#' # example using different naming/code conventions
#' three_digit <- get_admin_names("TGO")   # using 3 digit iso codes
#' two_digit <- get_admin_names("TG")      # using 2 digit iso codes
#' un_code <- get_admin_names(768)         # using UN codes
#' full_name <-  get_admin_names("Togo")   # using full names
#'
#'str(full_name$adm2)
#'}
#'
#' @seealso `Geonames` website for the source of admin
#' names data
#'
#' @importFrom dplyr select filter mutate all_of
#' @importFrom tidyr pivot_wider
#' @importFrom utils download.file unzip
#' @importFrom rlang .data
#' @importFrom withr with_tempfile
#'
#' @export

get_admin_names <- function(country_name_or_code, silent_mode = TRUE) {

  # Convert name/code to ISO code to use for districtr name data----------------

  char_coding_schemes <- c(
    'cctld', 'country.name', 'country.name.de', 'country.name.fr',
    'country.name.it', 'genc2c', 'genc3c', 'iso2c', 'iso3c'
  )

  num_coding_schemes <- c(
    'cowc', 'cown', 'dhs', 'ecb', 'eurostat', 'fao', 'fips',
    'gaul', 'genc3n', 'gwc', 'gwn', 'imf', 'ioc', 'iso3n', 'p5n',
    'p5c', 'p4n', 'p4c', 'unicode.symbol', 'unhcr', 'unpd', 'vdem',
    'wb', 'wvs', "un"
  )

  # Choose the appropriate coding schemes based on the type of input
  if (is.character(country_name_or_code)) {
    coding_schemes <- char_coding_schemes
  } else {
    coding_schemes <- c(char_coding_schemes, num_coding_schemes)
  }

  # Convert country name or code to ISO-3166 2-letter country code
  iso_code <- NA
  for (scheme in coding_schemes) {
    iso_code <- suppressWarnings(
      countrycode::countrycode(
        country_name_or_code,
        origin = scheme, destination = "iso2c"))
    if (!is.na(iso_code)) break
  }

  if (is.na(iso_code)) {
    stop("Country name or code not recognized.")
  }

  # retrieve admin names from geonames website ---------------------------------


  tmpdir <- tempfile()
  dir.create(tmpdir)


  withr::with_dir(tmpdir, {
    # Construct the URL for the requested ISO code
    url <- paste0(
      "https://download.geonames.org/export/dump/", iso_code, ".zip")

    # Set the timeout option
    # old_options <- options(timeout = 320)

    # Download the ZIP file
    download.file(url, "geonames.zip", quiet = silent_mode, mode = "wb")

    # Unzip the file to a subdirectory
    unzip("geonames.zip", exdir = "geonames")


    txt_file <- paste0("geonames/", iso_code, ".txt")

    data <- epiCleanr::import(txt_file)

  })

  # Remove the temporary directory and its contents
  unlink(tmpdir, recursive = TRUE)

  # Select required columns
  data <- data |>
    dplyr::select(
      "country_code" = "V9",
      "asciiname" = "V3", "alternatenames" = "V4",
      "latitude" = "V5", "longitude" = "V6",
      "feature_code" = "V8", "name" = "V2",
      "admin1_code" = "V11", "admin2_code" = "V12",
      "admin3_code" = "V13", "admin4_code" = "V14",
      "last_updated" = "V19"
    )

  # Manipulate datasets
  state_region_names <- c("ADM1", "ADM1H", "ADM2", "ADM2H", "ADM3",
                          "ADM3H", "ADM4", "ADM4H")

  data <- data |>
    dplyr::filter(.data$feature_code %in% state_region_names) |>
    tidyr::pivot_wider(values_from = "name", names_from = "feature_code") |>
    dplyr::mutate(
      alternatenames = stringr::str_replace_all(
        .data$alternatenames, ",", ", "),
      alternatenames = stringr::str_replace_all(
        .data$alternatenames, " , ", ","),
      alternatenames = stringr::str_replace_all(
        .data$alternatenames, '"', "")
    )

  # Create empty list
  admin_names <- list()

  # Select admin data
  for (i in 1:4) {
    adm_column <- paste0("ADM", i)
    if (adm_column %in% colnames(data)) {
      admin_names[[paste0("adm", i)]] <- data |>
        dplyr::select(
          "country_code", "asciiname", "alternatenames",
          dplyr::all_of(adm_column),
          "latitude", "longitude", "last_updated") |>
        dplyr::filter(!is.na(.data[[adm_column]])) |>
        as.data.frame() |>
        dplyr::rename_with(tolower)
    }
  }

  return(admin_names)

}
