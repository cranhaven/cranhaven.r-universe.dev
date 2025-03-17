#' Scrape data from noegletal.dk
#'
#' @param muni_codes Vector of municipality codes
#' @param years Vector of years
#' @param variable_ids Vector of variable IDs
#' @returns HTML content of the response
#' @noRd
noegletal_scrape <- function(muni_codes, years, variable_ids) {
  # From my testing, all these parameters should be provided for the request to
  # 'noegletal.dk' to be valid.
  payload <- list(
    qUdv = 1,
    qUdvBasis = 0,
    qSort = 1,
    qSortBasis = 0,
    qSaveAs = 0,
    qCommand = "Rap",
    qReform = 1,
    qListAar = paste(years, collapse = ","),
    qListKom = paste(muni_codes, collapse = ","),
    qListNog = paste(variable_ids, collapse = ","),
    qPreKom = "Alle kommuner",
    qPreKomNr = 921,
    qPreNog = "x",
    qPreNogNr = 0,
    qPris = 1,
    qPrisBasis = 0,
    qKomInddel = 0,
    qHighlight = 0,
    qHighl_kom = 0,
    qVisKunGns = 0,
    qVers = "24A",
    qHtmlVers = 43,
    qLevel = 1,
    qResVs = 42,
    qUserResol = "X",
    qUserAppl = "X",
    qUserAgent = "X"
  )

  response <- httr::VERB(
    "POST",
    url = "https://www.noegletal.dk/noegletal/nctrlman",
    body = payload,
    encode = "form",
    httr::content_type("application/x-www-form-urlencoded")
  )

  httr::content(response, "text", encoding = "latin1")
}

#' Parse HTML content from noegletal.dk
#'
#' @param html_content HTML content from noegletal_scrape
#' @param n_munis Number of municipalities
#' @param years Vector of years
#' @returns Tibble with parsed data
#' @noRd
noegletal_parse_html <- function(html_content, n_munis, years) {
  parsed <- rvest::read_html(html_content)

  table_rows <- parsed |>
    rvest::html_elements(xpath = "//tr[td[@class = 'nwDatL' or contains(concat(' ', normalize-space(@class), ' '), ' nwDatL ')]]")

  data_matrix <- do.call(rbind, lapply(table_rows, function(tr) {
    rvest::html_elements(tr, "td") |> rvest::html_text(trim = TRUE)
  }))

  df <- data_matrix |>
    tibble::as_tibble() |>
    dplyr::select(-1)  # delete the first col (it is empty)

  names(df) <- c("muni_code", years)

  var_names <- parsed |>
    rvest::html_elements(".nwGrp") |>
    rvest::html_text(trim = TRUE)

  df |>
    dplyr::mutate(
      variable = rep(var_names, each = n_munis)[1:nrow(df)],
      .after = .data$muni_code
    ) |>
    dplyr::mutate(
      muni_name = stringr::str_split_fixed(.data$muni_code, " ", 2)[,2],
      .after = .data$muni_code
    ) |>
    dplyr::mutate(
      muni_code = stringr::str_split_fixed(.data$muni_code, " ", 2)[,1]
    )
}

#' Wrangle parsed data from noegletal.dk
#'
#' @param df Parsed data from noegletal_parse_html
#' @returns Tidy tibble
#' @noRd
noegletal_wrangle <- function(df) {
  df |>
    tidyr::pivot_longer(cols = 4:ncol(df), names_to = "year") |>
    dplyr::mutate(
      # remove thousand separator
      value = stringr::str_replace_all(.data$value, "\\.", ""),
      # Replace commas with periods for proper decimal handling in R.
      value = stringr::str_replace_all(.data$value, ",", "."),
      # According to "noegletal.dk", a dash "-" means 0.
      value = stringr::str_replace_all(.data$value, "-", "0")
    ) |>
    # Coerce to numeric, values of "M" (missing, according to Noegletal.dk),
    # can't be coerced and is therefore made NA.
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) |>
    tidyr::pivot_wider(
      id_cols = c(.data$muni_code, .data$muni_name, .data$year),
      names_from = .data$variable,
      values_from = .data$value
    )
}

#' Get data from noegletal.dk
#'
#' @param muni_codes Vector of municipality codes
#' @param years Vector of years
#' @param variable_ids Vector of variable IDs
#' @returns Tidy [tibble::tibble()] with requested data
#' @export
#' @examples
#' noegletal_get(muni_codes = c(101, 155), years = 2018:2024, variable_ids = c(001))
noegletal_get <- function(muni_codes = ALLOWED_MUNI_CODES,
                          years = ALLOWED_YEARS,
                          variable_ids) {
  validate_input(muni_codes, years, variable_ids)

  # sort years in chronological order for proper alignment.
  years <- sort(years)

  message("Getting requested data from noegletal.dk ...")

  html_content <- noegletal_scrape(muni_codes, years, variable_ids)
  parsed_data <- noegletal_parse_html(html_content, n_munis = length(muni_codes), years)
  noegletal_wrangle(parsed_data)

  # TODO: make muni_code, muni_name, year as factors.
}

#' Validate input parameters
#'
#' @param muni_codes Vector of municipality codes
#' @param years Vector of years
#' @param variable_ids Vector of variable IDs
#' @returns None
#' @noRd
validate_input <- function(muni_codes, years, variable_ids) {
  if (!all(muni_codes %in% ALLOWED_MUNI_CODES)) {
    stop("Invalid municipality code(s) provided.")
  }

  if (!all(years %in% ALLOWED_YEARS)) {
    stop("Invalid year(s) provided. Years must be between 2007 and 2024.")
  }

  n_selected_rows <- length(muni_codes) * length(years) * length(variable_ids)
  if (n_selected_rows > 25000) {
    # TODO: as a more helpful message, write how many rows that was selected
    # including the calculation.
    stop("Too many rows selected. Maximum allowed is 25000.")
  }
}

# Constants
ALLOWED_MUNI_CODES <- c(101, 147, 151, 153, 155,
                        157, 159, 161, 163, 165,
                        167, 169, 173, 175, 183,
                        185, 187, 190, 201, 210,
                        217, 219, 223, 230, 240,
                        250, 253, 259, 260, 265,
                        269, 270, 306, 316, 320,
                        326, 329, 330, 336, 340,
                        350, 360, 370, 376, 390,
                        400, 410, 420, 430, 440,
                        450, 461, 479, 480, 482,
                        492, 510, 530, 540, 550,
                        561, 563, 573, 575, 580,
                        607, 615, 621, 630, 657,
                        661, 665, 671, 706, 707,
                        710, 727, 730, 740, 741,
                        746, 751, 756, 760, 766,
                        773, 779, 787, 791, 810,
                        813, 820, 825, 840, 846,
                        849, 851, 860)

ALLOWED_YEARS <- 2007:2024
