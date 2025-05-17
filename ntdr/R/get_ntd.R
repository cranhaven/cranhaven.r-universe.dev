
#' Get National Transit Database (NTD) data
#'
#' [get_ntd()] downloads Excel file with the adjusted or raw monthly ridership
#' files provided by the National Transit Database and transforms the data into
#' a long-format data frame with one or more variable. Optionally cache the
#' source spreadsheet or filter by agency or mode.
#'
#' @param agency Name of the transit agency to retrieve. Defaults to `all` agencies
#' @param data_type Type of NTD data. Either "raw" for data released without
#'   adjustments or "adjusted" for data with adjustments and estimates
#' @param ntd_variable Which variable or variables to return. `UPT` for unlinked
#'   passenger trips, `VRM` for vehicle revenue miles, `VRH` for vehicle revenue
#'   hours, or `VOMS` for vehicles operated in maximum service.
#' @param modes Transit mode or modes to retrieve. Common modes include `MB`
#'   (bus), `CR` (commuter rail), `HR` (heavy rail), `LR` (light rail). Defaults
#'   to `all` modes.
#' @param cache Cache downloaded data. Defaults to `FALSE`. Set a default value
#'   with the "ntdr.cache" option.
#'
#' @return A data frame of monthly NTD data with the requested `ntd_variable` in
#'   the `value` column
#' @export
#'
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils download.file
#' @importFrom purrr map list_rbind
#'
#' @examplesIf interactive()
#' get_ntd(agency = "City of Madison", modes = c("MB", "DR"))
get_ntd <- function(
    agency = "all",
    data_type = "adjusted",
    ntd_variable = "UPT",
    modes = "all",
    cache = getOption("ntdr.cache", FALSE)) {
  # check for invalid parameters
  data_type <- arg_match0(data_type, c("raw", "adjusted"))

  # Use temp directory by default or package cache if `cache = TRUE`
  ntd_dir <- tempdir()

  if (cache) {
    ntd_dir <- rappdirs::user_cache_dir("ntdr")

    if (!dir.exists(ntd_dir)) {
      dir.create(ntd_dir, recursive = TRUE)
    }
  }

  ntd_path <- file.path(
    ntd_dir,
    paste0("ntd_download_", data_type, ".xlsx")
  )

  # Download file if cache file doesn't exist or cache is FALSE
  if (!file.exists(ntd_path) || !cache) {
    url <- get_ntd_url(data_type)
    utils::download.file(url, ntd_path, method = "curl")
  }

  # Convert lower and mixed case values for ntd_variable
  ntd_variable <- toupper(ntd_variable)

  ntd_variable <- arg_match(
    ntd_variable,
    c("UPT", "VRM", "VRH", "VOMS"),
    multiple = TRUE
  )

  # Read and combine one or more sheets from the source xlsx file
  if (length(ntd_variable) > 1) {
    ntd_list <- purrr::map(
      ntd_variable,
      \(var) {
        read_ntd_data(
          ntd_path,
          ntd_variable = var,
          agency = agency,
          modes = modes
        )
      }
    )

    ntd_data <- purrr::list_rbind(ntd_list)
  } else {
    ntd_data <- read_ntd_data(
      ntd_path,
      ntd_variable = ntd_variable,
      agency = agency,
      modes = modes
    )
  }

  ntd_data
}

#' Read a single variable from the download NTD Excel file
#' @noRd
#' @importFrom readxl read_excel
#' @importFrom rlang set_names
#' @importFrom dplyr filter all_of mutate
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate parse_date_time
read_ntd_data <- function(path,
                          ntd_variable = "UPT",
                          agency = NULL,
                          modes = NULL,
                          ...,
                          call = caller_env()) {
  sheet <- switch(ntd_variable,
    "UPT" = 3,
    "VRM" = 4,
    "VRH" = 5,
    "VOMS" = 6
  )

  ntd_data <- readxl::read_excel(
    path,
    sheet = sheet
  )

  ntd_cols <- c(
    "ntd_id_5", "ntd_id_4", "agency", "active", "reporter_type",
    "uace", "uza_name", "modes", "tos", "modes_simplified"
  )

  monthly_cols <- seq(length(ntd_cols) + 1, ncol(ntd_data))

  ntd_data <- ntd_data |>
    # Set names
    rlang::set_names(c(ntd_cols, names(ntd_data)[monthly_cols])) |>
    # Remove summary rows at end of sheet
    dplyr::filter(!is.na(.data[["ntd_id_5"]])) |>
    # Filter by agency
    filter_all_data(
      filter_var = "agency",
      filter_param = agency,
      call = call
    ) |>
    # Filter by mode
    filter_all_data(
      filter_var = "modes",
      filter_param = modes,
      call = call
    )

  # pivot data
  ntd_data_long <- ntd_data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(monthly_cols),
      names_to = "month",
      values_to = "value"
    ) |>
    dplyr::mutate(
      month = as.Date(lubridate::parse_date_time(month, orders = "m/Y")),
      ntd_variable = ntd_variable
    )

  ntd_data_long
}

#' Read a row in spreadsheet to check the number of columns
#' @noRd
#' @importFrom readxl read_excel
get_xlsx_ncol <- function(path,
                          sheet,
                          skip = 9,
                          n_max = 1,
                          offset = 10,
                          ...,
                          .name_repair = "minimal") {
  temp_sheet <- readxl::read_xlsx(
    path,
    sheet = sheet,
    skip = skip,
    n_max = n_max,
    ...,
    .name_repair = .name_repair
  )

  ncol(temp_sheet) - offset
}

#' Filter data by parameter optionally returning all
#' @noRd
#' @importFrom rlang inform
filter_all_data <- function(
    data,
    filter_var,
    filter_param,
    arg = caller_arg(filter_param),
    call = caller_env()) {
  if ((filter_param[1] == "all") || is.null(filter_param)) {
    if (length(filter_param) > 1) {
      inform(
        "Additional {.arg {arg}} values are ignored when {.arg {arg}}
        includes {.val 'all'}"
      )
    }
    return(data)
  }

  values <- unique(data[[filter_var]])

  arg_match(
    filter_param,
    values,
    multiple = TRUE,
    error_arg = arg,
    error_call = call
  )

  dplyr::filter(data, .data[[filter_var]] %in% filter_param)
}

#' retrieve URL for downloading NTD data
#' @noRd
#' @importFrom curl has_internet
#' @importFrom httr2 request req_url_path_append req_perform resp_body_html
#' @importFrom rvest html_element html_attr
get_ntd_url <- function(data_type = "adjusted", call = caller_env()) {
  data_type <- switch(data_type,
    raw = "monthly-module-raw-data-release",
    adjusted = "monthly-module-adjusted-data-release"
  )

  # tests for functioning internet connection
  if (!curl::has_internet()) {
    abort(
      "An internet connection is required to download data
      from the National Transit Database."
    )
  }

  ntd_page <- httr2::request("https://www.transit.dot.gov/ntd/data-product/") |>
    httr2::req_url_path_append(data_type) |>
    httr2::req_perform(error_call = call) |>
    httr2::resp_body_html()

  ntd_url <- ntd_page |>
    rvest::html_element(".file--x-office-spreadsheet a") |>
    rvest::html_attr("href")

  paste0("https://www.transit.dot.gov", ntd_url)
}

# retrieve archived NTD data pin from board
# # for future version
# retrieve_pin <- function(ntd_variable, data_type){
#   stopifnot("The archived data cannot be reached. Check your internet connection, try again later, or set `cache = FALSE`." = check_board_status())
#   ntdr_board <- pins::board_url("https://ntdr-pins.s3.us-west-2.amazonaws.com/")
# }
