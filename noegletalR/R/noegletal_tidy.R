#' Transform a csv file from noegletal.dk into a tidy tibble
#'
#' @description
#' `noegletal_tidy` takes as input a csv-file downloaded from noegletal.dk and
#' parses it into a tidy [tibble::tibble()]. This [tibble::tibble()] has one row
#' for each municipality-year, a `muni_code` column, a `year` column and a
#' column for each `variable` (as selected on noegletal.dk).
#'
#' As per the noegletal.dk documentation, cells with a dash `-` as a value is
#' converted to a 0, while cells with a value of `M` or `U` is converted to
#' `NA`, since these represent missing values.
#'
#' @param file Path to a csv file downloaded from noegletal.dk.
#' @returns A tidy [tibble::tibble()] with one row for each municipality-year,
#'   and one column for each included variable (nøgletal).
#'
#' @export
#' @examples
#' path_to_file <- system.file("extdata",
#'                             "nwRap-10Sep2024-101803.csv",
#'                             package = "noegletalR",
#'                             mustWork = TRUE)
#' noegletal_tidy(file = path_to_file)
noegletal_tidy <- function(file) {

  if (!file.exists(file)) {
    stop("The file to be converted does not exist: ", file)
  }

  data <- readr::read_delim(
    file,
    delim = ";",
    locale = readr::locale(encoding = "latin1"),
    skip = 2
  ) |>
    dplyr::rename(variable = .data$`...1`, muni_code = .data$Kom.nr) |>
    dplyr::filter(!stringr::str_ends(.data$variable, "Kommune") |
                    !rowSums(is.na(dplyr::across(
                      dplyr::everything()
                    ))) > 1)

  # Insert the correct `variable` in the variable col.
  for (i in seq_len(nrow(data))) {
    if (i > 1 && stringr::str_ends(data$variable[i], "Kommune")) {
      data$variable[i] <- data$variable[i - 1]
    }
  }

  # Remove rows that does not include muni_code (removes footnotes and rows with
  # only variable headings)
  data <- data |>
    dplyr::filter(!is.na(.data$muni_code))

  # Pivot to long format
  data <- data |>
    dplyr::mutate(dplyr::across(3:ncol(data), as.character)) |>
    tidyr::pivot_longer(cols = 3:ncol(data), names_to = "year")

  # Clean up values
  data <- data |>
    dplyr::mutate(
      # remove thousand separator
      value = stringr::str_replace_all(.data$value, "\\.", ""),
      # Replace commas with periods for proper decimal handling in R.
      value = stringr::str_replace_all(.data$value, ",", "."),
      # According to "noegletal.dk", a dash "-" means 0.
      value = stringr::str_replace_all(.data$value, "-", "0")
    ) |>
    # Coerce to numeric, values of "M" or "U" (missing, according to
    # noegletal.dk), can't be coerced and is therefore made NA.
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value)))

  # Pivot to tidy format
  data <- data |>
    tidyr::pivot_wider(
      id_cols = c(.data$muni_code, .data$year),
      names_from = .data$variable,
      values_from = .data$value
    )

  # TODO: make muni_code, muni_name, year as factors.
  return(data)
}
