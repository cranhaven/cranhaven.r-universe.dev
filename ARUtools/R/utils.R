#' List files in the project directory
#'
#' Grabs a list of files and directories based on the subset definied.
#' Loose wrapper around `fs::dir_ls()`
#'
#' @noRd
#'
list_files <- function(project_dir, subset, subset_type,
                       type = c("file", "directory")) {
  fs::dir_ls(project_dir,
    type = type,
    # Add filters
    regexp = subset,
    invert = subset_type == "omit",
    recurse = TRUE
  )
}

#' Extract and replace a pattern
#'
#' Grab a pattern (dropping everything else), then replace it with a cleaned up
#' version.
#'
#' @param string Character string
#' @param pattern Named character vector. Names are what are extracted, contents
#'   what it is replaced with.
#'
#' @examples
#' extract_replace("ARU1000/SM1000/4562.wav", c("SM[0-9]{1,4}" = "SongMeter"))
#'
#' @noRd
extract_replace <- function(string, pattern) {
  string |>
    stringr::str_extract(
      stringr::regex(paste0("(", names(pattern), ")", collapse = "|"),
        ignore_case = TRUE
      )
    ) |>
    stringr::str_replace_all(stringr::regex(pattern, ignore_case = TRUE))
}

#' Report/flag missing data
#'
#' @param missing Numeric. Number of missing data points.
#' @param total Numeric. Total data points.
#' @param name Character. Name of missing data.
#' @param what Character. What was done to the data? (e.g., extracted, detected,
#'   etc.)
#'
#' @noRd
report_missing <- function(missing, total, name, what = "detected") {
  msg <- NULL
  if (missing > 0) {
    if (missing == total) type <- "No" else type <- "Not all"
    msg <- c("x" = paste0(
      type, " ", name, " were successfully ", what, " (",
      missing, "/", total, ")"
    ))
  }
  msg
}

#' Join data by date range
#'
#' @param x Data frame. To be joined
#' @param y Data frame. To be joined
#' @param by Character vector. Non-date/time columns to use in the join.
#' @param id Character. Column(s) that identify a record.
#' @param col Character. Date/time column to use in the join (in `x`)
#' @param int Character. Name of the column with the date/time interval to use in joining. (in `y`)
#' @param check_col Character. Name of the column to create to identify multiple matches.
#'
#' Join `x` and `y` by date/times in `x` being within the date/time interval in `y`
#' (within a set of unique `by` variables)
#'
#' @noRd
date_join <- function(x, y, by, id, col = "date", int = "date_range",
                      check_col = "...n") {
  # Nested filters
  match <- y |>
    dplyr::ungroup() |>
    tidyr::nest(add = -dplyr::all_of(int)) |>
    dplyr::mutate(data = purrr::map2(
      .data[[int]], .data$add,
      ~ dplyr::filter(x, lubridate::`%within%`(.data[[col]], ..1)) |>
        dplyr::inner_join(..2, by = .env$by)
    )) |>
    dplyr::select(-dplyr::any_of(int), -"add") |>
    tidyr::unnest("data")

  no_match <- dplyr::anti_join(x, match, by = id)

  all <- dplyr::bind_rows(match, no_match)

  if (nrow(x) != nrow(all)) {
    all <- dplyr::add_count(all, .data[[id]], name = "n_matches")
    all$n_matches[is.na(all[[check_col]])] <- NA_integer_
  }

  all
}

#' Check if character is easily convertable to Date
#'
#' Checks if `lubridate::as_date()` can convert the string.
#' If warning or error returns `FALSE` else returns `TRUE`.
#'
#' @param x Character/Date. Date in text (if Date, passes through, no problem).
#'
#' @return TRUE/FALSE
#'
#' @examples
#' is_dateable("2023-01-01") # TRUE
#' is_dateable("20-01-01") # TRUE
#' is_dateable("2023-01-01 01:00:00") # TRUE
#' is_dateable("05/16/2020") # FALSE
#'
#' @noRd
is_dateable <- function(x) {
  if (is.numeric(x)) {
    return(FALSE)
  }
  tryCatch(
    expr = {
      lubridate::as_date(x)
      TRUE
    },
    error = \(x) FALSE,
    warning = \(x) FALSE
  )
}

#' Quiet min/max functions
#'
#' Quietly return NA if no non-missing values (not -Inf or Inf)
#'
#' @param x
#'
#' @noRd
min_q <- function(x) minmax_q(x, min)
max_q <- function(x) minmax_q(x, max)

minmax_q <- function(x, fun) {
  if (length(x) == 0) {
    r <- NA
  } else if (all(is.na(x))) {
    r <- x[1]
  } else {
    r <- fun(x, na.rm = TRUE)
  }
  r
}



#' Convert spatial data frame to non-spatial data frame
#'
#' Extract geometry as longitude and latitude columns.
#'
#' @noRd
sf_to_df <- function(sf) {
  if (inherits(sf, "sf")) {
    sf <- sf::st_transform(sf, crs = 4326)

    df <- sf |>
      sf::st_drop_geometry() |>
      dplyr::bind_cols(sf::st_coordinates(sf)) |>
      dplyr::rename("longitude" = "X", "latitude" = "Y")
  } else {
    df <- sf
  }
  df
}

#' Convert data frame to spatial data frame if possible
#'
#' @param df Data frame to convert
#' @param sf Original sf data frame (optional, used to get CRS only)
#' @param crs CRS to use when converting (required if no sf)
#'
#' If no sf and no crs, assumes original was a data frame and silently returns
#' data frame.
#'
#' If sf/crs, sses the original sf data frame to get the CRS if not provided. First
#' checks for missing coordinates. If any are missing warns user and returns
#' data frame for troubleshooting.
#'
#' @noRd
df_to_sf <- function(df, sf = NULL, crs = NA, call = caller_env()) {
  if (!is.null(sf)) crs <- sf::st_crs(sf)

  if (!is.na(crs) && !inherits(df, "sf")) {
    if (any(is.na(df$longitude) | is.na(df$latitude))) {
      warn(c(
        "Cannot have missing coordinates in spatial data frames",
        "Returning non-spatial data frame"
      ), call = call)
      sf <- df
    } else {
      sf <- df |>
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
        sf::st_transform(crs)
    }
  } else {
    sf <- df
  }

  sf
}

suppressCat <- function(code, quiet = FALSE) {
  if (quiet) {
    invisible(utils::capture.output(x <- {
      code
    }))
  } else {
    x <- {
      code
    }
  }
  x
}

is_whole <- function(x, tolerance = 0.00001) {
  if (is.numeric(x)) {
    abs(x - round(x)) < tolerance
  } else {
    FALSE
  }
}

#' Run with provided seed unless NULL
#'
#' Wrapper around `withr::with_seed()` to ensure that if `seed` is `NULL`, it
#' is just *quietly* ignored (otherwise `withr` sends a warning)
#'
#' @param seed Numeric. Seed to use.
#' @param code Code. Code to be evaluated with the seed.
#' @noRd
#' @examples
#'
#' run_with_seed_if_provided(NULL, sample(1:10, 2))
#' run_with_seed_if_provided(NULL, sample(1:10, 2))
#' run_with_seed_if_provided(NULL, sample(1:10, 2))
#'
#' run_with_seed_if_provided(123, sample(1:10, 2))
#' run_with_seed_if_provided(123, sample(1:10, 2))
#' run_with_seed_if_provided(123, sample(1:10, 2))
run_with_seed_if_provided <- function(seed, code) {
  if (is.null(seed)) code else withr::with_seed(seed, code)
}

nse_names <- function(col) {
    rlang::expr_text(col) |>
    stringr::str_split(",") |>
    unlist() |>
    stringr::str_remove_all("~|c\\(|list\\(|\\,|\\)") |>
    stringr::str_trim() |>
    stringr::str_subset(".+")
}
