#' Filter ID/events
#'
#' @description
#' Given a vector of ID/events (concatenated like
#' `"{participant_id}_{session_id}"`), or a dataframe
#' with `participant_id` and `session_id` columns,
#' this function filters the data to keep or alternatively
#' remove the rows for the given _ID/events_.
#'
#' @param data tibble. The data to be filtered.
#' @param id_events character (vector) or dataframe. _(Vector of) ID/event(s)_
#' *or a dataframe with `participant_id` and `session_id` columns*.
#' @param revert logical. Whether to revert the filter, i.e., to keep only rows
#' NOT matching the `id_events` (Default: `FALSE`, i.e., keep only the rows
#' matching the `id_events`).
#' @return A tibble with the filtered data.
#' @export
#' @examples
#' data <- tibble::tribble(
#'   ~participant_id, ~session_id,
#'   "sub-001",       "ses-001",
#'   "sub-001",       "ses-002",
#'   "sub-002",       "ses-001",
#'   "sub-002",       "ses-002",
#'   "sub-003",       "ses-001",
#'   "sub-003",       "ses-002"
#' )
#'
#' # filter using a vector of ID/events
#' filter_id_events(
#'   data,
#'   id_events = c("sub-001_ses-001", "sub-003_ses-002")
#' )
#'
#' # filter using a dataframe with participant_id and session_id
#' data_filter <- tibble::tibble(
#'   participant_id = c("sub-001", "sub-003"),
#'   session_id = c("ses-001", "ses-002")
#' )
#' filter_id_events(
#'   data,
#'   id_events = data_filter
#' )
#'
#' # revert filter
#' filter_id_events(
#'   data,
#'   id_events = c("sub-001_ses-001", "sub-003_ses-002"),
#'   revert = TRUE
#' )
filter_id_events <- function(data, id_events, revert = FALSE) {
  chk::chk_data(data)
  if (is.data.frame(id_events)) {
    chk::check_names(id_events, c("participant_id", "session_id"))
    id_events <- paste0(id_events$participant_id, "_", id_events$session_id)
  } else {
    chk::chk_character(id_events)
  }
  chk::chk_logical(revert)

  data |>
    mutate(
      id_event = paste0(participant_id, "_", session_id)
    ) |>
    filter(
      xor(id_event %in% id_events, revert)
    ) |>
    select(
      -id_event
    )
}

#' Filter ABCD events
#'
#' @description
#' Given a (set of) condition(s), filters the events included in an ABCD
#' dataset. Conditions can be specified as a vector of strings, where each
#' string can be one of the following conditions:
#'
#' - `"core"`: events for the ABCD core study
#' - `"annual"`: annual events for the ABCD core study
#' - `"mid_year"`: mid-year events for the ABCD core study
#' - `"substudy"`: events for ABCD substudies
#' - `"covid"`: events for the COVID substudy
#' - `"sdev"`: events for the Social Development substudy
#' - `"even"`: even-numbered events
#' - `"odd"`: odd-numbered events
#' - numerical expressions like `>2` or `<=5` to filter events by number
#' - any other string to be used as filter for the `session_id` column
#'
#' The conditions can be combined with logical `"and"` or `"or"`.
#'
#' @param data tibble. The data to be filtered.
#' @param conditions character (vector). The events to keep.
#' @param connect character. Whether to connect the conditions with `"and"`
#' (an event is retained if _all_ conditions are met; the default) or `"or"`
#' (an event is retained if _any_ condition is met).
#'
#' @return A tibble with the filtered data.
#' @export
#' @examples
#' data <- tibble::tribble(
#'   ~session_id,     ~study,     ~type,
#'   "ses-00S",       "core",     "screener",
#'   "ses-00M",       "core",     "mid-year",
#'   "ses-00A",       "core",     "even",
#'   "ses-01M",       "core",     "mid-year",
#'   "ses-01A",       "core",     "odd",
#'   "ses-02M",       "core",     "mid-year",
#'   "ses-02A",       "core",     "even",
#'   "ses-03M",       "core",     "mid-year",
#'   "ses-03A",       "core",     "odd",
#'   "ses-04M",       "core",     "mid-year",
#'   "ses-04A",       "core",     "even",
#'   "ses-05M",       "core",     "mid-year",
#'   "ses-05A",       "core",     "odd",
#'   "ses-06M",       "core",     "mid-year",
#'   "ses-06A",       "core",     "even",
#'   "ses-C01",       "substudy", "covid",
#'   "ses-C02",       "substudy", "covid",
#'   "ses-C03",       "substudy", "covid",
#'   "ses-C04",       "substudy", "covid",
#'   "ses-C05",       "substudy", "covid",
#'   "ses-C06",       "substudy", "covid",
#'   "ses-C07",       "substudy", "covid",
#'   "ses-S01",       "substudy", "sdev",
#'   "ses-S02",       "substudy", "sdev",
#'   "ses-S03",       "substudy", "sdev",
#'   "ses-S04",       "substudy", "sdev",
#'   "ses-S05",       "substudy", "sdev"
#' )
#'
#' # ABCD core study events
#' filter_events_abcd(data, c("core"))
#'
#' # COVID substudy events
#' filter_events_abcd(data, c("covid"))
#'
#' # imaging events
#' filter_events_abcd(data, c("annual", "even"))
#'
#' # mid-years before year 5
#' filter_events_abcd(data, c("mid_year", "<5"))
#'
#' # COVID or Social Development substudy events
#' filter_events_abcd(data, c("covid", "sdev"), connect = "or")
filter_events_abcd <- function(data, conditions, connect = "and") {
  chk::chk_data(data)
  chk::chk_character(conditions)
  chk::chk_subset(connect, c("and", "or"))

  patterns <- conditions |>
    stringr::str_subset("even|odd|>|<", negate = TRUE) |>
    stringr::str_replace_all("core", "^ses-\\\\d{2}\\\\w") |>
    stringr::str_replace_all("annual", "^ses-\\\\d{2}A") |>
    stringr::str_replace_all("mid_year", "^ses-\\\\d{2}M") |>
    stringr::str_replace_all("substudy", "^ses-\\\\w\\\\d{2}.*") |>
    stringr::str_replace_all("covid", "^ses-C\\\\d{2}.*") |>
    stringr::str_replace_all("sdev", "^ses-S\\\\d{2}.*")

  event_numbers <- as.numeric(stringr::str_extract(data$session_id, "\\d{2}"))

  if (connect == "and") {
    filter_conditions <- purrr::reduce(
      patterns,
      ~ .x & stringr::str_detect(data$session_id, .y),
      .init = TRUE
    )
    if (any(stringr::str_detect(conditions, "even"))) {
      filter_conditions <- filter_conditions & event_numbers %% 2 == 0
    }
    if (any(stringr::str_detect(conditions, "odd"))) {
      filter_conditions <- filter_conditions & event_numbers %% 2 == 1
    }
    if (any(stringr::str_detect(conditions, ">|<"))) {
      numeric_conditions <- purrr::reduce(
        stringr::str_subset(conditions, ">|<"),
        ~ .x & eval(parse(text = paste0("event_numbers ", .y))),
        .init = TRUE
      )
      filter_conditions <- filter_conditions & numeric_conditions
    }
  } else if (connect == "or") {
    filter_conditions <- purrr::reduce(
      patterns,
      ~ .x | stringr::str_detect(data$session_id, .y),
      .init = FALSE
    )
    if (any(stringr::str_detect(conditions, "even"))) {
      filter_conditions <- filter_conditions | event_numbers %% 2 == 0
    }
    if (any(stringr::str_detect(conditions, "odd"))) {
      filter_conditions <- filter_conditions | event_numbers %% 2 == 1
    }
    if (any(stringr::str_detect(conditions, ">|<"))) {
      numeric_conditions <- purrr::reduce(
        stringr::str_subset(conditions, ">|<"),
        ~ .x | eval(parse(text = paste0("event_numbers ", .y))),
        .init = FALSE
      )
      filter_conditions <- filter_conditions | numeric_conditions
    }
  }

  data |>
    filter(filter_conditions)
}

#' Filter empty rows
#'
#' @description
#' This function filters out rows that are empty
#'
#' @param data tibble. The data to be filtered.
#' @param id_cols character (vector). The names of the ID columns to be
#' excluded from the filtering (Default: identifier columns used in ABCD and
#' HBCD).
#'
#' @return A tibble with the filtered data.
#' @export
#' @examples
#' data <- tibble::tibble(
#'   participant_id = c("sub-001", "sub-002", "sub-003"),
#'   session_id = c("ses-001", "ses-001", "ses-002"),
#'   var1 = c(NA, NA, 1),
#'   var2 = c(NA, NA, 2),
#'   var3 = c(NA, NA, 3)
#' )
#' filter_empty_rows(data)
filter_empty_rows <- function(
  data,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd())
) {
  chk::chk_data(data)
  chk::chk_character(id_cols)

  data |>
    filter(
      rowSums(!is.na(select(data, -any_of(id_cols)))) > 0
    )
}

#' Filter empty columns
#'
#' @description
#' This function filters out columns that are empty.
#'
#' @param data tibble. The data to be filtered.
#' @param id_cols character (vector). The names of the ID columns to be
#' excluded from the filtering (Default: identifier columns used in ABCD and
#' HBCD).
#' @return A tibble with the filtered data.
#' @export
#' @examples
#' data <- tibble::tibble(
#'   participant_id = c("sub-001", "sub-002", "sub-003"),
#'   session_id = c("ses-001", "ses-001", "ses-002"),
#'   var1 = c(NA, NA, NA),
#'   var2 = c(NA, NA, 2),
#'   var3 = c(NA, NA, 3)
#' )
#' filter_empty_cols(data)
filter_empty_cols <- function(
  data,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd())
) {
  chk::chk_data(data)
  chk::chk_character(id_cols)

  data |>
    select(
      any_of(id_cols),
      where(~ !all(is.na(.x)))
    )
}
