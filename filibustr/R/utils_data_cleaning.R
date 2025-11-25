# convert state and expectation to factors (using `haven::as_factor()` if applicable)
# used in `fix_hvw_coltypes()` and `fix_les_coltypes()`
create_factor_columns <- function(df, local_path) {
  if (isTRUE(tools::file_ext(local_path) == "dta")) {
    df <- df |>
      # no need to specify levels if data is already coming from saved DTA file
      dplyr::mutate(dplyr::across(
        .cols = dplyr::any_of(c("state", "st_name", "expectation1", "expectation2")),
        .fns = haven::as_factor))
  } else {
    df <- df |>
      dplyr::mutate(
        dplyr::across(.cols = dplyr::any_of("state"),
                      # Senate data (`state`) just has members from the states
                      .fns = ~ factor(.x, levels = datasets::state.abb)),
        dplyr::across(.cols = dplyr::any_of("st_name"),
                      .fns = ~ factor(.x, levels = c(
                        # House data (`st_name`) includes non-voting members
                        datasets::state.abb, "AS", "DC", "GU", "MP", "PR", "VI"
                      ))),
        # LES vs. expectation
        dplyr::across(.cols = dplyr::any_of(c("expectation1", "expectation2")),
                      .fns = as.factor)
      )
  }

  df
}

# filter (Voteview) data by chamber
filter_chamber <- function(df, chamber) {
  # filter chamber
  chamber_code <- match_chamber(chamber = chamber)
  if (chamber_code == "H") {
    df <- df |>
      dplyr::filter(chamber != "Senate")
  } else if (chamber_code == "S") {
    df <- df |>
      dplyr::filter(chamber != "House")
  }

  # remove filtered-out chambers
  # so filtered local files are identical to single-chamber online downloads
  df |> dplyr::mutate(chamber = droplevels(chamber))
}

# filter (Voteview) data by Congress number
filter_congress <- function(df, congress, call = rlang::caller_env()) {
  if (!is.null(congress)) {
    # check for invalid Congress numbers
    match_congress(congress = congress, call = call)

    # check that Congress numbers are present in data
    # NOTE: only error if no data is present. Ok if only some Congress numbers are present
    if (!any(congress %in% unique(df$congress))) {
      len <- length(congress)
      cli::cli_abort(
        # `qty()`: pluralize based on the length of `congress`, not its value
        paste("Congress {cli::qty(length(congress))} number{?s} ({.val {congress}})",
              "{cli::qty(length(congress))} {?was/were} not found in data."),
        call = call
      )
    }

    df <- df |>
      dplyr::filter(congress %in% {{ congress }})
  }

  df
}
