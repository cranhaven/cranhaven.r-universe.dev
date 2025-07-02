#' Create parameters and simulate selection weights
#'
#' This function creates and explores parameters for generating selections.
#' These parameters define the selection distribution of minutes (`min`) around
#' the sun event (sunrise/sunset), as well as of days (`day`).
#'
#' @param min_range Numeric vector. Range of the sampling distribution of
#'   minutes around the sun event.
#' @param min_mean Numeric. Mean of the sampling distribution of minutes to the
#'   sun event.
#' @param min_sd Numeric. SD in minutes of the sampling distribution of minutes
#'   around the sun event.
#' @param day_range Date/Datetime/Numeric vector. Range of sampling distribution
#'   of days. Can be Dates, Date-times, or DOY (day-of-year, 1-366).
#' @param day_mean Date/Datetime/Numeric. Mean date of the sampling distribution
#'   of days. Can be Date, Date-time, or DOY (day-of-year, 1-366).
#' @param day_sd Numeric. SD in days of the sampling distribution of days.
#' @param offset Numeric. Offset to shift for time of day in minutes.
#' @param return_log Logical. Log the density in the selection function?
#' @param selection_fun Character. Selection function to use. Options are
#'   `lognorm`, `norm` (default), or `cauchy`.
#' @param selection_var Character. Selection variable to plot
#'   (if `plot = TRUE`). Options are are `psel`, `psel_doy`, `psel_min`,
#'   `psel_std`, `psel_scaled`, or `psel_normalized` (default).
#' @param return_params Logical. Return parameter list for use in
#'   calc_selection_weights()?
#' @param plot Logical. Create plot of simulated selection weights? If
#'   `return_param = TRUE` and `plot = TRUE` plot is created as a side effect.
#'   Other wise, plot is returned directly.
#'
#' @return Returns either a list of selection parameters or a plot of simulated
#'   selection weights
#' @export
#'
#' @examples
#' params <- sim_selection_weights()
sim_selection_weights <- function(
    min_range = c(-70, 240), min_mean = 30, min_sd = 60,
    day_range = c(120, 201), day_mean = 161, day_sd = 20,
    offset = 0, return_log = TRUE, selection_fun = "norm",
    selection_var = "psel_normalized", return_params = TRUE, plot = TRUE) {
  check_installed("ggplot2",
      c("Package \"ggplot2\" must be installed to use this function",
        "!" = "Use `install.packages(\"ggplot2\") to install"
      )
    )

  # Create parameter list
  params <- list(
    min_range = min_range, min_mean = min_mean, min_sd = min_sd,
    day_range = day_range, day_mean = day_mean, day_sd = day_sd,
    offset = offset, return_log = return_log,
    selection_fun = selection_fun
  )

  # Checks
  params <- check_selection_params(params)
  check_logical(return_params)
  check_logical(plot)
  check_text(selection_var,
    n = 1,
    opts = c(
      "psel_normalized", "psel", "psel_day", "psel_min",
      "psel_std", "psel_scaled", "psel_normalized"
    )
  )

  # Simulate weights
  if (plot) {
    weights <- expand.grid(
      date = seq(day_range[1], day_range[2]),
      min = seq(min_range[1], min_range[2]),
      site_id = 1
    ) |>
      calc_selection_weights(params = params, col_min = min) |>
      dplyr::mutate(date = lubridate::ymd("2022-01-01") + date - 1)

    p1 <- ggplot2::ggplot(
      weights, ggplot2::aes(.data[["date"]], .data[["psel_doy"]])
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Date", y = "Selection weight (date)")

    p2 <- ggplot2::ggplot(
      weights, ggplot2::aes(.data[["min"]], .data[["psel_min"]])
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Time to sun event", y = "Selection weight (time)")

    p3 <- ggplot2::ggplot(
      weights, ggplot2::aes(.data[["date"]], .data[["min"]],
        fill = .data[[selection_var]]
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(x = "Date", y = "Time", fill = "Selection\nWeight")

    if (is_installed("patchwork")) {
      withr::with_package(
        package = "patchwork",
        p <- (p1 + p2) / p3 &
          ggplot2::theme_minimal(base_size = 14)
      )
    } else {
      inform("Package \"patchwork\" not installed, returning plots as list.")
      p <- list(
        p1 + ggplot2::theme_minimal(base_size = 14),
        p2 + ggplot2::theme_minimal(base_size = 14),
        p3 + ggplot2::theme_minimal(base_size = 14)
      )
    }

    if (return_params) print(p) # Print if not returning
  }

  if (return_params) r <- params else r <- p
  r
}



#' Calculate Selection Weights
#'
#' Calculate selection weights for a series of recordings based on the selection
#' parameters defined by `sim_selection_weights()`.
#'
#' @param meta_sun (Spatial) Data frame. Recording meta data with time to
#'   sunrise/sunset. Output of `calc_sun()`. Must have at least `col_min`,
#'   `col_day`, and `col_site_id`.
#' @param col_min Column. Unquoted column containing minutes to sunrise (`t2sr`)
#'   or sunset (`t2ss`) output from `calc_sun()` (defaults to `t2sr`).
#' @param col_day Column. Unquoted column containing dates or day-of-year (doy)
#'   to use (defaults to `date`).
#' @param params Named list. Parameters created by `sim_selection_weights()`, containing
#'    `min_range`, `min_mean`, `min_sd`, `day_range`, `day_mean`, `day_sd`,
#'    `offset`, `return_log`, `selection_fun`.
#'
#' @inheritParams common_docs
#'
#' @return   Returns data with appended selection weights columns:
#'   - `psel_by` - The minutes column used
#'   - `psel_min` - Probability of selection by time of day (min column)
#'   - `psel_doy` - Probability of selection by day of year
#'   - `psel` - Probability of selection overall
#'   - `psel_scaled` - Probability of selection scaled overall
#'   - `psel_std` - Probability of selection standardized within a site
#'   - `psel_normalized` - Probability of selection normalized within a site
#'
#' @export
#'
#' @examples
#' s <- clean_site_index(example_sites_clean,
#'   name_date_time = c("date_time_start", "date_time_end")
#' )
#' m <- clean_metadata(project_files = example_files) |>
#'   add_sites(s) |>
#'   calc_sun()
#'
#' params <- sim_selection_weights()
#' calc_selection_weights(m, params = params)
calc_selection_weights <- function(meta_sun,
                                   params,
                                   col_site_id = site_id,
                                   col_min = t2sr,
                                   col_day = date) {
  # Checks
  # No need for multiple preemptive enclosures (so no `enquo(col...)`)
  check_cols(meta_sun, cols = c(!!enquo(col_site_id), !!enquo(col_min), !!enquo(col_day)))
  params <- check_selection_params(params)
  name_min <- nse_names(enquo(col_min))

  # Get params as environment objects
  # (done this way to avoid 'no visible binding notes')
  min_range <- params$min_range
  min_mean <- params$min_mean
  min_sd <- params$min_sd
  day_range <- params$day_range
  day_mean <- params$day_mean
  day_sd <- params$day_sd
  offset <- params$offset
  return_log <- params$return_log
  selection_fun <- params$selection_fun


  # Prepare selection weights data frame
  sp <- meta_sun |>
    dplyr::mutate(doy = check_doy({{ col_day }})) |>
    dplyr::filter(
      .data[["doy"]] >= day_range[[1]],
      .data[["doy"]] <= day_range[[2]],
      {{ col_min }} >= min_range[[1]],
      {{ col_min }} <= min_range[[2]]
    )

  if (nrow(sp) == 0) {
    abort(
      "No selections possible within this range of dates and times",
      call = caller_env()
    )
  }

  # Prepare selection functions
  min_fun <- switch(selection_fun,
    "lognorm" = function(x, m, sd, log) stats::dlnorm(x + offset, log(m + offset), sd, log),
    "norm" = stats::dnorm,
    "cauchy" = stats::dcauchy
  )

  # Check offset
  if ((min(dplyr::pull(meta_sun, {{ col_min }})) + offset) <= 0 && selection_fun == "lognorm") {
    abort(
      paste0(
        "If `selection_fun = 'lognorm'` and any `min` values are less than 0\n",
        "you must provide an `offset` large enough to ensure all values are greater than 0."
      )
    )
  }

  dens_min <- min_fun(seq(min_range[1], min_range[2]), min_mean, min_sd, log = return_log)
  dens_doy <- stats::dnorm(seq(day_range[1], day_range[2]), day_mean, day_sd, log = return_log)
  fun_psel <- function(x1, x2, return_log) if (return_log) exp(x1 + x2) else x1 * x2


  # Arrange
  # - Match order of starting data (meta_sun)
  # - No changes from arrange, joins, or groupings, so no need to arrange

  # Calculate selection weights
  sp |>
    dplyr::mutate(
      psel_by = name_min, # Keep a record for sample_recordings()
      psel_min = min_fun(round({{ col_min }}, 0), min_mean, min_sd, log = return_log) / max(abs(dens_min)),
      psel_doy = stats::dnorm(.data[["doy"]], mean = day_mean, sd = day_sd, log = return_log) / max(abs(dens_doy)),
      psel = fun_psel(.data[["psel_min"]], .data[["psel_doy"]], return_log),
      psel_scaled = .data[["psel"]] / max(.data[["psel"]])
    ) |>
    dplyr::group_by({{ col_site_id }}) |>
    dplyr::mutate(
      psel_std = .data[["psel"]] / max(.data[["psel"]]),
      psel_normalized = pmax(
        1e-3,
        (.data[["psel"]] - min(.data[["psel"]])) / (max(.data[["psel"]]) - min(.data[["psel"]])),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()
}

check_selection_params <- function(params) {
  # Check parameters values
  check_num(params$min_range, n = 2)
  check_num(params$min_mean, n = 1)
  check_num(params$min_sd, n = 1, range = c(0, Inf))
  params$day_range <- check_doy(params$day_range)
  params$day_mean <- check_doy(params$day_mean)
  check_num(params$day_sd, n = 1, range = c(0, Inf))
  check_num(params$offset, n = 1)
  check_text(params$selection_fun, n = 1, opts = c("norm", "lognorm", "cauchy"))

  check_logical(params$return_log)
  if (params$offset != 0 & params$selection_fun != "lognorm") {
    inform(
      c("Ignoring `offset`",
        "i" = paste(
          "`offset` is only relevant when `selection_fun = `lognorm`",
          "to shift the minutes range to > 0"
        )
      )
    )
  }

  params
}

#' Sample recordings
#'
#' Sample recordings based on selection weights from `calc_selection_weights()`
#' using `spsurvey::grts()`.
#'
#' @param meta_weights (Spatial) Data frame. Recording meta data selection
#'   weights. Output of `calc_selection_weights()`. Must have at least the
#'   columns identified by `col_site_id` and `col_sel_weights`, as well as the
#'   probability of selection columns (those starting with `psel`) and `doy`.
#' @param n Numeric, Data frame, Vector, or List. Number of base samples to
#'   choose. For stratification by site, a named vector/list of samples per site, or
#'   a data frame with columns `n` for samples, `n_os` for oversamples and the
#'   column matching that identified by `col_site_id`.
#' @param os Numeric, Vector, or List. Over sample size (proportional) or named
#'   vector/list of number of samples per site Ignored if `n` is a data
#'   frame.
#' @param col_sel_weights Column. Unquoted name of column identifying selection
#'   weights (defaults to `psel_std`)
#' @param seed Numeric. Random seed to use for random sampling. Seed only
#'   applies to specific sampling events (does not change seed in the
#'   environment). `NULL` does not set a seed.
#' @param ... Extra named arguments passed on to `spsurvey::grts()`.
#'
#' @inheritParams common_docs
#'
#' @return A sampling run from grts. Note that the included dataset is spatial,
#'   but is a dummy spatial dataset created by using dates and times to create
#'   the spatial landscape.
#'
#' @export
#' @examples
#' s <- clean_site_index(example_sites_clean,
#'   name_date_time = c("date_time_start", "date_time_end")
#' )
#' m <- clean_metadata(project_files = example_files) |>
#'   add_sites(s) |>
#'   calc_sun()
#'
#' params <- sim_selection_weights()
#' w <- calc_selection_weights(m, params = params)
#'
#' # No stratification by site
#' samples <- sample_recordings(w, n = 10, os = 0.1, col_site_id = NULL)
#'
#' # Stratification by site defined by...
#'
#' # lists
#' samples <- sample_recordings(w, n = list(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2)
#'
#' # vectors
#' samples <- sample_recordings(w, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2)
#'
#' # data frame
#' samples <- sample_recordings(
#'   w,
#'   n = data.frame(
#'     site_id = c("P01_1", "P02_1", "P03_1"),
#'     n = c(2, 5, 2),
#'     n_os = c(0, 0, 1)
#'   )
#' )
#'
sample_recordings <- function(meta_weights,
                              n, os = NULL,
                              col_site_id = site_id,
                              col_sel_weights = psel_std,
                              seed = NULL, ...) {
  col_site_id <- enquo(col_site_id)
  col_sel_weights <- enquo(col_sel_weights)
  name_site_id <- nse_names(col_site_id)

  # Checks
  check_data(meta_weights, type = "meta_weights", ref = "calc_sleection_weights()")
  check_cols(meta_weights, c(!!enquo(col_site_id), !!enquo(col_sel_weights)))
  if (is.data.frame(n)) check_names(dplyr::rename_with(n, tolower),
                                    c(name_site_id, "n", "n_os"))
  check_num(seed, not_null = FALSE)

  if (!is_named(os) && length(os) == 1 && (os < 0 || os > 1)) {
    abort("`os` as a single value is a proportion, and must range between 0 and 1")
  } else if (is.null(os) && !inherits(n, "data.frame")) {
    abort("`os` can only be NULL if `n` is a data frame with a column `n_os`")
  }

  # If sf, convert to df
  meta_weights <- sf_to_df(meta_weights)

  # Convert to time-spatial
  meta_weights_sf <- sf::st_as_sf(meta_weights,
    coords = c("doy", meta_weights$psel_by[1]),
    crs = 3395
  )

  # Assemble n and os (based on BASSR::run_grts_on_BASS()

  # Check for stratification - Create a list of problems
  s <- c(inherits(n, "data.frame") | length(n) > 1 | is_named(n)) # Stratification exists in samples

  # Not stratified
  if (all(!s)) {
    if (!quo_is_null(col_site_id)) {
      warn("No stratification by site included in `n` or `os`. Ignoring `col_site_id`")
    }

    if (length(os) > 1) {
      abort("`os` must be a single value unless using stratification by site")
    }

    name_site_id <- NULL
    n_sites <- rep(n, length(n))
    n_os <- round(n * os)
    if (n_os == 0) n_os <- NULL

    # Stratified
  } else {
    # Missing site column name
    if (quo_is_null(col_site_id)) {
      abort_strat("`col_site_id` must contain site names matching those in `n`")
    }

    # Missing appropriate n object
    if (!(inherits(n, "data.frame") |
      length(n) > 1 |
      is_named(n))) {
      abort_strat()
    }

    sites <- meta_weights |>
      dplyr::pull({{ col_site_id }}) |>
      unique()

    # Get n_site and n_os depending on inputs

    # Check data frame
    if (inherits(n, "data.frame")) {
      # Problem: Wrong strata
      n_sites <- dplyr::pull(n, {{ col_site_id }})
      if (!all(n_sites %in% sites)) abort_strat()

      # Convert from data frame
      # Set oversample to null if all zeros
      if(sum(n$n_os)==0 || is_null(n$n_os)){
        n_os <- NULL
      } else{n_os <- as.list(n$n_os) |>
        stats::setNames(n_sites)
      }
      n <- as.list(n$n) |>
        stats::setNames(n_sites)
    } else {
      # Check list (convert if vector)
      n <- as.list(n)
      if (length(os) > 1) os <- as.list(os)

      # Problem: List not named correctly
      if (!(is_named(n) && all(names(n) %in% sites))) {
        abort_strat()
      }

      # Problem: List not named correctly (and not length = 1)
      if (!((is_named(os) && all(names(os) %in% sites)) ||
        length(os) == 1)) {
        abort_strat("`os` must be a single value, or a vector/list named by strata")
      }

      if (!is_named(os) && length(os) == 1) {
        n_os <- lapply(n, \(x) round(x * os))

        # If all 0, use NULL
        if (all(vapply(n_os, \(x) x == 0, logical(1)))) n_os <- NULL
      } else {
        n_os <- os
      }
    }

    # Problem: Chose stratification, but only one strata
    if (length(n) == 1 || (is_named(n_os) && length(n_os) == 1)) {
      abort_strat("There is only one stratum")
    }
  }

  # Check sample sizes
  msg <- NULL
  if(is.null(n_os)){
    n_check <- unlist(n)
  } else{
    n_check <- unlist(n) + unlist(n_os)
  }
  if (length(n_check) == 1) {
    if (n_check > nrow(meta_weights)) {
      msg <- c("i" = paste0(n_check, " samples, but only ", nrow(meta_weights), " data"))
    }
  } else {
    cnts <- sf::st_drop_geometry(meta_weights) |>
      dplyr::count({{ col_site_id }}) |>
      dplyr::pull("n", name = name_site_id)
    if (any(n_check > cnts[names(n)])) {
      msg <- c("i" = paste0(
        "Selected more samples than exist in some sites (",
        paste0(names(n)[n_check > cnts[names(n)]], collapse = ", "), ")"
      ))
    }
  }

  if (!is.null(msg)) abort(c("Cannot sample (n + oversampling) more points than there are in the data", msg))

  # Declare here to make the call info included in the output of spsurvey::grst
  # slightly more useful
  col_sel_weights <- nse_names(col_sel_weights)

  run_with_seed_if_provided(seed, {
    spsurvey::grts(
      sframe = meta_weights_sf,
      n_over = n_os,
      n_base = n,
      stratum_var = name_site_id,
      DesignID = "sample",
      aux_var = col_sel_weights,
      ...
    )
  })
}


#' Abort during stratification
#'
#' Wrapper around `abort()` for consistent messaging when stratification
#' arguments are not correct.
#'
#' @param msg Alternative message if required (otherwise returns default message
#'   regarding the `n` parameter)
#'
#' @noRd
abort_strat <- function(msg = NULL, call = caller_env()) {
  m <- "Not all requirements met for sampling with stratification by site"
  if (is.null(msg)) {
    msg <- paste0(
      "`n` must be a data frame with appropriate ",
      "columns, or vector/list named by site"
    )
  }
  abort(c(m, "x" = msg), call = call)
}
