.onLoad <- function(libname, pkgname) {
  options(cli.progress_show_after = 0)
  options(cli.progress_clear = FALSE)
}

#' Validate names
#'
#' @noRd
match_names <- function(x, lookup, error = NULL, name_string = "names", lookup_vec_last = " and ") {
  unmatched <- c(setdiff(x, lookup), setdiff(lookup, x))
  if (length(unmatched) > 0) {
    stop(
      cli::format_error(c(
        error,
        "i" = "Valid {name_string} are {.val {cli::cli_vec(lookup, style = list(vec_sep = ', ', vec_last = {lookup_vec_last}))}}.",
        "x" = "You supplied {.val {x}}."
      )),
      call. = FALSE
    )
  }
}

#' Validate registration parameters
#'
#' @noRd
validate_params <- function(stretches, shifts, registration_type = c("optimisation", "manual")) {
  # Registration with optimisation
  if (registration_type == "optimisation") {
    if (all(is.na(stretches), is.na(shifts))) {
      cli::cli_alert_info("Using computed stretches and shifts search space limits.")
    } else if (all(!is.na(stretches), !is.na(shifts))) {
      cli::cli_alert_info("Using provided stretches and shifts to define search space limits.")
    } else {
      var_na <- ifelse(is.na(stretches), "stretches", "shifts")
      var_num <- ifelse(is.na(stretches), "shifts", "stretches")

      cli::cli_alert_info("Using provided {var_num} and computed {var_na} to define search space limits.")
    }
  }

  # Manual registration
  if (registration_type == "manual") {
    if (any(is.na(stretches), is.na(shifts))) {
      stop(
        cli::format_error(c(
          "{.var stretches} and {.var shifts} must be {.cls numeric} vectors.",
          "x" = "You supplied vectors with {.cls NA} values."
        )),
        call. = FALSE
      )
    }
  }
}

#' Perform crossing in {data.table}
#'
#' @noRd
cross_join <- function(a, b) {
  cj <- data.table::CJ(
    seq_len(nrow(a)),
    seq_len(nrow(b))
  )
  cbind(a[cj[[1]], ], b[cj[[2]], ])
}

#' Get approximate stretch factor
#'
#' \code{get_approximate_stretch()} is a function to get a stretch factor
#' estimation given input data. This function will take the time point ranges
#' of both reference and query data and compare them to estimate the stretch
#' factor.
#'
#' @param data Input data frame, either containing all replicates of gene expression or not.
#' @param reference Accession name of reference data.
#' @param query Accession name of query data.
#'
#' @return This function returns an estimation of a stretch factor for registering the data.
#'
#' @export
get_approximate_stretch <- function(data, reference = "ref", query = "query") {
  # Suppress "no visible binding for global variable" note
  accession <- NULL
  timepoint <- NULL
  time_range <- NULL

  # Make sure the data are data.tables
  data <- data.table::as.data.table(data)

  # Validate accession names
  match_names(
    x = c(reference, query),
    lookup = unique(data$accession),
    error = "Must review the supplied {.var reference} and {.var query} values:",
    name_string = "accession values"
  )

  # Calculate approximate stretch factor
  deltas <- data[, .(time_range = max(timepoint) - min(timepoint)), by = .(accession)]

  stretch_factor <- deltas[accession == reference, time_range] / deltas[accession == query, time_range]

  return(stretch_factor)
}

#' Calculate limits of the search space
#'
#' @noRd
get_search_space_limits <- function(data, stretches = NA, shifts = NA, overlapping_percent = 0.5) {
  stretch_space_lims <- get_stretch_search_space_limits(data, stretches, overlapping_percent)
  shift_space_lims <- get_shift_search_space_limits(data, shifts, stretch_space_lims, overlapping_percent)
  space_lims <- c(stretch_space_lims, shift_space_lims)

  return(space_lims)
}

#' Calculate limits of the stretch search space
#'
#' @noRd
get_stretch_search_space_limits <- function(data, stretches = NA, overlapping_percent = 0.5) {
  # Suppress "no visible binding for global variable" note
  accession <- NULL
  timepoint <- NULL

  # Check calculation mode
  if (all(is.na(stretches))) {
    calc_mode <- "auto"
  } else if (length(stretches) == 1) {
    calc_mode <- "init"
  } else if (length(stretches) >= 2) {
    calc_mode <- "bound"
  }

  # Calculate boundary
  if (calc_mode == "bound") {
    # Calculate limits
    stretch_init <- mean(stretches)
    stretch_lower <- min(stretches)
    stretch_upper <- max(stretches)
  } else {
    # Initial value
    if (calc_mode == "auto") {
      stretch_init <- get_approximate_stretch(data)
    } else if (calc_mode == "init") {
      stretch_init <- stretches
    }

    # Calculate limits
    stretch_lower <- overlapping_percent * stretch_init
    stretch_upper <- 1.5 * stretch_init
  }

  # Results object
  results_list <- list(
    stretch_init = stretch_init,
    stretch_lower = stretch_lower,
    stretch_upper = stretch_upper
  )

  return(results_list)
}

#' Calculate limits of the shift search space
#'
#' @noRd
get_shift_search_space_limits <- function(data, shifts = NA, stretch_space_lims, overlapping_percent = 0.5) {
  # Suppress "no visible binding for global variable" note
  accession <- NULL
  timepoint <- NULL

  # Check calculation mode
  if (all(is.na(shifts))) {
    calc_mode <- "auto"
  } else if (length(shifts) == 1) {
    calc_mode <- "init"
  } else if (length(shifts) >= 2) {
    calc_mode <- "bound"
  }

  # Extract time point ranges
  timepoints_ref <- unique(data[accession == "ref", timepoint])
  timepoints_query <- unique(data[accession == "query", timepoint])
  range_ref <- range(timepoints_ref)
  range_query <- range(timepoints_query)

  # Read init stretch limit
  stretch_init <- stretch_space_lims$stretch_init
  range_query_init_stretch <- stretch_init * diff(range_query)

  # Calculate boundary
  if (calc_mode == "bound") {
    # Calculate limits
    shift_lower <- min(shifts)
    shift_upper <- max(shifts)
  } else {
    # Read stretch limits
    stretch_lower <- stretch_space_lims$stretch_lower
    stretch_upper <- stretch_space_lims$stretch_upper

    # Calculate minimum and maximum timepoints in which the curves overlap
    min_timepoint <- range_ref[1] + overlapping_percent * diff(range_ref) - stretch_upper * diff(range_query)
    max_timepoint <- range_ref[2] - overlapping_percent * diff(range_ref) + stretch_upper * diff(range_query)

    # Calculate limits
    shift_lower <- min_timepoint - stretch_upper * range_query[1]
    shift_upper <- max_timepoint - stretch_lower * range_query[1]
  }

  # Calculate initial value (centered curves)
  if (calc_mode %in% c("auto", "bound")) {
    midpoint_ref <- range_ref[1] + diff(range_ref) * 0.5
    midpoint_query_init <- range_query[1] * stretch_space_lims$stretch_init + range_query_init_stretch * 0.5
    shift_init <- midpoint_ref - midpoint_query_init
  } else {
    shift_init <- shifts
  }

  # Results object
  results_list <- list(
    shift_init = shift_init,
    shift_lower = shift_lower,
    shift_upper = shift_upper
  )

  return(results_list)
}

#' Calculate overlapping percentage between reference and query data time point ranges
#'
#' @noRd
calc_overlapping_percent <- function(data) {
  # Suppress "no visible binding for global variable" note
  accession <- NULL
  timepoint <- NULL

  # Extract time point ranges
  range_ref <- range(unique(data[accession == "ref", timepoint]))
  range_query <- range(unique(data[accession == "query", timepoint]))

  # Calculate how much of ref is contained in query
  overlap <- min(c(range_ref[2], range_query[2])) - max(c(range_ref[1], range_query[1]))
  overlapping_percent <- overlap / diff(range_ref)

  return(overlapping_percent)
}

#' Combine different registration results
#'
#' @noRd
bind_results <- function(...) {
  # Parse arguments
  args <- list(...)
  if (length(args) == 1 && is.list(args[[1]])) {
    results <- unlist(args, recursive = FALSE)
  } else {
    results <- args
  }

  # Check accessions
  ref <- unique(sapply(results, function(x) attr(x$data, "ref")))
  query <- unique(sapply(results, function(x) attr(x$data, "query")))

  if (any(length(ref) > 1, length(query) > 1)) {
    stop(
      cli::format_error(c(
        "{.var ref} and {.var query} must be unique",
        "x" = "Your data contained {ref} for {.var ref} and {query} for {.var query}."
      )),
      call. = FALSE
    )
  }

  # Bind results
  data <- data.table::rbindlist(lapply(results, function(x) x$data))
  model_comparison <- data.table::rbindlist(lapply(results, function(x) x$model_comparison))

  # Add accession values as data attributes
  data.table::setattr(data, "ref", ref)
  data.table::setattr(data, "query", query)

  # Parse fun_args
  fun_args_list <- lapply(results, function(x) x$fun_args)
  fun_args <- lapply(
    unique(unlist(lapply(fun_args_list, names))),
    function(name) sapply(fun_args_list, function(x) x[[name]])
  )
  names(fun_args) <- unique(unlist(lapply(fun_args_list, names)))

  # Results object
  results_list <- list(
    data = data,
    model_comparison = model_comparison,
    fun_args = fun_args
  )

  return(new_res_greatR(results_list))
}
