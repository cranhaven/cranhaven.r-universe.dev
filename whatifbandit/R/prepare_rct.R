#' @name prep_rct_data
#' @title Pre-Simulation Setup to Simulate a MAB Trial From an RCT

#' @description Common function for all the actions that need to take place before
#' running the Multi-Arm-Bandit re-simulation. Intakes the data and column names to
#' check for valid arguments, format and create new columns as needed, and pre-compute
#' key values to avoid doing so within the simulation loop.
#' @param blocking Logical; Whether or not treatment blocking is occurring
#' @param clustering Logical; Whether or not treatment clustering is occurring
#' @inheritParams mab_from_rct
#' @param col_names List holding the columns required from the provided data as strings and symbols.
#'
#' @returns Named list containing:
#' \itemize{
#' \item `data`: Prepared `data.frame` or `data.table` containing all the necessary columns to
#' conduct the adaptive trial simulation, subset from the originally provided data to reduce memory usage.
#' columns required for [run_mab()].
#' \item `conditions`: character vector of treatment arm names, named to reflect treatment or
#' control status.
#' \item `imputation_information`: List containing necessary information
#' for outcome and date imputation for [run_mab()].
#' \item `period_idxs`: List of numeric vectors containing  period start and end indexes.
#' }
#' @details
#'  If a `data.table` is passed it is copied to avoid modifying the
#' original dataset in the users environment.

#'
#' @keywords internal

prep_rct_data <- function(
  data,
  random_assign_prop,
  control_augment,
  control_condition,
  period_method,
  time_unit,
  period_length,
  prior_periods,
  discount_rate,
  col_names,
  delayed_feedback,
  whole_experiment,
  verbose,
  ndraws,
  check_args,
  r,
  keep_data,
  keep_models,
  blocking,
  clustering
) {
  if (is.null(data) || !is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }
  if (data.table::is.data.table(data)) {
    data <- data.table::copy(data)
  }

  # Input Validation
  if (check_args) {
    check_rct_args(
      data = data,
      random_assign_prop = random_assign_prop,
      control_augment = control_augment,
      period_method = period_method,
      time_unit = time_unit,
      period_length = period_length,
      prior_periods = prior_periods,
      discount_rate = discount_rate,
      col_names = col_names,
      delayed_feedback = delayed_feedback,
      whole_experiment = whole_experiment,
      verbose = verbose,
      ndraws = ndraws,
      r = r,
      keep_data = keep_data,
      keep_models = keep_models,
      blocking = blocking
    )
  }
  conditions <- create_conditions(
    control_condition = control_condition,
    data = data,
    condition_col = col_names$condition_col,
    control_augment = control_augment
  )

  # Preparing Data to be simulated
  verbose_log(verbose, "Preparing Data")
  vars_keep <- c(unlist(col_names), "period_number") |> unname()

  data <- create_cutoff(
    data = data,
    col_names = col_names,
    period_length = period_length,
    period_method = period_method,
    time_unit = time_unit
  ) |>
    create_new_cols(
      col_names = col_names,
      delayed_feedback = delayed_feedback,
      blocking = blocking,
      vars_keep = vars_keep
    )
  # Pre-computing Important values to be accessed for the simulation
  verbose_log(verbose, "Precomputing")

  imputation_information <- precompute_imputation(
    data = data,
    whole_experiment = whole_experiment,
    col_names = col_names,
    delayed_feedback = delayed_feedback
  )

  period_sizes <- compute_period_sizes(data)
  end_idxs <- cumsum(period_sizes)
  start_idxs <- c(1, end_idxs[-length(period_sizes)] + 1)
  if (clustering) {
    check_clusters(data, cluster_col = col_names$cluster_col)
  }

  return(list(
    data = data,
    imputation_information = imputation_information,
    conditions = conditions,
    period_idxs = list(start_idxs = start_idxs, end_idxs = end_idxs)
  ))
}
#---------------------------------------------------------------------------------
#' @describeIn prep_rct_data This function creates a character vector of treatment conditions
#' using the conditions column in the provided data, and if `control_augment` is greater
#' than 0, it also labels the control condition. Throws an error of `control_condition` is not
#' present.
#' @returns Character vector of unique treatment conditions. Throws error if an invalid specification
#' is used.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @keywords internal
create_conditions <- function(
  control_condition,
  data,
  condition_col,
  control_augment
) {
  conditions <- sort(as.character(unique(data[[
    condition_col
  ]])))
  if (!is.null(control_condition)) {
    if (length(control_condition) != 1) {
      rlang::abort(c(
        "`control_condition` must have a length of 1",
        "x" = sprintf(
          "You passed a vector of length: %d",
          length(control_condition)
        )
      ))
    }
    if (
      is.null(control_condition) ||
        is.na(control_condition) ||
        !as.character(control_condition) %in% conditions
    ) {
      rlang::abort(c(
        "`control_condition` is not present in the conditions column",
        "x" = sprintf(
          "Potential Conditions: %s",
          paste0(conditions, collapse = ", ")
        ),
        "x" = paste0("You Passed: ", deparse(control_condition))
      ))
    }

    names(conditions) <- ifelse(
      conditions == as.character(control_condition),
      "control",
      "treatment"
    )
  }
  return(conditions)
}
#' Create Treatment Wave Cutoffs
#' @name create_cutoff
#' @description Used to assign each observation a new treatment assignment period, based
#' on user-supplied specifications, and user supplied data from
#' `date_col` and `month_col` in `col_names`, and the `period_length`. Creates a new
#' column indicating with period each observation belongs to.
#'
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @details
#' The assignment periods do not strictly have to line up with the original experiment, it
#' is up to the researcher to test the possible options.
#'
#' Month based assignment can be specified either using the months inside the `month_col` or `date_col`,
#' if `month_col` is passed into the function it will be used.
#'
#' @returns Updated `tibble`/`data.table` with the new `period_number` column. `period_number` is an integer
#' representing an observation's new assignment period.
#' @keywords internal
#------------------------------------------------------------------------------------------
create_cutoff <- function(
  data,
  col_names,
  period_length = NULL,
  period_method,
  time_unit
) {
  data <- switch(
    period_method,
    "individual" = create_cutoff.individual(data = data),
    "batch" = create_cutoff.batch(data = data, period_length = period_length),
    "date" = create_cutoff.date(
      data = data,
      period_length = period_length,
      date_col = col_names$date_col,
      month_col = col_names$month_col,
      time_unit = time_unit
    ),
    rlang::abort(
      "Invalid Assignment Method: valid methods are `individual`, `batch`, `date`"
    )
  )
  return(invisible(data))
}
#------------------------------------------------------------------------------------------
#' @rdname create_cutoff
create_cutoff.date <- function(
  data,
  time_unit,
  date_col,
  month_col,
  period_length
) {
  time_length <- switch(
    time_unit,
    "day" = lubridate::days(1),
    "week" = lubridate::weeks(1),
    "month" = months(1)
  )
  start_date <- min(data[[date_col]])

  if (data.table::is.data.table(data)) {
    if (time_unit == "month" && !is.null(month_col)) {
      first_month <- data[
        order(get(date_col)),
        ..month_col
      ][1]

      start_month <- lubridate::ymd(paste0(
        lubridate::year(start_date),
        "-",
        first_month,
        "-01"
      ))

      data[,
        month_date := lubridate::ymd(
          paste0(
            lubridate::year(get(date_col)),
            "-",
            get(month_col),
            "-01"
          )
        )
      ]
      data[,
        period_number := floor(
          lubridate::interval(start_month, month_date) /
            months(1) /
            period_length
        ) +
          1
      ]
      data[, month_date := NULL]

      data.table::setkey(data, period_number)
      data.table::setorderv(data, cols = c(date_col, "period_number"))
    } else {
      data[,
        period_number := floor(
          lubridate::interval(start_date, get(date_col)) /
            time_length /
            period_length
        ) +
          1
      ]
      data.table::setkey(data, period_number)
      data.table::setorderv(data, cols = c(date_col, "period_number"))
    }
  } else {
    if (time_unit == "month" && !is.null(month_col)) {
      first_month <- data |>
        dplyr::slice_min(
          order_by = .data[[date_col]],
          n = 1,
          with_ties = FALSE
        ) |>
        dplyr::pull(.data[[month_col]])

      start_month <- lubridate::ymd(
        paste0(lubridate::year(start_date), "-", first_month, "-01")
      )
      data <- data |>
        dplyr::mutate(
          month_date = lubridate::ymd(paste0(
            lubridate::year(.data[[date_col]]),
            "-",
            .data[[month_col]],
            "-01"
          )),
          period_number = floor(
            lubridate::interval(start_month, month_date) /
              months(1) /
              period_length
          ) +
            1
        ) |>
        dplyr::select(-month_date) |>
        dplyr::arrange(.data[[date_col]], period_number)
    } else {
      data <- data |>
        dplyr::mutate(
          period_number = floor(
            lubridate::interval(start_date, .data[[date_col]]) /
              time_length /
              period_length
          ) +
            1
        ) |>
        dplyr::arrange(.data[[date_col]], period_number)
    }
  }
  return(data)
}

#--------------------------------------------------------------------------
#' @rdname create_cutoff
create_cutoff.individual <- function(data) {
  if (data.table::is.data.table(data)) {
    data[, period_number := .I]
    data.table::setkey(data, period_number)
    data.table::setorder(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(period_number = dplyr::row_number()) |>
      dplyr::arrange(period_number)
    return(data)
  }
}
#----------------------------------------------------------------------------------
#' @rdname create_cutoff
create_cutoff.batch <- function(data, period_length) {
  if (data.table::is.data.table(data)) {
    data[, period_number := ceiling((.I / period_length))]
    data.table::setkey(data, period_number)
    data.table::setorder(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(
        period_number = ceiling(dplyr::row_number() / period_length)
      ) |>
      dplyr::arrange(period_number)
    return(data)
  }
}
#------------------------------------------------------------------------------------
#' @title Create Necessary Columns for Multi-Arm Bandit Trial
#' @name create_new_cols
#' @description Initializes partially empty columns in `data` to initialize them for the simulation.
#' These are initialized as `NA` except for observations with `period_number` = 1, whose values are copied
#' from the provided columns, and used as the starting point for the simulation.
#'
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @param vars_keep Character vector of variables to keep
#'
#' @returns A `data.frame`/`data.table` subsetted to all the user provided columns plus these 6 new columns:
#' \itemize{
#' \item `mab_success`: New variable to hold new success from Multi-arm bandit procedure, NA until assigned.
#' \item `mab_condition`: New variable to hold new treatment condition from Multi-arm bandit procedure, NA until assigned.
#' \item `impute_req`: Binary indicator for imputation requirement, NA until assigned.
#' \item `new_success_date`: New variable to hold the new success date under Multi-arm bandit procedure, NA until assigned.
#' \item `block`: New variable indicating the variables to block by for assignment.
#' \item `treatment_block`: New variable combining block with original treatment condition.
#' }
#'
#' @keywords internal
create_new_cols <- function(
  data,
  col_names,
  blocking,
  delayed_feedback,
  vars_keep
) {
  UseMethod("create_new_cols", data)
}
# --------------------------------------------------
#' @method create_new_cols data.frame
#' @rdname create_new_cols
#' @export

create_new_cols.data.frame <- function(
  data,
  col_names,
  blocking,
  delayed_feedback,
  vars_keep
) {
  data <- data |>
    dplyr::select(dplyr::all_of(vars_keep)) |>
    dplyr::mutate(
      period_number = match(
        period_number,
        sort(unique(period_number))
      ),
      mab_condition = dplyr::if_else(
        period_number == 1,
        as.character(.data[[col_names$condition_col]]),
        NA_character_
      ),
      mab_success = dplyr::if_else(
        period_number == 1,
        .data[[col_names$success_col]],
        NA_real_
      ),
      impute_req = dplyr::if_else(period_number == 1, 0, NA),
      impute_block = NA_character_
    )

  if (delayed_feedback) {
    data <- data |>
      dplyr::mutate(
        new_success_date = dplyr::if_else(
          period_number == 1,
          .data[[col_names$success_date_col]],
          NA
        )
      )
  }

  if (blocking) {
    data <- data |>
      dplyr::mutate(
        block = do.call(
          paste,
          c(data[, col_names$block_cols], sep = "_")
        ),
        treatment_block = do.call(
          paste,
          c(
            data[, c(col_names$condition_col, col_names$block_cols)],
            sep = "_"
          )
        )
      )
  } else {
    data <- data |>
      dplyr::mutate(
        treatment_block = as.character(.data[[col_names$condition_col]])
      )
  }

  return(data)
}
#---------------------------------------------------------------------------------
#' @rdname create_new_cols
#' @method create_new_cols data.table
#' @export

create_new_cols.data.table <- function(
  data,
  col_names,
  blocking,
  delayed_feedback,
  vars_keep
) {
  data <- data[, .SD, .SDcols = unique(vars_keep)]
  data[,
    period_number := match(
      period_number,
      sort(unique(period_number))
    )
  ][
    period_number == 1,
    `:=`(
      mab_condition = as.character(
        get(col_names$condition_col)
      ),
      mab_success = get(col_names$success_col),
      impute_req = 0,
      impute_block = NA_character_
    )
  ]
  if (delayed_feedback) {
    data[
      period_number == 1,
      new_success_date := get(col_names$success_date_col)
    ]
  }
  if (blocking) {
    data[,
      block := do.call(paste, c(.SD, sep = "_")),
      .SDcols = col_names$block_cols
    ]
    data[,
      treatment_block := do.call(paste, c(.SD, sep = "_")),
      .SDcols = c(col_names$condition_col, col_names$block_cols)
    ]
  } else {
    data[,
      treatment_block := as.character(get(
        col_names$condition_col
      ))
    ]
  }
  return(invisible(data))
}

#' @title Compute exact period sizes
#' @name compute_period_sizes
#' @inheritParams mab_from_rct
#' @returns Numeric vector of `length(max(period_nummber))` with each element representing the number of units in each period.
#' @keywords internal
compute_period_sizes <- function(
  data
) {
  UseMethod("compute_period_sizes", data)
}

#' @method compute_period_sizes data.frame
#' @rdname compute_period_sizes
#' @export
compute_period_sizes.data.frame <- function(data) {
  data |>
    dplyr::group_by(period_number) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::arrange(period_number) |>
    dplyr::pull(count)
}

#' @method compute_period_sizes data.table
#' @rdname compute_period_sizes
#' @export
compute_period_sizes.data.table <- function(data) {
  counts <- data[, .(count = .N), by = period_number][order(
    period_number
  )]
  counts[["count"]]
}
