### internal functions

# For printing - upper case first letter of each word
.title_case <- function(x) {
  sapply(x, function(word) {
    word_split <- strsplit(word, " ")
    paste(sapply(word_split, function(w) {
      paste0(toupper(substring(w, 1, 1)),
             tolower(substring(w, 2, nchar(w))))
    }), collapse = " ")
  })
}

# Computes the metainfo from the internal HCE data.
.compute_metainfo <- function(hce_dat) {
  n <- dplyr::n
  `%>%` <- dplyr::`%>%`

  meta1 <- hce_dat  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      n = n(),
      proportion = n / dim(hce_dat)[[1]] * 100,
      maxday = max(value, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      startx = c(0, cumsum(utils::head(proportion, -1))),
      endx = cumsum(proportion),
      starty = 0,
      n.groups = length(unique(outcome))
    ) %>%
    dplyr::ungroup()

  meta2 <- hce_dat  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome, arm) %>%
    dplyr::summarise(n = n(), proportion = n / dim(hce_dat)[[1]] * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("arm" = gsub(" ", "_", tolower(arm))) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(n, proportion),
                       values_fill = 0)

  meta_missing <- hce_dat %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      missing = sum(is.na(value))
    ) %>%
    dplyr::ungroup()

  meta <- dplyr::left_join(meta1, meta2, "outcome")
  meta <- dplyr::left_join(meta, meta_missing, "outcome")

  return(meta)
}

# Calculates the cumulative distribution for TTE outcomes
.compute_ecdf_by_outcome <- function(
  hce_dat, meta, step_outcomes, step_types,
  last_outcome, arm_levels, fixed_followup_days
) {

  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  # Calculate the number of unique step outcomes
  num_step_outcomes <- length(step_outcomes)

  # Vectorize fixed follow-up days if there is only one value provided
  # For binary values, follow-up time is always 2 (to create a jump
  # in the middle of the step at 1)
  if (length(fixed_followup_days) == 1) {
    fixed_followup_days <- sapply(step_types, function(type) {
      ifelse(type == "binary", 2, fixed_followup_days)
    }, USE.NAMES = FALSE)
  }

  # Every step outcome will be plotted over an x-axis range from 0
  # to the fixed_follow_up days associated with the outcome

  # Initialize the cumulative time-to-event (t_cdf) with a maximum
  # value that is higher than the sum of all x-axis range parts
  # The reason for this is that when fitting the individual step
  # parts, we will go chronological and want to make sure that all
  # steps at a later stage have been initialized with a later time
  hce_dat$t_cdf <- sum(fixed_followup_days) + 2 * max(fixed_followup_days)
  # Initialize step_values column recording the size of the step (percentage
  # of number t risk) at each time point
  hce_dat$step_values <- 0

  # Iterate over each step outcome
  for (i in seq_len(num_step_outcomes)) {

    # Filter rows by outcome
    idx <- hce_dat$outcome == step_outcomes[[i]]
    # By default the value recorded in the data is the actual time of the step.
    # Since we add concatenate different step functions, we need to update the
    # x-axis to reflect the time of the step plus the cumulation of all the
    # x-axis ranges of the previous step functions
    add_previous_end <- ifelse(i == 1, 0, sum(fixed_followup_days[1:(i - 1)]))
    hce_dat[idx, ]$t_cdf <- hce_dat[idx, ]$value + add_previous_end

    # Iterate over each treatment arm
    for (arm in arm_levels) {
      # Filter rows by outcome and arm
      idx <- hce_dat$outcome == step_outcomes[[i]] & hce_dat$arm == arm

      # Fit the ECDF to the above updated x-axis range for the
      # cumulative time-to-event by treatment arm
      hce_dat[idx, ]$step_values <-
        100 *
        stats::ecdf(hce_dat[hce_dat$arm == arm,
                    ]$t_cdf)(hce_dat[idx, ]$t_cdf)

    }

  }

  hce_ecdf <- hce_dat %>%
    dplyr::filter(outcome %in% step_outcomes) %>%
    unique()

  # Double-check that all combinations of treatment and outcome have
  # been included (not the case if one combination has no patients)
  poss_comb <- expand.grid("outcome" = step_outcomes,
                           "arm" = arm_levels)
  missing_row <- dplyr::anti_join(poss_comb,
                                  hce_ecdf[, c("outcome", "arm")],
                                  by = c("outcome", "arm"))

  # If there are missing rows, fill them in
  if (nrow(missing_row) > 0) {

    for (i in 1:num_step_outcomes) {
      # Check if current step outcome is missing
      if (step_outcomes[[i]] %in% missing_row$outcome) {
        tmp <- missing_row[missing_row$outcome == step_outcomes[[i]], ]
        # Determine step values based on previous step if available
        if (i == 1) {
          step_values <- 0
        } else {
          tmp2 <-  hce_ecdf[hce_ecdf$outcome == step_outcomes[i - 1] &
                              hce_ecdf$arm == tmp$arm, ]
          step_values <- max(tmp2$step_values)
        }
        # Fetch existing data for the same outcome but different arm
        tmp3 <-  hce_ecdf[hce_ecdf$outcome == step_outcomes[[i]] &
                            hce_ecdf$arm != tmp$arm, ]
        # Append missing row to the main data frame
        hce_ecdf <-
          rbind(hce_ecdf,
                data.frame(outcome = step_outcomes[[i]],
                           arm = tmp$arm,
                           t_cdf = mean(tmp3$t_cdf),
                           step_values = step_values,
                           value = 0))
      }
    }
  }

  # Order the data frame by step values
  hce_ecdf <- hce_ecdf[order(hce_ecdf$step_values), ]

  # Add names of set outcomes and associated type (tte or binary) to data
  endpoint <- data.frame("outcome" = step_outcomes,
                         "type" = step_types)
  hce_ecdf <- dplyr::left_join(hce_ecdf, endpoint,
                               by = "outcome")

  hce_ecdf$adjusted.time <- 0
  for (i in seq_len(num_step_outcomes)) {
    entry <- step_outcomes[i]
    outcome_filter <- hce_ecdf$outcome == entry
    hce_ecdf[outcome_filter, ]$adjusted.time <-
      meta[meta$outcome == entry, ]$startx +
      hce_ecdf[outcome_filter, ]$value /
      fixed_followup_days[i] *
      meta[meta$outcome == entry, ]$proportion
  }

  # Summarize maximum step values, type, and sum of events
  hce_ecdf_meta <- hce_ecdf %>%
    dplyr::group_by(arm, outcome) %>%
    dplyr::summarise(max = max(step_values, na.rm = TRUE),
      type = unique(type),
      sum.event = ifelse(type == "tte", n(),
                         unique(t_cdf))
    ) %>%
    dplyr::arrange(max) %>%
    dplyr::mutate(
      ecdf_end = utils::tail(max, 1)
    ) %>%
    dplyr::ungroup()

  return(list(
    data = hce_ecdf,
    meta = hce_ecdf_meta
  ))
}

# Support function for the range
.to_rangeab <- function(x, start_continuous_endpoint, minval, maxval) {
  (100 - start_continuous_endpoint) * (x - minval) /
    (maxval - minval) + start_continuous_endpoint
}

.logTicks <- function(range) {
  a <- floor(log2(range[1]))
  b <- ceiling(log2(range[2]))
  steps <- unique(round(pretty(c(a, b))))
  return((2 ^ steps))
}

.log10Ticks <- function(range) {
  if (range[1] <= 0) {
    range[1] <- 0.0000001
  }
  range <- log10(range)
  get_axp <- function(x) 10^c(floor(x[1]), ceiling(x[2]))
  n <- ifelse(range[2] > 4, 1, 2)
  steps <- graphics::axTicks(side = 1, usr = range, axp = c(get_axp(range),
                                                            n = n),
                             log = TRUE)
  return((steps))
}

# Computes the continuous information
.compute_continuous <- function(
    hce_dat, meta, ecdf_mod, step_outcomes, last_outcome, arm_levels) {
  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  ctrl <- unname(arm_levels["control"])

  continuous_data <- hce_dat[hce_dat$outcome == last_outcome, ]
  start_continuous_endpoint <- meta[meta$outcome == last_outcome, ]$startx

  continuous_data$x <- .to_rangeab(
    continuous_data$value,
    start_continuous_endpoint,
    min(continuous_data$value, na.rm = TRUE),
    max(continuous_data$value, na.rm = TRUE)
  )
  continuous_meta <- continuous_data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(n = n(), median = stats::median(x, na.rm = TRUE),
                     average = base::mean(x, na.rm = TRUE)) %>%
    dplyr::ungroup()

  continuous_data$y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == unname(arm_levels["active"]) &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end
  continuous_data[continuous_data$arm == ctrl, ]$y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == ctrl &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end

  return(list(
    data = continuous_data,
    meta = continuous_meta
  ))
}

# Computes the binary information
.compute_binary <- function(
    hce_dat, meta, ecdf_mod, step_outcomes, last_outcome, arm_levels) {

  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  # Extract the active and control arm treatment names
  actv <- unname(arm_levels["active"])
  ctrl <- unname(arm_levels["control"])

  # Retrieve hce data for the last outcome as well as the x-axis position
  # to start from
  binary_data <- hce_dat[hce_dat$outcome == last_outcome, ]
  start_binary_endpoint <- meta[meta$outcome == last_outcome, ]$startx

  # Get the y-values that the step outcomes ended on for both arms
  actv_y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == actv &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end
  ctrl_y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == ctrl &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end

  # Calculate difference of proportion statistics for each arm (estimate
  # and lower confidence interval boundary) using prop.test
  # Note: we are using percentages rather than proportions (*100)
  binary_meta <- binary_data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(n = n(),
                     x = base::sum(value, na.rm = TRUE),
                     estimate = 100 *
                       as.numeric(stats::prop.test(x, n)$estimate),
                     ci_diff = abs(estimate -
                         (100 * as.numeric(stats::prop.test(x, n)$conf.int)[1])
                     )) %>%
    dplyr::ungroup()

  # To create ellipsis shape and avoid overlapping between both of them,
  # set the height to 80% of the CI (minimum scaled in x-axis or y-axis range)
  width <- (100 - start_binary_endpoint) * min(binary_meta$ci_diff) / 100
  y_range <- (max(actv_y, ctrl_y) + 10)  * (width / 100)
  y_height <- min(c(0.4 * abs(actv_y - ctrl_y), 0.8 * min(width, y_range)))

  # Create ellipsis centered around proportion estimate (x0) as well as
  # y-value that the step outcomes ended on for each arm,
  # with the standard error as width and the height as calculated above
  actv_point <-
    .create_ellipsis_points(unlist(binary_meta[binary_meta$arm == actv,
                                               "estimate"]),
                            actv_y,
                            unlist(binary_meta[binary_meta$arm == actv,
                                               "ci_diff"]),
                            y_height)
  ctrl_point <-
    .create_ellipsis_points(unlist(binary_meta[binary_meta$arm == ctrl,
                                               "estimate"]),
                            ctrl_y,
                            unlist(binary_meta[binary_meta$arm == ctrl,
                                               "ci_diff"]),
                            y_height)

  binary_data <- rbind(data.frame("outcome" = last_outcome,
                                  "arm" = actv,
                                  "value" = 1,
                                  actv_point),
    data.frame("outcome" = last_outcome,
               "arm" = ctrl,
               "value" = 1,
               ctrl_point)
  )

  lowest_value <- binary_meta$estimate - binary_meta$ci_diff
  highest_value <- binary_meta$estimate + binary_meta$ci_diff
  x_range <- c(min(0, floor(lowest_value / 10) * 10),
               max(100, ceiling(highest_value / 10) * 10))

  binary_data$x <- .to_rangeab(
    binary_data$x,
    start_binary_endpoint,
    x_range[1],
    x_range[2]
  )

  binary_meta$average <- .to_rangeab(
    binary_meta$estimate,
    start_binary_endpoint,
    x_range[1],
    x_range[2]
  )

  binary_meta$y <- 0
  binary_meta[binary_meta$arm == actv, "y"] <- actv_y
  binary_meta[binary_meta$arm == ctrl, "y"] <- ctrl_y

  return(list(
    data = binary_data,
    meta = binary_meta
  ))
}

# Create ellipsis centered around point (x0,y0),
# with range (x0+a,y0+b)
.create_ellipsis_points <- function(x0, y0, a, b) {

  # First create equally spaced points on a unit
  # circle (with x-coordinates cos_p and y-coordinates
  # sin_p), ranging from -1 to 1
  points <- seq(0, 2 * pi, length.out = 361)
  cos_p <- cos(points)
  sin_p <- sin(points)
  # Change the shape by changing the x-axis range (to 2*a)
  # and y axis range (to 2*b)
  x_tmp <- abs(cos_p) * a * sign(cos_p)
  y_tmp <- abs(sin_p) * b * sign(sin_p)
  # Move x and y values to be centered around x0 and y0
  edata <- data.frame(x = x0 + x_tmp, y = y0 + y_tmp)

  return(edata)

}

# Reformats the data coming in from outside so that it fits our expectation.
.reformat_and_check_data <- function(
    data, step_outcomes, last_outcome, arm_levels, column_names) {
  `%>%` <- dplyr::`%>%`
  vars <- dplyr::vars
  all_of <- dplyr::all_of

  hce_dat <- data %>%
    dplyr::rename(all_of(column_names)) %>%
    dplyr::select(all_of(names(column_names)))

  # Make sure outcome and arm columns are not factors
  hce_dat$outcome <- as.character(hce_dat$outcome)
  hce_dat$arm <- as.character(hce_dat$arm)

  endpoints <- c(step_outcomes, last_outcome)

  if (!all(as.character(unique(hce_dat[, "arm"])) %in%
             unname(arm_levels))) {
    stop(paste("Arm variable contains different values",
               "then given in parameter arm_levels"))
  }
  if (!all(as.character(unique(hce_dat[, "outcome"])) %in%
             unname(endpoints))) {
    stop(paste("Outcome variable contains different values",
               "then given in parameters step_outcomes and",
               "last_outcome"))
  }

  hce_dat <- hce_dat %>%
    dplyr::filter(outcome %in% endpoints) %>%
    dplyr::mutate_at(vars(outcome), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(arm), factor,
                     levels = c(arm_levels)[c("active", "control")])

  # Check if the endpoints are all present
  for (entry in c(step_outcomes, last_outcome)) {
    if (!any(hce_dat$outcome == entry)) {
      stop(paste(
        "Outcome", entry, "is not present in column",
        column_names[["outcome"]]
      ))
    }
  }

  return(hce_dat)

}

.minor_grid <- function(values, scale, continuous_grid_spacing_x) {
  minval <- min(values, na.rm = TRUE)
  maxval <- max(values, na.rm = TRUE)

  minor_grid_left <- c(0)
  if ((10^scale) * floor(minval * 10^(-scale)) < 0) {
    minor_grid_left <- rev(seq(
      0,
      (10^scale) * floor(minval * 10^(-scale)),
      by = -continuous_grid_spacing_x
    ))
  }

  minor_grid_right <- c(0)
  if ((10^scale) * ceiling(maxval * 10^(-scale)) > 0) {
    minor_grid_right <- seq(
      0,
      (10^scale) * ceiling(maxval * 10^(-scale)),
      by = continuous_grid_spacing_x
    )
  }
  minor_grid <- unique(c(minor_grid_left, minor_grid_right))
  minor_grid <- minor_grid[minor_grid >= minval & minor_grid <= maxval]

  return(minor_grid)
}

.maraca_from_hce_data <- function(x, step_outcomes, last_outcome, arm_levels,
                                  fixed_followup_days, compute_win_odds,
                                  step_types = "tte",
                                  last_type = "continuous",
                                  lowerBetter = FALSE) {

  checkmate::assert_string(last_outcome)
  checkmate::assert_names(names(x),
                          must.include = c("GROUP", "TRTP", "AVAL0"))

  checkmate::assert_names(
    names(arm_levels),
    permutation.of = c("active", "control")
  )

  checkmate::assert_flag(compute_win_odds)

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  if (is.null(step_outcomes)) {
    if (!(last_outcome %in% x$GROUP)) {
      stop(paste("last_outcome", last_outcome,
                 "is not in the outcome variable"))
    }
    step_outcomes <- sort(unique(x$GROUP)[unique(x$GROUP) != last_outcome])
  }

  # Small bugfix to allow for name change of variable TTEFixed in newer
  # version of HCE package
  if ("PADY" %in% names(x)) {
    x$TTEfixed <- x$PADY
  }

  if (is.null(fixed_followup_days)) {
    checkmate::assertNames(names(x), must.include = "TTEfixed")
    checkmate::assert_integerish(x$TTEfixed)

    fixed_followup_days <- unname(sapply(step_outcomes, function(tte) {
      x[x$GROUP == tte, "TTEfixed"][[1]]
    }))
  }

  maraca_obj <- maraca(
    data = x,
    step_outcomes = step_outcomes,
    last_outcome = last_outcome,
    column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
    arm_levels = arm_levels,
    fixed_followup_days = fixed_followup_days,
    compute_win_odds = compute_win_odds,
    step_types = step_types,
    last_type = last_type,
    lowerBetter = lowerBetter
  )

  return(maraca_obj)
}

.checks_continuous_outcome <- function(density_plot_type,
                                       vline_type) {
  checkmate::assert_choice(
    density_plot_type, c("default", "violin", "box", "scatter")
  )

  if (!(is.null(vline_type) ||
          checkmate::testString(vline_type))) {
    stop("vline_type has to be a string or NULL")
  }

  if (is.null(vline_type)) {
    vline_type <- "median"
  } else {
    checkmate::assert_choice(
      vline_type, c("median", "mean", "none")
    )
  }

  return(vline_type)
}

.checks_binary_outcome <- function(density_plot_type,
                                   vline_type) {
  checkmate::assert_choice(
    density_plot_type, c("default")
  )

  if (!(is.null(vline_type) ||
          checkmate::testString(vline_type))) {
    stop("vline_type has to be a string or NULL")
  }

  if (is.null(vline_type)) {
    vline_type <- "mean"
  } else {
    checkmate::assert_choice(
      vline_type, c("mean", "none")
    )
  }

  return(vline_type)
}
