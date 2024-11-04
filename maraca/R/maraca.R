#' @description Creates the maraca analysis object as an S3 object of
#' class 'maraca'.
#'
#' @param data A data frame with columns for the following information:
#'             - outcome column, containing the time-to-event and continuous
#'               labels
#'             - arm column, containing the arm a given row belongs to.
#'             - value column, containing the values.
#' @param step_outcomes A vector of strings containing the outcome labels
#'                      for all outcomes displayed as part of the step function
#'                      on the left side of the plot.
#'                      The order is kept for the plot.
#' @param last_outcome A single string containing the last outcome label
#'                     displayed on the right side of the plot.
#' @param arm_levels A named vector of exactly two strings, mapping the
#'                   values used for the active and control arms to the values
#'                   used in the data. The names must be "active" and "control"
#'                   in this order. Note that this parameter only need to
#'                   be specified if you have labels different from
#'                    "active" and "control".
#' @param column_names A named vector to map the
#'        outcome, arm, value to the associated column names
#'        in the data. The vector names must match in order "outcome", "arm",
#'        and "value". Note that this parameter only need to be
#'        specified if you have column names different from the ones above.
#' @param fixed_followup_days A mandatory specification of the fixed follow-up
#'                            days in the study. Can be a single integer value
#'                            for all tte-outcomes or a vector with one
#'                            integer value per tte-outcome.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @param step_types The type of each outcome in the step_outcomes vector.
#'                   Can be a single string (if all outcomes of same type) or
#'                   a vector of same length as step_outcomes. Possible values
#'                   in the vector are "tte" (default) or "binary".
#' @param last_type A single string giving the type of the last outcome.
#'                  Possible values are "continuous" (default), "binary" or
#'                  "multinomial".
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param tte_outcomes Deprecated and substituted by the more general
#'                     'step_outcomes'. A vector of strings containing the
#'                     time-to-event outcome labels. The order is kept for the
#'                     plot.
#' @param continuous_outcome Deprecated and substituted by the more general
#'                           'last_outcome'. A single string containing the
#'                           continuous outcome label.
#' @return An object of class 'maraca'. The object information must be
#'         considered private.
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' @export
maraca <- function(
  data,
  step_outcomes,
  last_outcome,
  arm_levels = c(
    active = "active",
    control = "control"
  ),
  column_names = c(
    outcome = "outcome",
    arm = "arm",
    value = "value"
  ),
  fixed_followup_days = NULL,
  compute_win_odds = FALSE,
  step_types = "tte",
  last_type = "continuous",
  lowerBetter = FALSE,
  tte_outcomes = lifecycle::deprecated(),
  continuous_outcome = lifecycle::deprecated()
) {

  checkmate::assert_data_frame(data)

  if (lifecycle::is_present(tte_outcomes)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(tte_outcomes)",
                              "maraca(step_outcomes)")
    step_outcomes <- tte_outcomes
  }

  if (lifecycle::is_present(continuous_outcome)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(continuous_outcome)",
                              "maraca(last_outcome)")
    last_outcome <- continuous_outcome
  }

  checkmate::assert_character(step_outcomes, any.missing = FALSE)
  checkmate::assert_string(last_outcome)
  checkmate::assert_character(arm_levels, len = 2, any.missing = FALSE)
  checkmate::assert_names(
    names(arm_levels),
    permutation.of = c("active", "control")
  )
  checkmate::assert_character(column_names, len = 3, any.missing = FALSE)
  checkmate::assert_names(
    names(column_names),
    permutation.of = c("outcome", "arm", "value")
  )

  checkmate::assert_numeric(fixed_followup_days)

  checkmate::assert_character(step_types)
  checkmate::assert_subset(step_types,
                           choices = c("tte", "binary"),
                           empty.ok = FALSE)

  if (!(length(step_types) %in% c(1, length(step_outcomes)))) {
    stop(paste("step_types needs to be either a single string or",
               "a vector with one value for each tte outcome"))
  }

  checkmate::assert_string(last_type)
  checkmate::assert_subset(last_type,
                           choices = c("continuous", "binary"),
                           empty.ok = FALSE)

  checkmate::assert_flag(lowerBetter)

  if (!(length(fixed_followup_days) %in%
          c(1, length(step_outcomes[step_types == "tte"])))) {
    stop(paste("fixed_followup_days needs to be either a single value or",
               "a vector with one value for each tte outcome"))
  }

  checkmate::assert_flag(compute_win_odds)

  `%>%` <- dplyr::`%>%`

  # Make sure that data is a data.frame
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Remove unwanted outcomes and arm levels, and normalise column names
  # in the internal data.
  # Note: We use HCE to refer to our internal, normalised data frame.
  # and with "data" to the user-provided, external, dirty data frame.
  hce_dat <- .reformat_and_check_data(data, step_outcomes,
                                      last_outcome,
                                      arm_levels, column_names)

  # Calculate meta information from the entire HCE dataset needed for plotting
  meta <- .compute_metainfo(hce_dat)

  # In the current implementation of the package,
  # the fixed follow-up days given cannot be smaller
  # than the follow-up times for all tte-outcomes in the
  # in the dataset - this has to do with the fact that
  # we don't have information on patients that had multiple
  # events of different severity - for example a patient
  # having a myocardial infarction on day 300 and dies day
  # 800 - if we now change follow-up time to 500, we will
  # discard the death event for this patient (after 500)
  # but will at the same time not include the MI since
  # we don't know about it
  if (any(fixed_followup_days <
            unlist(meta[meta$outcome %in% step_outcomes, "maxday"]))) {
    stop(paste("Time-to-event data contain events",
               "after the fixed_followup_days - either",
               "provide a longer follow-up time or",
               "re-derive your input dataset for the",
               "follow-up time provided."))
  }

  # Remove rows with missing values - previously done
  # automatically by survival package (should we done)
  # after the meta data is collected to keep information
  # on if missing data was removed
  hce_dat <- hce_dat %>%
    dplyr::filter(!is.na(value))

  # Vectorize step type if singular value
  if (length(step_types) == 1) {
    step_types <- rep(step_types, times = length(step_outcomes))
  }

  ecdf_by_outcome <- .compute_ecdf_by_outcome(
    hce_dat, meta, step_outcomes, step_types,
    last_outcome, arm_levels,
    fixed_followup_days
  )

  if (last_type == "continuous") {
    data_last_outcome <- .compute_continuous(
      hce_dat, meta, ecdf_by_outcome, step_outcomes, last_outcome, arm_levels
    )
  } else if (last_type == "binary") {
    data_last_outcome <- .compute_binary(
      hce_dat, meta, ecdf_by_outcome, step_outcomes, last_outcome, arm_levels
    )
  } else if (last_type == "multinomial") {
    data_last_outcome <- NULL
  }

  win_odds <- list("win_odds" = NULL, "win_odds_outcome" = NULL,
                   "wins_forest" = NULL, "wo_bar" = NULL)
  if (compute_win_odds) {
    win_odds <- .compute_win_odds(hce_dat, arm_levels,
                                  step_outcomes, last_outcome,
                                  lowerBetter)
  }

  return(
    structure(
      list(
        step_outcomes = step_outcomes,
        last_outcome = last_outcome,
        step_types = step_types,
        last_type = last_type,
        arm_levels = arm_levels,
        fixed_followup_days = fixed_followup_days,
        column_names = column_names,
        meta = meta,
        ecdf_by_outcome = ecdf_by_outcome,
        data_last_outcome = data_last_outcome,
        win_odds = win_odds[["win_odds"]],
        win_odds_outcome = win_odds[["win_odds_outcome"]],
        wins_forest = win_odds[["wins_forest"]],
        wo_bar = win_odds[["wo_bar"]],
        lowerBetter = lowerBetter
      ),
      class = c("maraca")
    )
  )
}

#' @param x an object of class maraca
#' @param ... further arguments passed to or
#' from other methods.
#' @method print maraca
#' @rdname maraca
#' @export
print.maraca <- function(x, ...) {

  `%>%` <- dplyr::`%>%`

  cat(paste("Maraca object for plotting maraca graph created for",
            sum(x$meta$n), "patients.\n\n"))

  if (sum(x$meta$missing) > 0) {
    cat(paste(sum(x$meta$missing),
              "patient(s) removed because of missing values.\n\n"))
  }

  if (!is.null(x$win_odds)) {
    cat(paste0("Win odds (95% CI): ", round(x$win_odds[[1]], 2),
               " (", round(x$win_odds[[2]], 2), ", ",
               round(x$win_odds[[3]], 2), ")", "\n",
               "Win odds p-value: ",
               format.pval(x$win_odds[[4]], digits = 3, eps = 0.001), "\n\n"))
  } else {
    cat("Win odds not calculated.\n\n")
  }

  tmp <- x$meta %>%
    dplyr::select(outcome, n, proportion,
                  dplyr::starts_with("n_"), missing) %>%
    as.data.frame()
  names(tmp) <- .title_case(gsub("_", " ", names(tmp)))
  print(tmp, row.names = FALSE)

}


#' Creates and returns the plot of the maraca data.
#'
#' @param obj an object of S3 class 'maraca'
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type which type of plot to display in the continuous
#'        part of the plot. Options are "default", "violin", "box", "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#' @return a ggplot2 object of the data. This function
#' will not render the plot immediately. You have to print() the returned
#' object for it to be displayed.
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' plot <- plot_maraca(hce_test)
#' @export
plot_maraca <- function(
    obj, continuous_grid_spacing_x = NULL,
    trans = c("identity", "log", "log10", "sqrt", "reverse")[1],
    density_plot_type = c("default", "violin", "box", "scatter")[1],
    vline_type = NULL,
    theme = "maraca") {

  checkmate::assert_class(obj, "maraca")

  if (!(is.null(continuous_grid_spacing_x) ||
          is.numeric(continuous_grid_spacing_x))) {
    stop("continuous_grid_spacing_x has to be numeric or NULL")
  }

  checkmate::assert_string(trans)
  checkmate::assert_subset(trans,
                           choices = c("identity", "log", "log10",
                                       "sqrt", "reverse"),
                           empty.ok = FALSE)

  aes <- ggplot2::aes
  `%>%` <- dplyr::`%>%`

  meta <- obj$meta
  step_outcomes <- obj$step_outcomes
  step_types <- obj$step_types
  which_tte <- which(step_types == "tte")
  which_binary <- which(step_types == "binary")
  last_data <- obj$data_last_outcome
  last_type <- obj$last_type

  if (last_type == "binary" && trans %in% c("log", "log10", "sqrt")) {
    stop(paste(trans, "transformation only implemented for continuous",
               "last endpoint."))
  }

  vline_type <-
    switch(last_type,
           "continuous" = .checks_continuous_outcome(density_plot_type,
                                                     vline_type),
           "binary" = .checks_binary_outcome(density_plot_type,
                                             vline_type),
           stop("Unsupported last outcome type"))

  ecdf_mod <- obj$ecdf_by_outcome
  win_odds <- obj$win_odds
  start_last_endpoint <-
    meta[meta$outcome == obj$last_outcome, ]$startx

  if (is.null(continuous_grid_spacing_x)) {
    continuous_grid_spacing_x <- 10
  }

  plotdata_ecdf <- ecdf_mod$data[, c("outcome", "arm", "value",
                                     "adjusted.time", "step_values",
                                     "type")]
  names(plotdata_ecdf) <- c("outcome", "arm", "value", "x", "y", "type")
  plotdata_last <- last_data$data[, c("outcome", "arm", "value", "x", "y")]
  plotdata_last$type <- last_type

  # Add points at (0, 0) on both curves so that they start from the origin
  add_points <- plotdata_ecdf %>%
    dplyr::group_by(arm) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  add_points$x <- 0
  add_points$y <- 0
  plotdata_ecdf <- rbind(
    add_points,
    plotdata_ecdf
  )

  plotdata_ecdf <- plotdata_ecdf[order(plotdata_ecdf$x), ]

  # Add end point of previous curve to avoid jumps
  add_points <-
    do.call("rbind",
            lapply(2:length(step_outcomes),
                   function(i) {
                     plotdata_ecdf %>%
                       dplyr::group_by(arm) %>%
                       dplyr::filter(outcome == step_outcomes[i - 1]) %>%
                       dplyr::slice_tail(n = 1) %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(outcome = step_outcomes[i]) %>%
                       dplyr::ungroup()
                   }))

  plotdata_ecdf <- rbind(
    add_points,
    plotdata_ecdf
  )
  plotdata_ecdf <- plotdata_ecdf[order(plotdata_ecdf$x), ]

  # Add points at (100, y) on both curves so that they end at x=100%
  add_points <- plotdata_ecdf %>%
    dplyr::group_by(arm) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  add_points$x <- 100
  plotdata_ecdf <- rbind(
    plotdata_ecdf,
    add_points
  )

  plotdata_ecdf <- plotdata_ecdf[order(plotdata_ecdf$x), ]

  plotdata <- rbind(plotdata_ecdf, plotdata_last)

  scale <- sign(log10(continuous_grid_spacing_x)) * floor(
    abs(log10(continuous_grid_spacing_x))
  )

  if (last_type == "continuous") {

    range <- c(min(plotdata_last$value, na.rm = TRUE),
               max(plotdata_last$value, na.rm = TRUE))

    if (trans %in% c("log", "log10", "sqrt")) {
      minor_grid <- switch(trans,
                           "log" = .logTicks(range),
                           "log10" = .log10Ticks(range),
                           "sqrt" = pretty(range))
      minor_grid <- minor_grid[minor_grid >= range[1] &
                                 minor_grid <= range[2]]
      minor_grid_x <- eval(parse(text = paste0(trans, "(minor_grid)")))
    } else {
      minor_grid <- .minor_grid(plotdata_last$value, scale,
                                continuous_grid_spacing_x)
      minor_grid_x <- minor_grid
    }

  } else if (last_type == "binary") {

    lowest_value <- last_data$meta$estimate - last_data$meta$ci_diff
    highest_value <- last_data$meta$estimate + last_data$meta$ci_diff
    range <- c(min(0, floor(lowest_value / 10) * 10),
               max(100, ceiling(highest_value / 10) * 10))
    minor_grid <- seq(range[1], range[2], continuous_grid_spacing_x)
    minor_grid_x <- minor_grid

  }

  vline_data <- NULL
  if (vline_type == "median") {
    vline_data <- last_data$meta %>%
      dplyr::select("x" = median, arm)
  } else if (vline_type == "mean") {
    vline_data <- last_data$meta %>%
      dplyr::select("x" = average, arm)
  }

  if (trans %in% c("log", "log10", "sqrt")) {

    if (range[1] < 0) {
      warning(paste("Continuous endpoint has negative values - the",
                    trans, "transformation will result in missing values."))
    }
    plotdata_last$value <- eval(parse(text = paste0(trans,
                                                    "(plotdata_last$value)")))
    range <- c(min(plotdata_last$value, na.rm = TRUE),
               max(plotdata_last$value, na.rm = TRUE))
    plotdata_last$x <- .to_rangeab(plotdata_last$value, start_last_endpoint,
                                   range[1], range[2])

    if (!is.null(vline_data)) {
      vline_data$x <- eval(parse(text = paste0(trans, "(vline_data$x)")))
    }
  }

  if (trans == "reverse") {
    if (!is.null(win_odds) && !obj$lowerBetter) {
      message(paste("Last endpoint axis has been reversed, which might",
                    "indicate that lower values are considered advantageous.",
                    "Note that the win odds were calculated assuming that",
                    "higher values are better. If that is not correct, please",
                    "use the parameter lowerBetter = TRUE in the",
                    "maraca function."))
    }

    minor_grid_x <- rev(minor_grid_x)
    minor_grid <- rev(minor_grid)
    plotdata_last$x <- start_last_endpoint - plotdata_last$x + 100

    if (!is.null(vline_data)) {
      vline_data$x <- start_last_endpoint - vline_data$x + 100
    }
  }

  # Plot the information in the Maraca plot
  plot <- ggplot2::ggplot(plotdata) +
    ggplot2::geom_vline(
      xintercept = cumsum(c(0, meta$proportion)),
      color = "grey80"
    )

  if (!is.null(vline_data)) {
    plot <- plot +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = x,
          color = arm
        ),
        data = vline_data,
        linetype = "dashed",
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  for (outcome in step_outcomes[which_tte]) {
    plot <- plot +
      ggplot2::geom_step(data =
                           plotdata_ecdf[plotdata_ecdf$outcome == outcome, ],
                         aes(x = x, y = y, color = arm))
  }

  if (length(which_binary) > 0) {

    tmp <- plotdata_ecdf[plotdata_ecdf$outcome %in%
                           step_outcomes[which_binary], ]
    tmp <- tmp[order(tmp$x), ]

    if (step_types[length(step_types)] == "binary") {
      tmp <- dplyr::slice_head(tmp, n = -2)
    }

    tmp1 <- tmp %>%
      dplyr::group_by(outcome, arm) %>%
      dplyr::summarize("xend" = max(x),
                       "x" = min(x),
                       "y" = min(y)) %>%
      dplyr::ungroup()

    tmp2 <- tmp %>%
      dplyr::group_by(outcome, arm) %>%
      dplyr::summarize("x" = max(x),
                       "yend" = max(y),
                       "y" = min(y)) %>%
      dplyr::ungroup()

    plot <- plot +
      ggplot2::geom_segment(
        data = tmp1,
        aes(x = x, y = y, xend = xend, yend = y,
            color = arm)
      ) +
      ggplot2::geom_segment(
        data = tmp2,
        aes(x = x, y = y, xend = x, yend = yend,
            group = arm),
        color = "darkgrey", linetype = 2
      )
  }

  if (step_types[length(step_types)] == "binary") {

    tmp <- plotdata_ecdf %>%
      dplyr::filter(outcome == step_outcomes[length(step_types)]) %>%
      dplyr::group_by(arm) %>%
      dplyr::slice_tail(n = -1) %>%
      dplyr::summarize("xend" = max(x),
                       "x" = min(x),
                       "y" = max(y)) %>%
      dplyr::ungroup()

    plot <- plot +
      ggplot2::geom_segment(
        data = tmp,
        aes(x = x, y = y, xend = xend, yend = y,
            color = arm)
      )
  }

  if (density_plot_type == "default") {
    if (last_type == "continuous") {
      plot <- plot +
        ggplot2::geom_violin(
          data = plotdata_last,
          aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
        ) + ggplot2::geom_boxplot(
        data = plotdata_last,
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5,
        width =
          abs(diff(as.numeric(unique(plotdata_last$y)))) / 3
      )
    } else if (last_type == "binary") {
      plot <- plot +
        ggplot2::geom_polygon(data = plotdata_last,
                              ggplot2::aes(x = x, y = y, color = arm,
                                           fill = arm),
                              alpha = 0.5,
                              show.legend = FALSE) +
        ggplot2::geom_point(data = last_data$meta,
                            ggplot2::aes(x = average, y = y,
                                         color = arm))
    }
  } else if (density_plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(
        data = plotdata_last,
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      )
  } else if (density_plot_type == "box") {
    plot <- plot +
      ggplot2::geom_boxplot(
        data = plotdata_last,
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      )
  } else if (density_plot_type == "scatter") {
    plot <- plot +
      ggplot2::geom_jitter(
        data = plotdata_last,
        aes(x = x, y = y, color = arm),
        # Jittering only vertically, keep the correct x-value
        width = 0
      )
  }

  labels <- lapply(
    minor_grid,
    function(x) {
      s <- ifelse(scale > 0, 0, scale)
      return(as.character(round(x, -s + 1)))
    }
  )

  m_breaks <- .to_rangeab(
    minor_grid_x,
    start_last_endpoint,
    range[1],
    range[2]
  )

  if (trans == "reverse") {
    m_breaks <- start_last_endpoint - m_breaks + 100
  }

  plot <- plot +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(meta$proportion / 2 + meta$startx + 0.1),
      labels = c(obj$step_outcomes, obj$last_outcome),
      minor_breaks = m_breaks
    ) +
    ggplot2::annotate(
      geom = "text",
      x = m_breaks,
      y = 0,
      label = labels,
      color = "grey60"
    )

  if (!is.null(win_odds)) {

    plot <- .add_win_odds_to_plot(plot, win_odds, 0, Inf,
                                  hjust = 0)

    # Meta data on win odds will be added to plot
    win_odds <- unname(win_odds)
    params <- list(
      "win_odds" = win_odds[[1]],
      "lower_ci" = win_odds[[2]],
      "upper_ci" = win_odds[[3]],
      "p_value" = win_odds[[4]]
    )

    # Add win odds meta data as a label so retrievable
    plot$labels$win.odds <- params
  }

  plot <- switch(theme,
                 "maraca" = .theme_maraca(plot),
                 "maraca_old" = .theme_maraca_old(plot),
                 "color1" = .theme_color1(plot),
                 "color2" = .theme_color2(plot),
                 "none" = plot,
                 stop("Please provide theme that exists"))

  plot <- plot +
    ggplot2::theme(
      axis.ticks.x.bottom = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  # Add label to plot - maracaPlot
  class(plot) <- c("maracaPlot", class(plot))

  return(plot)
}

#' Generic function to generate validation data for the maraca plot object.
#'
#' This will produce the 4 validation datasets.
#'
#' @param x An object of S3 class 'maracaPlot'.
#' @param \dots Not used.
#' @return Creates a list of datasets for validation purposes.
#'
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' p <- plot(hce_test)
#' validate_maraca_plot(p)
#'
#' @export
validate_maraca_plot <- function(x,  ...) {
  checkmate::assert_class(x, "maracaPlot")

  pb <- ggplot2::ggplot_build(x)
  layers <- sapply(pb$plot$layers, function(lb) {
    class(lb$geom)[1]
  })

  proportions <- diff(pb$data[[1]][, c("xintercept")])
  names(proportions) <- unique(x$data$outcome)

  arms <- levels(unlist(pb$plot$data[, pb$plot$labels$colour]))

  tte_data <- .create_validation_tte(layers, x, arms)
  binary_step_data <- .create_validation_binary_step(layers, x, arms)
  binary_last_data <- .create_validation_binary_last(layers, x, arms)
  scatter_data <- .create_validation_scatter(layers, x, arms)
  boxstat_data <- .create_validation_box(layers, x, arms)
  violin_data <- .create_validation_violin(layers, x, arms)

  possible_plot_types <- c("GeomViolin", "GeomBoxplot", "GeomPoint")
  plot_type <- paste(possible_plot_types[possible_plot_types %in% layers],
                     collapse = "+")

  if ("win.odds" %in% names(x$labels)) {
    params <- x$labels$win.odds
    wo_stats <- c(winodds = params$win_odds,
                  lowerCI = params$lower_ci,
                  upperCI = params$upper_ci,
                  p_value = params$p_value)
  } else {
    wo_stats <- NULL
  }

  return(
    list(
      plot_type = plot_type,
      proportions = proportions,
      tte_data = tte_data,
      binary_step_data = binary_step_data,
      binary_last_data = binary_last_data,
      scatter_data = scatter_data,
      boxstat_data = boxstat_data,
      violin_data = violin_data,
      wo_stats = wo_stats
    )
  )
}

#' Generic function to plot the maraca object using plot().
#'
#' @param x An object of S3 class 'maraca'.
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#' @param \dots not used
#' @return Returns ggplot2 plot of the maraca object.
#'
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' plot(hce_test)
#'
#' @export
plot.maraca <- function(
    x,
    continuous_grid_spacing_x = 10,
    trans = c("identity", "log", "log10", "sqrt", "reverse")[1],
    density_plot_type = c("default", "violin", "box", "scatter")[1],
    vline_type = NULL,
    theme = "maraca",
    ...) {
  plot_maraca(x, continuous_grid_spacing_x,
              trans, density_plot_type,
              vline_type, theme)
}
#' Generic function to plot the hce object using plot().
#'
#' @param x an object of S3 class 'hce'.
#' @param step_outcomes A vector of strings containing the outcome labels
#'                      for all outcomes displayed as part of the step function
#'                      on the left side of the plot.
#'                      The order is kept for the plot.
#'                      By default (when set to NULL) this is automatically
#'                      updated by taking the non-continuous outcomes from
#'                      the GROUP variable in alphabetical order.
#' @param last_outcome A single string containing the last outcome label
#'                     displayed on the right side of the plot.
#'                     Default value "C".
#' @param arm_levels A named vector of exactly two strings, mapping the
#'                   values used for the active and control arms to the values
#'                   used in the data. The names must be "active" and "control"
#'                   in this order. Note that this parameter only need to
#'                   be specified if you have labels different from
#'                   "active" and "control".
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param fixed_followup_days Not needed if HCE object contains information
#'                            on fixed follow-up days in the study
#'                            (column PADY or TTEfixed,
#'                            depending on hce version).
#'                            Otherwise, this argument must be specified
#'                            to give the fixed follow-up days in the study.
#'                            Can be a single integer value
#'                            for all tte-outcomes or a vector with one
#'                            integer value per tte-outcome.
#'                            Note: If argument is specified and HCE object
#'                            also contains PADY or TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @param step_types The type of each outcome in the step_outcomes vector.
#'                   Can be a single string (if all outcomes of same type) or
#'                   a vector of same length as step_outcomes. Possible values
#'                   in the vector are "tte" (default) or "binary".
#' @param last_type A single string giving the type of the last outcome.
#'                  Possible values are "continuous" (default), "binary" or
#'                  "multinomial".
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#'        [companion vignette for package users](themes.html)
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param tte_outcomes Deprecated and substituted by the more general
#'                     'step_outcomes'. A vector of strings containing the
#'                     time-to-event outcome labels. The order is kept for the
#'                     plot.
#' @param continuous_outcome Deprecated and substituted by the more general
#'                           'last_outcome'. A single string containing the
#'                           continuous outcome label.
#' @param \dots not used
#' @return Returns ggplot2 plot of the hce object.
#'
#' @examples
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
#'              seed = 31337)
#' plot(hce_dat)
#' plot(hce_dat, fixed_followup_days = 3 * 365)
#'
#' @export
plot.hce <- function(x,
                     step_outcomes = NULL,
                     last_outcome = "C",
                     arm_levels = c(active = "A", control = "P"),
                     continuous_grid_spacing_x = 10,
                     trans = c("identity", "log", "log10",
                               "sqrt", "reverse")[1],
                     density_plot_type = c("default", "violin",
                                           "box", "scatter")[1],
                     vline_type = NULL,
                     fixed_followup_days = NULL,
                     compute_win_odds = FALSE,
                     step_types = "tte",
                     last_type = "continuous",
                     theme = "maraca",
                     lowerBetter = FALSE,
                     tte_outcomes = lifecycle::deprecated(),
                     continuous_outcome = lifecycle::deprecated(),
                     ...) {

  if (lifecycle::is_present(tte_outcomes)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(tte_outcomes)",
                              "maraca(step_outcomes)")
    step_outcomes <- tte_outcomes
  }

  if (lifecycle::is_present(continuous_outcome)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(continuous_outcome)",
                              "maraca(last_outcome)")
    last_outcome <- continuous_outcome
  }

  maraca_obj <- .maraca_from_hce_data(x, step_outcomes,
                                      last_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds,
                                      step_types = step_types,
                                      last_type = last_type,
                                      lowerBetter = lowerBetter)

  plot_maraca(maraca_obj, continuous_grid_spacing_x,
              trans, density_plot_type, vline_type, theme)
}
