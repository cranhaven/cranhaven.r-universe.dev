#' Function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome for a hierarchical endpoint.
#'
#' Implemented for objects of type 'maraca' and 'hce'.
#'
#' @param x an object of S3 class 'maraca' or 'hce'.
#' @param \dots further arguments to be passed to the
#'        object-specific functions
#' @export
component_plot <- function(x, ...) {
  UseMethod("component_plot", x)
}

#' @export
component_plot.default <- function(x,
                                   ...) {
  paste0("component_plot() function can only handle inputs of class ",
         "'hce' or 'maraca'. Your input has class ", class(x), ".")
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome directly from a maraca object.
#' Note that for this plot, when creating the maraca object using the maraca()
#' function, the argument "compute_win_odds" has to be set to TRUE.
#' Check the vignette "Maraca Plots - Plotting win odds" for more details.
#'
#' @param x an object of S3 class 'maraca'.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Plotting win odds".
#' @param \dots not used
#' @return Component plot as a ggplot2 object.
#' @examples
#'
#' data(hce_scenario_a)
#'
#' maraca_dat <- maraca(data = hce_scenario_a,
#'                      step_outcomes = c("Outcome I", "Outcome II",
#'                                       "Outcome III", "Outcome IV"),
#'                      last_outcome = "Continuous outcome",
#'                      fixed_followup_days = 3 * 365,
#'                      column_names = c(outcome = "GROUP",
#'                                       arm = "TRTP",
#'                                       value = "AVAL0"),
#'                      arm_levels = c(active = "Active",
#'                                     control = "Control"),
#'                      compute_win_odds = TRUE
#'                      )
#'
#' component_plot(maraca_dat)
#'
#' @export
component_plot.maraca <- function(x,
                                  theme = "maraca",
                                  ...) {

  # Check that win odds were calculated for the maraca object
  if (is.null(x[["win_odds_outcome"]])) {
    stop(paste0("Win odds not calculated for maraca object.\n",
                "  Make sure to set compute_win_odds = TRUE when ",
                "creating the maraca object."))
  }

  # Get win odds by outcome from maraca object
  win_odds_outcome <- x$win_odds_outcome
  # List of outcomes in order of plotting
  endpoints <- c(x$step_outcomes, x$last_outcome)
  # Create data set for potting
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints,
                                         x$arm_levels)
  # Create component plot
  plot <- .create_component_plot(wo_bar_nc, endpoints, theme)

  plot <- .add_win_odds_to_plot(plot, x$win_odds,
                                x = (length(endpoints) + 1.5),
                                y = max(wo_bar_nc$percentage) * 1.2,
                                hjust = 0.1)

  return(plot)
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome directly from an hce object.
#' Check the vignette "Maraca Plots - Plotting win odds" for more details.
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
#'                    "active" and "control".
#' @param fixed_followup_days Not needed if HCE object contains information
#'                            on fixed follow-up days in the study
#'                            (column PADY or TTEfixed,
#'                            depending on hce version).
#'                            Otherwise, this argument must be specified.
#'                            Note: If argument is specified and HCE object
#'                            contains PADY or TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Plotting win odds".
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param \dots not used
#' @return Component plot as a ggplot2 object.
#' @examples
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
#'              seed = 31337)
#'
#' component_plot(hce_dat)
#' @export
#'
component_plot.hce <- function(x, step_outcomes = NULL,
                               last_outcome = "C",
                               arm_levels = c(active = "A", control = "P"),
                               fixed_followup_days = NULL,
                               theme = "maraca",
                               lowerBetter = FALSE,
                               ...) {

  # Create maraca object
  maraca_dat <- .maraca_from_hce_data(x, step_outcomes,
                                      last_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds = TRUE,
                                      lowerBetter = lowerBetter)

  # Get win odds by outcome from maraca object
  win_odds_outcome <- maraca_dat$win_odds_outcome
  # List of outcomes in order of plotting
  endpoints <- c(maraca_dat$step_outcomes, maraca_dat$last_outcome)
  # Create data set for potting
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints,
                                         maraca_dat$arm_levels)
  # Create component plot
  plot <- .create_component_plot(wo_bar_nc, endpoints, theme)

  plot <- .add_win_odds_to_plot(plot, maraca_dat$win_odds,
                                x = (length(endpoints) + 1.5),
                                y = max(wo_bar_nc$percentage) * 1.2,
                                hjust = 0.1)

  return(plot)
}

#' Function to create a plot showing the components used in
#' calculating win odds (wins and ties) cumulated for all
#' outcomes for a hierarchical endpoint.
#'
#' Implemented for objects of type 'maraca' and 'hce'.
#'
#' @param x an object of S3 class 'maraca' or 'hce'.
#' @param \dots further arguments to be passed to the
#'        object-specific functions
#' @export
cumulative_plot <- function(x, ...) {
  UseMethod("cumulative_plot", x)
}

#' @export
cumulative_plot.default <- function(x, ...) {
  paste0("cumulative_plot() function can only handle inputs of class ",
         "'hce' or 'maraca'. Your input has class ", class(x), ".")
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) cumulated for all
#' outcomes directly from a maraca object.
#' Note that for this plot, when creating the maraca object using the maraca()
#' function, the argument "compute_win_odds" has to be set to TRUE.
#' Check the vignette "Maraca Plots - Plotting win odds" for more details.
#'
#' @param x an object of S3 class 'maraca'.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Plotting win odds".
#' @param include Vector or single string indicating which statistics to
#'        include in the right hand side plot. Acceptable values are
#'        "win odds" and/or "win ratio". Default is c("win odds", "win ratio").
#' @param reverse Flag indicating if the cumulated outcomes should be
#'        displayed in order from top to bottom (FALSE, the default)
#'        or in reverse (TRUE).
#' @param \dots not used
#' @return Cumulative plot as a patchwork list. Individual plots can
#'         be accessed like list items (plot[[1]] and plot[[2]]).
#' @examples
#'
#' data(hce_scenario_a)
#'
#' maraca_dat <- maraca(data = hce_scenario_a,
#'                      step_outcomes = c("Outcome I", "Outcome II",
#'                                       "Outcome III", "Outcome IV"),
#'                      last_outcome = "Continuous outcome",
#'                      fixed_followup_days = 3 * 365,
#'                      column_names = c(outcome = "GROUP",
#'                                       arm = "TRTP",
#'                                       value = "AVAL0"),
#'                      arm_levels = c(active = "Active",
#'                                     control = "Control"),
#'                      compute_win_odds = TRUE
#'                      )
#'
#' cumulative_plot(maraca_dat)
#'
#' @export
cumulative_plot.maraca <- function(x, theme = "maraca",
                                   include = c("win odds", "win ratio"),
                                   reverse = FALSE, ...) {

  checkmate::assert_subset(include,
                           choices = c("win odds", "win ratio"),
                           empty.ok = FALSE)

  # Check that win odds were calculated for the maraca object
  if (is.null(x[["wins_forest"]]) || is.null(x[["wo_bar"]])) {
    stop(paste0("Win odds not calculated for maraca object.\n",
                "  Make sure to set compute_win_odds = TRUE when ",
                "creating the maraca object."))
  }

  # Get win odds by outcome from maraca object
  wo_bar <- x$wo_bar
  wins_forest <- x$wins_forest
  # Include only methods of interest
  wins_forest <- wins_forest[wins_forest$method %in% include, ]
  # Create forest plot
  plot_bar <- .create_bar_plot(wo_bar, theme, reverse)
  plot_forest <- .create_forest_plot(wins_forest, theme, include, reverse)

  plot <-  patchwork::wrap_plots(plot_bar, plot_forest,
                                 nrow = 1, widths = c(2.5, 1))

  # Add class to plot - cumulativePlot
  class(plot) <- c("cumulativePlot", class(plot))

  return(plot)
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) cumulated for all
#' outcomes directly from an hce object.
#' Check the vignette "Maraca Plots - Plotting win odds" for more details.
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
#'                    "active" and "control".
#' @param fixed_followup_days Not needed if HCE object contains information
#'                            on fixed follow-up days in the study
#'                            (column PADY or TTEfixed,
#'                            depending on hce version).
#'                            Otherwise, this argument must be specified.
#'                            Note: If argument is specified and HCE object
#'                            contains PADY or TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Plotting win odds".
#' @param include Vector or single string indicating which statistics to
#'        include in the right hand side plot. Acceptable values are
#'        "win odds" and/or "win ratio". Default is c("win odds", "win ratio").
#' @param reverse Flag indicating if the cumulated outcomes should be
#'        displayed in order from top to bottom (FALSE, the default)
#'        or in reverse (TRUE).
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param \dots not used
#' @return Cumulative plot as a patchwork list. Individual plots can
#'         be accessed like list items (plot[[1]] and plot[[2]]).
#' @examples
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
#'              seed = 31337)
#'
#' cumulative_plot(hce_dat)
#' @export
#'
cumulative_plot.hce <- function(x, step_outcomes = NULL,
                                last_outcome = "C",
                                arm_levels = c(active = "A", control = "P"),
                                fixed_followup_days = NULL,
                                theme = "maraca",
                                include = c("win odds", "win ratio"),
                                reverse = FALSE,
                                lowerBetter = FALSE,
                                ...) {

  # Create maraca object
  maraca_dat <- .maraca_from_hce_data(x, step_outcomes,
                                      last_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds = TRUE,
                                      lowerBetter = lowerBetter)

  plot <- cumulative_plot(maraca_dat, theme = theme, include = include,
                          reverse = reverse)

  return(plot)
}
