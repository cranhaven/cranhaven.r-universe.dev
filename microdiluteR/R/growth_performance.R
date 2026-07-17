#' @title Calculate and visualize growth performance
#' @description \code{calculate_growth_performance} standardizes data by subtracting the average value of the control group from each treatment level
#' for each concentration level, applied within each experiment. It assumes the input data is a data frame with columns 'Experiment', 'Concentration', 'Treatment', 
#' and 'Value', where 'Concentration' represents different concentration levels, 'Treatment' represents different 
#' treatment groups, and 'Value' represents the corresponding absorption values. 
#' @param input_data A data frame containing columns 'Group, 'Experiment', 'Concentration', 'Treatment', and 'Value', e.g.
#' from function calls to \code{tidy_plate*} and \code{subtract_T0}.
#' @param treatment_grouping A Boolean value that specifies whether or not (default) there is a treatment grouping within the plate.
#' @param concentration_grouping A Boolean value that specifies whether or not (default) there is a concentration grouping within the plate.
#' @param group The column containing group information. Defaults to 'Group'.
#' @param experiment The column containing experiment information. Defaults to 'Experiment'. The hierarchy is group > experiment, i.e.
#' within a single group, there might be several experiments taking place (e.g. multiple extracts from the same plant species
#' tested with plant species being the group and type of extract being the experiment).
#' @param treatment The column containing treatment information. Defaults to 'Treatment'.
#' @param concentration The column containing concentration information. Defaults to 'Concentration'.
#' @param timepoint The column containing timepoint information. Defaults to 'Timepoint'.
#' @param value The column containing the absorption values to calculate growth performance. Defaults to 'Value'.
#' @param control_mean he column containing the absorption values to calculate growth performance. Defaults to 'control_mean'.
#' @return \code{calculate_growth_performance} returns a modified data frame with the control mean subtracted from each treatment level for each concentration level, applied within each experiment.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by left_join mutate summarize ungroup if_else
#' @importFrom purrr map_lgl
#' @importFrom rlang sym
#' @seealso
#' \code{\link{tidy_single_plate}}, \code{\link{tidy_plates_via_params}}, \code{\link{tidy_plates_via_prompts}}
#' @export
calculate_growth_performance <- function(input_data,
                                         treatment_grouping = FALSE,
                                         concentration_grouping = FALSE,
                                         group = "Group",
                                         experiment = "Experiment",
                                         treatment = "Treatment",
                                         concentration = "Concentration",
                                         timepoint = "Timepoint",
                                         value = "Value",
                                         control_mean = "control_mean") {
  # Check if treatment is a grouping factor in plate design
  if (treatment_grouping) {
    grouping <- c(group, experiment, treatment, timepoint)
  } else {
    grouping <- c(group, experiment, timepoint)
  }
    
  # Check if concentration is a grouping factor in plate design
  if (concentration_grouping) {
    grouping <- c(grouping, concentration)
  } else {
    grouping
  }
  
  # Calculate control mean per grouping factor
  if("Treatment" %in% grouping) {
    control_means <- input_data %>%
      dplyr::filter(!!sym(treatment) == "Control") %>%
      group_by_at(vars({{ grouping }})) %>% 
      summarize(control_mean = mean(!!sym(value))) %>%
      ungroup() %>% 
      dplyr::select(-{{treatment}})
  } else {
    control_means <- input_data %>%
      dplyr::filter(!!sym(treatment) == "Control") %>%
      group_by_at(vars({{ grouping }})) %>% 
      summarize(control_mean = mean(!!sym(value))) %>%
      ungroup()
  }
  
  # For joining purposes with mean values from Control levels
  grouping_without_treatment <- grouping[grouping != "Treatment"]
    
  # Join control means back to the input data if Treatment.x is present
  percentage_data <- left_join(input_data, control_means,
                           by = grouping_without_treatment) %>% 
    dplyr::rowwise() %>%
    mutate(Value = ifelse(!!sym(treatment) != "Control",
                          calculate_percentage_change(!!sym(value), !!sym(control_mean)), NA),
           Value = round(!!sym(value),1)) %>% 
    dplyr::filter(!!sym(treatment) != "Control")
  
  return(percentage_data)
}


#' @title Calculate percentage change to baseline
#' @description \code{calculate_percentage_change} calculates the percentage change between a vector of values (or a
#' single value) and a reference value as the baseline. If a value in the vector is less than the reference, it returns
#' the negative percentage difference; otherwise, it returns the positive percentage difference.
#' @param input A single numeric value.
#' @param reference A single numeric value serving as the baseline for comparison.
#' @return \code{calculate_percentage_change} returns a numeric vector containing the percentage change for each value in the vector compared to the reference.
#' @rdname calculate_growth_performance
#' @export
calculate_percentage_change <- function(input, reference) {
  
  if (length(input) != 1 || length(reference) != 1) {
    stop("Input and reference must be scalar values")
  }
  
  if (input == 0 && reference == 0) {
    change <- 0
  } else {
    change <- ifelse(input < reference,
                     -abs(input - reference) / reference * 100,
                     abs(input - reference) / reference * 100)
  }
  return(change)
}


#' @title Summarize Growth Performance
#' @description \code{summarize_growth_performance} summarizes a data frame containing growth performance by
#' computing the mean and either the standard error or standard deviation.
#' @param input_data A data frame containing the growth performance data to be summarized, e.g. from a function
#' call to \code{calculate_growth_performance}.
#' @param compute_sd Logical, indicating whether to compute the standard deviation (default) or standard error.
#' @param grouping A character vector specifying the columns to use for grouping.
#' Defaults to \code{c("Group", "Experiment", "Treatment", "Concentration", "Timepoint")}.
#' @param treatment The column containing treatment information. Defaults to 'Treatment'.
#' @param value The column containing the absorption values to be summarized Defaults to "Value".
#' @return \code{summarize_growth_performance} returns a data frame containing the summary statistics.
#' @importFrom stats sd setNames
#' @importFrom dplyr group_by across filter summarise ungroup n all_of
#' @importFrom rlang ensyms sym
#' @importFrom ggplot2 geom_text scale_y_continuous
#' @aliases summarise_growth_performance
#' @rdname calculate_growth_performance
#' @export
summarize_growth_performance <- function(input_data,
                                         compute_sd = FALSE,
                                         grouping = c("Group", "Treatment", "Concentration", "Timepoint"),
                                         treatment = "Treatment",
                                         value = "Value") {
  # Group data
  grouped_data <- input_data %>% 
    group_by_at(vars({{ grouping }}))
  
  # Compute mean and standard deviation or standard error
  if (compute_sd) {
    summarized_data <- grouped_data %>%
      dplyr::filter(!(!!sym(treatment) == "Control")) %>% 
      summarise(mean = mean(!!sym(value)), sd = sd(!!sym(value)), n = n()) %>% 
      ungroup()
    summarized_data$stderr <- summarized_data$sd / sqrt(summarized_data$n)
  } else {
    summarized_data <- grouped_data %>%
      dplyr::filter(!(!!sym(treatment) == "Control")) %>% 
      summarise(mean = mean(!!sym(value)), stderr = sd(!!sym(value)) / sqrt(n())) %>% 
      ungroup()
  }
  
  return(summarized_data)
}


#' @title Visualize growth performance
#' @description \code{plot_growth_performance} visualizes growth performance using bar charts with error bars.
#' @param input_data A data frame containing summarized data, e.g. from function call to \code{summarize_growth_performance}.
#' @param stats_data Optional. A data frame containing growth performance data, e.g. from function call to
#' \code{calculate_growth_performance}. Only necessary, if 'apply_sign_test' parameter is set to TRUE.
#' @param level_unit Optional. The unit of applied concentrations to display on the y-axis.
#' @param treatment_order Optional. An alternative order of factor levels on the x-axis.
#' @param apply_sign_test Logical. Should the sign test be applied to specified levels? For this, the 'stats_data' and
#' 'grouping' parameters need to be specified. 
#' @param grouping Optional. A character vector specifying the grouping variables on which to apply the sign
#' test. If not specified and 'apply_sign_test' is set to TRUE, then the test will be applied on the
#' whole dataset.
#' @param x_var The variable name for the x-axis. Defaults to "Treatment".
#' @param y_var The variable name for the y-axis. Defaults to "mean".
#' @param error_var The variable name to generate the error bars. Defaults to 'stderr'.
#' @param x_lab The label for the x-axis. Defaults to "Treatment".
#' @param y_lab Optional. The label for the y-axis. If not provided will return "Relative growth performance".
#' @param fill_var The variable used to fill facets. Defaults to "Concentration".
#' @param row_facets A character vector specifying nested column facets. Defaults to \code{NULL}.
#' @param col_facets A character vector specifying nested row facets. Defaults to "Group".
#' @param value The column containing the absorption values to be assessed via \code{apply_sign_test}. Defaults to 'Value'.
#' @param p_values The column containing the (adjusted) p-values. Defaults to 'p.adj.signif' from a
#' function call to \code{apply_sign_test} and \code{rstatix::sign_test}.
#' @param level_colors Optional. The colors for different levels. If not specified, will be determined
#' based on levels of 'fill_var' using \code{gray.colors}.
#' @param ... Additional arguments to be passed to \code{\link{apply_sign_test}}.
#' @return \code{plot_growth_performance} returns a ggplot object.
#' @details \code{plot_growth_performance} uses ggplot2 to create bar charts of summarized data with error bars.
#' @importFrom grDevices gray.colors
#' @importFrom dplyr vars
#' @importFrom ggplot2 ggplot aes facet_grid geom_bar geom_errorbar labs 
#' @importFrom ggplot2 scale_x_discrete scale_fill_manual position_dodge
#' @importFrom ggplot2 element_blank margin element_rect theme geom_hline
#' @importFrom ggplot2 element_text
#' @importFrom ggthemes theme_base
#' @importFrom ggh4x facet_nested
#' @importFrom grid unit
#' @importFrom rlang ensyms sym .data
#' @rdname calculate_growth_performance
#' @export
plot_growth_performance <- function(input_data,
                                    stats_data = NULL,
                                    level_unit = NULL,
                                    treatment_order = NULL,
                                    apply_sign_test = FALSE,
                                    grouping = NULL,
                                    x_var = "Treatment",
                                    y_var = "mean",
                                    error_var = "stderr",
                                    x_lab = "Treatment",
                                    y_lab = NULL,
                                    fill_var = "Concentration",
                                    row_facets = NULL,
                                    col_facets = "Group",
                                    value = "Value",
                                    p_values = "p.signif",
                                    level_colors = NULL,
                                    ...) {
  
  # Generate colors for grouping variable on x-axis
  if (is.null(level_colors)) {
    level_colors <- c("white", gray.colors(length(unique(as.factor(input_data[[fill_var]])))-1))
  } else {
    level_colors <- level_colors
  }
  
  # Generate formula for nested facets
  if (is.null(row_facets) & !is.null(col_facets)) {
    col_facets <- paste(col_facets, collapse = " + ")
    facet_formula <- reformulate(col_facets)
  } else if (!is.null(row_facets) & is.null(col_facets)) {
    row_facets <- paste(row_facets, collapse = " + ")
    facet_formula <- reformulate(row_facets)
  } else {
    row_facets <- paste(row_facets, collapse = " + ")
    col_facets <- paste(col_facets, collapse = " + ")
    facet_formula <- reformulate(col_facets, row_facets)
  }
  
  # Check stats
  if (apply_sign_test) {
    data <- apply_sign_test(summarized_data = input_data,
                            stats_data = stats_data,
                            grouping = grouping,
                            value = value,
                            ...)
  } else {
    data = input_data
    data$p.signif <- ""
  }
  
  # Check y-axis label
  y_lab <- if(is.null(y_lab)) {
    bquote("Relative growth performance [%]")
  } else {
    y_lab
  }
  
  # Replace NA values in stderr with zero for plotting convenience
  data$stderr[is.na(data$stderr)] <- 0
  
  # Extract y_values for evaluation in geom_text
  modified_data <- data %>%
    mutate(y_label_pos = ifelse(.data[[y_var]] < 0,
                                .data[[y_var]] - .data[[error_var]] - 20,
                                .data[[y_var]] + .data[[error_var]] + 20))
  
  # Visualize data 
    ggplot(modified_data,
           aes(x = !!sym(x_var),
               y = !!sym(y_var),
               fill = factor(!!sym(fill_var)))) +
      facet_nested(facet_formula) +
      geom_bar(stat = "identity",
               width=.6,
               color="black",
               position = position_dodge()) +
      geom_errorbar(aes(ymin = mean - stderr, ymax = mean + stderr),
                    position = position_dodge(width = 0.6), width = 0.25) +
      geom_text(aes(label = !!sym(p_values),
                y = .data$y_label_pos,
                vjust = ifelse(y_var < 0, -0.5, 0.5)),
                position = position_dodge(width = 0.6)) +
    labs(x = x_lab,
         y = y_lab,
         fill = fill_var) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(min(data[, y_var], na.rm = TRUE)-50,
                                max(data[, y_var], na.rm = TRUE)+100)) +
    scale_x_discrete(limits = treatment_order) +
    scale_fill_manual(values = level_colors,
                      labels = ~paste(.x, level_unit)) +
    theme_base() +
    theme(plot.background = element_blank(),
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          legend.position = "top",
          legend.justification = c(0, 1.1),
          legend.title = element_text(size=10, face = "bold"),
          legend.text = element_text(size=10),
          legend.key.size = unit(1,"line"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
          strip.background =element_rect(fill="transparent"))
}
