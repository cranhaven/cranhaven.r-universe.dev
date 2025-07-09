
#' @title .add_EQ5D_utilities
#' @description This function calculates utilities for the provided EQ5D version and value sets.
#' @param df A data frame to which the utilities will be added.
#' @param value_sets A character vector specifying the country value sets for the given EQ5D version.
#' @param version A character string specifying the EQ5D version. Valid versions are "5L", "3L", "XW", and "XWR".
#' @param dim.names A vector of dimension names to identify dimension columns in the df data frame
#' @param colnames (Optional) A character vector specifying the column names for the added utilities. 
#' @return The df data frame with the added utility columns.
#' @export

.add_EQ5D_utilities <- function(df, value_sets, version, dim.names = c("mo", "sc", "ua", "pd", "ad"), colnames = NULL) {
  
  # Validate the version
  valid_versions <- c("5L", "3L", "XW", "XWR")
  if (!version %in% valid_versions) {
    stop("Invalid version provided.")
  }
  
  # Validate value_sets 
  if (length(value_sets) == 0) {
    stop("The list of value sets is empty.")
  }
  if (version %in% c("3L", "5L")){
    to_version <- version
  } else if (version %in% c("XW", "XWR")){
    to_version <- ifelse(version == "XW", "3L", "5L")
  }
  value_sets_colname <- sprintf("EQ5D%s (%s)", to_version, value_sets)
  pkgenv <- getOption("eq.env")
  available_value_sets <- pkgenv$country_codes[[to_version]][["ISO3166Alpha2"]]
  unavailable_value_sets <- setdiff(value_sets, available_value_sets) 
  if (length(unavailable_value_sets)>0) {
    stop(paste0("The value sets '", paste(unavailable_value_sets, collapse = ", "), "' are not available for the ", to_version, " version."))
  }
  
  # Validate colnames
  if(is.null(colnames) || length(colnames) != length(value_sets)){
    colnames <- value_sets
  }
  
  # Calculate utilities and add to df
  utilities <- eq5dsuite::eq5d(x = df, country = value_sets, version = version, dim.names = dim.names)
  df <- cbind(df, utilities)
  colnames(df)[(ncol(df) - length(value_sets) + 1):ncol(df)] <- colnames
  
  return(df)
}

#' @title .get_EQ5D_value_sets
#' @description This function generates all EQ states based on the provided EQ5D version and adds utilities.
#' @param version A character string specifying the EQ5D version.
#' @param value_sets A character vector specifying the country value sets for the given EQ5D version. 
#' @param value_sets_XW A character vector specifying the country value sets for the crosswalk EQ5D version.
#' @return A list containing:
#' \itemize{
#'   \item \code{utilityColumn}: A character vector with the names of the added utility columns.
#'   \item \code{df}: A data frame containing all generated EQ states with their utilities.
#' }
#' @keywords internal

.get_EQ5D_value_sets <- function(version, value_sets, value_sets_XW) {
    
    # Generate all EQ states and add utilities
    scores <- make_all_EQ_states(version = version, append_index = TRUE)
    
    # Add utilities
    if (length(value_sets)>0){
      value_sets_colname <- sprintf("EQ5D%s (%s)", version, value_sets)
      scores <- .add_EQ5D_utilities(df = scores, value_sets = value_sets, version = version, colnames = value_sets_colname)
    } else {
      value_sets_colname <- NULL
    }
    
    # Add XW utilities
    if (length(value_sets_XW)>0){
      XW_version <- ifelse(version == "5L", "XW", "XWR")
      value_sets_XW_colname <- sprintf("EQ%s (%s)", XW_version, value_sets_XW)
      scores <- .add_EQ5D_utilities(df = scores, value_sets = value_sets_XW, version = XW_version, colnames = value_sets_XW_colname)
    } else {
      value_sets_XW_colname <- NULL
    }
    return(list(utilityColumn = c(value_sets_colname, value_sets_XW_colname), df = scores))
}

#' @title .get_EQ5D_version
#' @description This function determines the version of EQ5D used based on the provided stateColumn of a data frame.
#' @param df A data frame containing EQ5D health states.
#' @param stateColumn A character string specifying the name of the column in the df data frame that contains the EQ5D health states.
#' @return A character string indicating the version of EQ5D used. Valid return values are "3L" for EQ5D-3L, "5L" for EQ5D-5L, or NULL if the version cannot be determined.
#' @keywords internal

.get_EQ5D_version <- function(df, stateColumn){
  
  # Extract the stateColumn from df and convert it to EQ5D dimensions
  eq5d_dims <- suppressWarnings(toEQ5Ddims(df[[stateColumn]]))
  
  # Check if all entries in the dimensions are between 1 and 3
  if (all(apply(eq5d_dims, 2, function(col) all(col %in% 1:3)))) {
    return("3L")
  } 
  # Else, check if all entries in the dimensions are between 1 and 5
  else if (all(apply(eq5d_dims, 2, function(col) all(col %in% 1:5)))) {
    return("5L")
  }
  # If neither condition is met, return NULL
  else {
    return(NULL)
  }
}

#' @title .get_VS_input_list
#' @description This function generates a list of input specifications based on provided EQ5D versions and value sets.
#' @param value_sets_3L (Optional) A character vector specifying the country value sets for EQ5D-3L version.
#' @param value_sets_5L (Optional) A character vector specifying the country value sets for EQ5D-5L version.
#' @param value_sets_XW (Optional) A character vector specifying the country value sets for EQ5D-XW version.
#' @param value_sets_XWR (Optional) A character vector specifying the country value sets for EQ5D-XWR version.
#' @param value_sets_others (Optional) A list of lists specifying the inputs for other instruments. Each list within the main list should be named and contain a data frame ("df"), a column specifying the health states ("stateColumn"), and a column specifying the utility values ("utilityColumn").
#' @return A list containing input specifications for each provided value set. Each element of the list is a list including three elements: "df", which is a dataframe, "stateColumn" which specifies the name of the column in the "df" dataframe that contains health states, and "utilityColumn" which is the name of the column in the "df" dataframe that contains utility values.
#' @keywords internal

.get_VS_input_list <- function(value_sets_3L = NULL, 
                               value_sets_5L = NULL, 
                               value_sets_XW = NULL, 
                               value_sets_XWR = NULL, 
                               value_sets_others = NULL){
  
  # Initialize input list
  input_list <- list()
  
  # Add 3L and XWR value sets
  if ((length(value_sets_3L) + length(value_sets_XWR))>0) {
    value_sets_3L <- .get_EQ5D_value_sets("3L", value_sets_3L, value_sets_XWR)
    input_list <- c(input_list, unlist(lapply(value_sets_3L$utilityColumn, function(utilityColumn) {
      list(list(df = value_sets_3L$df, stateColumn = "state", utilityColumn = utilityColumn))
    }), recursive = FALSE))
  }
  
  # Add 5L and XW value sets
  if ((length(value_sets_5L) + length(value_sets_XW))>0) {
    value_sets_5L <- .get_EQ5D_value_sets("5L", value_sets_5L, value_sets_XW)
    input_list <- c(input_list, unlist(lapply(value_sets_5L$utilityColumn, function(utilityColumn) {
      list(list(df = value_sets_5L$df, stateColumn = "state", utilityColumn = utilityColumn))
    }), recursive = FALSE))
  }
  
  # Add names
  names(input_list) <- sapply(input_list, function(value_set) value_set$utilityColumn)
  
  # Add other instrument value sets
  if (!is.null(value_sets_others)){
    # Check if the required components are present in each instrument
    if (!all(sapply(value_sets_others, function(value_set) {
      all(c("df", "stateColumn", "utilityColumn") %in% names(value_set))
    }))) {
      stop("Each item in 'value_sets_others' should have 'df', 'stateColumn', and 'utilityColumn'.")
    }
    # Add to input list
    input_list <- c(input_list, unlist(lapply(value_sets_others, function(value_set) {
      list(list(df = value_set$df, stateColumn = value_set$stateColumn, utilityColumn = value_set$utilityColumn))
    }), recursive = FALSE))
  }
  
  #Return input list
  return(input_list)
  
}

#' @title calculate_mean_transition
#' @description This function calculates the mean transitions for EQ-5D health states based on the version specified.
#' @param df Data frame containing the health state and utility data.
#' @param version Character string specifying the EQ-5D version ("5L" or "3L").
#' @param utilityColumn Character string specifying the column name in `df` containing utility values.
#' @param stateColumn Character string specifying the column name in `df` containing health state values.
#' @return A data frame containing the following columns:
#'   - Columns for each transition (e.g., "MOb", "MOw", "SCb", "SCw", etc.)
#'   - `baseline_HS`: Baseline health state for each transition.
#'   - `LSS`: Level Sum Score for the baseline health state.
#'   - `baseline_utility`: Baseline utility value for each transition.
#'   - `mean_transition`: Mean of the transitions for each row.
#' @keywords internal
  
.calculate_mean_transition <- function(df, version, utilityColumn, stateColumn){
  
  # Calculate the dimension size based on the version
  dim_size <- ifelse(version == "5L", 5, 3)
  extended_dim_size <- dim_size + 2
  
  # Initialize index for subsetting arrays
  index_values <- 2:(4 + 2 * (version == "5L"))
  current_index <- list(index_values)[rep(1, 5)]
  
  # Initialize dim-size array with utility values
  eq5d_array <- array(data = df[[utilityColumn]], dim = rep(dim_size, 5))
  
  # Initialize and fill extended-dim-size array with utility values
  eq7d_array <- array(data = NA, dim = rep(extended_dim_size, 5))
  eq7d_array[index_values, index_values, index_values, index_values, index_values] <- eq5d_array
  
  # Calculate single transitions
  single_transitions <- as.data.frame(lapply(1:10, function(x) {
    # Update the current index based on x
    current_dim <- x %% 5 + 1
    original_index <- current_index[[current_dim]]
    current_index[[current_dim]] <- current_index[[current_dim]] + ifelse(x > 5, 1, -1)
    # Calculate the absolute difference in utilities
    abs_diff <- abs(as.vector(eq5d_array - do.call('[', c(list(eq7d_array), current_index))))
    return(abs_diff)
  }))
  colnames(single_transitions) <- outer(c("MO", "SC", "UA", "PD", "AD"), c("b", "w"), paste0)
  
  # Add baseline HS, utility and mean
  single_transitions$baseline_HS <- df[[stateColumn]]
  single_transitions$LSS <- rowSums(eq5dsuite::toEQ5Ddims(df[[stateColumn]]))
  single_transitions$baseline_utility <- df[[utilityColumn]]
  single_transitions$mean_transition <- rowMeans(single_transitions[,1:10], na.rm = TRUE)
  
  return(single_transitions)
}

#' @title compute_utility_stats
#' @description Compute utility statistics for given EQ5D versions and value sets.
#' @param value_sets_3L (Optional) A character vector specifying the country value sets for EQ5D-3L version.
#' @param value_sets_5L (Optional) A character vector specifying the country value sets for EQ5D-5L version.
#' @param value_sets_XW (Optional) A character vector specifying the country value sets for EQ5D-XW version.
#' @param value_sets_XWR (Optional) A character vector specifying the country value sets for EQ5D-XWR version.
#' @param value_sets_others (Optional) A list of lists specifying the inputs for other instruments. Each list within the main list should be named and contain a data frame (`df`), a column specifying the health states (`stateColumn`), and a column specifying the utility values (`utilityColumn`).
#' @param format_results (Optional) A logical indicating whether the result should be formatted. Default is FALSE.
#' @return A data frame containing utility statistics for each provided value set. 
#' @examples
#' compute_utility_stats(value_sets_3L = "ES", value_sets_5L = "ES")
#' value_set_other <- list(instrument1 = list(df = data.frame(state=c(1, 2, 3), utility = c(-1, 0, 1)), 
#'                         stateColumn = "state", 
#'                         utilityColumn = "utility"))
#' compute_utility_stats(value_sets_3L = "AR", value_sets_others = value_set_other)
#' @note Mean single level transitions are calculated only for EQ-5D.
#' @export

compute_utility_stats <- function(value_sets_3L = NULL, 
                                          value_sets_5L = NULL, 
                                          value_sets_XW = NULL, 
                                          value_sets_XWR = NULL, 
                                          value_sets_others = NULL,
                                          format_results = FALSE
                                          ) {
  # Helper function
  utility_stats_helper <- function(df, stateColumn, utilityColumn, formatResults = FALSE) {
    # Validate input columns
    required_columns <- c(utilityColumn, stateColumn)
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      stop(paste0("The variable(s) '", paste(missing_columns, collapse = ", "), "' is not found in the data frame."))
    }
    # Compute statistics
    utility_data <- df[[utilityColumn]]
    sorted_unique_utilities <- sort(unique(utility_data), decreasing = TRUE)
    stats <- data.frame(
      nrow(df),
      diff(range(utility_data, na.rm = TRUE)),
      sorted_unique_utilities[1],
      sum(utility_data < 0, na.rm = TRUE),
      (sum(utility_data < 0, na.rm = TRUE) / nrow(df)) * 100,
      sorted_unique_utilities[1] - sorted_unique_utilities[2],
      NA,
      NA,
      stringsAsFactors = FALSE
    )
    colnames(stats) <- c("Number of health states", 
                         "Range of scale", 
                         "Value for best health state", 
                         "Number of health states WTD", 
                         "Percent of health states WTD", 
                         "Utility difference between best and next best health state", 
                         "Mean single level transition across all health states", 
                         "SD of single level transition")
    
    # Calculate single transition if EQ-5D instrument
    version <- .get_EQ5D_version(df, stateColumn)
    if (!is.null(version)) {
      singleTransitions <- .calculate_mean_transition(df, version, utilityColumn, stateColumn)
      stats$'Mean single level transition across all health states' <- mean(singleTransitions$mean_transition, na.rm = TRUE)
      stats$'SD of single level transition' <- sd(singleTransitions$mean_transition, na.rm = TRUE)
    }
    # Format results
    if (formatResults) {
      stats$"Range of scale" <- sprintf("%.3f", stats$"Range of scale")
      stats$"Value for best health state" <- sprintf("%.3f", stats$"Value for best health state")
      stats$"Number of health states WTD" <- sprintf("%d", stats$"Number of health states WTD")
      stats$"Percent of health states WTD" <- sprintf("%.1f%%", stats$"Percent of health states WTD")
      stats$"Utility difference between best and next best health state" <- sprintf("%.3f", stats$"Utility difference between best and next best health state")
      if (!is.null(version)) {
        stats$"Mean single level transition across all health states" <- sprintf("%.3f", stats$"Mean single level transition across all health states")
        stats$"SD of single level transition" <- sprintf("%.3f", stats$"SD of single level transition")
      }
    }
    return(stats)
  }
  
  # Get input list
  input_list <- .get_VS_input_list(value_sets_3L = value_sets_3L, 
                                   value_sets_5L = value_sets_5L, 
                                   value_sets_XW = value_sets_XW,
                                   value_sets_XWR = value_sets_XWR,
                                   value_sets_others = value_sets_others)
  
  # Call utility statistics
  utility_stats <- do.call(rbind, lapply(seq_along(input_list), function(i){
    utility_stats_helper(df = input_list[[i]]$df, 
                                 stateColumn = input_list[[i]]$stateColumn, 
                                 utilityColumn = input_list[[i]]$utilityColumn,
                                 formatResults = format_results)
  }))
  rownames(utility_stats) <- sapply(input_list, function(value_set) value_set$utilityColumn)
  
  return (utility_stats)
}

#' @title single_transition_plots
#' @description This function creates a scatter plot of mean one-level transitions for different EQ5D versions and specified value sets.
#' @param value_sets_3L A character vector specifying the country value sets for the EQ5D-3L version.
#' @param value_sets_5L A character vector specifying the country value sets for the EQ5D-5L version.
#' @param value_sets_XW A character vector specifying the country value sets for the EQ5D-XW version.
#' @param value_sets_XWR A character vector specifying the country value sets for the EQ5D-XWR version.
#' @param graph_title A character string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A character string specifying the title of the x-axis. Default is "Utility".
#' @param x_min_value A numeric specifying the minimum value for the x-axis. Default is -1.
#' @param x_max_value A numeric specifying the maximum value for the x-axis. Default is 1.
#' @param y_axis_title A character string specifying the title of the y-axis. Default is "Mean 1-level transition".
#' @param y_min_value A numeric specifying the minimum value for the y-axis. Default is 0.
#' @param y_max_value A numeric specifying the maximum value for the y-axis. Default is 0.5.
#' @param legend_name A character string specifying the name of the legend. Default is NULL.
#' @return A list of ggplot objects visualizing the mean one-level transitions for the specified EQ5D versions and value sets.
#' @examples
#' single_transition_plots(value_sets_5L = "IT")
#' single_transition_plots(value_sets_3L = c("JP", "US"))  
#' single_transition_plots(value_sets_3L ="ES", 
#'                         value_sets_5L = "ES", 
#'                         value_sets_XW = "ES", 
#'                         value_sets_XWR = "ES")        
#' @note This function is primarily intended to work with EQ5D data and it is not be applicable to other instruments.
#' @export

single_transition_plots <- function(value_sets_3L = NULL, 
                             value_sets_5L = NULL, 
                             value_sets_XW = NULL, 
                             value_sets_XWR = NULL, 
                             graph_title = "", 
                             x_axis_title = NULL, 
                             x_min_value = -1, 
                             x_max_value = 1,
                             y_axis_title = "Mean 1-level transition", 
                             y_min_value = 0, 
                             y_max_value = 0.5, 
                             legend_name = NULL){
  
  # Get input list
  input_list <- .get_VS_input_list(value_sets_3L = value_sets_3L, 
                                   value_sets_5L = value_sets_5L, 
                                   value_sets_XW = value_sets_XW,
                                   value_sets_XWR = value_sets_XWR)
  if(is.null(x_axis_title)){
    x_axis_titles <- sapply(input_list, function(value_set) value_set$utilityColumn)
  } else {
    x_axis_titles <- rep(x_axis_title, length(input_list))
  }
  
  # Get mean transition plot list
  plot_list <- lapply(seq_along(input_list), function(i){
    version <- .get_EQ5D_version(input_list[[i]]$df, input_list[[i]]$stateColumn)
    singleTransitions <- .calculate_mean_transition(input_list[[i]]$df, version, input_list[[i]]$utilityColumn, input_list[[i]]$stateColumn)
    ggplot(singleTransitions, aes(x = .data$baseline_utility, y = .data$mean_transition)) +
      geom_point(aes(color = .data$LSS), size = 2) +
      geom_hline(yintercept = mean(singleTransitions[["mean_transition"]]), color = "darkgray", linewidth=1) +  
      labs(title = graph_title, x = x_axis_titles[[i]], y = y_axis_title) +
      coord_cartesian(ylim = c(y_min_value, y_max_value), xlim = c(x_min_value, x_max_value)) +
      theme_minimal() +
      scale_colour_gradientn(name = legend_name, colours=c("#4092BAff", "#B8E196ff", "#F3E68Dff", "#FC8E59ff", "#DA4453ff")) +
      theme(legend.position = "bottom")
  })
  names(plot_list) <- sapply(input_list, function(value_set) value_set$utilityColumn)
  
  # Return plot list
  return(plot_list)
  
}

#' @title density_plot_theorical
#' @description This function creates a smoothed kernel density plot of utilities for different EQ5D versions and specified value sets.
#' @param value_sets_3L A character vector specifying the country value sets for the EQ5D-3L version.
#' @param value_sets_5L A character vector specifying the country value sets for the EQ5D-5L version.
#' @param value_sets_XW A character vector specifying the country value sets for the EQ5D-XW version.
#' @param value_sets_XWR A character vector specifying the country value sets for the EQ5D-XWR version.
#' @param value_sets_others A list of lists specifying the inputs for other instruments. Each list within the main list should be named and contain a data frame (`df`), a column specifying the health states (`stateColumn`), and a column specifying the utility values (`utilityColumn`).
#' @param graph_title A character string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A character string specifying the title of the x-axis. Default is "Index Value".
#' @param x_min_value A numeric specifying the minimum value for the x-axis. Default is NULL.
#' @param x_max_value A numeric specifying the maximum value for the x-axis. Default is NULL.
#' @param y_axis_title A character string specifying the title of the y-axis. Default is "Density".
#' @param y_min_value A numeric specifying the minimum value for the y-axis. Default is NULL.
#' @param y_max_value A numeric specifying the maximum value for the y-axis. Default is NULL.
#' @param legend_name A character string specifying the name of the legend. Default is "".
#' @param color_palette A character vector specifying the colors for the density lines. Default is a predefined color palette.
#' @param line_types A character vector specifying the line types for the density lines. Default is solid.
#' @return A ggplot object visualizing the density of utilities for the specified EQ5D versions and other instruments value sets.
#' @examples
#' density_plot_theorical(value_sets_3L = "NL", value_sets_5L = "NL")
#' instrument <- data.frame(HS=c(123, 456, 789), val = c(-0.3, 0.1, 0.75))
#' value_set_other <- list(test_instrument = list(df = instrument, 
#'                         stateColumn = "HS", 
#'                         utilityColumn = "val"))
#' density_plot_theorical(value_sets_3L = "HU", value_sets_others = value_set_other)
#' @export

density_plot_theorical <- function(value_sets_3L = NULL, 
                         value_sets_5L = NULL, 
                         value_sets_XW = NULL, 
                         value_sets_XWR = NULL, 
                         value_sets_others = NULL,
                         graph_title = "", 
                         x_axis_title = "Index Value", 
                         x_min_value = NULL, 
                         x_max_value = NULL,
                         y_axis_title = "Density", 
                         y_min_value = NULL, 
                         y_max_value = NULL, 
                         legend_name = "", 
                         color_palette = NULL,
                         line_types = NULL){
  
  # Get input list
  input_list <- .get_VS_input_list(value_sets_3L = value_sets_3L, 
                                   value_sets_5L = value_sets_5L, 
                                   value_sets_XW = value_sets_XW,
                                   value_sets_XWR = value_sets_XWR,
                                   value_sets_others = value_sets_others)
  
  # Check for input_list length
  if (length(input_list) > 10) {
    stop("Please reduce the number of value sets.")
  }
  if (is.null(color_palette)){
    color_palette <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF", "#fccde5", "#ffff67", "#80b1d3")
  }
  if (is.null(line_types)){
    line_types <- rep("solid", length(input_list))
  }
  
  # Create long format data frame
  df_long <- do.call(rbind, lapply(seq_along(input_list), function(i){
    data.frame(
      type = names(input_list)[[i]],
      utility = input_list[[i]]$df[[input_list[[i]]$utilityColumn]]
    )
  }))
  
  # Create plot
  density_plot <- ggplot(df_long, aes(x = .data$utility, color = .data$type, linetype = .data$type)) +
    geom_density(size = 1) +
    labs(title = graph_title, x = x_axis_title, y = y_axis_title) +
    coord_cartesian(xlim = c(x_min_value, x_max_value), ylim = c(y_min_value, y_max_value)) + 
    scale_color_manual(name = legend_name, breaks = unique(df_long$type),values = color_palette) +
    scale_linetype_manual(name = legend_name, breaks = unique(df_long$type), values = line_types[1:length(unique(df_long$type))]) +  # Added this line
    theme_bw() + 
    theme(legend.position = "bottom") 
  
  return(density_plot)
  
}