
#' @title density_plot_empirical
#' @description This function creates a smoothed kernel density plot of the empirical distribution of utility values in a given data frame.
#' @param df A data frame containing the utility and weight columns.
#' @param utility_columns A character vector specifying the names of utility columns.
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
#' dim.names.3L <- c("mobility", "selfcare", "activity", "pain", "anxiety")
#' cdta$EQ5D3L <- eq5dsuite::eq5d3l(x = cdta, 
#'                                 country = "US", 
#'                                 dim.names = dim.names.3L)
#' dim.names.5L <- c("mobility5L", "selfcare5L", "activity5L", "pain5L", "anxiety5L")
#' cdta$EQ5D5L <- eq5dsuite::eq5d5l(x = cdta, 
#'                                 country = "US", 
#'                                 dim.names = dim.names.5L)
#' cdta$EQXW <- eq5dsuite::eqxw(x = cdta, 
#'                             country = "US", 
#'                             dim.names = dim.names.5L)
#' density_plot_empirical(df = cdta, utility_columns = c("EQ5D3L", "EQ5D5L", "EQXW"))
#' @export

density_plot_empirical <- function(df, 
                                   utility_columns,
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
  
  if (!is.data.frame(df)) {
    stop("Input df must be a data frame.")
  } else {
    if (nrow(df) == 0 || ncol(df) == 0) {
      stop("Input data frame should not be empty.")
    }
  }
  # Check utility columns
  if (length(utility_columns) == 0 || length(utility_columns) > 10){
    stop("Number of utility columns must be between 1 and 10.")
  }
  if (!is.character(utility_columns)) {
    stop("utility_columns and must be of character type.")
  }
  if (length(unique(utility_columns)) != length(utility_columns)) {
    stop("Utility columns should not have duplicates.")
  }
  unavailable_vars <- setdiff(utility_columns, names(df))
  if (length(unavailable_vars) > 0) {
    stop(paste0("The variable(s) '", paste(unavailable_vars, collapse = ", "), "' are not found in the data frame."))
  }
  # Color palette and line types
  if (is.null(color_palette)){
    color_palette <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF", "#fccde5", "#ffff67", "#80b1d3")
  }
  if (is.null(line_types)){
    line_types <- rep("solid", length(utility_columns))
  }
  
  # Create long format data frame
  df_long <- do.call(rbind, lapply(seq_along(utility_columns), function(i){
    data.frame(
      type = utility_columns[[i]],
      utility = df[[utility_columns[[i]]]]
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

#' @title .makeWeightsTriangular
#' @description A function to calculate weights based on a triangular approach.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x.
#' @param rng A numeric vector indicating the range of x.
#' @return A numeric vector of calculated weights.
#' @export

.makeWeightsTriangular <- function(x, pointval, rng) {
  # Calculate ranges
  toprange <- pointval - max(rng)
  botrange <- pointval - min(rng)
  max_range <- max(rng)
  min_range <- min(rng)
  # Logical indices
  idx_pointval <- x == pointval
  idx_top <- x > pointval & x <= max_range
  idx_bot <- x < pointval & x >= min_range
  # Calculate weights based on triangular approach
  weights <- numeric(length(x))
  weights[idx_pointval] <- 1
  weights[idx_top] <- (x[idx_top] - max_range) / toprange
  weights[idx_bot] <- (x[idx_bot] - min_range) / botrange
  return(weights)
}

#' @title .makeWeightsGradient
#' @description A function to calculate weights based on a gradient approach.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x.
#' @param rng A numeric vector indicating the range of x.
#' @return A numeric vector of calculated weights.
#' @export

.makeWeightsGradient <- function(x, pointval, rng) {
  # Pre-compute common values
  max_range <- max(rng)
  min_range <- min(rng)
  range_diff <- max_range - min_range
  nmaxval <- (pointval - min_range) / range_diff
  nminval <- 1 - nmaxval
  # Calculate weights based on gradient approach
  weights <- ((nmaxval - nminval) * (x - min_range) / range_diff) + nminval
  return(weights)
}

#' @title .makeWeightsMixed
#' @description A function to calculate weights based on a mixed approach.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x.
#' @param rng A numeric vector indicating the range of x.
#' @return A numeric vector of calculated weights.
#' @export

.makeWeightsMixed <- function(x, pointval, rng) {
  # Calculate weights based on mixed approach
  weights <- (.makeWeightsTriangular(x, pointval, rng) + .makeWeightsGradient(x, pointval, rng)) / 2
  return(weights)
}

#' @title .makeWeights
#' @description A function to calculate weights with specified variant function.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x, default is 100.
#' @param rng A numeric vector indicating the range of x, default is 0 to 100.
#' @param variant_fun A function to calculate weights. It can be makeWeightsTriangular, makeWeightsGradient, makeWeightsMixed, or a custom function name. Default is makeWeightsTriangular.
#' @return A numeric vector of calculated and scaled weights.
#' @keywords internal

.makeWeights <- function(x, pointval = 100, rng = c(0:100), variant_fun = .makeWeightsMixed) {
  # Check if range is within the range of x
  if (any(rng < min(x)) || any(rng > max(x))) {
    warning('Some values in the specified range are outside the range of observed values. These will have a weight of 0.')
  }
  # Check if variant_fun is a function
  if (!is.function(variant_fun)) {
    stop("variant_fun must be a function. Please use default options (makeWeightsTriangular, makeWeightsGradient, makeWeightsMixed) or a custom function.")
  }
  # Call the variant function to calculate weights
  weights <- variant_fun(x, pointval, rng)
  # Scale weights
  weights <- (weights + min(rng)) / (1 + min(rng))
  return(weights)
}

#' @title .gen_samples
#' @description This function generates stratified samples from a specified dataframe.
#' @param df A data frame from which the samples are to be generated. 
#' @param weight_column A string specifying the name of the column to use for weighting.Defaults to "VAS".
#' @param weight_range A numeric vector indicating the range of weight_column, default is 0 to 100.
#' @param weight_function A function used to compute weights. Defaults to "makeWeightsTriangular".
#' @param weight_values A numeric vector specifying the weight values of interest. Defaults to c(0, 25, 50, 75, 100).
#' @param sample_size An integer specifying the size of each sample. Defaults to 1000.
#' @param number_of_samples An integer specifying the number of samples to generate. Defaults to 1000.
#' @return An array containing the indices of the sampled rows in the original data frame. The dimensions of the array are determined by the number of weight values and the number of samples.
#' @keywords internal

.gen_samples <- function(df, 
                         weight_column = "VAS", 
                         weight_range = c(0:100), 
                         weight_values = NULL,
                         weight_function = .makeWeightsMixed,
                         sample_size = 1000, 
                         number_of_samples = 1000) {
  # Add names
  if (is.null(weight_values)){
    weight_values <- weight_range
  }
  names(weight_values) <- weight_values
  
  # Pre-compute weights
  weights_list <- lapply(weight_values, function(w) {
    .makeWeights(df[[weight_column]], pointval = w, rng = weight_range, variant_fun = weight_function)
  })
  names(weights_list) <- weight_values
  # Generate samples
  samples_list <- lapply(1:number_of_samples, function(i) {
    sapply(weight_values, function(w) {
      sample(x = 1:nrow(df), size = sample_size, replace = TRUE, prob = weights_list[[as.character(w)]])
    }, USE.NAMES = TRUE)
  })
  # Convert list to array
  samples_array <- array(data = unlist(samples_list, recursive = FALSE), 
                         dim = c(dim(samples_list[[1]]), number_of_samples),
                         dimnames = c(dimnames(samples_list[[1]]), list(paste0("sample_", 1:number_of_samples))))
  return(samples_array[,,1:number_of_samples])
}

#' @title .extract_columns
#' @description This function extracts the specified columns from a data frame for the given samples. 
#' @param df A data frame from which to extract columns.
#' @param column_names A character vector of the names of the columns to be extracted from df.
#' @param sample_indices An array of sampled indices, each element of the array represents the index of a row in df.
#' @return A list of arrays, with each array  containing the values of the respective column for the sampled indices. 
#'   Each array has the same dimensions as sampledIndices. The names of the list elements are the names of column_names.
#' @keywords internal

.extract_columns <- function(df, column_names = c("VAS", "utility_3L", "utility_5L", "utility_xw"), sample_indices) {
  result_list <- lapply(X = column_names, FUN = function(column_name) {
    outv <- array(data = df[sample_indices, column_name], dim = dim(sample_indices), dimnames = dimnames(sample_indices))
    attr(outv, "gr") <- attr(sample_indices, "gr")
    return(outv)
  })
  names(result_list) <- column_names
  return(result_list)
}

#' @title .calculate_quantiles
#' @description This function calculates the mean, standard deviation, and specified quantiles for each column in a provided data array.
#' @param data_array A numeric array where calculations will be performed on each column. The array can be 2D or 3D.
#' @param data_margin An integer that indicates the margin on which to apply the function. Default is 2.
#' @param quantile_levels A named numeric vector of probabilities for which quantiles are required.
#' @return A data frame with columns for each calculated statistic, including mean, standard deviation, and user-specified quantiles.
#' @keywords internal

.calculate_quantiles <- function(data_array, data_margin = 2, quantile_levels = c("min" = 0, "2.5%" = 0.025, "25%" = 0.25, "median" = 0.5, "75%" = 0.75, "97.5%" = 0.975, "max" = 1)) {
  quantile_results <- as.data.frame(t(apply(X = data_array, MARGIN = data_margin, FUN = function(x) {
    c(MEAN = mean(x, na.rm = TRUE), SD = stats::sd(x, na.rm = TRUE), stats::quantile(x, probs = quantile_levels, na.rm = TRUE))
  })))
  return(quantile_results)
}

#' @title .calculate_weighted_statistics
#' @description This function calculate the mean, standard deviation, and quantiles for each column in a provided data array.
#' @param data_array A data array where calculations will be performed on each column.
#' @param quantile_levels A named vector of probabilities for which quantiles are required.
#' @return A data frame with columns for each calculated statistic, including mean, standard deviation, upper and lower bounds, and user-specified quantiles.
#' @keywords internal

.calculate_weighted_statistics <- function(data_array, data_margin = 2, quantile_levels = c("min" = 0, "2.5%" = 0.025, "25%" = 0.25, "median" = 0.5, "75%" = 0.75, "97.5%" = 0.975, "max" = 1)) {
  # Check if quantile_levels is not empty and contains values in [0, 1]
  if (length(quantile_levels) == 0 || any(as.numeric(quantile_levels) < 0) || any(as.numeric(quantile_levels) > 1)) {
    stop("quantile_levels should be a non-empty list containing values in the range [0, 1]")
  }
  # Calculate bootstrapped means
  boot_means <- lapply(data_array, function(y)  (apply(y, MARGIN  = 2, FUN = colMeans)))
  # Calculate weighted quantiles
  weighted_quants <- mapply(function(x, xn) {
    data_array <- .calculate_quantiles(x, quantile_levels, data_margin = 2)
    cbind(type = xn, weight_top = rownames(data_array), data_array)
  }, x = boot_means, xn = names(boot_means), SIMPLIFY = FALSE)
  # Convert weight_top to integer and calculate upper and lower bounds
  weighted_quants <- do.call(rbind, weighted_quants)
  weighted_quants$topval <- as.integer(weighted_quants$weight_top)
  weighted_quants$ub <- weighted_quants$MEAN + weighted_quants$SD
  weighted_quants$lb <- weighted_quants$MEAN - weighted_quants$SD
  return(weighted_quants)
}

#' @title .create_severity_ribbon_plot
#' @description This function creates a ggplot2 plot with confidence intervals and ribbons for the given data frame.
#' @param df A data frame containing the data to be plotted. The data frame should have columns for 'MEAN', 'lb', 'ub', 'topval', and 'type'.
#' @param graph_title A string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A string specifying the title for the y-axis. Default is an empty string.
#' @param legend_name A string specifying the name of the legend. Default is "Type".
#' @param legend_labels A named vector specifying custom labels for the legend. Default is NULL.
#' @param y_axis_limits A numeric vector specifying the limits for the y-axis. Default is c(0.15, 0.95).
#' @param y_min_value A string specifying the column name for the lower bound of the ribbon. Default is "2.5".
#' @param y_max_value A string specifying the column name for the upper bound of the ribbon. Default is "97.5".
#' @param alpha_1 A numeric value between 0 and 1 to define the transparency of the interquartile range. Default is 0.15.
#' @param alpha_2 A numeric value between 0 and 1 to define the transparency of the confidence interval range. Default is 0.05.
#' @param linetype_1 A numeric value between 0 and 1 to define the linetype of the interquartile range. Default is 1 "solid".
#' @param linetype_2 A numeric value between 0 and 1 to define the linetype of the confidence interval range. Default 2 "dashed".
#' @param color_palette A character vector specifying the color palette to use for the plot. Default is a set of 10 colors.
#' @return A ggplot2 object representing the plot.
#' @keywords internal
#' @importFrom rlang .data

.create_severity_ribbon_plot <- function(df, 
                                         graph_title = "", 
                                         x_axis_title = "", 
                                         y_axis_title = "", 
                                         legend_name = "Type", 
                                         legend_labels = NULL,
                                         y_axis_limits = c(0.15, 0.95), 
                                         y_min_value = "2.5%", 
                                         y_max_value = "97.5%",
                                         alpha_1 = 0.5,
                                         alpha_2 = 0.3,
                                         linetype_1 = 5, 
                                         linetype_2 = 2,
                                         color_palette = c("#bebada", "#fb8072","#8dd3c7","#80b1d3", "#ffff67", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")) {
  # Create initial plot
  ribbon_graph <- ggplot(data = df, mapping = aes(x = .data$topval, 
                                                  y = .data$MEAN, 
                                                  ymin = .data$lb, 
                                                  ymax = .data$ub, 
                                                  color = .data$type, 
                                                  fill = .data$type, 
                                                  group = .data$type)) +
    theme_bw() +  
    geom_line(size = 1.2) +
    geom_ribbon(alpha = alpha_1, linetype = linetype_1) +
    geom_ribbon(aes(ymin = df[[y_min_value]], ymax = df[[y_max_value]]), alpha = alpha_2, linetype = linetype_2)
  
  # Add labels, title, and color scales
  ribbon_graph <- ribbon_graph +
    labs(x = x_axis_title, y = y_axis_title, title = graph_title, color = legend_name, fill = legend_name) +
    ylim(y_axis_limits[1], y_axis_limits[2]) +
    theme(legend.position = "bottom",  
          text = element_text(size = 12),  
          axis.title = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold"),
          panel.grid.major = element_line(color = "grey90"),  
          panel.grid.minor = element_line(color = "grey95"))
  
  # Modify legend labels if specified
  if (!is.null(legend_labels)) {
    # Add color scales with custom labels
    ribbon_graph <- ribbon_graph +
      scale_color_manual(values = color_palette, labels = legend_labels) + 
      scale_fill_manual(values = color_palette, labels = legend_labels)
  } else {
    # Add color scales without custom labels
    ribbon_graph <- ribbon_graph +
      scale_color_manual(values = color_palette) + 
      scale_fill_manual(values = color_palette)
  }
  
  return(ribbon_graph)
}

#' @title .merge_adjacent_intervals
#' @description This function takes a character vector of intervals (e.g., "0-25", "25-50") and merges adjacent intervals.
#' @param intervals A character vector of intervals to be merged.
#' @return A character vector of merged intervals.
#' @keywords internal

.merge_adjacent_intervals <- function(intervals) {
  if (length(intervals) <= 1) return(intervals)
  merged_intervals <- c()
  start <- as.numeric(strsplit(intervals[1], "-")[[1]][1])
  end <- as.numeric(strsplit(intervals[1], "-")[[1]][2])
  for (i in 2:length(intervals)) {
    current_start <- as.numeric(strsplit(intervals[i], "-")[[1]][1])
    current_end <- as.numeric(strsplit(intervals[i], "-")[[1]][2])
    if (current_start == end) {
      end <- current_end
    } else {
      merged_intervals <- c(merged_intervals, paste0(start, "-", end))
      start <- current_start
      end <- current_end
    }
  }
  merged_intervals <- c(merged_intervals, paste0(start, "-", end))
  return(merged_intervals)
}

#' @title .get_severity_interpretation
#' @description This function extracts and interprets the results from a given severity ribbon plot.
#' @param ribbon_plot A severity ribbon plot to be interpreted.
#' @param quartiles A numeric vector specifying the quartiles for interpretation. Default is c(0, 0.25, 0.5, 0.75, 1).
#' @param elevation_threshold A numeric value specifying the threshold for elevation differences. Default is 0.05.
#' @param slope_threshold A numeric value specifying the threshold for slope differences. Default is 0.02.
#' @return A list containing the interpretation results for each combination of types in the ribbon plot.
#'         Each list element contains details about crossing, elevation differences, and slope changes.
#' @keywords internal

.get_severity_interpretation <- function(ribbon_plot, 
                                         quartiles = c(0, 0.25, 0.5, 0.75, 1),  
                                         elevation_threshold = 0.05, 
                                         slope_threshold = 0.02) {
  
  # Extract data from the ggplot object
  g <- ggplot_build(ribbon_plot)
  plot_data <- ggplot_build(ribbon_plot)$data[[1]]
  type_data <- split(plot_data, plot_data$group)
  names(type_data) <- g$plot$scales$scales[[3]]$get_labels()
  # Get combinations of types
  combinations <- combn(x = names(type_data), 2)
  result_names <- sapply(1:ncol(combinations), function(i) {
    return(paste0(combinations[1, i], " vs ", combinations[2, i], sep = ""))
  })
  # Helper function to determine slope
  calculate_slope <- function(data, x_start, x_end, q_start, q_end) {
    abs((data[data$x == x_end, "y"] - data[data$x == x_start, "y"])/(q_end - q_start))
  }
  # Get results for each combination
  results <- lapply(1:ncol(combinations), function(i) {
    # Select comparison data
    data1 <- type_data[[combinations[1, i]]]
    data2 <- type_data[[combinations[2, i]]]
    # Get quartile breaks
    quartile_breaks <- quantile(plot_data$x, probs = quartiles)
    range_strings <- sapply(1:(length(quartile_breaks) - 1), function(i) {
      paste0(round(quartile_breaks[i]), "-", round(quartile_breaks[i + 1]))
    })
    # Initialize empty list 
    cross_list <- elevation_list <- slope_change_list <- vector("list", length(quartile_breaks) - 1)
    names(cross_list) <- names(elevation_list) <- names(slope_change_list) <- range_strings
    # Loop though each interval Q1, Q2, Q3, Q4. 
    for (q in 1:(length(quartile_breaks) - 1)) {
      x_start <- quartile_breaks[[q]]
      x_end <- quartile_breaks[[q + 1]]
      data1_subset <- data1[data1$x >= x_start & data1$x <= x_end, ]
      data2_subset <- data2[data2$x >= x_start & data2$x <= x_end, ]
      # Cross
      cross_list[[q]] <- length(unique(sign(data1_subset$y - data2_subset$y))) != 1
      # Elevation
      mean_diff <- mean(data1_subset$y) - mean(data2_subset$y)
      elevation <- list(
        mean_diff = mean_diff,
        category = ifelse(abs(mean_diff) < elevation_threshold, 3,
                          ifelse(mean_diff > 0, 1, 2))
      )
      elevation_list[[q]] <- elevation
      # Slope 
      slope_diff <- calculate_slope(data1_subset, x_start, x_end, quartiles[[q]], quartiles[[q+1]]) - calculate_slope(data2_subset, x_start, x_end, quartiles[[q]], quartiles[[q+1]])
      slope_change <- list(
        slope_diff = slope_diff,
        category = ifelse(abs(slope_diff) < slope_threshold, 3,
                          ifelse(slope_diff > 0, 1, 2))
      )
      slope_change_list[[q]] <- slope_change
    }
    
    return(list(types = c(combinations[1, i], combinations[2, i]), 
                elevation_threshold = elevation_threshold,
                slope_threshold = slope_threshold,
                cross = cross_list, 
                elevation = elevation_list, 
                slope_change = slope_change_list))
  })
  names(results) <- result_names
  return(results)
}

#' @title .write_severity_interpretation
#' @description This function generates the interpretation of the results obtained from a severity ribbon plot.
#' @param ribbon_plot A severity ribbon plot to be interpreted.
#' @param quartiles A numeric vector specifying the quartiles for interpretation. Default is c(0, 0.25, 0.5, 0.75, 1).
#' @param elevation_threshold A numeric value specifying the threshold for elevation differences. Default is 0.05.
#' @param slope_threshold A numeric value specifying the threshold for slope differences. Default is 0.02.
#' @return A character vector containing the interpretation paragraphs for each combination of types in the ribbon plot.
#' @keywords internal

.write_severity_interpretation <- function(ribbon_plot, 
                                          quartiles = c(0, 0.25, 0.5, 0.75, 1),  
                                          elevation_threshold = 0.05, 
                                          slope_threshold = 0.02) {
  # Get results
  intepretation_results <- .get_severity_interpretation(ribbon_plot, quartiles, elevation_threshold, slope_threshold)
  # Write results
  paragraphs <- lapply(intepretation_results, function(res) {
    type1 <- res$types[1]
    type2 <- res$types[2]
    # Cross
    quartile_names <- names(res$cross)
    cross_intervals <- quartile_names[unlist(res$cross)]
    cross_count <- length(cross_intervals)
    if (cross_count == 0) {
      cross_desc <- sprintf("The %s and %s curves did not intersect.", type1, type2)
    } else if (cross_count == 1) {
      cross_desc <- sprintf("The %s and %s curves intersected within the severity range of %s.", type1, type2, cross_intervals[1])
    } else {
      merged_cross_intervals <- .merge_adjacent_intervals(cross_intervals)
      cross_desc <- sprintf("The %s and %s curves intersected within the severity range of %s.", type1, type2, paste(merged_cross_intervals, collapse=", "))
    }
    # Elevation
    elevation_categories <- sapply(res$elevation, function(e) e$category)
    quartile_names <- names(elevation_categories)
    mean_difference <- round(mean(sapply(res$elevation, function(e) e$mean_diff)),2) 
    elevation_type1_intervals <- .merge_adjacent_intervals(quartile_names[elevation_categories == 2])
    elevation_type2_intervals <- .merge_adjacent_intervals(quartile_names[elevation_categories == 1])
    elevation_close_intervals <- .merge_adjacent_intervals(quartile_names[elevation_categories == 3])
    elevation_desc_parts <- list()
    if (length(elevation_type1_intervals) > 0) {
      elevation_desc_parts <- c(elevation_desc_parts, sprintf("For severity values between %s, %s positioned above %s.", 
                                                              paste(elevation_type1_intervals, collapse=", "), type2, type1))
    }
    if (length(elevation_type2_intervals) > 0) {
      elevation_desc_parts <- c(elevation_desc_parts, sprintf("For severity values between %s, %s positioned above %s.", 
                                                              paste(elevation_type2_intervals, collapse=", "), type1, type2))
    }
    if (length(elevation_close_intervals) > 0) {
      elevation_desc_parts <- c(elevation_desc_parts, sprintf("For severity values between %s, %s and %s closely paralleled each other (mean differnce below %s). ", 
                                                              paste(elevation_close_intervals, collapse=", "), type1, type2, sprintf("%0.2f", res$elevation_threshold)))
    }
    elevation_desc_parts <- c(elevation_desc_parts, sprintf("Across the entire severity spectrum, the average difference between %s and %s was %s.", 
                                                            type1, type2, mean_difference))
    elevation_desc <- paste(elevation_desc_parts, collapse=" ")
    
    # Slope Change
    slope_categories <- sapply(res$slope_change, function(s) s$category)
    quartile_names <- names(slope_categories)
    steeper_type1_intervals <- .merge_adjacent_intervals(quartile_names[slope_categories == 1])
    steeper_type2_intervals <- .merge_adjacent_intervals(quartile_names[slope_categories == 2])
    similar_slope_intervals <- .merge_adjacent_intervals(quartile_names[slope_categories == 3])
    slope_desc_parts <- list()
    if (length(steeper_type1_intervals) > 0) {
      slope_desc_parts <- c(slope_desc_parts, sprintf("%s showed a steeper slope than %s throughout the %s severity range.", 
                                                      type1, type2, paste(steeper_type1_intervals, collapse=", ")))
    }
    if (length(steeper_type2_intervals) > 0) {
      slope_desc_parts <- c(slope_desc_parts, sprintf("%s showed a steeper slope than %s throughout the %s severity range.", 
                                                      type2, type1, paste(steeper_type2_intervals, collapse=", ")))
    }
    if (length(similar_slope_intervals) > 0) {
      slope_desc_parts <- c(slope_desc_parts, sprintf("Both %s and %s showed similar slopes throughout the %s severity range.", 
                                                      type1, type2, paste(similar_slope_intervals, collapse=", ")))
    }
    slope_desc <- paste(slope_desc_parts, collapse=" ")
    
    # Combine descriptions
    return(paste(c(cross_desc, elevation_desc, slope_desc), collapse = " "))
  })
  return(paragraphs)
}

#' @title .gen_samples_proportional
#' @description The function takes in a dataframe and a column (factor_column) that dictates the groupings. It then 
#'   generates bootstrap samples ensuring that each sample is proportionally representative of the original dataset based on the given groupings.
#' @param df A dataframe containing the dataset.
#' @param factor_column A string specifying the column name in the dataframe that contains the groupings or factors.
#' @param sample_size An integer indicating the size of each bootstrap sample. Default is 1000.
#' @param number_of_samples An integer indicating the number of bootstrap samples to generate. Default is 1.
#' @return A matrix containing bootstrap samples with rows corresponding to individual samples and columns corresponding to observations in each sample.
#'    The matrix has an attribute "gr" that contains the calculated size for each group to ensure proportional representation.
#' @keywords internal

.gen_samples_proportional<- function(df, factor_column = "vasdecile", sample_size = 1000, number_of_samples = 1000) {
  # Convert column to factor
  grf <- as.factor(df[, factor_column])
  # Calculate group frequencies
  grprops <- table(grf)
  # Calculate number of samples for each group to ensure proportional representation
  integer_grpsize <- function(grprops) {
    grpsize <- sample_size * grprops / NROW(df)
    grpsize_integer <- floor(grpsize)
    # Handling rounding issues
    if(sum(grpsize_integer) < sample_size) {
      grpsize_remain <- grpsize - grpsize_integer
      grpsize_dev <- sum(grpsize - grpsize_integer)
      grpsize_dev <- sample(x = 1:length(grpsize), size = round(grpsize_dev), replace = FALSE, prob = grpsize_remain)
      grpsize_integer[grpsize_dev] <- grpsize_integer[grpsize_dev] + 1  
    }
    return(grpsize_integer)
  }
  grpsizes <- integer_grpsize(grprops)
  # Create a list where each element contains indices of rows corresponding to a particular group
  grp_list <- split(1:NROW(df), grf)
  # Generate bootstrap samples with weights
  generate_samples <- function() {
    samples <- unlist(lapply(names(grpsizes), function(grn) {
      sample(x = grp_list[[grn]], size = grpsizes[grn], replace = TRUE, prob = NULL)
    }))
    return(samples)
  }
  # Generate multiple bootstrap samples
  all_samples <- replicate(number_of_samples, generate_samples())
  all_samples <- t(all_samples)
  attr(all_samples, "gr") <- grpsizes
  return(all_samples)
}

#' @title .cut_variable
#' @description This function cuts a numeric variable into intervals based on the provided breaks.
#' @param variable A numeric vector that you want to cut into intervals.
#' @param breaks A numeric vector specifying the breakpoints for cutting the variable.
#' @return A factor vector representing the intervals into which the variable has been cut.
#' @export

.cut_variable <- function(variable, breaks) {
  if (is.null(breaks)) {
    stop("Please provide breaks for the 'cut' method.")
  }
  return(cut(variable, breaks = breaks, include.lowest = TRUE, right = FALSE))
}

#' @title .factorize_variable
#' @description This function factorizes a numeric variable in a data frame based on a specified variant function and optional breaks.
#' @param df A data frame containing the variable to be factorized.
#' @param weight_column A string specifying the name of the column in the data frame that contains the variable to be factorized.
#' @param variant_fun A function that will be applied to the variable for factorization.
#' @param breaks An optional numeric vector specifying the breakpoints for cutting the variable, if applicable.
#' @return A factor or numeric vector representing the factorized variable.
#' @keywords internal

.factorize_variable <- function(df, weight_column, variant_fun, breaks = NULL){
  # Chech variable exists
  if (!(weight_column %in% names(df))) {
    stop(paste0("The variable '", weight_column, "' is not found in the data frame."))
  }
  # Ensure the variable is numeric
  variable <- df[[weight_column]]
  if (!is.numeric(variable)) {
    stop("The variable must be numeric or factor.")
  }
  # Check if variant_fun is a function
  if (!is.function(variant_fun)) {
    stop("variant_fun must be a function.")
  }
  # Extract the variable from the data frame
  result <- variant_fun(variable, breaks)
  return(result)
}

#' @title .flatten_group_to_df
#' @description This function takes a matrix with an attribute "gr" representing group sizes and flattens it into a data frame, adding a 'gr' column to indicate the group each row belongs to.
#' @param x A matrix containing the data to be flattened. The matrix should have an attribute "gr" that contains the size for each group.
#' @return A data frame containing the flattened data with an additional 'gr' column indicating the group each row belongs to.
#' @keywords internal

.flatten_group_to_df <- function(x) {
  gr <- attr(x, "gr")
  x <- as.data.frame(t(x))
  grns <- names(gr)
  gr <- rep(grns, gr)
  return(cbind(gr = gr, x))
}

#' @title .f_stat_from_df
#' @description The function is designed to compute F-statistic(s) using one-way ANOVA for each numeric column in the provided data frame against a reference column named 'gr'. 
#' @param df A data frame containing one or more numeric columns and a reference column named 'gr'.
#' @param include_p A logical value indicating whether to include the p-values in  the result.
#' @return If include_p is FALSE, a named numeric vector of F-statistics for each column against the 'gr' column. 
#'   If include_p is TRUE, a matrix with  two rows containing F-statistics and p-values, respectively.
#' @keywords internal

.f_stat_from_df <- function(df, include_p = FALSE) {
  df <- as.data.frame(df)
  # Function to compute F-statistic for a given column
  compute_stat <- function(column_name) {
    # Handle special characters in column names by wrapping them in backticks
    safe_column_name <- if (grepl("[[:space:]()]", column_name)) {
      paste0("`", column_name, "`")
    } else {
      column_name
    }
    formula_str <- paste(safe_column_name, "~ gr")
    anova_result <- aov(as.formula(formula_str), data = df)
    summary_matrix <- as.matrix(summary(anova_result)[[1]])
    if (include_p) {
      return(summary_matrix[1, c(4, 5)])  # Return F-statistic and p-value
    } else {
      return(summary_matrix[1, 4])        # Return only F-statistic
    }
  }
  # Apply the function to each column (excluding the 'gr' column)
  columns_to_analyze <- setdiff(colnames(df), "gr")
  result <- sapply(columns_to_analyze, compute_stat)
  return(result)
}

#' @title .compute_ratios
#' @description The function takes in a data frame and a set of utility variables. For each pair of utility variables, 
#'     it computes their ratio and adds a new column to the data frame. The new columns are named in the format "F_ratio_Var1_Var2".
#' @param df A data frame containing the utility variables for which ratios are to be computed.
#' @param combinations A matrix variable specifying the utility_columns combinations.
#' @param utility_columns A character vector specifying the names of the utility variables in the data frame.
#' @return A data frame with additional columns corresponding to the computed ratios.
#' @keywords internal

.compute_ratios <- function(df, combinations, utility_columns) {
  colnames(df)<-c("VAS",utility_columns)
  # Generate combinations of utility variables taken 2 at a time
  new_cols <- lapply(1:ncol(combinations), function(i) {
    pair <- combinations[,i]
    var1 <- as.character(pair[[1]])
    var2 <- as.character(pair[[2]])
    ratio_name <- paste0("F_ratio\n (", var1, " / \n", var2, ")")
    new_col <- df[[var1]] / df[[var2]]
    # Return as named list
    return(stats::setNames(list(new_col), ratio_name))
  })
  # Flatten the list of new columns and bind them to the original data frame
  new_cols_df <- do.call(cbind.data.frame, new_cols)
  df <- cbind(df, new_cols_df)
  return(df)
}

#' @title .plot_F_statistics
#' @description This function creates a bar plot for F-statistics along with error bars for specified utility columns.
#' @param df A data frame containing the F-statistics.
#' @param utility_columns A character vector specifying the names of the utility columns in the data frame.
#' @param graph_title A string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A string specifying the title for the y-axis. Default is an empty string.
#' @param y_min_value A numeric value specifying the minimum limit for the y-axis. Default is NULL.
#' @param y_max_value A numeric value specifying the maximum limit for the y-axis. Default is NULL.
#' @return A ggplot object representing the bar plot with error bars.
#' @keywords internal

.plot_F_statististics <- function(df, utility_columns, utility_combinations = NULL, graph_title = "", x_axis_title = "", y_axis_title = "", y_min_value = NULL, y_max_value = NULL) {
  # Get data frame
  F_ratio_df <- as.data.frame(tail(df, ncol(utility_combinations)))
  F_ratio_df$type <- rownames(F_ratio_df)
  
  # Set the order of the 'type' column to match utility_combinations
  combination_order <- apply(utility_combinations, 2, function(col) {
    paste0("F_ratio\n (", col[1], " / \n", col[2], ")")
  })
  F_ratio_df$type <- factor(F_ratio_df$type, levels = combination_order)
  
  # Create ggplot
  plot <- ggplot(F_ratio_df) +
    theme_bw() + 
    geom_bar(aes(x=.data$type, y=.data$`Full sample`),  stat = "identity", position = "dodge", fill = "#d9d9d9") +
    geom_errorbar(aes(x= .data$type, ymin = .data$`2.5%`, ymax = .data$`97.5%`),
                  width = 0.4,
                  colour = "orange",
                  alpha = 0.9,
                  size = 1.3) + 
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
    geom_text(aes(x = .data$type, y = .data$`Full sample`, label = sprintf("%.2f", .data$`Full sample`)), vjust = -0.5) +
    ggtitle(graph_title) + 
    xlab(x_axis_title) + 
    ylab(y_axis_title) 
  # Add y-axis limits if specifiedj
  if (!is.null(y_min_value) && !is.null(y_max_value)) {
    plot <- plot + ylim(y_min_value, y_max_value)
  }
  # Display the plot
  return(plot)
}

#' @title .get_Fstatistics_interpretation
#' @description This function extracts and interprets the results from a given F-statistics error bar plot.
#' @param errorbar_plot A ggplot object containing an error bar plot.
#' @param utility_combinations A matrix or data frame of utility combinations corresponding to the x-axis labels (default is NULL).
#' @return A list containing the interpretation results for each combination of types in the F-statistic plot.
#'         Each list element contains details about significance, mean, confidence interval.
#' @keywords internal

.get_Fstatistics_interpretation <- function(errorbar_plot, utility_combinations = NULL) {
  # Extract data from the ggplot object
  g <- ggplot_build(errorbar_plot)
  plot_data <- g$data[[1]]
  plot_data$ymin <- g$data[[2]]$ymin
  plot_data$ymax <- g$data[[2]]$ymax
  # Extract x-axis labels
  x_labels <- g$layout$panel_params[[1]]$x$get_labels()
  # Function to compute interpretation for each x-axis label
  interpretation_list <- lapply(seq_along(x_labels), function(i){
    data_subset <- plot_data[plot_data$group == i,]
    return(list(
      "vs_names" = utility_combinations[,i],
      "mean" = data_subset$y,
      "CI_LB" = data_subset$ymin,
      "CI_UB" = data_subset$ymax,
      "significant" = !(1 >= data_subset$ymin & 1 <= data_subset$ymax)
    ))
  })
  names(interpretation_list) <- x_labels
  return(interpretation_list)
}

#' @title .write_Fstatistics_interpretation
#' @description This function generates the interpretation of the results obtained from a F-statistics plot.
#' @param errorbar_plot A ggplot object containing an error bar plot.
#' @param utility_combinations A matrix or data frame of utility combinations corresponding to the x-axis labels (default is NULL). 
#' @return A character vector containing the interpretation paragraphs for each combination of types in the F-statistic plot.
#' @keywords internal

.write_Fstatistics_interpretation <- function(errorbar_plot, utility_combinations = NULL) {
  # Get interpretation results
  interpretation_results <- .get_Fstatistics_interpretation(errorbar_plot, utility_combinations)
  # Use lapply to loop through each interpretation result and generate a summary
  summary_list <- lapply(seq_along(interpretation_results), function(i) {
    vs_names <- interpretation_results[[i]]$vs_names
    if (interpretation_results[[i]]$significant == FALSE){
      if(interpretation_results[[i]]$mean > 1){
        paragraph <- sprintf("The results from the F-statistic comparing %s and %s indicated no statistically significant difference (F-statistic ratio: %.2f, 95%% CI %.2f-%.2f).
                             Although this comparison does not achieve statistical significance, it suggest a higher statistically efficiency in the %s value set compared to %s.", 
                             vs_names[[1]], vs_names[[2]], interpretation_results[[i]]$mean, interpretation_results[[i]]$CI_LB, interpretation_results[[i]]$CI_UB, vs_names[[1]], vs_names[[2]])
      } else {
        paragraph <- sprintf("The results from the F-statistic comparing %s and %s indicated no statistically significant difference (F-statistic ratio: %.2f, 95%% CI %.2f-%.2f).
                             Although this comparison does not achieve statistical significant, it suggest a higher statistically efficiency in the %s value set compared to %s.", 
                             vs_names[[1]], vs_names[[2]], interpretation_results[[i]]$mean, interpretation_results[[i]]$CI_LB, interpretation_results[[i]]$CI_UB, vs_names[[2]], vs_names[[1]])
      }
    } else {
      if (interpretation_results[[i]]$mean > 1){
        paragraph <- sprintf("The %s value set tended to be more discriminative than the %s (F-statistic ratio: %.2f, 95%% CI %.2f-%.2f).", 
                             vs_names[[1]], vs_names[[2]], interpretation_results[[i]]$mean, interpretation_results[[i]]$CI_LB, interpretation_results[[i]]$CI_UB)
      } else{
        paragraph <- sprintf("The %s value set tended to be less discriminative than the %s (F-statistic ratio: %.2f, 95%% CI %.2f-%.2f).", 
                             vs_names[[1]], vs_names[[2]], interpretation_results[[i]]$mean, interpretation_results[[i]]$CI_LB, interpretation_results[[i]]$CI_UB)
      }
    }
    return(paragraph)
  })
  names(summary_list) <- names(interpretation_results)
  return(summary_list)
}

#' @title severity_ribbon_plot
#' @description This function generates a ribbon plot for given utility columns, based on weighted statistics.
#' @param df A data frame containing the utility and weight columns.
#' @param utility_columns A character vector specifying the utility columns for which the ribbon plot will be generated.
#' @param weight_column A string specifying the column that contains the weights. Default is "VAS".
#' @param weight_range A numeric vector specifying the range of weights. Default is c(0:100).
#' @param weight_values A numeric vector specifying the weight values to be used. Default is NULL, in which case the weight_range will be used.
#' @param weight_function A function to generate weights. Default is .makeWeightsMixed.
#' @param sample_size An integer specifying the sample size for bootstrapping. Default is 1000.
#' @param number_of_samples An integer specifying the number of bootstrap samples. Default is 1000.
#' @param probability_levels A named vector specifying the probability levels for quantile. 
#' @param graph_title A string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A string specifying the title for the y-axis. Default is an empty string.
#' @param legend_name A string specifying the name for the legend. Default is "Type".
#' @param legend_labels A character vector specifying the labels for the legend. Default is NULL.
#' @param y_axis_limits A numeric vector specifying the limits for the y-axis. Default is c(0.15, 0.95).
#' @param y_min_value A string specifying the minimum value for the y-axis. 
#' @param y_max_value A string specifying the maximum value for the y-axis. 
#' @param alpha_1 A numeric value between 0 and 1 to define the transparency of the interquartile range. Default is 0.15.
#' @param alpha_2 A numeric value between 0 and 1 to define the transparency of the confidence interval range. Default is 0.05.
#' @param linetype_1 A numeric value between 0 and 1 to define the line type of the interquartile range. Default is 1 "solid".
#' @param linetype_2 A numeric value between 0 and 1 to define the line type of the confidence interval range. Default 2 "dashed".
#' @param color_palette A character vector specifying the color palette for the plot. Default is c("#8dd3c7", "#bebada", "#80b1d3", "#fb8072", "#ffff67", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd").
#' @param interpretation_quartiles A numeric vector of values between 0 and 1 to define the quantile ranges for the figure interpretation. Default is c(0, 0.25, 0.5, 0.75, 1)
#' @param elevation_threshold A numeric value specifying the threshold for elevation differences. Default is 0.05.
#' @param elevation_threshold A numeric value specifying the threshold for elevation differences. Default is 0.05.
#' @param slope_threshold A numeric value specifying the threshold for slope differences. Default is 0.02.
#' @param weighted_statistics An optional data frame of pre-computed weighted statistics. Default is NULL.
#' @return A list containing three elements: 'df' which is a data frame of weighted statistics, 'plot' which is the ggplot object representing the ribbon plot and 'interpretation' with the automatic interpretation of the ribbon plot.
#' @examples
#' \donttest{
#'   # Define dimension names for EQ-5D-3L and EQ-5D-5L
#'   dim.names.3L <- c("mobility", "selfcare", "activity", "pain", "anxiety")
#'   dim.names.5L <- c("mobility5L", "selfcare5L", "activity5L", "pain5L", "anxiety5L")
#'   # Compute EQ-5D scores using the eq5dsuite package
#'   cdta$EQ5D3L <- eq5dsuite::eq5d3l(x = cdta,
#'                                    country = "US", 
#'                                    dim.names = dim.names.3L)
#'   cdta$EQ5D5L <- eq5dsuite::eq5d5l(x = cdta, 
#'                                    country = "US", 
#'                                    dim.names = dim.names.5L)
#'   cdta$EQXW <- eq5dsuite::eqxw(x = cdta, 
#'                                country = "US", 
#'                                dim.names = dim.names.5L)
#'   # Get severity ribbon plot
#'   result <- severity_ribbon_plot(df = cdta, utility_columns = c("EQ5D3L", "EQ5D5L", "EQXW"))
#' }
#' @export

severity_ribbon_plot <- function(df, 
                                 utility_columns, 
                                 weight_column = "VAS", 
                                 weight_range = c(0:100),
                                 weight_values = NULL,
                                 weight_function = .makeWeightsMixed,
                                 sample_size = 1000, 
                                 number_of_samples = 1000, 
                                 probability_levels = c("min" = 0, "2.5%" = 0.025, "25%" = 0.25, "median" = 0.5, "75%" = 0.75, "97.5%" = 0.975, "max" = 1), 
                                 graph_title = "", 
                                 x_axis_title = "", 
                                 y_axis_title = "", 
                                 legend_name = "Type", 
                                 legend_labels = NULL, 
                                 y_axis_limits = c(0.15, 0.95),  
                                 y_min_value = "2.5%", 
                                 y_max_value = "97.5%",
                                 alpha_1 = 0.15,
                                 alpha_2 = 0.05,
                                 linetype_1 = 1, 
                                 linetype_2 = 2,
                                 interpretation_quartiles = c(0, 0.25, 0.5, 0.75, 1),
                                 elevation_threshold = 0.05, 
                                 slope_threshold = 0.02,
                                 color_palette = NULL,
                                 weighted_statistics = NULL){
  
  # Check df
  if (!is.data.frame(df)) {
    stop("Input df must be a data frame.")
  } else {
    if (nrow(df) == 0 || ncol(df) == 0) {
      stop("Input data frame should not be empty.")
    }
  }
  # Check df columns
  if (!is.character(utility_columns) || !is.character(weight_column)) {
    stop("utility_columns and must be of character type.")
  }
  if (length(unique(utility_columns)) != length(utility_columns)) {
    stop("Utility columns should not have duplicates.")
  }
  if (length(utility_columns) > 10){
    stop("Number of utility columns should not exceed 10.")
  }
  unavailable_vars <- setdiff(c(utility_columns, weight_column), names(df))
  if (length(unavailable_vars) > 0) {
    stop(paste0("The variable(s) '", paste(unavailable_vars, collapse = ", "), "' are not found in the data frame."))
  }
  # Check weight_values
  if(is.null(weight_values)){
    weight_values <- weight_range
  } else{
    if (!is.numeric(weight_values)) {
      stop("weight_values must be of numeric type.")
    }
    if (!all(weight_values %in% weight_range)) {
      stop("Some weight values are not included in the weight range.")
    }  
  }
  # Check if weight_function is a function
  if (!is.function(weight_function)) {
    stop("weight_function must be a function.")
  }
  if (is.null(color_palette)){
    color_palette <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF", "#fccde5", "#ffff67", "#80b1d3")
  }
  
  
  # Generate simulated data (if condition default shiny apps)
  if (is.null(weighted_statistics)){
    sample_indices <- .gen_samples(df, weight_column = weight_column, weight_range = weight_range, weight_values = weight_values, weight_function = weight_function, sample_size = sample_size, number_of_samples = number_of_samples) 
    boot_data <- .extract_columns(df, column_names = c(utility_columns, weight_column), sample_indices)
    weighted_statistics <- .calculate_weighted_statistics(boot_data, quantile_levels = probability_levels)
  }
  
  # Analyze and plot
  ribbon_plot <- .create_severity_ribbon_plot(df = weighted_statistics[weighted_statistics$type != weight_column, ],
                                              graph_title = graph_title, 
                                              x_axis_title = x_axis_title, 
                                              y_axis_title = y_axis_title, 
                                              legend_name = legend_name, 
                                              legend_labels = legend_labels,
                                              y_axis_limits = y_axis_limits, 
                                              y_min_value = y_min_value, 
                                              y_max_value = y_max_value, 
                                              alpha_1 = alpha_1, 
                                              alpha_2 = alpha_2, 
                                              linetype_1 = linetype_1,
                                              linetype_2 = linetype_2,
                                              color_palette = color_palette)
  # Get interpretation
  interpretation_description <- .write_severity_interpretation(ribbon_plot, quartiles = interpretation_quartiles, elevation_threshold = elevation_threshold, slope_threshold = slope_threshold)
  return(list(df = weighted_statistics, plot = ribbon_plot, interpretation = interpretation_description))
  
}

#' @title compute_F_statistics
#' @description This function computes F-statistics for specified utility columns in a data frame. 
#' @param df A data frame containing the utility and weight columns.
#' @param utility_columns A character vector specifying the names of utility columns.
#' @param utility_combinations A matrix with two rows indicating the utility columns combinations. Default is all possible combinations of the elements of utility_columns taken 2 at a time.
#' @param weight_column A character string specifying the name of the weight column. Default is "VAS".
#' @param weight_range A numeric vector specifying the range of weights. Default is c(0:100).
#' @param sample_size An integer specifying the sample size for bootstrapping. Default is 1000.
#' @param number_of_samples An integer specifying the number of bootstrap samples. Default is 1000.
#' @param variant_fun A function to be applied for factorizing the weight column. Default is .cut_variable.
#' @param breaks A numeric vector specifying the breaks for the 'cut' method. Default is c(0,10,20,30,40,50,60,70,80,90,100).
#' @param graph_title A character string specifying the title of the plot. Default is an empty string.
#' @param x_axis_title A character string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A character string specifying the title for the y-axis. Default is an empty string.
#' @param y_min_value A numeric value specifying the minimum value for the y-axis. Default is NULL.
#' @param y_max_value A numeric value specifying the maximum value for the y-axis. Default is NULL.
#' @param F_stats_groups An optional data frame of pre-computed group-based F-statistics. Default is NULL.
#' @return A list containing two elements: 'df' which is a data frame of weighted statistics, and 'plot' which is the ggplot object representing the ribbon plot.
#' @examples
#' \donttest{
#'   # Define dimension names for EQ-5D-3L and EQ-5D-5L
#'   dim.names.3L <- c("mobility", "selfcare", "activity", "pain", "anxiety")
#'   dim.names.5L <- c("mobility5L", "selfcare5L", "activity5L", "pain5L", "anxiety5L")
#'   # Compute EQ-5D scores using the eq5dsuite package
#'   cdta$EQ5D3L <- eq5dsuite::eq5d3l(x = cdta,
#'                                    country = "US", 
#'                                    dim.names = dim.names.3L)
#'   cdta$EQ5D5L <- eq5dsuite::eq5d5l(x = cdta, 
#'                                    country = "US", 
#'                                    dim.names = dim.names.5L)
#'   cdta$EQXW <- eq5dsuite::eqxw(x = cdta, 
#'                                country = "US", 
#'                                dim.names = dim.names.5L)
#'   # Define combinations of utility columns for F-statistics calculation
#'   utility_combinations <- matrix(c("EQ5D5L", "EQ5D3L", "EQ5D5L", "EQXW"), nrow = 2)
#'   # Compute F-statistics for the utility columns
#'   result <- compute_F_statistics(df = cdta, 
#'                                  utility_columns = c("EQ5D3L", "EQ5D5L", "EQXW"), 
#'                                  utility_combinations = utility_combinations)
#'   # Plot the results
#'   print(result$plot)
#' }
#' @export

compute_F_statistics <- function(df, 
                                 utility_columns, 
                                 utility_combinations = NULL,
                                 weight_column = "VAS", 
                                 weight_range = c(0:100), 
                                 sample_size = nrow(df), 
                                 number_of_samples = 1000, 
                                 variant_fun = .cut_variable,
                                 breaks = c(0,10,20,30,40,50,60,70,80,90,100), 
                                 graph_title = "", 
                                 x_axis_title = "", 
                                 y_axis_title = "", 
                                 y_min_value = NULL, 
                                 y_max_value = NULL, 
                                 F_stats_groups = NULL) {
  
  if (length(utility_columns) < 2 || length(utility_columns) > 10){
    stop("Number of utility columns should be between 2 amd 10.")
  }
  unavailable_vars <- setdiff(c(utility_columns, weight_column), names(df))
  if (length(unavailable_vars) > 0) {
    stop(paste0("The variable(s) '", paste(unavailable_vars, collapse = ", "), "' are not found in the data frame."))
  }
  # Validate utility combinations
  combinations <- combn(x = utility_columns , 2)
  if (is.null(utility_combinations)){
    utility_combinations <- combn(x = utility_columns , 2)
  } else {
    is_subset <- all(apply(utility_combinations, 2, function(col) {
      any(apply(combinations, 2, function(cmb) {
        all(cmb == col) || all(cmb == rev(col))
      }))
    }))
    if (!is_subset) {
      stop("Provided utility_combinations is not a subset of combinations from utility_columns.")
    }
  }
  
  # Compute factor variable
  df$gr <- .factorize_variable(df, weight_column, variant_fun, breaks = breaks)
  column_names <- c(weight_column, utility_columns)
  
  # All sample
  F_stats_all <- as.data.frame(as.list(.f_stat_from_df(df[, c("gr", column_names)])))
  F_stats_all <- .compute_ratios(F_stats_all, utility_combinations, column_names[-1])
  rownames(F_stats_all) <- "Full sample"
  
  # By groups (if condition default shiny apps)
  if (is.null(F_stats_groups)){
    sample_indices_group <- .gen_samples_proportional(df, factor_column = "gr", sample_size = sample_size, number_of_samples = number_of_samples)
    boot_data_group <- .extract_columns(df, column_names = column_names, sample_indices = sample_indices_group)
    boot_flat_group <- lapply(boot_data_group, FUN = .flatten_group_to_df) 
    F_stats_groups <- as.data.frame(lapply(X = boot_flat_group, FUN = .f_stat_from_df))
  }
  F_stats_groups <- .compute_ratios(F_stats_groups, utility_combinations, column_names[-1])
  
  # F statistics table
  result <- t(rbind(F_stats_all, t(.calculate_quantiles(t(F_stats_groups), data_margin = 1))))
  
  # Plot F statistics
  plot <- .plot_F_statististics(df = result, 
                                utility_columns = utility_columns, 
                                utility_combinations = utility_combinations,
                                graph_title = graph_title, 
                                x_axis_title = x_axis_title, 
                                y_axis_title = y_axis_title, 
                                y_min_value = y_min_value, 
                                y_max_value = y_max_value) 
  # Get interpretation
  interpretation_description <- .write_Fstatistics_interpretation(plot, utility_combinations)
  return(list(df = result, plot = plot, interpretation = interpretation_description))
  # return(list(df = result, plot = plot))
}