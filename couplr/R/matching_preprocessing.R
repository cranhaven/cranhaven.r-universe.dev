# ==============================================================================
# Matching Preprocessing - Automatic scaling and variable health checks
# ==============================================================================

#' Check variable health for matching
#'
#' Analyzes variables for common problems that can affect matching quality:
#' constant columns, high missingness, extreme skewness, and outliers.
#'
#' @param left Data frame of left units
#' @param right Data frame of right units
#' @param vars Character vector of variable names to check
#' @param high_missingness_threshold Threshold for high missingness warning (default: 0.5)
#' @param low_variance_threshold Threshold for nearly-constant variables (default: 1e-6)
#'
#' @return A list with class "variable_health" containing:
#'   - `summary`: Tibble with per-variable diagnostics
#'   - `issues`: List of detected issues with severity levels
#'   - `exclude_vars`: Variables that should be excluded
#'   - `warnings`: Human-readable warnings
#'
#' @keywords internal
check_variable_health <- function(left, right, vars,
                                   high_missingness_threshold = 0.5,
                                   low_variance_threshold = 1e-6) {

  if (length(vars) == 0) {
    stop("No variables provided to check", call. = FALSE)
  }

  # Combine datasets for overall statistics
  combined <- rbind(
    left[, vars, drop = FALSE],
    right[, vars, drop = FALSE]
  )

  issues <- list()
  exclude_vars <- character(0)
  warnings_list <- character(0)

  summary_list <- list()

  for (v in vars) {
    var_data <- combined[[v]]

    # Basic statistics
    n_total <- length(var_data)
    n_na <- sum(is.na(var_data))
    prop_na <- n_na / n_total

    # Get non-NA values
    valid_data <- var_data[!is.na(var_data)]
    n_valid <- length(valid_data)

    # Initialize diagnostics
    var_summary <- list(
      variable = v,
      n_total = n_total,
      n_na = n_na,
      prop_na = prop_na,
      n_valid = n_valid
    )

    # Check for all-NA
    if (n_valid == 0) {
      issues[[length(issues) + 1]] <- list(
        variable = v,
        severity = "error",
        type = "all_na",
        message = sprintf("Variable '%s' is all NA", v)
      )
      exclude_vars <- c(exclude_vars, v)
      warnings_list <- c(warnings_list, sprintf("Excluding '%s': all values are NA", v))

      var_summary$mean <- NA_real_
      var_summary$sd <- NA_real_
      var_summary$min <- NA_real_
      var_summary$max <- NA_real_
      var_summary$range <- NA_real_
      var_summary$skewness <- NA_real_
      var_summary$issue <- "all_na"

    } else {
      # Compute statistics on valid data
      var_mean <- mean(valid_data)
      var_sd <- stats::sd(valid_data)
      var_min <- min(valid_data)
      var_max <- max(valid_data)
      var_range <- var_max - var_min

      var_summary$mean <- var_mean
      var_summary$sd <- var_sd
      var_summary$min <- var_min
      var_summary$max <- var_max
      var_summary$range <- var_range

      # Compute skewness (using moment formula)
      if (var_sd > 0) {
        skew <- mean(((valid_data - var_mean) / var_sd)^3)
        var_summary$skewness <- skew
      } else {
        var_summary$skewness <- NA_real_
      }

      var_summary$issue <- "none"

      # Check for constant column (SD = 0)
      if (var_sd == 0) {
        issues[[length(issues) + 1]] <- list(
          variable = v,
          severity = "error",
          type = "constant",
          message = sprintf("Variable '%s' is constant (SD = 0)", v)
        )
        exclude_vars <- c(exclude_vars, v)
        warnings_list <- c(warnings_list, sprintf("Excluding '%s': constant variable (SD = 0)", v))
        var_summary$issue <- "constant"

      } else if (var_sd < low_variance_threshold) {
        # Nearly constant
        issues[[length(issues) + 1]] <- list(
          variable = v,
          severity = "warning",
          type = "low_variance",
          message = sprintf("Variable '%s' has very low variance (SD = %.2e)", v, var_sd)
        )
        warnings_list <- c(warnings_list, sprintf("Warning: '%s' has very low variance (SD = %.2e)", v, var_sd))
        var_summary$issue <- "low_variance"
      }

      # Check for high missingness
      if (prop_na > high_missingness_threshold) {
        issues[[length(issues) + 1]] <- list(
          variable = v,
          severity = "warning",
          type = "high_missingness",
          message = sprintf("Variable '%s' has %.1f%% missing values", v, prop_na * 100)
        )
        warnings_list <- c(warnings_list, sprintf("Warning: '%s' has %.1f%% missing values", v, prop_na * 100))
        if (var_summary$issue == "none") {
          var_summary$issue <- "high_missingness"
        }
      }

      # Check for extreme skewness (|skewness| > 2)
      if (!is.na(var_summary$skewness) && abs(var_summary$skewness) > 2) {
        issues[[length(issues) + 1]] <- list(
          variable = v,
          severity = "info",
          type = "skewed",
          message = sprintf("Variable '%s' is highly skewed (skewness = %.2f)", v, var_summary$skewness)
        )
        if (var_summary$issue == "none") {
          var_summary$issue <- "skewed"
        }
      }
    }

    summary_list[[length(summary_list) + 1]] <- var_summary
  }

  # Convert to tibble (use bind_rows for robust column name preservation)
  summary_df <- dplyr::bind_rows(lapply(summary_list, tibble::as_tibble))

  structure(
    list(
      summary = summary_df,
      issues = issues,
      exclude_vars = exclude_vars,
      warnings = warnings_list
    ),
    class = "variable_health"
  )
}

#' Suggest scaling method based on variable characteristics
#'
#' Analyzes variable distributions and suggests appropriate scaling methods.
#'
#' @param left Data frame of left units
#' @param right Data frame of right units
#' @param vars Character vector of variable names
#'
#' @return A character string with the suggested scaling method:
#'   "standardize", "range", "robust", or "none"
#'
#' @keywords internal
suggest_scaling <- function(left, right, vars) {

  if (length(vars) == 0) {
    return("none")
  }

  # Combine datasets
  combined <- rbind(
    left[, vars, drop = FALSE],
    right[, vars, drop = FALSE]
  )

  # Compute statistics for each variable
  has_outliers <- FALSE
  has_different_scales <- FALSE

  sds <- numeric(length(vars))
  skews <- numeric(length(vars))

  for (i in seq_along(vars)) {
    v <- vars[i]
    var_data <- combined[[v]][!is.na(combined[[v]])]

    if (length(var_data) == 0) {
      next
    }

    var_mean <- mean(var_data)
    var_sd <- stats::sd(var_data)
    sds[i] <- var_sd

    # Check for skewness
    if (var_sd > 0) {
      skew <- mean(((var_data - var_mean) / var_sd)^3)
      skews[i] <- abs(skew)
    }

    # Check for outliers using IQR method
    q1 <- stats::quantile(var_data, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(var_data, 0.75, na.rm = TRUE)
    iqr <- q3 - q1

    lower_bound <- q1 - 3 * iqr
    upper_bound <- q3 + 3 * iqr

    n_outliers <- sum(var_data < lower_bound | var_data > upper_bound)
    if (n_outliers > length(var_data) * 0.05) {
      has_outliers <- TRUE
    }
  }

  # Check if variables have very different scales
  if (length(sds) > 1) {
    sd_ratio <- max(sds) / (min(sds[sds > 0]) + 1e-10)
    if (sd_ratio > 10) {
      has_different_scales <- TRUE
    }
  }

  # Suggest scaling method
  if (has_outliers || max(skews, na.rm = TRUE) > 2) {
    return("robust")
  } else if (has_different_scales) {
    return("standardize")
  } else if (length(vars) > 1) {
    # Multiple variables with similar scales - still standardize
    return("standardize")
  } else {
    # Single variable or very similar scales
    return("none")
  }
}

#' Automatically encode categorical variables
#'
#' Converts categorical variables to numeric representations suitable for matching.
#' Currently supports binary variables (0/1) and ordered factors.
#'
#' @param left Data frame of left units
#' @param right Data frame of right units
#' @param var Variable name to encode
#'
#' @return List with encoded left and right columns, plus encoding metadata
#'
#' @keywords internal
auto_encode_categorical <- function(left, right, var) {

  left_var <- left[[var]]
  right_var <- right[[var]]

  combined <- c(left_var, right_var)

  # Check if already numeric
  if (is.numeric(combined)) {
    return(list(
      left = left_var,
      right = right_var,
      method = "none",
      metadata = list(type = "numeric")
    ))
  }

  # Check if binary
  unique_vals <- unique(combined[!is.na(combined)])

  if (length(unique_vals) == 2) {
    # Binary variable
    mapping <- c(0, 1)
    names(mapping) <- as.character(unique_vals)

    left_encoded <- as.numeric(factor(left_var, levels = unique_vals)) - 1
    right_encoded <- as.numeric(factor(right_var, levels = unique_vals)) - 1

    return(list(
      left = left_encoded,
      right = right_encoded,
      method = "binary",
      metadata = list(
        type = "binary",
        levels = unique_vals,
        mapping = mapping
      )
    ))
  }

  # Check if ordered factor
  if (is.ordered(left_var) || is.ordered(right_var)) {
    # Use numeric codes
    if (is.ordered(left_var)) {
      levels_order <- levels(left_var)
    } else {
      levels_order <- levels(right_var)
    }

    left_encoded <- as.numeric(factor(left_var, levels = levels_order, ordered = TRUE))
    right_encoded <- as.numeric(factor(right_var, levels = levels_order, ordered = TRUE))

    return(list(
      left = left_encoded,
      right = right_encoded,
      method = "ordered",
      metadata = list(
        type = "ordered",
        levels = levels_order
      )
    ))
  }

  # Unordered categorical - not supported yet
  stop(sprintf("Variable '%s' is categorical but not binary or ordered. Please encode manually or use Gower distance.", var),
       call. = FALSE)
}

#' Preprocess matching variables with automatic checks and scaling
#'
#' Main preprocessing function that orchestrates variable health checks,
#' categorical encoding, and automatic scaling selection.
#'
#' @param left Data frame of left units
#' @param right Data frame of right units
#' @param vars Character vector of variable names
#' @param auto_scale Logical, whether to perform automatic preprocessing (default: TRUE)
#' @param scale_method Scaling method: "auto", "standardize", "range", "robust", or FALSE
#' @param check_health Logical, whether to check variable health (default: TRUE)
#' @param remove_problematic Logical, automatically exclude constant/all-NA variables (default: TRUE)
#' @param verbose Logical, whether to print warnings (default: TRUE)
#'
#' @return A list with class "preprocessing_result" containing:
#'   - `left`: Preprocessed left data frame
#'   - `right`: Preprocessed right data frame
#'   - `vars`: Final variable names (after exclusions)
#'   - `health`: Variable health diagnostics
#'   - `scaling_method`: Selected scaling method
#'   - `excluded_vars`: Variables that were excluded
#'   - `warnings`: List of warnings issued
#'
#' @export
preprocess_matching_vars <- function(left, right, vars,
                                      auto_scale = TRUE,
                                      scale_method = "auto",
                                      check_health = TRUE,
                                      remove_problematic = TRUE,
                                      verbose = TRUE) {

  if (length(vars) == 0) {
    stop("No variables provided for preprocessing", call. = FALSE)
  }

  # Check that variables exist
  missing_left <- setdiff(vars, names(left))
  if (length(missing_left) > 0) {
    stop(sprintf("Variables not found in left: %s", paste(missing_left, collapse = ", ")),
         call. = FALSE)
  }

  missing_right <- setdiff(vars, names(right))
  if (length(missing_right) > 0) {
    stop(sprintf("Variables not found in right: %s", paste(missing_right, collapse = ", ")),
         call. = FALSE)
  }

  warnings_list <- character(0)
  excluded_vars <- character(0)
  health <- NULL

  # Step 1: Check variable health
  if (check_health) {
    health <- check_variable_health(left, right, vars)

    if (verbose && length(health$warnings) > 0) {
      for (w in health$warnings) {
        warning(w, call. = FALSE)
      }
    }

    warnings_list <- c(warnings_list, health$warnings)

    # Remove problematic variables if requested
    if (remove_problematic && length(health$exclude_vars) > 0) {
      excluded_vars <- health$exclude_vars
      vars <- setdiff(vars, excluded_vars)

      if (length(vars) == 0) {
        stop("All variables were excluded due to health issues", call. = FALSE)
      }
    }
  }

  # Step 2: Handle categorical variables (encode them)
  # For now, we'll skip this and assume numeric variables
  # This will be enhanced in future iterations

  # Step 3: Determine scaling method
  final_scale_method <- scale_method

  if (auto_scale && (scale_method == "auto" || scale_method == TRUE)) {
    final_scale_method <- suggest_scaling(left, right, vars)

    if (verbose && final_scale_method != "none") {
      message(sprintf("Auto-selected scaling method: %s", final_scale_method))
    }
  } else if (scale_method == FALSE || scale_method == "none") {
    final_scale_method <- "none"
  }

  # Note: Actual scaling will be applied in build_cost_matrix
  # This function just determines the method

  structure(
    list(
      left = left,
      right = right,
      vars = vars,
      health = health,
      scaling_method = final_scale_method,
      excluded_vars = excluded_vars,
      warnings = warnings_list
    ),
    class = "preprocessing_result"
  )
}

#' Print method for variable health
#'
#' @param x A variable_health object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object `x`.
#' @export
#' @method print variable_health
print.variable_health <- function(x, ...) {
  cat("Variable Health Diagnostics\n")
  cat("===========================\n\n")

  if (length(x$exclude_vars) > 0) {
    cat("Variables to exclude:\n")
    for (v in x$exclude_vars) {
      cat(sprintf("  - %s\n", v))
    }
    cat("\n")
  }

  if (length(x$issues) > 0) {
    cat("Issues detected:\n")
    errors <- sum(sapply(x$issues, function(i) i$severity == "error"))
    warnings <- sum(sapply(x$issues, function(i) i$severity == "warning"))
    infos <- sum(sapply(x$issues, function(i) i$severity == "info"))

    cat(sprintf("  Errors: %d, Warnings: %d, Info: %d\n\n", errors, warnings, infos))
  }

  cat("Variable summary:\n")
  print(x$summary, n = Inf)

  invisible(x)
}

#' Print method for preprocessing result
#'
#' @param x A preprocessing_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object `x`.
#' @export
#' @method print preprocessing_result
print.preprocessing_result <- function(x, ...) {
  cat("Preprocessing Result\n")
  cat("====================\n\n")

  cat(sprintf("Variables: %d\n", length(x$vars)))
  cat(sprintf("Scaling method: %s\n", x$scaling_method))

  if (length(x$excluded_vars) > 0) {
    cat(sprintf("Excluded: %d variables\n", length(x$excluded_vars)))
    for (v in x$excluded_vars) {
      cat(sprintf("  - %s\n", v))
    }
  }

  if (length(x$warnings) > 0) {
    cat(sprintf("\nWarnings: %d\n", length(x$warnings)))
  }

  invisible(x)
}
