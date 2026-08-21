#' Default Value Operator
#'
#' Helper for default NULL values (coalesce)
#'
#' @name op_null_or
#' @rdname op_null_or
#' @param x Left hand side
#' @param y Right hand side
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# Variable names are kept ASCII-safe because several modelling functions parse
# names as part of a formula or lavaan expression.
normalize_variable_names <- function(x) {
  old_names <- names(x)
  if (is.null(old_names)) return(x)

  turkish_names <- old_names
  tr_codes <- c(
    0x00C7, 0x011E, 0x0130, 0x00D6, 0x015E, 0x00DC,
    0x00E7, 0x011F, 0x0131, 0x00F6, 0x015F, 0x00FC
  )
  tr_letters <- vapply(tr_codes, intToUtf8, character(1))
  tr_map <- stats::setNames(
    c("C", "G", "I", "O", "S", "U", "c", "g", "i", "o", "s", "u"),
    tr_letters
  )
  for (letter in names(tr_map)) {
    turkish_names <- gsub(letter, tr_map[[letter]], turkish_names, fixed = TRUE)
  }
  ascii_names <- iconv(turkish_names, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "_")
  ascii_names[is.na(ascii_names) | !nzchar(ascii_names)] <- "variable"
  ascii_names <- gsub("[^A-Za-z0-9_.]", "_", ascii_names)
  ascii_names <- make.names(ascii_names, unique = TRUE)
  names(x) <- ascii_names
  attr(x, "variable_name_map") <- stats::setNames(ascii_names, old_names)
  x
}

# Pull the measurement part of a lavaan model into a named list.
parse_factor_dictionary <- function(model_syntax) {
  if (is.null(model_syntax) || !nzchar(trimws(model_syntax))) return(list())
  lines <- unlist(strsplit(model_syntax, "\\r?\\n"))
  lines <- trimws(sub("#.*$", "", lines))
  lines <- lines[grepl("=~", lines, fixed = TRUE)]
  result <- lapply(lines, function(line) {
    parts <- strsplit(line, "=~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return(NULL)
    factor <- trimws(parts[1])
    items <- trimws(unlist(strsplit(parts[2], "+", fixed = TRUE)))
    items <- trimws(sub("^.*\\*", "", items))
    items <- items[nzchar(items)]
    if (!nzchar(factor) || !length(items)) return(NULL)
    stats::setNames(list(items), factor)
  })
  result <- Filter(Negate(is.null), result)
  if (!length(result)) return(list())

  merged <- list()
  for (entry in result) {
    factor <- names(entry)
    merged[[factor]] <- unique(c(merged[[factor]], entry[[1]]))
  }
  merged
}

observed_factor_dictionary <- function(model_syntax, observed_names) {
  parsed <- parse_factor_dictionary(model_syntax)
  latent_names <- names(parsed)
  observed_names <- as.character(observed_names)
  parsed <- lapply(parsed, function(items) {
    intersect(setdiff(items, latent_names), observed_names)
  })
  Filter(length, parsed)
}

safe_lavaan_name <- function(value) {
  value <- trimws(as.character(value %||% "")[[1]])
  if (!nzchar(value)) return("")

  ascii_value <- iconv(value, from = "", to = "ASCII//TRANSLIT", sub = "_")
  if (is.na(ascii_value) || !nzchar(ascii_value)) ascii_value <- "F"
  make.names(gsub("[^A-Za-z0-9_.]", "_", ascii_value))
}

set_lavaan_measurement <- function(model_syntax, factor, indicators) {
  factor <- safe_lavaan_name(factor)
  indicators <- unique(trimws(as.character(indicators)))
  indicators <- indicators[nzchar(indicators)]
  if (!nzchar(factor)) stop("Enter a factor name.")
  if (!length(indicators)) stop("Select at least one indicator.")

  syntax <- as.character(model_syntax %||% "")[[1]]
  lines <- if (nzchar(trimws(syntax))) {
    unlist(strsplit(syntax, "\\r?\\n"))
  } else {
    character(0)
  }
  measurement_lines <- grepl("=~", lines, fixed = TRUE)
  measurement_lhs <- rep(NA_character_, length(lines))
  measurement_lhs[measurement_lines] <- trimws(sub(
    "=~.*$",
    "",
    lines[measurement_lines]
  ))
  lines <- lines[!(measurement_lines & measurement_lhs == factor)]
  lines <- c(lines, paste0(factor, " =~ ", paste(indicators, collapse = " + ")))
  paste(lines, collapse = "\n")
}

build_second_order_syntax <- function(model_syntax, higher_factor,
                                      lower_factors) {
  higher_factor <- safe_lavaan_name(higher_factor)
  lower_factors <- unique(trimws(as.character(lower_factors)))
  lower_factors <- lower_factors[nzchar(lower_factors)]
  if (!nzchar(higher_factor)) stop("Enter a second-order factor name.")
  if (length(lower_factors) < 2L) {
    stop("Select at least two first-order factors.")
  }
  if (higher_factor %in% lower_factors) {
    stop("The second-order factor cannot be one of its own lower-order factors.")
  }
  set_lavaan_measurement(model_syntax, higher_factor, lower_factors)
}

build_bifactor_syntax <- function(model_syntax, dictionary, general_factor,
                                  group_factors, orthogonal = TRUE) {
  if (!is.list(dictionary)) dictionary <- list()
  general_factor <- safe_lavaan_name(general_factor)
  group_factors <- unique(trimws(as.character(group_factors)))
  group_factors <- group_factors[nzchar(group_factors)]

  if (!nzchar(general_factor)) stop("Enter a general factor name.")
  if (length(group_factors) < 2L) {
    stop("Select at least two group factors for a bifactor model.")
  }
  if (general_factor %in% group_factors) {
    stop("The general factor must have a different name from the group factors.")
  }

  missing_groups <- setdiff(group_factors, names(dictionary))
  if (length(missing_groups)) {
    stop("Group factors are not defined: ", paste(missing_groups, collapse = ", "))
  }
  group_dictionary <- dictionary[group_factors]
  if (any(!lengths(group_dictionary))) {
    stop("Every group factor must contain at least one indicator.")
  }

  general_indicators <- unique(unlist(group_dictionary, use.names = FALSE))
  result <- set_lavaan_measurement(
    model_syntax,
    general_factor,
    general_indicators
  )

  latent_factors <- c(general_factor, group_factors)
  lines <- unlist(strsplit(result, "\\r?\\n"))
  zero_covariance <- vapply(lines, function(line) {
    candidate <- trimws(sub("#.*$", "", line))
    match <- regexec(
      "^([.A-Za-z][.A-Za-z0-9_]*)[[:space:]]*~~[[:space:]]*0[[:space:]]*\\*[[:space:]]*([.A-Za-z][.A-Za-z0-9_]*)$",
      candidate
    )
    parts <- regmatches(candidate, match)[[1]]
    length(parts) == 3L && all(parts[2:3] %in% latent_factors)
  }, logical(1))
  lines <- lines[!zero_covariance]

  if (isTRUE(orthogonal)) {
    pairs <- utils::combn(latent_factors, 2L, simplify = FALSE)
    constraints <- vapply(pairs, function(pair) {
      paste0(pair[[1]], " ~~ 0*", pair[[2]])
    }, character(1))
    lines <- c(lines, constraints)
  }
  paste(lines, collapse = "\n")
}

# Standardize missing-value markers without changing the meaning or storage
# type of categorical columns.
clean_missing_data <- function(data, remove_na = TRUE) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  missing_tokens <- c("", " ", "NA", "N/A", "na", "n/a", ".", "-", "?", "missing")
  cleaned <- as.data.frame(lapply(data, function(column) {
    if (is.character(column)) {
      column[trimws(column) %in% missing_tokens] <- NA_character_
    } else if (is.factor(column)) {
      values <- as.character(column)
      values[trimws(values) %in% missing_tokens] <- NA_character_
      column <- factor(values, levels = levels(column))
    }
    column
  }), check.names = FALSE)

  original_nrow <- nrow(cleaned)

  if (remove_na) {
    cleaned_data_final <- stats::na.omit(cleaned)
    removed_rows_count <- original_nrow - nrow(cleaned_data_final)
  } else {
    cleaned_data_final <- cleaned
    removed_rows_count <- 0
  }

  return(list(
    cleaned_data = cleaned_data_final,
    removed_rows = removed_rows_count
  ))
}

# Reverse-score selected numeric variables with fixed or observed scale bounds.
# A 1-5 item is scored as 6 - response.
reverse_score_variables <- function(data, variables, lower = NULL, upper = NULL) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  variables <- intersect(unique(variables), names(data))
  if (!length(variables)) stop("Select at least one variable to reverse-score.")
  use_fixed_limits <- !is.null(lower) || !is.null(upper)
  if (use_fixed_limits) {
    if (is.null(lower) || is.null(upper) || !is.finite(lower) || !is.finite(upper) || lower >= upper) {
      stop("Enter valid scale limits with the minimum below the maximum.")
    }
  }

  specs <- vector("list", length(variables))
  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    values <- data[[variable]]
    if (!is.numeric(values)) stop(paste0("'", variable, "' is not numeric."))
    finite_values <- values[is.finite(values)]
    if (!length(finite_values)) stop(paste0("'", variable, "' has no finite values."))
    item_lower <- if (use_fixed_limits) lower else min(finite_values)
    item_upper <- if (use_fixed_limits) upper else max(finite_values)
    if (any(finite_values < item_lower | finite_values > item_upper)) {
      stop(paste0("'", variable, "' contains values outside ", item_lower, "-", item_upper, "."))
    }
    constant <- item_lower + item_upper
    data[[variable]] <- ifelse(is.na(values), NA_real_, constant - values)
    specs[[i]] <- data.frame(
      Variable = variable,
      Minimum = item_lower,
      Maximum = item_upper,
      Formula = paste0(format(constant, trim = TRUE), " - ", variable),
      stringsAsFactors = FALSE
    )
  }

  list(data = data, specifications = do.call(rbind, specs))
}

# These helpers cover the small calculations used in the assumptions screen.
standardized_moment <- function(values, order) {
  values <- values[is.finite(values)]
  if (length(values) < 2) return(NA_real_)

  centered <- values - mean(values)
  second_moment <- mean(centered^2)
  if (!is.finite(second_moment) || second_moment <= 0) return(NA_real_)

  mean(centered^order) / second_moment^(order / 2)
}

collinearity_summary <- function(data) {
  result <- data.frame(
    VIF_min = NA_real_, VIF_max = NA_real_,
    TOL_min = NA_real_, TOL_max = NA_real_,
    CI_min = NA_real_, CI_max = NA_real_
  )

  if (ncol(data) < 2) return(result)

  correlation_matrix <- stats::cor(data)
  inverse_correlation <- tryCatch(
    solve(correlation_matrix),
    error = function(e) NULL
  )
  if (!is.null(inverse_correlation)) {
    vif <- diag(inverse_correlation)
    tolerance <- 1 / vif
    finite_vif <- vif[is.finite(vif)]
    finite_tolerance <- tolerance[is.finite(tolerance)]
    if (length(finite_vif)) {
      result$VIF_min <- min(finite_vif)
      result$VIF_max <- max(finite_vif)
    }
    if (length(finite_tolerance)) {
      result$TOL_min <- min(finite_tolerance)
      result$TOL_max <- max(finite_tolerance)
    }
  }

  design_matrix <- cbind(`(Intercept)` = 1, as.matrix(data))
  column_lengths <- sqrt(colSums(design_matrix^2))
  if (all(is.finite(column_lengths)) && all(column_lengths > 0)) {
    scaled_design <- sweep(design_matrix, 2, column_lengths, "/")
    eigenvalues <- eigen(
      crossprod(scaled_design),
      symmetric = TRUE,
      only.values = TRUE
    )$values
    cutoff <- max(eigenvalues) * .Machine$double.eps
    condition_indices <- ifelse(
      eigenvalues > cutoff,
      sqrt(max(eigenvalues) / eigenvalues),
      Inf
    )
    finite_indices <- condition_indices[is.finite(condition_indices)]
    if (length(finite_indices)) {
      result$CI_min <- min(finite_indices)
      result$CI_max <- max(finite_indices)
    }
  }

  result
}

cronbach_alpha_value <- function(data) {
  data <- as.data.frame(data)
  item_count <- ncol(data)
  if (item_count < 2) return(NA_real_)

  covariance_matrix <- stats::cov(data)
  total_variance <- sum(covariance_matrix)
  if (!is.finite(total_variance) || total_variance == 0) return(NA_real_)

  item_count / (item_count - 1) *
    (1 - sum(diag(covariance_matrix)) / total_variance)
}

stratified_alpha_value <- function(data, strata) {
  strata <- as.character(strata)
  if (length(strata) != ncol(data)) {
    stop("Length of strata definition does not match the number of items.")
  }

  stratum_names <- unique(strata)
  error_variances <- vapply(stratum_names, function(stratum) {
    stratum_data <- data[, strata == stratum, drop = FALSE]
    if (ncol(stratum_data) < 2) {
      stop("Each stratum must contain at least two items.")
    }
    stratum_total <- rowSums(stratum_data)
    stratum_variance <- stats::var(stratum_total)
    stratum_variance * (1 - cronbach_alpha_value(stratum_data))
  }, numeric(1))

  total_variance <- stats::var(rowSums(data))
  if (!is.finite(total_variance) || total_variance == 0) {
    stop("The total score has zero variance.")
  }

  1 - sum(error_variances) / total_variance
}

corrected_item_statistics <- function(data) {
  data <- as.data.frame(data)
  if (ncol(data) < 2) stop("At least two items are required.")

  difficulty <- vapply(data, function(item) {
    if (all(is.na(item))) NA_real_ else mean(item, na.rm = TRUE)
  }, numeric(1))

  corrected_reliability <- vapply(seq_along(data), function(index) {
    item <- data[[index]]
    remaining_total <- rowSums(data[-index], na.rm = TRUE)
    available_items <- rowSums(!is.na(data[-index]))
    remaining_total[available_items == 0] <- NA_real_
    item_total_correlation <- suppressWarnings(stats::cor(
      item,
      remaining_total,
      use = "pairwise.complete.obs"
    ))
    observed <- item[is.finite(item)]
    if (length(observed) < 2 || !is.finite(item_total_correlation)) {
      return(NA_real_)
    }
    population_sd <- sqrt(mean((observed - mean(observed))^2))
    item_total_correlation * population_sd
  }, numeric(1))

  data.frame(
    Difficulty = unname(difficulty),
    Item.Rel.woi = corrected_reliability,
    row.names = names(data),
    check.names = FALSE
  )
}

# Assumption checks
#' Calculate Statistical Assumptions
#'
#' Calculates descriptives, multicollinearity, Mahalanobis distance, and Mardia's tests.
#' @param x A numeric data frame.
#' @param mah_p_threshold P-value threshold for Mahalanobis outliers.
#' @return A list of assumption check results.
#' @importFrom mvnormalTest mardia
#' @importFrom stats na.omit mahalanobis pchisq cov median cor
#' @noRd
assumptions <- function(x, mah_p_threshold = 0.001) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  if (ncol(x) == 0) {
    stop("Input data 'x' has 0 columns for assumptions checking.")
  }

  # Assumption statistics are defined only for numeric variables.
  x_numeric <- x
  if (!all(sapply(x_numeric, is.numeric))) {
    warning("Not all columns are numeric in data passed to assumptions(). Attempting conversion.")
    x_numeric <- as.data.frame(lapply(x, function(col) {
      if(!is.numeric(col)) suppressWarnings(as.numeric(as.character(col))) else col
    }))
    if (!all(sapply(x_numeric, is.numeric))) {
      stop("Failed to convert all columns to numeric for assumptions checking.")
    }
  }
  x <- x_numeric # Use the numeric version

  # Descriptive statistics are calculated column by column so that one missing
  # value does not discard an otherwise usable observation.
  descr <- as.data.frame(matrix(NA, nrow = ncol(x), ncol = 8 ))
  colnames(descr) <- c("N", "N (missing)", "Min", "Max", "Median", "Mean", "Skewness", "Kurtosis")
  rownames(descr) <- colnames(x) # Assign row names for clarity

  if (nrow(x) > 0) { # Proceed only if data has rows
    descr[,"N"] <- colSums(!is.na(x))
    descr[,"N (missing)"] <- colSums(is.na(x))
    safe_summary <- function(column, fun) {
      values <- column[!is.na(column)]
      if (!length(values)) return(NA_real_)
      fun(values)
    }
    descr[,"Min"] <- vapply(x, safe_summary, numeric(1), fun = min)
    descr[,"Max"] <- vapply(x, safe_summary, numeric(1), fun = max)
    descr[,"Median"] <- vapply(x, safe_summary, numeric(1), fun = stats::median)
    descr[,"Mean"] <- vapply(x, safe_summary, numeric(1), fun = mean)
    descr[,"Skewness"] <- vapply(x, standardized_moment, numeric(1), order = 3)
    descr[,"Kurtosis"] <- vapply(x, standardized_moment, numeric(1), order = 4) - 3
  } else {
    warning("Input data for 'assumptions' has 0 rows. Descriptive statistics will be NA.")
  }

  # Multivariate checks require complete rows.
  x_complete <- stats::na.omit(x) # Use the already numeric 'x'

  mc_control <- data.frame(VIF_min=NA_real_, VIF_max=NA_real_, TOL_min=NA_real_, TOL_max=NA_real_, CI_min=NA_real_, CI_max=NA_real_)
  Mah_significant <- data.frame(Row_Number_In_Data=integer(), MD=numeric(), MD_p=numeric(), stringsAsFactors = FALSE)

  mardia_default_row <- data.frame(Test="N/A", Statistic=NA_real_, "p-value"=NA_real_, Result=NA_character_, check.names = FALSE, stringsAsFactors = FALSE)
  mardia_kurt_result_df <- mardia_default_row[1, , drop=FALSE]; mardia_kurt_result_df$Test <- "Kurtosis"
  mardia_skew_result_df <- mardia_default_row[1, , drop=FALSE]; mardia_skew_result_df$Test <- "Skewness"
  n_outlier <- 0

  if (nrow(x_complete) >= 2 && ncol(x_complete) >= 1 && nrow(x_complete) > ncol(x_complete)) {
    if (ncol(x_complete) >= 2) {
      mc_control <- collinearity_summary(x_complete)
    }

    # Keep original row positions so removal is correct when some rows are incomplete.
    if (nrow(x_complete) > ncol(x_complete) && ncol(x_complete) > 0) {
      distance <- tryCatch(as.matrix(stats::mahalanobis(x_complete, colMeans(x_complete), cov(x_complete))), error = function(e) NULL)
      if (!is.null(distance)) {
        complete_row_ids <- which(stats::complete.cases(x))
        mahalanobis_table <- data.frame(
          Row_Number_In_Data = complete_row_ids,
          MD = as.numeric(distance),
          stringsAsFactors = FALSE
        )
        mahalanobis_table$MD_p <- stats::pchisq(
          mahalanobis_table$MD,
          df = ncol(x_complete),
          lower.tail = FALSE
        )
        Mah_significant <- mahalanobis_table[
          mahalanobis_table$MD_p <= mah_p_threshold,
          c("Row_Number_In_Data", "MD", "MD_p"),
          drop = FALSE
        ]
        if (nrow(Mah_significant)) {
          row_order <- order(Mah_significant$MD_p, -Mah_significant$MD)
          Mah_significant <- Mah_significant[row_order, , drop = FALSE]
          rownames(Mah_significant) <- NULL
        }
        n_outlier <- nrow(Mah_significant)
      }
    } else {
      warning("Not enough observations relative to variables (N <= P) for Mahalanobis distance.")
    }

    # Mardia's test is reported as separate skewness and kurtosis rows.
    if (nrow(x_complete) >= 2 && ncol(x_complete) >= 2) {
      mardia_test_output <- tryCatch(mvnormalTest::mardia(as.matrix(x_complete)), error = function(e) {
        warning(paste("Mardia test failed:", e$message)); NULL
      })
      if (!is.null(mardia_test_output) && !is.null(mardia_test_output$mv.test) && is.data.frame(mardia_test_output$mv.test)) {
        mardia_full_df <- mardia_test_output$mv.test

        skew_row <- mardia_full_df[mardia_full_df$Test == "Skewness",, drop = FALSE]
        if (nrow(skew_row) == 1) {
          mardia_skew_result_df <- data.frame(
            Test = "Skewness",
            Statistic = as.numeric(as.character(skew_row[1, "Statistic"])),
            "p-value" = as.numeric(as.character(skew_row[1, "p-value"])),
            Result = as.character(skew_row[1, "Result"]),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }

        kurt_row <- mardia_full_df[mardia_full_df$Test == "Kurtosis", , drop = FALSE]
        if (nrow(kurt_row) == 1) {
          mardia_kurt_result_df <- data.frame(
            Test = "Kurtosis",
            Statistic = as.numeric(as.character(kurt_row[1, "Statistic"])),
            "p-value" = as.numeric(as.character(kurt_row[1, "p-value"])),
            Result = as.character(kurt_row[1, "Result"]),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      }
    } else {
      warning("Not enough data (N<2 or P<2) for Mardia's tests.")
    }
  } else {
    warning("Not enough complete cases (N < 2 or N <= P) for some assumption checks.")
  }

  mvn_table <- rbind(mardia_skew_result_df, mardia_kurt_result_df)
  rownames(mvn_table) <- NULL

  return(list(
    descriptives      = round(descr, 2),
    multicollinearity = round(mc_control, 2),
    Mah_significant   = Mah_significant,
    n_outlier         = n_outlier,
    Mardia_Kurtosis   = mardia_kurt_result_df,
    Mardia_Skewness   = mardia_skew_result_df,
    mvn_table         = mvn_table          # All MVN tests in one data frame
  ))
}


# Scree plot
#' Calculate eigenvalues for a scree plot
#'
#' @param x A numeric data frame or matrix.
#' @return A data frame containing component numbers and eigenvalues.
#' @noRd
calculate_scree_eigenvalues <- function(x) {
  if (!is.data.frame(x) && !is.matrix(x)) x <- as.data.frame(x)

  numeric_columns <- vapply(as.data.frame(x), is.numeric, logical(1))
  x <- as.data.frame(x)[, numeric_columns, drop = FALSE]
  if (ncol(x) < 2) stop("Scree plot requires at least two numeric variables.")

  usable_columns <- vapply(x, function(column) {
    values <- column[is.finite(column)]
    length(values) >= 2 && stats::var(values) > 0
  }, logical(1))
  if (!all(usable_columns)) {
    invalid_names <- names(x)[!usable_columns]
    stop(paste(
      "Remove variables with insufficient data or zero variance:",
      paste(invalid_names, collapse = ", ")
    ))
  }

  correlation_matrix <- stats::cor(x, use = "pairwise.complete.obs")
  if (any(!is.finite(correlation_matrix))) {
    stop("The correlation matrix contains missing or infinite values.")
  }

  eigenvalues <- eigen(
    correlation_matrix,
    symmetric = TRUE,
    only.values = TRUE
  )$values

  data.frame(
    Component = seq_along(eigenvalues),
    Eigenvalue = as.numeric(eigenvalues),
    check.names = FALSE
  )
}

#' Draw a standard eigenvalue scree plot
#'
#' @param eigenvalue_table Output from `calculate_scree_eigenvalues()`.
#' @return Invisibly returns the eigenvalue table.
#' @noRd
draw_scree_plot <- function(eigenvalue_table) {
  stopifnot(
    is.data.frame(eigenvalue_table),
    all(c("Component", "Eigenvalue") %in% names(eigenvalue_table))
  )

  old_parameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_parameters), add = TRUE)
  graphics::par(
    family = "sans",
    mar = c(4.5, 4.5, 2.5, 1),
    las = 1
  )

  y_limits <- range(c(0, 1, eigenvalue_table$Eigenvalue), finite = TRUE)
  graphics::plot(
    eigenvalue_table$Component,
    eigenvalue_table$Eigenvalue,
    type = "b",
    pch = 19,
    lwd = 2,
    col = "#2563EB",
    xlab = "Component",
    ylab = "Eigenvalue",
    main = "Scree Plot",
    ylim = y_limits,
    xaxt = "n"
  )
  graphics::axis(1, at = eigenvalue_table$Component)
  graphics::abline(h = 1, col = "#DC2626", lty = 2, lwd = 1.5)
  graphics::grid(col = "#E2E8F0", lty = 1)
  graphics::lines(
    eigenvalue_table$Component,
    eigenvalue_table$Eigenvalue,
    col = "#2563EB",
    lwd = 2
  )
  graphics::points(
    eigenvalue_table$Component,
    eigenvalue_table$Eigenvalue,
    col = "#2563EB",
    pch = 19
  )

  invisible(eigenvalue_table)
}

# Lubbe's categorical parallel analysis keeps each item's marginal distribution
# intact while breaking the relationships among items.
lubbe_parallel_analysis <- function(data, use = "pairwise", SMC = FALSE,
                                    fa = "both", fm = "minres", nfactors = 1,
                                    n.iter = 20, quant = 0.95, seed = NULL, ...) {
  data <- as.data.frame(data, check.names = FALSE)
  fa <- match.arg(fa, c("both", "fa", "pc"))

  n.iter <- as.integer(n.iter)
  if (length(n.iter) != 1L || is.na(n.iter) || n.iter < 1L) {
    stop("The number of permutations must be a positive whole number.")
  }
  if (length(quant) != 1L || (!is.na(quant) &&
      (!is.finite(quant) || quant <= 0 || quant >= 1))) {
    stop("The reference quantile must be between 0 and 1, or NA for the mean.")
  }
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) != 1L || is.na(seed)) {
      stop("The random seed must be a whole number.")
    }
  }

  cases <- nrow(data)
  variables <- ncol(data)
  if (cases < 3L || variables < 2L) {
    stop("At least three rows and two variables are required.")
  }

  usable_values <- vapply(data, function(column) {
    length(unique(column[!is.na(column)]))
  }, integer(1))
  if (any(usable_values < 2L)) {
    stop("Every item must contain at least two observed response values.")
  }

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (!is.null(seed)) set.seed(seed)

  automatic_correlation <- function(values) {
    correlation <- suppressMessages(
      qgraph::cor_auto(values, missing = use, verbose = FALSE, ...)
    )
    correlation <- as.matrix(correlation)
    if (!identical(dim(correlation), c(variables, variables)) ||
        any(!is.finite(correlation))) {
      stop("The automatic correlation matrix could not be estimated.")
    }
    correlation
  }

  count_dimensions <- function(observed, reference) {
    comparison <- observed >= reference
    comparison[is.na(comparison)] <- FALSE
    first_failure <- match(FALSE, comparison)
    if (is.na(first_failure)) length(comparison) else first_failure - 1L
  }

  empirical_correlation <- automatic_correlation(data)
  resampled <- lapply(seq_len(n.iter), function(iteration) {
    as.data.frame(
      lapply(data, function(column) sample(column, length(column), replace = FALSE)),
      check.names = FALSE
    )
  })
  models <- if (identical(fa, "both")) c("fa", "pc") else fa
  results <- list()

  if ("fa" %in% models) {
    if (isTRUE(SMC)) {
      fa_empirical_correlation <- empirical_correlation
      diag(fa_empirical_correlation) <- psych::smc(fa_empirical_correlation)
      fa_empirical <- eigen(
        fa_empirical_correlation, symmetric = TRUE, only.values = TRUE
      )$values
    } else {
      fa_empirical <- psych::fa(
        empirical_correlation, n.obs = cases, nfactors = nfactors,
        rotate = "none", fm = fm, warnings = FALSE
      )$values
    }

    fa_resampled <- vapply(resampled, function(values) {
      resampled_correlation <- automatic_correlation(values)
      if (isTRUE(SMC)) {
        diag(resampled_correlation) <- psych::smc(resampled_correlation)
        eigen(
          resampled_correlation, symmetric = TRUE, only.values = TRUE
        )$values
      } else {
        psych::fa(
          resampled_correlation, n.obs = cases, nfactors = nfactors,
          rotate = "none", fm = fm, warnings = FALSE
        )$values
      }
    }, numeric(variables))

    fa_reference <- if (is.na(quant)) {
      rowMeans(fa_resampled)
    } else {
      apply(fa_resampled, 1L, stats::quantile, probs = quant, names = FALSE)
    }
    results$fa.values <- fa_empirical
    results$fa.sim <- fa_reference
    results$nfact <- count_dimensions(fa_empirical, fa_reference)
  }

  if ("pc" %in% models) {
    pc_empirical <- eigen(
      empirical_correlation, symmetric = TRUE, only.values = TRUE
    )$values
    pc_resampled <- vapply(resampled, function(values) {
      eigen(
        automatic_correlation(values), symmetric = TRUE, only.values = TRUE
      )$values
    }, numeric(variables))

    pc_reference <- if (is.na(quant)) {
      rowMeans(pc_resampled)
    } else {
      apply(pc_resampled, 1L, stats::quantile, probs = quant, names = FALSE)
    }
    results$pc.values <- pc_empirical
    results$pc.sim <- pc_reference
    results$ncomp <- count_dimensions(pc_empirical, pc_reference)
    results$values <- t(pc_resampled)
  }

  results$iterations <- n.iter
  results$quantile <- quant
  results
}

# Factor retention
#' Factor Retention Methods
#'
#' Applies methods to suggest the number of factors to retain.
#' @param x A numeric data frame or matrix.
#' @param method Character string for the retention method.
#' @param n.iter Number of permutations for the Lubbe parallel analysis.
#' @param quant Reference quantile for the Lubbe parallel analysis.
#' @param seed Random seed for the Lubbe parallel analysis.
#' @return A data frame with suggested number of factors.
#' @importFrom stats na.omit
#' @importFrom EFA.MRFA parallelMRFA hullEFA
#' @importFrom psych fa.parallel principal
#' @importFrom EFA.dimensions MAP EMPKC
#' @importFrom EGAnet EGA
#' @importFrom EFAtools CD
#' @noRd
factor_ret <- function(x, method = "hull_method", n.iter = 100,
                       quant = 0.95, seed = 2026) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    x <- as.data.frame(x)
  }
  supported_columns <- if (identical(method, "pa_lubbe")) {
    vapply(x, function(column) is.numeric(column) || is.factor(column), logical(1))
  } else {
    vapply(x, is.numeric, logical(1))
  }
  if (!all(supported_columns)) {
    unsupported_columns <- colnames(x)[!supported_columns]
    warning(paste0(
      "Unsupported columns were excluded from factor retention: ",
      paste(unsupported_columns, collapse = ", ")
    ))
    x <- x[, supported_columns, drop = FALSE]
    if(ncol(x) < 2) {
      return(data.frame(Suggested_Factors = NA, row.names = paste("Error in", method, ": Requires at least 2 usable columns.")))
    }
  }

  if (method == "pa_lubbe") {
    return(tryCatch({
      lubbe_result <- lubbe_parallel_analysis(
        x,
        fa = "pc",
        n.iter = n.iter,
        quant = quant,
        seed = seed
      )
      data.frame(
        Suggested_Factors = lubbe_result$ncomp,
        Permutations = lubbe_result$iterations,
        Reference_Quantile = lubbe_result$quantile,
        row.names = "Parallel Analysis (Lubbe, 2019; permutation PCA)"
      )
    }, error = function(e) {
      data.frame(
        Suggested_Factors = NA,
        row.names = paste("Error in pa_lubbe:", conditionMessage(e))
      )
    }))
  }

  x_complete <- stats::na.omit(x)

  if (nrow(x_complete) < max(3, ncol(x_complete) +1 ) || ncol(x_complete) < 2 ) {
    return(data.frame(Suggested_Factors = NA, row.names = paste("Error in", method, ": Insufficient data (N=",nrow(x_complete),", P=",ncol(x_complete),")")))
  }

  col_variances <- apply(x_complete, 2, var)
  if (any(col_variances == 0, na.rm = TRUE)) {
    zero_var_cols <- colnames(x_complete)[col_variances == 0]
    return(data.frame(Suggested_Factors = NA, row.names = paste("Error in", method, ": Zero variance in column(s)")))
  }

  if (method == "pa_mrfa") {
    return(tryCatch({
      op_pa_analysis <- EFA.MRFA::parallelMRFA(x_complete)
      data.frame(Suggested_Factors = op_pa_analysis$N_factors_percentiles, row.names = "Optimal Parallel Analysis (MRFA)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in pa_mrfa:", conditionMessage(e)))
    }))
  } else if (method == "pa_traditional") {
    return(tryCatch({
      tra_pa_analysis <- psych::fa.parallel(x_complete, fa = "fa", plot = FALSE, show.legend = FALSE, error.bars = FALSE, fm="pa")
      data.frame(Suggested_Factors = tra_pa_analysis$nfact, row.names = "Traditional Parallel Analysis (FA based)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in pa_traditional:", conditionMessage(e)))
    }))
  } else if (method == "hull_method") {
    return(tryCatch({
      hull_analysis <- EFA.MRFA::hullEFA(x_complete, display = FALSE)
      data.frame(Suggested_Factors = hull_analysis$n_factors, row.names = "Hull Method (EFA)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in hull_method:", conditionMessage(e)))
    }))
  } else if (method == "map_method_tra") {
    return(tryCatch({
      map_analysis <- suppressMessages(EFA.dimensions::MAP(x_complete, corkind = "pearson", verbose = FALSE))
      data.frame(Suggested_Factors = map_analysis$NfactorsMAP, row.names = "Minimum Average Partial (MAP - Original)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in map_method_tra:", conditionMessage(e)))
    }))
  } else if (method == "map_method_rev") {
    return(tryCatch({
      map_analysis <- suppressMessages(EFA.dimensions::MAP(x_complete, corkind = "pearson", verbose = FALSE))
      data.frame(Suggested_Factors = map_analysis$NfactorsMAP4, row.names = "Minimum Average Partial (MAP - Revised)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in map_method_rev:", conditionMessage(e)))
    }))
  } else if (method == "EGA_tmfg") {
    return(tryCatch({
      ega_analysis_tmfg <- EGAnet::EGA(data = x_complete, model = "TMFG", plot.EGA = FALSE, verbose = FALSE)
      data.frame(Suggested_Factors = ega_analysis_tmfg$n.dim, row.names = "Exploratory Graph Analysis (TMFG)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in EGA_tmfg:", conditionMessage(e)))
    }))
  } else if (method == "EGA_glasso") {
    return(tryCatch({
      ega_analysis_glasso <- EGAnet::EGA(data = x_complete, model = "glasso", plot.EGA = FALSE, verbose = FALSE)
      data.frame(Suggested_Factors = ega_analysis_glasso$n.dim, row.names = "Exploratory Graph Analysis (Glasso)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in EGA_glasso:", conditionMessage(e)))
    }))
  } else if (method == "EK_C") {
    return(tryCatch({
      emkpc_analysis <- suppressMessages(EFA.dimensions::EMPKC(x_complete, corkind = "pearson", verbose = FALSE))
      data.frame(Suggested_Factors = emkpc_analysis$NfactorsEMPKC, row.names = "Empirical Kaiser Criterion (EKC)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in EK_C:", conditionMessage(e)))
    }))
  } else if (method == "comp_data_method") {
    return(tryCatch({
      cd_analysis <- EFAtools::CD(x_complete)
      data.frame(Suggested_Factors = cd_analysis$n_factors, row.names = "Comparison Data (CD)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in comp_data_method:", conditionMessage(e)))
    }))
  } else {
    return(data.frame(Suggested_Factors = NA, row.names = paste("Error: Unknown method", method)))
  }
}

# Stratified Alpha follows the item order defined under each dimension.
build_stratified_alpha_spec <- function(dictionary, data_names) {
  if (!is.list(dictionary)) dictionary <- list()
  dictionary <- Filter(length, dictionary)
  if (length(dictionary) < 2) {
    stop("Define or select at least two dimensions for Stratified Alpha.")
  }

  items <- unlist(dictionary, use.names = FALSE)
  duplicated_items <- unique(items[duplicated(items)])
  if (length(duplicated_items)) {
    stop(
      "Each item can belong to only one Stratified Alpha dimension: ",
      paste(duplicated_items, collapse = ", ")
    )
  }

  missing_items <- setdiff(items, data_names)
  if (length(missing_items)) {
    stop(
      "Model items are not in the active dataset: ",
      paste(missing_items, collapse = ", ")
    )
  }

  strata <- unlist(lapply(seq_along(dictionary), function(index) {
    rep(index, length(dictionary[[index]]))
  }))
  list(
    items = items,
    strata = paste(strata, collapse = ",")
  )
}

# Reliability coefficients
#' Calculate Reliability Coefficients
#'
#' Calculates various reliability coefficients like Alpha, Omega, Theta, etc.
#' @param x A numeric data frame or matrix.
#' @param method Reliability method ("alpha", "omega", "omega_h", "theta",
#'   "s_alpha", or "cr").
#' @param cor_kind Correlation type (for "theta" and "cr").
#' @param defined_structure Lavaan model syntax (for "cr").
#' @param strata_define Strata definition string (for "s_alpha").
#' @return A string with the formatted reliability coefficient or an error message.
#' @importFrom psych alpha omega principal
#' @importFrom lavaan lavaanify cfa
#' @importFrom stats na.omit cov
#' @noRd
reliability_func <- function(x, method = "alpha", cor_kind = "cor", defined_structure = NULL, strata_define = NULL) {
  if (!is.data.frame(x) && !is.matrix(x)) x <- as.data.frame(x)
  if (!all(sapply(x, is.numeric))) {
    x_numeric_cols <- sapply(x, is.numeric)
    warning(paste0("Non-numeric columns found and will be excluded from reliability analysis: ", paste(colnames(x)[!x_numeric_cols], collapse=", ")))
    x <- x[, x_numeric_cols, drop=FALSE]
    if(ncol(x) < 2) stop("At least two numeric columns required for reliability analysis.")
  }
  x_complete <- stats::na.omit(x)
  if (nrow(x_complete) < 2 || ncol(x_complete) < 2 ) {
    stop("Not enough data (rows/columns) after NA removal and numeric filtering for reliability analysis.")
  }

  result_value <- tryCatch({
    if (method == "alpha") {
      alpha_analysis <- psych::alpha(x_complete, check.keys = FALSE)
      alpha_total_df <- as.data.frame(alpha_analysis$total)
      std_alpha_val <- alpha_total_df[["std.alpha"]] %||% alpha_total_df[["raw_alpha"]] %||% alpha_total_df[1,1] %||% NA_real_
      as.numeric(std_alpha_val)
    } else if (method == "omega") {
      omega_psych <- tryCatch(psych::omega(x_complete, plot=FALSE, fm="pa"), error = function(e) {
        warning(paste("psych::omega failed:", e$message, "Trying a one-factor fallback.")); NULL
      })
      if(!is.null(omega_psych) && "omega.tot" %in% names(omega_psych)){
        return(omega_psych$omega.tot)
      } else {
        fallback_fit <- tryCatch(
          psych::fa(x_complete, nfactors = 1, rotate = "none", fm = "pa"),
          error = function(e) NULL
        )
        if (is.null(fallback_fit)) {
          warning("Omega calculation failed.")
          return(NA_real_)
        }
        loadings <- as.numeric(fallback_fit$loadings[, 1])
        uniquenesses <- as.numeric(fallback_fit$uniquenesses)
        denominator <- sum(loadings)^2 + sum(uniquenesses)
        if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
        return(sum(loadings)^2 / denominator)
      }
    } else if (method == "theta") {
      armor_theta_calc <- function(data_in, correlation_type_internal = "cor") {
        num_items <- ncol(data_in)
        if (num_items < 2) return(NA_real_)
        valid_cor_types <- c("cor", "cov", "poly", "tet")
        if(!(correlation_type_internal %in% valid_cor_types)) {
          warning(paste("Invalid correlation_type '", correlation_type_internal, "' for psych::principal. Defaulting to 'cor'."))
          correlation_type_internal <- "cor"
        }
        pca_res <- psych::principal(data_in, nfactors = 1, rotate = "none", cor = correlation_type_internal)
        first_eigenvalue <- pca_res$Vaccounted[1, 1]
        if (is.na(first_eigenvalue) || (first_eigenvalue <= 1 && num_items > 1) || num_items <= 1) {
          return(NA_real_)
        }
        theta_val <- (num_items / (num_items - 1)) * (1 - (1 / first_eigenvalue))
        return(theta_val)
      }
      armor_theta_calc(x_complete, correlation_type_internal = cor_kind)
    } else if (method == "s_alpha") {
      if (is.null(strata_define) || nchar(trimws(strata_define)) == 0) {
        stop("Strata definition must be provided for 'stratified alpha'.")
      }
      strata_num_vector <- suppressWarnings(as.numeric(unlist(strsplit(strata_define, ","))))
      if(any(is.na(strata_num_vector))) stop("Strata definition contains non-numeric values.")
      if (length(strata_num_vector) != ncol(x_complete)) {
        stop("Length of strata definition does not match the number of items.")
      }
      stratified_alpha_value(x_complete, strata_num_vector)
    } else if (method == "omega_h") {
      omega_res <- tryCatch(
        psych::omega(x_complete, plot = FALSE, fm = "pa"),
        error = function(e) NULL
      )
      if (!is.null(omega_res) && "omega_h" %in% names(omega_res)) {
        return(omega_res$omega_h)
      }
      warning("omega_h: psych::omega failed or omega_h not found.")
      return(NA_real_)

    } else if (method == "cr") {
      if (is.null(defined_structure) || nchar(trimws(defined_structure)) == 0) {
        stop("A lavaan CFA model syntax is required for Composite Reliability (CR) and AVE.")
      }
      cr_parts <- lavaan::lavaanify(defined_structure)
      factor_names_cr <- unique(cr_parts$lhs[cr_parts$op == "=~"])
      manifest_vars_cr <- setdiff(unique(cr_parts$rhs[cr_parts$op == "=~"]), factor_names_cr)
      ordered_arg_cr <- if (cor_kind == "poly" && length(manifest_vars_cr) > 0) manifest_vars_cr else FALSE
      estimator_cr   <- if (cor_kind == "poly" && length(manifest_vars_cr) > 0) "WLSMV" else "ML"

      cfa_fit_cr <- lavaan::cfa(
        model = defined_structure, data = x_complete,
        ordered = ordered_arg_cr, estimator = estimator_cr, warn = FALSE
      )
      std_sol_cr  <- lavaan::standardizedSolution(cfa_fit_cr)
      load_rows   <- std_sol_cr[std_sol_cr$op == "=~", , drop = FALSE]
      factors     <- unique(load_rows$lhs)

      lines <- vapply(factors, function(f) {
        l       <- load_rows[load_rows$lhs == f, "est.std"]
        l       <- l[!is.na(l)]
        cr_val  <- sum(l)^2 / (sum(l)^2 + sum(1 - l^2))
        ave_val <- sum(l^2) / (sum(l^2) + sum(1 - l^2))
        sprintf("%s  ->  CR = %.3f  |  AVE = %.3f", f, cr_val, ave_val)
      }, character(1))

      header <- paste0(rep("-", 42), collapse = "")
      return(paste(c(header, lines, header), collapse = "\n"))

    } else {
      stop(paste("Unknown reliability method:", method))
    }
  }, error = function(e) {
    warning(paste("Error in reliability_func method '", method, "': ", e$message, sep=""))
    return(NA_real_)
  })

  if (is.na(result_value)) {
    return("Calculation failed or N/A.")
  } else {
    return(sprintf("%.3f", as.numeric(result_value)))
  }
}

# Missing-value methods are shared by normal runs and project restoration.
apply_imputation_method <- function(raw_df, method = "none") {
  if (identical(method, "listwise")) {
    return(stats::na.omit(raw_df))
  }

  if (identical(method, "mean")) {
    return(as.data.frame(lapply(raw_df, function(column) {
      if (is.numeric(column)) column[is.na(column)] <- mean(column, na.rm = TRUE)
      column
    }), check.names = FALSE))
  }

  if (identical(method, "median")) {
    return(as.data.frame(lapply(raw_df, function(column) {
      if (is.numeric(column)) {
        column[is.na(column)] <- stats::median(column, na.rm = TRUE)
      }
      column
    }), check.names = FALSE))
  }

  if (identical(method, "amelia")) {
    if (!requireNamespace("Amelia", quietly = TRUE)) stop("Package 'Amelia' required.")
    return(Amelia::amelia(raw_df, m = 1, p2s = 0, idvars = NULL)$imputations[[1]])
  }

  if (identical(method, "mice")) {
    if (!requireNamespace("mice", quietly = TRUE)) stop("Package 'mice' required.")
    return(mice::complete(mice::mice(raw_df, m = 1, printFlag = FALSE), 1))
  }

  if (identical(method, "missForest_cont")) {
    if (!requireNamespace("missForest", quietly = TRUE)) stop("Package 'missForest' required.")
    prepared <- raw_df
    prepared[] <- lapply(prepared, function(column) {
      if (is.character(column)) factor(column) else column
    })
    imputed <- missForest::missForest(prepared, verbose = FALSE)$ximp
    for (variable in names(raw_df)) {
      if (is.character(raw_df[[variable]])) {
        imputed[[variable]] <- as.character(imputed[[variable]])
      }
    }
    return(imputed)
  }

  if (identical(method, "missForest_cat")) {
    if (!requireNamespace("missForest", quietly = TRUE)) stop("Package 'missForest' required.")
    prepared <- raw_df
    prepared[] <- lapply(prepared, as.factor)
    imputed <- missForest::missForest(prepared, verbose = FALSE)$ximp
    for (variable in names(raw_df)) {
      if (is.numeric(raw_df[[variable]])) {
        imputed[[variable]] <- as.numeric(as.character(imputed[[variable]]))
      } else if (is.character(raw_df[[variable]])) {
        imputed[[variable]] <- as.character(imputed[[variable]])
      }
    }
    return(imputed)
  }

  raw_df
}

# Item weighting
#' Item Weighting Function
#'
#' Applies a specific item weighting algorithm.
#' @param x A numeric data frame.
#' @return A data frame with weighted scores.
#' @importFrom stats na.omit
#' @noRd
item_weighting <- function(x) {
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (!all(vapply(x, is.numeric, logical(1)))) stop("Item weighting requires numeric variables.")
  scale_max <- max(unlist(x, use.names = FALSE), na.rm = TRUE)
  if (!is.finite(scale_max) || scale_max == 0) stop("A positive finite maximum score is required.")

  item_stats <- corrected_item_statistics(x)
  item_difficulty <- item_stats$Difficulty / scale_max
  respondent_average <- rowSums(x, na.rm = TRUE) / (rowSums(!is.na(x)) * scale_max)
  threshold_matrix <- outer(respondent_average, item_difficulty, "+")
  adjustment_matrix <- matrix(
    item_stats$Item.Rel.woi,
    nrow = nrow(x), ncol = ncol(x), byrow = TRUE
  )
  source_matrix <- as.matrix(x)
  weighted_matrix <- ifelse(threshold_matrix >= 1, source_matrix + adjustment_matrix, source_matrix)
  weighted_data <- as.data.frame(weighted_matrix, check.names = FALSE)
  names(weighted_data) <- names(x)
  weighted_data
}
