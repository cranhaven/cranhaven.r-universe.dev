# FAfA Dynamic Fit Index module
# Copyright (C) 2025-2026 Abdullah Faruk KILIC and Ahmet Caliskan
# Upstream copyright holders: Melissa G. Wolf and Daniel McNeish
#
# Original source: dynamic R package, version 1.1.0, by Melissa G. Wolf and
# Daniel McNeish, distributed under the GNU Affero General Public License
# version 3 (AGPL-3).
#
# Adaptation note: the simulation design was adapted for FAfA on 2026-08-01.
# The implementation was rewritten around FAfA's lavaan workflow; upstream
# function bodies were not copied verbatim. See inst/COPYRIGHTS for details.
#
# This file is distributed under the GNU Affero General Public License,
# version 3. It is provided without any warranty.

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
parse_dynamic_mad <- function(value) {
  if (is.numeric(value)) {
    result <- as.numeric(value)
  } else {
    parts <- unlist(strsplit(as.character(value %||% ""), "[,;[:space:]]+"))
    parts <- parts[nzchar(parts)]
    result <- suppressWarnings(as.numeric(parts))
  }
  if (!length(result) || any(!is.finite(result)) || any(result <= 0 | result >= 1)) {
    stop("MAD values must be numbers between 0 and 1.", call. = FALSE)
  }
  unique(result)
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
normalize_dynamic_cutoffs <- function(result, language = "en") {
  cutoffs <- result$cutoffs %||% result
  if (is.null(cutoffs)) return(NULL)
  cutoff_table <- as.data.frame(cutoffs, stringsAsFactors = FALSE, check.names = FALSE)
  criterion <- trimws(rownames(cutoff_table) %||% rep("", nrow(cutoff_table)))
  criterion[!nzchar(criterion)] <- fafa_text(language, "Detail", "Ayr\u0131nt\u0131")
  criterion <- make.unique(criterion, sep = " ")
  rownames(cutoff_table) <- NULL
  criterion_name <- fafa_text(language, "Criterion", "\u00d6l\u00e7\u00fct")
  cutoff_table <- cbind(stats::setNames(data.frame(criterion), criterion_name), cutoff_table)
  cutoff_table[] <- lapply(cutoff_table, as.character)
  cutoff_table
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_off_diagonal_mad <- function(x, reference = NULL) {
  if (!is.matrix(x) || nrow(x) != ncol(x) || nrow(x) < 2L) {
    stop("A square matrix with at least two variables is required.", call. = FALSE)
  }
  difference <- if (is.null(reference)) x else x - reference
  mean(abs(difference[lower.tri(difference)]), na.rm = TRUE)
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_nearest_correlation <- function(x, tolerance = 1e-8) {
  x <- as.matrix(x)
  x <- (x + t(x)) / 2
  diag(x) <- 1

  decomposition <- eigen(x, symmetric = TRUE)
  values <- pmax(decomposition$values, tolerance)
  adjusted <- sweep(decomposition$vectors, 2L, values, `*`) %*%
    t(decomposition$vectors)
  scales <- sqrt(pmax(diag(adjusted), tolerance))
  adjusted <- adjusted / outer(scales, scales)
  adjusted <- (adjusted + t(adjusted)) / 2
  diag(adjusted) <- 1
  dimnames(adjusted) <- dimnames(x)
  adjusted
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_discrepancy_direction <- function(model_correlation,
                                          observed_correlation = NULL) {
  dimension <- nrow(model_correlation)
  random_part <- matrix(stats::rnorm(dimension * dimension), dimension, dimension)
  random_part <- (random_part + t(random_part)) / 2
  diag(random_part) <- 0

  if (!is.null(observed_correlation) &&
      identical(dim(observed_correlation), dim(model_correlation)) &&
      all(is.finite(observed_correlation))) {
    empirical_part <- observed_correlation - model_correlation
    diag(empirical_part) <- 0
    empirical_scale <- dynamic_off_diagonal_mad(empirical_part)
    if (is.finite(empirical_scale) && empirical_scale > 1e-8) {
      empirical_part <- empirical_part / empirical_scale
      random_part <- random_part + empirical_part
    }
  }

  direction_scale <- dynamic_off_diagonal_mad(random_part)
  if (!is.finite(direction_scale) || direction_scale <= 1e-8) {
    stop("A discrepancy direction could not be generated.", call. = FALSE)
  }
  random_part / direction_scale
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_misspecified_correlation <- function(model_correlation,
                                             target_mad,
                                             observed_correlation = NULL) {
  if (!is.finite(target_mad) || target_mad <= 0 || target_mad >= 1) {
    stop("The target MAD must be between 0 and 1.", call. = FALSE)
  }

  direction <- dynamic_discrepancy_direction(
    model_correlation,
    observed_correlation
  )
  candidate_at <- function(multiplier) {
    dynamic_nearest_correlation(model_correlation + multiplier * direction)
  }

  lower <- 0
  upper <- target_mad
  upper_candidate <- candidate_at(upper)
  upper_mad <- dynamic_off_diagonal_mad(upper_candidate, model_correlation)
  while (upper_mad < target_mad && upper < 4) {
    upper <- upper * 2
    upper_candidate <- candidate_at(upper)
    upper_mad <- dynamic_off_diagonal_mad(upper_candidate, model_correlation)
  }

  best_candidate <- upper_candidate
  best_difference <- abs(upper_mad - target_mad)
  for (iteration in seq_len(35L)) {
    midpoint <- (lower + upper) / 2
    candidate <- candidate_at(midpoint)
    achieved <- dynamic_off_diagonal_mad(candidate, model_correlation)
    difference <- abs(achieved - target_mad)
    if (difference < best_difference) {
      best_candidate <- candidate
      best_difference <- difference
    }
    if (achieved < target_mad) lower <- midpoint else upper <- midpoint
  }

  list(
    correlation = best_candidate,
    achieved_mad = dynamic_off_diagonal_mad(best_candidate, model_correlation)
  )
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_gaussian_sample <- function(correlation, sample_size,
                                    means, standard_deviations) {
  covariance <- correlation * outer(standard_deviations, standard_deviations)
  covariance <- (covariance + t(covariance)) / 2
  decomposition <- eigen(covariance, symmetric = TRUE)
  root <- sweep(
    decomposition$vectors,
    2L,
    sqrt(pmax(decomposition$values, 1e-8)),
    `*`
  ) %*% t(decomposition$vectors)
  sample <- matrix(
    stats::rnorm(sample_size * ncol(covariance)),
    nrow = sample_size,
    ncol = ncol(covariance)
  ) %*% root
  sample <- sweep(sample, 2L, means, `+`)
  colnames(sample) <- colnames(correlation)
  as.data.frame(sample, check.names = FALSE)
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_apply_margins <- function(simulated_data, source_data, scale) {
  if (identical(scale, "normal")) return(simulated_data)

  result <- simulated_data
  for (variable in names(result)) {
    observed <- source_data[[variable]]
    observed <- observed[!is.na(observed)]
    if (!is.numeric(observed) || length(observed) < 2L) {
      stop(
        paste("Dynamic simulation requires numeric observations for", variable),
        call. = FALSE
      )
    }

    probabilities <- (rank(result[[variable]], ties.method = "first") - 0.5) /
      nrow(result)
    category_count <- length(unique(observed))
    quantile_type <- if (identical(scale, "categorical") && category_count < 10L) {
      1L
    } else {
      8L
    }
    result[[variable]] <- as.numeric(stats::quantile(
      observed,
      probs = probabilities,
      type = quantile_type,
      names = FALSE,
      na.rm = TRUE
    ))
  }

  if (anyNA(source_data) && nrow(source_data) == nrow(result)) {
    result[is.na(source_data)] <- NA
  }
  result
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_extract_fit_measures <- function(fit) {
  measures <- suppressWarnings(tryCatch(
    lavaan::fitMeasures(fit),
    error = function(e) NULL
  ))
  if (is.null(measures)) return(NULL)

  first_finite <- function(candidates) {
    available <- intersect(candidates, names(measures))
    values <- unname(measures[available])
    values <- values[is.finite(values)]
    if (length(values)) values[[1L]] else NA_real_
  }

  result <- c(
    CFI = first_finite(c("cfi.robust", "cfi.scaled", "cfi")),
    RMSEA = first_finite(c("rmsea.robust", "rmsea.scaled", "rmsea")),
    RCI = first_finite(c(
      "rmsea.ci.upper.robust",
      "rmsea.ci.upper.scaled",
      "rmsea.ci.upper"
    ))
  )
  if (all(is.finite(result))) result else NULL
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_fit_simulated_sample <- function(model, simulated_data,
                                         estimator, ordered_variables) {
  arguments <- list(
    model = model,
    data = simulated_data,
    estimator = estimator,
    std.lv = TRUE,
    se = "none",
    warn = FALSE,
    check.gradient = FALSE,
    check.post = FALSE,
    check.vcov = FALSE,
    control = list(rel.tol = 0.001)
  )
  if (length(ordered_variables)) arguments$ordered <- ordered_variables
  if (anyNA(simulated_data) && !length(ordered_variables) &&
      estimator %in% c("ML", "MLR")) {
    arguments$missing <- "fiml"
  }

  fitted <- suppressWarnings(tryCatch(
    do.call(lavaan::cfa, arguments),
    error = function(e) NULL
  ))
  if (is.null(fitted)) return(NULL)
  converged <- tryCatch(lavaan::lavInspect(fitted, "converged"), error = function(e) FALSE)
  if (!isTRUE(converged)) return(NULL)
  dynamic_extract_fit_measures(fitted)
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_simulation_distribution <- function(model, model_correlation,
                                            observed_correlation,
                                            source_data, means,
                                            standard_deviations, estimator,
                                            ordered_variables, scale, reps,
                                            target_mad = 0) {
  successful <- 0L
  attempts <- 0L
  maximum_attempts <- max(reps * 3L, reps + 25L)
  measures <- matrix(NA_real_, nrow = reps, ncol = 3L)
  colnames(measures) <- c("CFI", "RMSEA", "RCI")
  achieved_mad <- numeric(reps)

  while (successful < reps && attempts < maximum_attempts) {
    attempts <- attempts + 1L
    simulation_correlation <- model_correlation
    current_mad <- 0
    if (target_mad > 0) {
      discrepancy <- dynamic_misspecified_correlation(
        model_correlation,
        target_mad,
        observed_correlation
      )
      simulation_correlation <- discrepancy$correlation
      current_mad <- discrepancy$achieved_mad
    }

    simulated <- dynamic_gaussian_sample(
      simulation_correlation,
      nrow(source_data),
      means,
      standard_deviations
    )
    simulated <- dynamic_apply_margins(simulated, source_data, scale)
    fit_measures <- dynamic_fit_simulated_sample(
      model,
      simulated,
      estimator,
      ordered_variables
    )
    if (is.null(fit_measures)) next

    successful <- successful + 1L
    measures[successful, ] <- fit_measures
    achieved_mad[successful] <- current_mad
  }

  minimum_success <- max(20L, ceiling(reps * 0.8))
  if (successful < minimum_success) {
    stop(
      paste0(
        "Only ", successful, " of ", reps,
        " Dynamic Fit Index simulations converged. Review the CFA model."
      ),
      call. = FALSE
    )
  }

  measures <- as.data.frame(measures[seq_len(successful), , drop = FALSE])
  list(
    measures = measures,
    achieved_mad = mean(achieved_mad[seq_len(successful)])
  )
}

# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
dynamic_cutoff_summary <- function(consistent_distribution,
                                   misspecified_distributions,
                                   mad_values) {
  consistent <- consistent_distribution$measures
  probability <- function(x, p) {
    as.numeric(stats::quantile(x, probs = p, names = FALSE, na.rm = TRUE))
  }

  consistent_cfi <- probability(consistent$CFI, 0.05)
  consistent_rmsea <- probability(consistent$RMSEA, 0.95)
  consistent_rci <- probability(consistent$RCI, 0.95)
  rows <- list(data.frame(
    MAD = 0,
    `Simulated MAD` = 0,
    CFI = consistent_cfi,
    RMSEA = consistent_rmsea,
    `RMSEA 90% CI` = consistent_rci,
    `CFI Sensitivity (%)` = NA_real_,
    `CFI Specificity (%)` = 100 * mean(consistent$CFI >= consistent_cfi),
    `RMSEA Sensitivity (%)` = NA_real_,
    `RMSEA Specificity (%)` = 100 * mean(consistent$RMSEA <= consistent_rmsea),
    check.names = FALSE
  ))

  for (index in seq_along(mad_values)) {
    misspecified <- misspecified_distributions[[index]]$measures
    cfi_cutoff <- probability(misspecified$CFI, 0.95)
    rmsea_cutoff <- probability(misspecified$RMSEA, 0.05)
    rci_cutoff <- probability(misspecified$RCI, 0.05)
    rows[[index + 1L]] <- data.frame(
      MAD = mad_values[[index]],
      `Simulated MAD` = misspecified_distributions[[index]]$achieved_mad,
      CFI = cfi_cutoff,
      RMSEA = rmsea_cutoff,
      `RMSEA 90% CI` = rci_cutoff,
      `CFI Sensitivity (%)` = 100 * mean(misspecified$CFI <= cfi_cutoff),
      `CFI Specificity (%)` = 100 * mean(consistent$CFI >= cfi_cutoff),
      `RMSEA Sensitivity (%)` = 100 * mean(misspecified$RMSEA >= rmsea_cutoff),
      `RMSEA Specificity (%)` = 100 * mean(consistent$RMSEA <= rmsea_cutoff),
      check.names = FALSE
    )
  }

  result <- do.call(rbind, rows)
  numeric_columns <- vapply(result, is.numeric, logical(1))
  result[numeric_columns] <- lapply(result[numeric_columns], round, digits = 3)
  default_labels <- c("Close", "Fair", "Mediocre")
  labels <- if (length(mad_values) == 3L &&
                isTRUE(all.equal(mad_values, c(0.038, 0.05, 0.06)))) {
    default_labels
  } else {
    paste("MAD", format(mad_values, trim = TRUE))
  }
  rownames(result) <- c("Consistent", labels)
  result
}

#' Run the FAfA Dynamic Fit Index Simulation
#'
#' @details This implementation is adapted from version 1.1.0 of the
#'   `dynamic` R package under the GNU Affero General Public License,
#'   version 3 (AGPL-3). It was rewritten for FAfA's lavaan workflow; no
#'   upstream function body was copied verbatim.
#' @references McNeish, D., & Wolf, M. G. (2023). Dynamic fit index cutoffs
#'   for confirmatory factor analysis models. *Psychological Methods, 28*(1),
#'   61-88. \doi{10.1037/met0000425}
#' @noRd
# Source: dynamic 1.1.0, Wolf and McNeish, AGPL-3. Rewritten for FAfA;
# no upstream function body was copied verbatim.
run_dynamic_fit <- function(fit, data, scale = "normal", reps = 250,
                            mad = c(0.038, 0.05, 0.06), model = NULL,
                            seed = 2026L) {
  if (!inherits(fit, "lavaan")) {
    stop("Run CFA before requesting Dynamic Fit Index cutoffs.", call. = FALSE)
  }
  if (is.null(model) || !nzchar(trimws(model))) {
    stop("The fitted lavaan model syntax is required.", call. = FALSE)
  }

  scale <- match.arg(scale, c("normal", "nonnormal", "categorical"))
  reps <- as.integer(reps)
  seed <- as.integer(seed)
  if (!is.finite(reps) || reps < 50L) {
    stop("At least 50 simulation replications are required.", call. = FALSE)
  }
  if (!is.finite(seed) || seed < 1L) {
    stop("The random seed must be a positive integer.", call. = FALSE)
  }
  mad <- parse_dynamic_mad(mad)

  implied <- lavaan::lavInspect(fit, "implied")
  if (is.null(implied$cov) && length(implied) == 1L) implied <- implied[[1L]]
  implied_covariance <- as.matrix(implied$cov)
  variable_names <- colnames(implied_covariance)
  if (is.null(variable_names) || length(variable_names) < 2L) {
    stop("The fitted model did not return a usable covariance matrix.", call. = FALSE)
  }

  source_data <- as.data.frame(data)[, variable_names, drop = FALSE]
  if (!all(vapply(source_data, is.numeric, logical(1)))) {
    stop("Dynamic Fit Index simulation requires numeric indicators.", call. = FALSE)
  }
  model_correlation <- dynamic_nearest_correlation(
    stats::cov2cor(implied_covariance)
  )
  observed_correlation <- suppressWarnings(stats::cor(
    source_data,
    use = "pairwise.complete.obs"
  ))
  if (!all(is.finite(observed_correlation))) observed_correlation <- NULL

  means <- implied$mean
  if (is.null(means)) {
    means <- vapply(source_data, mean, numeric(1), na.rm = TRUE)
  } else {
    means <- as.numeric(means[variable_names])
  }
  standard_deviations <- sqrt(pmax(diag(implied_covariance), 1e-8))
  names(means) <- names(standard_deviations) <- variable_names

  fit_options <- tryCatch(lavaan::lavInspect(fit, "options"), error = function(e) list())
  estimator <- toupper(fit_options$estimator %||% if (scale == "categorical") "WLSMV" else "ML")
  category_counts <- vapply(source_data, function(x) length(unique(x[!is.na(x)])), integer(1))
  ordered_variables <- if (identical(scale, "categorical")) {
    names(category_counts)[category_counts > 1L & category_counts < 10L]
  } else {
    character()
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  consistent <- dynamic_simulation_distribution(
    model = model,
    model_correlation = model_correlation,
    observed_correlation = observed_correlation,
    source_data = source_data,
    means = means,
    standard_deviations = standard_deviations,
    estimator = estimator,
    ordered_variables = ordered_variables,
    scale = scale,
    reps = reps,
    target_mad = 0
  )
  misspecified <- lapply(mad, function(target) {
    dynamic_simulation_distribution(
      model = model,
      model_correlation = model_correlation,
      observed_correlation = observed_correlation,
      source_data = source_data,
      means = means,
      standard_deviations = standard_deviations,
      estimator = estimator,
      ordered_variables = ordered_variables,
      scale = scale,
      reps = reps,
      target_mad = target
    )
  })

  list(
    cutoffs = dynamic_cutoff_summary(consistent, misspecified, mad),
    scale = scale,
    reps = reps,
    seed = seed,
    method = "FAfA adaptation of the Direct Discrepancy Dynamic Fit Index framework"
  )
}
