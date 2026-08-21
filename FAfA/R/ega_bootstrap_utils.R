# Bootstrap EGA results are returned as nested lists by EGAnet. These helpers
# keep the Shiny module focused on the analysis flow and give every table a
# stable structure for display, download, and reporting.

prepare_bootega_data <- function(data) {
  data <- as.data.frame(data, check.names = FALSE)
  numeric_columns <- vapply(data, is.numeric, logical(1))
  if (!all(numeric_columns)) {
    stop("Bootstrap EGA requires numeric variables.", call. = FALSE)
  }

  data[] <- lapply(data, function(column) {
    values <- as.numeric(column)
    storage.mode(values) <- "double"
    values
  })
  data
}

validate_bootega_settings <- function(iter, ncores, seed) {
  iter <- suppressWarnings(as.integer(iter))
  ncores <- suppressWarnings(as.integer(ncores))
  seed <- suppressWarnings(as.numeric(seed))

  if (length(iter) != 1L || !is.finite(iter) || iter < 20L) {
    stop("Bootstrap EGA requires at least 20 bootstrap samples.", call. = FALSE)
  }
  if (length(ncores) != 1L || !is.finite(ncores) || ncores < 1L) {
    stop("The number of processor cores must be a positive integer.", call. = FALSE)
  }
  if (length(seed) != 1L || !is.finite(seed) || seed < 1 || seed != floor(seed)) {
    stop("The Bootstrap EGA seed must be a positive integer.", call. = FALSE)
  }

  list(iter = iter, ncores = ncores, seed = seed)
}

is_lavcor_non_pd_error <- function(error) {
  message <- conditionMessage(error)
  mentions_lavaan <- grepl(
    "lavaan|lavCor|lav_samp_icov",
    message,
    ignore.case = TRUE
  )
  mentions_non_pd <- grepl(
    "positive[- ]definite",
    message,
    ignore.case = TRUE
  )

  mentions_lavaan && mentions_non_pd
}

run_bootega_with_correlation_fallback <- function(
    data,
    corr,
    bootega_fun = EGAnet::bootEGA,
    ...
) {
  arguments <- list(data = data, corr = corr, ...)

  tryCatch(
    list(
      result = do.call(bootega_fun, arguments),
      correlation = corr,
      used_fallback = FALSE
    ),
    error = function(error) {
      can_retry <- identical(corr, "cor_auto") && is_lavcor_non_pd_error(error)
      if (!can_retry) stop(error)

      arguments$corr <- "pearson"
      fallback_result <- tryCatch(
        do.call(bootega_fun, arguments),
        error = function(fallback_error) {
          stop(
            paste0(
              "Automatic ordinal correlation could not be estimated, and the ",
              "Pearson fallback also failed: ",
              conditionMessage(fallback_error)
            ),
            call. = FALSE
          )
        }
      )

      list(
        result = fallback_result,
        correlation = "pearson",
        used_fallback = TRUE,
        original_error = conditionMessage(error)
      )
    }
  )
}

bootega_dimension_stability_table <- function(stability_object) {
  dimension_values <- stability_object$dimension.stability
  if (is.null(dimension_values)) return(NULL)

  structural <- dimension_values$structural.consistency
  average_item <- dimension_values$average.item.stability
  if (is.null(structural) || is.null(average_item)) return(NULL)

  dimensions <- union(names(structural), names(average_item))
  if (!length(dimensions)) {
    dimensions <- as.character(seq_len(max(length(structural), length(average_item))))
    names(structural) <- names(average_item) <- dimensions
  }

  data.frame(
    Dimension = dimensions,
    Structural_Consistency = as.numeric(structural[dimensions]),
    Average_Item_Stability = as.numeric(average_item[dimensions]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

bootega_item_stability_table <- function(stability_object) {
  item_object <- stability_object$item.stability
  if (is.null(item_object)) return(NULL)

  item_values <- item_object$item.stability$empirical.dimensions
  membership <- item_object$membership$structure
  if (is.null(membership)) membership <- item_object$membership$empirical
  if (is.null(item_values) || is.null(membership)) return(NULL)

  item_names <- names(item_values)
  if (is.null(item_names) || !length(item_names)) item_names <- names(membership)
  if (is.null(item_names) || !length(item_names)) {
    item_names <- paste0("Item_", seq_along(item_values))
    names(item_values) <- names(membership) <- item_names
  }

  data.frame(
    Item = item_names,
    Empirical_Dimension = as.numeric(membership[item_names]),
    Item_Stability = as.numeric(item_values[item_names]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

prepare_bootega_results <- function(bootega_object, stability_object) {
  summary_table <- bootega_object$summary.table
  frequency_table <- bootega_object$frequency
  if (!is.null(summary_table)) {
    summary_table <- as.data.frame(summary_table, check.names = FALSE)
  }
  if (!is.null(frequency_table)) {
    frequency_table <- as.data.frame(frequency_table, check.names = FALSE)
  }

  item_plot <- stability_object$item.stability$plot

  list(
    summary_table = summary_table,
    frequency_table = frequency_table,
    dimension_table = bootega_dimension_stability_table(stability_object),
    item_table = bootega_item_stability_table(stability_object),
    item_plot = item_plot
  )
}
