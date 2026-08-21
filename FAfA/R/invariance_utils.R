ordinal_categories <- function(values) {
  observed <- values[!is.na(values)]
  if (!length(observed)) return(character())

  if (is.factor(values)) {
    return(levels(droplevels(values)))
  }
  if (is.numeric(values)) {
    return(as.character(sort(unique(observed))))
  }
  sort(unique(as.character(observed)))
}

find_empty_ordinal_cells <- function(data, group, variables) {
  group_values <- droplevels(as.factor(data[[group]]))
  group_levels <- levels(group_values)
  findings <- list()

  for (variable in variables) {
    categories <- ordinal_categories(data[[variable]])
    if (!length(categories)) next
    values <- as.character(data[[variable]])

    for (group_level in group_levels) {
      counts <- table(factor(
        values[group_values == group_level],
        levels = categories
      ), useNA = "no")
      empty <- categories[counts == 0L]
      if (!length(empty)) next

      findings[[length(findings) + 1L]] <- data.frame(
        Variable = variable,
        Group = group_level,
        Empty_Categories = paste(empty, collapse = ", "),
        Frequencies = paste0(
          categories,
          "=",
          as.integer(counts),
          collapse = ", "
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(findings)) {
    return(data.frame(
      Variable = character(),
      Group = character(),
      Empty_Categories = character(),
      Frequencies = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, findings)
}

collapse_empty_ordinal_categories <- function(data, group, variables) {
  group_values <- droplevels(as.factor(data[[group]]))
  group_levels <- levels(group_values)
  audit <- list()

  for (variable in variables) {
    original <- data[[variable]]
    categories <- ordinal_categories(original)
    values <- as.character(original)
    observed_by_group <- lapply(group_levels, function(group_level) {
      unique(values[group_values == group_level & !is.na(values)])
    })
    shared <- Reduce(intersect, observed_by_group)
    shared <- categories[categories %in% shared]
    unshared <- categories[!categories %in% shared]

    if (!length(unshared)) next
    if (length(shared) < 2L) {
      stop(
        paste0(
          "Ordinal variable '", variable,
          "' does not have at least two response categories shared by every group. ",
          "Combine categories manually, remove the variable, or reconsider the grouping variable."
        ),
        call. = FALSE
      )
    }

    category_positions <- seq_along(categories)
    names(category_positions) <- categories
    total_counts <- table(factor(values, levels = categories), useNA = "no")

    for (category in unshared) {
      distances <- abs(
        category_positions[shared] - category_positions[[category]]
      )
      candidates <- shared[distances == min(distances)]
      if (length(candidates) > 1L) {
        candidate_counts <- total_counts[candidates]
        candidates <- candidates[candidate_counts == max(candidate_counts)]
      }
      replacement <- candidates[[1L]]
      values[values == category & !is.na(values)] <- replacement

      missing_groups <- group_levels[vapply(
        observed_by_group,
        function(group_categories) !category %in% group_categories,
        logical(1)
      )]
      audit[[length(audit) + 1L]] <- data.frame(
        Variable = variable,
        Original_Category = category,
        Recoded_Category = replacement,
        Missing_In_Groups = paste(missing_groups, collapse = ", "),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    if (is.numeric(original)) {
      numeric_lookup <- stats::setNames(
        sort(unique(original[!is.na(original)])),
        categories
      )
      data[[variable]] <- unname(numeric_lookup[values])
    } else if (is.ordered(original)) {
      data[[variable]] <- ordered(values, levels = shared)
    } else if (is.factor(original)) {
      data[[variable]] <- factor(values, levels = shared)
    } else {
      data[[variable]] <- values
    }
  }

  audit_table <- if (length(audit)) {
    do.call(rbind, audit)
  } else {
    data.frame(
      Variable = character(),
      Original_Category = character(),
      Recoded_Category = character(),
      Missing_In_Groups = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  list(data = data, audit = audit_table)
}

measurement_invariance_lrt <- function(models) {
  if (length(models) < 2L) {
    stop("At least two fitted models are required for an LRT comparison.", call. = FALSE)
  }

  model_names <- names(models)
  if (is.null(model_names) || any(!nzchar(model_names))) {
    model_names <- paste("Model", seq_along(models))
  } else {
    model_names <- tools::toTitleCase(gsub("_", " ", model_names, fixed = TRUE))
  }

  comparison <- do.call(
    lavaan::lavTestLRT,
    c(unname(models), list(model_names = model_names))
  )
  comparison <- as.data.frame(comparison, check.names = FALSE)
  comparison <- data.frame(
    Model = rownames(comparison),
    comparison,
    row.names = NULL,
    check.names = FALSE
  )
  comparison
}
