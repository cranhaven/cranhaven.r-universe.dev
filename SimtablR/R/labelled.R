# Labelled Data Integration
#
# Interoperability helpers converting haven and labelled data frame columns
# with value labels into standard R factors while preserving variable labels.

#########
# HAVEN AND LABELLED COERCION
# Data frame sweep and factor conversion honoring SPSS/Stata value mappings.

#' Coerce labelled columns in data frame into standard factors
#' @keywords internal
#' @noRd
.as_simtab_data <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  changed <- FALSE
  kept_numeric <- character(0)
  out <- data
  for (nm in names(out)) {
    converted <- .as_simtab_factor(out[[nm]])
    if (isTRUE(attr(converted, "simtab_partially_labelled"))) {
      attr(converted, "simtab_partially_labelled") <- NULL
      kept_numeric <- c(kept_numeric, nm)
    }
    if (!identical(converted, out[[nm]])) {
      out[[nm]] <- converted
      changed <- TRUE
    }
  }
  if (length(kept_numeric) > 0) {
    message(sprintf(
      paste0(
        "Kept partially labelled numeric column(s) numeric: %s. Labelled codes ",
        "(e.g. a missing-value code) remain numeric values; recode them to NA or ",
        "convert with haven::as_factor() if they are categories."
      ),
      paste(kept_numeric, collapse = ", ")
    ))
  }

  if (isTRUE(changed)) out else data
}

#' Convert single labelled vector into factor using attached value labels
#' @keywords internal
#' @noRd
.as_simtab_factor <- function(x) {
  if (!inherits(x, c("haven_labelled", "labelled", "labelled_spss"))) {
    return(x)
  }

  value_labels <- attr(x, "labels", exact = TRUE)
  if (is.null(value_labels) || is.null(names(value_labels)) ||
      any(!nzchar(names(value_labels)))) {
    return(x)
  }

  var_label <- attr(x, "label", exact = TRUE)
  raw <- unclass(x)

  # A numeric column with only some values labelled (e.g. ages with
  # 999 = "Unknown") is a measurement with annotated codes, not a set of
  # categories: keep it numeric rather than making every value a level.
  observed <- raw[!is.na(raw)]
  if (is.numeric(raw) && length(observed) > 0 && !all(observed %in% unname(value_labels))) {
    out <- as.vector(raw)
    if (!is.null(var_label)) {
      attr(out, "label") <- var_label
    }
    attr(out, "simtab_partially_labelled") <- TRUE
    return(out)
  }

  raw_key <- as.character(raw)
  label_key <- stats::setNames(names(value_labels), as.character(unname(value_labels)))

  values <- raw_key
  matched <- raw_key %in% names(label_key)
  values[matched] <- unname(label_key[raw_key[matched]])
  values[is.na(raw)] <- NA_character_

  ordered_labels <- names(value_labels)[order(unname(value_labels), na.last = TRUE)]
  extras <- setdiff(unique(values[!is.na(values)]), ordered_labels)
  out <- factor(values, levels = c(ordered_labels, extras))
  if (!is.null(var_label)) {
    attr(out, "label") <- var_label
  }
  out
}
