##############################################################

# LAPOP tagged missing values from numeric sentinel codes      #

##############################################################

#' Convert LAPOP missing-value codes to haven tagged NAs
#'
#' Converts numeric sentinel codes used in LAPOP/Stata workflows to
#' [haven::tagged_na()] values. By default, `888888`, `988888`, and `999999`
#' are converted to `NA(a)`, `NA(b)`, and `NA(c)`, respectively.
#'
#' Unlike ordinary `NA` values or variable-level attributes, tagged NAs retain
#' the missing-value reason for each observation and can be written to Stata as
#' extended missing values by haven.
#'
#' @param data A data frame.
#' @param na_values Numeric vector of sentinel values to convert.
#' @param na_tags Character vector of single-letter missing-value tags. Must be
#'   the same length as `na_values`.
#' @param vars Optional character vector of variable names to process. Defaults
#'   to all variables.
#' @param preserve_labels Logical. If `TRUE`, labels attached to converted
#'   sentinel values are reassigned to the corresponding tagged NAs.
#' @param print Logical. If `TRUE`, prints a compact missing-value summary for
#'   each processed variable. Defaults to `FALSE`.
#'
#' @return `data` with selected numeric variables converted from sentinel
#'   missing-value codes to haven tagged NAs.
#'
#' @examples
#' x <- haven::labelled(
#'   c(1, 888888, 988888, 999999),
#'   labels = c(Yes = 1, DK = 888888, NR = 988888, NotApplicable = 999999)
#' )
#' dat <- data.frame(x = x)
#' out <- lpr_na_attributes(dat)
#' haven::is_tagged_na(out$x)
#'
#' @export
lpr_na_attributes <- function(data,
                              na_values = c(888888, 988888, 999999),
                              na_tags = c("a", "b", "c"),
                              vars = NULL,
                              preserve_labels = TRUE,
                              print = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.numeric(na_values) || anyNA(na_values)) {
    stop("`na_values` must be a numeric vector with no missing values.",
         call. = FALSE)
  }

  if (!is.character(na_tags) || anyNA(na_tags)) {
    stop("`na_tags` must be a character vector with no missing values.",
         call. = FALSE)
  }

  if (length(na_values) != length(na_tags)) {
    stop("`na_values` and `na_tags` must have the same length.",
         call. = FALSE)
  }

  if (any(nchar(na_tags) != 1L)) {
    stop("Each value in `na_tags` must contain exactly one character.",
         call. = FALSE)
  }

  if (anyDuplicated(na_values)) {
    stop("`na_values` must not contain duplicates.", call. = FALSE)
  }

  if (anyDuplicated(na_tags)) {
    stop("`na_tags` must not contain duplicates.", call. = FALSE)
  }

  if (is.null(vars)) {
    vars <- names(data)
  } else {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars)) {
      stop("Variables not found in `data`: ",
           paste(missing_vars, collapse = ", "),
           call. = FALSE)
    }
  }

  for (var in vars) {
    data[[var]] <- lpr_na_attributes_col(
      data[[var]],
      na_values = na_values,
      na_tags = na_tags,
      preserve_labels = preserve_labels
    )

    if (isTRUE(print)) {
      print_lpr_na_attributes(data, var, na_tags = na_tags)
    }
  }

  data
}

lpr_na_attributes_col <- function(x,
                                  na_values,
                                  na_tags,
                                  preserve_labels) {
  if (!is.numeric(x)) {
    return(x)
  }

  out <- x

  for (i in seq_along(na_values)) {
    out[!is.na(out) & out == na_values[i]] <- haven::tagged_na(na_tags[i])
  }

  if (preserve_labels && inherits(out, "haven_labelled")) {
    labels <- attr(out, "labels", exact = TRUE)

    if (!is.null(labels) && is.numeric(labels)) {
      for (i in seq_along(na_values)) {
        labels[!is.na(labels) & labels == na_values[i]] <-
          haven::tagged_na(na_tags[i])
      }

      attr(out, "labels") <- labels
    }
  }

  out
}

print_lpr_na_attributes <- function(data, variable_name, na_tags = c("a", "b", "c")) {
  variable <- data[[variable_name]]

  cat("Variable:", variable_name, "\n")
  cat("Summary:\n")
  print(summary(variable))

  cat("\nTagged NAs:\n")
  if (is.numeric(variable)) {
    for (tag in na_tags) {
      n_tagged <- sum(haven::is_tagged_na(variable) &
                        haven::na_tag(variable) == tag,
                      na.rm = TRUE)
      cat(sprintf("NA(%s): %s\n", tag, n_tagged))
    }
  } else {
    cat("Not a numeric variable; no tagged NAs added.\n")
  }

  if (inherits(variable, "haven_labelled")) {
    cat("\nValue Labels:\n")
    print(attr(variable, "labels", exact = TRUE))
  }

  cat("\n")
}
