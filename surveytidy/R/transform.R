# R/transform.R
#
# Vector-level transformation functions for survey variables.
# These are NOT dplyr verbs - they operate on plain R vectors and integrate
# with mutate.survey_base() via the surveytidy_recode attribute protocol.
#
# Functions defined here:
#   .strip_first_word()         - remove first word from multi-word label string
#   make_factor()               - convert labelled/numeric/character to R factor
#   make_dicho()                - collapse multi-level factor to 2 levels
#   make_binary()               - convert dichotomous variable to 0/1 integer
#   make_rev()                  - reverse numeric scale values
#   make_flip()                 - flip semantic valence of a variable
#
# Shared helpers (in R/utils.R):
#   .validate_transform_args()  - validate .label/.description for transform fns
#   .set_recode_attrs()         - set label, labels, surveytidy_recode attrs

#  internal helpers (used only in transform.R)

# Remove first whitespace-delimited word from a label string.
# Single-word labels are returned unchanged.
# The first character of the result is uppercased.
.strip_first_word <- function(label) {
  stripped <- sub("^\\S+\\s+", "", label)
  if (nchar(stripped) == 0L || stripped == label) {
    return(label)
  }
  # Capitalize first character
  paste0(
    toupper(substr(stripped, 1L, 1L)),
    substr(stripped, 2L, nchar(stripped))
  )
}

#  make_factor()

#' Convert a vector to a factor using value labels
#'
#' @description
#' `make_factor()` converts a labelled numeric, factor, or character vector to
#' an R factor. For labelled numeric input (e.g., from
#' \pkg{haven} or with a `"labels"` attribute), factor levels are derived from
#' the value labels. For factor input, levels are preserved. For character
#' input, levels are set alphabetically.
#'
#' When called inside [mutate()], metadata is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param x Vector to convert. Must be a labelled numeric, plain numeric with
#'   a `"labels"` attribute, R factor, or character vector.
#' @param ordered `logical(1)`. If `TRUE`, returns an ordered factor.
#' @param drop_levels `logical(1)`. If `TRUE` (the default), removes levels
#'   with no observed values in `x`.
#' @param force `logical(1)`. If `TRUE`, coerce a numeric `x` without value
#'   labels via `as.factor()`, issuing a
#'   `surveytidy_warning_make_factor_forced` warning. If `FALSE` (the
#'   default), error instead.
#' @param na.rm `logical(1)`. If `TRUE`, values in `attr(x, "na_values")` and
#'   `attr(x, "na_range")` are converted to `NA` before building factor
#'   levels, so they do not produce factor levels. Ignored for factor and
#'   character input.
#' @param .label `character(1)` or `NULL`. Variable label override. If `NULL`,
#'   inherits from `attr(x, "label")`; if that is also `NULL`, falls back to
#'   the column name.
#' @param .description `character(1)` or `NULL`. Transformation description
#'   stored in `surveytidy_recode`.
#'
#' @return An R factor (ordered if `ordered = TRUE`).
#'
#' @examples
#' # attach value labels to a numeric vector and convert to a factor
#' x <- c(1, 2, 1, 2)
#' attr(x, "labels") <- c("Yes" = 1, "No" = 2)
#' make_factor(x)
#'
#' @family transformation
#' @export
make_factor <- function(
  x,
  ordered = FALSE,
  drop_levels = TRUE,
  force = FALSE,
  na.rm = FALSE,
  .label = NULL,
  .description = NULL
) {
  # Capture var_name before x is evaluated or modified
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )

  # Validate scalar logical args
  for (arg_nm in c("ordered", "drop_levels", "force", "na.rm")) {
    val <- switch(
      arg_nm,
      ordered = ordered,
      drop_levels = drop_levels,
      force = force,
      na.rm = na.rm
    )
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      cli::cli_abort(
        c(
          "x" = "{.arg {arg_nm}} must be a single {.cls logical} value.",
          "i" = "Got {.cls {class(val)}} of length {length(val)}."
        ),
        class = "surveytidy_error_make_factor_bad_arg"
      )
    }
  }

  # Validate .label and .description
  .validate_transform_args(
    .label,
    .description,
    "surveytidy_error_make_factor_bad_arg"
  )

  # Effective label
  effective_label <- if (!is.null(.label)) {
    .label
  } else {
    attr(x, "label", exact = TRUE) %||% var_name
  }

  # Input dispatch
  if (is.factor(x)) {
    # Factor pass-through: apply ordered and drop_levels
    result <- factor(x, levels = levels(x), ordered = ordered)
    if (isTRUE(drop_levels)) {
      result <- droplevels(result)
    }
    return(.set_recode_attrs(
      result,
      effective_label,
      NULL,
      "make_factor",
      var_name,
      .description
    ))
  }

  if (is.character(x)) {
    result <- factor(x)
    if (isTRUE(ordered)) {
      result <- factor(result, levels = levels(result), ordered = TRUE)
    }
    if (isTRUE(drop_levels)) {
      result <- droplevels(result)
    }
    return(.set_recode_attrs(
      result,
      effective_label,
      NULL,
      "make_factor",
      var_name,
      .description
    ))
  }

  if (typeof(x) %in% c("double", "integer")) {
    labels_attr <- attr(x, "labels", exact = TRUE)

    if (is.null(labels_attr)) {
      if (!isTRUE(force)) {
        cli::cli_abort(
          c(
            "x" = "{.arg x} has no value labels.",
            "i" = paste0(
              "Numeric input requires a {.code labels} attribute to ",
              "determine factor levels."
            ),
            "v" = paste0(
              "Set {.arg force = TRUE} to coerce via {.fn as.factor}, ",
              "or attach labels first."
            )
          ),
          class = "surveytidy_error_make_factor_no_labels"
        )
      }
      # force = TRUE: warn and coerce via as.factor
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.arg x} has no value labels - coercing to factor via ",
            "{.fn as.factor}."
          ),
          "i" = "Set {.arg force = FALSE} to error instead."
        ),
        class = "surveytidy_warning_make_factor_forced"
      )
      result <- as.factor(x)
      if (isTRUE(ordered)) {
        result <- factor(result, levels = levels(result), ordered = TRUE)
      }
      if (isTRUE(drop_levels)) {
        result <- droplevels(result)
      }
      return(.set_recode_attrs(
        result,
        effective_label,
        NULL,
        "make_factor",
        var_name,
        .description
      ))
    }

    # Apply na.rm: convert na_values and na_range to NA
    x_clean <- x
    if (isTRUE(na.rm)) {
      na_values <- attr(x, "na_values")
      na_range <- attr(x, "na_range")
      if (!is.null(na_values)) {
        x_clean[x_clean %in% na_values] <- NA
      }
      if (!is.null(na_range) && length(na_range) == 2L) {
        x_clean[
          !is.na(x_clean) & x_clean >= na_range[1] & x_clean <= na_range[2]
        ] <- NA
      }
    }

    # Check label completeness: every observed non-NA value must have a label
    observed <- unique(x_clean[!is.na(x_clean)])
    unlabelled_vals <- observed[!observed %in% unname(labels_attr)]
    if (length(unlabelled_vals) > 0L) {
      # Special missing values (na_values/na_range) without labels are allowed
      # when na.rm = FALSE. Exclude them from the error.
      na_values_attr <- attr(x, "na_values")
      na_range_attr <- attr(x, "na_range")
      is_special_missing <- function(v) {
        if (!is.null(na_values_attr) && v %in% na_values_attr) {
          return(TRUE)
        }
        if (
          !is.null(na_range_attr) &&
            length(na_range_attr) == 2L &&
            v >= na_range_attr[1] &&
            v <= na_range_attr[2]
        ) {
          return(TRUE)
        }
        FALSE
      }
      # Filter out special missing values that are tolerated without labels
      if (!isTRUE(na.rm)) {
        unlabelled_non_special <- unlabelled_vals[
          !vapply(unlabelled_vals, is_special_missing, logical(1L))
        ]
      } else {
        unlabelled_non_special <- unlabelled_vals
      }
      if (length(unlabelled_non_special) > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "{.arg x} contains {length(unlabelled_non_special)} ",
              "value{?s} with no label: {.val {unlabelled_non_special}}."
            ),
            "i" = "Every observed value must have a label entry.",
            "v" = paste0(
              "Add the missing labels or use {.fn na_if} to convert ",
              "those values to {.code NA} first."
            )
          ),
          class = "surveytidy_error_make_factor_incomplete_labels"
        )
      }
    }

    # Build factor: levels ordered by numeric value from labels_attr
    # For na.rm = TRUE, the labels used should not include na_values/na_range
    labels_for_levels <- if (isTRUE(na.rm)) {
      na_values2 <- attr(x, "na_values")
      na_range2 <- attr(x, "na_range")
      keep <- rep(TRUE, length(labels_attr))
      if (!is.null(na_values2)) {
        keep <- keep & !(unname(labels_attr) %in% na_values2)
      }
      if (!is.null(na_range2) && length(na_range2) == 2L) {
        keep <- keep &
          !(unname(labels_attr) >= na_range2[1] &
            unname(labels_attr) <= na_range2[2])
      }
      labels_attr[keep]
    } else {
      labels_attr
    }
    # Sort by numeric value ascending
    labels_for_levels <- labels_for_levels[order(unname(labels_for_levels))]
    lvl_names <- names(labels_for_levels)

    # Map observed values to level names
    level_map <- stats::setNames(
      names(labels_attr),
      as.character(unname(labels_attr))
    )
    factor_vals <- level_map[as.character(x_clean)]

    result <- factor(factor_vals, levels = lvl_names, ordered = ordered)

    if (isTRUE(drop_levels)) {
      result <- droplevels(result)
    }

    return(.set_recode_attrs(
      result,
      effective_label,
      NULL,
      "make_factor",
      var_name,
      .description
    ))
  }

  # Unsupported type
  cli::cli_abort(
    c(
      "x" = paste0(
        "{.arg x} must be a labelled numeric, factor, or character vector."
      ),
      "i" = "Got class {.cls {class(x)}}."
    ),
    class = "surveytidy_error_make_factor_unsupported_type"
  )
}

#  make_dicho()

#' Collapse a multi-level factor to two levels
#'
#' @description
#' `make_dicho()` converts a variable to a two-level factor by stripping the
#' first qualifier word from each level label and grouping the resulting stems.
#' For example, a 4-level Likert scale with labels
#' `c("Strongly agree", "Agree", "Disagree", "Strongly disagree")` collapses
#' to `c("Agree", "Disagree")` by removing the qualifier "Strongly".
#'
#' When called inside [mutate()], metadata is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param x Vector. Same types as [make_factor()].
#' @param flip_levels `logical(1)`. If `TRUE`, reverse the order of the two
#'   output levels.
#' @param .exclude `character` or `NULL`. Level name(s) to set to `NA` before
#'   collapsing. Intended for middle categories and "don't know"/"refused".
#' @param .label `character(1)` or `NULL`. Variable label override. Falls back
#'   to `attr(x, "label")` then the column name.
#' @param .description `character(1)` or `NULL`. Transformation description.
#'
#' @return A 2-level R factor.
#'
#' @examples
#' # build a 4-level Likert factor
#' x <- factor(
#'   c(
#'     "Always agree",
#'     "Sometimes agree",
#'     "Sometimes disagree",
#'     "Always disagree"
#'   ),
#'   levels = c(
#'     "Always agree",
#'     "Sometimes agree",
#'     "Sometimes disagree",
#'     "Always disagree"
#'   )
#' )
#'
#' # collapse to 2 levels by stripping the qualifier word
#' make_dicho(x)
#'
#' @family transformation
#' @export
make_dicho <- function(
  x,
  flip_levels = FALSE,
  .exclude = NULL,
  .label = NULL,
  .description = NULL
) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )

  # Validate args
  if (
    !is.logical(flip_levels) || length(flip_levels) != 1L || is.na(flip_levels)
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg flip_levels} must be a single {.cls logical} value.",
        "i" = "Got {.cls {class(flip_levels)}} of length {length(flip_levels)}."
      ),
      class = "surveytidy_error_transform_bad_arg"
    )
  }
  .validate_transform_args(
    .label,
    .description,
    "surveytidy_error_transform_bad_arg"
  )

  # Effective label
  effective_label <- if (!is.null(.label)) {
    .label
  } else {
    attr(x, "label", exact = TRUE) %||% var_name
  }

  # Input normalization: convert to factor if not already
  x_factor <- if (is.factor(x)) {
    x
  } else {
    make_factor(x)
  }

  # .exclude application: set matching levels to NA and remove from level set
  if (!is.null(.exclude)) {
    unknown <- .exclude[!.exclude %in% levels(x_factor)]
    if (length(unknown) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{length(unknown)} level{?s} in {.arg .exclude} not found in ",
            "{.arg x}: {.val {unknown}}."
          ),
          "i" = "Spelling must match exactly. Current levels: {.val {levels(x_factor)}}."
        ),
        class = "surveytidy_warning_make_dicho_unknown_exclude"
      )
    }
    known_exclude <- .exclude[.exclude %in% levels(x_factor)]
    if (length(known_exclude) > 0L) {
      x_factor[x_factor %in% known_exclude] <- NA
      x_factor <- droplevels(x_factor)
    }
  }

  # Check minimum levels
  n_remaining <- nlevels(x_factor)
  if (n_remaining < 2L) {
    cli::cli_abort(
      c(
        "x" = "Fewer than 2 levels remain after applying {.arg .exclude}.",
        "i" = paste0(
          "{length(.exclude)} level{?s} excluded; ",
          "{n_remaining} level{?s} remain."
        ),
        "v" = paste0(
          "Remove entries from {.arg .exclude} or check that ",
          "{.arg x} has sufficient levels."
        )
      ),
      class = "surveytidy_error_make_dicho_too_few_levels"
    )
  }

  # First-word collapse via .strip_first_word()
  remaining_levels <- levels(x_factor)
  stems <- vapply(remaining_levels, .strip_first_word, character(1L))

  # Unique stems case-insensitive
  stems_lower <- tolower(stems)
  unique_stems_lower <- unique(stems_lower)
  n_stems <- length(unique_stems_lower)

  if (n_stems != 2L) {
    # Get the actual title-cased stems for the error message
    stem_display <- unique(stems)
    cli::cli_abort(
      c(
        "x" = paste0(
          "First-word stripping produced {n_stems} stem{?s}, not 2: ",
          "{.val {stem_display}}."
        ),
        "i" = paste0(
          "Automatic collapse requires exactly 2 unique stems after ",
          "removing first-word prefixes."
        ),
        "v" = paste0(
          "Use {.arg .exclude} to remove middle categories, or manually ",
          "recode to 2 groups before calling {.fn make_dicho}."
        )
      ),
      class = "surveytidy_error_make_dicho_collapse_ambiguous"
    )
  }

  # Map each original level to its stem (title-cased)
  # Output levels: 2 stems in order they first appear among remaining_levels
  # (preserving original label order, not alphabetical)
  ordered_unique_lower <- unique(stems_lower)
  stem_canonical <- character(2L)
  for (i in seq_along(ordered_unique_lower)) {
    idx <- which(stems_lower == ordered_unique_lower[i])[1L]
    stem_canonical[i] <- stems[idx]
  }

  # Build level mapping: old level  canonical stem
  level_map <- stats::setNames(stems, remaining_levels)
  # Map canonical stems to avoid case issues
  for (i in seq_along(remaining_levels)) {
    level_map[remaining_levels[i]] <- stem_canonical[
      match(stems_lower[i], ordered_unique_lower)
    ]
  }

  # Apply the mapping
  new_values <- level_map[as.character(x_factor)]
  result <- factor(new_values, levels = stem_canonical)

  if (isTRUE(flip_levels)) {
    result <- factor(result, levels = rev(levels(result)))
  }

  .set_recode_attrs(
    result,
    effective_label,
    NULL,
    "make_dicho",
    var_name,
    .description
  )
}

#  make_binary()

#' Convert a dichotomous variable to a numeric 0/1 indicator
#'
#' @description
#' `make_binary()` converts a variable that can be collapsed to exactly two
#' levels (via [make_dicho()]) into an integer vector of 0s and 1s. By default,
#' the first level maps to `1L` and the second to `0L`. Use
#' `flip_values = TRUE` to reverse the mapping.
#'
#' When called inside [mutate()], metadata is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param x Vector. Same types as [make_factor()]. Must yield exactly 2 levels
#'   (after `.exclude`) or error.
#' @param flip_values `logical(1)`. If `TRUE`, map the first level to `0L` and
#'   the second to `1L`. Default maps first level to `1L`.
#' @param .exclude `character` or `NULL`. Passed directly to [make_dicho()].
#'   Level names to set to `NA` before encoding.
#' @param .label `character(1)` or `NULL`. Variable label override. Falls back
#'   to `attr(x, "label")` then the column name.
#' @param .description `character(1)` or `NULL`. Transformation description.
#'
#' @return An integer vector with values `0L`, `1L`, or `NA_integer_`.
#'
#' @examples
#' # build a 2-level factor with one NA
#' x <- factor(
#'   c("Agree", "Disagree", "Agree", NA),
#'   levels = c("Agree", "Disagree")
#' )
#'
#' # encode as a 0/1 integer indicator
#' make_binary(x)
#'
#' @family transformation
#' @export
make_binary <- function(
  x,
  flip_values = FALSE,
  .exclude = NULL,
  .label = NULL,
  .description = NULL
) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )

  # Validate args
  if (
    !is.logical(flip_values) || length(flip_values) != 1L || is.na(flip_values)
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg flip_values} must be a single {.cls logical} value.",
        "i" = "Got {.cls {class(flip_values)}} of length {length(flip_values)}."
      ),
      class = "surveytidy_error_transform_bad_arg"
    )
  }
  .validate_transform_args(
    .label,
    .description,
    "surveytidy_error_transform_bad_arg"
  )

  # Effective label
  effective_label <- if (!is.null(.label)) {
    .label
  } else {
    attr(x, "label", exact = TRUE) %||% var_name
  }

  # Call make_dicho() - errors propagate unchanged
  dicho <- make_dicho(x, .exclude = .exclude)

  # Encode
  if (isTRUE(flip_values)) {
    result <- as.integer(dicho) - 1L
  } else {
    result <- 2L - as.integer(dicho)
  }

  # Build labels attr from dicho level names
  lvl_names <- levels(dicho)
  if (isTRUE(flip_values)) {
    labels_vec <- stats::setNames(c(0L, 1L), lvl_names)
  } else {
    labels_vec <- stats::setNames(c(1L, 0L), lvl_names)
  }

  .set_recode_attrs(
    result,
    effective_label,
    labels_vec,
    "make_binary",
    var_name,
    .description
  )
}

#  make_rev()

#' Reverse the numeric values of a scale variable
#'
#' @description
#' `make_rev()` reverses the direction of a numeric scale variable using the
#' formula `min(x) + max(x) - x`. This preserves the scale range: a 1-4 scale
#' reversed stays a 1-4 scale; a 2-5 scale reversed stays a 2-5 scale.
#'
#' Value labels are remapped: each label's numeric value becomes `min + max -
#' old_value`, so the label string stays tied to its original concept at its
#' new position.
#'
#' When called inside [mutate()], metadata is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param x A numeric vector. `typeof(x)` must be `"double"` or `"integer"`.
#' @param .label `character(1)` or `NULL`. Variable label override. If `NULL`,
#'   inherits from `attr(x, "label")`; if that is also `NULL`, falls back to
#'   the column name.
#' @param .description `character(1)` or `NULL`. Transformation description.
#'
#' @return A numeric vector (same `typeof()` as `x`) with reversed values.
#'
#' @examples
#' # reverse a 1-4 numeric scale: 1 swaps with 4, 2 swaps with 3
#' x <- c(1, 2, 3, 4)
#' make_rev(x)
#'
#' @family transformation
#' @export
make_rev <- function(x, .label = NULL, .description = NULL) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )

  .validate_transform_args(
    .label,
    .description,
    "surveytidy_error_transform_bad_arg"
  )

  # Type check: factors have typeof "integer" so check class first
  if (
    is.factor(x) || is.character(x) || !typeof(x) %in% c("double", "integer")
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a numeric vector (double or integer).",
        "i" = "Got type {.val {typeof(x)}} with class {.cls {class(x)}}.",
        "v" = "Use {.fn make_factor} for factor or character inputs."
      ),
      class = "surveytidy_error_make_rev_not_numeric"
    )
  }

  # Effective label
  effective_label <- if (!is.null(.label)) {
    .label
  } else {
    attr(x, "label", exact = TRUE) %||% var_name
  }

  labels_attr <- attr(x, "labels", exact = TRUE)

  # All-NA short-circuit
  if (all(is.na(x))) {
    cli::cli_warn(
      c(
        "!" = "{.arg x} contains only {.code NA} values - reversal is a no-op."
      ),
      class = "surveytidy_warning_make_rev_all_na"
    )
    # Preserve labels unchanged
    return(.set_recode_attrs(
      x,
      effective_label,
      labels_attr,
      "make_rev",
      var_name,
      .description
    ))
  }

  # Reversal formula
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  m <- mn + mx
  result <- m - x

  # Label remapping
  new_labels <- if (!is.null(labels_attr)) {
    new_vals <- m - unname(labels_attr)
    remapped <- stats::setNames(new_vals, names(labels_attr))
    # Sort ascending by new value
    remapped[order(unname(remapped))]
  } else {
    NULL
  }

  .set_recode_attrs(
    result,
    effective_label,
    new_labels,
    "make_rev",
    var_name,
    .description
  )
}

#  make_flip()

#' Flip the semantic valence of a variable
#'
#' @description
#' `make_flip()` reverses the label string associations of a numeric variable
#' without changing its values. This is used to flip the polarity of a survey
#' item for composite scoring - for example, converting "I like the color blue"
#' to "I dislike the color blue" without changing the underlying numeric codes.
#'
#' Unlike [make_rev()], which changes numeric values and keeps label strings in
#' place, `make_flip()` keeps values unchanged and reverses which label strings
#' are attached to which values.
#'
#' A new variable label is **required** because flipping always changes the
#' semantic meaning of the variable.
#'
#' When called inside [mutate()], metadata is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param x A numeric vector. `typeof(x)` must be `"double"` or `"integer"`.
#' @param label `character(1)`. **Required.** New variable label describing the
#'   flipped semantic meaning.
#' @param .description `character(1)` or `NULL`. Transformation description.
#'
#' @return A numeric vector (same `typeof()` as `x`). Values are unchanged.
#'
#' @examples
#' # build a labelled numeric vector with a 4-level Likert scale
#' x <- c(1, 2, 3, 4)
#' attr(x, "labels") <- c(
#'   "Strongly agree" = 1,
#'   "Agree" = 2,
#'   "Disagree" = 3,
#'   "Strongly disagree" = 4
#' )
#'
#' # flip the semantic meaning while keeping numeric values unchanged
#' make_flip(x, "I dislike the color blue")
#'
#' @family transformation
#' @export
make_flip <- function(x, label, .description = NULL) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )

  # label check: must be supplied and character(1)
  if (missing(label) || is.null(label) || !rlang::is_string(label)) {
    cli::cli_abort(
      c(
        "x" = "{.arg label} is required.",
        "i" = paste0(
          "{.fn make_flip} reverses the semantic meaning of a variable \u2014 ",
          "a new variable label is needed to document the change."
        ),
        "v" = paste0(
          "Supply a string describing the flipped meaning, e.g. ",
          "{.val \"I dislike the color blue\"}."
        )
      ),
      class = "surveytidy_error_make_flip_missing_label"
    )
  }

  .validate_transform_args(
    NULL,
    .description,
    "surveytidy_error_transform_bad_arg"
  )

  # Type check: factors have typeof "integer" so check class first
  if (
    is.factor(x) || is.character(x) || !typeof(x) %in% c("double", "integer")
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a numeric vector (double or integer).",
        "i" = "Got type {.val {typeof(x)}} with class {.cls {class(x)}}.",
        "v" = "Use {.fn make_factor} for factor or character inputs."
      ),
      class = "surveytidy_error_make_flip_not_numeric"
    )
  }

  labels_attr <- attr(x, "labels", exact = TRUE)

  # Reverse label string associations, keeping values unchanged
  new_labels <- if (!is.null(labels_attr)) {
    stats::setNames(unname(labels_attr), rev(names(labels_attr)))
  } else {
    NULL
  }

  .set_recode_attrs(x, label, new_labels, "make_flip", var_name, .description)
}
