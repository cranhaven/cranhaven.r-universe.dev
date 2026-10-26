# ==============================================================================
# Matching Utilities - Shared helpers for matching layer
# ==============================================================================

#' Validate matching inputs
#'
#' @return Invisibly returns TRUE if validation passes; otherwise throws an error.
#' @keywords internal
validate_matching_inputs <- function(left, right, vars = NULL) {
  # Check that inputs are data frames or can be coerced
  if (!is.data.frame(left)) {
    stop("left must be a data frame", call. = FALSE)
  }
  if (!is.data.frame(right)) {
    stop("right must be a data frame", call. = FALSE)
  }

  # Check for empty inputs
  if (nrow(left) == 0) {
    stop("left must have at least one row", call. = FALSE)
  }
  if (nrow(right) == 0) {
    stop("right must have at least one row", call. = FALSE)
  }

  # Check that required variables exist in both datasets
  if (!is.null(vars)) {
    missing_left <- setdiff(vars, names(left))
    if (length(missing_left) > 0) {
      stop(sprintf("left is missing required variables: %s",
                   paste(missing_left, collapse = ", ")), call. = FALSE)
    }

    missing_right <- setdiff(vars, names(right))
    if (length(missing_right) > 0) {
      stop(sprintf("right is missing required variables: %s",
                   paste(missing_right, collapse = ", ")), call. = FALSE)
    }

    # Check that variables are numeric
    for (v in vars) {
      if (!is.numeric(left[[v]])) {
        stop(sprintf("Variable '%s' in left must be numeric", v), call. = FALSE)
      }
      if (!is.numeric(right[[v]])) {
        stop(sprintf("Variable '%s' in right must be numeric", v), call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

#' Extract and standardize IDs from data frames
#'
#' The id a matching carries is the key every downstream verb joins on, so it
#' is resolved once, here, and the same resolution answers `match_couples()`
#' and `join_matched()`. `id_col` names the column to read; with no name given
#' a column called `id` is used, then meaningful row names, then ids
#' synthesized from `prefix`.
#'
#' Ids read from the data must be unique: a repeated value makes every
#' id-keyed join downstream many-to-many, which expands rows and attaches one
#' unit's covariates to another unit's pair. Synthesized ids are unique by
#' construction and are not checked.
#'
#' @param df Data frame to read ids from.
#' @param prefix Prefix for synthesized ids, also the side name used in
#'   messages ("left" / "right").
#' @param id_col Name of the id column, or NULL to resolve one.
#' @param warn_synthetic If TRUE, warn when ids are synthesized because no id
#'   column was named or found.
#' @return Character vector of IDs.
#' @keywords internal
extract_ids <- function(df, prefix = "id", id_col = NULL,
                        warn_synthetic = FALSE) {
  side <- prefix

  # A named column is used as given; a missing one is an error rather than a
  # silent fall-through to synthesized ids that would join to nothing.
  if (!is.null(id_col)) {
    if (!id_col %in% names(df)) {
      stop(sprintf("id column '%s' not found in %s", id_col, side),
           call. = FALSE)
    }
    return(check_unique_ids(as.character(df[[id_col]]), id_col, side))
  }

  if ("id" %in% names(df)) {
    return(check_unique_ids(as.character(df$id), "id", side))
  }

  rn <- rownames(df)
  if (!is.null(rn) && !all(rn == as.character(seq_len(nrow(df))))) {
    return(check_unique_ids(rn, "row names", side))
  }

  if (warn_synthetic) {
    warning(sprintf(
      paste0("No id column found in %s, so ids %s_1 ... %s_%d were used. ",
             "Downstream verbs (join_matched(), match_data(), ",
             "balance_diagnostics()) join on these values, so pass ",
             "%s_id = \"<column>\" to key the matching on your own ",
             "identifier."),
      side, prefix, prefix, nrow(df), side), call. = FALSE)
  }

  paste0(prefix, "_", seq_len(nrow(df)))
}

#' Error on repeated id values
#'
#' @return The ids, invisibly unchanged, when they are unique.
#' @keywords internal
check_unique_ids <- function(ids, id_col, side) {
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) > 0L) {
    shown <- paste(utils::head(dup, 5L), collapse = ", ")
    if (length(dup) > 5L) {
      shown <- paste0(shown, ", ... (", length(dup), " values in total)")
    }
    stop(sprintf(
      paste0("Duplicate IDs found in %s dataset: column '%s' repeats %s. ",
             "Matching keys pairs on this column, so repeated values pair ",
             "the wrong units downstream."),
      side, id_col, shown), call. = FALSE)
  }
  ids
}

#' What the design identifies
#'
#' Every matching design in the package weights the left side at 1 and the
#' right side to reproduce the left distribution within a pair, group or
#' stratum, so what it targets is the effect on the left population: the ATT,
#' in the orientation the matching layer is written in, where `left` holds the
#' treated units.
#'
#' `focal_discarded` is how many left units the design did not retain.
#' Calipers, `max_distance` and coarsened strata all drop focal units, and the
#' estimate then speaks about the retained ones rather than about the whole
#' treated group, which is what [as_matchit()] reports when it hands the
#' estimand to MatchIt and marginaleffects.
#'
#' @return Named list of fields to merge into a result's `info`.
#' @keywords internal
design_estimand <- function(n_left, n_left_matched) {
  list(
    estimand = "ATT",
    focal = "left",
    focal_discarded = as.integer(n_left - n_left_matched)
  )
}

#' Extract matching variables from data frame
#'
#' @return Numeric matrix of matching variables.
#' @keywords internal
extract_matching_vars <- function(df, vars) {
  mat <- as.matrix(df[, vars, drop = FALSE])

  # Check for NA/NaN/Inf
  if (any(is.na(mat))) {
    stop("Missing values (NA) not allowed in matching variables", call. = FALSE)
  }
  if (any(is.nan(mat))) {
    stop("NaN values not allowed in matching variables", call. = FALSE)
  }
  if (any(is.infinite(mat))) {
    stop("Infinite values not allowed in matching variables", call. = FALSE)
  }

  mat
}

#' Standardize block ID column name
#'
#' @return Character string with column name, or NULL if not found.
#' @keywords internal
get_block_id_column <- function(df) {
  # Check for common block ID column names
  candidates <- c("block_id", "blockid", "block", "stratum", "stratum_id")

  found <- intersect(candidates, names(df))
  if (length(found) > 0) {
    return(found[1])
  }

  NULL
}

#' Check if data frame has blocking information
#'
#' @return Logical indicating whether data frame has block ID column.
#' @keywords internal
has_blocks <- function(df) {
  !is.null(get_block_id_column(df))
}

#' Validate weights parameter
#'
#' @return Numeric vector of validated weights.
#' @keywords internal
validate_weights <- function(weights, vars) {
  if (is.null(weights)) {
    return(rep(1, length(vars)))
  }

  if (is.numeric(weights)) {
    if (length(weights) != length(vars)) {
      stop(sprintf("weights must have length %d (one per variable)", length(vars)),
           call. = FALSE)
    }
    if (any(weights < 0)) {
      stop("weights must be non-negative", call. = FALSE)
    }
    return(weights)
  }

  # Named weights
  if (is.list(weights) || (is.numeric(weights) && !is.null(names(weights)))) {
    w_vec <- rep(1, length(vars))
    names(w_vec) <- vars

    for (nm in names(weights)) {
      if (!(nm %in% vars)) {
        stop(sprintf("weights contains unknown variable: %s", nm), call. = FALSE)
      }
      w_vec[nm] <- weights[[nm]]
    }
    return(as.numeric(w_vec))
  }

  stop("weights must be a numeric vector or named list", call. = FALSE)
}

#' Validate calipers parameter
#'
#' @return Validated calipers (list or named numeric), or NULL if none.
#' @keywords internal
validate_calipers <- function(calipers, vars) {
  if (is.null(calipers)) {
    return(NULL)
  }

  if (!is.list(calipers) && !is.numeric(calipers)) {
    stop("calipers must be a named numeric vector or list", call. = FALSE)
  }

  if (is.null(names(calipers))) {
    stop("calipers must be named (variable names)", call. = FALSE)
  }

  # Check that all caliper variables exist in vars
  unknown <- setdiff(names(calipers), vars)
  if (length(unknown) > 0) {
    stop(sprintf("calipers contains unknown variables: %s",
                 paste(unknown, collapse = ", ")), call. = FALSE)
  }

  # Check that values are positive
  vals <- as.numeric(calipers)
  if (any(vals <= 0)) {
    stop("caliper values must be positive", call. = FALSE)
  }

  calipers
}
