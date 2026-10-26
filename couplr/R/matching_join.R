#' Join Matched Pairs with Original Data
#'
#' Creates an analysis-ready dataset by joining matched pairs with variables
#' from the original left and right datasets. This eliminates the need for
#' manual joins and provides a convenient format for downstream analysis.
#'
#' @param result A result object from \code{match_couples()},
#'   \code{full_match()}, or \code{cem_match()}
#' @param left The original left dataset
#' @param right The original right dataset
#' @param left_vars Character vector of variable names to include from left.
#'   If NULL (default), includes all variables except the ID column.
#' @param right_vars Character vector of variable names to include from right.
#'   If NULL (default), includes all variables except the ID column.
#' @param left_id Name of the ID column in left dataset (default: "id")
#' @param right_id Name of the ID column in right dataset (default: "id")
#' @param suffix Character vector of length 2 specifying suffixes for
#'   left and right variables (default: c("_left", "_right"))
#' @param include_distance Include the matching distance in output (default: TRUE)
#' @param include_pair_id Include pair_id column (default: TRUE)
#' @param include_block_id Include block_id if blocking was used (default: TRUE)
#' @param ... Additional arguments passed to methods
#'
#' @return A tibble with one row per matched pair, containing:
#'   - \code{pair_id}: Sequential pair identifier (if include_pair_id = TRUE)
#'   - \code{left_id}: ID from left dataset
#'   - \code{right_id}: ID from right dataset
#'   - \code{distance}: Matching distance (if include_distance = TRUE)
#'   - \code{block_id}: Block identifier (if blocking used and include_block_id = TRUE)
#'   - Variables from left dataset (with left suffix)
#'   - Variables from right dataset (with right suffix)
#'
#' @details
#' This function simplifies the common workflow of joining matched pairs
#' with original data. Instead of manually merging result$pairs with left
#' and right datasets, \code{join_matched()} handles the joins automatically
#' and applies consistent naming conventions.
#'
#' When variables appear in both left and right datasets, suffixes are
#' appended to distinguish them (e.g., "age_left" and "age_right"). This
#' makes it easy to compute differences or use both values in models.
#'
#' @examples
#' # Basic usage
#' left <- data.frame(
#'   id = 1:5,
#'   treatment = 1,
#'   age = c(25, 30, 35, 40, 45),
#'   income = c(45000, 52000, 48000, 61000, 55000)
#' )
#'
#' right <- data.frame(
#'   id = 6:10,
#'   treatment = 0,
#'   age = c(24, 29, 36, 41, 44),
#'   income = c(46000, 51500, 47500, 60000, 54000)
#' )
#'
#' result <- match_couples(left, right, vars = c("age", "income"))
#' matched_data <- join_matched(result, left, right)
#' head(matched_data)
#'
#' # Specify which variables to include
#' matched_data <- join_matched(
#'   result, left, right,
#'   left_vars = c("treatment", "age", "income"),
#'   right_vars = c("age", "income"),
#'   suffix = c("_treated", "_control")
#' )
#'
#' # Without distance or pair_id
#' matched_data <- join_matched(
#'   result, left, right,
#'   include_distance = FALSE,
#'   include_pair_id = FALSE
#' )
#'
#' @export
join_matched <- function(result, ...) {
  UseMethod("join_matched")
}


#' @export
join_matched.default <- function(result, ...) {
  stop("result must be a matching_result object from match_couples()")
}


#' @rdname join_matched
#' @export
join_matched.matching_result <- function(result,
                         left,
                         right,
                         left_vars = NULL,
                         right_vars = NULL,
                         left_id = "id",
                         right_id = "id",
                         suffix = c("_left", "_right"),
                         include_distance = TRUE,
                         include_pair_id = TRUE,
                         include_block_id = TRUE,
                         ...) {

  if (!is.data.frame(left) || !is.data.frame(right)) {
    stop("left and right must be data frames")
  }

  if (!left_id %in% names(left)) {
    stop("left_id column '", left_id, "' not found in left dataset")
  }

  if (!right_id %in% names(right)) {
    stop("right_id column '", right_id, "' not found in right dataset")
  }

  if (length(suffix) != 2) {
    stop("suffix must be a character vector of length 2")
  }

  if (nrow(result$pairs) == 0) {
    warning("No matched pairs to join. Returning empty tibble.")
    return(dplyr::tibble())
  }

  # Determine which variables to include
  if (is.null(left_vars)) {
    left_vars <- setdiff(names(left), left_id)
  } else {
    missing_left <- setdiff(left_vars, names(left))
    if (length(missing_left) > 0) {
      stop("Variables not found in left: ", paste(missing_left, collapse = ", "))
    }
  }

  if (is.null(right_vars)) {
    right_vars <- setdiff(names(right), right_id)
  } else {
    missing_right <- setdiff(right_vars, names(right))
    if (length(missing_right) > 0) {
      stop("Variables not found in right: ", paste(missing_right, collapse = ", "))
    }
  }

  # Start with pairs
  matched <- result$pairs

  # Add pair_id if requested
  if (include_pair_id && !"pair_id" %in% names(matched)) {
    matched <- dplyr::mutate(matched, pair_id = dplyr::row_number(), .before = 1)
  } else if (!include_pair_id && "pair_id" %in% names(matched)) {
    matched$pair_id <- NULL
  }

  # Prepare left dataset for joining
  left_join_data <- left[, c(left_id, left_vars), drop = FALSE]

  # Prepare right dataset for joining
  right_join_data <- right[, c(right_id, right_vars), drop = FALSE]

  # The join key is the id as a string on both sides. `pairs$left_id` already
  # holds as.character() of the original column (extract_ids()), so coercing
  # the join table's key the same way is lossless in both directions and
  # matches whatever the column held: zero-padded codes, decimal-looking
  # strings and factor levels all survive the round trip unchanged.
  left_key <- left_join_data[[left_id]]
  right_key <- right_join_data[[right_id]]
  matched$left_id <- as.character(matched$left_id)
  matched$right_id <- as.character(matched$right_id)
  left_join_data[[left_id]] <- as.character(left_key)
  right_join_data[[right_id]] <- as.character(right_key)

  # Identify overlapping variable names (excluding IDs)
  overlap <- intersect(left_vars, right_vars)

  # Rename overlapping variables in left
  if (length(overlap) > 0) {
    for (var in overlap) {
      old_name <- var
      new_name <- paste0(var, suffix[1])
      names(left_join_data)[names(left_join_data) == old_name] <- new_name
    }
  }

  # Rename overlapping variables in right
  if (length(overlap) > 0) {
    for (var in overlap) {
      old_name <- var
      new_name <- paste0(var, suffix[2])
      names(right_join_data)[names(right_join_data) == old_name] <- new_name
    }
  }

  # Rename non-overlapping variables in left (add suffix to all)
  non_overlap_left <- setdiff(left_vars, overlap)
  for (var in non_overlap_left) {
    old_name <- var
    new_name <- paste0(var, suffix[1])
    names(left_join_data)[names(left_join_data) == old_name] <- new_name
  }

  # Rename non-overlapping variables in right (add suffix to all)
  non_overlap_right <- setdiff(right_vars, overlap)
  for (var in non_overlap_right) {
    old_name <- var
    new_name <- paste0(var, suffix[2])
    names(right_join_data)[names(right_join_data) == old_name] <- new_name
  }

  # Join with left data
  matched <- dplyr::left_join(
    matched,
    left_join_data,
    by = stats::setNames(left_id, "left_id")
  )

  # Join with right data
  matched <- dplyr::left_join(
    matched,
    right_join_data,
    by = stats::setNames(right_id, "right_id")
  )

  # The join ran on strings, so an id that only looks numeric keyed correctly.
  # Report it back in the type the caller's own column holds by reading the
  # value out of that column rather than parsing the string.
  matched$left_id <- left_key[match(matched$left_id, as.character(left_key))]
  matched$right_id <- right_key[match(matched$right_id, as.character(right_key))]

  # Remove distance if not requested
  if (!include_distance && "distance" %in% names(matched)) {
    matched$distance <- NULL
  }

  # Remove block_id if not requested or not present
  if (!include_block_id && "block_id" %in% names(matched)) {
    matched$block_id <- NULL
  }

  # Reorder columns for better readability
  # Priority: pair_id, left_id, right_id, distance, block_id, then variables
  col_order <- c()
  if (include_pair_id && "pair_id" %in% names(matched)) {
    col_order <- c(col_order, "pair_id")
  }
  col_order <- c(col_order, "left_id", "right_id")
  if (include_distance && "distance" %in% names(matched)) {
    col_order <- c(col_order, "distance")
  }
  if (include_block_id && "block_id" %in% names(matched)) {
    col_order <- c(col_order, "block_id")
  }

  # Add remaining columns
  other_cols <- setdiff(names(matched), col_order)
  col_order <- c(col_order, other_cols)

  matched <- matched[, col_order, drop = FALSE]

  dplyr::as_tibble(matched)
}


#' @rdname join_matched
#' @export
join_matched.full_matching_result <- function(result, left, right,
                                              left_id = "id",
                                              right_id = "id",
                                              ...) {
  # For full matching, return the stacked match_data format
  match_data(result, left, right, left_id = left_id, right_id = right_id)
}


#' @rdname join_matched
#' @export
join_matched.cem_result <- function(result, left, right,
                                    left_id = "id",
                                    right_id = "id",
                                    ...) {
  match_data(result, left, right, left_id = left_id, right_id = right_id)
}


#' @rdname join_matched
#' @param data Data frame used for subclassification
#' @export
join_matched.subclass_result <- function(result, data = NULL, ...) {
  match_data(result, data = data)
}


#' Augment Matching Results with Original Data (broom-style)
#'
#' S3 method for augmenting matching results following the broom package
#' conventions. This is a thin wrapper around \code{join_matched()} with
#' sensible defaults for quick exploration.
#'
#' @param x A matching_result object
#' @param left The original left dataset
#' @param right The original right dataset
#' @param ... Additional arguments passed to \code{join_matched()}
#'
#' @return A tibble with matched pairs and original data (see \code{join_matched()})
#'
#' @details
#' This method follows the \code{augment()} convention from the broom package,
#' making it easy to integrate couplr into tidymodels workflows. It's
#' equivalent to calling \code{join_matched()} with default parameters.
#'
#' If the broom package is not loaded, you can use \code{couplr::augment()}
#' to access this function.
#'
#' @examples
#' left <- data.frame(
#'   id = 1:5,
#'   treatment = 1,
#'   age = c(25, 30, 35, 40, 45)
#' )
#'
#' right <- data.frame(
#'   id = 6:10,
#'   treatment = 0,
#'   age = c(24, 29, 36, 41, 44)
#' )
#'
#' result <- match_couples(left, right, vars = "age")
#' augment(result, left, right)
#'
#' @importFrom generics augment
#' @exportS3Method generics::augment
augment.matching_result <- function(x, left, right, ...) {
  join_matched(x, left, right, ...)
}

#' @export
generics::augment
