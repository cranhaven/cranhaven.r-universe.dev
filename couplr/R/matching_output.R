# ==============================================================================
# Unified Output Layer - match_data() generic
# ==============================================================================

#' Extract Analysis-Ready Data from Matching Results
#'
#' A generic function that converts any couplr matching result into a single
#' analysis-ready data frame with \code{weights}, \code{subclass}, and
#' \code{distance} columns. This is the couplr equivalent of MatchIt's
#' \code{match.data()}.
#'
#' @param result A couplr result object (matching_result, full_matching_result,
#'   cem_result, or subclass_result)
#' @param ... Additional arguments passed to methods
#'
#' @return A tibble with all original variables plus standardized columns:
#' \describe{
#'   \item{id}{Unit identifier}
#'   \item{treatment}{1 for left/treated, 0 for right/control}
#'   \item{weights}{Matching weights}
#'   \item{subclass}{Matched group/stratum identifier}
#'   \item{distance}{Matching distance (where applicable)}
#' }
#'
#' @details
#' The output format is compatible with downstream packages like \pkg{cobalt},
#' \pkg{WeightIt}, and \pkg{marginaleffects}. The stacked (long) format with
#' \code{treatment} and \code{weights} columns is the standard layout expected
#' by these tools.
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
#' right <- data.frame(id = 6:15, age = runif(10, 20, 70))
#' result <- match_couples(left, right, vars = "age")
#' md <- match_data(result, left, right)
#' head(md)
#'
#' @export
match_data <- function(result, ...) {
  UseMethod("match_data")
}


# Row positions in `df` for a vector of ids, keyed on `id_col`. Every id-keyed
# lookup in this file goes through here rather than attaching a column onto a
# separately-ordered frame, which is what made weights and subclass follow
# merge()'s lexicographic order instead of their own units.
.id_row_index <- function(df, id_col, ids, side) {
  if (!id_col %in% names(df)) {
    stop(sprintf("id column '%s' not found in %s", id_col, side), call. = FALSE)
  }
  key <- check_unique_ids(as.character(df[[id_col]]), id_col, side)
  idx <- match(as.character(ids), key)
  if (anyNA(idx)) {
    missing <- unique(as.character(ids)[is.na(idx)])
    shown <- paste(utils::head(missing, 5L), collapse = ", ")
    if (length(missing) > 5L) {
      shown <- paste0(shown, ", ... (", length(missing), " in total)")
    }
    stop(sprintf(
      "%d matched %s id(s) are absent from `%s`: %s. Pass the same data the matching was run on, and the same %s_id column.",
      length(missing), side, side, shown, side), call. = FALSE)
  }
  idx
}

# Stack the two sides under one key column called `id`, which is the name the
# output is documented under and the one as_matchit() reads. Renaming only
# when the two sides disagreed left a frame keyed on the caller's own column
# name whenever they agreed on something other than "id".
.harmonize_id_names <- function(left_rows, right_rows, left_id, right_id) {
  names(left_rows)[names(left_rows) == left_id] <- "id"
  names(right_rows)[names(right_rows) == right_id] <- "id"
  tibble::as_tibble(rbind(left_rows, right_rows))
}

#' @rdname match_data
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#' @export
match_data.matching_result <- function(result, left, right,
                                       left_id = "id", right_id = "id",
                                       ...) {
  pairs <- result$pairs
  if (nrow(pairs) == 0) {
    return(tibble::tibble())
  }

  li <- .id_row_index(left, left_id, pairs$left_id, "left")
  ri <- .id_row_index(right, right_id, pairs$right_id, "right")

  # One row per matched pair. A ratio > 1 design puts each left unit in `ratio`
  # pairs and a with-replacement design reuses a right unit across pairs, so a
  # unit appears once per pair it belongs to and carries that pair's subclass
  # and distance.
  #
  # The weight on a pair row is 1 / (pairs its left unit is in), so every
  # matched left unit totals 1 and the right side totals the same. On the 1:1
  # design every share is 1.
  share <- 1 / as.numeric(
    table(as.character(pairs$left_id))[as.character(pairs$left_id)]
  )
  subclass <- seq_len(nrow(pairs))

  left_rows <- left[li, , drop = FALSE]
  left_rows$treatment <- 1L
  left_rows$weights <- share
  left_rows$subclass <- subclass
  left_rows$distance <- pairs$distance

  right_rows <- right[ri, , drop = FALSE]
  right_rows$treatment <- 0L
  right_rows$weights <- share
  right_rows$subclass <- subclass
  right_rows$distance <- pairs$distance

  out <- .harmonize_id_names(left_rows, right_rows, left_id, right_id)
  rownames(out) <- NULL
  out
}


#' @rdname match_data
#' @export
match_data.full_matching_result <- function(result, left, right,
                                            left_id = "id", right_id = "id",
                                            ...) {
  groups <- result$groups
  if (nrow(groups) == 0) return(tibble::tibble())

  # Left units. Full-matching weights vary per unit, so each one is read
  # through its own id rather than assigned by position from a sorted merge.
  left_groups <- groups[groups$side == "left", ]
  li <- .id_row_index(left, left_id, left_groups$id, "left")
  left_rows <- left[li, , drop = FALSE]
  left_rows$treatment <- 1L
  left_rows$weights <- left_groups$weight
  left_rows$subclass <- left_groups$group_id
  left_rows$distance <- NA_real_

  # Right units
  right_groups <- groups[groups$side == "right", ]
  ri <- .id_row_index(right, right_id, right_groups$id, "right")
  right_rows <- right[ri, , drop = FALSE]
  right_rows$treatment <- 0L
  right_rows$weights <- right_groups$weight
  right_rows$subclass <- right_groups$group_id
  right_rows$distance <- NA_real_

  out <- .harmonize_id_names(left_rows, right_rows, left_id, right_id)
  rownames(out) <- NULL
  out
}


#' @rdname match_data
#' @param data Data frame containing all units (for CEM and subclassification,
#'   left and right are not always needed separately)
#' @export
match_data.cem_result <- function(result, left, right,
                                  left_id = "id", right_id = "id",
                                  ...) {
  matched <- result$matched

  # Left units. CEM stratum weights vary per unit, so each is read through its
  # own id rather than assigned by position from a sorted merge.
  left_matched <- matched[matched$side == "left", ]
  li <- .id_row_index(left, left_id, left_matched$id, "left")
  left_rows <- left[li, , drop = FALSE]
  left_rows$treatment <- 1L
  left_rows$weights <- left_matched$weight
  left_rows$subclass <- left_matched$stratum
  left_rows$distance <- NA_real_

  # Right units
  right_matched <- matched[matched$side == "right", ]
  ri <- .id_row_index(right, right_id, right_matched$id, "right")
  right_rows <- right[ri, , drop = FALSE]
  right_rows$treatment <- 0L
  right_rows$weights <- right_matched$weight
  right_rows$subclass <- right_matched$stratum
  right_rows$distance <- NA_real_

  # Only include matched units (weight > 0)
  out <- .harmonize_id_names(left_rows, right_rows, left_id, right_id)
  rownames(out) <- NULL
  out[out$weights > 0, ]
}


#' @rdname match_data
#' @export
match_data.subclass_result <- function(result, data = NULL, ...) {
  matched <- result$matched

  if (!is.null(data)) {
    # Merge in original data columns
    out <- merge(data, matched[, c("id", "subclass", "ps", "weight")],
                 by = "id", all.x = FALSE)
    out$treatment <- ifelse(matched$side[match(out$id, matched$id)] == "left",
                            1L, 0L)
    out$weights <- out$weight
    out$weight <- NULL
    out$distance <- out$ps
  } else {
    out <- matched
    out$treatment <- ifelse(out$side == "left", 1L, 0L)
    out$weights <- out$weight
    out$weight <- NULL
    out$distance <- out$ps
  }

  # Only include units with weight > 0
  out <- out[out$weights > 0, ]
  tibble::as_tibble(out)
}
