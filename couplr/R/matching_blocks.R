# ==============================================================================
# Matching Blocks - Block construction and matchmaker()
# ==============================================================================

#' Create blocks for stratified matching
#'
#' Constructs blocks (strata) for matching, using either grouping variables
#' or clustering algorithms. Returns the input data frames with block IDs
#' assigned, along with block summary statistics.
#'
#' This function does NOT perform matching - it only creates the block structure.
#' Use [match_couples()] to perform matching within blocks.
#'
#' @param left Data frame of "left" units (e.g., treated, cases)
#' @param right Data frame of "right" units (e.g., control, controls)
#' @param block_type Type of blocking to use:
#'   - "none": No blocking (default)
#'   - "group": Block by existing categorical variable(s)
#'   - "cluster": Block using clustering algorithm
#' @param block_by Variable name(s) for grouping (if block_type = "group")
#' @param block_vars Variable names for clustering (if block_type = "cluster")
#' @param block_method Clustering method (if block_type = "cluster"):
#'   - "kmeans": K-means clustering
#'   - "hclust": Hierarchical clustering
#' @param n_blocks Target number of blocks (for clustering)
#' @param min_left Minimum number of left units per block
#' @param min_right Minimum number of right units per block
#' @param drop_imbalanced Drop blocks with extreme imbalance
#' @param imbalance_threshold Maximum allowed |n_left - n_right| / max(n_left, n_right)
#' @param return_dropped Include dropped blocks in output
#' @param ... Additional arguments passed to clustering function
#'
#' @return A list with class "matchmaker_result" containing:
#'   - `left`: Left data frame with block_id column added
#'   - `right`: Right data frame with block_id column added
#'   - `block_summary`: Summary statistics for each block
#'   - `dropped`: Information about dropped blocks (if any)
#'   - `info`: Metadata about blocking process
#'
#' @examples
#' # Group blocking
#' left <- data.frame(id = 1:10, region = rep(c("A", "B"), each = 5), x = rnorm(10))
#' right <- data.frame(id = 11:20, region = rep(c("A", "B"), each = 5), x = rnorm(10))
#' blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
#' print(blocks$block_summary)
#'
#' # Clustering
#' blocks <- matchmaker(left, right, block_type = "cluster",
#'                      block_vars = "x", n_blocks = 3)
#'
#' @export
matchmaker <- function(left, right,
                       block_type = c("none", "group", "cluster"),
                       block_by = NULL,
                       block_vars = NULL,
                       block_method = "kmeans",
                       n_blocks = NULL,
                       min_left = 1,
                       min_right = 1,
                       drop_imbalanced = FALSE,
                       imbalance_threshold = Inf,
                       return_dropped = TRUE,
                       ...) {

  block_type <- match.arg(block_type)

  # Validate inputs
  validate_matching_inputs(left, right)

  # Handle "none" case
  if (block_type == "none") {
    left$block_id <- "all"
    right$block_id <- "all"

    block_summary <- tibble::tibble(
      block_id = "all",
      n_left = nrow(left),
      n_right = nrow(right)
    )

    return(structure(
      list(
        left = left,
        right = right,
        block_summary = block_summary,
        dropped = list(
          blocks = character(0),
          reason = character(0),
          left = left[integer(0), ],
          right = right[integer(0), ]
        ),
        info = list(
          block_type = "none",
          n_blocks_initial = 1,
          n_blocks_kept = 1,
          n_blocks_dropped = 0
        )
      ),
      class = c("matchmaker_result", "couplr_result")
    ))
  }

  # Assign block IDs based on type
  if (block_type == "group") {
    if (is.null(block_by)) {
      stop("For block_type = 'group', must specify block_by", call. = FALSE)
    }
    result <- assign_blocks_group(left, right, block_by)
  } else if (block_type == "cluster") {
    if (is.null(block_vars)) {
      stop("For block_type = 'cluster', must specify block_vars", call. = FALSE)
    }
    result <- assign_blocks_cluster(left, right, block_vars, block_method,
                                    n_blocks, ...)
  }

  left <- result$left
  right <- result$right

  # Filter blocks based on size and balance
  filtered <- filter_blocks(left, right, min_left, min_right,
                           drop_imbalanced, imbalance_threshold)

  left_kept <- filtered$left
  right_kept <- filtered$right
  dropped_info <- filtered$dropped

  # Compute block summary
  block_summary <- summarize_blocks(left_kept, right_kept, block_vars)

  # Prepare output
  structure(
    list(
      left = left_kept,
      right = right_kept,
      block_summary = block_summary,
      dropped = if (return_dropped) dropped_info else NULL,
      info = list(
        block_type = block_type,
        block_by = block_by,
        block_vars = block_vars,
        n_blocks_initial = result$n_blocks_initial,
        n_blocks_kept = nrow(block_summary),
        n_blocks_dropped = length(dropped_info$blocks)
      )
    ),
    class = c("matchmaker_result", "couplr_result")
  )
}

#' Assign blocks based on grouping variable(s)
#'
#' @return List with modified left/right data frames (with block_id) and n_blocks_initial.
#' @keywords internal
assign_blocks_group <- function(left, right, block_by) {
  # Check that variables exist
  missing_left <- setdiff(block_by, names(left))
  if (length(missing_left) > 0) {
    stop(sprintf("left is missing block_by variables: %s",
                 paste(missing_left, collapse = ", ")), call. = FALSE)
  }

  missing_right <- setdiff(block_by, names(right))
  if (length(missing_right) > 0) {
    stop(sprintf("right is missing block_by variables: %s",
                 paste(missing_right, collapse = ", ")), call. = FALSE)
  }

  # Create block IDs by pasting variables together
  if (length(block_by) == 1) {
    left$block_id <- as.character(left[[block_by]])
    right$block_id <- as.character(right[[block_by]])
  } else {
    left$block_id <- do.call(paste, c(left[block_by], sep = "_"))
    right$block_id <- do.call(paste, c(right[block_by], sep = "_"))
  }

  n_blocks_initial <- length(unique(c(left$block_id, right$block_id)))

  list(left = left, right = right, n_blocks_initial = n_blocks_initial)
}

#' Assign blocks using clustering
#'
#' @return List with modified left/right data frames (with block_id) and n_blocks_initial.
#' @keywords internal
assign_blocks_cluster <- function(left, right, block_vars, method, n_blocks, ...) {
  # Validate that variables exist
  validate_matching_inputs(left, right, block_vars)

  # Extract variables for clustering
  left_mat <- extract_matching_vars(left, block_vars)
  right_mat <- extract_matching_vars(right, block_vars)

  # Combine for clustering
  combined_mat <- rbind(left_mat, right_mat)
  n_left <- nrow(left_mat)
  n_total <- nrow(combined_mat)

  # Determine number of blocks
  if (is.null(n_blocks)) {
    n_blocks <- max(2, floor(sqrt(n_total / 2)))
  }

  # Perform clustering
  if (method == "kmeans") {
    if (!requireNamespace("stats", quietly = TRUE)) {
      stop("stats package required for kmeans clustering", call. = FALSE)
    }

    cluster_result <- stats::kmeans(combined_mat, centers = n_blocks, ...)
    cluster_ids <- cluster_result$cluster
  } else if (method == "hclust") {
    if (!requireNamespace("stats", quietly = TRUE)) {
      stop("stats package required for hierarchical clustering", call. = FALSE)
    }

    dist_mat <- stats::dist(combined_mat)
    hc <- stats::hclust(dist_mat, ...)
    cluster_ids <- stats::cutree(hc, k = n_blocks)
  } else {
    stop(sprintf("Unknown clustering method: %s", method), call. = FALSE)
  }

  # Assign block IDs
  left$block_id <- paste0("cluster_", cluster_ids[1:n_left])
  right$block_id <- paste0("cluster_", cluster_ids[(n_left + 1):n_total])

  n_blocks_initial <- length(unique(cluster_ids))

  list(left = left, right = right, n_blocks_initial = n_blocks_initial)
}

#' Filter blocks based on size and balance criteria
#'
#' @return List with filtered left/right data frames and dropped block info.
#' @keywords internal
filter_blocks <- function(left, right, min_left, min_right,
                         drop_imbalanced, imbalance_threshold) {
  # Count units per block
  left_counts <- table(left$block_id)
  right_counts <- table(right$block_id)

  all_blocks <- unique(c(names(left_counts), names(right_counts)))

  # Determine which blocks to keep
  blocks_to_drop <- character(0)
  drop_reasons <- character(0)

  for (block in all_blocks) {
    n_l <- if (block %in% names(left_counts)) left_counts[[block]] else 0
    n_r <- if (block %in% names(right_counts)) right_counts[[block]] else 0

    # Check minimum size
    if (n_l < min_left || n_r < min_right) {
      blocks_to_drop <- c(blocks_to_drop, block)
      drop_reasons <- c(drop_reasons, "too_small")
      next
    }

    # Check imbalance
    if (drop_imbalanced && is.finite(imbalance_threshold)) {
      imbalance <- abs(n_l - n_r) / max(n_l, n_r)
      if (imbalance > imbalance_threshold) {
        blocks_to_drop <- c(blocks_to_drop, block)
        drop_reasons <- c(drop_reasons, "imbalanced")
      }
    }
  }

  # Filter data frames
  left_kept <- left[!(left$block_id %in% blocks_to_drop), ]
  right_kept <- right[!(right$block_id %in% blocks_to_drop), ]

  left_dropped <- left[left$block_id %in% blocks_to_drop, ]
  right_dropped <- right[right$block_id %in% blocks_to_drop, ]

  list(
    left = left_kept,
    right = right_kept,
    dropped = list(
      blocks = blocks_to_drop,
      reason = drop_reasons,
      left = left_dropped,
      right = right_dropped
    )
  )
}

#' Summarize block structure
#'
#' @return Tibble with block_id, n_left, n_right, and optional variable means.
#' @keywords internal
summarize_blocks <- function(left, right, block_vars = NULL) {
  # Count by block
  left_summary <- as.data.frame(table(left$block_id))
  names(left_summary) <- c("block_id", "n_left")
  left_summary$block_id <- as.character(left_summary$block_id)

  right_summary <- as.data.frame(table(right$block_id))
  names(right_summary) <- c("block_id", "n_right")
  right_summary$block_id <- as.character(right_summary$block_id)

  # Merge counts
  summary <- merge(left_summary, right_summary, by = "block_id", all = TRUE)
  summary$n_left[is.na(summary$n_left)] <- 0
  summary$n_right[is.na(summary$n_right)] <- 0

  # Add variable summaries if requested
  if (!is.null(block_vars)) {
    for (var in block_vars) {
      if (var %in% names(left)) {
        # Compute mean for this variable in each block (left side)
        var_means <- stats::aggregate(left[[var]], by = list(left$block_id), FUN = mean)
        names(var_means) <- c("block_id", paste0("mean_", var))
        summary <- merge(summary, var_means, by = "block_id", all.x = TRUE)
      }
    }
  }

  tibble::as_tibble(summary)
}

#' Print method for matchmaker results
#'
#' @param x A matchmaker_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object `x`.
#' @export
#' @method print matchmaker_result
print.matchmaker_result <- function(x, ...) {
  cat("Matchmaker Result\n")
  cat("=================\n\n")

  cat("Block type:", x$info$block_type, "\n")
  cat("Blocks created:", x$info$n_blocks_initial, "\n")
  cat("Blocks kept:", x$info$n_blocks_kept, "\n")
  if (x$info$n_blocks_dropped > 0) {
    cat("Blocks dropped:", x$info$n_blocks_dropped, "\n")
  }

  cat("\nBlock summary:\n")
  print(x$block_summary)

  if (!is.null(x$dropped) && length(x$dropped$blocks) > 0) {
    cat("\nDropped blocks:", paste(x$dropped$blocks, collapse = ", "), "\n")
  }

  invisible(x)
}
