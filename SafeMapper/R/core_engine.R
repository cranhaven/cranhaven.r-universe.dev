# =============================================================================
# Core Execution Engine
# =============================================================================

#' Null-coalescing Operator
#'
#' @param x First value
#' @param y Alternative value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
#' @name null-default
#' @aliases grapes-or-or-grapes
NULL

#' @rdname null-default
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Generate Data Fingerprint
#'
#' Creates a stable identifier based on input data characteristics.
#' Used for automatic session identification without user intervention.
#'
#' @param data List of input data vectors.
#' @param mode Character string indicating the operation mode.
#' @return Character string fingerprint.
#' @keywords internal
.make_fingerprint <- function(data, mode) {
  n <- length(data[[1]])
  if (n == 0) return(paste0(mode, "_empty"))
  
  features <- list(
    mode  = mode,
    len   = n,
    class = class(data[[1]]),
    first = data[[1]][[1]],
    last  = data[[1]][[n]]
  )
  
  paste0(mode, "_", digest::digest(features, algo = "xxhash64"))
}

#' Get Checkpoint File Path
#'
#' @param session_id Character session identifier.
#' @return Character file path.
#' @keywords internal
.get_checkpoint_path <- function(session_id) {
  cache_dir <- .get_cache_dir()
  file.path(cache_dir, "checkpoints", paste0(session_id, ".rds"))
}

#' Try to Restore from Checkpoint
#'
#' Attempts to load an existing checkpoint and verify it matches current data.
#'
#' @param session_id Character session identifier.
#' @param total_items Integer total number of items expected.
#' @return List with results and completed_items, or NULL if not found/invalid.
#' @keywords internal
.try_restore <- function(session_id, total_items) {
  config <- .get_config()
  if (!config$auto_recover) return(NULL)
  
  checkpoint_file <- .get_checkpoint_path(session_id)
  if (!file.exists(checkpoint_file)) return(NULL)
  
  checkpoint <- tryCatch(
    readRDS(checkpoint_file),
    error = function(e) NULL
  )
  
  if (is.null(checkpoint)) return(NULL)
  
  # Verify data length matches
  if (checkpoint$metadata$total_items != total_items) {
    return(NULL)
  }
  
  # Return results and completed count
  list(
    results = checkpoint$results,
    completed_items = checkpoint$metadata$completed_items %||% 0
  )
}

#' Save Checkpoint
#'
#' @param session_id Character session identifier.
#' @param results List of results so far.
#' @param total_items Integer total number of items.
#' @param mode Character operation mode.
#' @param completed_idx Integer index of last completed item.
#' @keywords internal
.save_checkpoint <- function(session_id, results, total_items, mode, completed_idx) {
  checkpoint_file <- .get_checkpoint_path(session_id)
  
  checkpoint_data <- list(
    results = results,
    metadata = list(
      session_id = session_id,
      total_items = total_items,
      completed_items = completed_idx,
      mode = mode,
      created = Sys.time(),
      last_updated = Sys.time()
    )
  )
  
  saveRDS(checkpoint_data, checkpoint_file)
}

#' Cleanup Checkpoint File
#'
#' @param session_id Character session identifier.
#' @keywords internal
.cleanup_checkpoint <- function(session_id) {
  checkpoint_file <- .get_checkpoint_path(session_id)
  if (file.exists(checkpoint_file)) {
    file.remove(checkpoint_file)
  }
}

#' Execute Single Batch
#'
#' @param data List of input data.
#' @param func Function to apply.
#' @param batch_indices Integer vector of indices for this batch.
#' @param mode Character operation mode.
#' @param ... Additional arguments passed to func.
#' @return List of batch results.
#' @keywords internal
.execute_batch <- function(data, func, batch_indices, mode,
                           .options = NULL, .env_globals = NULL,
                           .progress = FALSE, ...) {
  batch_data <- lapply(data, function(x) x[batch_indices])
  
  # Sequential modes
  if (mode == "map") {
    return(purrr::map(batch_data[[1]], func, ...))
  }
  
  if (mode == "walk") {
    purrr::walk(batch_data[[1]], func, ...)
    return(rep(list(NULL), length(batch_indices)))
  }
  
  if (mode == "imap") {
    # For imap, we need to pass the original indices
    return(purrr::imap(batch_data[[1]], func, ...))
  }
  
  if (mode == "map2") {
    return(purrr::map2(batch_data[[1]], batch_data[[2]], func, ...))
  }
  
  if (mode == "walk2") {
    purrr::walk2(batch_data[[1]], batch_data[[2]], func, ...)
    return(rep(list(NULL), length(batch_indices)))
  }
  
  if (mode == "pmap") {
    return(purrr::pmap(batch_data, func, ...))
  }
  
  # Future (parallel) modes - require furrr
  if (grepl("^future_", mode)) {
    .check_furrr()
    
    if (is.null(.options)) .options <- furrr::furrr_options()
    if (is.null(.env_globals)) .env_globals <- parent.frame(2)
    
    base_mode <- sub("^future_", "", mode)
    
    if (base_mode == "map") {
      return(furrr::future_map(batch_data[[1]], func, ...,
                               .options = .options,
                               .progress = .progress))
    }
    
    if (base_mode == "map2") {
      return(furrr::future_map2(batch_data[[1]], batch_data[[2]], func, ...,
                                .options = .options,
                                .progress = .progress))
    }
    
    if (base_mode == "pmap") {
      return(furrr::future_pmap(batch_data, func, ...,
                                .options = .options,
                                .progress = .progress))
    }
    
    if (base_mode == "walk") {
      furrr::future_walk(batch_data[[1]], func, ...,
                         .options = .options,
                         .progress = .progress)
      return(rep(list(NULL), length(batch_indices)))
    }
    
    if (base_mode == "walk2") {
      furrr::future_walk2(batch_data[[1]], batch_data[[2]], func, ...,
                          .options = .options,
                          .progress = .progress)
      return(rep(list(NULL), length(batch_indices)))
    }
    
    if (base_mode == "imap") {
      return(furrr::future_imap(batch_data[[1]], func, ...,
                                .options = .options,
                                .progress = .progress))
    }
  }
  
  stop("Unknown mode: ", mode, call. = FALSE)
}

#' Execute Batch with Retry Logic
#'
#' @param data List of input data.
#' @param func Function to apply.
#' @param batch_indices Integer vector of indices.
#' @param mode Character operation mode.
#' @param config Configuration list.
#' @param ... Additional arguments.
#' @return List of batch results.
#' @keywords internal
.execute_batch_with_retry <- function(data, func, batch_indices, mode, config, ...) {
  last_error <- NULL
  
  for (attempt in seq_len(config$retry_attempts)) {
    result <- tryCatch(
      .execute_batch(data, func, batch_indices, mode, ...),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
    
    if (attempt < config$retry_attempts) {
      message(sprintf("  Retry %d/%d: %s",
                      attempt, config$retry_attempts, last_error$message))
      Sys.sleep(1)
    }
  }
  
  stop(sprintf("Batch failed after %d attempts: %s",
               config$retry_attempts, last_error$message), call. = FALSE)
}

#' Core Safe Execution Engine
#'
#' Main entry point for all safe mapping operations. Handles automatic
#' checkpointing and recovery without user intervention.
#'
#' @param data List of input data vectors.
#' @param func Function to apply.
#' @param session_id Optional character session ID (auto-generated if NULL).
#' @param mode Character operation mode.
#' @param output_type Character output type for formatting.
#' @param .options furrr options (for parallel modes).
#' @param .env_globals Environment for globals (for parallel modes).
#' @param .progress Logical show progress (for parallel modes).
#' @param ... Additional arguments passed to func.
#' @return Formatted results.
#' @keywords internal
.safe_execute <- function(data, func, session_id, mode, output_type,
                          .options = NULL, .env_globals = NULL,
                          .progress = FALSE, ...) {
  # Handle empty input
  total <- length(data[[1]])
  if (total == 0) {
    return(.format_output(list(), output_type))
  }
  
  # Validate multi-input lengths
  if (length(data) > 1) {
    lengths <- vapply(data, length, integer(1))
    if (!all(lengths == lengths[1])) {
      stop("All input vectors must have the same length", call. = FALSE)
    }
  }
  
  config <- .get_config()
  
  # Auto-generate session_id from data fingerprint if not provided
  session_id <- session_id %||% .make_fingerprint(data, mode)
  
  # Try to restore from checkpoint
  restored <- .try_restore(session_id, total)
  
  if (!is.null(restored)) {
    results <- restored$results
    start_idx <- restored$completed_items + 1
    
    if (start_idx <= total) {
      message(sprintf("Resuming from item %d/%d", start_idx, total))
    }
  } else {
    results <- vector("list", total)
    start_idx <- 1
  }
  
  # Already completed
  if (start_idx > total) {
    .cleanup_checkpoint(session_id)
    return(.format_output(results, output_type))
  }
  
  # Process remaining batches
  batch_size <- config$batch_size
  
  for (batch_start in seq(start_idx, total, by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, total)
    batch_indices <- batch_start:batch_end
    
    # Progress indicator
    pct <- round(100 * batch_start / total)
    message(sprintf("[%d%%] Processing items %d-%d of %d",
                    pct, batch_start, batch_end, total))
    
    # Execute batch with retry
    batch_results <- .execute_batch_with_retry(
      data = data,
      func = func,
      batch_indices = batch_indices,
      mode = mode,
      config = config,
      .options = .options,
      .env_globals = .env_globals,
      .progress = .progress,
      ...
    )
    
    # Store results
    results[batch_indices] <- batch_results
    
    # Save checkpoint with completed index
    .save_checkpoint(session_id, results, total, mode, batch_end)
  }
  
  # Success - cleanup checkpoint
  .cleanup_checkpoint(session_id)
  message(sprintf("Completed %d items", total))
  
  .format_output(results, output_type)
}

#' Check if furrr is Available
#'
#' @keywords internal
.check_furrr <- function() {
  if (!requireNamespace("furrr", quietly = TRUE)) {
    stop("Package 'furrr' is required for parallel functions. ",
         "Install it with: install.packages('furrr')", call. = FALSE)
  }
}

#' Format Output According to Type
#'
#' @param results List of results.
#' @param output_type Character output type.
#' @return Formatted output.
#' @keywords internal
.format_output <- function(results, output_type) {
  switch(output_type,
         "list" = results,
         "character" = unlist(results),
         "double" = unlist(results),
         "integer" = unlist(results),
         "logical" = unlist(results),
         "walk" = NULL,
         results
  )
}
