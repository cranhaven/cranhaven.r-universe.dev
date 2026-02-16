# =============================================================================
# Session Management Functions
# =============================================================================

#' List All Available SafeMapper Sessions
#'
#' Returns a data frame with information about all stored checkpoint sessions,
#' including progress, completion rates, and status.
#'
#' @return Data frame with session information.
#'
#' @keywords internal
s_list_sessions <- function() {
  cache_dir <- .get_cache_dir()
  checkpoint_dir <- file.path(cache_dir, "checkpoints")

  if (!dir.exists(checkpoint_dir)) {
    return(.empty_sessions_df())
  }

  files <- list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(.empty_sessions_df())
  }

  sessions <- data.frame(
    session_id = tools::file_path_sans_ext(basename(files)),
    file_path = files,
    created = file.mtime(files),
    stringsAsFactors = FALSE
  )

  # Add progress info
  sessions$items_completed <- integer(nrow(sessions))
  sessions$total_items <- integer(nrow(sessions))
  sessions$completion_rate <- numeric(nrow(sessions))
  sessions$status <- character(nrow(sessions))

  for (i in seq_len(nrow(sessions))) {
    tryCatch(
      {
        data <- readRDS(sessions$file_path[i])
        sessions$items_completed[i] <- length(data$results)
        sessions$total_items[i] <- data$metadata$total_items
        sessions$completion_rate[i] <- length(data$results) / data$metadata$total_items
        sessions$status[i] <- if (!is.null(data$metadata$error_message)) "failed" else "in_progress"
      },
      error = function(e) {
        sessions$items_completed[i] <<- NA
        sessions$total_items[i] <<- NA
        sessions$completion_rate[i] <<- NA
        sessions$status[i] <<- "corrupted"
      }
    )
  }

  sessions$file_path <- NULL
  return(sessions[order(sessions$created, decreasing = TRUE), ])
}

#' Create Empty Sessions Data Frame
#' @keywords internal
.empty_sessions_df <- function() {
  data.frame(
    session_id = character(0),
    created = as.POSIXct(character(0)),
    items_completed = integer(0),
    total_items = integer(0),
    completion_rate = numeric(0),
    status = character(0),
    stringsAsFactors = FALSE
  )
}

#' Recover a Specific SafeMapper Session
#'
#' Recovers and returns the results from a specific checkpoint session.
#'
#' @param session_id Character. Session ID to recover.
#'
#' @return List of results from the session, or NULL if session not found.
#'
#' @keywords internal
s_recover_session <- function(session_id) {
  checkpoint_file <- .get_checkpoint_path(session_id)

  if (file.exists(checkpoint_file)) {
    data <- readRDS(checkpoint_file)

    if (!is.null(data$metadata$error_message)) {
      message(sprintf("Session '%s' failed with error: %s", session_id, data$metadata$error_message))
      message("Tip: Fix the error before resuming computation")
    }

    message(sprintf(
      "Recovered session '%s' with %d completed items",
      session_id, length(data$results)
    ))
    return(data$results)
  } else {
    message(sprintf("Session '%s' not found", session_id))
    return(NULL)
  }
}

#' Clean Old SafeMapper Sessions
#'
#' Removes old checkpoint files to free up disk space. Can filter by age,
#' specific session IDs, or status.
#'
#' @param older_than_days Integer. Remove sessions older than this many days.
#' @param session_ids Character vector. Specific session IDs to remove.
#' @param status_filter Character vector. Remove only sessions with these statuses
#'   ("in_progress", "failed", "corrupted").
#'
#' @return Integer. Number of files removed (invisible).
#'
#' @examples
#' \donttest{
#' # Clean sessions older than 7 days
#' s_clean_sessions(older_than_days = 7)
#'
#' # Clean only failed sessions
#' s_clean_sessions(status_filter = "failed")
#'
#' # Clean specific sessions
#' s_clean_sessions(session_ids = c("session1", "session2"))
#' }
#'
#' @export
s_clean_sessions <- function(older_than_days = 7, session_ids = NULL, status_filter = NULL) {
  cache_dir <- .get_cache_dir()
  checkpoint_dir <- file.path(cache_dir, "checkpoints")

  if (!dir.exists(checkpoint_dir)) {
    message("No sessions found")
    return(invisible(0L))
  }

  if (!is.null(session_ids)) {
    files_to_remove <- file.path(checkpoint_dir, paste0(session_ids, ".rds"))
    files_to_remove <- files_to_remove[file.exists(files_to_remove)]
  } else {
    all_files <- list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)

    if (length(all_files) == 0) {
      message("No session files found")
      return(invisible(0L))
    }

    # Filter by age
    cutoff_time <- Sys.time() - (older_than_days * 24 * 60 * 60)
    files_to_remove <- all_files[file.mtime(all_files) < cutoff_time]

    # Filter by status if specified
    if (!is.null(status_filter)) {
      valid_files <- character(0)
      for (file in files_to_remove) {
        file_status <- tryCatch(
          {
            data <- readRDS(file)
            if (!is.null(data$metadata$error_message)) "failed" else "in_progress"
          },
          error = function(e) {
            "corrupted"
          }
        )

        if (file_status %in% status_filter) {
          valid_files <- c(valid_files, file)
        }
      }
      files_to_remove <- valid_files
    }
  }

  if (length(files_to_remove) > 0) {
    file.remove(files_to_remove)
    message(sprintf("Removed %d session files", length(files_to_remove)))
  } else {
    message("No files to remove")
  }

  invisible(length(files_to_remove))
}
