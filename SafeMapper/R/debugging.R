# =============================================================================
# Debugging and Utility Functions
# =============================================================================

#' Debug a SafeMapper Session
#'
#' Provides diagnostic information about a specific checkpoint session,
#' including progress, errors, and suggested fixes.
#'
#' @param session_id Character. Session ID to debug.
#'
#' @keywords internal
s_debug_session <- function(session_id) {
  checkpoint_file <- .get_checkpoint_path(session_id)

  if (!file.exists(checkpoint_file)) {
    message("Session not found: ", session_id)
    return(invisible(NULL))
  }
  
  data <- readRDS(checkpoint_file)

  cat("Session Debug Info\n")
  cat(strrep("-", 40), "\n")
  cat("Session ID:      ", data$metadata$session_id, "\n")
  cat("Mode:            ", data$metadata$mode, "\n")

  cat("Total items:     ", data$metadata$total_items, "\n")
  cat("Completed items: ", data$metadata$completed_items %||% length(data$results), "\n")
  cat("Progress:        ", sprintf("%.1f%%", 
      100 * (data$metadata$completed_items %||% length(data$results)) / 
      data$metadata$total_items), "\n")
  cat("Created:         ", format(data$metadata$created), "\n")

  if (!is.null(data$metadata$last_updated)) {
    cat("Last updated:    ", format(data$metadata$last_updated), "\n")
  }

  if (!is.null(data$metadata$error_message)) {
    cat("\nError: ", data$metadata$error_message, "\n")
  } else {
    cat("\nStatus: In progress\n")
  }
  
  invisible(data)
}

#' Get SafeMapper Session Statistics
#'
#' Provides summary statistics of all checkpoint sessions.
#'
#' @return Invisible data frame of session information.
#'
#' @keywords internal
s_session_stats <- function() {
  sessions <- s_list_sessions()

  if (nrow(sessions) == 0) {
    cat("No active sessions.\n")
    return(invisible(NULL))
  }

  cat("SafeMapper Session Statistics\n")
  cat(strrep("-", 40), "\n")
  cat("Total sessions:     ", nrow(sessions), "\n")
  cat("In progress:        ", sum(sessions$status == "in_progress", na.rm = TRUE), "\n")
  cat("Failed:             ", sum(sessions$status == "failed", na.rm = TRUE), "\n")
  cat("Corrupted:          ", sum(sessions$status == "corrupted", na.rm = TRUE), "\n")

  total_items <- sum(sessions$total_items, na.rm = TRUE)
  completed_items <- sum(sessions$items_completed, na.rm = TRUE)

  if (total_items > 0) {
    cat("Items completed:    ", completed_items, "/", total_items, 
        sprintf(" (%.1f%%)", 100 * completed_items / total_items), "\n")
  }

  cat("Oldest session:     ", format(min(sessions$created, na.rm = TRUE)), "\n")
  cat("Newest session:     ", format(max(sessions$created, na.rm = TRUE)), "\n")

  invisible(sessions)
}

#' Get SafeMapper Package Information
#'
#' Displays package version and basic information.
#'
#' @keywords internal
s_version <- function() {
  cat("SafeMapper version 1.0.0\n")
  cat("Author: Zaoqu Liu <liuzaoqu@163.com>\n")
  cat("https://github.com/Zaoqu-Liu/SafeMapper\n")
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("SafeMapper: Fault-tolerant functional programming")
}
