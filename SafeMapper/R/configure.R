# =============================================================================
# SafeMapper Configuration Functions
# =============================================================================

# Internal environment for package state (lazy initialized)
.safemapper_env <- new.env(parent = emptyenv())

#' Get Cache Directory
#'
#' Returns the configured checkpoint directory for SafeMapper.
#' Creates the directory structure on first access if needed.
#'
#' @return Character string of checkpoint directory path.
#' @keywords internal
.get_cache_dir <- function() {
  # Use custom directory if configured, otherwise use R user cache dir
 config <- .get_config()
  if (!is.null(config$checkpoint_dir)) {
    cache_dir <- config$checkpoint_dir
  } else {
    cache_dir <- tools::R_user_dir("SafeMapper", "cache")
  }
 
  # Create directory structure on first access
  checkpoint_dir <- file.path(cache_dir, "checkpoints")
  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  cache_dir
}

#' Get Configuration (Lazy Loading)
#'
#' Returns the current configuration, initializing with defaults if needed.
#'
#' @return List of configuration settings.
#' @keywords internal
.get_config <- function() {
  if (is.null(.safemapper_env$config)) {
    .safemapper_env$config <- list(
      batch_size = 100L,
      retry_attempts = 3L,
      auto_recover = TRUE,
      checkpoint_dir = NULL
    )
  }
  .safemapper_env$config
}

#' Configure SafeMapper Settings
#'
#' Optionally customize SafeMapper behavior. This function is NOT required
#' for basic usage - SafeMapper works out of the box with sensible defaults.
#'
#' @param batch_size Integer. Number of items to process per batch before
#'   checkpointing. Smaller values provide more frequent saves but may slow
#'   processing. Default is 100.
#' @param retry_attempts Integer. Number of retry attempts for failed batches.
#'   Default is 3.
#' @param auto_recover Logical. Whether to automatically resume from checkpoints
#'   when restarting operations. Default is TRUE.
#' @param checkpoint_dir Character. Optional custom directory for storing
#'   checkpoint files. If NULL (default), uses the standard R user cache
#'   directory via \code{tools::R_user_dir()}.
#'
#' @return Invisible list of current configuration settings.
#'
#' @examples
#' # Basic usage - no configuration needed!
#' # result <- s_map(1:100, slow_function)
#'
#' # Optional: customize for large operations
#' s_configure(batch_size = 50)
#'
#' # Optional: customize for unstable operations
#' s_configure(retry_attempts = 5)
#'
#' \donttest{
#' # Use custom checkpoint directory (e.g., for testing)
#' s_configure(checkpoint_dir = tempdir())
#' }
#'
#' @export
s_configure <- function(batch_size = 100L,
                        retry_attempts = 3L,
                        auto_recover = TRUE,
                        checkpoint_dir = NULL) {
  .safemapper_env$config <- list(
    batch_size = as.integer(batch_size),
    retry_attempts = as.integer(retry_attempts),
    auto_recover = auto_recover,
    checkpoint_dir = checkpoint_dir
  )
  
  invisible(.safemapper_env$config)
}
