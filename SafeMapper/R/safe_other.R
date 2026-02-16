# =============================================================================
# Safe Other Functions - pmap, walk, imap
# =============================================================================

# pmap Functions ------------------------------------------------------------

#' Safe PMap - Drop-in Replacement for purrr::pmap with Auto-Recovery
#'
#' @param .l A list of lists/vectors to map over.
#' @param .f A function, formula, or vector.
#' @param ... Additional arguments passed to .f.
#' @param .session_id Character. Optional session ID.
#' @return A list.
#' @export
s_pmap <- function(.l, .f, ..., .session_id = NULL) {
  if (!is.list(.l) || length(.l) == 0) {
    stop(".l must be a non-empty list", call. = FALSE)
  }
  .safe_execute(
    data = .l, func = .f, session_id = .session_id,
    mode = "pmap", output_type = "list", ...
  )
}

#' @rdname s_pmap
#' @param .options A furrr_options object (NULL uses defaults).
#' @param .env_globals The environment to look for globals.
#' @param .progress A single logical for progress bar.
#' @export
s_future_pmap <- function(.l, .f, ..., .options = NULL,
                          .env_globals = parent.frame(), .progress = FALSE,
                          .session_id = NULL) {
  .check_furrr()
  
  if (!is.list(.l) || length(.l) == 0) {
    stop(".l must be a non-empty list", call. = FALSE)
  }
  
  if (is.null(.options)) .options <- furrr::furrr_options()
  
  .safe_execute(
    data = .l, func = .f, session_id = .session_id,
    mode = "future_pmap", output_type = "list",
    .options = .options, .env_globals = .env_globals,
    .progress = .progress, ...
  )
}

# walk Functions ------------------------------------------------------------

#' Safe Walk - Drop-in Replacement for purrr::walk with Auto-Recovery
#'
#' @param .x A list or atomic vector.
#' @param .f A function, formula, or vector.
#' @param ... Additional arguments passed to .f.
#' @param .session_id Character. Optional session ID.
#' @return Invisibly returns .x.
#' @export
s_walk <- function(.x, .f, ..., .session_id = NULL) {
  .safe_execute(
    data = list(.x), func = .f, session_id = .session_id,
    mode = "walk", output_type = "walk", ...
  )
  invisible(.x)
}

#' @rdname s_walk
#' @param .y A list or atomic vector (same length as .x).
#' @export
s_walk2 <- function(.x, .y, .f, ..., .session_id = NULL) {
  .safe_execute(
    data = list(.x, .y), func = .f, session_id = .session_id,
    mode = "walk2", output_type = "walk", ...
  )
  invisible(.x)
}

#' @rdname s_walk
#' @param .options A furrr_options object (NULL uses defaults).
#' @param .env_globals The environment to look for globals.
#' @param .progress A single logical.
#' @export
s_future_walk <- function(.x, .f, ..., .options = NULL,
                          .env_globals = parent.frame(), .progress = FALSE,
                          .session_id = NULL) {
  .check_furrr()
  
  if (is.null(.options)) .options <- furrr::furrr_options()
  
  .safe_execute(
    data = list(.x), func = .f, session_id = .session_id,
    mode = "future_walk", output_type = "walk",
    .options = .options, .env_globals = .env_globals,
    .progress = .progress, ...
  )
  invisible(.x)
}

#' @rdname s_walk
#' @export
s_future_walk2 <- function(.x, .y, .f, ..., .options = NULL,
                           .env_globals = parent.frame(), .progress = FALSE,
                           .session_id = NULL) {
  .check_furrr()
  
  if (is.null(.options)) .options <- furrr::furrr_options()
  
  .safe_execute(
    data = list(.x, .y), func = .f, session_id = .session_id,
    mode = "future_walk2", output_type = "walk",
    .options = .options, .env_globals = .env_globals,
    .progress = .progress, ...
  )
  invisible(.x)
}

# imap Functions ------------------------------------------------------------

#' Safe IMap - Drop-in Replacement for purrr::imap with Auto-Recovery
#'
#' @param .x A list or atomic vector.
#' @param .f A function, formula, or vector.
#' @param ... Additional arguments passed to .f.
#' @param .session_id Character. Optional session ID.
#' @return A list.
#' @export
s_imap <- function(.x, .f, ..., .session_id = NULL) {
  .safe_execute(
    data = list(.x), func = .f, session_id = .session_id,
    mode = "imap", output_type = "list", ...
  )
}

#' @rdname s_imap
#' @return A character vector.
#' @export
s_imap_chr <- function(.x, .f, ..., .session_id = NULL) {
  result <- .safe_execute(
    data = list(.x), func = .f, session_id = .session_id,
    mode = "imap", output_type = "character", ...
  )
  as.character(result)
}

#' @rdname s_imap
#' @param .options A furrr_options object (NULL uses defaults).
#' @param .env_globals The environment to look for globals.
#' @param .progress A single logical.
#' @export
s_future_imap <- function(.x, .f, ..., .options = NULL,
                          .env_globals = parent.frame(), .progress = FALSE,
                          .session_id = NULL) {
  .check_furrr()
  
  if (is.null(.options)) .options <- furrr::furrr_options()
  
  .safe_execute(
    data = list(.x), func = .f, session_id = .session_id,
    mode = "future_imap", output_type = "list",
    .options = .options, .env_globals = .env_globals,
    .progress = .progress, ...
  )
}
