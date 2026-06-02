# Package verbosity option + flexible logger
# Canonical definitions of hm_get_verbosity() and hm_inform() for the package.
# All other files must NOT redefine these functions.

#' @keywords internal
hm_get_verbosity <- function() {
  lv <- getOption("healthmarkers.verbose", default = "none")
  if (isTRUE(lv)) return("inform")
  if (identical(lv, FALSE)) return("none")
  lv <- tolower(as.character(lv))
  if (lv %in% c("info")) lv <- "inform"
  if (lv %in% c("none","inform","debug")) lv else "none"
}

#' @keywords internal
hm_inform <- function(level = c("inform","info","debug"), msg = NULL, verbose = NULL) {
  # Support hm_inform("message", level="inform")
  if (is.null(msg) && !missing(level) && is.character(level) && length(level) == 1L &&
      !(tolower(level) %in% c("inform","info","debug"))) {
    msg <- level
    level <- "inform"
  }
  level <- tolower(if (length(level)) level[[1]] else "inform")
  if (level == "info") level <- "inform"

  # "inform"-level messages always emit (the caller already chose level = "inform"
  # only when verbose = TRUE).  "debug"-level messages are gated by the global
  # option so they only appear when the user opts into debug logging.
  emit <- switch(level,
    inform = TRUE,
    debug  = hm_get_verbosity() %in% c("debug"),
    FALSE
  )
  if (emit && !is.null(msg)) {
    if (length(msg) != 1L) msg <- paste(msg, collapse = "\n")
    if (nzchar(msg)) message(msg)
  }
  invisible(NULL)
}