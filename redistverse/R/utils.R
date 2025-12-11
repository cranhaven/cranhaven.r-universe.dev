# adopted from `tidyverse`
inform_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return(invisible(NULL))
  }
  if (isTRUE(getOption("redistverse.quiet"))) {
    return(invisible(NULL))
  }

  packageStartupMessage(msg, ...)
}
