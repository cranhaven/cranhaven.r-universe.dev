# This function is called when the package is loaded.
.onLoad <- function(libname, pkgname) {

  # Custom format for the log messages.
  logging_format <- paste(
    "{time}",      # Time of the log message
    "{node}",      # Hostname of the machine
    "{pid}",       # Process ID
    "{namespace}", # Namespace for the logger
    "{fn}",        # Function generated the log message
    "{level}:",    # Log message level (e.g., INFO, DEBUG, ...)
    "{msg}",       # Log message content
    sep = " "
  )

  flogger <- logger::layout_glue_generator(format = logging_format)
  logger::log_layout(flogger, index = 1)
}
