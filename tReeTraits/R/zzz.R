# Suppress notes about no visible binding for data.table
utils::globalVariables(c("X", "Y", "Z", ".data"))

# Set package global option to track whether python setup is completed
.onAttach <- function(libname, pkgname) {
  options(pytlidar.activated = FALSE)
  packageStartupMessage('To enable QSM functions you must run `setup_pytlidar()`')
}

.check_pytlidar <- function() {
  # Return immediately if already activated
  if (isTRUE(getOption("pytlidar.activated", FALSE))) {
    return(TRUE)
  }

  message('Activating r-reticulate-pytlidar environment')
  envname <- "r-reticulate-pytlidar"

  if (!reticulate::condaenv_exists(envname)) {
    stop(
      "PyTLidar environment '", envname, "' not found.\n",
      "Run setup_pytlidar() to create it.",
      call. = FALSE
    )
  }

  # Use the environment explicitly
  reticulate::use_condaenv(envname, required = TRUE)

  # Mark as activated for this session
  options(pytlidar.activated = TRUE)

  TRUE
}
