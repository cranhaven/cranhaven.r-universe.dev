#' Setup PyTLidar Python environment
#'
#' Ensures Python 3.11 is available via conda, creates 'r-reticulate-pytlidar' environment if needed,
#' activates it, and installs required Python modules for PyTLidar.
#'
#' This function **must be called manually** before using any Python-dependent functions.
#' @return `TRUE` if the environment is successfully activated and all required Python modules are available,
#'   `FALSE` if installation of Miniconda was required and R should be restarted.
#' @examples
#' \dontrun{
#' setup_pytlidar()
#' }
#' @export
setup_pytlidar <- function() {
  envname <- "r-reticulate-pytlidar"
  python_version <- "3.11"
  required_modules <- c("torch", "numpy==2.2", "robpy", "PyTLidar")

  # 1. Ensure conda exists
  conda_exists <- tryCatch({
    reticulate::conda_binary()
    TRUE
  }, error = function(e) {
    FALSE
  })
  if (!conda_exists) {
    message("[INFO] No conda detected. Installing r-miniconda...")
    reticulate::install_miniconda()
    warning("[ACTION REQUIRED] Terminate R and re-run setup_pytlidar()")
    options("pytlidar-activated" = FALSE)
    return(invisible(FALSE))
  }

  # 2. Create environment if missing
  if (!reticulate::condaenv_exists(envname)) {
    message("[INFO] Environment ", envname, " not found. Creating Python ", python_version, " environment...")
    reticulate::conda_create(envname, python_version = python_version)
  }

  # 3. Activate environment
  message("[INFO] Activating environment ", envname, "...")
  reticulate::use_condaenv(envname, required = TRUE)

  # 4. Install missing Python modules
  # Check which are installed
  module_names <- sub("==.*$", "", required_modules) #remove version info
  installed <- vapply(module_names, reticulate::py_module_available, logical(1))
  if (!all(installed)) {
    missing <- required_modules[!installed]  # use full spec with version for installation
    message("[INFO] Installing missing Python packages: ", paste(missing, collapse = ", "))
    reticulate::py_install(missing, envname = envname, method = "auto", pip = TRUE)
  } else {
    message("[INFO] All required Python modules are already installed.")
  }

  # 5. Success
  message("[SUCCESS] PyTLidar environment ready! You can now use Python functions.")
  options("pytlidar-activated" = TRUE)
  invisible(TRUE)
}
