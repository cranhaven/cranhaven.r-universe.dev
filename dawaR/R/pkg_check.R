check_sf_installation <- function(verbose = FALSE) {
  # Check if the OS is macOS
  if (Sys.info()["sysname"] == "Darwin") {
    dirs <- basename(list.dirs(find.package("sf")))
    if ("gdal" %in% dirs && verbose == TRUE) {
      cli::cli_alert_success("Package {.pkg sf} is installed correctly.")
    } else if (!"gdal" %in% dirs) {
      cli::cli_abort(
        "Package {.pkg sf} has not been installed correctly.
      Please consult
        {.url https://dawar.aleksanderbl.dk/articles/renv-issues.html}."
      )
    } else if ("gdal" %in% dirs) {
      return(TRUE)
    }
  }
}
