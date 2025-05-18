#' Initialize JavaSTICS preferences
#'
#' @description Initialize the JavaSTICS `preferences.xml` file.
#'
#' @param javastics JavaSTICS installation folder
#' @param overwrite Boolean. Overwrite the existing preference file ?
#'
#' @return `TRUE` if the file was created, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' init_javastics_pref("/path/to/JavaSTICS/folder")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
init_javastics_pref <- function(javastics, overwrite = FALSE) {
  check_java_path(javastics)

  config_pref <- file.path(javastics, "config", "preferences.xml")

  if (file.exists(config_pref) && overwrite == FALSE) {
    cli::cli_alert_danger(
      paste0("Preference file already exists, try with {.code overwrite= TRUE}")
    )
    return(FALSE)
  }

  if (is_windows()) {
    pref_file <- "extdata/preferences/preferences_win.xml"
  } else if (is_unix()) {
    pref_file <- "extdata/preferences/preferences_lin.xml"
  } else if (is_mac()) {
    pref_file <- "extdata/preferences/preferences_mac.xml"
  } else {
    stop("Unable to recognize the operating system")
  }


  pref_copy <- file.copy(
    from = system.file(pref_file, package = "SticsOnR", mustWork = TRUE),
    to = normalizePath(config_pref, mustWork = FALSE, winslash = "/"),
    overwrite = overwrite
  )

  if (pref_copy) {
    cli::cli_alert_success("Preference file added in: {.val {config_pref}}")
    return(TRUE)
  } else {
    cli::cli_alert_danger(
      paste0(
        "Couldn't add a {.val preference.xml}",
        "file in the JavaSTICS installation. ",
        "Please run {.pkg Javastics} once to create it."
      ))
    return(FALSE)
  }
}
