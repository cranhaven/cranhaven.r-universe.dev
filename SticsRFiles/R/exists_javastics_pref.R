#' @title Evaluate if model preferences have been set
#'
#' @description Testing if preferences.xml file exist in JavaSTICS
#' installation folder
#'
#' @param javastics JavaSTICS installation folder
#'
#' @examples
#' \dontrun{
#' exists_pref <- exists_javastics_pref("/path/to/JavaSTICS/folder")
#' }
#'
#' @return logical value, TRUE if file exists, FALSE otherwise
#'
#' @keywords internal
#'
#' @noRd
#'
#'
exists_javastics_pref <- function(javastics) {
  # checking javastics path
  check_java_path(javastics)

  # Returning if file exists
  return(file.exists(file.path(javastics, "config", "preferences.xml")))
}
