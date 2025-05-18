#' @title Getting current workspace in JavaSTICS preferences configuration
#'
#' @description Getting current JavaSTICS working directory,
#' if not any setting to `example` directory
#'
#' @param javastics JavaSTICS installation root folder
#'
#' @return Current JavaStics workspace
#'
#' @examples
#' \dontrun{
#' get_java_workspace("/path/to/JavaSTICS/folder")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'

get_java_workspace <- function(javastics) {


  # checking javastics path
  check_java_path(javastics)

  # if no preference have been set yet
  if (!exists_javastics_pref(javastics)) {
    init_javastics_pref(javastics)
  }

  xml_path <- file.path(javastics, "config", "preferences.xml")

  xml_pref <- xmldocument(xml_path)
  current_wd <- get_values(xml_pref, '//entry[@key="workingDirectory.current"]')

  if (base::is.null(current_wd))
    stop("JavaSTICS working directory hasn't been set ",
         "(use set_java_wd to do so)!")

  delete(xml_pref)

  return(current_wd)
}
