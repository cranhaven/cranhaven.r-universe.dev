#' @title Setting JavaSTICS workspace
#'
#' @description Setting a new JavaSTICS working directory, a relative directory
#' to JavaSTICS path or an absolute one
#'
#' @details Checking if the directory is a JavaSTICS workspace
#' (any usms.xml file), and if it's already registered
#' @details before setting new one
#' @param javastics JavaSTICS installation root folder
#' @param workspace JavaSTICS working directory
#' (absolute,relative to javastics path)
#'
#' @examples
#' \dontrun{
#' set_java_workspace("/path/to/JavaSTICS/folder", "my_wd")
#' set_java_workspace("/path/to/JavaSTICS/folder", "/path/to/my_wd")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'

set_java_workspace <- function(javastics, workspace) {


  # checking javastics path
  check_java_path(javastics)

  # if no preference have been set yet
  if (!exists_javastics_pref(javastics)) {
    init_javastics_pref(javastics)
  }

  xml_path <- file.path(javastics, "config", "preferences.xml")

  xml_pref <- xmldocument(xml_path)

  # checking if workspace is a relative to javaStics path or
  # an absolute one exists
  if (dirname(workspace) == ".") {
    workspace <- file.path(javastics, workspace)
  }

  # checking if exists if it is a workspace a
  ws <- check_java_workspace(javastics, workspace)
  if (base::is.null(ws)) {
    delete(xml_pref)
    return()
  }

  # getting current registered wd
  current_wd <- get_values(xml_pref, '//entry[@key="workingDirectory.current"]')

  # entry doesn't exist, normally it could not occur because
  # we set pref file before, but using JavaSTICS interface first
  # doesn't fix a default workspace, so ...
  if (base::is.null(current_wd)) {
    n <- XML::xmlParseString(
      paste0("<entry key=\"workingDirectory.current\">", workspace, "</entry>")
      )
    add_nodes(xml_pref, n)
  } else {
    # if it's not different from the new one,
    if (current_wd == workspace ||
        (dirname(workspace) == javastics) &&
        basename(workspace) == current_wd) {
      return()
    }

    # else, setting entry value
    set_values(xml_pref, '//entry[@key="workingDirectory.current"]', workspace)
  }

  # writing file
  save_xml_doc(xml_pref, xml_path)

  delete(xml_pref)
}
