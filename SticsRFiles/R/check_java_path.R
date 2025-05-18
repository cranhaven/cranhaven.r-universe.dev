#' @title Checking JavaSTICS directory content
#'
#' @description Checking if directory exists and if it contains JavaSTICS jar
#' files
#' @details Rising an exception for each checking step !
#' @param javastics JavaSTICS installation root folder
#'
#' @examples
#' \dontrun{
#' check_java_path("/path/to/JavaSTICS/folder")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
check_java_path <- function(javastics) {
  if (!file.exists(javastics)) {
    stop("The JavaStics folder doesn't exist : ", javastics)
  }

  # checking if it's a JavaSTICS root directory
  if (!file.exists(file.path(javastics, "JavaStics.exe")) &&
    !file.exists(file.path(javastics, "JavaStics.jar"))) {
    stop("This directory is not a JavaSTICS one: ", javastics)
  }
}
