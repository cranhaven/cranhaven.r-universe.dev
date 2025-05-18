#' @title Checking and getting JavaSTICS workspace path
#'
#' @description Looking in JavaSTICS for a workspace path in JavaSTICS
#' preferences or producing a full path to a workspace located in JavaSTICS root
#' directory or validating an external absolute path. And also checking if the
#' path is a valid JavaSTICS workspace
#'
#' @param javastics Path of JavaSTICS installation directory
#' @param workspace An absolute or relative path (to JavaSTICS path)
#' of a workspace (Optional)
#'
#' @return An absolute JavaSTICS workspace path
#'
#' @keywords internal
#' @noRd

check_java_workspace <- function(javastics, workspace = NULL) {

  # Ensure that the user working directory is unchanged after
  # the function has run
  current_wd <- getwd()
  on.exit(setwd(current_wd))

  setwd(javastics)

  ws <- NULL

  if (!base::is.null(workspace)) {
    if (dirname(workspace) == ".") {
      # relative path to javastics path
      ws <- file.path(javastics, workspace)
    } else {
      ws <- workspace
    }
  } else {
    tt <- try(ws <- get_java_workspace(javastics), silent = TRUE)
    if (methods::is(tt, "try-error")) {
      warning("No workspace directory has been set, use set_java_wd to do so,
              or \n give it as input of the function !")
      return()
    }
  }

  if (base::is.null(ws) || !dir.exists(ws)) {
    warning(paste("The given directory does not exist or JavaSTICS working
                  directory is not set :\n", ws))
    return()
  }

  # checking if it's a workspace directory: searching usms.xml
  if (!file.exists(file.path(ws, "usms.xml"))) {
    warning("This directory is not a JavaSTICS workspace: ", ws)
    return()
  }
  return(ws)
}
