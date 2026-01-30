#' log_file_ops
#'
#' Create the necessary file infrastructure to efficiently start logging with
#' "log4r".
#'
#' @param dir_path The name of the folder in which the logfile should be saved.
#' Creates the folder if required.
#'
#' @param logfile_nm Provide a name for the logfile. Do not include suffix.
#' Defaults to "logfile".
#'
#' @return Creates log directory and log file if required. Calls log_enable()
#' to assign necessary logging objects in specified scope.
#'
#' @examples
#'
#' \dontshow{.old_wd <- setwd(tempdir())}
#'
#' log_file_ops(dir_path = "logs")
#'
#' unlink("logs", recursive = TRUE)
#'
#' \dontshow{setwd(.old_wd)}
#'
#' @export
log_file_ops <- function(dir_path = NULL, logfile_nm = "logfile"){
  # store log location
  log_loc <- paste0(dir_path, "/", logfile_nm, ".txt")

  if(file.exists(log_loc)){
    # if logfile exists output warning & stop
    stop("Logfile with name matching logfile_nm found. Have you previously run
         `ptspotter::log_enable()`?")

  } else if(!dir.exists(dir_path)){
    # only create folder if it doesn't exist
    # create log directory
    dir.create(dir_path)

    if(dir.exists(dir_path)){
      message(paste0("Logging directory successfully created at '",
                   dir_path, "/'"))
    }

  }

  # create the logfile
  file.create(log_loc)
  if(file.exists(log_loc)){
    message(paste0("Logfile successfully created at '", log_loc, "'"))
  }

}
