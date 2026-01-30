#' log_enable
#'
#' Assigns the necessary global scope objects for logging with "log4r".
#'
#' @param logfile_loc The path to the logfile. Suggested use "logs/logfile.txt".
#'
#' @param pos The environment which to assign pipeline_message. Defaults to 1,
#' equivalent to the .GlobalEnv.
#'
#' @param logger_nm What to call the logger. Provide unquoted strings with no
#' spaces. Defaults to my_logger.
#'
#' @param appender_nm What to call the appender function. Provide unquoted
#' strings with no spaces. Defaults to file_app.
#'
#' @return Creates logger and file appender.
#'
#' @import log4r
#'
#' @examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#'
#' # create logging infrastructure
#' log_file_ops(dir_path = "logs/logfile.txt")
#' # enable logging
#' log_enable(logfile_loc = "logs/logfile.txt")
#'
#' # tidy up environment
#' unlink("logs", recursive = TRUE)
#'
#' \dontshow{setwd(.old_wd)}
#'
#' @export
log_enable <- function(logfile_loc = NULL, pos = 1,
                       logger_nm = my_logger, appender_nm = file_app) {

  file_app <- file_appender(logfile_loc,
                            append = TRUE,
                            layout = default_log_layout())
  my_logger <- logger(
    threshold = "INFO",
    appenders = file_app)

  # file appender
  # check for presence of file_app in case of reruns
  if(!deparse(substitute(appender_nm)) %in% ls(name = as.environment(pos))){
    # assign file appender
    assign(deparse(substitute(appender_nm)), file_app,
           envir = as.environment(pos))

    #test for presence
    if(deparse(substitute(appender_nm)) %in% ls(name = as.environment(pos))){
      message(paste("File appender successfully assigned to",
                    deparse(substitute(appender_nm)))
      )

      } else{
        stop("File appender not assigned. Logging not enabled.")

        }
    } else {
      message("File appender already exists. Not re-assigned.")

    }

  # logger object
  #test for presence of my_logger in case of reruns
  if(!deparse(substitute(logger_nm)) %in% ls(name = as.environment(pos))){
    # create logger
    assign(deparse(substitute(logger_nm)), my_logger,
           envir = as.environment(pos))
    # test for presence
    if(deparse(substitute(logger_nm)) %in% ls(name = as.environment(pos))){
      message(paste("Logger object sucessfully assigned to",
                    deparse(substitute(logger_nm))))

        } else{
          stop("Logger not assigned. Logging not enabled.")

        }
      } else{
        message("Logger already exists. Not re-assigned.")

      }

  # Check if the logfile already exists, if not output a warning.
  if(!file.exists(logfile_loc)){
    warning(
      "Logfile not found. Please run `ptspotter::log_file_ops()`.")
    }
  }
