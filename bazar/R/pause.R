#' @title 
#' Have a rest, make a pause
#' 
#' @description 
#' The \code{pause} function stops momentarily the 
#' execution of a program. 
#' Pressing <Enter> continues the execution; typing 'stop' 
#' (without quotation marks) ends the program. 
#' 
#' @param duration
#' numeric or infinite. 
#' If \code{duration} is infinite (the default), then a pause is 
#' made until the user presses <Enter> or types 'stop'. 
#' Else if \code{x = duration} is a number, then a pause is made 
#' during \code{x} seconds. 
#' 
#' @seealso 
#' \code{\link{Sys.sleep}}. 
#' 
#' @export
#' 
pause <-
function(duration = Inf)
{

  if (is.infinite(duration)) {
    arg <- "*"
    while (arg != "") {
      arg <- readline("Pause. [<Enter> to continue / 'stop' to exit] ")
      if (arg == "stop") {
        stop("End of the program", call. = FALSE)
      }
    }
  } else {
    cat("Pause of", duration, "seconds\n")
    Sys.sleep(duration)
  }
  invisible(NULL)
}
