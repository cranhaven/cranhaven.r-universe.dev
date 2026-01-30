#' Wrap up file execution.
#'
#' Used to interrupt sequential script execution while testing or debugging.
#' Outputs an auditory signal and breaks sequential script execution,
#' identifying the script at which execution was interrupted.
#' Is a Sys.time() object is passed to `start_time`, messages the elapsed time.
#'
#' @param start_time Optional POSIXct object, created by assigning Sys.time()
#' to an object prior to executing `wrap_up()`.
#'
#' @return Interrupts sequential script execution with an auditory signal. Logs
#' the elapsed time if start_time is used, outputs the script location.
#'
#' @import beepr this.path utils
#'
#' @examples
#'
#' # halt execution with no timing
#' try(wrap_up())
#'
#' # create timing checkpoint
#' s_time <- Sys.time()
#' # halt execution with timing
#' try(wrap_up(s_time))
#'
#' @export
wrap_up <- utils::removeSource(function(start_time = NULL) {
  if(!is.null(start_time)) {
    # calculate elapsed time
    elapsed <- Sys.time() - start_time
    # message execution duration
    message(paste(
    "Script executed. Duration:",
    capture.output(round(elapsed, digits = 3))))
  }

  # sound alert when script completes
  beep("coin")

  # stop execution
  stop(paste("wrap_up at", basename(this.path())))

})
