#' Progress bar of given length
#'
#' Simple wrapper for \code{\link[progress]{progress_bar}} for standard
#' and quickly ready progress bars; including messages, bar progression,
#' percentage and time elapsed, and ETA.
#'
#' @param .x (int) total number of step to count
#' @param width (int, default = 76) total console width used by the bar
#' @param show_after (num, default = 2) minimum number of seconds needed
#'      for the process to display the progress bar
#' @param clear (lgl, default = FALSE) if TRUE, at the end of the
#'      process the progress bar will be cleared
#'
#' @return a \code{\link[progress]{progress_bar}} object
#' @export
#'
#' @examples
#' \donttest{
#'   pb <- pb_len(100)
#'   for (i in 1:100) {
#'     Sys.sleep(0.1)
#'     tick(pb, paste("i = ", i))
#'   }
#' }
pb_len <- function(.x, width = 76L, show_after = 2L, clear = FALSE) {
  if (!is.numeric(.x) || (.x != trunc(.x))) {
    ui_stop("{ui_code('.x')} must be an integer.")
  }

  progress::progress_bar[["new"]](
    format =
      "evaluated: :what [:bar] :percent in :elapsed [ETA: :eta]",
    total = .x,
    width = width,
    clear = clear,
    show_after = show_after
  )
}

#' @describeIn pb_len wrapper function to update the progress bar
#' @param pb an object of class \code{\link[progress]{progress_bar}}
#' @param what (chr, default = "") short prompt to see at the beginning
#'     of the progressbar
#' @export
tick <- function(pb, what = "") {
  pb[["tick"]](tokens = list(what = what))
}
