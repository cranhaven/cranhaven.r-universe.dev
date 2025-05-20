
#' print the abbreviated usage information from spip
#'
#' This simply calls spip with the `--help` option.
#' @export
#' @return This returns the exit status of `spip` if spip is installed,
#' but the return value is of little use.  Mainly this is run for the side
#' effect of printing the `spip` help menu to the console.
#' @examples
#' \dontrun{spip_help()}
spip_help <- function() {
  system2(command = spip_binary(), args = "--help")
}
