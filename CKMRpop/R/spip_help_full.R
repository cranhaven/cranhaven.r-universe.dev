
#' print the full usage information from spip
#'
#' This simply calls spip with the `--help-full` option.
#' @export
#' @return This returns the exit status of `spip` if spip is installed,
#' but the return value is of little use.  Mainly this is run for the side
#' effect of printing the `spip` full help menu to the console.
#' @examples
#' \dontrun{spip_help()}
spip_help_full <- function() {
  system2(command = spip_binary(), args = "--help-full")
}
