#' papciUI
#'
#' Launch the papci user interface in local machine
#'
#'#'
#' @export
#' @examples
#' \dontrun{
#'    papciUI()
#' }

papciUI <- function() {
  shiny::runApp(system.file("shiny",package="papci"))
}
