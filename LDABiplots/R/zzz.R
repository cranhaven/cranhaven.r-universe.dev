#' @export
.onAttach <- function(...) {
  packageStartupMessage("\nTo start with the shiny web-interface, please digit:
runLDABiplots()\n")
}