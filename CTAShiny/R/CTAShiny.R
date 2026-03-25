#' Start CTAShiny
#' @title Launch CTAShiny Interface
#' @return Nothing
#' @description CTAShiny() loads interactive user interface built using R shiny.
#' @details The interactive user interface is to provide an easy way for people who are learning Contingency Table Analysis. Includes example data for testing out a few example analysis.
#' @keywords CTAShiny
#' @export CTAShiny:: CTAShiny()
#' @examples
#' if(interactive()){
#' library(shiny)
#' CTAShiny()
#' }
 CTAShiny <- function() {
  shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "CTAShiny"))
  Sys.setenv("R_TESTS" = "")
}
