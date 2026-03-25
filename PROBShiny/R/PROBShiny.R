#' Start PROBShiny
#' @title Launch 'PROBShiny' Interface
#' @return Nothing
#' @description PROBShiny() loads interactive user interface built using R 'shiny'.
#' @details The interactive user interface is to provide an easy way for solving basic probability problems based on relative frequency and subjective approach.
#' @keywords PROBShiny
#' @examples
#' if(interactive()){
#' library(rmarkdown)
#' PROBShiny()
#' }

PROBShiny <- function() {

  rmarkdown::run(system.file("img", "PROBShiny.Rmd", package = "PROBShiny"))
  Sys.setenv("R_TESTS" = "")
}


 
