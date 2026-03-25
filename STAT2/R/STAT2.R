#' Start STAT2
#' @title Launch 'STAT2' Interface
#' @return Nothing
#' @description STAT2() loads interactive user interface built using R 'shiny'.
#' @details The interactive user interface is to provide an easy way for basic statistical analysis and downloading plots.
#' @keywords STAT2
#' @examples
#' if(interactive()){
#' library(rmarkdown)
#' STAT2()
#' }

STAT2 <- function() {

  rmarkdown::run(system.file("img", "STAT2.Rmd", package = "STAT2"))
  Sys.setenv("R_TESTS" = "")
}


 
