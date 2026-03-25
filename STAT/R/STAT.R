#' Start STAT
#' @title Launch 'STAT' Interface
#' @return Nothing
#' @description STAT() loads interactive user interface built using R 'shiny'.
#' @details The interactive user interface is to provide an easy way for basic statistical analysis and downloading plots.
#' @keywords STAT
#' @examples
#' if(interactive()){
#' library(rmarkdown)
#' STAT()
#' }

STAT <- function() {

  rmarkdown::run(system.file("img", "STAT.Rmd", package = "STAT"))
  Sys.setenv("R_TESTS" = "")
}


 
