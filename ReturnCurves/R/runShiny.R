#' Complementary Shiny app for the ReturnCurves package
#' 
#' @name runShiny
#' 
#' @description 
#' Launches the R Shiny app complementary to the \code{\link{ReturnCurves-package}}. 
#' 
#' @docType methods
#' 
#' @rdname runshiny
#' 
#' @aliases runShiny
#'
#' @export
#' 
runShiny <- function(){
  appDir <- system.file("shiny-examples", "shinyapp", package = "ReturnCurves")
  if(appDir == ""){
    stop("Unable to find the directory for the shiny app example.\n Try re-installing the package,",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}