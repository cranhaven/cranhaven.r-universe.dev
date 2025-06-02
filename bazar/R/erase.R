#' @title 
#' Delete objects
#' 
#' @description
#' The function \code{erase} deletes all objects 
#' that live in the calling environment. 
#' 
#' @param ask
#' logical. If \code{TRUE} (the default), a confirmation is interactively 
#' asked to the user. 
#' 
#' @section 
#' Warning: use this function with care!
#'    
#' @export
#' 
erase <- 
function(ask = TRUE)
{
  e <- parent.frame()
  if (ask) {
    arg <- "*"
    while (arg %nin% c("Y", "n")) {
      arg <- readline("Delete all objects from the current environment? [Y/n] ")
    }
  } else {
    arg <- "Y"
  }
  
  if (arg == "Y") {
    rm(list=ls(all.names = TRUE, envir = e), envir = e)
  }
  invisible()
}
