#' Runs on package attach
#'
#' The package startup message is defined here. It shall give a clue on how to
#' start the owosGUI and which suggests might want to be installed.
#'
#' @keywords internal
#' @return None
.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    packageStartupMessage("*** Welcome to the EventDetectGUI package ***\n\n
                          There is only one command that you will need to use the GUI:\n
                          runGUI()\n\n
                          ")
}
