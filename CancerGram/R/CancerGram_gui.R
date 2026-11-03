#' CancerGram Graphical User Interface
#'
#' Launches graphical user interface that predicts presence of 
#' anticancer peptides.
#'
#' @importFrom shiny runApp
#' @seealso \code{\link[shiny]{runApp}}
#' @return No return value, called for side effects.
#' @section Warning : Any ad-blocking software may cause malfunctions.
#' @export CancerGram_gui
CancerGram_gui <- function() {
  require_CancerGramModel()
  runApp(system.file("CancerGram", package = "CancerGram"))
}