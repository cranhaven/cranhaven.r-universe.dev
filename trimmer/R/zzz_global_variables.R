#' Fix til at undgå R CMD check notes for "no visible binding for global variable"
#'
#' Dette script gør det muligt at referere til kolonner i data frames ved hjælp
#' af Non Standard Evaluation (NSE) i databehandlingspakker som data.table og
#' dplyr, uden at dette medfører R CMD check notes angående "no visible binding
#' for global variable". Navnene på de variable, der refereres til ved hjælp af
#' NSE, skal blot angives i en vektor til funktionen globalVariables() nedenfor.
#'
#' Dette er den anbefalede løsning fra CRAN.
#'
#' @importFrom utils globalVariables
#'
fix_undefined_global_vars <- function() {
  if (getRversion() >= "2.15.1")
    globalVariables(
       c(# Indsæt variabelnavne nedenfor
         "..cols", 
         "..on_cols", 
         "i1", 
         "size"
      )
    )
}

fix_undefined_global_vars()
