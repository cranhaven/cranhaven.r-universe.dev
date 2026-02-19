# Written by Rob Young at the University of Guelph in Ontario Canada, May, 2024
# ********************************************Main program section*************
#Roxygen2 Documentation:
#' @export
#'
#' @title Launch DBTCShiny
#'
#' @author Robert G. Young
#'
#' @description
#' This function launches the DBTCShiny Application
#'
#' @details
#' This function launches a DBTCShiny Application which allows a user to run the 'DBTC'
#' functions and process high throughput sequencing data.
#'
#' @examples
#' if(interactive()){
#' launchDBTCShiny()
#' }
#'
#' @param verbose If set to TRUE then there will be output to the R console, if
#' FALSE then this reporting data is suppressed (Default TRUE).
#'
#' @returns
#' There are no values or files returned from this function
#'
#' @references
#' <https://github.com/rgyoung6/DBTC>
#' Young, R. G., Hanner, R. H. (Submitted October 2023). Metabarcoding analysis
#' using Dada-BLAST-Taxon Assign-Condense shiny Application (DBTCShiny). Biodiversity Data Journal.
#'
#' @note
#' This is a wrapper function which launches the DBTCShiny package as a 'Shiny'
#' application in the systems default browser program
#'
#' @seealso
#' dada_implement()
#' combine_dada_output()
#' make_BLAST_DB()
#' seq_BLAST()
#' taxon_assign()
#' combine_assign_output()
#' reduce_taxa()
#' combine_reduced_output()

# wrapper for shiny::shinyApp()
launchDBTCShiny <- function(verbose = TRUE) {

  #Get the initial working directory
  start_wd <- getwd()
  on.exit(setwd(start_wd))
  shiny::shinyApp(ui = shinyAppUI, server = function(input, output, session) {shinyAppServer(input, output, session,verbose)}, options = list(launch.browser = TRUE))
}
