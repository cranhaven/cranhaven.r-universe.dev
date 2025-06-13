#' Returns description for a table
#'
#' @param x Name of the table to get the description
#'
#' @return The description for a table based on the
#' description provided by Coursera in the data exports
#' @examples
#' crsra_tabledesc("assessments")
#' @export
crsra_tabledesc <- function(x){
    table_names = names(crsra::tabdesc)
    x = match.arg(x, choices = table_names)
    crsra::tabdesc[x]
}







