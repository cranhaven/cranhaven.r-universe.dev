#' Convert design to JSON
#' 
#' Convert a design list to JSON notation for archiving (e.g. in scienceverse)
#'
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' @param filename option name of file to save the json to
#' @param digits number of digits to save
#' @param pretty whether to print condensed or readable
#' @param ... other options to send to jsonlite::toJSON
#'
#' @return a JSON string
#' @export
#'
#' @examples
#' des <- check_design(2,2)
#' json_design(des)
#' json_design(des, pretty = TRUE)
json_design <- function(design, filename = NULL, 
                        digits = 8, pretty = FALSE, ...) {
  valid_design <-  check_design(design = design, plot = FALSE)
  valid_design$params <- NULL
  
  j <- jsonlite::toJSON(valid_design, auto_unbox = TRUE, digits = digits, pretty = pretty, ...)
  
  if (!is.null(filename)) {
    # fix filename
    if (!length(grep("\\.json$", filename))) {
      # add .json extension if not already specified
      filename <- paste0(filename, ".json")
    }
  
    writeLines(j, filename)
  }
  
  j
}
