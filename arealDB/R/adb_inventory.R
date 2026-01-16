#' Load the inventory of the currently active areal database
#'
#' @param type [`character(1)`][character]\cr the inventory sub-table to load,
#'   either \code{"dataseries"}, \code{"tables"}, \code{"geometries"} or
#'   \code{"references"}.
#' @return returns the table selected in \code{type}
#' @importFrom checkmate assertChoice
#' @export

adb_inventory <- function(type = NULL){

  assertChoice(x = type, choices = c("dataseries", "tables", "geometries", "references"))

  temp <- readRDS(paste0(getOption(x = "adb_path"), "/_meta/inventory.rds"))

  if(!is.null(type)){
    out <- temp[[type]]
  } else {
    out <- temp
  }

  return(out)
}