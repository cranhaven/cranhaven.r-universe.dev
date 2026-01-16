#' Load the translation tables of the currently active areal database
#'
#' @param type [`character(1)`][character]\cr the type of ontology for which to
#'   load translation tables, either \code{"ontology"} to get the thematic
#'   concepts, or \code{"gazetteer"} to get the territories.
#' @param dataseries [`character(1)`][character]\cr the name of a dataseries as
#'   registered in \code{\link{regDataseries}}.
#' @return returns the selected translation table
#' @importFrom checkmate assertChoice
#' @importFrom stringr str_split
#' @export

adb_translations <- function(type = NULL, dataseries = NULL){

  assertChoice(x = type, choices = c("gazetteer", "ontology"))

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  temp <- readRDS(paste0(intPaths, "/_meta/inventory.rds"))
  inv_dataseries <- temp$dataseries

  assertChoice(x = dataseries, choices = inv_dataseries$name)

  if(type == "gazetteer"){
    tabType <- getOption("gazetteer_path")
  } else {
    tabType <- getOption("ontology_path")
  }
  tabType <- str_split(tabType, "[.]")[[1]][1]

  tables <- list.files(path = tabType, pattern = paste0(dataseries, "*.rds"), full.names = TRUE)
  out <- readRDS(file = tables)

  return(out)

}