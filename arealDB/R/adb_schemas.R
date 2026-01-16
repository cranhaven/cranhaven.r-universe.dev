#' Load the schemas of the currently active areal database
#'
#' @param pattern [`character(1)`][character]\cr an optional regular expression.
#'   Only schema names which match the regular expression will be processed.
#' @return returns a list of schema descriptions
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @export

adb_schemas <- function(pattern = NULL){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  schemas <- list.files(path = paste0(intPaths, "/_meta/schemas/"), pattern = pattern, full.names = TRUE)

  schemaNames <- map(.x = seq_along(schemas), .f = function(ix){

    str_split(tail(str_split(string = schemas[ix], pattern = "/")[[1]], 1), "[.]")[[1]][1]

  })

  schemas <- map(.x = schemas, function(ix){
    readRDS(file = ix)
  })
  names(schemas) <- schemaNames

  return(schemas)

}