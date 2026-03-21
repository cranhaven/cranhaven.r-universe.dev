#' Get unique species in a given genus in dataframe.
#'
#' \code{get_species} returns all species name that correspond to genus name
#' input in a FishNet2 dataframe.
#'
#' This is a function to get the species name of a given genus name. Names are
#' found using the 'ScientificName' column in a FishNet2 dataframe. If "value is
#' only one word, no species name is returned.
#'
#' @export
#' @param df A dataframe in FishNet2 standard format (by using read.csv())
#' @param genus Genus of species
#' @return Vector of unique species values or character(0) if empty

#' @examples
#' get_species(ictaluridae, "Ameirus")
#' get_species(ictaluridae, "Noturus")
#' get_species(louisiana, "Scaphirhynchus")

get_species <- function(df, genus) {

  if(!is.data.frame(df)) {
    stop("Error: Function input is not a dataframe")
  }
  if(!("ScientificName" %in% names(df))){
    stop("Error: Column 'ScientificName' not found.")
  }

  len <- length(df[,"ScientificName"])
  s <- vector(mode="character",length=len)
  d <- df[,"ScientificName"]

  # check if genus name equals first word in ScientificName column
  for(val in 1:len){
    if(strsplit(as.character(d[val])," ")[[1]][1] == genus) {
      # add species name to list
      s[val] <- strsplit(as.character(d[val])," ")[[1]][2]
    }
  }
  # return unique values
  return(unique(s))
}
