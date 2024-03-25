#' Get Species Representatives
#'
#' Get one observation/row per species from a specimen dataframe.
#'
#' @param specimen_dataframe A specimen dataframe.
#'
#' @return A dataframe where each column pertains to a variable and each row an individual specimen.
#' @export
#'
#' @examples # query data then extract one observation for each of the species available
#' specdata <- PhyInsight::querySpecData("Alouatta")
#'
#' specdata <- getSpeciesRepr(specdata)
getSpeciesRepr <- function(specimen_dataframe){

  species_split <- split(specimen_dataframe, specimen_dataframe$species_name)

  species_sample <- list()

  for(i in 1:length(species_split)){

    species_sample[[i]] <- species_split[[i]][1,]

  }

  species_sample <- do.call(rbind, species_sample)

  return(species_sample)

}
