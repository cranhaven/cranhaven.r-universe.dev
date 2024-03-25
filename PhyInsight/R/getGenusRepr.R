#' Get Genus Representatives
#'
#' Get one observation/row per genus from a specimen dataframe.
#'
#' @param specimen_dataframe A specimen dataframe.
#'
#' @return A dataframe where each column pertains to a variable and each row an individual specimen.
#' @export
#'
#' @examples # query data then extract one observation for each of the genera available
#' specdata <- querySpecData("Alouatta")
#'
#' specdata <- getGenusRepr(specdata)
getGenusRepr <- function(specimen_dataframe){

  genus_split <- split(specimen_dataframe, specimen_dataframe$genus_name)

  genus_sample <- list()

  for(i in 1:length(genus_split)){

    genus_sample[[i]] <- genus_split[[i]][1,]

  }

  genus_sample <- do.call(rbind, genus_sample)

  return(genus_sample)

}
