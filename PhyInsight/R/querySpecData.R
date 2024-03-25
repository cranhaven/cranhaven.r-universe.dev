#' Query Specimen Data
#'
#' Get specimen and DNA sequence data from the Bold Systems database returned as a dataframe.
#'
#' @import bold
#'
#' @param taxonName  A character string that is the scientific name of a chosen taxon.
#'
#' @return A dataframe where each column pertains to a variable and each row an individual specimen.
#' @export
#'
#' @examples # generate a specimen data frame using the taxon 'Antheraea polyphemus'
#' specdf_Anth <- querySpecData("Antheraea polyphemus")[1:10,]
querySpecData <- function(taxonName){

  specimen_dataframe <- bold::bold_seqspec(taxon = taxonName)

  specimen_dataframe <- do.call(rbind, specimen_dataframe)

  specimen_dataframe <- as.data.frame(specimen_dataframe)

  specimen_dataframe <- t(specimen_dataframe)

  specimen_dataframe <- as.data.frame(specimen_dataframe)

  specimen_dataframe <- specimen_dataframe[nchar(specimen_dataframe$nucleotides) > 0,]

  row.names(specimen_dataframe) <- 1:length(specimen_dataframe[,1])

  return(specimen_dataframe)

}
