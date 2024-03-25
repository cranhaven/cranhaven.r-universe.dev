#' Generate a DNA Bin
#'
#' Create a DNA bin object using a specimen dataframe.
#'
#' @import ape
#' @import Biostrings
#'
#' @param specimen_dataframe A dataframe with specimen data created using querySpecData()
#'
#' @return A DNA bin object.
#' @export
#'
#' @examples # generate a DNA bin object using the taxon 'Antheraea polyphemus'
#' specdf_Anth <- querySpecData("Antheraea polyphemus")[1:10,]
#'
#' DNABin_Anth <- genDNABin(specdf_Anth)
genDNABin <- function(specimen_dataframe){

  bin_list <- list()

  for(i in 1:length(specimen_dataframe$nucleotides)){
    bin_list[[i]] <- ape::as.DNAbin(Biostrings::DNAString(specimen_dataframe$nucleotides[i]))
  }

  bin <- do.call(c, bin_list)

  return(bin)

}
