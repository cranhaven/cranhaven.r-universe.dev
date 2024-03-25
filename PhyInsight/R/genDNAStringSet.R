#' Generate a DNA String Set
#'
#' Create a DNA string set object using a DNA bin object to write and read a FASTA file.
#'
#' @import ape
#'
#' @param DNABin A DNA bin object.
#'
#' @return A DNA string set
#' @export
#'
#' @examples # generate a DNA string set object using the taxon 'Antheraea polyphemus'
#' specdf_Anth <- querySpecData("Antheraea polyphemus")[1:10,]
#'
#' DNABin_Anth <- genDNABin(specdf_Anth)
#'
#' DNAStringset_Anth <- genDNAStringSet(DNABin_Anth)
genDNAStringSet <- function(DNABin){

  orig_dir <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)
  on.exit(setwd(orig_dir))

  FASTA_fileName <- gsub(" ", "", "temp_DNAString_file.fasta")

  ape::write.FASTA(x = ape::del.gaps(DNABin), file = FASTA_fileName)

  FASTA_strings <- Biostrings::readDNAStringSet(filepath = FASTA_fileName,
                                                format = "fasta"
  )

  return(FASTA_strings)

}
