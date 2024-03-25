#' Manipulate String Set
#'
#' Orient and align DNA sequences in a DNA string set object.
#'
#' @param DNAStringSet A DNA string set object.
#'
#' @return A DNA string set (that is ready for analysis)
#' @export
#'
#' @examples # generate and manipulate a DNA string set object using the taxon 'Antheraea polyphemus'
#' specdf_Anth <- querySpecData("Antheraea polyphemus")[1:10,]
#'
#' DNABin_Anth <- genDNABin(specdf_Anth)
#'
#' DNAStringset_Anth <- genDNAStringSet(DNABin_Anth)
#'
#' DNAStringSet_Anth_manipulated <- ManipStringSet(DNAStringset_Anth)
ManipStringSet <- function(DNAStringSet){

  FASTA_strings <- DECIPHER::OrientNucleotides(DNAStringSet)

  FASTA_strings <- DECIPHER::AlignSeqs(FASTA_strings)

  return(FASTA_strings)

}
