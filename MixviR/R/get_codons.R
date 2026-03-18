#' Get Codons From Gene Sequence
#'
#' Group a gene sequence into codons (triplets) that can be used for subsequent translation. If any elements of the character vector have length >1 (insertions in ALT column of VCF), they are trimmed to the first base. Used by id.snps, id.indels, and create_ref functions.
#' @param gene.seq Character vector containing gene sequence. The length of this vector should be equal to the length of sequence.
#' @param rev Logical indicating whether the reverse complement of the gene.seq should be used.
#' @keywords codons
#' @return A character vector containing translated amino acids associated with each nucleotide position in the input vector.  
#' @importFrom magrittr %>%
#' @export
#' @examples
#' get_codons(gene.seq = c("A","U","G","C","A","T","T","T","A","C","A","G","T","A","A"))

get_codons <- function(gene.seq, rev = FALSE) {
  if (rev == TRUE) {
    gene.seq <- paste0(gene.seq, collapse = "")
    gene.seq <- Biostrings::DNAStringSet(gene.seq)
    gene.seq <- Biostrings::reverseComplement(gene.seq)
    gene.seq <- stringr::str_split(gene.seq, "") %>% unlist()
  }
  
  CodonStartPositions <- 0:(length(gene.seq)/3-1) * 3 + 1
  CodonEndPositions <- 1:(length(gene.seq)/3) * 3
  gene.seq <- paste0(gene.seq, collapse = "")
  codons <- stringr::str_sub(string = gene.seq, start = CodonStartPositions, end = CodonEndPositions)
  
  if (rev == FALSE) {
    codons <- rep(codons, each = 3)
    return(codons)
  } else {
    codons <- rep(codons, each = 3)
    codons <- rev(codons)
    return(codons)
  }
}
