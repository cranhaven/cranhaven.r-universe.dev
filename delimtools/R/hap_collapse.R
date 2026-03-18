#' Removes Duplicated Sequences from Alignment
#'
#' @description
#' `hap_collapse()` collapses haplotypes from a [DNAbin][ape::DNAbin] object,
#' keeping unique haplotypes only.
#'
#' @param dna A [DNAbin][ape::DNAbin] object.
#' @param clean logical. Whether to remove or not remove non ACTG bases from alignment.
#' @param collapseSubstrings logical. Whether to collapse or not collapse shorter but identical sequences.
#' @param verbose logical. Returns a warning if any sequence contains non ACTG bases.
#' See [clean_dna] for details.

#'
#' @details
#' `hap_collapse()` collapses a [DNAbin][ape::DNAbin] object, keeping unique
#' haplotypes only. If `clean = TRUE`, the function will call [clean_dna] to remove
#' any non ACTG bases from alignment prior to collapsing haplotypes. If `clean = FALSE`,
#' the function will treat data as it is, and will not remove any bases. If
#' `collapseSubstrings = TRUE`, the function will consider shorter but identical
#' sequences as the same haplotype and collapse them, returning the longest
#' sequence. If `collapseSubstrings = FALSE`, the function will consider
#' shorter but identical sequences as different haplotypes and will keep them.
#'
#'
#' @return
#' A [DNAbin][ape::DNAbin] object.
#'
#' @author
#' Rupert A. Collins
#'
#' @examples
#'
#' # collapse into unique haplotypes, including shorter sequences
#' hap_collapse(geophagus, clean = TRUE, collapseSubstrings = TRUE)
#'
#' # collapse into unique haplotypes keeping shorter sequences
#' hap_collapse(geophagus, clean = TRUE, collapseSubstrings = FALSE)
#'
#' @export
hap_collapse <- function(dna, clean = TRUE, collapseSubstrings = TRUE, verbose = TRUE) {
  if (clean == TRUE) {
    data <- delimtools::clean_dna(dna, verbose = verbose)
  } else {
    data <- as.list(dna)
  }
  if (collapseSubstrings == TRUE) {
    # sort by length
    data.ord <- data[order(mapply(length, data, SIMPLIFY = TRUE, USE.NAMES = FALSE), decreasing = TRUE)]
    # make a copy to index later
    data.ord.copy <- data.ord
    # collapse into a strings
    data.ord <- mapply(FUN = function(x) paste(x, collapse = ""), as.character(data.ord), SIMPLIFY = TRUE, USE.NAMES = FALSE)
    # get the indices of the first match to each seq (the longest)
    ind <- unique(mapply(FUN = function(x) which(stringr::str_detect(string = data.ord, pattern = x) == TRUE)[1], data.ord, SIMPLIFY = TRUE, USE.NAMES = FALSE))
    # get names of unique haplotypes
    names_haps <- names(data.ord.copy)[ind]
    # drop sequences from fasta
    dna_haps <- delimtools::drop_sequences(data, names_haps, drop = FALSE)
  } else {
    # make a copy to index later
    data.copy <- data
    # collapse into a strings
    data.char <- mapply(FUN = function(x) paste(x, collapse = ""), as.character(data), SIMPLIFY = TRUE, USE.NAMES = FALSE)
    # get duplicates
    dups <- duplicated(data)
    # get names of duplicated haplotypes
    names_dups <- names(data.copy)[dups]
    # drop sequences from fasta
    dna_haps <- delimtools::drop_sequences(data, names_dups, drop = TRUE)
  }
  return(dna_haps)
}
