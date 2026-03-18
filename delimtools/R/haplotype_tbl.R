#' Summarise Haplotypes Down to One Row
#'
#' @description
#' `haplotype_tbl()` returns a [tbl_df][tibble::tbl_df] summarising
#' all unique haplotype frequencies and duplicates into a single row.
#'
#' @param dna an object of class [DNAbin][ape::DNAbin].
#' @param clean logical. Whether to remove or not remove non ACTG bases from alignment.
#' @param collapseSubstrings logical. Whether to collapse or not collapse shorter but identical sequences.
#' @param verbose logical. Returns a warning if any sequence contains non ACTG bases. 
#' See [clean_dna] for details.
#'
#' @details
#' `haplotype_tbl()` uses a combination of [clean_dna] and [hap_collapse] to summarise 
#' haplotypes into a tibble. Each row of the tibble has an unique haplotype, 
#' its frequency and all its collapsed duplicates in a flattened string.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt.
#'
#' @examples
#' 
#' # get haplotype table
#' haplotype_tbl(geophagus) 
#'
#' @export
haplotype_tbl <- function(dna, clean= TRUE, collapseSubstrings= TRUE, verbose= TRUE){
  
  if(clean==TRUE){
    # clean dna
    dat_all_ali <- delimtools::clean_dna(dna, verbose = verbose)
  }
  
  # collapse to haplotypes
  dat_all_haps <- delimtools::hap_collapse(dat_all_ali, clean = FALSE, collapseSubstrings = collapseSubstrings)
  
  # convert to all to character
  dat_haps_char <- lapply(dat_all_haps, function(x) paste(x, collapse=""))
  dat_all_char <- lapply(dat_all_ali, function(x) paste(x, collapse=""))
  dat_collapseds_char <- dat_all_char[which(!names(dat_all_char) %in% names(dat_haps_char))]
  
  # detect strings for all collapseds of each unique haplotype
  seqs_in <- mapply(FUN=function(x) which(stringr::str_detect(string=dat_haps_char, pattern=x)==TRUE), dat_collapseds_char, SIMPLIFY=TRUE, USE.NAMES=FALSE)
  seqs_in <- mapply(function(x) names(dat_haps_char[x]), seqs_in)
  names(seqs_in) <- names(dat_collapseds_char)
  
  # turn into dataframe
  pair_list <- tidyr::unnest(tibble::enframe(seqs_in,name="collapsed", value= "labels"), cols = "labels") |>
    dplyr::distinct(.data$collapsed, .keep_all = TRUE) |>
    dplyr::group_by(labels) |>
    dplyr::reframe(n_seqs= dplyr::n(),
                   collapsed= paste(.data$collapsed, collapse = ", "))
  
  # join
  haps_df <- tibble::tibble(labels= unlist(names(dat_haps_char))) |>
    dplyr::full_join(pair_list, by="labels") |>
    dplyr::mutate(n_seqs= tidyr::replace_na(.data$n_seqs, 1),
                  n_seqs= dplyr::if_else(!is.na(.data$collapsed), .data$n_seqs+1, .data$n_seqs)) |>
    dplyr::arrange(dplyr::desc(.data$n_seqs))
  
  return(haps_df)
}
