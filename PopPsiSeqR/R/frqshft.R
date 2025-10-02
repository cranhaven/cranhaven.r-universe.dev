
#' Frequency Shift Calculator
#'
#' This function accepts a GRanges object containing allele frequencies from two parental populations and an offspring population. It then polarizes each variant site and calculates how the offspring has shifted from equilibrium.
#'
#' @section polarization of alleles:
#' At each variant site, either the selected parent or the backcrossed parent might have the alternate allele at a higher frequency than the other (sites in which they have the same allele frequency are not informative and are assumed to have been filtered out). To regularize the data, each site was independently polarized, which is to say, the alternate and reference alleles were reassigned ad hoc to make the selected parent population have the higher frequency.
#' @section putting the offspring in context:
#' At each site, the offspring's allele frequency is compared to the hypothetical equilibrium frequency expected by simply averaging the parents' frequencies. This is reported as the mean_oriented_shift; also reported is the distance to fixation in each direction (max_oriented_shift, min_oriented_shift), and the difference between parental allele frequencies (AF_difference)
#'
#' @param freqbed_in in goes the file containing the grouped frequency measurements (extended bed format)
#'
#' @return per-site PopPsiSeq frequency shifts
#' @export
#'
#' @examples
#'\donttest{
#' merged_frequencies.filename <- system.file("extdata",
#' "merged_frequencies.example_data.tbl", package = "PopPsiSeqR")
#' frequencies.bg <- import.freqtbl(merged_frequencies.filename)
#' frequency_shifts.bg <- freqShifter(frequencies.bg)
#' }
freqShifter <- function(freqbed_in) {

  freqCompare.bg <- freqbed_in
  freqCompare.bg$backcrossed_parent_introg_deltaF <- (freqCompare.bg$offspring_alt_af - freqCompare.bg$backcrossed_parent_alt_af) * sign(freqCompare.bg$backcrossed_parent_alt_af-freqCompare.bg$selected_parent_alt_af)
  freqCompare.bg$selected_parent_depletion_deltaF <- (freqCompare.bg$offspring_alt_af - freqCompare.bg$selected_parent_alt_af) * sign(freqCompare.bg$backcrossed_parent_alt_af-freqCompare.bg$selected_parent_alt_af)

  freqCompare.bg$central <-  (freqCompare.bg$selected_parent_alt_af + freqCompare.bg$backcrossed_parent_alt_af)/2
  freqCompare.bg$mean_oriented_shift <- sign(freqCompare.bg$central - freqCompare.bg$selected_parent_alt_af)*(freqCompare.bg$central - freqCompare.bg$offspring_alt_af)

  freqCompare.bg <- freqCompare.bg %>% rtracklayer::as.data.frame() %>% dplyr::mutate(max_oriented_shift = dplyr::case_when(.data$selected_parent_alt_af < .data$backcrossed_parent_alt_af ~ central, T ~ 1-central )) %>% dplyr::mutate( min_oriented_shift = - (1-.data$max_oriented_shift) ) %>% dplyr::mutate(AF_difference = abs(.data$selected_parent_alt_af - .data$backcrossed_parent_alt_af) ) %>% GenomicRanges::GRanges()

  return(freqCompare.bg)

}

