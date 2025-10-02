#' Load Merged Allele Frequency Table
#'
#' This function accepts as input a path to a BED file containing allele frequency and returns a GRanges object ready for the freqShifter function.
#'
#' @section input format:
#'
#' This function accepts as input a path to a file in an extended BED6+ format; specifically,
#'
#'
#' `chrom start end name  score strand  reference_allele  alternate_allele  selected_parent_count selected_parent_allele_frequency  backcrossed_parent_count  backcrossed_parent_allele_frequency offspring_count offspring_allele_frequency`
#'
#' eg,
#'
#'  `chr2L	8517	8518	0	0	+	G	A	8	0	16	0.25	8	0.25`
#'
#' Some of these fields (name, score, strand, reference_allele, alternate_allele, selected_parent_count, backcrossed_parent_count, offspring_count) are required as placeholders but not used in the current PopPsiSeq algorithm
#' This format is the output of joining and filtering the output of vcftools' --freq output; see vignette for details
#'
#' @param freqtbl_filename file containing the allele frequencies as an extended BED6+ file
#'
#' @return frequency table as bedgraph
#' @export
#' @examples
#'\donttest{
#' merged_frequencies.filename <- system.file("extdata",
#' "merged_frequencies.example_data.tbl", package = "PopPsiSeqR")
#' frequencies.bg <- import.freqtbl(merged_frequencies.filename)
#'}
import.freqtbl <- function(freqtbl_filename) {
  freqTbl.bg <- rtracklayer::import.bedGraph(freqtbl_filename)
  names(S4Vectors::mcols(freqTbl.bg)) <-  c("score", "name", "blup", "ref", "alt", "selected_parent_count", "selected_parent_alt_af", "backcrossed_parent_count", "backcrossed_parent_alt_af","offspring_count", "offspring_alt_af")

  freqTbl.bg <- freqTbl.bg %>% rtracklayer::as.data.frame() %>% dplyr::filter( .data$selected_parent_alt_af != .data$backcrossed_parent_alt_af) %>% dplyr::select(-c("blup")) %>% GenomicRanges::GRanges()
  return(freqTbl.bg)
}




#' Save the shifted frequencies
#'
#'
#'
#'
#' @param frequency_shifts table of allele frequency shifts, as output by freqShifter()
#' @param output_file path to savefile
#' @return nothing
#' @export
#' @examples
#'\donttest{
#' merged_frequencies.filename <- system.file("extdata",
#' "merged_frequencies.example_data.tbl", package = "PopPsiSeqR")
#' frequencies.bg <- import.freqtbl(merged_frequencies.filename)
#' frequency_shifts.bg <- freqShifter(frequencies.bg)
#' export.freqshft(frequency_shifts.bg , tempfile())
#'}
export.freqshft <- function( frequency_shifts, output_file) {
  #options(scipen=999)
  withr::local_options(scipen=999)
  frequency_shifts.df <- frequency_shifts %>% rtracklayer::as.data.frame() %>% dplyr::select(c("seqnames","start","end","name","score","strand","ref","alt","selected_parent_count","selected_parent_alt_af","backcrossed_parent_count","backcrossed_parent_alt_af","offspring_count","offspring_alt_af","backcrossed_parent_introg_deltaF","selected_parent_depletion_deltaF","mean_oriented_shift", "max_oriented_shift", "min_oriented_shift", "AF_difference"))
  utils::write.table(frequency_shifts.df, file=output_file, quote=FALSE, sep="\t", row.names=FALSE)
  invisible(NULL)
}


#' Load Smoothed Frequency Shift
#'
#'
#'
#'
#' @param filename file containing the allele frequencies as an extended BED6+ file; see vignette for formatting
#' @param selected_parent name of the First Parent (that which is Selected for)
#' @param backcrossed_parent name of the Second Parent (that which is Backcrossed to)
#' @return loaded data as a bedgraph
#' @export
#' @examples
#'\donttest{
#' windowed_shifts.filename <- system.file("extdata",
#' "windowed_shifts.example_data.bed", package = "PopPsiSeqR")
#' windowed_shifts.bg <- import.smvshift(windowed_shifts.filename)
#' }
import.smvshift <- function(filename, selected_parent = "sim", backcrossed_parent= "sec"){
    windowedFreqShift.bg <- rtracklayer::import.bedGraph(filename)


    enhancement_col <-  paste("sum_",selected_parent,"_deltaF", sep = "")
    depletion_col <- paste("sum_",backcrossed_parent,"_deltaF", sep="")
    tared_col  <- paste("sum_",selected_parent,"ward_AFshift", sep="")

    max_col  <- paste("max_",selected_parent,"ward_AFshift", sep="")
    min_col  <- paste("min_",selected_parent,"ward_AFshift", sep="")
    diff_col  <- paste(selected_parent,"_",backcrossed_parent,"_difference", sep="")


    names(S4Vectors::mcols(windowedFreqShift.bg)) <-  c("name", enhancement_col, depletion_col, tared_col, max_col, min_col, diff_col,"num_snp")

    S4Vectors::mcols(windowedFreqShift.bg)[[enhancement_col]] <- as.numeric(S4Vectors::mcols(windowedFreqShift.bg)[[enhancement_col]])
    S4Vectors::mcols(windowedFreqShift.bg)[[depletion_col]] <- as.numeric(S4Vectors::mcols(windowedFreqShift.bg)[[depletion_col]])
    S4Vectors::mcols(windowedFreqShift.bg)[[tared_col]] <- as.numeric(S4Vectors::mcols(windowedFreqShift.bg)[[tared_col]])

    S4Vectors::mcols(windowedFreqShift.bg)[[max_col]] <- as.numeric(S4Vectors::mcols(windowedFreqShift.bg)[[max_col]])
    S4Vectors::mcols(windowedFreqShift.bg)[[min_col]] <- as.numeric(S4Vectors::mcols(windowedFreqShift.bg)[[min_col]])
    S4Vectors::mcols(windowedFreqShift.bg)[[diff_col]] <- as.numeric(S4Vectors::mcols(windowedFreqShift.bg)[[diff_col]])

    S4Vectors::mcols(windowedFreqShift.bg)[[paste("avg_",selected_parent,"_deltaF", sep = "")]] <- S4Vectors::mcols(windowedFreqShift.bg)[[enhancement_col]]/windowedFreqShift.bg$num_snp
    S4Vectors::mcols(windowedFreqShift.bg)[[paste("avg_",backcrossed_parent,"_deltaF", sep = "")]] <- S4Vectors::mcols(windowedFreqShift.bg)[[depletion_col]]/windowedFreqShift.bg$num_snp

    S4Vectors::mcols(windowedFreqShift.bg)[[paste("avg_",selected_parent,"ward_AFshift", sep = "")]] <- S4Vectors::mcols(windowedFreqShift.bg)[[tared_col]]/windowedFreqShift.bg$num_snp
    S4Vectors::mcols(windowedFreqShift.bg)[[paste("max_",selected_parent,"ward_AFshift", sep = "")]] <- S4Vectors::mcols(windowedFreqShift.bg)[[max_col]]/windowedFreqShift.bg$num_snp
    S4Vectors::mcols(windowedFreqShift.bg)[[paste("min_",selected_parent,"ward_AFshift", sep = "")]] <- S4Vectors::mcols(windowedFreqShift.bg)[[min_col]]/windowedFreqShift.bg$num_snp

    S4Vectors::mcols(windowedFreqShift.bg)[[paste(selected_parent,"_",backcrossed_parent,"_difference", sep = "")]] <- S4Vectors::mcols(windowedFreqShift.bg)[[diff_col]]/windowedFreqShift.bg$num_snp

    windowedFreqShift.bg$win <- seq(1,nrow(S4Vectors::mcols(windowedFreqShift.bg)))

    return(windowedFreqShift.bg)
}




