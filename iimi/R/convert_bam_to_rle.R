#' @title convert_bam_to_rle
#'
#' @export
#' @importFrom Rsamtools BamFile
#' @importFrom GenomicAlignments readGAlignments
#' @importFrom GenomicAlignments readGAlignmentPairs
#' @importFrom IRanges coverage
#' @importFrom stats setNames
#' @importFrom Rdpack reprompt
#'
#'
#' @examples
#' \dontrun{
#' ## Please change the path to your folder where you
#' ## store sorted and indexed BAM files of mapped samples
#'
#' rles <- convert_bam_to_rle("path/to/bam/file")
#' }
#'
#' @description Converts one or more indexed and sorted BAM files (ending in
#'     `*.sorted.bam` and `*.bai`) into a run-length encodings (RLEs) list.
#'
#' @param bam_file path to BAM file(s).
#' @param paired Indicate if the sequencing paired is single-end or paired-end
#'     reads. `TRUE` if paired-end. `FALSE` if single-end.
#'
#' @return A list of coverage profile(s) in RLE format with one or more samples.

convert_bam_to_rle <- function(bam_file, paired = FALSE) {
  covs <- setNames(
    lapply(bam_file,
           function(x) {
             bams <- BamFile(x)

             if (!paired) {
               xread <- readGAlignments(bams)
             } else if (paired) {
               xread <- readGAlignmentPairs(bams)
             }

             cov <- coverage(xread)

             return(cov)
           }),
    sub(
      pattern = "(.*)\\.sorted.*$",
      replacement = "\\1",
      basename(bam_file)
    )
  )

  covs_only_mapped <- lapply(covs, function(x) {
    x[sapply(x, function(y) {
      !all(y == 0)
    })]
  })

  covs_only_mapped
}
