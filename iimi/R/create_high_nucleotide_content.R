#' @title create_high_nucleotide_content
#'
#' @export
#' @importFrom R.utils seqToIntervals
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @importFrom Biostrings letterFrequencyInSlidingView
#'
#' @examples
#' \dontrun{high_nucleotides_regions <- create_high_nucleotide_content()}
#'
#' @description Creates a data frame of the start and end positions of the
#'     regions_a that are considered high in A% and GC%.
#' @param gc The threshold for GC content. It is the proportion of G and C
#'     nucleotides in a sliding window. Default is 0.6.
#' @param a The threshold for A nucleotide. It is the proportion of A nucleotide
#'     in a sliding window. Default is 0.45.
#' @param window The sliding window size of your choice. Default is 75.
#' @param virus_info A DNAStringSet of virus segments. The format should be similar to `virus_segments`.
#'
#' @return A data frame of the start and end positions of the regions_a that are
#'     considered high in A% and GC%.


create_high_nucleotide_content <-
  function(gc = 0.6,
           a = 0.45,
           window = 75,
           virus_info) {
    unreliable_regions = data.frame(matrix(NA, ncol = 4, nrow = 0))

    for (ii in 1:length(virus_info)) {
      segment <- names(virus_info)[ii]
      seq <- virus_info[[segment]]
      gc_seq = rowSums(letterFrequencyInSlidingView(seq, window, c("G", "C"))) /
        window
      a_seq = rowSums(letterFrequencyInSlidingView(seq, window, c("A"))) /
        window


      regions_gc <- rep(0, nchar(as.character(seq)))
      for (jj in 1:length(gc_seq)) {
        if (gc_seq[jj] > gc) {
          regions_gc[jj:(jj + window - 1)] <- 1
        }
      }
      regions_gc <- which(regions_gc == 1)

      region_gc_index <- seqToIntervals(regions_gc)

      for (nn in 1:nrow(region_gc_index)) {
        if (nrow(region_gc_index) != 0) {
          unreliable_regions <- rbind(
            unreliable_regions,
            c(
              region_gc_index[nn, 1],
              region_gc_index[nn, 2],
              segment,
              "CG% > 60%"
            )
          )
        }
      }

      regions_a <- rep(0, nchar(as.character(seq)))
      for (jj in 1:length(a_seq)) {
        if (a_seq[jj] > a) {
          regions_a[jj:(jj + window - 1)] <- 1
        }
      }
      regions_a <- which(regions_a == 1)

      region_a_index <- seqToIntervals(regions_a)

      for (nn in 1:nrow(region_a_index)) {
        if (nrow(region_a_index) != 0) {
          unreliable_regions <- rbind(
            unreliable_regions,
            c(
              region_a_index[nn, 1],
              region_a_index[nn, 2],
              segment,
              "A% > 45%"
            )
          )
        }
      }

    }

    colnames(unreliable_regions) = c("Start", "End", "Virus segment", "Categories")

    unreliable_regions$Start <- as.numeric(unreliable_regions$Start)
    unreliable_regions$End <- as.numeric(unreliable_regions$End)
    unreliable_regions$`Virus segment` <- as.factor(unreliable_regions$`Virus segment`)
    unreliable_regions$Categories <- as.factor(unreliable_regions$Categories)

    unreliable_regions

  }
