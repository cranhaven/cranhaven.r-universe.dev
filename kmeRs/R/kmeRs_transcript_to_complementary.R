#' @title Translate Given K-mers To Complementary Sequences
#'
#' @description
#' The \code{kmeRs_transcript_to_complementary} function transcripts DNA given k-mers to complementary sequences
#'
#' @aliases kmeRs_transcript_to_complementary
#'
#' @param kmers_given vector contains given k-mers
#'
#' @return vector contains complementary sequences
#'
#' @examples
#' # Returns complementary sequence to GATTACA
#'
#' kmeRs_transcript_to_complementary('GATTACA')
#'
#' @export


  kmeRs_transcript_to_complementary <- function(kmers_given){

    new_all <- NULL

  # -- For every kmeRs transcript to complementary sequence --

    for (a in 1:length(kmers_given)){

      kmer <- kmers_given[a]

      new <- NULL

      for (i in 1:nchar(kmer)){

        letter <- substr(kmer, i, i)

        # A -> T
        if (toupper(letter) == "A") new <- paste(new, "T", sep="")

        # T -> A
        if (toupper(letter) == "T") new <- paste(new, "A", sep="")

        # C -> G
        if (toupper(letter) == "C") new <- paste(new, "G", sep="")

        # G -> C
        if (toupper(letter) == "G") new <- paste(new, "C", sep="")

      }

      new_all <- c(new_all, new)

    }

    return(new_all)

 }
