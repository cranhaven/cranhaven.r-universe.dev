#' This class is a generic container for RTIGER analysis
#'
#' @slot matobs Nested lists. the first level is a list of samples. For each sample there are 5 matrices that contains the allele counts for each position.
#' @slot params a list with the parameters after training.
#' @slot info List with phenotipic data of the samples.
#' @slot Viterbi List of chromosomes with the viterbi path per sample.
#' @slot Probabilities Computed probabilites for the EM algorithm.
#' @slot num.iter Number of iterations needed to stop the EM algorithm.
#' @rdname RTIGERDataSet
#' @exportClass RTIGER

.RTIGER = setClass("RTIGER",
                   representation = representation(
                     # Grangesobs = "list",
                     matobs = "list",
                     params = "list",
                     # FilteringThreshold = "list",
                     info = "list",
                     # Model = "list",
                     Viterbi = "list",
                     Probabilities = "list",
                     num.iter = "numeric"
                   ))

#' Prints description of RTIGER object
#'
#' @keywords internal
#' @noRd
#'

setMethod(f = "show", signature = c("RTIGER"), function(object) {

  cat("An object of class \"", class(object), "\" \n", sep = "")
  cat(" Number of samples:  ", object@info$sample_nr, "\n", sep = "")

  cat(" Number of chromosomes per sample: ", object@info$part_nr, "\n",
      sep = "")
  cat(" Chromosome names: ", paste(as.character(object@info$part_names), collapse = " "), "\n", sep = "")
  cat(" Rigidity value: ", object@params$rigidity, "\n", sep = "")
  # cat(" Number of observations per chromosome: ", paste(as.character(object@info$part_lengths), collapse = " "), "\n", sep = "")

})


