#' Remove Sequences of a DNAbin list object
#'
#' @description
#' `drop_sequences()` removes sequences of a FASTA file by its names.
#'
#' @param dna a [DNAbin][ape::DNAbin] list object.
#' @param identifier a character vector containing sequence names.
#' @param drop Logical. If `TRUE`, sequence names in `identifier` will
#' be dropped from `dna`. If `FALSE`, sequence names absent in `identifier`
#' will be dropped instead.
#'
#' @details
#' `drop_sequences()` relies on exact match between sequence names within
#' a fasta file and `identifier` argument.
#'
#' @return
#' an object of class [DNAbin][ape::DNAbin].
#'
#' @author
#' Pedro S. Bittencourt
#'
#' @examples
#'
#' # Create a vector of sequence names to drop or keep.
#' identifier <- names(geophagus)[1:3]
#'
#' # Remove sequences listed in identifier
#' drop_sequences(geophagus, identifier, drop = TRUE)
#'
#' # Remove sequences not listed in identifier
#' drop_sequences(geophagus, identifier, drop = FALSE)
#'
#' @export
drop_sequences <- function(dna, identifier, drop = TRUE) {
  if (!methods::is(dna, "DNAbin")) {
    cli::cli_abort(c("Input data must have class {.cls DNAbin}.",
      "x" = "You've supplied an input of class {.cls {class(dna)}}.",
      "i" = "Try importing your input file using {.pkg ape} {.fn read.FASTA}."
    ))
  }

  if (!is.list(dna)) {
    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} {.arg dna} is a {typeof(dna)} object. Coercing to a {.cls DNAbin} list object.")

    dna <- ape::as.list.DNAbin(dna)
  }

  if (drop == TRUE) {
    dna <- dna |> purrr::discard_at(identifier)
  } else {
    dna <- dna |> purrr::keep_at(identifier)
  }

  return(dna)
}
