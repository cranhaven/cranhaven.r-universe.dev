#' Boostrapping DNA sequences
#'
#' @description
#' `boot_dna()` generates random bootstrap alignments for confidence interval estimation
#' using [confidence_intervals]. Thus, it is meant to be an internal function of this package.
#'
#' @param dna an object of class [DNAbin][ape::DNAbin].
#' @param block integer. Number of columns to be resampled together. Default to 1.
#'
#' @return a [DNAbin][ape::DNAbin] object.
#'
#' @author Pedro S. Bittencourt
#'
#' @examples
#' boot <- boot_dna(geophagus)
#'
#' @keywords internal
#' @export
boot_dna <- function(dna, block = 1) {
  if (!methods::is(dna, "DNAbin")) {
    cli::cli_abort(c("Input data must have class {.cls DNAbin}.",
      "x" = "You've supplied an input of class {.cls {class(dna)}}.",
      "i" = "Try importing your input file using {.pkg ape} {.fn read.dna}."
    ))
  }

  if (!is.matrix(dna)) {
    mat <- ape::as.matrix.DNAbin(dna)
  }

  # from ape::boot.phylo()
  if (block > 1) {
    a <- seq(from = 1, to = ncol(mat) - 1, by = block)

    b <- seq(from = block, to = ncol(mat), by = block)

    y <- mapply(":", a, b, SIMPLIFY = FALSE)

    iboot <- sample(y, replace = TRUE) |> unlist()
  } else {
    iboot <- sample.int(ncol(mat), replace = TRUE)
  }

  matb <- mat[, iboot]

  return(matb)
}
