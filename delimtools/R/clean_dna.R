#' Removes Gaps, Ambiguities and Missing Data from DNA Sequences
#'
#' @description
#' `clean_dna()` removes all character not a valid ACTG base from a [DNAbin][ape::DNAbin]
#' object.
#'
#' @param dna an object of class [DNAbin][ape::DNAbin].
#' @param verbose logical. Returns a warning if any sequence contains non ACTG bases.
#'
#' @details
#' `clean_dna()` detects and removes any non ACTG bases from alignment. This includes:
#' "N", "-", "?", "R", "Y", etc. If `verbose = TRUE`, returns a warning if these characters
#' are inside the sequences, i.e, are not alignment padding chars at the ends.
#'
#' @return
#' an object of class [DNAbin][ape::DNAbin].
#'
#' @author
#' Rupert A. Collins
#'
#' @examples
#' geo_clean <- clean_dna(geophagus)
#'
#' @export
clean_dna <- function(dna, verbose = TRUE) {
  # convert to a list
  dat <- as.list(dna)
  # convert to character
  datc <- as.character(dat)
  # make a warning
  # collapse text into vector
  cdat <- lapply(datc, function(x) paste(x, collapse = ""))
  # find all internal missing data
  res <- lapply(cdat, function(x) stringr::str_detect(string = x, pattern = "[actg][^actg]+[actg]"))
  # get names
  errs <- names(which(res != FALSE))
  # if else warning
  if (length(errs >= 1) & verbose == TRUE) {
    cli::cli_warn(c("{cli::col_yellow({cli::symbol$warning})} You have missing data {.val ('N','-' '?')}
    or ambiguity inside your sequence, i.e. not padding the ends, and this may have unintended consequences later, as they have now been removed!",
      "i" = "The names of the samples are bellow.",
      stringr::str_flatten_comma(errs)
    ))
  }

  # get positions of all non bases
  inds <- lapply(datc, function(x) grep("a|c|t|g", x, ignore.case = TRUE))
  # match positions and remove
  dlimp <- mapply(function(x, y) x[y], x = datc, y = inds, SIMPLIFY = FALSE)
  # convert to dnabin
  dbin <- ape::as.DNAbin(dlimp)
  return(as.list(dbin))
}
