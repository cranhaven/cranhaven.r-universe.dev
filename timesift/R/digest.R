#' The cross-language digest of a representation
#'
#' The MD5 the contract in `inst/spec/representation.md` defines, and what the fixtures in
#' `inst/spec/fixtures/` are checked against. `timesift.digest_array()` on the Python side is the
#' same function: the same array gives the same string in either language, which is what makes a
#' digest a statement about the representation rather than about the machine.
#'
#' Values are traversed in the array's own order, unit fastest, then bin, then channel; each is
#' formatted to twelve decimal places, joined with a line feed and terminated with one, and the
#' UTF-8 bytes of that are hashed. The line ending is a line feed on every platform. It is written
#' explicitly because `writeLines()` emits CRLF on Windows, which would make a digest depend on
#' the machine that produced it.
#'
#' The array has to be finite. `%.12f` writes an infinity as `Inf` here and as `inf` in Python, and
#' R tells a missing value apart from a not-a-number where Python has one spelling for both, so a
#' digest over such an array would say something about the language rather than about the
#' representation. A representation never holds one: a reading that is not a finite number is
#' refused where the record is read.
#'
#' @param x A [grain_matrix()] result, or any numeric array.
#'
#' @return The digest, a single string of 32 hexadecimal characters.
#'
#' @examples
#' digest_array(array(c(1, -0.5), dim = c(2L, 1L, 1L)))
#'
#' @export
digest_array <- function(x) {
  v <- as.numeric(x)
  if (!all(is.finite(v))) {
    stop(sum(!is.finite(v)), " of ", length(v), " values are not finite. A digest is defined for ",
         "a finite array: the two languages spell an infinity and a missing value differently, so ",
         "a digest over one would compare the language rather than the representation.",
         call. = FALSE)
  }
  body <- paste0(paste(sprintf("%.12f", v), collapse = "\n"), "\n")
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  con <- file(f, open = "wb")
  writeBin(charToRaw(body), con)
  close(con)
  unname(tools::md5sum(f))
}
