### This file is part of 'PGRdup' package for R.

### Copyright (C) 2014-2026, ICAR-NBPGR.
#
# PGRdup is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# PGRdup is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/



#' 'Double Metaphone' phonetic algorithm
#'
#' \code{DoubleMetaphone} converts strings to double metaphone phonetic codes.
#'
#' An implementation of the Double Metaphone phonetic algorithm in \code{R}. If
#' non-ASCII characters encountered in the input character vector \code{str}, a
#' warning is issued and they are transliterated so that the accented characters
#' are converted to their ASCII unaccented versions.
#'
#' @param str A character vector whose strings are to be encoded by double
#'   metaphone algorithm.
#' @return Returns a list with two character vectors of the same length as the
#'   input vector. The first character vector contains the primary double
#'   metaphone encodings, while the second character vector contains the
#'   alternate encodings.
#' @seealso \code{\link[stringdist]{phonetic}},
#'   \href{https://cran.r-project.org/package=RecordLinkage}{\code{phonetics}}
#'
#' @section Acknowledgement: The \code{C} code for the double metaphone
#'   algorithm was adapted from Maurice Aubrey's perl module hosted at the
#'   \strong{gitpan/Text-DoubleMetaphone}
#'   \href{https://github.com/gitpan/Text-DoubleMetaphone/blob/master/double_metaphone.c}{public
#'    github library} along with the corresponding
#'   \href{https://github.com/gitpan/Text-DoubleMetaphone/blob/master/README}{license
#'    information}.
#' @references Philips, Lawrence. 2000. "The Double Metaphone Search Algorithm."
#'   \emph{C/C++ Users Journal} 18 (6): 38-43.
#'   \url{https://dl.acm.org/doi/10.5555/349124.349132}.
#' @encoding UTF-8
#' @note In case of non-ASCII characters in strings, a warning is issued and
#'   accented characters are converted to their ASCII unaccented versions.
#' @examples
#' # Return the primary and secondary Double Metaphone encodings for a character vector.
#' str1 <- c("Jyothi", "Jyoti")
#' str2 <- c("POLLACHI", "BOLLACHI")
#' DoubleMetaphone(str1)
#' DoubleMetaphone(str2)
#' \dontrun{
#' # Issue a warning in case of non-ASCII characters.
#' str3 <- c("J\xf5geva", "Jogeva")
#' DoubleMetaphone(str3) }
#' @export
#' @useDynLib PGRdup, .registration = TRUE
DoubleMetaphone <- function(str) {
  if (is.character(str) == FALSE) {
    # Check if str is a character vector and stop if not
    stop("str is not a character vector")
  }
  if (any(grepl("NON_ASCII",
                iconv(str, "latin1", "ASCII", sub = "NON_ASCII"))) == TRUE) {
    str <- iconv(str, to = "ASCII//TRANSLIT")
    warning("Non-ASCII characters were encountered.")
  }
  out <- .C("fdouble_metaphone", as.character(str),
            primary =  as.character(str),
            alternate =  as.character(str),
            len = length(str), PACKAGE = "PGRdup")
  out <- out[2:3]
  return(out)
}

# character(length(str))
