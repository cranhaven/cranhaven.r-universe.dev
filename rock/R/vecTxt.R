#' Easily parse a vector into a character value
#'
#' @param vector The vector to process.
#' @param delimiter,firstDelimiter,lastDelimiter The delimiters
#'   to use for respectively the middle, first
#'   `firstElements`, and last `lastElements` elements.
#' @param useQuote This character string is pre- and appended to all elements;
#' so use this to quote all elements (`useQuote="'"`), doublequote all
#' elements (`useQuote='"'`), or anything else (e.g. `useQuote='|'`).
#' The only difference between `vecTxt` and `vecTxtQ` is that the
#' latter by default quotes the elements.
#' @param firstElements,lastElements The number of elements for which to use
#' the first respective last delimiters
#' @param lastHasPrecedence If the vector is very short, it's possible that the
#' sum of firstElements and lastElements is larger than the vector length. In
#' that case, downwardly adjust the number of elements to separate with the
#' first delimiter (`TRUE`) or the number of elements to separate with the
#' last delimiter (`FALSE`)?
#' @param ... Any addition arguments to `vecTxtQ` are passed on to
#'   `vecTxt`.
#'
#' @return A character vector of length 1.
#' @export
#'
#' @examples vecTxtQ(names(mtcars));
vecTxt <- function(vector, delimiter = ", ", useQuote = "",
                   firstDelimiter = NULL, lastDelimiter = " & ",
                   firstElements = 0, lastElements = 1,
                   lastHasPrecedence = TRUE) {

  vector <- paste0(useQuote, vector, useQuote);

  if (length(vector) == 1) {
    return(vector);
  }

  if (firstElements + lastElements > length(vector)) {
    if (lastHasPrecedence) {
      firstElements <- length(vector) - lastElements;
    } else {
      lastElements <- length(vector) - firstElements;
    }
  }

  firstTxt <- lastTxt <- "";

  if (is.null(firstDelimiter)) {
    firstDelimiter <- delimiter;
  }
  if (is.null(lastDelimiter)) {
    lastDelimiter <- delimiter;
  }

  midBit <- vector;
  if (firstElements > 0) {
    firstBit <- utils::head(vector, firstElements);
    midBit <- utils::tail(vector, -firstElements);
    firstTxt <- paste0(paste0(firstBit, collapse=firstDelimiter),
                       firstDelimiter);
  }
  if (lastElements > 0) {
    lastBit <- utils::tail(vector, lastElements);
    midBit <- utils::head(midBit, -lastElements);
    lastTxt <- paste0(lastDelimiter, paste0(lastBit,
                                            collapse=lastDelimiter));
  }

  midTxt <- paste0(midBit, collapse=delimiter);

  return(paste0(firstTxt, midTxt, lastTxt));

}

#'@rdname vecTxt
#'@export
vecTxtQ <- function(vector, useQuote = "'", ...) {
  return(vecTxt(vector, useQuote = useQuote, ...));
}
