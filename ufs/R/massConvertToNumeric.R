#' Converting many dataframe columns to numeric
#'
#' This function makes it easy to convert many dataframe
#' columns to numeric.
#'
#' @param dat The dataframe with the columns.
#' @param byFactorLabel When converting factors, whether to do this
#' by their label value (`TRUE`) or their level value (`FALSE`).
#' @param ignoreCharacter Whether to convert (`FALSE`) or
#' ignore (`TRUE`) character vectors.
#' @param stringsAsFactors In the returned dataframe, whether
#' to return string (character) vectors as factors or not.
#'
#' @return A data.frame.
#' @export
#'
#' @examples ### Create a dataset
#' a <- data.frame(var1 = factor(1:4),
#'                 var2 = as.character(5:6),
#'                 stringsAsFactors=FALSE);
#'
#' ### Ignores var2
#' b <- ufs::massConvertToNumeric(a);
#'
#' ### Converts var2
#' c <- ufs::massConvertToNumeric(a,
#'                                ignoreCharacter = FALSE);
massConvertToNumeric <- function(dat,
                                 byFactorLabel = FALSE,
                                 ignoreCharacter = TRUE,
                                 stringsAsFactors = FALSE) {
  storedAttributes <- attributes(dat);
  dat <- data.frame(lapply(dat, function(x) {
    if (is.character(x) && ignoreCharacter) {
      return(x);
    }
    else {
      return(convertToNumeric(x, byFactorLabel = byFactorLabel));
    }
  }), stringsAsFactors=stringsAsFactors);
  attributes(dat) <- storedAttributes;
  return(dat);
}
