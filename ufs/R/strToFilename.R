#' Convert a string to a safe filename
#'
#' @param str The string to convert.
#' @param ext Optionally, an extension to append.
#'
#' @return The string, processed to remove potentially problematic characters.
#' @export
#'
#' @examples strToFilename("this contains: illegal characters, spaces, et cetera.");
strToFilename <- function(str, ext=NULL) {
  str <- trimws(tolower(str));
  str <- gsub('\\s', '-', str);
  str <- gsub('[[:punct:]]*$', '', str);
  str <- gsub('[[:punct:]]', '-', str);
  if (is.null(ext)) {
    return(str);
  } else {
    return(paste0(str,
                  ".",
                  trimws(tolower(ext))));
  }
}
