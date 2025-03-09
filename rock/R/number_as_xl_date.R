#' Convert a number to a date using Excel's system
#'
#' @param x The number(s)
#'
#' @return The date(s)
#' @export
#'
#' @examples preregr::number_as_xl_date(44113);
number_as_xl_date <- function(x) {
  if (!is.numeric(x)) {
    y <- TRUE
    tryCatch(
      y <- as.numeric(x),
      error = invisible,
      warning = invisible
    );
    if (isTRUE(y)) {
      ### Error in conversion, just return what we got, but as a date
      return(as.Date(x));
    } else {
      x <- y;
    }
  }
  return(as.Date(as.numeric(x), origin = "1899-12-30"));
}
