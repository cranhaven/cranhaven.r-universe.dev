#' @rdname base30conversion
#' @export
base30toNumeric <- function(x) {
  symbols <- rev(strsplit(tolower(x), "")[[1]]);
  res <- 0L;
  for (i in seq_along(symbols)) {
    res <- res + (30^(i-1L) * (which(base30==symbols[i])-1L));
  }
  return(res);
}
