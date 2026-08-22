#' Sync (expand or compress) a vector
#'
#' @param x The vector
#' @param newLength The new length
#'
#' @return The synced vector
#' @inheritParams syncing_vector_compress
#' @inheritParams syncing_vector_expand
#' @export
#'
#' @examples rock::sync_vector(letters[1:10], 15);
#' rock::sync_vector(letters[1:10], 5);
sync_vector <- function(x,
                        newLength,
                        sep = " ",
                        fill = TRUE,
                        compressFun = NULL,
                        expandFun = NULL,
                        compressFunPart = NULL,
                        silent = rock::opts$get('silent')) {

  oldLength <- length(x);

  if (oldLength == newLength) {
    return(x);
  } else if (oldLength < newLength) {
    return(
      syncing_vector_expand(
        x = x,
        newLength = newLength,
        fill = fill,
        expandFun = expandFun,
        silent = silent
      )
    );
  } else if (oldLength > newLength) {
    return(
      syncing_vector_compress(
        x = x,
        newLength = newLength,
        sep = sep,
        compressFun = compressFun,
        compressFunPart = compressFunPart,
        silent = silent
      )
    );
  }

}
