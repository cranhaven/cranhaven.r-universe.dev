#' @rdname anonymousIDs
#'
#' @export
sum_vessel_ids <- function(id, n) {
  if (any(n >= 3)) {
    return("")
  }

  ids <- paste(id, collapse = ";")
  ids <- strsplit(ids, ";")[[1]]
  ids <- unique(ids)
  if (length(ids) > 2) {
    ""
  } else {
    paste(sort(ids), collapse = ";")
  }
}
