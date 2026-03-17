

#' @title Parallel \link[base]{mean} and \link[stats]{median}
#' 
#' @param ... \link[base]{numeric} \link[base]{vector}s
#' 
#' @param na.rm \link[base]{logical} scalar, default `TRUE`
#' 
#' 
#' @note
#' The functions [pmean()] and [pmedian()] mimic functions \link[base]{pmax.int} and \link[base]{pmin.int}.
#' They are written in a very naive way.
#' The pipeline `cbind() |> rowMeans()` is extremely slow.
#' 
#' @returns
#' The functions [pmean()] and [pmedian()] return a \link[base]{numeric} \link[base]{vector}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/pmean.html}
#' 
#' @examples
#' pmean(1:3, c(11, 12, 14), c(21, 22, 25))
#' @keywords internal
#' @name pmean
#' @export
pmean <- function(..., na.rm = TRUE) {
  list(...) |>
    do.call(what = cbind, args = _) |>
    rowMeans(na.rm = na.rm)
}

if (FALSE) {
  pmean <- function(..., na.rm = TRUE) {
    list(...) |>
      .mapply(FUN = c, dots = _, MoreArgs = NULL) |>
      vapply(FUN = mean, FUN.VALUE = NA_real_) 
    # really slow!! why?
  }
}




#' @rdname pmean
#' @importFrom matrixStats rowMedians
#' @export
pmedian <- function(..., na.rm = TRUE) {
  list(...) |>
    do.call(what = cbind) |>
    rowMedians(na.rm = na.rm)
}

