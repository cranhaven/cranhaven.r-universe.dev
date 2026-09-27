#
# Copyright (c) 2009-2013, Stephen B. Weston
# Updated 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA



#' Combine an iterator's values into chunks.
#'
#' Create an iterator that issues lists of values from the underlying iterable.
#' This is useful for manually \dQuote{chunking} values from an iterable.
#'
#'
#' @param iterable Iterable to iterate over.
#' @param size Maximum number of values from \code{iterable} to return
#'   in each value issued by the resulting iterator.
#' @param mode Mode of the objects returned by the iterator.
#' @param fill Value to use to pad the last chunk to size, if it is
#'   short. If missing, no padding will be done.
#' @param ... Further arguments will be forwarded to `iteror(iterable, ...)`.
#' @return an iteror that yields items of length `size` and mode `mode`.
#' @seealso \code{iteror.default}
#'
#' Argument `size` does not need to be an integer, for instance a
#'   `chunk` of 3.5 will produce chunks of sizes 3 and 4
#'   alternating. The precise behavior will be subject to floating
#'   point precision.
#' @examples
#'
#' # Split the vector 1:10 into "chunks" with a maximum length of three
#' it <- i_chunk(1:10, 3)
#' repeat print(unlist(nextOr(it, break)))
#'
#' # Same as previous, but return integer vectors rather than lists
#' it <- i_chunk(1:10, 3, mode='integer')
#' repeat print(unlist(nextOr(it, break)))
#'
#' it <- i_chunk(iterators::iter(1:5), 2, fill=NA)
#' # List: list(1, 2, 3)
#' nextOr(it, NULL)
#' # List: list(4, 5, NA)
#' nextOr(it, NULL)
#'
#' it2 <- i_chunk(levels(iris$Species), 4, fill="weeee")
#' # Returns: list("setosa", "versicolor", "virginica", "weeee")
#' nextOr(it2, NA)
#'
#' @export i_chunk
i_chunk <- function(iterable, size, mode='list', fill, ...) {
  force(iterable)
  force(size)
  it <- iteror(iterable, ...)
  doFill <- !missing(fill)
  sendAt <- 0

  legal.modes <- c('list', 'logical', 'integer', 'numeric', 'double',
                   'complex', 'character', 'raw')
  if (! mode %in% legal.modes)
    stop(sprintf("cannot make a vector of mode '%s'", mode))
  if (length(size) != 1 || !is.numeric(size) || size <= 0)
    stop("'size' must be a positive number of length 1")

  if (mode == "list")
    wrap <- list
  else wrap <- identity

  nextOr_ <- function(or) {
    sendAt <<- sendAt + size
    r <- vector(mode, floor(sendAt))
    i <- 0L
    while (i < sendAt) {
      r[i+1L] <- wrap(it(or = {
        if (i == 0L) {
          return(or)
        } else if (doFill) {
          fill
        } else {
          length(r) <- i
          break
        }
      }))
      i <- i + 1L
    }
    sendAt <<- sendAt - length(r)
    r
  }

  iteror_internal(nextOr_)
}
