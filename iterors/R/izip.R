#
# Copyright (c) 2009-2010, Stephen B. Weston
# Translated 2013 by Peter Meilstrup
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



#' Combine several iterables in parallel.
#'
#' The resulting iterator aggregates one element from each of the
#' iterables into a list for each iteration. Used for lock-step
#' iteration over several iterables at a time.
#'
#' For `[i_zip]`, the output will finish when any of the underlying iterables finish.
#'
#' @param ... multiple arguments to iterate through in parallel
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Iterate over two iterables of different sizes
#' as.list(i_zip(a=1:2, b=letters[1:3]))
#'
#' @export i_zip
i_zip <- function(...) {
  iterators <- lapply(list(...), iteror)

  if (length(iterators) == 0) {
    stop("At least one argument must be supplied.")
  } else {
    nextOr_ <- function(or) {
      lapply(iterators, nextOr, return(or))
    }
  }

  iteror_internal(nextOr_)
}

#' For `[i_zip_longest]`, any iterators that finish early are padded
#' with \code{fill}, and iteration continues until the longest
#' iterable is exhausted.
#'
#' @rdname i_zip
#' @details Originally from package `itertools2`.
#' @export
#' @param fill the value used to replace missing values when the iterables in
#' \code{...} are of uneven length
#' @return iterator that iterates through each argument in sequence
#'
#' @examples
#' it <- i_zip_longest(x=1:3, y=4:6, z=7:9)
#' nextOr(it, NA) # list(x=1, y=4, z=7)
#' nextOr(it, NA) # list(x=2, y=5, z=8)
#' nextOr(it, NA) # list(x=3, y=6, z=9)
#'
#' it2 <- i_zip_longest(1:3, 4:8)
#' nextOr(it2, NA) # list(1, 4)
#' nextOr(it2, NA) # list(2, 5)
#' nextOr(it2, NA) # list(3, 6)
#' nextOr(it2, NA) # list(NA, 7)
#' nextOr(it2, NA) # list(NA, 8)
#'
#' it3 <- i_zip_longest(1:2, 4:7, levels(iris$Species), fill="w00t")
#' nextOr(it3, NA) # list(1, 4, "setosa")
#' nextOr(it3, NA) # list(2, 5, "versicolor")
#' nextOr(it3, NA) # list("w00t", 6, "virginica")
#' nextOr(it3, NA) # list("w00t", 7, "w00t")
i_zip_longest <- function(..., fill=NA) {
  iter_list <- lapply(list(...), iteror)
  if (length(iter_list) == 0) {
    stop("At least one argument must be supplied.")
  }
  running <- rep(TRUE, length(iter_list))

  nextOr_ <- function(or) {
    if (all(!running)) return(or)
    out <- rep(list(fill), length(iter_list))
    for (i in seq_along(iter_list)) {
      if (running[i]) {
        out[i] <- list(iter_list[[i]](or = {
          running[i] <<- FALSE
          fill
        }))
      }
    }
    if (all(!running)) return(or)
    out
  }

  iteror_internal(nextOr_)
}
