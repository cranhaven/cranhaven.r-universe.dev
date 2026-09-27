#
# Copyright (c) 2010, Stephen B. Weston
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



#' Repeat values from an iterator.
#'
#' An analogue of the \code{rep} function operating on iterables.
#'
#' @param iterable The iterable to iterate over repeatedly.
#' @param ... further arguments passed along to `iteror(iterable,
#'   ...)`
#' @param times How many times to recycle the underlying iteror (via
#'   [i_recycle]).
#' @param length.out The maximum length of output. If this is given
#'   `times` is ignored.
#' @param each The number of times to repeat each element.  You can
#'   pass a vector (recycled), or another iterable, to repeat each
#'   element a varying number of times.
#' @details Note that arguments `times` and `each` can work slightly
#'   differently from [rep]; `times` must always be of length 1; to
#'   repeat each element a specific number of times, provide a vector to
#'   `each` rather than `times`.
#' @return an iteror yilding and repeating values from `iterable`.
#' @seealso [base::rep], [i_recycle]
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' as.numeric(i_rep(1:4, 2))
#' as.numeric(i_rep(1:4, each=2))
#' as.numeric(i_rep(1:4, each=c(2,2,2,2)))
#' as.numeric(i_rep(1:4, each=c(2,1,2,1)))
#' as.numeric(i_rep(1:4, each=2, len=4))
#' as.numeric(i_rep(1:4, each=2, len=10))
#' as.numeric(i_rep(1:4, each=2, times=3))
#'
#' # Note `rep` makes `times` behave like `each` when given a vector.
#' # `i_rep` does not reproduce this behavior; give the vector to `each`.
#' # These are equivalent:
#' as.numeric(i_rep(1:4, each = 1:8, times=2))
#' rep(rep(1:4, times=2), times=1:8)
#'
#' @export i_rep
i_rep <- function(iterable, times=1, length.out=NULL, each=1, ...) {

  it <- iteror(iterable, ...)
  if (!is.null(length.out)) {
    # Ignore "times" if "length.out" is specified
    it <- i_recycle(it)
  } else {
    it <- i_recycle(it, times)
  }
  if (!is.null(each)) {
    if (is.iteror(each)) {
      it <- i_rep.eachi(it, each)
    } else {
      if (length(each) == 1) {
        it <- i_rep.each(it, each)
      } else {
        it <- i_rep.eachi(it, iteror(each, recycle=TRUE))
      }
    }
  }
  if(!is.null(length.out)) {
    it <- i_limit(it, length.out)
  }
  it
}

# Internal function used to handle the i_rep "each" argument
i_rep.each <- function(it, each) {
  list(it, each)
  each <- as.integer(each[1])

  if (is.na(each)) {
    each <- 1L
  } else if (each < 0) {
    stop("invalid 'each' argument")
  }

  if (each == 1) return(it)
  if (each == 0) return(iteror(function(or) or))

  n <- 0L
  value <- NULL

  nextOr_ <- function(or) {
    if (n <= 0) {
      value <<- it(or = or)
      n <<- each
    }
    n <<- n - 1L
    value
  }

  iteror(nextOr_)
}

# Internal function used to handle the i_rep "each" argument when not
# a single integer
i_rep.eachi <- function(it, times) {
  list(it, times)

  n <- 0L
  value <- NULL
  done <- FALSE

  nextOr_ <- function(or) {
    if(done) return(or)
    while (n <= 0) {
      n <<- times(or = {done <<- TRUE; return(or)})
      if (length(n) != 1 || is.na(n) || !is.numeric(n)) {
        stop("Invalid value from 'times'")
      }
      value <<- it(or = {done <<- TRUE; return(or)})
    }
    if (n <= 0) {
      or
    } else {
      n <<- n - 1L
      value
    }
  }

  iteror_internal(nextOr_)
}
