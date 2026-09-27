#
# Copyright (c) 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3 of the License, or
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


#' Does This Iterator Have A Next Element
#'
#' `wrapped <- ihasnext(obj)` wraps an [iteror] object with the
#' `ihasNext` class. Then `hasNext(wrapped)` will indicate if the
#' iterator has another element.
#'
#' @aliases hasNext hasNext.ihasNext
#' @param obj an iterable
#' @param ... extra arguments may be passed along to [iteror].
#' @return Logical value indicating whether the iterator has a next
#'   element.
#' @keywords methods
#' @details A class `ihasNext` was introduced in the `itertools`
#'   package to try to reduce the boilerplate around extracting the
#'   next value using [iterators::nextElem].  `ihasNext` is included
#'   in `iterors` for backward compatibility with iterator code; however,
#'   it is less needed when using the [nextOr] iteration method, as you can
#'   directly give an action to take at end of iteration.
#' @examples
#' # The bad old style of consuming an iterator in a loop with `nextElem`:
#'   it <- ihasNext(iteror(c('a', 'b', 'c')))
#'   tryCatch(repeat {
#'     print(iterators::nextElem(it))
#'   }, error=function(err) {
#'     if (conditionMessage(err) != "StopIteration")
#'       stop(err)
#'   })
#'
#' # with ihasNext, this became:
#'   it <- ihasNext(iteror(c('a', 'b', 'c')))
#'   while (hasNext(it))
#'     print(iterators::nextElem(it))
#'
#' # But using `nextOr` all you need is:
#'   iteror(c('a', 'b', 'c'))
#'   repeat print(nextOr(it, break))
#'
#' @export hasNext
hasNext <- function(obj, ...) {
  UseMethod("hasNext")
}

#' @exportS3Method
hasNext.ihasNextOr <- function(obj, ...) {
  attr(obj, "hasNext")()
}

#' @export
#' @rdname hasNext
ihasNext <- function(obj, ...) {
  UseMethod("ihasNext")
}

#' @exportS3Method
ihasNext.default <- function(obj, ...) ihasNext(iteror(obj, ...))

#' @exportS3Method
ihasNext.iteror <- function(obj, ...) {
  noValue <- new.env() #sigil values
  endIter <- new.env()
  last <- noValue
  nextOr_ <- function(or) {
    if (identical(last, noValue))
      last <<- obj(endIter)
    if (identical(last, endIter))
      or
    else {
      tmp <- last
      last <<- noValue
      tmp
    }
  }

  hasNext_ <- function() {
    if (identical(last, noValue))
      last <<- obj(endIter)
    !identical(last, endIter)
  }

  it <- iteror_internal(nextOr_)
  structure(it, class=c("ihasNextOr", class(it)), hasNext=hasNext_)
}

#' @export
ihasNext <- function(obj, ...) {
  UseMethod("ihasNext")
}

#' @exportS3Method
ihasNext.ihasNextOr <- function(obj, ...) obj

#' @exportS3Method
ihasNext.default <- function(obj, ...) ihasNext(iteror(obj, ...))
