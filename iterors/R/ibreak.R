#
# Copyright (c) 2013, Stephen B. Weston
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



#' Create an iterator that can be told to stop.
#'
#' Create an iterator that iterates over another iterator until a specified
#' function returns \code{FALSE}. This can be useful for breaking out of a
#' foreach loop, for example.
#'
#'
#' @param iterable Iterable to iterate over.
#' @param finished Function that returns a logical value.  The iterator stops
#' when this function returns \code{FALSE}.
#' @param ... Further arguments forwarded to `iteror`.
#' @return an iteror which will stop when `finished()` is `TRUE`
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # See how high we can count in a tenth of a second
#' mkfinished <- function(time) {
#'   starttime <- proc.time()[3]
#'   function() proc.time()[3] > starttime + time
#' }
#' length(as.list(i_break(icount(), mkfinished(0.1))))
#'
#' @export i_break
i_break <- function(iterable, finished, ...) {
  force(finished)
  it <- iteror(iterable, ...)
  stopped <- FALSE

  nextOr_ <- function(or) {
    if (stopped || finished()) {
      stopped <<- TRUE
      or
    } else
      it(or = or)
  }

  iteror_internal(nextOr_)
}
