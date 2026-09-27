#
# Copyright (c) 2009-2010, Stephen B. Weston
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



#' Limit the length of an iterator.
#'
#' Create an iterator that limits the specified iterable to a specified number of items.
#'
#' @param iterable Iterable to iterate over.
#' @param n Maximum number of values to return.
#' @param ... Extra arguments for `iteror(iterable, ...)`
#' @return an [iteror] which will stop after yielding `n` values.
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Limit icount to only return three values
#' as.list(i_limit(icount(), 3))
#'
#' @export i_limit
i_limit <- function(iterable, n, ...) {
  it <- iteror(iterable, ...)
  n <- as.integer(n)

  nextOr_ <- function(or) {
    if (n <= 0) return(or)
    n <<- n - 1L
    it(or = or)
  }

  iteror(nextOr_)
}
