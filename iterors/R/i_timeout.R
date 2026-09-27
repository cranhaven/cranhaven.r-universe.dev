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



#' Create a timeout iterator
#'
#' Create an iterator that iterates over another iterator for a specified
#' period of time, and then stops.  This can be useful when you want to search
#' for something, or run a test for awhile, and then stop.
#'
#'
#' @param iterable Iterable to iterate over.
#' @param time The time interval to iterate for, in seconds.
#' @param ... passed along to `iteror(iterable, ...)`
#' @return an [iteror] yielding values from `iterable` so long as `time` is in the future
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # See how high we can count in a tenth of a second
#' length(as.list(i_timeout(icount(), 0.1)))
#'
#' @export i_timeout
i_timeout <- function(iterable, time, ...) {
  force(time)
  it <- iteror(iterable, ...)
  starttime <- proc.time()[3]

  nextOr_ <- function(or) {
    delta <- proc.time()[3] - starttime
    if (delta >= time) return(or)
    it(or = or)
  }

  iteror_internal(nextOr_)
}
