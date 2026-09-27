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



#' Create a repeating iterator
#'
#' Create an iterator that returns a value a specified number of times.
#'
#'
#' @param x The value to return repeatedly.
#' @param times The number of times to repeat the value.  Default value is
#' infinity.
#' @return an [iteror].
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Repeat a value 10 times
#' unlist(as.list(i_repeat(42, 10)))
#'
#' @export i_repeat
i_repeat <- function(x, times) {
  if (missing(times)) {
    nextOr_ <- function(or) {
      x
    }
  } else {
    times <- as.integer(times)
    if (is.na(times)) {
      stop('times must be a valid number')
    }

    nextOr_ <- function(or) {
      if (times <= 0) return(or)
      times <<- times - 1L
      x
    }
  }

  iteror_internal(nextOr_, "basicIteror")
}
