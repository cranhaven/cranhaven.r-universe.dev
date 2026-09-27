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

#' Create a recycling iterator
#'
#' Create an iterator that recycles a specified iterable. On the first
#' repeat the iterable is buffered into memory until it finishes, then
#' we repeat the same sequence of values.
#'
#' @details Originally from the `itertools` package.
#' @param iterable The iterable to recycle.
#' @param times integer.  Number of times to recycle the values .
#'   Default value of \code{Inf} means to recycle indefinitely.
#' @param ... Further arguments will be passed along to [iteror].
#' @return an [iteror] recycling the values from the underlying iterable.
#' @examples
#'
#' # Recycle over 'a', 'b', and 'c' three times
#' i <- i_recycle(letters[1:3], 3)
#' as.character(i)
#'
#' it <- i_recycle(1:3)
#' nextOr(it, NA) # 1
#' nextOr(it, NA) # 2
#' nextOr(it, NA) # 3
#' nextOr(it, NA) # 1
#' nextOr(it, NA) # 2
#' nextOr(it, NA) # 3
#' nextOr(it, NA) # 1
#'
#' it2 <- i_recycle(1:3, times=2)
#' as.list(it2)
#'
#' @export i_recycle
i_recycle <- function(iterable, times=Inf, ...) {
  if (!is.numeric(times) || length(times) != 1 || (!is.na(times) && times < 0)) {
    stop('argument "times" must be a non-negative numeric value')
  }

  iterable <- iteror(iterable, ...)

  if(times == 1) return(iterable)
  if(times == 0) return(iteror_internal(function(or)or))

  bsize <- 256  # allocated size of buffer
  bsize.max <- 2 ^ 31 - 1  # maximum allowable allocated size of buffer
  buffer <- vector('list', length=bsize)
  blen <- 0  # number of values currently in buffer
  buffer.iter <- NULL  # will become an iterator over buffer

  # This is used until the underlying iterator runs out
  nextOr.buffering <- function(or) {
    # Check if buffer is full
    if (blen >= bsize) {
      # Don't attempt to create a list with more than 2^31-1 elements
      if (blen == bsize.max) {
        stop('underlying iterator has too many values to buffer')
      }
      # Double the size of buffer
      bsize <<- min(2 * bsize, bsize.max)
      length(buffer) <<- bsize
    }
    e <- iterable(or = {
      times <<- times - 1L  # will still be greater than zero
      length(buffer) <<- blen
      iterable <<- NULL
      buffer.iter <<- iteror(buffer)
      nextOr.pointer <<- nextOr.cycling
      return(nextOr.pointer(or))
    })
    blen <<- blen + 1
    buffer[blen] <<- list(e)
    e

  }

  # This will be used once we've run through the underlying iterator
  nextOr.cycling <- function(or) {
    buffer.iter(or = {
      if (times <= 1) {
        times <<- 0
        return(or)
      }
      times <<- times - 1
      buffer.iter <<- iteror(buffer)
      # If this throws 'StopIteration', we're done
      buffer.iter(or = or)
    })
  }

  # Set the initial value of nextOr.pointer
  nextOr.pointer <- if (is.null(buffer.iter)) nextOr.buffering else nextOr.cycling

  # This is the function that will be stored in the iterator object,
  # which will call either nextOr.buffering of nextOr.cycling, depending
  # on the value of nextOr.pointer variable
  nextOr_ <- function(or) {
    nextOr.pointer(or)
  }

  iteror_internal(nextOr_)
}
