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


# @examples
# # a vector iterator
# i1 <- iteror(1:3)
# nextOr(i1)
# nextOr(i1)
# nextOr(i1)
#
# # a data frame iterator by column
# i2 <- iteror(data.frame(x = 1:3, y = 10, z = c("a", "b", "c")))
# nextOr(i2)
# nextOr(i2)
# nextOr(i2)
#
# # a data frame iterator by row
# i3 <- iteror(data.frame(x = 1:3, y = 10), by = "row")
# nextOr(i3)
# nextOr(i3)
# nextOr(i3)
#
# # a function iterator
# i4 <- iteror(function() rnorm(1), sigil=NULL)
# nextOr(i4)
# nextOr(i4)
# nextOr(i4)
#

#' Make an iteror from a given object.
#'
#' `it <- iteror(obj, ...)` is a generic constructor that creates
#' objects of class "iteror" from its input. An iteror outputs a
#' single element of a sequence each time you call
#' `nextOr(it)`. Different `iteror` methods exist for different data
#' types and may take different optional arguments as listed in this
#' page.
#'
#' When called, an iteror may either return a new value or stop. The
#' way an iteror signals a stop is that it does whatever you write in
#' the argument `or`. For instance you can write `or=break` to exit a
#' loop. Summing over an iteror this way looks like:
#'
#' ```{R}
#' sum <- 0
#' it <- iteror(iseq(0, 100, 7))
#' repeat {
#'   sum <- sum + nextOr(it, break)
#' }
#' ```
#'
#' Another way to use the "or" argument is to give it a sentinel value;
#' that is, a special value that you will interpret as end of
#' iteration.  If the result of calling `nextOr` is `identical()` to
#' the value you provided, then you know the iterator has ended. This
#' pattern looks like:
#'
#' ```{R}
#' sum <- 0
#' stopped <- new.env()
#' it <- iteror(iseq(0, 100, 7))
#' repeat {
#'   val <- nextOr(it, stopped)
#'   if (identical(val, stopped)) break
#'   sum <- sum + val
#' }
#' ```
#'
#' (Here I'm using `new.env()` as a sentinel value. In R it is
#' commonplace to use `NULL` or `NA` as a kind of sentinel value, but
#' that only works until you have an iterator that needs to yield NULL
#' itself. A safer alternative is to use a local, one-shot sentinel value;
#' `new.env()` is ideal, as it constructs an object that is
#' not [identical] to any other object in the R session.)
#'
#' Note that `iteror` objects are simply functions with a class
#' attribute attached, and all `nextOr.iteror` does is call the
#' function. So if you were in the mood, you could skip calling
#' `nextOr` involving S3 dispatch and instead call the iteror
#' directly. If you take this approach, make sure you have called
#' `iteror()` first to ensure that you have a true `iteror` object.
#'
#' ```{R}
#' sum <- 0
#' it <- iteror(iseq(0, 100, 7))
#' repeat sum <- sum + it(or=break)
#' sum
#' ```
#'
#' To create iterors with custom-defined behavior, see
#' [iteror.function].
#'
#' @export
#' @param obj An object to iterate with.
#' @param ... Different `iteror` methods may take additional options
#'   depending on the class of `obj`.
#' @return an object of classes 'iteror' and 'iter'.
#' @seealso iteror.array iteror.function iteror.data.frame
iteror <- function(obj, ...) {
  UseMethod("iteror")
}

#' @exportS3Method
iteror.iteror <- function(obj, ...) obj

#' @exportS3Method
#' @rdname iteror
#' @return The method `iteror.iter` wraps an [iterators::iter] object
#' and returns an iteror.
iteror.iter <- function(obj, ...) {
  nextOr_ <- function(or) {
    tryCatch(
      iterators::nextElem(obj),
      error=function(e)
        if (identical(conditionMessage(e), 'StopIteration')) or else stop(e))
  }
  iteror_internal(nextOr_)
}

#' Construct an iteror object with custom-programmed behavior.
#'
#' Pass `obj` a function that has a first argument named "or".  In
#' writing this function, you can maintain state by using enclosed
#' variables and update using `<<-`, Whatever value `obj()` returns is
#' the next element of the iteror. Treat argument `or` as a lazy value;
#' do not touch it until until you need to signal end of iteration;
#' to signal end of iteration, force and immediately return `or`.
#'
#' You can also provide `obj` a simple function of no arguments, as
#' long as you specify one of `catch`, `sentinel`, or `count` to specify
#' how to detect end of iteration.
#'
#' @exportS3Method iteror "function"
#' @export
#' @rdname iteror.function
#' @param obj A function. It should have having an argument named "or"
#' @return An object of mode "function" and class "iteror".
#' @param ... Undocumented.
#' @param catch If `obj` does not have an `or` argument, specify
#'   e.g. `catch="StopIteration"` to interpret that an error with
#'   that message as end of iteration.
#' @param sentinel If `obj` does not have an `or` argument, you can specify
#'   a special value to watch for end of iteration. Stop will be signaled
#'   if the function result is [identical()] to `sentinel`.
#' @param count If `obj` does not have an `or` argument, you can specify
#'   how many calls before stop iteration, or
#'   give `NA` or `Inf` to never stop.
#' @return An [iteror] which calls the given function to produce values.
#' @examples
#'
#' # an iterator that counts from start to stop
#' irange <- function(from=1, to=Inf) {
#'   current <- from
#'   iteror(function(or) {
#'     if (current > to) {
#'       return(or)
#'     } else {
#'       tmp <- current
#'       current <<- current + 1
#'       tmp
#'     }
#'   })
#' }
#' it <- irange(5, 10)
#' as.vector(it, "numeric")
#'
#' # an endless random number generator
#' irand <- function(min, max) {
#'  iteror(function() runif(1, min=min, max=max), count=Inf)
#' }
#' take(irand(5, 10), 10)
iteror.function <- function(obj, ..., catch, sentinel, count) {
  stop_unused(...) # reject extra args
  if ("or" %in% names(formals(obj))) {
    fn <- obj
  } else {
    if (!missing(sentinel)) {
      force(sentinel)
      fn <- function(or, ...) {
        x <- obj(...)
        if (identical(x, sentinel)) or else x
      }
    } else if (!missing(catch)) {
      force(catch)
      fn <- function(or, ...) {
        tryCatch(obj(...), error=function(e) {
          if (identical(conditionMessage(e), catch)) {
            or
          } else stop(e)
        })
      }
    } else if (!missing(count)) {
      if (is.finite(count)) {
        fn <- function(or, ...) {
          if (count > 0) {
            count <<- count - 1L
            obj(...)
          } else or
        }
      } else {
        fn <- function(or, ...) obj(...)
      }
    } else {
      stop("iteror: function must have an 'or' argument, or else specify one of 'catch', 'sentinel' or 'count'")
    }
  }
  iteror_internal(fn)
}


iteror_internal <- function(obj, class=character(0)) {
  structure(obj, class=c(class, "iteror", "iter"))
}

#' @exportS3Method
#' @rdname iteror
#' @param recycle a boolean describing whether the iterator should
#'   reset after running through all its values.
#' @param chunks Split the input into this many chunks.
#' @param chunkSize How many elements (or slices) to include in each
#'   chunk.
#' @return The default method `iteror.default` treats `obj` as a
#'   vector to yield values from.
iteror.default <- count_template(
  input = alist(obj=),
  preamble = alist(
    if (is.function(obj)) return(iteror.function(obj, ...)),
    count <- length(obj)),
  output = function(ix) substitute(obj[[ix]]), # unboxing!!!,
  output_chunk = function(ix, len) substitute(obj[ix + seq_len(len)])
)

#' Retreive the next element from an iteror.
#' @export
#' @param obj An [iteror].
#' @param or If the iterator has reached its end, this argument
#'   will be forced and returned.
#' @param ... Other arguments may be used by specific iterors.
#' @return Either the next value of `iteror`, or the value of `or`.
nextOr <- function(obj, or, ...) {
  UseMethod("nextOr")
}

#' @exportS3Method
nextOr.iteror <- function(obj, or, ...) {
  obj(or=or, ...)
}

#' @exportS3Method iterators::nextElem iteror
nextElem.iteror <- function(obj, ...) {
  obj(stop("StopIteration", call.=FALSE), ...)
}

#' @exportS3Method
nextOr.iter <- function(obj, or, ...) {
  # :( this means that if you use nextOr over a regular iter, you
  # are setting up and tearing down a tryCatch in each iteration...
  tryCatch(
    iterators::nextElem(obj, ...),
    error=function(e)
      if (!identical(conditionMessage(e), 'StopIteration')) stop(e) else or)
}

#' \code{is.iteror} indicates if an object is an iteror.
#'
#' @aliases is.iterator
#' @param x any object.
#' @return TRUE if the object has class `iteror`.
#' @examples
#'
#' it <- iteror(1:3)
#' stopifnot(is.iteror(it))
#' repeat {
#'   print(nextOr(it, break))
#' }
#'
#' @export is.iteror
is.iteror <- function(x) inherits(x, 'iteror')
