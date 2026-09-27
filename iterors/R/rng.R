#
# Copyright (c) 2008-2010 Revolution Analytics
# Updated 2023 by Peter Meilstrup
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


#' Iterator Constructor-Constructor Function Wrapper
#'
#' The \code{makeIwrapper} function wraps an R function to produce an
#' iterator constructor.  It is used to construct random sampling
#' iterators in this package; for instance `i_rnorm` is defined as
#' `i_rnorm <- makeIwrapper(rnorm)`.
#'
#' The resulting iterator constructor will have additional optional arguments:
#' * `count` specifies the number of times the
#' resulting iterator should fire.
#' * `independent` which enables independent tracking of the random
#' number seed.
#' * `seed` specifies the starting seed.
#' * `kind` specifies the randum number generator used
#' * `normal.kind` and `sample.kind` are passed along to `set.seed`
#'
#' For more details on these arguments see \link[iterors]{rng}.
#'
#' @aliases makeIwrapper
#' @param FUN a function that generates different values each time it
#'   is called; typically one of the standard random number generator
#'   functions.
#' @return An iterator that is a wrapper around the corresponding
#'   function.
#' @details Original version appeared in the `iterators` package.
#' @examples
#'
#' # create an iterator maker for the sample function
#' mysample <- makeIwrapper(sample)
#' # use this iterator maker to generate an iterator that will generate three five
#' # member samples from the sequence 1:100
#' it <- mysample(1:100, 5, count = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # NULL
#' @export
makeIwrapper <- function(FUN) {
  def <- bquote(
    splice=TRUE,
    .(quote(`function`))(
      .(as.pairlist(c(formals(FUN),
                      list(count=quote(Inf),
                           independent=quote(!missing(seed) || !missing(kind)),
                           seed=NULL,
                           kind=NULL,
                           normal.kind=NULL,
                           sample.kind=NULL)))),
      {
        list(..(lapply(names(formals(FUN)), as.name)))
        if (independent) {
          if (is.null(seed) && is.null(kind)) {
            kind <- "L'Ecuyer-CMRG"
            seed <- rng.state$stream() # see iterors-package.R
          } else if (is.null(seed)) {
            seed <- convseed(NULL, kind, normal.kind, sample.kind)
          } else {
            if (length(seed) == 1) {
              seed <- convseed(seed, kind, normal.kind, sample.kind)
            } else {
              checkseed(seed, kind, normal.kind, sample.kind)
            }
          }

          next_ <- function(or) {
            if (count > 0) {
              count <<- count - 1L
              oldSeed <- .Random.seed
              assign('.Random.seed', seed, envir=.GlobalEnv)
              on.exit({
                assign('.Random.seed', oldSeed, envir=.GlobalEnv)
              })
              val <- .(substitute(FUN))(
                ..(structure(lapply(names(formals(FUN)), as.name),
                             names=names(formals(FUN)))))
              seed <<- .Random.seed
              val
            } else {
              return(or)
            }
          }

          iteror(next_)
        } else {
          next_ <- function(or) {
            if (count > 0) {
              count <<- count - 1L
              .(substitute(FUN))(
                ..(structure(lapply(names(formals(FUN)), as.name),
                             names=names(formals(FUN)))))
            } else {
              return(or)
            }
          }
          iteror(next_)
        }
      })
    )
  eval(def, parent.frame())
}

#' @rdname rng
#' @title Random Number Iterators
#'
#' @description These functions each construct an iterator that produces
#'   random numbers of various distributions. Each one is a wrapper
#'   around a base R function.
#'
#' @param count number of times that the iterator will fire.  If not
#'   specified, it will fire values forever.
#' @param independent If TRUE, this iterator will keep its own private
#'   random state, so that its output is reproducible and independent
#'   of anything else in the program; this comes at some performance
#'   cost. Default is FALSE _unless_ `seed` or `kind` are given.  If
#'   `independent=TRUE` but neither `seed` nor `kind` are specified,
#'   we will use the "L'Ecuyer-CMRG" generator with a seed value
#'   taken from a package-private instance of [iRNGStream].
#' @param seed A specific seed value for reproducibility. If given,
#'   `independent=TRUE` is implied. This can be a single number (which
#'   will be passed to [`set.seed(seed, kind, normal.kind,
#'   sample.kind)`][set.seed]; it can also be a vector containing a
#'   complete, valid state for [.Random.seed]. If the latter,
#'   arguments `kind`, etc. are ignored.
#' @param kind Which random number algorithm to use; passed along to
#'   [set.seed], If given, `independent=TRUE` is implied.
#' @param normal.kind Passed along to [set.seed].
#' @param sample.kind Passed along to [set.seed].
#'
#' @return An iterator that is a wrapper around the corresponding
#'   random number generator function.
#' @param n How many samples to compute per call; see e.g. [rnorm].
#' @param mean see [rnorm].
#' @param sd see [rnorm].
#' @param size see e.g. [rbinom].
#' @param prob see e.g. [rbinom].
#' @param mu see [rnbinom].
#' @param lambda see [rpois].
#' @param x see [isample].
#' @param replace see [isample].
#' @param min see [runif].
#' @param max see [runif].
#'
#' @details Originally from the `iterators` package.
#' @seealso If you are creating multiple independent iterators,
#'   [iRNGStream] will create well-separated seed values, which may
#'   help avoid spurious correlations between iterators.
#' @examples
#'
#' # create an iterator that returns three random numbers
#' it <- irnorm(1, count = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)
#'
#' # iterators created with a specific seed will make reproducible values
#' it <- irunif(n=1, seed=314, kind="L'Ecuyer-CMRG")
#' nextOr(it) # 0.4936700
#' nextOr(it) # 0.5103891
#' nextOr(it) # 0.2338745
#'
#' # the iRNGStream produces a sequence of well separated seed values,
#' rng.seeds <- iRNGStream(313)
#' it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' take(it1, 5, "numeric") # 0 1 0 0 1
#' take(it2, 5, "numeric") # 0 1 0 0 0
#' take(it3, 5, "numeric") # 0 0 0 1 1
#'
#' @importFrom stats rbinom rnbinom rnorm rpois runif
#' @export irnorm irbinom irnbinom irpois isample irunif
#' @aliases irnorm irunif irbinom irnbinom irpois isample i_rnorm i_runif i_rbinom i_rpois i_rnbinom i_sample i_runif

# function() NULL is to quiet CMD check "no visible global function definition"
i_rnorm <- function() NULL
i_rnorm <- makeIwrapper(rnorm)
#' @rdname rng
irnorm <- function() NULL
irnorm <- i_rnorm
#' @rdname rng
i_runif <- function() NULL
i_runif <- makeIwrapper(runif)
#' @rdname rng
irunif <- function() NULL
irunif <- i_runif
#' @rdname rng
i_rbinom <- function() NULL
i_rbinom <- makeIwrapper(rbinom)
#' @rdname rng
irbinom <- function() NULL
irbinom <- i_rbinom
#' @rdname rng
i_rpois <- function() NULL
i_rpois <- makeIwrapper(rpois)
#' @rdname rng
irpois <- function() NULL
irpois <- i_rpois
#' @rdname rng
i_rnbinom <- function() NULL
i_rnbinom <- makeIwrapper(rnbinom)
#' @rdname rng
irnbinom <- function() NULL
irnbinom <- i_rnbinom
#' @rdname rng
i_sample <- function() NULL
i_sample <- makeIwrapper(sample)
#' @rdname rng
isample <- function() NULL
isample <- i_sample
#' @rdname rng
i_runif <- function() NULL
i_runif <- makeIwrapper(runif)
#' @rdname rng
irunif <- function() NULL
irunif <- i_runif
