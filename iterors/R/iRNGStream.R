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

#' Iterators returning distant random-number seeds.
#'
#' The \code{i_RNGStream} creates a sequence of random number seeds
#' that are very "far apart" (2^127 steps) in the overall random
#' number sequence, so that each can be used to make a parallel,
#' psudo-independent random iterator. This uses
#' [parallel::nextRNGStream] and the "L'Ecuyer-CMRG" generator.
#'
#' i_RNGSubStream creates seeds that are somewhat less far apart (2^76
#' steps), which might be used as "substream" seeds.
#'
#' @aliases i_RNGSubStream iRNGStream iRNGSubStream
#' @param seed Either a single number to be passed to \code{set.seed} or a
#' vector to be passed to \code{nextRNGStream} or \code{nextRNGSubStream}.
#' @return An [iteror] which produces seed values.
#' @seealso \code{\link[base]{set.seed}},
#' \code{\link[parallel]{nextRNGStream}},
#' \code{\link[parallel]{nextRNGSubStream}}
#' @references For more details on the L'Ecuyer-CMRG generator, see
#'   `vignette("parallel", package="parallel")`.
#' @details Originally from the `itertools` package.
#' @return An [iteror] which yields successive seed values.
#' @examples
#'
#' global.seed <- .Random.seed
#'
#' rng.seeds <- iRNGStream(313)
#' print(nextOr(rng.seeds))
#' print(nextOr(rng.seeds))
#'
#' # create three pseudo-independent and
#' # reproducible random number streams
#' it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#'
#' all(.Random.seed == global.seed)
#' take(it1, 5, "numeric") # 0 0 0 1 1
#' take(it2, 5, "numeric") # 0 1 1 1 1
#' take(it3, 5, "numeric") # 1 1 1 0 0
#'
#' # none of this affects the global seed
#' all(global.seed == .Random.seed)
#'
#' \donttest{
#' # Compute random numbers in three parallel processes with three
#' # well-separated seeds. Requires package "foreach"
#' library(foreach)
#' foreach(1:3, rseed=iRNGSubStream(1970), .combine='c') %dopar% {
#'   RNGkind("L'Ecuyer-CMRG") # would be better to initialize workers only once
#'   assign('.Random.seed', rseed, pos=.GlobalEnv)
#'   runif(1)
#' }
#' }
#' @importFrom parallel nextRNGStream nextRNGSubStream
#' @export i_RNGStream i_RNGSubStream
i_RNGStream <- function(seed) {

  # Convert a single number into the appropriate vector for "L'Ecuyer-CMRG"
  if (length(seed) == 1) {
    seed <- convseed(seed, "L'Ecuyer-CMRG")
  } else {
    if (checkseed(seed)[1] != "L'Ecuyer-CMRG") {
      stop("Only \"L'Ecuyer-CMRG\" seed values supported")
      # TODO: there is also a step-by-2^127 algorithm for Mersenne-Twister...
    }
  }

  # Error checking: this will throw an error right away if the seed is bad
  nextRNGStream(seed)

  # Define the "Next Element" function for the iterator
  nextOr_ <- function(or) (seed <<- nextRNGStream(seed))

  iteror_internal(nextOr_)
}

#'@export
iRNGStream <- i_RNGStream

i_RNGSubStream <- function(seed) {

  # Convert a single number into the appropriate vector for "L'Ecuyer-CMRG"
  if (length(seed) == 1) {
    seed <- convseed(seed, "L'Ecuyer-CMRG")
  } else {
    if (checkseed(seed)[1] != "L'Ecuyer-CMRG") {
      stop("Only \"L'Ecuyer-CMRG\" seed values supported")
      # TODO: there is also a step-by-2^127 algorithm for Mersenne-Twister.
    }
  }

  # Error checking: this will throw an error right away if the seed is bad
  nextRNGSubStream(seed)

  # Define the "Next Element" function for the iterator
  nextOr_ <- function(or) (seed <<- nextRNGSubStream(seed))

  iteror_internal(nextOr_)
}

#' @export
iRNGSubStream <- i_RNGSubStream

convseed <- function(iseed, kind = NULL, normal.kind = NULL, sample.kind = NULL) {
  if (!exists(".Random.seed", envir=.GlobalEnv)) set.seed()
  saveseed <- .Random.seed
  on.exit({
    assign('.Random.seed', saveseed, pos=.GlobalEnv)
  })
  set.seed(iseed, kind=kind, normal.kind = normal.kind, sample.kind = sample.kind)
  get('.Random.seed', pos=.GlobalEnv, inherits=FALSE)
}

checkseed <- function(seed, kind = NULL, normal.kind = NULL, sample.kind = NULL) {
  if (!exists(".Random.seed", envir=.GlobalEnv)) set.seed()
  saveseed <- .Random.seed
  on.exit({
    assign('.Random.seed', saveseed, pos=.GlobalEnv)
  })
  assign('.Random.seed', seed, pos = .GlobalEnv)
  nextSeed <- tryCatch(warning = \(err)stop(conditionMessage(err)),
                       message = \(err)stop(conditionMessage(err)), {
    rkind <- RNGkind()
    runif(1)
  })
  if (length(.Random.seed) != length(seed)) {
    stop("seed does not have the right length")
  }
  RNGkind(rkind[1], rkind[2], rkind[3]) # so that it may throw warnings
}
