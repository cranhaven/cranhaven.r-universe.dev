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

iwhich <- function(nf, ind) {
  n <- length(ind)
  if (n == 0)
    stop('illegal zero length vector')

  x <- rep(TRUE, length(nf[[1]]))
  for (i in seq_len(n))
    x <- x & nf[[i]] == ind[i]

  which(x)
}

#' Split Iterator
#'
#' Returns an iterator that divides the data in the vector \code{x} into the
#' groups defined by \code{f}.
#'
#'
#' @param x vector or data frame of values to be split into groups.
#' @param f a factor or list of factors used to categorize \code{x}.
#' @param drop logical indicating if levels that do not occur should be
#' dropped.
#' @param \dots current ignored.
#' @return The split iterator.
#' @seealso \code{\link{split}}
#' @details Originally from the `iterators` package.
#' @examples
#'
#' x <- rnorm(200)
#' f <- factor(sample(1:10, length(x), replace = TRUE))
#'
#' it <- isplit(x, f)
#' expected <- split(x, f)
#'
#' for (i in expected) {
#'     actual <- nextOr(it, break)
#'     stopifnot(actual$value == i)
#' }
#'
#' @export isplit
isplit <- function(x, f, drop=FALSE, ...) {
  UseMethod('isplit')
}

#' @exportS3Method
isplit.default <- function(x, f, drop=FALSE, ...) {
  if (!is.list(f)) f <- list(f)
  cf <- lapply(f, function(a) if (is.factor(a)) a else as.factor(a))
  nf <- lapply(cf, as.integer)
  flevels <- lapply(f, function(a) if (is.factor(a)) levels(a) else sort(unique.default(a)))
  it <- icountn(unlist(lapply(cf, nlevels)))

  nextOr_ <- function(or) {
    repeat {
      i <- nextOr(it, return(or))
      j <- iwhich(nf, i)
      if (!drop || length(j) > 0)
        break
    }
    k <- seq_along(i)
    names(k) <- names(cf)
    key <- lapply(k, function(x) flevels[[x]][i[x]])
    list(value=x[j], key=key)
  }

  iteror_internal(nextOr_)
}

# define the data frame method which uses the default method
#' @exportS3Method isplit data.frame
isplit.data.frame <- function(x, f, drop=FALSE, ...) {
  it <- isplit(seq_len(nrow(x)), f, drop=drop, ...)
  nextOr_ <- function(or) {
    i <- nextOr(it, return(or))
    list(value=x[i$value,, drop=FALSE], key=i$key)
  }
  iteror_internal(nextOr_)
}
