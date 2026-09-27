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

#' Counting Iterators
#'
#' Returns an iterator that counts starting from one.
#'
#' @param count number of times that the iterator will fire. Use NA or
#'   Inf to make an iterator that counts forever.
#' @param ... Undocumented.
#' @param recycle Whether to restart the count after finishing.
#' @param chunkSize How many valies to return from each call to nextOr().
#' @param chunks How many chunks to split the input. Either `chunks` or `chunkSize` may be given but not both.
#' @return The counting iterator.
#' @seealso For more control over starting number and step size, see
#'   [iseq].
#' @details Originally from the `iterators` package.
#' @aliases icount
#' @export
#' @examples
#'
#' # create an iterator that counts from 1 to 3.
#' it <- icount(3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' x <- icount(5)
#' repeat print(nextOr(x, break))
#'
#' it2 <- icount(100)
#' all.equal(as.numeric(it2), 1:100)
i_count <- count_template(
  input=alist(count=Inf),
  output=function(ix) substitute(ix),
  output_chunk=function(base, len) substitute(base + seq_len(len)))

#' @export
#' @rdname i_count
icount <- function() NULL
icount <- i_count

#' Dividing Iterator
#'
#' Returns an iterator dividing a value into integer chunks, such that
#' `sum(idiv(n, ...)) == floor(n)`
#'
#' @param count The total
#' @param ... Unused.
#' @param recycle Whether to restart the count after finishing.
#' @param chunkSize the maximum size of the pieces that \code{n} should be
#' divided into.  This is useful when you know the size of the pieces that you
#' want.  If specified, then \code{chunks} should not be.
#' @param chunks the number of pieces that \code{n} should be divided into.
#' This is useful when you know the number of pieces that you want.  If
#' specified, then \code{chunkSize} should not be.
#' @return The dividing iterator.
#' @details Originally from the `iterators` package.
#' @examples
#'
#' # divide the value 10 into 3 pieces
#' it <- idiv(10, chunks = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' # divide the value 10 into pieces no larger than 3
#' it <- idiv(10, chunkSize = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # end of iterator
#'
#' @aliases idiv
#' @export i_div
i_div <- count_template(input=alist(count=),
                       output=function(ix) 1,
                       output_chunk= function(ix, size) substitute(size))
#' @export
idiv <- i_div

#' @rdname i_count
#' @param vn A vector of integers.
#' @param rowMajor If `TRUE` (default), the earliest indices will cycle fastest;
#'   if `FALSE`, last indices cycle fastest.
#' @description `icountn(vn)` takes a vector specifying an array size,
#'   and returns an iterator over array indices. Each returned element
#'   is a vector the same length as vn, with the first index varying fastest.
#'   If vn has a names attribute the output will have the same names.
#' @export
#' @aliases icountn
#' @examples
#' as.list(icountn(c(2, 3)))
i_countn <- count_template(
  input = alist(vn = ),
  options = alist(rowMajor = TRUE),
  preamble = alist(
    vn <- floor(vn+0),
    count <- prod(vn)),
  preamble_single = alist(
    indexer <- arrayIndexer(vn, rowMajor=rowMajor)),
  preamble_chunk = alist(
    indexer <- arrayIndexer(vn, rowMajor=rowMajor, chunk=TRUE)),
  output = function(ix) substitute(indexer(ix)),
  output_chunk = function(start, len) substitute(indexer(start, len)))

#' @export
icountn <- i_countn

arrayIndexer <- function(dim, rowMajor=TRUE, offset=FALSE, chunk=FALSE) {
  if (rowMajor) {
    reduction <- unname(cumprod(c(1, dim[-length(dim)])))
  } else {
    reduction <- unname(rev(cumprod(rev(c(dim[-1], 1)))))
  }
  if (offset) {
    offsets <- unname(cumsum(c(1, dim[-length(dim)])))
  } else {
    offsets <- 1
  }
  if(chunk) {
    # base is 0-based
    ndim <- length(dim)
    function(base, len) {
      (
        (     matrix(base + seq_len(len) - 1, nrow=len, ncol=ndim,
                     dimnames=list(NULL, names(dim)))
          %/% matrix(reduction, nrow=len, ncol=ndim, byrow=TRUE)
        ) %% matrix(dim, nrow=len, ncol=ndim, byrow=TRUE)
      ) + matrix(offsets, nrow=len, ncol=ndim, byrow=TRUE)
    }
  } else {
    # ix is 1-based
    function(ix) ((ix-1) %/% reduction) %% dim + offsets
  }
}
