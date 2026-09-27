nullish <- function(x) length(x) == 0

bracelist <- function(x) `attributes<-`(as.list(x)[-1], NULL)

#' Iterators for sequence generation
#'
#' Constructs iterators that generate regular sequences that follow the
#' \code{\link[base]{seq}} family.
#'
#' The \code{iseq} function generates a sequence of values beginning with
#' \code{from} and ending with \code{to}. The sequence of values between are
#' determined by the \code{by}, \code{length_out}, and \code{along_with}
#' arguments. The \code{by} argument determines the step size of the sequence,
#' whereas \code{length_out} and \code{along_with} determine the length of the
#' sequence. If \code{by} is not given, then it is determined by either
#' \code{length_out} or \code{along_with}. By default, neither are given, in
#' which case \code{by} is set to 1 or -1, depending on whether \code{to >
#' from}.
#'
#' @export
#' @param from the starting value of the sequence.
#' @param to the end value of the sequence.
#' @param by increment of the sequence.
#' @param length_out desired length of the sequence. A non-negative number,
#' which for \code{seq} will be rounded up if fractional.
#' @param along_with the length of the sequence will match the length of this
#' @param ... Unused.
#' @param chunkSize Optional; return this many values per call.
#' @param chunks Optional; return this many chunks.
#' @param recycle Whether to restart the sequence after it reaches `to`.
#' @seealso icount icountn
#' @return an [iteror].
#'
#' @aliases iseq iseq_along
#' @examples
#' it <- iseq(from=2, to=5)
#' unlist(as.list(it)) == 2:5
i_seq <- count_template(
  input=alist(from=1,
              to=NULL,
              by=NULL,
              length_out=NULL,
              along_with=NULL),
  preamble = bracelist(quote({
    mode(from) <- "numeric" # keeps names
    to <- as.numeric(to)
    by <- as.numeric(by)

    if (length(from) != 1) {
      stop("'from' must be a numeric value of length 1")
    }
    if (length(by) > 1 || (length(by) == 1 && by == 0)) {
      stop("'by' must be a nonzero numeric value of length 1")
    }
    if (nullish(to)) {
      if (nullish(by) || by > 0)
        to <- Inf
      else to <- -Inf
    }
    if (length(to) != 1) {
      stop("'to' must be a numeric value of length 1")
    }

    # If 'by' is not given, then it is determined by either 'length_out' or
    # 'along_with'.  # By default, neither are given, in which case 'by' is set to
    # 1 or -1, depending on whether to > from.
    if (nullish(by)) {
      if (!nullish(along_with)) {
        length_out <- length(along_with)
        by <- (to - from) / (length_out - 1)
      } else if (!nullish(length_out)) {
        by <- (to - from) / (length_out - 1)
      } else if (to >= from) {
        by <- 1
        length_out <- to-from+1
      } else if (to < from) {
        by <- -1
        length_out <- from-to+1
      }
      count <- floor(length_out)
    } else {
      count <- floor((to - from) / by) + 1
    }
    base <- from - by
  })),
  output = function(ix) substitute(base + by * ix),
  output_chunk = function(ix, len) substitute(base + by*(ix+seq_len(len)))
)
#' @export
iseq <- i_seq

#' `i_seq_along(obj)` is just an alias for i_count(length(obj)).
#' @rdname i_seq
#' @export
i_seq_along <- function(along_with, ...) icount(length(along_with), ...)

#' @export
iseq_along <- i_seq_along
