#' Iterator that covers the Cartesian product of the arguments.
#'
#' Given a number of vectors as arguments, constructs an iterator that enumerates the Cartesian product of all arguments.
#'
#' Although they share the same end goal, \code{igrid} can yield
#' drastic memory savings compared to \code{\link[base]{expand.grid}}.
#'
#' @export igrid
#' @param \dots Named vectors to iterate over.
#' @param rowMajor If TRUE, the left-most indices change fastest. If
#'   FALSE the rightmost indices change fastest.
#' @param simplify If TRUE, inputs are coerced to a common data type
#'   and results are returned in a vector (or matrix if chunking is
#'   enabled). If FALSE, results are returned as a list (or data.frame
#'   if chunking).
#' @param recycle If TRUE, the iteror starts over on reaching the end.
#' @param chunkSize Optional; how many rows to return in each step.
#' @param chunks Optional; how many chunks to divide the input into.
#' @return an [iteror] that iterates through each element from the
#'   Cartesian product of its arguments.
#' @aliases igrid
#' @examples
#' # Simulate a doubly-nested loop with a single while loop
#' it <- igrid(a=1:3, b=1:2)
#' repeat {
#'   x <- nextOr(it, break)
#'   cat(sprintf('a = %d, b = %d\n', x$a, x$b))
#' }
#'
#' it <- igrid(x=1:3, y=4:5)
#' nextOr(it, NA) # list(x=1, y=4)
#' nextOr(it, NA) # list(x=1, y=5)
#' nextOr(it, NA) # list(x=2, y=4)
#' nextOr(it, NA) # list(x=2, y=5)
#' nextOr(it, NA) # list(x=3, y=4)
#' nextOr(it, NA) # list(x=3, y=5)
#'
#' # Second Cartesian product
#' nextOr(it, NA) # list(x=1, y=4)
#' nextOr(it, NA) # list(x=1, y=5)
#' nextOr(it, NA) # list(x=2, y=4)
#' nextOr(it, NA) # list(x=2, y=5)
#' nextOr(it, NA) # list(x=3, y=4)
#' nextOr(it, NA) # list(x=3, y=5)
#'
#' # igrid is an iterator equivalent to base::expand.grid()
#' # Large data.frames are not created unless the iterator is manually consumed
#' a <- 1:2
#' b <- 3:4
#' c <- 5:6
#' it3 <- igrid(a=a, b=b, c=c)
#' df_igrid <- do.call(rbind, as.list(it3))
#' df_igrid <- data.frame(df_igrid)
#'
#' # Compare df_igrid with the results from base::expand.grid()
#' base::expand.grid(a=a, b=b, c=c)
i_grid <- count_template(
  input = alist(...=),
  options = alist(simplify = FALSE, rowMajor = TRUE),
  preamble = alist(
    args <- list(...),
    args_table <- c(if (simplify) NULL else list(),
                    ..., use.names=FALSE),
    dim <- vapply(args, length, 0),
    count <- prod(dim)),
  preamble_single = alist(
    names(args_table) <- rep(names(args), dim),
    indexer <- arrayIndexer(dim, rowMajor=rowMajor,
                            chunk=FALSE, offset=TRUE)),
  preamble_chunk = alist(
    indexer <- arrayIndexer(dim, rowMajor=rowMajor,
                            chunk=TRUE, offset=simplify)),
  output = function(ix) substitute(args_table[indexer(ix)]),
  output_chunk = function(start, len) substitute({
    if(simplify) {
      matrix(args_table[indexer(start, len)], nrow=len,
             dimnames=list(NULL, names(args)))
    } else {
      out <- args
      indices <- indexer(start, len)
      for (i in seq_along(out)) {
        out[i] <- list(args[[i]][indices[,i]])
      }
      structure(out,
                row.names=as.integer(start) + seq_len(len),
                class="data.frame")
    }
  }
  )
)
#' @export
igrid <- i_grid
