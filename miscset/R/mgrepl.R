#' @name mgrepl
#' @keywords multiple regex grepl
#' @author Sven E. Templer
#' @title Multiple Pattern Matching and Replacement
#' @description 
#' \code{mgrepl} allows multiple patterns search in character vectors,
#' offering multicore support to parallelize search over all \code{patterns}
#' using \link[parallel]{mclapply}.
#' @param patterns A vector or list containing regular expressions
#' (\link{regex}) to be searched in \code{text}. Coerced to \code{character}.
#' @param text Character vector on which the search is performed.
#' @param log.fun A function to apply on the result of matching each pattern
#' on each element of \code{text}. Determines the output. See section \bold{Value}.
#' @param na.replace A single value to replace each \code{NA} with in the result.
#' @param use.which A logical value.
#' \code{TRUE} to convert result with \link{which}.
#' Only if output \link{is.atomic}, otherwise ignored. Deprecated.
#' @param cores Numeric value for how many cores to use for computation using 
#' \code{mclapply}.
#' @param \dots Further arguments passed to functions \link{grepl}.
#' @return
#' Depending on the function defined with \code{log.fun}, the return value is either
#' \itemize{
#' \item a \code{vector}, e.g. for functions like \link{any}, \link{all} or \link{sum}.
#' \item a \code{matrix} is obtained with e.g. \link{identity} or \link{as.integer}. Each
#' row holds the result of a single pattern.
#' \item a \code{list} is returned for functions which create results of different lengths
#' for each element, such as \link{which}.
#' }
#' @seealso
#' \link{grepl}, \link{mclapply}
#' @examples
#' #
#' 
#' # strings
#' s <- c("ab","ac","bc", NA)
#' 
#' # match all patterns (default)
#' mgrepl(c("a", "b"), s)
#' 
#' # match any of the patterns
#' mgrepl(c("a", "b"), s, any)
#' grepl("a|b", s)
#' 
#' # return logical matrix, one column for each pattern
#' mgrepl(c("a", "b"), s, identity)
#' 
#' # return count of matches
#' mgrepl(c("a", "b"), s, sum)
#' 
#' #

#' @export mgrepl
mgrepl <- function(patterns, text, log.fun = all, na.replace = FALSE,
                   use.which = FALSE, cores = 1, ...) {
  if (!is.atomic(na.replace) || length(na.replace) != 1) 
    stop("use a single value for na.replace")
  f <- match.fun(log.fun)
  ina <- which(is.na(text))
  patterns <- as.list(as.character(unlist(patterns)))
  i <- mclapply(patterns, function (y) grepl(y, text, ...), mc.cores = cores)
  i <- do.call(cbind, i)
  i <- apply(i, 1, f)
  if (use.which) {
    .Deprecated(msg = "use.which will be deprecated, use which(mgrepl(...)) instead")
    if (is.atomic(i)) return(which(i))
  }
  if (length(ina)) {
    if (is.matrix(i)) {
      i[,ina] <- na.replace
    } else {
      i[ina] <- na.replace
    }
  }
  return(i)
  
}
