#' @title 
#' Ensure that R expressions are false
#' 
#' @description 
#' If any of the expressions in \code{...} are not all \code{FALSE}, 
#' stop is called, producing an error message indicating 
#' the first of the elements of \code{...} which were not false. 
#' 
#' @param ...
#' Any number of (logical) R expressions, 
#' which should evaluate to \code{TRUE}.
#' 
#' @return 
#' (\code{NULL} if all statements in \code{...} are \code{FALSE}.)
#'    
#' @seealso \code{\link[base]{stopifnot}} from package \pkg{base}. 
#'    
#' @export
#' 
#' @examples 
#' \dontrun{
#' stopif(is.empty(c(2,1)), 4 < 3) # all FALSE
#' stopif(is.empty(numeric(0)))
#' }
#' 
stopif <-
function(...) 
{
  n <- length(ll <- list(...))
  if (n == 0L) 
    return(invisible())
  mc <- match.call()
  for (i in 1L:n) {
    if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && all(!r))) {
      ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
      if (length(ch) > 1L) 
        ch <- paste(ch[1L], "....")
      stop(sprintf(ngettext(length(r), "%s is TRUE", "%s are not all FALSE"), 
                   ch), 
           call. = FALSE, domain = NA)
    }
  }
  invisible()
}
