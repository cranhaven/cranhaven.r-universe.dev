
# Infix operator for python-style tuple assignment.
# (c) 2010, assumend Public Domain, author not easily identified, source:
# https://strugglingthroughproblems.wordpress.com/2010/08/27/matlab-style-multiple-assignment-in%C2%A0r/


#' Infix operator for python-style tuple assignment
#'
#' @return Last assigned value
#' @name tuple_assignment
#'
#' @examples
#' g(a,b) %=% c(1,2) # equivalent to a <- 1; b <- 2

# Generic form
#' @param l left-hand side: "tuple" or variables created by \code{g()}
#' @param r right-hand side: Vector to assign to left-hand side variable
#' @export
#' @rdname tuple_assignment
`%=%` <- function(l, r)
{
  UseMethod('%=%')
}

#' @export
`%=%.lbunch` <- function(l, r)
{
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

#' @param ... Left-hand side variables to group
#' @rdname tuple_assignment
#' @export
g <- function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

